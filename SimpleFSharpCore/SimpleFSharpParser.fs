// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCore.Parser

open SimpleFSharpCore.CoreTools
open SimpleFSharpCore.SyntaxTree

let lowPrecedence = 0      // anything should take precedence over this
let unaryPrecedence = 90   // should have max precedence
let applyPrecedence = 100  // should have precedence over all binary ops


let precedence (op: BinOpKind) =
  match op with
  | OpCompar _  -> 10
  | OpBool BoolOr -> 20
  | OpBool BoolAnd -> 30
  | Add | OpNum Sub -> 40
  | OpNum Mul | OpNum Div | OpNum Mod -> 50

let isPrefixToken = function
  | IntT _ | BoolT _ | StrT _ | Ident _ | Fun | LParen -> true
  | _ -> false

let startsExpr = function
  | Fun | IntT _ | BoolT _ | StrT _ | Ident _ 
  | Minus | BooleanNot | StringOpT | IntOpT 
  | LParen | InterpolatedStart -> true
  | _ -> false

let rec makeNestedLambda paramNames bodyBlock =
  match paramNames with
  | [] -> failwith "Lambda must have at least one parameter"
  | [p] -> LambdaExpr { prm = p; body = bodyBlock }
  | p :: ps ->
      let inner = makeNestedLambda ps bodyBlock
      let block = { statements = []; expr = inner }
      LambdaExpr { prm = p; body = block }

let rec parseParamList tokens =
  match tokens with
  | [] -> ([], [])
  | (Ident s, _) :: rest ->
      let (prms, rest') = parseParamList rest
      (s :: prms, rest')
  | _ -> ([], tokens)


let rec parseExpr (tokens: (Token*Position) list) (block: TokenNode list)  (minPrec: int) : Expr * (Token*Position) list =
  let prefix, rest =
    match tokens with
    | (Fun, _) :: _ -> parseLambda tokens block
//    | (IfT, _) :: _ -> parseIfEx tokens block
    | (IntT n, _) :: rest -> (Lit (IntLit n), rest)
    | (BoolT b, _) :: rest -> (Lit (BoolLit b), rest)
    | (StrT s, _) :: rest -> (Lit (StringLit s), rest)
    | (Ident s, _) :: rest -> (Var s, rest)
    | (Minus, _) :: rest ->
        let expr, rest' = parseExpr rest block unaryPrecedence
        (UnaryOp(Neg, expr), rest')
    | (BooleanNot, _) :: rest ->
        let expr, rest' = parseExpr rest block unaryPrecedence
        (UnaryOp(BoolNot, expr), rest')
    | (StringOpT, _) :: rest ->
        let expr, rest' = parseExpr rest block unaryPrecedence
        (UnaryOp(StringOp, expr), rest')
    | (IntOpT, _) :: rest ->
        let expr, rest' = parseExpr rest block unaryPrecedence
        (UnaryOp(IntOp, expr), rest')
    | (LParen, _) :: rest ->
        let e, rest' = parseExpr rest block lowPrecedence
        match rest' with
        | (RParen, _) :: tail -> (e, tail)
        | _ -> failwith "expected ')'"
    | (InterpolatedStart, _) :: (StringStart, _) :: rest ->
        parseInterpolatedString rest
    | (token, _) :: rest -> failwith $"Unexpected token ({token}) found when parsing expression"
    | _ -> failwith "No token found when parsing expression"

  parseInfixRest prefix rest block minPrec

and parseInfixRest left tokens block minPrec : Expr * (Token*Position) list =
    match tokens with
    | [] -> (left, [])
    | (next, _) :: _ when startsExpr next ->
        if applyPrecedence > minPrec then
            let arg, rest' = parseExpr tokens block applyPrecedence
            parseInfixRest (Apply(left, arg)) rest' block minPrec
        else
            (left, tokens)

    | (token, _) :: rest ->
        match getBinOp token with
        | Some op when precedence op >= minPrec ->
            let prec = precedence op
            let nextMinPrec = prec + 1
            let right, rest' = parseExpr rest block nextMinPrec
            let combined = BinOp(op, left, right)
            parseInfixRest combined rest' block minPrec
        | _ ->
            (left, tokens)

and parseLambda tokens nestedBlock =
  match tokens with
  | (Fun, _) :: rest ->
      let paramNames, rest' = parseParamList rest
      match rest' with
      | [(Arrow, _); (EOF, _)] ->
          let body =
            parseExprBlock nestedBlock
          let lambdaExpr =
            makeNestedLambda paramNames body
          (lambdaExpr, [])
      | (Arrow, _) :: rest'' ->
          let (expr, rest''') =
              parseExpr rest'' nestedBlock lowPrecedence
          let body = { statements = []; expr = expr }
          let lambdaExpr =
            makeNestedLambda paramNames body
          (lambdaExpr, rest''')
      | _ -> failwith "Expected '->' in lambda declaration"
  | _ -> failwith "Expected 'fun' at start of lambda"

and parseInterpolatedString (tokens: (Token*Position) list) : Expr * (Token*Position) list  =
  let rec loop (tokens: (Token*Position) list) : InterpolatedSegment list * (Token*Position) list =
    match tokens with
    | (InterpolatedText s, _) :: rest ->
        let (segments, remainder) = loop rest
        (StringSegment(s) :: segments, remainder)
    | (InterpolatedExprStart, _) :: rest ->
        let (expr, rest') = parseExpr rest [] lowPrecedence
        match rest' with
        | (InterpolatedExprEnd, _) :: rest'' ->
            let (segments, remainder) = loop rest''
            (InterpolatedExpr(expr) :: segments, remainder)
        | (token, _) :: _ -> failwith $"Unexpected token ({printToken token}) in interpolated string expression"
        | _ -> failwith "Unexpected termination in interpolated string expression"
    | (StringEnd, _) :: rest -> ([], rest)
    | (token, _) :: _ -> failwith $"Unexpected token ({printToken token}) in interpolated string"
    | _ -> failwith "Unexpected termination in interpolated string"
  let (segments, rest) = loop tokens
  (InterpolatedStr(segments), rest)

and parseExprBlock (nodes: TokenNode list) : ExprBlock =
  match nodes with
  | [] -> failwith "Expected expression"
  | [tn] ->
      match tn.tokens with
      | (IfT, _) :: _ ->
          let expr, _ = parseIfEx nodes
          { statements = []; expr = expr }
      | _ ->
          let expr, _ =
            parseExpr tn.tokens tn.nestedBlock lowPrecedence
          { statements = []; expr = expr }
  | tn :: remaining ->
      match tn.tokens with
      | (IfT, _) :: _ ->
          let expr, remaining' = parseIfEx nodes
          let stmt = Expression(expr)
          let blk = parseExprBlock remaining'
          { blk with statements = stmt :: blk.statements }
      | _ ->
          let stmt = parseStatement tn
          let blk = parseExprBlock remaining
          { blk with statements = stmt :: blk.statements }

and parseIfEx (nodes: TokenNode list) : Expr * TokenNode list =
  match nodes with
  | [] -> failwith "Unexpected expression"
  | tn :: remaining ->
      match tn.tokens with
      | (IfT, _) :: rest ->
          // construct the if clause
          printfn "pwrsing if"
          let condExpr, rest' =
            parseExpr rest [] lowPrecedence
          let thenBlock, rest'', isExpr =
            parseThenExpr rest' tn.nestedBlock
          let cond =
            { condition = condExpr; eblock = thenBlock }
          // cpnstruct the elif and else clauses
          match isExpr with
          | true ->
              let conds, tokens =
                parseElIfExpr rest''
              let elseBlock, tokens' =
                parseElseExpr tokens tn.nestedBlock
              (IfEx(cond :: conds, elseBlock), remaining)
          | false ->
              let conds, remaining' = 
                parseElIfBlock remaining
              match remaining' with
              | tn' :: nodes ->
                  let elseBlock, tokens' =
                    parseElseExpr tn'.tokens tn'.nestedBlock
                  (IfEx(cond :: conds, elseBlock), nodes)
              | _ -> failwith "Missing else in if expression"
      | _ -> failwith "Unexpected token"

and parseThenExpr tokens nestedBlock : ExprBlock * (Token*Position) list * bool =
  match tokens with
  | [(ThenT, _); (EOF, _)] ->
      ((parseExprBlock nestedBlock), [], false)
  | (ThenT, _) :: rest ->
      let thenExpr, rest' =
        parseExpr rest nestedBlock lowPrecedence
      ({ statements = []; expr = thenExpr }, rest', true)
  | (token, _) :: _ ->
      failwith $"Expected 'then' after conditional, received '{printToken token}'"
  | [] -> failwith "Missing 'then' after conditional"

and parseElIfExpr tokens : (ConditionalExpr list) * (Token*Position) list =
  match tokens with
  | (ElIfT, _) :: rest ->
      let condExpr, rest' =
        parseExpr rest [] lowPrecedence
      let thenBlock, rest'', _ =
        parseThenExpr rest' []
      let cond =
        { condition = condExpr; eblock = thenBlock }
      let (conds, rest''') = parseElIfExpr rest''
      (cond :: conds, rest''')
  | _ -> ([], tokens)

and parseElIfBlock (nodes: TokenNode list) : ConditionalExpr list * TokenNode list =
  match nodes with
  | [] -> ([], [])
  | tn :: remaining ->
      match tn.tokens with
      | (ElIfT, _) :: rest ->
          let condExpr, rest' =
            parseExpr rest [] lowPrecedence
          let thenBlock, rest'', _ =
            parseThenExpr rest' tn.nestedBlock
          let cond = { condition = condExpr; eblock = thenBlock }
          let (conds, nodes) = parseElIfBlock remaining
          (cond :: conds, nodes)
      | _ -> ([], nodes)

and parseElseExpr tokens nestedBlock : ExprBlock * (Token*Position) list =
  match tokens with
  | [(ElseT, _); (EOF, _)] ->
      ((parseExprBlock nestedBlock), [])
  | (ElseT, _) :: rest ->
      let elseExpr, rest' =
        parseExpr rest nestedBlock lowPrecedence
      ({ statements = []; expr = elseExpr }, rest')
  | (token, _) :: _ ->
      failwith $"Expected 'else' after conditional, received '{printToken token}'"
  | [] -> failwith "Missing 'else' after conditional"

and parseStmtBlock (nodes: TokenNode list) =
  match nodes with
  | [] -> { statements = [] }
  | tn :: rest ->
      match tn.tokens with
      | (IfT, _) :: _ ->
          let (stmt, rest') = parseIfStatement tn rest
          let block = parseStmtBlock rest'
          { statements = stmt :: block.statements }
      | _ ->
          let stmt = parseStatement tn
          let block = parseStmtBlock rest
          { statements = stmt :: block.statements }

and parseIfStatement (tn: TokenNode) (rest: TokenNode list) : Statement * TokenNode list =
  let firstCond = parseConditional tn.tokens tn.nestedBlock
  let rec collectElifs tokens nodes acc =
    match nodes with
    | [] -> (List.rev acc, None, [])
    | tn' :: rest' ->
        match tn'.tokens with
        | (ElIfT, _) :: tks ->
            let cond =
              parseConditional tks tn'.nestedBlock
            collectElifs tokens rest' (cond :: acc)
        | (ElseT, _) :: _ ->
            let elseBlock = parseStmtBlock tn'.nestedBlock
            (List.rev acc, Some elseBlock, rest')
        | _ -> (List.rev acc, None, nodes)

  let (elifs, elseOpt, rest') =
      collectElifs [] rest [firstCond]
  (IfSt(elifs, elseOpt), rest')

and parseConditional (tokens: (Token * Position) list) (block: TokenNode list) : ConditionalStmt =
  match tokens with
  | (IfT, _) :: rest
  | (ElIfT, _) :: rest ->
      let (condExpr, afterCond) =
          parseExpr rest block lowPrecedence
      match afterCond with
      | (ThenT, _) :: (EOF, _) :: _ ->
          let body = parseStmtBlock block
          { condition = condExpr; sblock = body }
      | (ThenT, _) :: rest' ->
          let (bodyExpr, rest'') =
              parseExpr rest' block lowPrecedence
          let block =
              { statements = [Expression(bodyExpr)] }
          { condition = condExpr; sblock = block }
      | _ -> failwith "Expected 'then'"
  | _ -> failwith "Expected 'if' or 'elif'"


and parseStatement (node: TokenNode) : Statement =
  match node.tokens with
  | (LetT, _) :: _ ->
      parseLet node.tokens node.nestedBlock
  | (Ident s, _) :: (ReverseArrow, _) :: rest ->
      let node' =
        { tokens = rest; nestedBlock = node.nestedBlock }
      let eblk = parseExprBlock [node']
      MutableAssign(s, eblk)      
  | (WhileT, _) :: rest ->
      parseWhile node.tokens node.nestedBlock
  | _ ->
      let (expr, rest) =
        parseExpr node.tokens node.nestedBlock lowPrecedence
      Expression(expr)

and parseLet tokens nestedBlock : Statement =
  match tokens with
    // recursive functions
  | (LetT, _) :: (RecT, _) :: rest ->
      let (vars, rest') = parseParamList rest
      match vars with
      | [] -> failwith "Expected parameters for function"
      | name :: prms ->
          let fn =
            parseFunction name prms true rest' nestedBlock
          Fn(fn)
  | (LetT, _) :: (MutableT, _) :: (Ident name, _) :: (EqualsT, _) :: rest ->
      let node = { tokens = rest; nestedBlock = nestedBlock }
      let eblk = parseExprBlock [node]
      Let(name, true, eblk)
  | (LetT, _) :: (Ident name, _) :: (EqualsT, _) :: rest ->
      let node = { tokens = rest; nestedBlock = nestedBlock }
      let eblk = parseExprBlock [node]
      Let(name, false, eblk)
  | (LetT, _) :: rest ->
      let (vars, rest') = parseParamList rest
      match vars with
      | [] -> failwith "Expected params for function"
      | name :: prms ->
          let fn =
            parseFunction name prms false rest' nestedBlock
          Fn(fn)
  | _ -> failwith "Unexpected token in let statement"

and parseFunction name prms isRecursive tokens block : Function =
  printfn "parsing function"
  printTokens tokens
  match prms with
  | p :: p' :: rest ->
      let fn =
        parseFunction name (p' :: rest) isRecursive tokens block
      let block =
        { statements = []; expr = LambdaExpr(fn.lambda) }
      let lambda = { prm = p; body = block }
      { fn with lambda = lambda }
  | p :: rest ->
      // TODO: handle tokens instesd of block
      let block' =
        match tokens with
        | [(EqualsT, _); (EOF, _)] ->
            parseExprBlock block
        | (EqualsT, _) :: rest' ->
            let node = { tokens = rest'; nestedBlock = block }
            parseExprBlock [node]
        | _ -> failwith "Expected '=' in function definition"
      let lambda = { prm = p; body = block' }
      { name = name; recursive = isRecursive; lambda = lambda }
  | _ -> failwith "Missing function parameters"

and parseWhile (tokens: (Token * Position) list) (block: TokenNode list) : Statement =
  match tokens with
  | (WhileT, _) :: rest ->
      let (condExpr, afterCond) =
          parseExpr rest block lowPrecedence
      match afterCond with
      | [(DoT, _); (EOF, _)] -> 
          // Expect a nested block for the while-body
          let body = parseStmtBlock block
          While(condExpr, body)      
      | (DoT, _) :: rest' ->
          // Inline do expression (rare but possible)
          let (expr, _) =
              parseExpr rest' block lowPrecedence
          let body = { statements = [Expression(expr)] }
          While(condExpr, body)
      | _ ->
          failwith "Expected 'do' after while condition"

  | _ -> failwith "Expected 'while'"

let parse (nodes: TokenNode list) : StatmtBlock =
  parseStmtBlock nodes

