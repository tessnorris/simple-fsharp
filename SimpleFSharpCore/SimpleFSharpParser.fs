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

let isUnaryOnly = function
  | Minus | BooleanNot | StringOpT | IntOpT -> true
  | _ -> false

let rec makeNestedLambda paramNames body =
  match paramNames with
  | [] -> failwith "Lambda must have at least one parameter"
  | [p] -> LambdaExpr { prm = p; body = body }
  | p :: ps ->
      let inner = makeNestedLambda ps body
      LambdaExpr { prm = p; body = inner }

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


    // parse Apply
and parseInfixRest left tokens block minPrec : Expr * (Token*Position) list =
    match tokens with
    | [] -> (left, [])
    | (next, _) :: _ when startsExpr next && not (isUnaryOnly next)->
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
            parseBlock nestedBlock
          let lambdaExpr =
            makeNestedLambda paramNames body
          (lambdaExpr, [])
      | (Arrow, _) :: rest'' ->
          let (expr, rest''') =
              parseExpr rest'' nestedBlock lowPrecedence
          let lambdaExpr =
            makeNestedLambda paramNames expr
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

and parseBlock (nodes: TokenNode list) : Expr =
  match nodes with
  | [] -> Block([])
  | _ ->
      let expr, remaining = parseNext nodes
      let rest = parseBlock remaining
      match rest with
      | Block (exprs) -> Block(expr :: exprs)
      | _ -> Block([expr; rest])
    
and parseNext nodes : Expr * TokenNode list =
  match nodes with
  | [] -> failwith "Expected expression"
  | tn :: remaining ->
      match tn.tokens with
      | (IfT, _) :: _ -> parseIf nodes
      | (LetT, _) :: _ -> parseLet nodes
      | (Ident s, _) :: (ReverseArrow, _) :: rest ->
          let node' =
            { tokens = rest;
              nestedBlock = tn.nestedBlock }
          let expr, remaining' =
            parseNext (node' :: remaining)
          (MutableAssign(s, expr), remaining')
      | (WhileT, _) :: rest ->
          ((parseWhile tn), remaining)
      | _ ->
          let expr, _ =
            parseExpr tn.tokens tn.nestedBlock lowPrecedence
          (expr, remaining)

and parseIf (nodes: TokenNode list) : Expr * TokenNode list =
  match nodes with
  | [] -> failwith "Unexpected expression"
  | tn :: remaining ->
      match tn.tokens with
      | (IfT, _) :: rest ->
          // construct the if clause
          let condExpr, rest' =
            parseExpr rest [] lowPrecedence
          let thenBlock, rest'', isExpr =
            parseThen rest' tn.nestedBlock
          let cond =
            { condition = condExpr; expr = thenBlock }
          // construct the elif and else clauses
          match isExpr with
          | true ->
              let conds, tokens =
                parseElIfExpr rest''
              let maybeElse, tokens' =
                parseElse tokens tn.nestedBlock
              (If(cond :: conds, maybeElse), remaining)
          | false ->
              let conds, remaining' = 
                parseElIfBlock remaining
              match remaining' with
              | [] ->
                  (If(cond :: conds, None), [])
              | tn' :: remaining'' ->
                let maybeElse, _ =
                  parseElse tn'.tokens tn'.nestedBlock
                (If(cond :: conds, maybeElse), remaining'')
      | _ -> failwith "Unexpected token in if"

and parseThen tokens nestedBlock : Expr * (Token*Position) list * bool =
  match tokens with
  | [(ThenT, _); (EOF, _)] ->
      ((parseBlock nestedBlock), [], false)
  | (ThenT, _) :: rest ->
      let thenExpr, rest' =
        parseExpr rest nestedBlock lowPrecedence
      (thenExpr, rest', true)
  | (token, _) :: _ ->
      failwith $"Expected 'then' after conditional, received '{printToken token}'"
  | [] -> failwith "Missing 'then' after conditional"

and parseElIfExpr tokens : Conditional list * (Token*Position) list =
  match tokens with
  | (ElIfT, _) :: rest ->
      let condExpr, rest' =
        parseExpr rest [] lowPrecedence
      let thenBlock, rest'', _ =
        parseThen rest' []
      let cond =
        { condition = condExpr; expr = thenBlock }
      let (conds, rest''') = parseElIfExpr rest''
      (cond :: conds, rest''')
  | _ -> ([], tokens)

and parseElIfBlock (nodes: TokenNode list) : Conditional list * TokenNode list =
  match nodes with
  | [] -> ([], [])
  | tn :: remaining ->
      match tn.tokens with
      | (ElIfT, _) :: rest ->
          let condExpr, rest' =
            parseExpr rest [] lowPrecedence
          let thenBlock, rest'', _ =
            parseThen rest' tn.nestedBlock
          let cond = { condition = condExpr; expr = thenBlock }
          let (conds, nodes) = parseElIfBlock remaining
          (cond :: conds, nodes)
      | _ -> ([], nodes)

and parseElse tokens nestedBlock : Expr option * (Token*Position) list =
  match tokens with
  | [(ElseT, _); (EOF, _)] ->
      (Some(parseBlock nestedBlock), [])
  | (ElseT, _) :: rest ->
      let elseExpr, rest' =
        parseExpr rest nestedBlock lowPrecedence
      (Some(elseExpr), rest')
  | _ -> (None, tokens)

and parseLet nodes : Expr * TokenNode list =
  match nodes with
  | [] -> failwith "Expected 'let'"
  | tn :: remaining ->
      match tn.tokens with
      | (LetT, _) :: (RecT, _) :: rest ->
          let (vars, rest') = parseParamList rest
          match vars with
          | name :: p1 :: prest ->
              let prms = p1 :: prest
              let fn =
                parseFunction name prms true rest' tn.nestedBlock
              (Fn(fn), remaining)
          | _ ->
              failwith "Expected parameters for function"
      | (LetT, _) :: (MutableT, _) :: (Ident name, _) :: (EqualsT, _) :: (EOF, _) :: [] ->
          let expr = parseBlock tn.nestedBlock
          (Let(name, true, expr), remaining)
      | (LetT, _) :: (MutableT, _) :: (Ident name, _) :: (EqualsT, _) :: rest ->
          let node =
            { tokens = rest; nestedBlock = tn.nestedBlock }
          let expr, remaining' =
            parseNext (node :: remaining)
          (Let(name, true, expr), remaining')
      | (LetT, _) :: (Ident name, _) :: (EqualsT, _) :: (EOF, _) :: [] ->
          let expr = parseBlock tn.nestedBlock
          (Let(name, false, expr), remaining)
      | (LetT, _) :: (Ident name, _) :: (EqualsT, _) :: rest ->
          let node =
            { tokens = rest; nestedBlock = tn.nestedBlock }
          let expr, remaining' =
            parseNext (node :: remaining)
          (Let(name, false, expr), remaining')
      | (LetT, _) :: rest ->
          let (vars, rest') = parseParamList rest
          match vars with
          | name :: p1 :: prest ->
              let prms = p1 :: prest
              let fn =
                parseFunction name prms false rest' tn.nestedBlock
              (Fn(fn), remaining)
          | _ -> failwith "Expected params for function"
      | _ -> failwith "Unexpected token in let statement"

and parseFunction name prms isRecursive tokens block : Function =
  match prms with
  | p :: p' :: rest ->
      let fn =
        parseFunction name (p' :: rest) isRecursive tokens block
      let lambda =
        { prm = p; body = LambdaExpr(fn.lambda) }
      { fn with lambda = lambda }
  | p :: rest ->
      // TODO: handle tokens instesd of block
      let block' =
        match tokens with
        | [(EqualsT, _); (EOF, _)] ->
            parseBlock block
        | (EqualsT, _) :: rest' ->
            let node = { tokens = rest'; nestedBlock = block }
            parseBlock [node]
        | _ -> failwith "Expected '=' in function definition"
      let lambda = { prm = p; body = block' }
      { name = name; recursive = isRecursive; lambda = lambda }
  | _ -> failwith "Missing function parameters"

and parseWhile node : Expr =
  match node.tokens with
  | (WhileT, _) :: rest ->
      let (condExpr, afterCond) =
        parseExpr rest [] lowPrecedence
      match afterCond with
      | [(DoT, _); (EOF, _)] -> 
          let body = parseBlock node.nestedBlock
          While(condExpr, body)      
      | (DoT, _) :: rest' ->
          let (expr, _) =
            parseExpr rest' node.nestedBlock lowPrecedence
          While(condExpr, expr)
      | _ ->
          failwith "Expected 'do' after while condition"
  | _ -> failwith "Expected 'while'"

let rec parse (nodes: TokenNode list) : Expr list =
  match nodes with
  | [] -> []
  | _ ->
      let expr, remaining = parseNext nodes
      let rest = parse remaining
      expr :: rest

