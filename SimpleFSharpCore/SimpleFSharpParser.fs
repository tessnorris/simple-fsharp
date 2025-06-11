module SimpleFSharpCore.Parser

open SimpleFSharpCore.CoreTools
open SimpleFSharpCore.SyntaxTree

let lowPrecedence = 0      // anything should take precedence over this
let unaryPrecedence = 100  // should have max precedence
let applyPrecedence = 90   // should have precedence over all binary ops

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


and parseExpr (tokens: Token list) (minPrec: int) : Expr * Token list =
  let prefix, rest =
    match tokens with
    | Fun :: _ -> parseLambda tokens
    | IntT n :: rest -> (Lit (IntLit n), rest)
    | BoolT b :: rest -> (Lit (BoolLit b), rest)
    | StrT s :: rest -> (Lit (StringLit s), rest)
    | Ident s :: rest -> (Var s, rest)
    | Minus :: rest ->
        let expr, rest' = parseExpr rest unaryPrecedence
        (UnaryOp(Neg, expr), rest')
    | BooleanNot :: rest ->
        let expr, rest' = parseExpr rest unaryPrecedence
        (UnaryOp(BoolNot, expr), rest')
    | StringOpT :: rest ->
        let expr, rest' = parseExpr rest unaryPrecedence
        (UnaryOp(StringOp, expr), rest')
    | IntOpT :: rest ->
        let expr, rest' = parseExpr rest unaryPrecedence
        (UnaryOp(IntOp, expr), rest')
    | LParen :: rest ->
        let e, rest' = parseExpr rest lowPrecedence
        match rest' with
        | RParen :: tail -> (e, tail)
        | _ -> failwith "expected ')'"
    | InterpolatedStart :: StringStart :: rest -> parseInterpolatedString rest
    | token :: rest -> failwith $"Unexpected token ({token}) found when parsing expression"
    | _ -> failwith "No token found when parsing expression"

  parseInfixRest prefix rest minPrec

and parseInfixRest left tokens minPrec : Expr * Token list =
    match tokens with
    | [] -> (left, [])
    | token :: _ when isPrefixToken token ->
        let arg, rest' = parseExpr tokens applyPrecedence
        parseInfixRest (Apply(left, arg)) rest' minPrec
    | token :: rest ->
        match getBinOp token with
        | Some (op) when precedence op >= minPrec ->
            let prec = precedence op
            let nextMinPrec = prec + 1
            let right, rest' = parseExpr rest nextMinPrec
            let combined =   BinOp (op, left, right)
            parseInfixRest combined rest' minPrec
        | _ ->
            (left, tokens)

and parseLambda tokens =
  match tokens with
  | Fun :: Ident var :: Arrow :: rest ->
      let body, rest' = parseExpr rest lowPrecedence
      Lambda(var, body), rest'
  | _ -> failwith "Unexpected 'fun''"

and parseInterpolatedString (tokens: Token list) : Expr * Token list  =
  let rec loop (tokens: Token list) : InterpolatedSegment list * Token list =
    match tokens with
    | InterpolatedText s :: rest ->
        let (segments, remainder) = loop rest
        (StringSegment(s) :: segments, remainder)
    | InterpolatedExprStart :: rest ->
        let (expr, rest') = parseExpr rest lowPrecedence
        match rest' with
        | InterpolatedExprEnd :: rest'' ->
            let (segments, remainder) = loop rest''
            (InterpolatedExpr(expr) :: segments, remainder)
        | token :: _ -> failwith $"Unexpected token ({printToken token}) in interpolated string expression"
        | _ -> failwith "Unexpected termination in interpolated string expression"
    | StringEnd :: rest -> ([], rest)
    | token :: _ -> failwith $"Unexpected token ({printToken token}) in interpolated string"
    | _ -> failwith "Unexpected termination in interpolated string"
  let (segments, rest) = loop tokens
  (InterpolatedStr(segments), rest)

// fuck, these come ib as nodes
let rec parseStatement (tokens: Token list) : Expr * Token list =
  match tokens with
  | LetT :: _ -> parseLet tokens
  | If of Conditional list // if there:s no else, no val
  | While of Expr * Block
  | _ -> parseValueStatement tokens

let rec parseValueStatement (tokens: Token list) : Expr * Token list =
  match tokens with
  | Fn of Function
  | If of Conditional list * Block // must have else clause
  | _ -> parseExpr tokens lowPrecedence

and parseLet tokens =
  match tokens with
  | LetT :: Fun :: rest ->
      // parse list of.string fot variables
        // make sure they're not keuwords
      // then equals, then an expression or a block
  | LetT :: RecT :: Fun :: rest ->
      // 
  | LetT :: Ident name :: EqualsT :: rest ->
      let expr, remaining = parseExpr rest lowPrecedence
      Let(name, expr), remaining
  | _ -> failwith "Unexpected token in let statement"

and parseFunction tokens =
  name: string
  recursive: bool
  parameters: string list
  block: Block

and parseIf tokens =

and parseWhile tokens =

and parseBlock tokens =
  statements: Statement list

and parseConditional tokens =
  condition: Expr
  block: Block



let parse tokens pos =
  let (expr, rest) = parseStatement tokens
  match rest with
  | [EOF] -> expr
  | _ -> failwith $"unexpected trailing tokens on line {pos.line}"
