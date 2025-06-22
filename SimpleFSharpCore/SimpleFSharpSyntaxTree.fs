// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCore.SyntaxTree

open SimpleFSharpCore.CoreTools

// operators
type BinOpNumKind = Sub | Mul | Div | Mod
type BinOpBoolKind = BoolOr | BoolAnd
type BinOpComparKind =
  | Equals
  | Greater
  | Less
  | NotEqual
  | LessEqual
  | GreaterEqual

type BinOpKind =
  | OpNum of BinOpNumKind
  | OpBool of BinOpBoolKind
  | OpCompar of BinOpComparKind
  | Add                             // could be numeric or string concat

type UnaryOpKind = Neg | BoolNot | StringOp | IntOp

let getBinOp (token : Token) : BinOpKind Option =
  match token with
  | Plus -> Some(Add)
  | Minus -> Some(OpNum Sub)
  | Star -> Some(OpNum Mul)
  | Slash -> Some(OpNum Div)
  | Percent -> Some(OpNum Mod)
  | EqualsT -> Some(OpCompar Equals)
  | NotEqualT -> Some(OpCompar NotEqual)
  | LessT -> Some(OpCompar Less)
  | GreaterT -> Some(OpCompar Greater)
  | LessEqualT -> Some(OpCompar LessEqual)
  | GreaterEqualT -> Some(OpCompar GreaterEqual)
  | BooleanOr -> Some(OpBool BoolOr)
  | BooleanAnd -> Some(OpBool BoolAnd)
  | _ -> None

let getUnaryOp (token : Token) : UnaryOpKind Option =
  match token with
  | Minus -> Some(Neg)
  | BooleanNot -> Some(BoolNot)
  | StringOpT -> Some(StringOp)
  | IntOpT -> Some(IntOp)
  | _ -> None


type LiteralValue =
  | IntLit of int
  | StringLit of string
  | BoolLit of bool


// AST expression
type Expr =
  | Lit of LiteralValue
  | InterpolatedStr of InterpolatedSegment list
  | Var of string
  | UnaryOp of UnaryOpKind * Expr
  | BinOp of BinOpKind * Expr * Expr
  | LambdaExpr of Lambda
  | IfEx of ConditionalExpr list * ExprBlock
  | Apply of Expr * Expr

and Lambda = {
  prm: string
  body: ExprBlock
}

and InterpolatedSegment =
  | InterpolatedExpr of Expr
  | StringSegment of string


// AST containers
and Statement =
  | Let of string * bool * ExprBlock
  | MutableAssign of string * ExprBlock
  | Fn of Function
  | IfSt of ConditionalStmt list * StatmtBlock Option
  | While of Expr * StatmtBlock
  | Expression of Expr

and Function = {
  name: string
  recursive: bool
  lambda: Lambda
}

and ExprBlock = {
  statements: Statement list
  expr: Expr
}

and StatmtBlock = {
  statements: Statement list
}

and ConditionalExpr = {
  condition: Expr
  eblock: ExprBlock
}

and ConditionalStmt = {
  condition: Expr
  sblock: StatmtBlock
}

// Printing the AST
let rec printBinOp op =
  match op with
  | OpNum op' ->
      match op' with
      | Sub -> "-"
      | Mul -> "*"
      | Div -> "/"
      | Mod -> "%"
  | OpBool op' ->
      match op' with
      | BoolOr -> "||"
      | BoolAnd -> "&&"
  | OpCompar op' ->
      match op' with
      | Equals -> "="
      | Greater -> ">"
      | Less -> "<"
      | NotEqual -> "<>"
      | LessEqual -> "<="
      | GreaterEqual -> ">="
  | Add -> "+"


let rec printUnaryOp op =
  match op with
  | Neg -> "-"
  | BoolNot -> "not"
  | StringOp -> "string"
  | IntOp -> "int"

let rec printExpr expr depth =
  let indent = String.replicate depth "  "
  match expr with
  | Lit (IntLit i) -> printfn $"{indent}Int {i}"
  | Lit (BoolLit b) -> printfn $"{indent}Bool {b}"
  | Lit (StringLit s) -> printfn $"{indent}String: \"{s}\""
  | InterpolatedStr segments ->
      printfn $"{indent}InterpolatedString"
      printInterpolatedSegments segments (depth + 1)
  | Var v -> printfn $"{indent}Var: {v}"
  | UnaryOp (op, ex) ->
      printfn $"{indent}UnaryOp: {printUnaryOp op}"
      printExpr ex (depth + 1)
  | BinOp (op, ex1, ex2) ->
      printfn $"{indent}BinOp: {printBinOp op}"
      printExpr ex1 (depth + 1)
      printExpr ex2 (depth + 1)
  | LambdaExpr { prm = arg; body = eb } ->
      printfn $"{indent}Lambda: {arg}"
      printExprBlock eb.statements eb.expr (depth + 1)
  | IfEx (conds, eb) ->
      printfn "If"
      printConditionalExpr conds true depth
      printfn "Else"
      printExprBlock eb.statements eb.expr (depth + 1)
  | Apply (fn, param) ->
      printfn $"{indent}Apply:"
      printExpr fn (depth + 1)
      printExpr param (depth + 1)

and printInterpolatedSegments segments depth =
  let indent = String.replicate depth "  "
  match segments with
  | [] -> ()
  | InterpolatedExpr ex :: rest ->
      printfn $"{indent}Interpolated Expression:"
      printExpr ex (depth + 1)
      printInterpolatedSegments rest depth
  | StringSegment s :: rest ->
      printfn $"{indent}Interpolated String Segment: {s}"
      printInterpolatedSegments rest depth

and printStatement statement depth =
  let indent = String.replicate depth "  "
  match statement with
  | Let (var, mut, eblk) ->
      if mut then
        printfn $"{indent}Let (mutable): {var}"
      else
        printfn $"{indent}Let: {var}"
      printExprBlock eblk.statements eblk.expr (depth + 1)
  | MutableAssign (var, eblk) ->
      printfn $"{indent}Mutable assign: {var}"
      printExprBlock eblk.statements eblk.expr (depth + 1)
  | Fn f ->
      printfn $"{indent}Function: {f.name}"
      printFunction f (depth + 1)
  | IfSt (conds, maybeElse) ->
      printfn $"{indent}If:"
      printConditionalStmt conds true depth
      match maybeElse with
      | Some(b) ->
          printfn $"{indent}Else:"
          printStmtBlock b (depth + 1)
      | None -> ()
  | While (expr, b) ->
      printfn $"{indent}While:"
      printWhile expr b depth
  | Expression expr ->
      printfn $"{indent}Expression:"
      printExpr expr (depth + 1)

and printFunction f depth =
  let indent = String.replicate depth "  "
  printf $"{indent}Recursive: {f.recursive}"
  printfn $"{indent}Param: {f.lambda.prm}"
  printf $"{indent}Body:"
  printExprBlock f.lambda.body.statements f.lambda.body.expr (depth + 1)

and printStatements sts depth =
  match sts with
  | [] -> ()
  | st :: rest ->
      printStatement st depth
      printStatements rest depth

and printStmtBlock block depth =
  printStatements block.statements depth

and printExprBlock statements expr depth =
  printStatements statements depth
  printExpr expr depth

and printConditionalStmt conds first depth =
  match conds with
  | [] -> ()
  | cond :: rest ->
      let indent = String.replicate depth "  "
      if first then () else printfn $"{indent}Elif:"
      printExpr cond.condition (depth + 1)
      printfn "Then:"
      printStmtBlock cond.sblock (depth + 1)
      printConditionalStmt rest false depth

and printConditionalExpr conds first depth =
  match conds with
  | [] -> ()
  | cond :: rest ->
      let indent = String.replicate depth "  "
      if first then () else printfn $"{indent}Elif:"
      printExpr cond.condition (depth + 1)
      printfn "Then:"
      printExprBlock cond.eblock.statements cond.eblock.expr (depth + 1)
      printConditionalExpr rest false depth

and printWhile expr block depth =
      let indent = String.replicate depth "  "
      printExpr expr (depth + 1)
      printfn "Do:"
      printStmtBlock block (depth + 1)
