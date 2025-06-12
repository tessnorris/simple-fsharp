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
  | Lambda of string * Expr
  | Apply of Expr * Expr

and InterpolatedSegment =
  | InterpolatedExpr of Expr
  | StringSegment of string


// AST containers
type Statement =
  | Fn of Function
  | If of Conditional list * Block Option
  | Let of string * Statement
  | Expression of Expr
  | While of Expr * Block

and Function = {
  name: string
  recursive: bool
  parameters: string list
  block: Block
}

and Block = {
  statements: Statement list
}

and Conditional = {
  condition: Expr
  block: Block
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
  | Lambda (var, ex) ->
      printfn $"{indent}Lambda: {var}"
      printExpr ex (depth + 1)
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

let rec printFunctionParameters vars =
  match vars with
  | [] -> ""
  | var :: rest -> var + " " + printFunctionParameters rest

let rec printStatement statement depth =
  let indent = String.replicate depth "  "
  match statement with
  | Fn f ->
      printfn $"{indent}Function: {f.name}"
      printFunction f (depth + 1)
  | If (conds, maybeElse) ->
      printfn $"{indent}If:"
      printConditional conds true depth
      match maybeElse with
      | Some(b) ->
          printfn $"{indent}Else:"
          printBlock b (depth + 1)
      | None -> ()
  | Let (var, statement) ->
      printfn $"{indent}Let: {var}"
      printStatement statement (depth + 1)
  | Expression expr ->
      printfn $"{indent}Expression:"
      printExpr expr (depth + 1)
  | While (expr, b) ->
      printfn $"{indent}While:"
      printWhile expr b (depth + 1)

and printFunction f depth =
  let indent = String.replicate depth "  "
  printf $"{indent}Recursive: {f.recursive}"
  printf $"{indent}Parameters: {printFunctionParameters f.parameters}"
  printf $"{indent}Body:"
  printBlock f.block (depth + 1)

and printStatements sts depth =
  match sts with
  | [] -> ()
  | st :: rest ->
      printStatement st depth
      printStatements rest depth

and printBlock block depth =
  let indent = String.replicate depth "  "
  printStatements block.statements depth

and printConditional conds first depth =
  match conds with
  | [] -> ()
  | cond :: rest ->
      let indent = String.replicate depth "  "
      if first then () else printfn $"{indent}Elif:"
      printExpr cond.condition (depth + 1)
      printfn "Then:"
      printBlock cond.block (depth + 1)

and printWhile expr block depth =
      let indent = String.replicate depth "  "
      printExpr expr (depth + 1)
      printfn "Do:"
      printBlock block (depth + 1)
