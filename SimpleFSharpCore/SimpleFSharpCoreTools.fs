// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCore.CoreTools

type Token =
  | IntT of int
  | BoolT of bool
  | StrT of string
  | Ident of string
  | InterpolatedStart   // like a $"... start
  | StringStart
  | InterpolatedText of string  // flat text between exprs
  | InterpolatedExprStart // {
  | InterpolatedExprEnd   // }
  | StringEnd
  | Plus
  | Minus
  | Star
  | Slash
  | Percent
  | EqualsT
  | GreaterT
  | LessT
  | NotEqualT
  | LessEqualT
  | GreaterEqualT
  | LParen
  | RParen
  | LetT
  | Fun
  | EOF
  | StringOpT
  | IntOpT
  | Arrow
  | FatArrow
  | ReverseArrow
  | BooleanNot
  | BooleanAnd
  | BooleanOr
  | IfT
  | ThenT
  | ElIfT
  | ElseT
  | RecT
  | MutableT
  | WhileT
  | DoT

type Position = {
  line: int
  column: int
}

type TokenNode = {
  tokens: (Token * Position) list
  nestedBlock: TokenNode list
}

let newPosition () =
  { line = 1; column = 0 }

let advance pos =
  { pos with column = pos.column + 1 }

let advanceN pos n =
  { pos with column = pos.column + n }

let advanceLine pos =
  { pos with column = 0; line = pos.line + 1 }

let posString pos =
  $"line {pos.line}, col {pos.column}"

let getIndent line =
  line
  |> Seq.takeWhile (fun c -> c = ' ' || c = '\t')
  |> Seq.fold (fun acc c -> if c = ' ' then acc + 1 elif c = '\t' then acc + 4 else acc) 0

let isLetter c = System.Char.IsLetter(c)
let isDigit c = System.Char.IsDigit(c)
let isWhitespace c = System.Char.IsWhiteSpace(c)

let stringFromList (cl: char list) : string =
  new string (List.toArray cl)

let stringToList (s: string): char list =
  s |> Seq.toList

let printToken token =
  match token with
  | IntT i -> $"IntT {i}"
  | BoolT b -> $"BoolT {b}"
  | StrT s -> $"StrT {s}"
  | Ident s -> $"Ident {s}"
  | InterpolatedStart -> "InterpolatedStart"
  | StringStart -> "StringStart"
  | InterpolatedText s -> $"InterpolatedText {s}"
  | InterpolatedExprStart -> "InterpolatedExprStart"
  | InterpolatedExprEnd -> "InterpolatedExprEnd"
  | StringEnd -> "StringEnd"
  | Plus -> "Plus"
  | Minus -> "Minus"
  | Star -> "Star"
  | Slash -> "Slash"
  | Percent -> "Percent"
  | EqualsT -> "Equals"
  | GreaterT -> "Greater"
  | LessT -> "Less"
  | LParen -> "LParen"
  | RParen -> "RParen"
  | LetT -> "LetT"
  | Fun -> "Fun"
  | EOF -> "EOF"
  | StringOpT -> "StringOp"
  | IntOpT -> "IntOp"
  | NotEqualT -> "NotEqual"
  | LessEqualT -> "LessEqual"
  | GreaterEqualT -> "GreaterEqual"
  | Arrow -> "Arrow"
  | FatArrow -> "FatArrow"
  | ReverseArrow -> "ReverseArrow"
  | BooleanNot -> "BooleanNot"
  | BooleanAnd -> "BooleanAnd"
  | BooleanOr -> "BooleanOr"
  | IfT -> "IfT"
  | ThenT -> "ThenT"
  | ElIfT -> "ElifT"
  | ElseT -> "ElseT"
  | RecT -> "RecT"
  | MutableT -> "MutableT"
  | WhileT -> "WhileT"
  | DoT -> "DoT"

let printTokens tokens =
  let rec loop tokens =
    match tokens with
    | [] -> ()
    | (token, _) :: rest ->
        printf $"{printToken token}, "
        loop rest

  loop tokens
  printfn ""

let rec printTokenNode node depth =
  let indent = String.replicate depth "  "
  printf $"{indent}"
  printTokens node.tokens
  printTokenNodes node.nestedBlock (depth + 1)

and printTokenNodes nodes depth =
  match nodes with
  | [] -> ()
  | node :: rest ->
      printTokenNode node depth
      printTokenNodes rest depth
