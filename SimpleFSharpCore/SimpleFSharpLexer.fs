// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCore.Lexer

open SimpleFSharpCore.CoreTools

let lexIdentifier input pos =
  let rec loop input pos =
    match input with
    | [] -> ([], [], pos)
    | c :: rest when isLetter c || isDigit c || c = '_' || c = '\'' || c = '?' ->
        let (ident, remainder, pos') = loop rest (advance pos)
        (c :: ident, remainder, pos')
    | _ -> ([], input, pos)

  let (chars, rest, pos') = loop input pos
  let ident = stringFromList chars
  let token =
    match ident with
    | "let" -> LetT
    | "fun" -> Fun
    | "not" -> BooleanNot
    | "true" -> BoolT true
    | "false" -> BoolT false
    | "string" -> StringOpT
    | "int" -> IntOpT
    | "if" -> IfT
    | "then" -> ThenT
    | "elif" -> ElIfT
    | "else" -> ElseT
    | "rec" -> RecT
    | "mutable" -> MutableT
    | "while" -> WhileT
    | "do" -> DoT
    | _ -> Ident ident
  ((token, pos), rest, pos')


let lexNumber input pos =
  let rec loop input pos =
    match input with
    | [] -> ([], [], pos)
    | c :: rest when isDigit c ->
        let (chars, remainder, pos') = loop rest (advance pos)
        (c :: chars, remainder, pos')
    | _ -> ([], input, pos)
  let (chars, rest, pos') = loop input pos
  let s = stringFromList chars
  ((IntT (int s), pos), rest, pos')


let lexString input pos =
  let rec loop input pos escaped =
    match input with
    | [] -> failwith $"Unterminated string literal '{stringFromList input}' at ${posString pos}"
    | c :: rest when escaped ->
        let esc =
          match c with
          | '"' -> '"'
          | '\\' -> '\\'
          | 'n' -> '\n'
          | 't' -> '\t'
          | other -> other
        let (chars, remainder, pos') = loop rest (advance pos) false
        (esc :: chars, remainder, pos')
    | c :: rest ->
        match c with
        | '\\' -> loop rest pos true
        | '"' -> ([], rest, pos)
        | _ ->
            let (chars, remainder, pos') = loop rest (advance pos) false
            (c :: chars, remainder, pos')
  let (chars, rest, pos') = loop input pos false
  let s = stringFromList chars
  ((StrT (s), pos), rest, pos')


let lexOperator input pos =
  let (op, rest, adv) =
    match input with
    | '<' :: '>' :: rest -> (NotEqualT, rest, 2)
    | '<' :: '=' :: rest -> (LessEqualT, rest, 2)
    | '>' :: '=' :: rest -> (GreaterEqualT, rest, 2)
    | '-' :: '>' :: rest -> (Arrow, rest, 2)
    | '=' :: '>' :: rest -> (FatArrow, rest, 2)
    | '<' :: '-' :: rest -> (ReverseArrow, rest, 2)
    | '&' :: '&' :: rest -> (BooleanAnd, rest, 2)
    | '|' :: '|' :: rest -> (BooleanOr, rest, 2)
    | '+' :: rest -> (Plus, rest, 1)
    | '-' :: rest -> (Minus, rest, 1)
    | '*' :: rest -> (Star, rest, 1)
    | '/' :: rest -> (Slash, rest, 1)
    | '%' :: rest -> (Percent, rest, 1)
    | '=' :: rest -> (EqualsT, rest, 1)
    | '>' :: rest -> (GreaterT, rest, 1)
    | '<' :: rest -> (LessT, rest, 1)
    | _ -> failwith $"Unrecornized operator at {posString pos}"
  ((op, pos), rest, (advanceN pos adv))


let rec skipComment input pos =
  match input with
  | [] -> ([], pos)
  | '\n' :: rest -> (rest, (advanceLine pos))
  |  _ :: rest -> skipComment rest (advance pos)


let readUntilSpecialChar input pos =
  let rec loop input pos =
    match input with
    | [] -> ([], [], pos)
    | c :: _ when c = '{' || c = '}' || c = '"' ->
        ([], input, pos)
    | '\\' :: '"' :: rest ->
        let (chars, remainder, pos') = loop rest (advanceN pos 2)
        ('"' :: chars, remainder, pos')
    | c :: rest ->
        let (chars, remainder, pos') = loop rest (advance pos)
        (c :: chars, remainder, pos')
  loop input pos


let rec tokenize (input: char list) (pos: Position) (interpolated: bool) : (Token * Position) list * char list * Position =
  let opChars = ['<'; '>'; '='; '+'; '-'; '*'; '/'; '%'; '&'; '|']
  match input with
  | [] -> ([(EOF, pos)], [], (advanceLine pos))
  | '/' :: '/' :: rest ->
      let (remainder, pos') = skipComment input pos
      tokenize remainder pos' interpolated
  | c :: _ when List.contains c opChars -> processToken lexOperator input pos interpolated
  | c :: _ when isDigit c -> processToken lexNumber input pos interpolated
  | c :: _ when isLetter c -> processToken lexIdentifier input pos interpolated
  | '"' :: rest -> processToken lexString rest pos interpolated
  | c :: rest when isWhitespace c ->
      tokenize rest (advance pos) interpolated
  | '(' :: ')' :: rest ->
     let (tokens, remainder, pos') =
       tokenize rest (advanceN pos 2) interpolated
     ((UnitT, pos) :: tokens, remainder, pos')
  | '(' :: rest ->
     let (tokens, remainder, pos') =
       tokenize rest (advance pos) interpolated
     ((LParen, pos) :: tokens, remainder, pos')
  | ')' :: rest ->
      let (tokens, remainder, pos') =
        tokenize rest (advance pos) interpolated
      ((RParen, pos) :: tokens, remainder, pos')
  | '$' :: '"' :: rest when not interpolated ->
      let (interpol, remainder, pos') = lexInterpolatedString rest (advanceN pos 2)
      let (tokens, remainder, pos'') = tokenize remainder pos' interpolated
      ((InterpolatedStart, pos) :: (StringStart, advance pos) :: interpol @ tokens, remainder, pos'')
  | '}' :: rest when interpolated ->
      ([], rest, (advance pos))
  | c :: _ -> failwith $"Lexing error with char {c} at {posString pos}"


and processToken f input pos interpolated =
  let (token, rest, pos') = f input pos
  let (tokens, remainder, pos'') = tokenize rest pos' interpolated
  (token :: tokens, remainder, pos'')


and lexInterpolatedString input pos =
  match input with
  | [] -> failwith $"Unexpected EOF in interpolated string at {posString pos}"
  | '"' :: rest -> ([(StringEnd, pos)], rest, (advance pos))
  | '{' :: rest ->
      let (exprTokens, rest', pos') = tokenize rest (advance pos) true
      let (moreTokens, remainder, pos'') = lexInterpolatedString rest' pos'
      (((InterpolatedExprStart, pos) :: exprTokens) @ ((InterpolatedExprEnd, pos') :: moreTokens), remainder, pos'')
  | '}' ::_ ->
      failwith $"Unexpected closing brace in interpolated string at {posString pos}"
  | c :: rest ->
      let (chars, rest', pos') = readUntilSpecialChar input pos
      let (tokens, remainder, pos'') = lexInterpolatedString rest' pos'
      ((InterpolatedText(stringFromList chars), pos') :: tokens, remainder, pos'')


let tokenizeString (input: string) (pos: Position) : (Token * Position) list * Position * int =
  let indent = getIndent input
  let (tokens, _, pos') = tokenize (stringToList input) pos false
  (tokens, pos', indent)

let rec tokenizeStringList (input: string list) (pos: Position) (indent: int) : TokenNode list * string list * Position * int =
  match input with
  | [] -> ([], [], pos, indent)
  | inputLine :: rest ->
      let lineIndent = getIndent inputLine
      if lineIndent < indent then
        // this is an outdent node, don't tokenize it yet
        ([], input, pos, indent)
      else
        let (tokens, pos', _) =
          tokenizeString inputLine pos
        match rest with
        | [] ->
            ([{ tokens = tokens; nestedBlock = [] } ],
             [], pos',
             lineIndent)
        | nextLine :: rest' ->
            let nextIndent = getIndent nextLine
            if nextIndent > lineIndent then
              let (childNodes, rest'', pos'', _) =
                tokenizeStringList rest pos' nextIndent
              let node =
                { tokens = tokens;
                  nestedBlock = childNodes }
              let (nodes, rest''', pos''', _) =
                tokenizeStringList rest'' pos'' lineIndent
              (node :: nodes, rest''', pos''', lineIndent)
            else
              let node =
                { tokens = tokens; nestedBlock = [] }
              let (nodes, rest'', pos'', _) =
                tokenizeStringList rest pos' lineIndent
              (node :: nodes, rest'', pos'', lineIndent)

let tokenizeLines (input: string list) : TokenNode list =
  let (nodes, _, _, _) = tokenizeStringList
                           input
                           { line = 0; column = 0 }
                           0
  nodes



