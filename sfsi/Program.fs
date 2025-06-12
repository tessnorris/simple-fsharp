// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
open SimpleFSharpCore.CoreTools
open SimpleFSharpCore.Lexer
open SimpleFSharpCore.SyntaxTree
open SimpleFSharpCore.Parser
open SimpleFSharpCore.Eval

let rec loop env debugMode =
  printf "tfsi> "
  let input = System.Console.ReadLine()
  match input with
  | ":q" -> 0
  | _ ->
      try
        let pos = newPosition ()
        let (tokens, _) = tokenizeString input pos
        if debugMode then
          printfn "Tokens: %A" tokens
        let expr = parse tokens pos
        if debugMode then
          printfn "Expression:"
          printExpr expr 0
        let result, newEnv = eval expr env
        match result with
        | IntV n -> printfn $"{n}"
        | BoolV b -> printfn $"{b}"
        | StrV s -> printfn $"\"{s}\""
        | Closure (args, body, env') ->
            printfn "Closure with"
            printfn "  args: %A" args
            printfn "  body: %A" body
            printfn "  env keys: %A" (Map.toList env' |> List.map fst)
        loop newEnv debugMode
      with e ->
        printfn "Error: %s" e.Message
        loop env debugMode


[<EntryPoint>]
let main argv =
  printfn "Welcome to Tiny F# REPL"
  let debugMode = argv |> Array.contains "-debug"
  if debugMode then
    printfn "Running in debug mode"
  else
    printfn ":q to quit"
  loop Map.empty debugMode
