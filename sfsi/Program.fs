// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
open SimpleFSharpCore.CoreTools
open SimpleFSharpCore.Lexer
open SimpleFSharpCore.SyntaxTree
open SimpleFSharpCore.Parser
open SimpleFSharpCore.Environment
open SimpleFSharpCore.Eval

let rec repl env debugMode =
  printf ">>> "
  let input = System.Console.ReadLine()
  readMultiline [input] env debugMode

and readMultiline lines env debugMode =
  match lines with
  | line :: rest when line.TrimEnd().EndsWith(";;") ->
    let cleanedInput = line.Substring(0, line.Length - 2)
    if cleanedInput = "#quit" then
      ()
    else
      try
        let orderedLines = List.rev (cleanedInput :: rest)
        let nodes = tokenizeLines orderedLines
        if debugMode then
          printfn "Tokens:"
          printTokenNodes nodes 0
        let exprs = parse nodes
        if debugMode then
          printfn "SyntaxTree:"
          printBlock exprs 0
        let (result, env') =
          evalBlock exprs env
        printfn $"{valueToStr result}"
        repl env' debugMode
      with ex ->
        printfn "Error: %s" ex.Message
        repl env debugMode     
  | _ ->
    printf "- "
    let nextLine = System.Console.ReadLine()
    readMultiline (nextLine :: lines) env debugMode

[<EntryPoint>]
let main argv =
  printfn "Welcome to Tiny F# REPL"
  let debugMode = argv |> Array.contains "--debug"
  if debugMode then
    printfn "Running in debug mode"
  else
    printfn "#quit;; to quit"
  repl [Map.empty] debugMode
  0
