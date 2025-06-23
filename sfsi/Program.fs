// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
open SimpleFSharpCore.CoreTools
open SimpleFSharpCore.Lexer
open SimpleFSharpCore.SyntaxTree
open SimpleFSharpCore.Parser
open SimpleFSharpCore.Inferrer
open SimpleFSharpCore.Environment
open SimpleFSharpCore.Eval

let mutable globalEnv : TypeEnv = Map.empty

let inferAndUpdate expr : Type =
  let subst, typ = infer globalEnv expr

  // Collect new bindings from block expressions
  let updatedEnv =
    match expr with
    | Block exprs ->
        let rec updateEnv env = function
          | [] -> env
          | Let(name, _, boundExpr)::rest ->
              let s, t = infer env boundExpr
              let generalized = generalize env t
              let env' =
                Map.map (fun _ scheme -> applyToScheme s scheme) env
                |> Map.add name generalized
              updateEnv env' rest
          | Fn fn :: rest ->
              let tvArg = freshTVar ()
              let tvRet = freshTVar ()
              let tvFn = TFun(tvArg, tvRet)
              let scheme = ([], tvFn)

              let env' =
                if fn.recursive then
                  Map.add fn.name scheme env
                else env

              let env'' = Map.add fn.lambda.prm ([], tvArg) env'

              let s, tBody = infer env'' fn.lambda.body
              let finalType = TFun(apply s tvArg, tBody)
              let generalized = generalize env finalType

              let newEnv =
                Map.map (fun _ scheme -> applyToScheme s scheme) env
                |> Map.add fn.name generalized

              updateEnv newEnv rest
          | _ :: rest -> updateEnv env rest
        updateEnv globalEnv exprs
    | _ -> globalEnv // Expressions don’t define new bindings

  globalEnv <- updatedEnv
  typ

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

        let expr : Expr = Block exprs
        let typ = inferAndUpdate expr
        if debugMode then
          printfn $"Inferred type: {printType typ}"

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
