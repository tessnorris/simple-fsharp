// For more information see https://aka.ms/fsharp-console-apps
open System
open SimpleFSharpCore.CoreTools
open SimpleFSharpCore.Lexer
open SimpleFSharpCore.Parser
open SimpleFSharpCompiler.Tools
open SimpleFSharpCompiler.Transformer
open SimpleFSharpCompiler.Compiler

type CompilerOptions = {
    Debug: bool
    Filename: string option
    OutputFilename: string option
  }

let writeToFile (path: string) (contents: string) =
    use writer = new System.IO.StreamWriter(path)
    writer.Write(contents)

let addString g s =
  { g with Strings = Map.add s g.Ctr g.Strings; Ctr = g.Ctr + 1 }

let rec processLine source ctx g debugMode pos =
  try
    match source with
    | sourceLine :: tail ->
        if debugMode then
          printfn "Source line: %A" sourceLine

        printfn $"Tokenizing {posString pos}"
        let (tokens, _, pos') = tokenizeString sourceLine pos
        if debugMode then
          printfn "Tokens: %A" tokens

        // let expr, _ = parseExpr tokens
        let expr, _ = parse tokens pos
        if debugMode then
          printfn "Parse Tree:"
          printExpr expr 0

        let (ctx', g', tfsExpr) = lower ctx g expr
        if debugMode then
          printfn "Context: %A" ctx'
          printfn "TfsIr:"
          printTfsExpr tfsExpr 0
          printfn ""

        match processLine tail ctx' g' debugMode pos' with
        | (0, None) -> (0, Some(ctx', g', [tfsExpr]))
        | (0, Some(ctx'', g'', exprList)) ->
            (0, Some(ctx'', g'', tfsExpr :: exprList))
        | (n, _) -> (n, None)

    | _ -> (0, None)
  with e ->
    printfn "Error: %s" e.Message
    (1, None)


let compile (input: string) (options: CompilerOptions) =
  printfn "tfsc executing"

  let pos = newPosition ()

  let lines =
    input.Split([| '\r'; '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.toList

  let emptyCtx : LoweringContext = {
    Env = Map.empty
    Scope = Set.empty
    Captured = Set.empty
  }

  let emptyGlobals : Globals = {
    Strings = Map.empty
    Lambdas = []
    Ctr = 1
  }
  let g1 = addString emptyGlobals "true"
  let g2 = addString g1 "false"
  let g3 = addString g2 "%d"
  let g4 = addString g3 "\n"

  printfn "\nIR:"
  match (processLine lines emptyCtx g4 options.Debug pos) with
  | (_, Some(ctx', g5, exprList)) ->
      let generatedIR = new System.Text.StringBuilder()

      // get all the string literals
      let stringsOutput = emitStrings g5.Strings
      List.map (fun (s: string) ->
        generatedIR.Append(s) |> ignore
        printf $"{s}") stringsOutput
      |> ignore

      // get all the external functions
      let fns = emitExternalFunctions ()
      printf $"{fns}\n"
      generatedIR.Append(fns) |> ignore

      // TODO: get all the functions

      // get the main function body
      let main = emitMain ctx' g5 exprList
      printf $"{main}"
      generatedIR.Append(main) |> ignore

      // write file
      match options.OutputFilename with
      | Some (filename) ->
          writeToFile filename (generatedIR.ToString())
          0
      | None ->
          0
  | (n, None) ->
      printfn $"No output with return code: {n}"
      n



let parseArgs (argv: string[]) =
    let rec loop args opts =
        match args with
        | [] -> opts
        | "-debug" :: rest -> loop rest { opts with Debug = true }
        | filename :: rest when filename.EndsWith(".fs") ->
            loop rest { opts with Filename = Some filename }
        | "-o" :: filename :: rest ->
            loop rest { opts with OutputFilename = Some filename }
        | unknown :: _ ->
            eprintfn $"Unrecognized option: {unknown}"
            Environment.Exit(1)
            opts

    loop (Array.toList argv) { Debug = false; Filename = None; OutputFilename = None }


[<EntryPoint>]
let main argv =
    let options = parseArgs argv

    let input =
        match options.Filename with
        | Some filename -> System.IO.File.ReadAllText(filename)
        | None ->
            printfn "Reading from stdin..."
            System.Console.In.ReadToEnd()

    if options.Debug then
        printfn "Debug mode ON"
        printfn "Raw source:\n%s" input

    // returns 0 if successful compile, 1 on compile error
    compile input options


