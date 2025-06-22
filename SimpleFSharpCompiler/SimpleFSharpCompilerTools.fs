// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCompiler.Tools

open System.Text

type TfsValue =
  | VInt of int
  | VBool of bool
  | VStr of int * int
  | VVar of string
  | VFunc of TfsFunction

and TfsExpr =
  | Const of TfsValue * int                  // literal value and register
  | Load of string * int                     // load a var into a register
  | Store of string * TfsExpr                // store result of expression to a var
  | Unary of string * TfsExpr * int          // unary op, e.g. "not", "-", store to register
  | Bin of string * TfsExpr * TfsExpr * int  // binary op, e.g. "+", "==", etc, store to register
  | Call of TfsFunction * TfsExpr list * int // call function, store to register
  | MakeClosure of int * TfsExpr list        // create closure w/ id for lambda and list of captured values
  | GetClosureEnv of int                     // load from closure env (e.g. slot 0 = s2)

and TfsFunction =
  | FLambda of int           // Lambda by identifier
  | FUser of string          // Usser function by name -- not implemented yet
  | FSystem of string        // system functions -- just printfn right now
  | FDynamic of TfsExpr      // evaluate expression to get fn to call

type LoweringContext = {
  Env: Map<string, int> // captured values (with full IR expressions)
  Scope: Set<string> // local vars in this scope
  Captured: Set<string>
}

type LambdaDef = {
  Args: string list
  Captured: string list
  Body: TfsExpr
}

type Globals = {
  Strings: Map<string,int>          // all the string literals in the program with a unique numeric identifier
  Lambdas: (LambdaDef * int) list   // all the lambdas in the program
  Ctr: int                          // unique counter for qnique identifiers for registers, strings, lambdas
}

// helper function that gets called a bit
let freshId globals =
  { globals with Ctr = globals.Ctr + 1 }


type IRBuilder = StringBuilder

let getIRConstructor () : IRBuilder =
    StringBuilder()

let emitIR (b: IRBuilder) (line: string) =
    b.AppendLine(line) |> ignore

let getIRText (b: IRBuilder) : string =
    b.ToString()


let makeIndent depth =
  String.replicate depth "  "

let getUid () =
  System.Guid.NewGuid().ToString("N")

let escapeLLVMString (s: string) =
    let sb = System.Text.StringBuilder()
    for c in s do
        match c with
        | '\\' -> sb.Append("\\5C") |> ignore  // backslash
        | '\"' -> sb.Append("\\22") |> ignore  // double quote
        | '\n' -> sb.Append("\\0A") |> ignore  // newline
        | '\t' -> sb.Append("\\09") |> ignore  // tab
        | '\r' -> sb.Append("\\0D") |> ignore
        | '\000' -> sb.Append("\\00") |> ignore
        | _ when System.Char.IsControl(c) || int c > 126 ->
            sb.Append(sprintf "\\%02X" (int c)) |> ignore
        | _ -> sb.Append(c) |> ignore
    sb.Append("\\00") |> ignore  // Null terminator
    sb.ToString()

let concatStrings ir s1ptr s2ptr outPtr =
  // Get length of string1
  let len1 = getUid ()
  emitIR ir $"%%u{len1} = call i64 @strlen(i8* %%{s1ptr})"

  // Get length of string2
  let len2 = getUid ()
  emitIR ir $"%%u{len2} = call i64 @strlen(i8* %%{s2ptr})"

  // Compute total length (+1 for null)
  let total1 = getUid ()
  let total2 = getUid ()
  emitIR ir $"%%u{total1} = add i64 %%u{len1}, %%u{len2}"
  emitIR ir $"%%u{total2} = add i64 %%u{total1}, 1"

  // Allocate space
  emitIR ir $"%%{outPtr} = call i8* @malloc(i64 %%u{total2})"

  // Copy first string
  emitIR ir $"call void @llvm.memcpy.p0i8.p0i8.i64("
  emitIR ir $"    i8* %%{outPtr},"
  emitIR ir $"    i8* %%{s1ptr},"
  emitIR ir $"    i64 %%u{len1},"
  emitIR ir $"    i1 false"
  emitIR ir $")"

  // Compute dest pointer for 2nd copy
  let dest2 = getUid ()
  emitIR ir  $"%%u{dest2} = getelementptr i8, i8* %%{outPtr}, i64 %%u{len1}"

  // Copy second string
  emitIR ir $"call void @llvm.memcpy.p0i8.p0i8.i64("
  emitIR ir $"    i8* %%u{dest2},"
  emitIR ir $"    i8* %%{s2ptr},"
  emitIR ir $"    i64 %%u{len2},"
  emitIR ir $"    i1 false"
  emitIR ir $")"

  // Null terminate
  let endptr = getUid ()
  emitIR ir $"%%u{endptr} = getelementptr i8, i8* %%{outPtr}, i64 %%u{total2}"
  emitIR ir $"store i8 0, i8* %%u{endptr}"


let rec printTfsExpr expr d =
  let indent = makeIndent d
  match expr with
  | Const (v, r) ->
      printfn $"{indent}Const: to {r}"
      match v with
      | VInt i -> printfn $"{makeIndent (d+1)}VInt: {i}"
      | VBool b -> printfn $"{makeIndent (d+1)}VBool: {b}"
      | VStr (sid, len) -> printfn $"{makeIndent (d+1)}VStr, id: {sid}, len: {len}"
      | VVar s -> printfn $"{makeIndent (d+1)}VVar: {s}"
      | VFunc f ->
          printfn $"{makeIndent (d+1)}VFunc:"
          match f with
          | FLambda id -> printfn $"{makeIndent (d+2)}FLambda: {id}"
          | FUser s -> printfn $"{makeIndent (d+2)}FUser: {s}"
          | FSystem s -> printfn $"{makeIndent (d+2)}FSystem: {s}"
          | FDynamic ex ->
              printfn $"{makeIndent (d+2)}FDynamic:"
              printTfsExpr ex (d + 3)
  | Load (v, r) -> printfn $"{indent}Load: {v} to {r}"
  | Store (v, ex) ->
      printfn $"{indent}Store: \"{v}\""
      printTfsExpr ex (d + 1)
  | Unary (op, ex, r) ->
      printfn $"{indent}UnaryOp: {op} to {r}"
      printTfsExpr ex (d + 1)
  | Bin (op, ex1, ex2, r) ->
      printfn $"{indent}BinOp: {op} to {r}"
      printTfsExpr ex1 (d + 1)
      printTfsExpr ex2 (d + 1)
  | Call (f, exps, r) ->
      printfn $"{indent}Call: to {r}"
      printfn $"{makeIndent (d+1)}VFunc:"
      match f with
      | FLambda id -> printfn $"{makeIndent (d+2)}FLambda: {id}"
      | FUser s -> printfn $"{makeIndent (d+2)}FUser: {s}"
      | FSystem s -> printfn $"{makeIndent (d+2)}FSystem: {s}"
      | FDynamic ex ->
          printfn $"{makeIndent (d+2)}FDynamic:"
          printTfsExpr ex (d + 3)
      List.iter (fun ex -> printTfsExpr ex (d + 1)) exps
  | MakeClosure (l, vals) ->
      printfn $"{indent}MakeClosure: Lambda {l}"
      List.iter (fun ex -> printTfsExpr ex (d + 1)) vals
  | GetClosureEnv reg ->
      printfn $"{indent}GetClosureEnv: {reg}"
  | _ -> failwith "Unimplemented expression type"
