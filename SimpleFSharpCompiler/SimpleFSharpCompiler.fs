// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCompiler.Compiler

open SimpleFSharpCompiler.Tools
open SimpleFSharpCompiler.Transformer

type VariableT =
  | VarInt
  | VarBool
  | VarStr
  | VarFunc

type Register =
  | RInt of int             // register
  | RBool of int            // register
  | RStr of int * int       // register * length
  | RFunc of int * string   // register * name
  | RNil                    // no return value

let getRegId reg =
  match reg with
  | RInt n -> n
  | RBool n -> n
  | RStr (n, _) -> n
  | RFunc (n, _) -> n
  | RNil -> failwith "Tried to get register id on RNil register"

// @.str = private unnamed_addr constant [14 x i8] c"Hello world!\0A\00", align 1
let emitString s i =
  let len = String.length s + 1
  let es = escapeLLVMString s
  $"@.str{i} = private unnamed_addr constant [{len} x i8] c\"{es}\", align 1\n\n"

let emitStrings (strings: Map<string,int>) =
  strings
  |> Map.toList
  |> List.map (fun (s, i) -> emitString s i)

let emitExternalFunctions () : string =
  let ir = getIRConstructor ()
  emitIR ir "declare i32 @printf(i8*, ...)"
  emitIR ir "declare i32 @sprintf(i8*, i8*, ...)"
  emitIR ir "declare i64 @strlen(i8* nocapture) nounwind readonly"
  emitIR ir "declare i8* @malloc(i64)"
  emitIR ir "declare void @llvm.memcpy.p0i8.p0i8.i64(i8* nocapture writeonly, i8* nocapture readonly, i64, i1)"
  getIRText ir

let rec emitConst v reg ctx g vars =
  match v with
  | VInt n -> ($"%%r{reg} = add i32 {n}, 0", RInt(reg), vars)
  | VBool b ->
      let i = if b then 1 else 0
      ($"%%r{reg} = add i1 {i}, 0", RBool(reg), vars)
  | VStr (sid, len) -> //failwith "VStr Not yet implemented"
      ($"%%r{reg} = getelementptr inbounds [{len} x i8], [{len} x i8]* @.str{sid}, i32 0, i32 0", RStr(reg, len), vars)
  | VVar name -> failwith "VVar Not yet implemented"
  | VFunc fn -> failwith "VDunc Not yet implemented"

let rec emitLoad v reg ctx g vars =
  match Map.tryFind v vars with
  | Some (vt) ->
      match vt with
      | VarInt -> ($"%%r{reg} = load i32, i32* %%{v}", RInt(reg), vars)
      | VarBool -> ($"%%r{reg} = load i1, i1* %%{v}", RBool(reg), vars)
      | VarStr -> ($"%%r{reg} = load i8*, i8** %%{v}", RStr(reg, -1), vars)
      | VarFunc -> failwith "Loading a function pointer from variable not yet implemented"
  | None -> failwith $"Tried to load a variable '{v}' that has not been set"

let rec emitStore v expr ctx g vars =
  let (line, reg, vars') = emitExpr ctx g expr vars
  if Map.containsKey v vars then
    failwith $"Tried to overwrite variable 'v'"
  match reg with
  | RInt n ->
      let vars'' = Map.add v VarInt vars
      let s1 = $"\n  %%{v} = alloca i32"
      let s2 = $"\n  store i32 %%r{n}, i32* %%{v}"
      ((line+s1+s2), RNil, vars'')
  | RBool n ->
      let vars'' = Map.add v VarBool vars
      let s1 = $"\n  %%{v} = alloca i1"
      let s2 = $"\n  store i1 %%r{n}, i1* %%{v}"
      ((line+s1+s2), RNil, vars'')
  | RStr (n, _) ->
      let vars'' = Map.add v VarStr vars
      let s1 = $"\n  %%{v} = alloca i8*"
      let s2 = $"\n  store i8* %%r{n}, i8** %%{v}"
      ((line+s1+s2), RNil, vars'')
  | RFunc (n, _) -> failwith "Storing function reference not yet implemented"
  | RNil -> failwith "Tried to get register id on RNil register"


and emitUnary op expr outReg ctx g vars =
  let (line, inReg, vars') = emitExpr ctx g expr vars
  match op with
  | "-" ->
      match inReg with
      | RInt(r) -> ($"{line}\n  %%r{outReg} = sub i32 0, %%r{r}", RInt(outReg), vars')
      | _ -> failwith "Non-integer operand with operator '-'"
  | "not" ->
      match inReg with
      | RBool(r) -> ($"{line}\n  %%r{outReg} = icmp eq i1 %%r{r}, 0", RBool(outReg), vars')
      | _ -> failwith "Non-boolean operand with operator 'not'"
  | "$" ->
      match inReg with
      | RInt(r) ->
          // Allocate a local buffer and call sprintf into it
          let ir = getIRConstructor ()
          let id1 = getUid ()
          let id2 = getUid ()
          emitIR ir line
          emitIR ir $"%%u{id1} = alloca [32 x i8], align 1"
          emitIR ir $"  %%r{outReg} = getelementptr inbounds [32 x i8], [32 x i8]* %%u{id1}, i32 0, i32 0"
          emitIR ir $"  %%u{id2} = call i32 (i8*, i8*, ...) @sprintf(i8* %%r{outReg}, i8* getelementptr inbounds ([3 x i8], [3 x i8]* @.str3, i32 0,  i32 0), i32 %%r{r})"
          // Return the GEP pointer, since that’s now your string
          (getIRText ir, RStr(outReg, -1), vars')

      | RBool(r) ->
          // Return "true" or "false" string
          let ir = getIRConstructor ()
          emitIR ir line
          emitIR ir $"  %%r{outReg} = select i1 %%r{r}, i8* getelementptr inbounds ([5 x i8], [5 x i8]* @.str1, i32 0, i32 0), i8* getelementptr inbounds ([6 x i8], [6 x i8]* @.str2, i32 0, i32 0)"
          (getIRText ir, RStr(outReg, -1), vars')

      | RStr(r, len) ->
          let ir = getIRConstructor ()
          emitIR ir line
          emitIR ir $"%%r{outReg} = bitcast i8* %%r{r} to i8*"
          (getIRText ir, RStr(outReg, -1), vars')
      | _ -> failwith "Unsupported operand for $"
  | sOp -> failwith $"Unknown unary operator '{sOp}'"

and emitBin op lhs rhs outReg ctx g vars =
  let (lline, lReg, vars') = emitExpr ctx g lhs vars
  let (rline, rReg, vars'') = emitExpr ctx g rhs vars'
  match op with
  | "=" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = icmp eq i32 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | RBool(lr), RBool(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = icmp eq i1 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | RStr(lr, llen), RStr(rr, rlen) -> failwith "String operator '=' not yet implemented for string operands"
      | _, _ -> failwith "Unexpected operand with operator '='"
  | "<>" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = icmp ne i32 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | RBool(lr), RBool(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = icmp ne i1 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | _, _ -> failwith "Unexpected operand with operator '<>'"
  | ">" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = icmp sgt i32 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | _, _ -> failwith "Non-integer operand with operator '>'"
  | "<" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = icmp slt i32 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | _, _ -> failwith "Non-integer operand with operator '<'"
  | "<=" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = icmp sle i32 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | _, _ -> failwith "Non-integer operand with operator '<='"
  | ">=" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = icmp sge i32 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | _, _ -> failwith "Non-integer operand with operator '>='"
  | "&&" ->
      match lReg, rReg with
      | RBool(lr), RBool(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = and i1 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | _, _ -> failwith "Non-boolean operand with operator '&&'"
  | "||" ->
      match lReg, rReg with
      | RBool(lr), RBool(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = or i1 %%r{lr}, %%r{rr}", RBool(outReg), vars'')
      | _, _ -> failwith "Non-boolean operand with operator '||'"
  | "+" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = add i32 %%r{lr}, %%r{rr}", RInt(outReg), vars'')
      | RStr(lr, llen), RStr(rr, rlen) ->
          let ir = getIRConstructor ()
          emitIR ir lline
          emitIR ir rline
          concatStrings ir $"r{lr}" $"r{rr}" $"r{outReg}"
          (getIRText ir, RStr(outReg, -1), vars'')
      | _, _ -> failwith "Unexpected operand with operator '+'"
  | "-" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = sub i32 %%r{lr}, %%r{rr}", RInt(outReg), vars'')
      | _, _ -> failwith "Non-integer operand with operator '-'"
  | "*" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = mul i32 %%r{lr}, %%r{rr}", RInt(outReg), vars'')
      | _, _ -> failwith "Non-integer operand with operator '*'"
  | "/" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = sdiv i32 %%r{lr}, %%r{rr}", RInt(outReg), vars'')
      | _, _ -> failwith "Non-integer operand with operator '/'"
  | "%" ->
      match lReg, rReg with
      | RInt(lr), RInt(rr) -> ($"{lline}\n  {rline}\n  %%r{outReg} = srem i32 %%r{lr}, %%r{rr}", RInt(outReg), vars'')
      | _, _ -> failwith "Non-integer operand with operator '%'"
  | sOp -> failwith $"Unknown unary operator '{sOp}'"

and emitCall fn parms outReg ctx g vars =
  // TODO: this is a placeholder that only works for hello world

  let head = List.head parms
  let (line, childReg, vars') = emitExpr ctx g head vars

  match fn with
  | FLambda lid -> failwith "FLambda not yet implemented"
  | FUser name -> failwith "FUser not yet implemented"
  | FSystem name ->
      match name with
      | "printf" ->
          let regId = getRegId childReg
          ($"{line}\n   %%r{outReg} = call i32 (i8*, ...) @printf(i8* %%r{regId})", RInt(outReg), vars')
      | _  -> failwith $"System function {name} not yet implemented"
  | FDynamic expr -> failwith "FDynamic not yet implemented"

and emitExpr (ctx: LoweringContext) (g: Globals) (expr: TfsExpr) (vars: Map<string, VariableT>) =
  match expr with
  | Const (v, reg) -> emitConst v reg ctx g vars
  | Load (var, reg) -> emitLoad var reg ctx g vars
  | Store (var, expr) -> emitStore var expr ctx g vars
  | Unary (op, expr, reg) -> emitUnary op expr reg ctx g vars
  | Bin (op, lhs, rhs, reg) -> emitBin op lhs rhs reg ctx g vars
  | Call (fn, parms, reg) -> emitCall fn parms reg ctx g vars
  | MakeClosure (lid, parms) -> failwith "MakeClosure not yet implemented"
  | GetClosureEnv slot -> failwith "SetClosureEnv not yet implemented"

let emitMain (ctx: LoweringContext) (g: Globals) (body: TfsExpr list) : string =
  let vars = Map.empty
  let instrsWithRegs, finalVars =
    body
    |> List.fold (fun (accInstrs, accVars) expr ->
        let inst, reg, newVars = emitExpr ctx g expr accVars
        ((inst, reg) :: accInstrs, newVars)
       )([], vars)
  let reversed = List.rev instrsWithRegs
  let instructions =
    reversed
    |> List.map (fun (inst, _) -> inst)
    |> String.concat "\n  "
  let finalReg =
    reversed
    |> List.map (fun (_, r) -> r)
    |> List.last

  let sb = new System.Text.StringBuilder()
  sb.Append("define i32 @main() {\n") |> ignore
  sb.Append("entry:\n\n  ") |> ignore
  sb.Append(instructions) |> ignore
  sb.Append("\n  ret i32 0\n") |> ignore
  sb.Append("}\n") |> ignore

  sb.ToString()






