/// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCore.Eval

open SimpleFSharpCore.CoreTools
open SimpleFSharpCore.SyntaxTree

type Value =
  | IntV of int
  | BoolV of bool
  | StrV of string
  | Closure of string * Expr * Map<string, Value>

let valueToStr v =
  match v with
  | IntV n -> string n
  | BoolV b -> string b
  | StrV s -> s
  | Closure _  -> "<Closure>"

let tryParseInt (s: string) =
  match System.Int32.TryParse(s) with
  | (true, n) -> Some n
  | (false, _) -> None


let rec eval expr env =
  match expr with
  | Lit (IntLit n) -> (IntV n, env)
  | Lit (BoolLit b) -> (BoolV b, env)
  | Lit (StringLit s) -> (StrV s, env)
  | InterpolatedStr segments ->
      (StrV(evalInterpolatedSegments segments env), env)
  | Var x ->
      match Map.tryFind x env with
      | Some v -> (v, env)
      | None -> failwith $"Undefined variable: {x}"
  | UnaryOp (op, value) ->
      let v, _ = eval value env
      match v with
      | IntV v ->
          let res =
            match op with
            | Neg -> IntV(-v)
            | StringOp -> StrV(string v)
            | _ -> failwith $"Unknown int operator: {op}"
          (res, env)
      | BoolV v ->
          let res =
            match op with
            | BoolNot -> BoolV(not v)
            | StringOp -> StrV(string v)
            | _ -> failwith $"Unknown bool operator: {op}"
          (res, env)
      | StrV s ->
          let res =
            match op with
            | IntOp ->
                match tryParseInt s with
                | Some n -> IntV(n)
                | None -> failwith $"Cannot convert string '{s}' to int"
            | _ -> failwith $"Unknown string operator: {op}"
          (res, env)
      | Closure (_, _, _) -> failwith "Unexpected closure"
  | BinOp (op, left, right) ->
      let lv, _ = eval left env
      let rv, _ = eval right env
      match lv, rv with
      | IntV l, IntV r ->
          let res =
            match op with
            | Add -> IntV(l + r)
            | OpNum Sub -> IntV(l - r)
            | OpNum Mul -> IntV(l * r)
            | OpNum Div -> IntV(l / r)
            | OpNum Mod -> IntV(l % r)
            | OpCompar Equals -> BoolV(l = r)
            | OpCompar Greater -> BoolV(l > r)
            | OpCompar Less -> BoolV(l < r)
            | OpCompar NotEqual -> BoolV(l <> r)
            | OpCompar LessEqual -> BoolV(l >= r)
            | OpCompar GreaterEqual -> BoolV(l <= r)
            | _ -> failwith $"Unknown numeric operator: {printBinOp op}"
          (res, env)
      | BoolV l, BoolV r ->
          let res =
            match op with
            | OpBool BoolOr -> BoolV(l || r)
            | OpBool BoolAnd -> BoolV(l && r)
            | OpCompar Equals -> BoolV(l = r)
            | OpCompar NotEqual -> BoolV(l <> r)
            | _ -> failwith $"Unknown boolean operator: {printBinOp op}"
          (res, env)
      | StrV l, StrV r ->
          let res =
            match op with
            | Add -> StrV(l + r)
            | OpCompar Equals -> BoolV(l = r)
            | OpCompar NotEqual -> BoolV(l <> r)
            | _ -> failwith $"Unknown string operator: {printBinOp op}"
          (res, env)
      | _, _ -> failwith $"Type mismatch: {printBinOp op}"
//  | Let (x, valueExpr) ->
//      let value, _ = eval valueExpr env
//      let newEnv = Map.add x value env
//      (value, newEnv)
  | Lambda (arg, body) -> (Closure (arg, body, env), env)
  | Apply (fnExpr, argExpr) ->
      let fnVal, _ = eval fnExpr env
      let argVal, _ = eval argExpr env
      match fnVal with
      | Closure (param, body, closureEnv) ->
          let newEnv = Map.add param argVal closureEnv
          let (res, _) = eval body newEnv
          (res, env)
      | _ -> failwith "Tried to apply something that wasn't a function"


and evalInterpolatedSegments segments env =
  match segments with
  | [] -> ""
  | InterpolatedExpr ex :: rest ->
      let (v, env') = eval ex env
      valueToStr(v) + evalInterpolatedSegments rest env
  | StringSegment s :: rest ->
      s + evalInterpolatedSegments rest env

