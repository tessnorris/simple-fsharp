/// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCore.Eval

open SimpleFSharpCore.CoreTools
open SimpleFSharpCore.SyntaxTree
open SimpleFSharpCore.Environment

let tryParseInt (s: string) =
  match System.Int32.TryParse(s) with
  | (true, n) -> Some n
  | (false, _) -> None


let rec eval expr env : Value * Env =
  match expr with
  | Lit (IntLit n) -> (IntV n, env)
  | Lit (BoolLit b) -> (BoolV b, env)
  | Lit (StringLit s) -> (StrV s, env)
  | Lit UnitLit -> (UnitV, env)
  | InterpolatedStr segments ->
      (StrV(evalInterpolatedSegments segments env), env)
  | Var x ->
      match findVar x env with
      | Some v -> (v, env)
      | None -> failwith $"Undefined variable: {x}"
  | UnaryOp (op, value) ->
      let v, _ = eval value env
      match v with
      | UnitV -> failwith "Cannot perform operation on Unit type"
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
      | UnitV, _
      | _, UnitV -> failwith "Cannot perform operation on Unit type"
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
  | LambdaExpr { prm = arg; body = body } ->
      (Closure (arg, body, ref env), env)
  | If (conds, maybeElse) ->
      evalConditional conds maybeElse env
  | Apply (fnExpr, argExpr) ->
      let fnVal, _ = eval fnExpr env
      let argVal, _ = eval argExpr env
      match fnVal with
      | Closure (param, body, closureEnv) ->
          let newEnv = pushScope !closureEnv
          let newEnv' = setVar param false argVal newEnv
          let (v, _) = eval body newEnv'
          // don't need to pop scope because it's closurr scope, not locsl
          (v, env)
      | _ -> failwith "Tried to apply something that wasn't a function"
  | Let(name, mut, expr) ->
      let value, env' = eval expr env
      (UnitV, setVar name mut value env')
  | MutableAssign(name, expr) ->
      let value, env' = eval expr env
      (UnitV, assignVar name value env')
  | Fn fn ->
      let prm = fn.lambda.prm
      let body = fn.lambda.body
      let placeholderEnv : Env ref = ref []
      let closure = Closure(prm, body, placeholderEnv)
      let env' = setOrReplaceVar fn.name false closure env
      if fn.recursive then
          placeholderEnv := env'
      else
          placeholderEnv := env
      (UnitV, env')
  | While(condExpr, body) ->
      let rec loop env =
        let result, env' = eval condExpr env
        match result with
        | BoolV true -> 
            let _ = eval body env'
            loop env
        | BoolV false -> (UnitV, env')
        | _ -> failwith "Condition in while loop must be a boolean"
      loop env
  | Block exprs ->
      let v, env' = evalBlock exprs (pushScope env)
      (v, popScope env')
      
and evalBlock exprs env =
  match exprs with
  | [] -> (UnitV, env)
  | [expr] -> eval expr env
  | expr :: rest ->
      let _, env' = eval expr env
      evalBlock rest env'

and evalInterpolatedSegments segments env =
  match segments with
  | [] -> ""
  | InterpolatedExpr ex :: rest ->
      let (v, env') = eval ex env
      valueToStr(v) + evalInterpolatedSegments rest env
  | StringSegment s :: rest ->
      s + evalInterpolatedSegments rest env

and evalConditional (conds: Conditional list) maybeElse env : Value * Env =
  match conds with 
  | [] ->
      match maybeElse with
      | Some expr ->
          let v, _ =
            eval expr env
          (v, env)
      | None -> (UnitV, env)
  | cond :: rest ->
      let result, env' = eval cond.condition env
      match result with
      | BoolV true ->
          let v, _ =
            eval cond.expr env'
          (v, env)
      | BoolV false -> evalConditional rest maybeElse env'
      | _ -> failwith "Condition must evaluate to a boolean"

