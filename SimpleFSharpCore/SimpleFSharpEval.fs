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


let rec evalExpr expr env : Value * Env =
  match expr with
  | Lit (IntLit n) -> (IntV n, env)
  | Lit (BoolLit b) -> (BoolV b, env)
  | Lit (StringLit s) -> (StrV s, env)
  | InterpolatedStr segments ->
      (StrV(evalInterpolatedSegments segments env), env)
  | Var x ->
      match findVar x env with
      | Some v -> (v, env)
      | None -> failwith $"Undefined variable: {x}"
  | UnaryOp (op, value) ->
      let v, _ = evalExpr value env
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
      let lv, _ = evalExpr left env
      let rv, _ = evalExpr right env
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
  | IfEx (conds, elseBlk) ->
      evalConditionalExpr conds elseBlk env
  | Apply (fnExpr, argExpr) ->
      let fnVal, _ = evalExpr fnExpr env
      let argVal, _ = evalExpr argExpr env
      match fnVal with
      | Closure (param, body, closureEnv) ->
          let newEnv = pushScope !closureEnv
          let newEnv' = setVar param false argVal newEnv
          let (v, _) = evalExprBlock body newEnv'
          // don't need to pop scope because it's closurr scope, not locsl
          (v, env)
      | _ -> failwith "Tried to apply something that wasn't a function"


and evalInterpolatedSegments segments env =
  match segments with
  | [] -> ""
  | InterpolatedExpr ex :: rest ->
      let (v, env') = evalExpr ex env
      valueToStr(v) + evalInterpolatedSegments rest env
  | StringSegment s :: rest ->
      s + evalInterpolatedSegments rest env


and evalStatement stmt env =
  match stmt with
  | Let(name, mut, eblk) ->
      let value, env' = evalExprBlock eblk env
      (UnitV, setVar name mut value env')
  | MutableAssign(name, eblk) ->
      let value, env' = evalExprBlock eblk env
      (UnitV, assignVar name value env')      
  | Fn fn ->
      let prm = fn.lambda.prm
      let body = fn.lambda.body
      let placeholderEnv : Env ref = ref []
      let value = Closure(prm, body, placeholderEnv)
      let env' = setVar fn.name false value env
      placeholderEnv := if fn.recursive then env' else env
      (UnitV, env')
  | IfSt(conds, maybeElse) ->
      let v, _ = evalConditionalStmt conds maybeElse env
      (v, env)
  | While(condExpr, body) ->
      let rec loop env =
        let result, env' = evalExpr condExpr env
        match result with
        | BoolV true -> 
            let _ = evalStmtBlock body.statements (pushScope env')
            loop env
        | BoolV false -> (UnitV, env')
        | _ -> failwith "Condition in while loop must be a boolean"
      loop env
  | Expression expr ->
      let v, env' = evalExpr expr env
      (v, env')

and evalStmtBlock (stmts: Statement list) env =
  List.fold (fun (_, envAcc) stmt ->
    evalStatement stmt envAcc
  ) (UnitV, env) stmts

and evalExprBlock (blk: ExprBlock) env =
  let _, env' = evalStmtBlock blk.statements env
  evalExpr blk.expr env'

and evalConditionalStmt (conds: ConditionalStmt list) maybeElse env : Value * Env =
  match conds with 
  | [] ->
      match maybeElse with
      | Some block ->
          let v, _ =
            evalStmtBlock block.statements (pushScope env)
          (v, env)
      | None -> (UnitV, env)
  | cond :: rest ->
      let result, env' = evalExpr cond.condition env
      match result with
      | BoolV true ->
          let v, _ =
            evalStmtBlock cond.sblock.statements (pushScope env')
          (v, env)
      | BoolV false -> evalConditionalStmt rest maybeElse env'
      | _ -> failwith "Condition must evaluate to a boolean"

and evalConditionalExpr (conds: ConditionalExpr list) (exprBlock: ExprBlock) env =
  match conds with
  | [] ->
      let v, _ = evalExprBlock exprBlock (pushScope env)
      (v, env)
  | cond :: rest ->
      let result, env' = evalExpr cond.condition env
      match result with
      | BoolV true ->
          let v, _ =
            evalExprBlock cond.eblock (pushScope env')
          (v, env)
      | BoolV false -> evalConditionalExpr rest exprBlock env'
      | _ -> failwith "Condition must evaluate to a boolean"
