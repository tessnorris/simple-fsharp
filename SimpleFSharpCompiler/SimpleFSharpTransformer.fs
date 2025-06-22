// Simple FSharp
// Copyright (c) 2025 Tessa Norris
// Licensed under the MIT License. See LICENSE file in the project root for full license information.
module SimpleFSharpCompiler.Transformer

open SimpleFSharpCore.CoreTools
open SimpleFSharpCompiler.Tools

let rec freeVars expr : Set<string> =
  match expr with
  | Int _ -> Set.empty
  | Bool _ -> Set.empty
  | Str _ -> Set.empty
  | InterpolatedStr _ -> Set.empty // TODO: fix this
  | Var x -> Set.singleton x
  | UnaryOp(_, e) -> freeVars e
  | BinOp(left, _, right) -> Set.union (freeVars left) (freeVars right)
  | Let(x, valueExpr) ->
      let fvValue = freeVars valueExpr
      Set.remove x fvValue
  | Lambda(args, body) ->
      let fvBody = freeVars body
      Set.difference fvBody (Set.ofList args)
  | Apply(funcExpr, argExprs) ->
      let fvFunc = freeVars funcExpr
      let fvArgs = argExprs |> List.map freeVars |> Set.unionMany
      Set.union fvFunc fvArgs

let extractInner (s: string) =
  if s.StartsWith("{") && s.EndsWith("}") && s.Length > 2 then
    s.Substring(1, s.Length - 2)
  else
    s

let rec lower (ctx: LoweringContext) (globals: Globals) (expr: Expr) : LoweringContext * Globals * TfsExpr =
//let rec lower ctx globals expr =
  match expr with
  | Int n -> (ctx, freshId globals, Const ((VInt n), globals.Ctr))

  | Bool b -> (ctx, freshId globals, Const ((VBool b), globals.Ctr))

  | Str s ->
      let len = String.length s
      match Map.tryFind s globals.Strings with
      | Some id ->
        (ctx, freshId globals, Const (VStr(id, len+1), globals.Ctr))
      | None ->
          let id = globals.Ctr
          let newGlobals = {
            globals with
              Strings = Map.add s id globals.Strings;
              Ctr = id + 2
          }
          (ctx, newGlobals, Const(VStr(id, len+1), (id+1)))

  // TODO: inplement
  | InterpolatedStr s -> failwith $"Interpolated string not yet implemented"

  | Var name ->
      if ctx.Scope.Contains(name) then
        (ctx, freshId globals, Load(name, globals.Ctr))
      elif ctx.Env.ContainsKey(name) then
        (ctx, globals, GetClosureEnv (ctx.Env.[name]))
      else
        failwith $"Unbound variable: {name}"

  | UnaryOp(op, e) ->
      // process the $ operator to get to the inner value
      if op = "$" then
        match e with
        | Str s ->
            let varName = extractInner s
            let load = Load(varName, globals.Ctr)
            let g1 = freshId globals
            (ctx, freshId g1, Unary(op, load, g1.Ctr))
        | _ -> failwith $"unexpected expression for $ operand: {printExpr e 1}"
      else
        let (ctx', g1, e') = lower ctx globals e
        (ctx', freshId g1, Unary(op, e', g1.Ctr))

  | BinOp(lhs, op, rhs) ->
      let (ctx', g1, lhs') = lower ctx globals lhs
      let (ctx'', g2, rhs') = lower ctx' g1 rhs
      (ctx'', freshId g2, Bin(op, lhs', rhs', g2.Ctr))

  | Let(name, value) ->
      let (ctx', g1, value') = lower ctx globals value
      let ctx'' = { ctx' with Scope = ctx.Scope.Add(name) }
      (ctx'', g1, Store(name, value'))

  | Lambda(args, body) ->
      // Find free vars
      let free = freeVars body
      let captured = Set.difference free (Set.ofList args)
      let capturedList = Set.toList captured

      // Build closure env mapping
      let env =
        capturedList
        |> List.mapi (fun i name -> name, i)
        |> Map.ofList

      let ctx' = {
        Env = env
        Scope = Set.union ctx.Scope (Set.ofList args)
        Captured = Set.ofList capturedList
      }

      let (ctx'', g1, loweredBody) = lower ctx' globals body

      // Register lambda
      let id = g1.Ctr
      let def = { Args = args; Captured = capturedList; Body = loweredBody }
      let g2 = { g1 with Lambdas = (def, id) :: g1.Lambdas; Ctr = id + 1 }

      // Emit closure with captured vars
      let g3, capturedExprs =
        capturedList
        |> List.fold (fun (gacc, acc) name ->
            let (_, gnext, e) = lower ctx gacc (Var name)
            gnext, acc @ [e]
        ) (g2, [])

      (ctx'', g3, MakeClosure(id, capturedExprs))

  | Apply(fnExpr, args) ->
      if fnExpr = Var("printf") then
        let g2, args' =
          args
          |> List.fold (fun (gacc, acc) a ->
              let (_, gnext, argIR) = lower ctx gacc a
              gnext, acc @ [argIR]
          ) (globals, [])
        (ctx, freshId g2, Call(FSystem("printf"), args', g2.Ctr))
      else
        let (ctx', g1, fn') = lower ctx globals fnExpr
        let g2, args' =
          args
          |> List.fold (fun (gacc, acc) a ->
              let (_, gnext, argIR) = lower ctx gacc a
              gnext, acc @ [argIR]
          ) (g1, [])

        // NOTE: Treating the function expr as dynamic
        // If you want to optimize for known lambdas, you can inspect here
        (ctx', freshId g2, Call(FDynamic fn', args', g2.Ctr))

  | _ -> failwith "Unimplemented expression type in lower"
