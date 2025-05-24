module FMinus

open AST
open Types

// Evaluate expression into a value, under the given environment.
let rec evalExp (exp: Exp) (env: Env) : Val =
  // TODO: fill in the remaining cases.
  match exp with
  | Num i -> Int i
  | True -> Bool true
  | False -> Bool false
  | Var x -> 
    match Map.tryFind x env with
    | Some v -> v
    | None -> raise UndefinedSemantics
  | Neg e1 ->
    match e1 with
    | Num i -> Int (-i)
    | _ -> raise UndefinedSemantics
  | Add (e1, e2) -> 
    let v1 = evalExp e1 env
    let v2 = evalExp e2 env
    match v1, v2 with
    | Int a, Int b -> Int (a + b)
    | _ -> raise UndefinedSemantics
  | Sub (e1, e2) -> 
    let v1 = evalExp e1 env
    let v2 = evalExp e2 env
    match v1, v2 with
    | Int a, Int b -> Int (a - b)
    | _ -> raise UndefinedSemantics
  | LessThan (e1, e2) -> 
    let v1 = evalExp e1 env
    let v2 = evalExp e2 env
    match v1, v2 with
    | Int a, Int b -> Bool (a < b)
    | _ -> raise UndefinedSemantics
  | GreaterThan (e1, e2) -> 
    let v1 = evalExp e1 env
    let v2 = evalExp e2 env
    match v1, v2 with
    | Int a, Int b -> Bool (a > b)
    | _ -> raise UndefinedSemantics
  | Equal (e1, e2) -> 
    let v1 = evalExp e1 env
    let v2 = evalExp e2 env
    match v1, v2 with
    | Int a, Int b -> Bool (a = b)
    | Bool a, Bool b -> Bool (a = b)
    | _ -> raise UndefinedSemantics
  | NotEq (e1, e2) -> 
    let v1 = evalExp e1 env
    let v2 = evalExp e2 env
    match v1, v2 with
    | Int a, Int b -> Bool (a <> b)
    | Bool a, Bool b -> Bool (a <> b)
    | _ -> raise UndefinedSemantics
  | IfThenElse (e1, e2, e3) ->
    let flag = evalExp e1 env
    match flag with
    | Bool true -> evalExp e2 env
    | Bool false -> evalExp e3 env
    | _ -> raise UndefinedSemantics
  | LetIn (x, e1, e2) ->
    let v1 = evalExp e1 env
    let env2 = Map.add x v1 env
    evalExp e2 env2
  | LetFunIn (f, x, e1, e2) ->
    let env2 = Map.add f (Func (x, e1, env)) env
    evalExp e2 env2
  | LetRecIn (f, x, e1, e2) ->
    let v1 = RecFunc (f, x, e1, env)
    let env2 = Map.add f v1 env
    evalExp e2 env2
  | Fun (x ,e1) -> Func (x, e1, env)
  | App (e1, e2) ->
    let v1 = evalExp e1 env
    let v2 = evalExp e2 env
    match v1 with
    | Func (x, body, fenv) -> 
      let env2 = Map.add x v2 fenv
      evalExp body env2
    | RecFunc (f, x, body, fenv) ->
      let env2 = Map.add x v2 (Map.add f v1 fenv)
      evalExp body env2
    | _ -> raise UndefinedSemantics

// Note: You may define more functions.

// The program starts execution with an empty environment. Do not fix this code.
let run (prog: Program) : Val =
  evalExp prog Map.empty
