module CMinusPtr

open AST
open Types

// Evaluate expression into a value, under the given memory.
let rec evalExp (exp: Exp) (mem: Mem) : Val =
  match exp with
  | Num i -> Int i
  // TODO: fill in the remaining cases.
  | True -> Bool true
  | False -> Bool false
  | LV lv -> evalLVal lv mem
  | AddrOf str -> Loc str
  | Add (e1, e2) -> 
    let v1 = evalExp e1 mem
    let v2 = evalExp e2 mem
    match v1, v2 with
    | Int a, Int b -> Int (a + b)
    | _ -> raise UndefinedSemantics
  | Sub (e1, e2) -> 
    let v1 = evalExp e1 mem
    let v2 = evalExp e2 mem
    match v1, v2 with
    | Int a, Int b -> Int (a - b)
    | _ -> raise UndefinedSemantics
  | LessThan (e1, e2) -> 
    let v1 = evalExp e1 mem
    let v2 = evalExp e2 mem
    match v1, v2 with
    | Int a, Int b -> Bool (a < b)
    | _ -> raise UndefinedSemantics
  | GreaterThan (e1, e2) -> 
    let v1 = evalExp e1 mem
    let v2 = evalExp e2 mem
    match v1, v2 with
    | Int a, Int b -> Bool (a > b)
    | _ -> raise UndefinedSemantics
  | Equal (e1, e2) -> 
    let v1 = evalExp e1 mem
    let v2 = evalExp e2 mem
    match v1, v2 with
    | Int a, Int b -> Bool (a = b)
    | Bool a, Bool b -> Bool (a = b)
    | _ -> raise UndefinedSemantics
  | NotEq (e1, e2) -> 
    let v1 = evalExp e1 mem
    let v2 = evalExp e2 mem
    match v1, v2 with
    | Int a, Int b -> Bool (a <> b)
    | Bool a, Bool b -> Bool (a <> b)
    | _ -> raise UndefinedSemantics

// Note: You may define more functions.
and evalLVal (lval: LVal) (mem: Mem) : Val =
  match lval with
  | Var vname -> 
    match Map.tryFind vname mem with
    | Some v -> v
    | None -> raise UndefinedSemantics
  | Deref e -> 
    let loc = evalExp e mem
    match loc with
    | Loc vname ->
      match Map.tryFind vname mem with
      | Some v -> v
      | None -> raise UndefinedSemantics
    | _ -> raise UndefinedSemantics

// Execute a statement and return the updated memory.
let rec exec (stmt: Stmt) (mem: Mem) : Mem =
  match stmt with
  | NOP -> mem // NOP does not change the memory.
  // TODO: fill in the remaining cases.
  | Assign (lv, e) -> 
    let v = evalExp e mem
    let mem2 = 
      match lv with
      | Var vname -> Map.add vname v mem
      | Deref e ->
        match evalExp e mem with
        | Loc vname -> Map.add vname v mem
        | _ -> raise UndefinedSemantics
    mem2
  | Seq (s1, s2) ->
    let mem2 = exec s1 mem
    exec s2 mem2
  | If (e, s1, s2) ->
    let v = evalExp e mem
    match v with
    | Bool true -> exec s1 mem
    | Bool false -> exec s2 mem
    | _ -> raise UndefinedSemantics
  | While (e, s) ->
    let rec loop mem =
      let v = evalExp e mem
      match v with
      | Bool true ->
      let mem2 = exec s mem
      loop mem2
      | Bool false -> mem
      | _ -> raise UndefinedSemantics
    loop mem

// The program starts execution with an empty memory. Do NOT fix this function.
let run (prog: Program) : Mem =
  exec prog Map.empty
