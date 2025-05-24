namespace FMinus

open AST

exception TypeError

type Type =
    | Int
    | Bool
    | TyVar of string
    | Func of Type * Type

type TypeEnv = Map<string, Type>

module Type =

    let rec toString (typ: Type): string =
        match typ with
        | Int -> "int"
        | Bool -> "bool"
        | TyVar s -> s
        | Func (t1, t2) -> sprintf "(%s) -> (%s)" (toString t1) (toString t2)

    let mutable counter = 0
    let freshTyVar () =
        let tv = sprintf "t%d" counter
        counter <- counter + 1
        TyVar tv

    let rec occurs v t =
        match t with
        | TyVar v' -> v = v'
        | Func (a, r) -> occurs v a || occurs v r
        | _ -> false

    let rec app (subst: Map<string, Type>) (t: Type) : Type =
        match t with
        | TyVar v -> Map.tryFind v subst |> Option.defaultValue t
        | Func (a, r) -> Func (app subst a, app subst r)
        | _ -> t

    let rec gen (t: Type) (env: TypeEnv): Type =
        match t with
        | TyVar v -> if Map.containsKey v env then TyVar v else t
        | Func (a, r) -> Func (gen a env, gen r env)
        | _ -> t

    let rec unify t1 t2 subst =
        let t1' = app subst t1
        let t2' = app subst t2
        extend t1' t2' subst

    and extend t1 t2 subst =
        match t1, t2 with
        | Int, Int -> subst
        | Bool, Bool -> subst
        | TyVar v, t | t, TyVar v ->
            if t = TyVar v then subst
            else if occurs v t then raise TypeError
            else Map.add v t subst
        | Func (a1, r1), Func (a2, r2) ->
            let subst1 = unify a1 a2 subst
            unify (app subst1 r1) (app subst1 r2) subst1
        | _ -> raise TypeError

    let rec applyEnvSubst (subst: Map<string, Type>) (env: TypeEnv) : TypeEnv =
        env |> Map.map (fun _ t -> app subst t)

    let rec inferExp (env: TypeEnv) (exp: AST.Exp) : Type * Map<string, Type> =
        match exp with
        | AST.Num _ -> (Int, Map.empty)
        | AST.True | AST.False -> (Bool, Map.empty)
        | AST.Var x ->
            match Map.tryFind x env with
            | Some t -> (gen t env, Map.empty)
            | None -> raise TypeError
        | AST.Neg e ->
            let (t, s) = inferExp env e
            let subst = unify t Int s
            (Int, subst)
        | AST.Add (e1, e2) | AST.Sub (e1, e2) ->
            let (t1, s1) = inferExp env e1
            let (t2, s2) = inferExp (applyEnvSubst s1 env) e2
            let subst = unify (app s2 t1) Int s2 |> unify (app s2 t2) Int
            (Int, Map.fold (fun acc k v -> Map.add k v acc) s1 subst)
        | AST.LessThan (e1, e2) | AST.GreaterThan (e1, e2) | AST.Equal (e1, e2) | AST.NotEq (e1, e2) ->
            let (t1, s1) = inferExp env e1
            let (t2, s2) = inferExp (applyEnvSubst s1 env) e2
            let subst = unify (app s2 t1) Int s2 |> unify (app s2 t2) Int
            (Bool, Map.fold (fun acc k v -> Map.add k v acc) s1 subst)
        | AST.IfThenElse (e1, e2, e3) ->
            let (t1, s1) = inferExp env e1
            let subst1 = unify t1 Bool s1
            let (t2, s2) = inferExp (applyEnvSubst subst1 env) e2
            let (t3, s3) = inferExp (applyEnvSubst (Map.fold (fun acc k v -> Map.add k v acc) s2 subst1) env) e3
            let subst2 = unify (app s3 t2) (app s3 t3) s3
            (app subst2 t2, Map.fold (fun acc k v -> Map.add k v acc) subst1 (Map.fold (fun acc k v -> Map.add k v acc) s3 subst2))
        | AST.LetIn (x, e1, e2) ->
            let (t1, s1) = inferExp env e1
            let env' = applyEnvSubst s1 env
            let env'' = Map.add x t1 env'
            let (t2, s2) = inferExp env'' e2
            (t2, s2)
        | AST.LetFunIn (f, x, e1, e2) ->
            let tv1 = freshTyVar ()
            let tv2 = freshTyVar ()
            let env' = Map.add x tv1 (Map.add f (Func (tv1, tv2)) env)
            let (t1, s1) = inferExp env' e1
            let subst1 = unify (app s1 tv2) (app s1 t1) s1
            let env'' = applyEnvSubst subst1 env'
            let (t2, s2) = inferExp (applyEnvSubst s1 env'') e2
            (app s2 t2, Map.fold (fun acc k v -> Map.add k v acc) subst1 (Map.fold (fun acc k v -> Map.add k v acc) s2 subst1))
        | AST.LetRecIn (f, x, e1, e2) ->
            let tv1 = freshTyVar ()
            let tv2 = freshTyVar ()
            let env' = Map.add f (Func (tv1, tv2)) (Map.add x tv1 env)
            let (t1, s1) = inferExp env' e1
            let subst = unify (app s1 tv2) (app s1 t1) s1
            let env'' = applyEnvSubst subst env'
            let (t2, s2) = inferExp env'' e2
            (t2, s2)
        | AST.Fun (x, body) ->
            let tv = freshTyVar ()
            let env' = Map.add x tv env
            let (bodyType, s) = inferExp env' body
            (Func (app s tv, bodyType), s)
        | AST.App (f, arg) ->
            let (fType, s1) = inferExp env f
            let (argType, s2) = inferExp (applyEnvSubst s1 env) arg
            let resultType = freshTyVar ()
            let subst = unify (app s2 fType) (Func (argType, resultType)) s2
            (app subst resultType, Map.fold (fun acc k v -> Map.add k v acc) s1 (Map.fold (fun acc k v -> Map.add k v acc) s2 subst))

    let infer (prog: AST.Program) : Type =
        let (progType, _) = inferExp Map.empty prog
        progType
