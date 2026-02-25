open Types
open Env

let rec apply_substitution (env : (int, ty) env) (t : ty) : ty =
  match t with
  | TVar x -> (
    match lookup env x with
    | Some t ->
        apply_substitution env t
    | None ->
        TVar x )
  | Arrow (t1, t2) ->
      Arrow (apply_substitution env t1, apply_substitution env t2)
  | Tuple l ->
      Tuple (List.map (fun tp -> apply_substitution env tp) l)
  | Int ->
      Int
  | Bool ->
      Bool

let rec occurs (env : (int, ty) env) (id : int) (t : ty) : bool =
  match apply_substitution env t with
  | TVar x ->
      x = id
  | Arrow (t1, t2) ->
      occurs env id t1 || occurs env id t2
  | Tuple l ->
      List.exists (fun tp -> occurs env id tp) l
  | Int | Bool ->
      false

let rec unify (env : (int, ty) env) (t1 : ty) (t2 : ty) : (int, ty) env =
  let t1 = apply_substitution env t1 in
  let t2 = apply_substitution env t2 in
  match (t1, t2) with
  | Int, Int | Bool, Bool ->
      env
  | TVar x, TVar y when x = y ->
      env
  | TVar x, t | t, TVar x ->
      if occurs env x t then failwith "unify: infinite type" else update env x t
  | Arrow (a1, a2), Arrow (b1, b2) ->
      let env = unify env a1 b1 in
      unify env a2 b2
  | _ ->
      failwith "unify: incompatible types"

let op_types (o : op) : ty * ty =
  match o with
  | Add | Sub | Mul | Div ->
      (Int, Int)
  | And | Or ->
      (Bool, Bool)
  | Eq | Leq | Geq | Lt | Gt ->
      (Int, Bool)

let rec infer_type (tenv : (var, ty) env) (sub : (int, ty) env) (e : exp) :
    ty * (int, ty) env =
  match e with
  | Var v -> (
    match lookup tenv v with
    | Some t ->
        (apply_substitution sub t, sub)
    | None ->
        failwith "infer: unbound variable" )
  | Con (Bcon _) ->
      (Bool, sub)
  | Con (Icon _) ->
      (Int, sub)
  | Oapp (o, e1, e2) ->
      let t1, sub = infer_type tenv sub e1 in
      let t2, sub = infer_type tenv sub e2 in
      let expected, result = op_types o in
      let sub = unify sub t1 expected in
      let sub = unify sub t2 expected in
      (result, sub)
  | Lam (x, e) ->
      let t = new_var () in
      let tenv = update tenv x t in
      let bt, sub = infer_type tenv sub e in
      (Arrow (apply_substitution sub t, bt), sub)
  | Fapp (f, e) ->
      let ft, sub = infer_type tenv sub f in
      let t, sub = infer_type tenv sub e in
      let ret = new_var () in
      let sub = unify sub ft (Arrow (t, ret)) in
      (apply_substitution sub ret, sub)
  | If (cond, e1, e2) ->
      let cond_ty, sub = infer_type tenv sub cond in
      let sub = unify sub cond_ty Bool in
      let t1, sub = infer_type tenv sub e1 in
      let t2, sub = infer_type tenv sub e2 in
      let sub = unify sub t1 t2 in
      (apply_substitution sub t1, sub)
  | Let (x, e, e1) ->
      let t, sub = infer_type tenv sub e in
      let tenv = update tenv x (apply_substitution sub t) in
      let t, sub = infer_type tenv sub e1 in
      (t, sub)
  | Letrec (f, x, e, e1) ->
      let ft = new_var () in
      let t = new_var () in
      let tenv' = update (update tenv f ft) x t in
      let rt, sub = infer_type tenv' sub e in
      let sub = unify sub ft (Arrow (apply_substitution sub t, rt)) in
      let tenv = update tenv f (apply_substitution sub ft) in
      let t, sub = infer_type tenv sub e1 in
      (t, sub)
  | Tuple l ->
      let types, sub = infer_tuple tenv sub l in
      (Tuple types, sub)

and infer_tuple tenv sub exps =
  List.fold_left
    (fun (types, sub) exp ->
      let t, sub = infer_type tenv sub exp in
      (types @ [t], sub) )
    ([], sub) exps
