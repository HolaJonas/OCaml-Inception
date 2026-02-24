open Types
open Env

let op_values (op : op) (v1 : value) (v2 : value) : value =
  match (op, v1, v2) with
  | Add, Ival v1, Ival v2 ->
      Ival (v1 + v2)
  | Sub, Ival v1, Ival v2 ->
      Ival (v1 - v2)
  | Mul, Ival v1, Ival v2 ->
      Ival (v1 * v2)
  | Div, Ival v1, Ival v2 ->
      Ival (v1 / v2)
  | And, Bval v1, Bval v2 ->
      Bval (v1 && v2)
  | Or, Bval v1, Bval v2 ->
      Bval (v1 || v2)
  | Eq, Ival v1, Ival v2 ->
      Bval (v1 = v2)
  | Leq, Ival v1, Ival v2 ->
      Bval (v1 <= v2)
  | Lt, Ival v1, Ival v2 ->
      Bval (v1 < v2)
  | Geq, Ival v1, Ival v2 ->
      Bval (v1 >= v2)
  | Gt, Ival v1, Ival v2 ->
      Bval (v1 > v2)
  | _, _, _ ->
      failwith "op_values: incompatible types"

let rec evaluate (env : (var, value) env) (e : exp) =
  match e with
  | Con (Bcon v) ->
      Bval v
  | Con (Icon v) ->
      Ival v
  | Var s -> (
    match lookup env s with
    | Some s ->
        s
    | None ->
        failwith "evaluate: unbound variable" )
  | If (e1, e2, e3) -> (
    match evaluate env e1 with
    | Bval v ->
        if v then evaluate env e2 else evaluate env e3
    | _ ->
        failwith "evaluate: if must have a bool as first attribute" )
  | Oapp (op, e1, e2) ->
      op_values op (evaluate env e1) (evaluate env e2)
  | Let (x, e1, e2) ->
      evaluate (update env x (evaluate env e1)) e2
  | Letrec (f, x, e1, e2) ->
      evaluate (update env f (Rclosure (f, x, e1, env))) e2
  | Lam (x, e) ->
      Closure (x, e, env)
  | Fapp (e1, e2) -> (
      let v1 = evaluate env e1 in
      let v2 = evaluate env e2 in
      match v1 with
      | Closure (x, e, env) ->
          evaluate (update env x v2) e
      | Rclosure (f, x, e, env) ->
          evaluate (update (update env f v1) x v2) e
      | _ ->
          failwith "evaluate: No closure in Fapp" )
