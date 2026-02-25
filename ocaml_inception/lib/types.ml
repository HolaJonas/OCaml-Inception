open Env

type const = BCONST of bool | ICONST of int

type token =
  | ADD
  | SUB
  | MUL
  | DIV
  | OR
  | AND
  | LP
  | RP
  | EQ
  | LEQ
  | L
  | GEQ
  | G
  | ARR
  | IF
  | THEN
  | ELSE
  | LAM
  | LET
  | IN
  | REC
  | COMMA
  | CON of const
  | VAR of string

type var = string

type con = Bcon of bool | Icon of int

type op = Add | Sub | Mul | Div | Leq | Geq | Gt | Lt | Eq | And | Or

type exp =
  | Var of var
  | Con of con
  | Oapp of op * exp * exp
  | Fapp of exp * exp
  | If of exp * exp * exp
  | Lam of var * exp
  | Let of var * exp * exp
  | Letrec of var * var * exp * exp
  | Tuple of exp list

type ty = Int | Bool | Arrow of ty * ty | TVar of int | Tuple of ty list

type scheme = Forall of int list * ty

let counter = ref 0

let new_var () : ty =
  let n = !counter in
  incr counter ; TVar n

type value =
  | Bval of bool
  | Ival of int
  | Tuple of value list
  | Closure of var * exp * (var, value) env
  | Rclosure of var * var * exp * (var, value) env
