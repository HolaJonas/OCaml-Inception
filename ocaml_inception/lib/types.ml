type const = BCONST of bool | ICONST of int

type token =
  | ADD
  | SUB
  | MUL
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
  | CON of const
  | VAR of string

type var = string

type con = Bcon of bool | Icon of int

type op = Add | Sub | Mul | Leq

type exp =
  | Var of var
  | Con of con
  | Oapp of op * exp * exp
  | Fapp of exp * exp
  | If of exp * exp * exp
  | Lam of var * exp
  | Let of var * exp * exp
  | Letrec of var * var * exp * exp
