open Env
open Lexer
open Parser
open Type_check

let interpret s =
  let tokens = lex s in
  let ast = (fun (a, _) -> a) (parse tokens) in
  let typ = infer_type empty empty ast in
  typ
