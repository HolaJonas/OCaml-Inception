open Types
open String
open List

let next_char_eq s i =
  if i + 1 < String.length s then get s (i + 1) = '=' else false

let is_whitespace (c : char) : bool =
  match c with ' ' | '\n' | '\t' -> true | _ -> false

let get_full_string (s : string) (i : int) : string * int =
  let rec aux (i' : int) ((substr : string), (length : int)) : string * int =
    if i' >= String.length s then (substr, length)
    else
      let x = get s i' in
      if is_whitespace x then (substr, length)
      else aux (i' + 1) (substr ^ String.make 1 x, length + 1)
  in
  aux i ("", 0)

let char_is_numeric (c : char) : bool =
  match c with
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      true
  | _ ->
      false

let string_is_numeric (s : string) : bool =
  String.fold_left (fun a b -> a && char_is_numeric b) true s

let lex (s : string) : token list =
  let whole_length = String.length s in
  let rec lex' (i : int) (l : token list) =
    if i >= whole_length then l
    else
      match get s i with
      | '+' ->
          lex' (i + 1) (ADD :: l)
      | '*' ->
          lex' (i + 1) (MUL :: l)
      | '=' ->
          lex' (i + 1) (EQ :: l)
      | '(' ->
          lex' (i + 1) (LP :: l)
      | ')' ->
          lex' (i + 1) (RP :: l)
      | '<' ->
          if next_char_eq s i then lex' (i + 2) (LEQ :: l)
          else lex' (i + 1) (L :: l)
      | '>' ->
          if next_char_eq s i then lex' (i + 2) (GEQ :: l)
          else lex' (i + 1) (G :: l)
      | '-' ->
          if i + 1 < whole_length && get s (i + 1) = '>' then
            lex' (i + 2) (ARR :: l)
          else lex' (i + 1) (SUB :: l)
      | x -> (
          if is_whitespace x then lex' (i + 1) l
          else
            let str, ln = get_full_string s i in
            if string_is_numeric str then
              lex' (i + ln) (CON (ICONST (int_of_string str)) :: l)
            else
              match str with
              | "true" ->
                  lex' (i + ln) (CON (BCONST true) :: l)
              | "false" ->
                  lex' (i + ln) (CON (BCONST false) :: l)
              | x ->
                  lex' (i + ln) (VAR x :: l) )
  in
  List.rev (lex' 0 [])
