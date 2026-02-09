open Types

let verify (t : token) (l : token list) =
  match l with
  | x :: tl ->
      if x = t then tl else failwith "verify: wrong token"
  | [] ->
      failwith "verify: no token"

let parse_infix (list : token list) : exp * token list =
  let rec parse_eq (l : token list) : exp * token list =
    match parse_add l with
    | e, EQ :: tl ->
        let e', tl = parse_add tl in
        (Oapp (Eq, e, e'), tl)
    | e, GEQ :: tl ->
        let e', tl = parse_add tl in
        (Oapp (Geq, e, e'), tl)
    | e, LEQ :: tl ->
        let e', tl = parse_add tl in
        (Oapp (Leq, e, e'), tl)
    | e, G :: tl ->
        let e', tl = parse_add tl in
        (Oapp (Gt, e, e'), tl)
    | e, L :: tl ->
        let e', tl = parse_add tl in
        (Oapp (Lt, e, e'), tl)
    | s ->
        s
  and parse_add (l : token list) : exp * token list = parse_add' (parse_mul l)
  and parse_add' (t : exp * token list) : exp * token list =
    match t with
    | e, ADD :: tl ->
        let e', tl = parse_mul tl in
        parse_add' (Oapp (Add, e, e'), tl)
    | e, SUB :: tl ->
        let e', tl = parse_mul tl in
        parse_add' (Oapp (Add, e, e'), tl)
    | s ->
        s
  and parse_mul (l : token list) : exp * token list =
    parse_mul' (parse_literal l)
  and parse_mul' (t : exp * token list) : exp * token list =
    match t with
    | e, MUL :: tl ->
        let e', tl = parse_literal tl in
        parse_mul' (Oapp (Mul, e, e'), tl)
    | s ->
        s
  and parse_literal (l : token list) : exp * token list =
    match l with
    | CON (BCONST x) :: tl ->
        (Con (Bcon x), tl)
    | CON (ICONST x) :: tl ->
        (Con (Icon x), tl)
    | VAR x :: tl ->
        (Var x, tl)
    | LP :: tl -> (
      match parse_eq tl with
      | e, RP :: tl ->
          (e, tl)
      | _ ->
          failwith "parse_literal: missing RP" )
    | _ ->
        failwith "parse_literal: error while parsing literal"
  in
  parse_eq list

let rec parse (list : token list) : exp * token list =
  match list with
  | LET :: REC :: VAR f :: VAR x :: EQ :: tl ->
      let e, tl = parse tl in
      let e', tl = parse (verify IN tl) in
      (Letrec (f, x, e, e'), tl)
  | LET :: VAR x :: EQ :: tl ->
      let e, tl = parse tl in
      let e', tl = parse (verify IN tl) in
      (Let (x, e, e'), tl)
  | IF :: tl ->
      let e, tl = parse tl in
      let e', tl = parse (verify THEN tl) in
      let e'', tl = parse (verify ELSE tl) in
      (If (e, e', e''), tl)
  | LAM :: VAR x :: ARR :: tl ->
      let e', tl = parse tl in
      (Lam (x, e'), tl)
  | tl ->
      parse_infix tl
