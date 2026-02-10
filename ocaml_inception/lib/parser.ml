open Types

let verify (t : token) (l : token list) =
  match l with
  | x :: tl ->
      if x = t then tl else failwith "verify: wrong token"
  | [] ->
      failwith "verify: no token"

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
      parse_eq tl

and parse_eq (l : token list) : exp * token list =
  match parse_or l with
  | e, EQ :: tl ->
      let e', tl = parse_or tl in
      (Oapp (Eq, e, e'), tl)
  | e, GEQ :: tl ->
      let e', tl = parse_or tl in
      (Oapp (Geq, e, e'), tl)
  | e, LEQ :: tl ->
      let e', tl = parse_or tl in
      (Oapp (Leq, e, e'), tl)
  | e, G :: tl ->
      let e', tl = parse_or tl in
      (Oapp (Gt, e, e'), tl)
  | e, L :: tl ->
      let e', tl = parse_or tl in
      (Oapp (Lt, e, e'), tl)
  | s ->
      s

and parse_or (l : token list) : exp * token list = parse_or' (parse_and l)

and parse_or' (t : exp * token list) : exp * token list =
  match t with
  | e, OR :: tl ->
      let e', tl = parse_and tl in
      parse_or' (Oapp (Or, e, e'), tl)
  | s ->
      s

and parse_and (l : token list) : exp * token list = parse_and' (parse_add l)

and parse_and' (t : exp * token list) : exp * token list =
  match t with
  | e, AND :: tl ->
      let e', tl = parse_add tl in
      parse_and' (Oapp (And, e, e'), tl)
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
      parse_add' (Oapp (Sub, e, e'), tl)
  | s ->
      s

and parse_mul (l : token list) : exp * token list = parse_mul' (parse_funcapp l)

and parse_mul' (t : exp * token list) : exp * token list =
  match t with
  | e, MUL :: tl ->
      let e', tl = parse_funcapp tl in
      parse_mul' (Oapp (Mul, e, e'), tl)
  | e, DIV :: tl ->
      let e', tl = parse_funcapp tl in
      parse_mul' (Oapp (Div, e, e'), tl)
  | s ->
      s

and parse_funcapp (l : token list) : exp * token list =
  parse_funcapp' (parse_atom l)

and parse_funcapp' (t : exp * token list) : exp * token list =
  match t with
  | e, (CON _ | VAR _ | LP) :: _ ->
      let e', tl = parse_atom ((fun (_, b) -> b) t) in
      parse_funcapp' (Fapp (e, e'), tl)
  | s ->
      s

and parse_atom (l : token list) : exp * token list =
  match l with
  | CON (BCONST x) :: tl ->
      (Con (Bcon x), tl)
  | CON (ICONST x) :: tl ->
      (Con (Icon x), tl)
  | VAR x :: tl ->
      (Var x, tl)
  | LP :: tl -> (
    match parse tl with
    | e, RP :: tl ->
        (e, tl)
    | _ ->
        failwith "parse_atom: missing RP" )
  | _ ->
      failwith "parse_atom: error while parsing literal"
