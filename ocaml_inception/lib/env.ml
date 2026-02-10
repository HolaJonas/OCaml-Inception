open Types

type 'a env = var -> 'a option

let empty = fun _ -> None

let update (env : 'a env) (key : var) (value : 'a) : 'a env =
 fun k -> if k = key then Some value else env k

let lookup (env : 'a env) (key : var) : 'a option = env key
