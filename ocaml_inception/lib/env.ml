type ('a, 'b) env = 'a -> 'b option

let empty = fun _ -> None

let update (env : ('a, 'b) env) (key : 'a) (value : 'b) : ('a, 'b) env =
 fun k -> if k = key then Some value else env k

let lookup (env : ('a, 'b) env) (key : 'a) : 'b option = env key
