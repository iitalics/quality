open Batteries

type t
  = Con of string
  | Ref of t
  | Fun of t list * t

let equal = (=)
let hash = Hashtbl.hash

let rec of_ast = function
  | Ast.T_Named (_, name) -> Con name
  | Ast.T_Fun (ts, t_ret) -> Fun (List.map of_ast ts, of_ast t_ret)

let rec to_string = function
  | Con s -> s
  | Ref t -> "&" ^ to_string t
  | Fun ([], t) -> "-> " ^ to_string t
  | Fun (ts, t) ->
     Printf.sprintf "%s -> %s"
       (String.concat ", " (List.map to_string ts))
       (to_string t)


let builtin_unit = Con "unit"
let builtin_bool = Con "bool"
let builtin_int = Con "int"


type type_repr
  = TR_Builtin of string * int
  | TR_Record of (string * t) list

type type_reprs = (string, type_repr) Hashtbl.t
