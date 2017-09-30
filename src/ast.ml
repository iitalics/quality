open Batteries

type pos = Lexing.position
type 'a stx = pos * 'a


type ('var, 'nfo) entire_program
  = ('var, 'nfo) toplevel list

and ('var, 'nfo) toplevel
  = TL_Anno of string stx * typ stx
  | TL_Defn of string stx * ('var, 'nfo) exp stx
  | TL_Record of string stx

(** Types **)
and typ
  = T_Named of string

(** Expressions **)
and ('var, 'nfo) exp
  (* atoms *)
  = E_Lit of lit
  (* path expressions *)
  | E_Ref of ('var, 'nfo) path stx
  | E_Copy of ('var, 'nfo) path stx
  | E_Move of ('var, 'nfo) path stx
  (* simple recursive *)
  | E_App of 'nfo * ('var, 'nfo) exp stx * ('var, 'nfo) exp stx list
  | E_Do of ('var, 'nfo) exp stx * ('var, 'nfo) exp stx
  (* binding *)
  | E_Let of 'var stx * ('var, 'nfo) exp stx * ('var, 'nfo) exp stx
  | E_Lam of 'var stx list * ('var, 'nfo) exp stx

and ('var, 'nfo) path = 'var

and lit = L_Unit | L_True | L_False | L_Int of int


module Exn = struct

  type t
    = Undef_var of string
    | ArgCount of int
    | TypeNotFunctionApp
    | TypeNotFunctionLam of string
    | TypeMismatch of string * string
    | TypeCannotInfer of string
    | Unimplemented

  open Printf

  let to_string = function
    | Undef_var x        -> sprintf "undefined variable `%s'" x
    | ArgCount n         -> sprintf "expected %d argument%s to function"
                              n (if n = 1 then "" else "s")
    | TypeNotFunctionApp -> "attempt to call non-function value"
    | TypeNotFunctionLam s -> sprintf "unexpected lambda, expected type `%s'" s
    | TypeMismatch (a,b) -> sprintf "expected type `%s', got `%s'" a b
    | TypeCannotInfer e  -> sprintf "cannot infer type of %s" e
    | Unimplemented      -> "unimplemented expression"

end

exception AstError of Exn.t stx

let raise_ast_error pos ex = raise (AstError (pos, ex))
