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
  | E_Let of 'var * ('var, 'nfo) exp stx * ('var, 'nfo) exp stx

and ('var, 'nfo) path
  = Var of 'var
  | Field of ('var, 'nfo) exp stx * string stx

and lit = L_Unit | L_True | L_False | L_Int of int


module Exn = struct

  type t
    = Undef_var of string

  open Printf

  let to_string = function
    | Undef_var x -> sprintf "undefined variable `%s'" x

end

exception AstError of Exn.t stx
