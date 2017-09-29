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
  | E_Ref of ('var, 'nfo) path
  | E_Copy of ('var, 'nfo) path
  | E_Move of ('var, 'nfo) path
  (* simple recursive *)
  | E_App of 'nfo * ('var, 'nfo) exp stx * ('var, 'nfo) exp
  | E_Do of ('var, 'nfo) exp * ('var, 'nfo) exp
  (* binding *)
  | E_Let of ('var, 'nfo) pat stx * ('var, 'nfo) exp * ('var, 'nfo) exp
  | E_Match of 'nfo * ('var, 'nfo) exp * ('var, 'nfo) match_case list

and ('var, 'nfo) path
  = Var of 'var
  | Field of ('var, 'nfo) exp stx * string stx

and ('var, 'nfo) match_case
  = Case of ('var, 'nfo) pat stx * ('var, 'nfo) exp stx

(** Patterns **)
and ('var, 'nfo) pat
  (* atoms *)
  = P_Lit of lit
  | P_Var of 'var
  | P_Hole
  (* recursive *)
  | P_Ref of ('var, 'nfo) pat stx
  | P_Record of 'nfo * string stx * ('var, 'nfo) pat stx list

and lit = L_Unit | L_True | L_False | L_Int of int
