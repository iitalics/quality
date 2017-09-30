open Batteries

type pos = Ast.pos
type 'a stx = pos * 'a

type entire_program = toplevel stx list

and toplevel
  = TL_Decl of string stx * typ stx
  | TL_Defn of string stx * exp stx
  | TL_Struct of string stx * field list

and field = string stx * typ stx

and typ
  = T_Con of string stx * typ stx list
  | T_Var of string

and exp
  = E_Lit of lit
  | E_Var of string
  | E_Ref of exp stx
  | E_Move of exp stx
  | E_Fieldof of exp stx * string stx
  | E_App of exp stx * exp stx list
  | E_Lam of string stx list * stmt stx list

and stmt
  = S_Let of string stx * exp stx
  | S_Do of exp stx
  | S_If of exp stx * stmt stx list * stmt stx list
  | S_While of exp stx * stmt stx list
  | S_Nop

and lit
  = L_Unit
  | L_Int of int
  | L_True | L_False
