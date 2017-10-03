open Batteries

type pos = Lexing.position
type 'a stx = pos * 'a

type entire_program = toplevel list

(* NOT BASE *)
and toplevel
  = TL_Sig of string stx * typ
  | TL_Defn of string stx * exp
  | TL_Type of string stx * type_repr

and typ
  = T_Var of string stx
  | T_Sig of typ list * typ

and exp
  = E_Lit of lit stx
  | E_Extern of pos * string
  | E_Ann of exp * typ
  | E_Var of string stx
  | E_Rec of ((string stx) * exp) list stx
  | E_Ref of exp
  | E_Move of exp
  | E_Fieldof of exp * string stx
  | E_App of exp * exp list
  | E_Lam of pos * string stx list * stmt list
  | E_BinOp of Operators.t * exp * exp
  | E_UniOp of Operators.t * exp

and stmt
  = S_Let of string stx * exp
  | S_Reass of exp * exp
  | S_Do of exp
  | S_If of pos * (pos * exp * stmt list) list * stmt list
  | S_While of exp * stmt list
  | S_Nop of pos

and lit
  = L_Unit
  | L_Int of int
  | L_Double of float
  | L_Str of string
  | L_True | L_False

and type_repr = fields

and fields = field list
and field = string stx * typ
