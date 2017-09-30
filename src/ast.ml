open Batteries

type pos = Lexing.position


type ('var, 'nfo) entire_program
  = ('var, 'nfo) toplvl list

and ('var, 'nfo) toplvl
  = TL_Sig of pos * string * typ
  | TL_Def of pos * string * ('var, 'nfo) exp
  | TL_Record of pos * string * field list

and field = pos * string * typ

(** Types **)
and typ
  = T_Named of pos * string
  | T_Fun of typ list * typ

(** Expressions **)
and ('var, 'nfo) exp
  (* atoms *)
  = E_Lit of pos * lit
  (* path expressions *)
  | E_Ref of pos * ('var, 'nfo) path
  | E_Copy of pos * ('var, 'nfo) path
  | E_Move of pos * ('var, 'nfo) path
  (* simple recursive *)
  | E_Anno of ('var, 'nfo) exp * typ
  | E_App of 'nfo * ('var, 'nfo) exp * ('var, 'nfo) exp list
  | E_Do of ('var, 'nfo) exp * ('var, 'nfo) exp
  (* binding *)
  | E_Let of pos * 'nfo * 'var * ('var, 'nfo) exp * ('var, 'nfo) exp
  | E_Lam of pos * 'nfo * 'var list * ('var, 'nfo) exp
  (* data constructors *)
  | E_MakeStruct of pos * 'nfo * (string * ('var, 'nfo) exp) list

and ('var, 'nfo) path
  = Pa_Var of pos * 'var
  | Pa_Field of ('var, 'nfo) path * string
  | Pa_Expr of ('var, 'nfo) exp

and lit = L_Unit | L_True | L_False | L_Int of int

type info_none = [ `No_info ]
type info_infer = [ info_none
                  | `Struct_typename of string ]


let rec pos_of_exp = function
  | E_Lit (pos, l) -> pos
  | E_Ref (pos, pa) -> pos
  | E_Move (pos, pa) -> pos
  | E_Copy (pos, pa) -> pos
  | E_Anno (e, t) -> pos_of_exp e
  | E_App (i, e_fun, e_args) -> pos_of_exp e_fun
  | E_Do (e_1, e_2) -> pos_of_exp e_1
  | E_Let (pos, i, x, e_rhs, e_body) -> pos
  | E_Lam (pos, i, xs, e) -> pos
  | E_MakeStruct (pos, i, flds) -> pos

and pos_of_path = function
  | Pa_Var (pos, _) -> pos
  | Pa_Field (pa, _) -> pos_of_path pa
  | Pa_Expr e -> pos_of_exp e



module Exn = struct

  type t
    = UndefVar of string
    | UndefType of string
    | ArgCount of int
    | TypeNotFunctionApp
    | TypeNotFunctionLam of string
    | TypeMismatch of string * string
    | TypeCannotInfer of string
    | Unimplemented

  open Printf

  let to_string = function
    | UndefVar x         -> sprintf "undefined variable `%s'" x
    | UndefType x        -> sprintf "undefined type `%s'" x
    | ArgCount n         -> sprintf "expected %d argument%s to function"
                              n (if n = 1 then "" else "s")
    | TypeNotFunctionApp -> "attempt to call non-function value"
    | TypeNotFunctionLam s -> sprintf "unexpected lambda, expected type `%s'" s
    | TypeMismatch (a,b) -> sprintf "expected type `%s', got `%s'" a b
    | TypeCannotInfer e  -> sprintf "cannot infer type of %s" e
    | Unimplemented      -> "unimplemented expression"

end

exception AstError of pos * Exn.t

let raise_ast_error pos ex = raise (AstError (pos, ex))
