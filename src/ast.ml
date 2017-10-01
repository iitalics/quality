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
  | E_Ref of ('var, 'nfo) path
  | E_Copy of ('var, 'nfo) path
  | E_Move of ('var, 'nfo) path
  | E_Assn of ('var, 'nfo) path * ('var, 'nfo) exp
  (* simple recursive *)
  | E_Anno of ('var, 'nfo) exp * typ
  | E_App of 'nfo * ('var, 'nfo) exp * ('var, 'nfo) exp list
  | E_Prim of Operators.t * ('var, 'nfo) exp list
  (* binding *)
  | E_Let of pos * 'nfo * 'var * ('var, 'nfo) exp * ('var, 'nfo) exp
  | E_Lam of pos * 'nfo * 'var list * ('var, 'nfo) exp
  (* data constructors *)
  | E_Rec of pos * 'nfo * (string * ('var, 'nfo) exp) list
  (* control flow *)
  | E_Do of ('var, 'nfo) exp * ('var, 'nfo) exp
  | E_If of pos * ('var, 'nfo) exp * ('var, 'nfo) exp * ('var, 'nfo) exp
  | E_While of pos * ('var, 'nfo) exp * ('var, 'nfo) exp

and ('var, 'nfo) path
  = Pa_Var of pos * 'var
  | Pa_Field of 'nfo * ('var, 'nfo) path * string
  | Pa_Expr of ('var, 'nfo) exp

and lit = L_Unit | L_True | L_False | L_Int of int

type info_none = [ `No_info ]


let rec pos_of_exp = function
  | E_Lit (pos, l) -> pos
  | E_Ref pa -> pos_of_path pa
  | E_Move pa -> pos_of_path pa
  | E_Copy pa -> pos_of_path pa
  | E_Assn (pa, e) -> pos_of_path pa
  | E_Anno (e, t) -> pos_of_exp e
  | E_App (i, e_fun, e_args) -> pos_of_exp e_fun
  | E_Prim(_, es) -> pos_of_exp (List.hd es)
  | E_Do (e_1, e_2) -> pos_of_exp e_1
  | E_If (pos, e_1, e_2, e_3) -> pos
  | E_While (pos, e_cond, e_body) -> pos
  | E_Let (pos, i, x, e_rhs, e_body) -> pos
  | E_Lam (pos, i, xs, e) -> pos
  | E_Rec (pos, i, flds) -> pos

and pos_of_path = function
  | Pa_Var (pos, _) -> pos
  | Pa_Field (_, pa, _) -> pos_of_path pa
  | Pa_Expr e -> pos_of_exp e



module Exn = struct

  type t
    = UndefVar of string
    | UndefType of string
    | Redef of string
    | SigMissing of string
    | ArgCount of int
    | TypeNotFunctionApp of string
    | TypeNotFunctionLam of string
    | TypeUnexpectRecord
    | TypeNoField of string
    | TypeMismatch of string * string
    | TypeCannotInfer of string
    | Unimplemented

  open Printf

  let to_string = function
    | UndefVar x         -> sprintf "undefined variable `%s'" x
    | UndefType x        -> sprintf "undefined type `%s'" x
    | Redef x            -> sprintf "attempt to redefine `%s'" x
    | SigMissing x       -> sprintf "missing signature for `%s'" x
    | ArgCount n         -> sprintf "expected %d argument%s to function"
                              n (if n = 1 then "" else "s")
    | TypeNotFunctionApp s -> sprintf "attempt to call non-function type `%s'" s
    | TypeNotFunctionLam s -> sprintf "unexpected lambda, expected type `%s'" s
    | TypeUnexpectRecord -> "unexpected struct here"
    | TypeNoField s -> sprintf "type does not contain field `%s'" s
    | TypeMismatch (a,b) -> sprintf "expected type `%s', got `%s'" a b
    | TypeCannotInfer e  -> "cannot infer type of " ^ e
    | Unimplemented      -> "unimplemented expression"

end

exception AstError of pos * Exn.t

let raise_ast_error pos ex = raise (AstError (pos, ex))
