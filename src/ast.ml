open Batteries

type pos = Lexing.position

(* some Ast with a source pos attached *)
type 'a stx = {
    pos : pos;
    a : 'a;
  }

let nopos = { Lexing.pos_fname = "...";
              Lexing.pos_lnum = 0;
              Lexing.pos_cnum = 0;
              Lexing.pos_bol = 0; }

let stx p a = { pos = p; a = a; }
let unstx s = s.pos, s.a


(** module signature for expressions (including types and patterns) *)
module type ExprAstSig = sig
  type expr
  type typ
  type pat
end

(** module signature for variables *)
module type VarAstSig = sig
  type t
  val to_string : t -> string
  val equal : t -> t -> bool
end


(* generic toplevel ast given the types of expressions *)
module ToplevelAst(Expr_ : ExprAstSig) = struct

  module Expr = Expr_
  open Expr

  (* the entire program is just a list of expressions *)
  type entire_program = toplevel list

  (* toplevel definition type *)
  and toplevel =
    | TLTypeDefn of string stx * generics * type_defn stx  (* type d<X, ...> = *)
    | TLAnno of string stx * generics * typ stx            (* x : t *)
    | TLDefn of string stx * expr stx                      (* x = e *)
    | TLExtern of string stx * string stx * typ stx        (* extern x = f : t *)

  and generics = string stx list (* <X, ...> *)

  and type_defn =
    | Record of field list
    | Union of ctor list

  and field = Field of string stx * typ stx               (* { y : t, ... } *)
  and ctor = Ctor of string stx * typ stx list            (* @c(t, ...) | ... *)

end


(** generic expression ast *)
module ExprAst(Var_ : VarAstSig) = struct

  module Var = Var_
  type var = Var.t

  (* types *)
  type typ =
    | TCon of string * typ stx list                       (* d / d<t, ...> *)
    | TVar of Var.t                                       (* X *)

  (* expressions *)
  and expr =
    (* atomic forms *)
    | ELit of literal                                     (* c *)
    | EVar of Var.t                                       (* x *)
    (* simple recursive forms *)
    | EAnno of expr stx * typ stx                         (* (e : t) *)
    | ERef of expr stx                                    (* &x *)
    | EMove of expr stx                                   (* *x *)
    | EField of expr stx * string stx                     (* x.y *)
    | EApp of expr stx * expr stx list                    (* f(e, ...) *)
    | ECons of string * expr stx list                     (* @C(e, ...) *)
    | ERecord of string * (string stx * expr stx) list    (* R { x=e, ... } *)
    (* variable-introducing forms *)
    | ELet of Var.t stx * pat stx * expr stx              (* let p = e; e *)
    | ELetVar of Var.t stx * expr stx * expr stx          (* var x = e; e *)
    | ELam of Var.t stx list * expr stx                   (* {x, ... -> e} *)
    | ELamAnno of (Var.t stx * typ stx) list * expr stx   (* {x:t, ... -> e} *)
    (* control flow expressions *)
    | EIf of expr stx * expr stx * expr stx option        (* if e then e [else e] *)
    | EWhile of expr stx * expr stx                       (* while e: e *)
    | EReturn of expr stx                                 (* return e *)
    (* pattern matching *)
    | EMatch of expr stx * (pat stx * expr stx) list      (* match e: p -> e | ... *)

  (* patterns *)
  and pat =
    (* atomic forms *)
    | PLit of literal                                     (* p *)
    | PVar of Var.t                                       (* x *)
    | PWildcard                                           (* _ *)
    (* recursive forms *)
    | PRef of pat                                         (* &p *)
    | PCons of string * pat stx list                      (* @C(e, ...) *)
    | PRecord of string * (Var.t stx * pat stx) list      (* R { x=p, ... } *)

  (* literals (used by both patterns & expressions) *)
  and literal =
    | LUnit                                               (* () *)
    | LInt of int                                         (* 123 *)
    | LFloat of float                                     (* 1.23 *)
    | LTrue                                               (* true *)
    | LFalse                                              (* true *)

end


(** raw ast = ast where variables are just strings *)
module RawVar = struct
  type t = string
  let to_string s = s
  let equal = String.equal
end

module RawExpr = ExprAst(RawVar)
module RawAst = ToplevelAst(RawExpr)

let pi_defn =
  let open RawAst in
  let open RawExpr in
  TLDefn (stx nopos "pi",
          stx nopos (ELit (LFloat 3.14159)))



(** scoped variables, which have a number to distinguish them *)
module ScopedVar = struct
  type t = string * int

  let to_string (s,_) = s
  let equal (_,i) (_,j) = (i = j)

  (* generate a fresh identifier with the given name *)
  let fresh =
    let next_id = ref 0 in
    (fun x ->
      let id = !next_id in
      next_id := id + 1;
      (x, id))
end

module ScopedExpr = ExprAst(ScopedVar)
