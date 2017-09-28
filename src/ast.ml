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
module type ExpAstSig = sig
  type typ
  type exp
  type pat
  type var

  (** map the subexpressions/types/patterns in the expression.
      calls the given function on each expression _within_ the given type *)
  val exp_map : (exp stx -> exp stx) -> exp -> exp
  val pat_map : (pat stx -> pat stx) -> pat -> pat
end

(** module signature for variables *)
module type VarAstSig = sig
  type t
  val to_string : t -> string
  val equal : t -> t -> bool
end


(* generic toplevel ast given the types of expressions *)
module ToplevelAst(Exp_ : ExpAstSig) = struct

  module Exp = Exp_
  open Exp

  (* the entire program is just a list of expressions *)
  type entire_program = toplevel list

  (* toplevel definition type *)
  and toplevel =
    | TLTypeDefn of string * type_defn stx  (* type d<X, ...> = *)
    | TLAnno of string stx * typ stx            (* x : t *)
    | TLDefn of string stx * exp stx                      (* x = e *)
    | TLExtern of string stx * string stx * typ stx        (* extern x = f : t *)

  and type_defn =
    | Record of field list
    | Union of ctor list

  and field = Field of string stx * typ stx               (* { y : t, ... } *)
  and ctor = Ctor of string stx * typ stx list            (* @c(t, ...) | ... *)

end


(** generic expression ast *)
module ExpAst(Var_ : VarAstSig) = struct

  module Var = Var_
  type var = Var.t

  (* types *)
  type typ =
    | TCon of string                                     (* d *)
    | TVar of Var.t                                      (* X *)

  (* expressions *)
  and exp =
    (* atomic forms *)
    | ELit of literal                                    (* c *)
    | EVar of Var.t                                      (* x *)
    (* simple recursive forms *)
    | EAnno of exp stx * typ stx                         (* (e : t) *)
    | ERef of exp stx                                    (* &x *)
    | EMove of exp stx                                   (* *x *)
    | EField of exp stx * string stx                     (* x.y *)
    | EApp of exp stx * exp stx list                     (* f(e, ...) *)
    | ECons of string * exp stx list                     (* @C(e, ...) *)
    | ERecord of string * (string stx * exp stx) list    (* R { x=e, ... } *)
    (* variable-introducing forms *)
    | ELet of pat stx * exp stx * exp stx                (* let p = e; e *)
    | ELetVar of Var.t stx * exp stx * exp stx           (* var x = e; e *)
    | ELam of Var.t stx list * exp stx                   (* {x, ... -> e} *)
    | ELamAnno of (Var.t stx * typ stx) list * exp stx   (* {x:t, ... -> e} *)
    (* control flow expressions *)
    | EIf of exp stx * exp stx * exp stx option          (* if e then e [else e] *)
    | EWhile of exp stx * exp stx                        (* while e: e *)
    | EReturn of exp stx                                 (* return e *)
    (* pattern matching *)
    | EMatch of exp stx * (pat stx * exp stx) list       (* match e: p -> e | ... *)

  (* patterns *)
  and pat =
    (* atomic forms *)
    | PLit of literal                                     (* p *)
    | PVar of Var.t                                       (* x *)
    | PWildcard                                           (* _ *)
    (* recursive forms *)
    | PRef of pat stx                                     (* &p *)
    | PCons of string * pat stx list                      (* @C(e, ...) *)
    | PRecord of string * (string stx * pat stx) list     (* R { x=p, ... } *)

  (* literals (used by both patterns & expressions) *)
  and literal =
    | LUnit                                               (* () *)
    | LInt of int                                         (* 123 *)
    | LFloat of float                                     (* 1.23 *)
    | LTrue                                               (* true *)
    | LFalse                                              (* true *)


  let exp_map f =
    function
    | EAnno (e,t) -> EAnno(f e, t)
    | ERef e -> ERef (f e)
    | EMove e -> EMove (f e)
    | EField (e,x) -> EField (f e, x)
    | EApp (e, es) -> EApp (f e, List.map f es)
    | ECons (c, es) -> ECons (c, List.map f es)
    | ERecord (r, flds) -> ERecord (r, List.map (fun (k, e) -> (k, f e)) flds)
    | ELet (p, e1, e2) -> ELet (p, f e1, f e2)
    | ELam (xs, e) -> ELam (xs, f e)
    | ELamAnno (xs, e) -> ELamAnno (xs, f e)
    | EIf (e1, e2, e3) -> EIf (f e1, f e2, Option.map (fun e -> f e) e3)
    | EWhile (ec, eb) -> EWhile (f ec, f eb)
    | EReturn e -> EReturn (f e)
    | EMatch (e, cs) -> EMatch (f e, List.map (fun (p, e) -> (p, f e)) cs)
    | atom -> atom

  let pat_map f =
    function
    | PRef p -> PRef (f p)
    | PCons (c, ps) -> PCons (c, List.map f ps)
    | PRecord (c, flds) -> PRecord (c, List.map (fun (k, p) -> (k, f p)) flds)
    | atom -> atom

end


(** raw ast = ast where variables are just strings *)
module RawVar = struct
  type t = string
  let to_string s = s
  let equal = String.equal
end

module RawExp = ExpAst(RawVar)
module RawAst = ToplevelAst(RawExp)

let pi_defn =
  let open RawAst in
  let open RawExp in
  TLDefn (stx nopos "pi",
          stx nopos (ELit (LFloat 3.14159)))

(** scoped variables, which have a number to distinguish them *)
module ScopedVar = struct
  type t = string * int

  let to_string (s,_) = s
  let equal (_,i) (_,j) = (i = j)

  (* generate a fresh identifier with the given name *)
  let fresh : string -> t =
    let next_id = ref 0 in
    (fun x ->
      let id = !next_id in
      next_id := id + 1;
      (x, id))
end

module ScopedExp = ExpAst(ScopedVar)

let f =
  let open ScopedExp in
  let open ScopedVar in
  let x' = fresh "x" in
  ELetVar
    (stx nopos x',
     stx nopos (ELit (LInt 3)),
     stx nopos (EVar x'))
