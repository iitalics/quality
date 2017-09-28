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

  (** map the subexpressions/types/patterns in the expression.
      calls the given function on each expression _within_ the given type *)
  val typ_map : (pos -> typ -> typ) -> typ -> typ
  val exp_map : (pos -> exp -> exp) -> exp -> exp
  val pat_map : (pos -> pat -> pat) -> pat -> pat
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
    | TLTypeDefn of string stx * generics * type_defn stx  (* type d<X, ...> = *)
    | TLAnno of string stx * generics * typ stx            (* x : t *)
    | TLDefn of string stx * exp stx                      (* x = e *)
    | TLExtern of string stx * string stx * typ stx        (* extern x = f : t *)

  and generics = string stx list (* <X, ...> *)

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
    | TCon of string * typ stx list                       (* d / d<t, ...> *)
    | TVar of Var.t                                       (* X *)

  (* expressions *)
  and exp =
    (* atomic forms *)
    | ELit of literal                                     (* c *)
    | EVar of Var.t                                       (* x *)
    (* simple recursive forms *)
    | EAnno of exp stx * typ stx                         (* (e : t) *)
    | ERef of exp stx                                    (* &x *)
    | EMove of exp stx                                   (* *x *)
    | EField of exp stx * string stx                     (* x.y *)
    | EApp of exp stx * exp stx list                    (* f(e, ...) *)
    | ECons of string * exp stx list                     (* @C(e, ...) *)
    | ERecord of string * (string stx * exp stx) list    (* R { x=e, ... } *)
    (* variable-introducing forms *)
    | ELet of pat stx * exp stx * exp stx              (* let p = e; e *)
    | ELetVar of Var.t stx * exp stx * exp stx          (* var x = e; e *)
    | ELam of Var.t stx list * exp stx                   (* {x, ... -> e} *)
    | ELamAnno of (Var.t stx * typ stx) list * exp stx   (* {x:t, ... -> e} *)
    (* control flow expressions *)
    | EIf of exp stx * exp stx * exp stx option        (* if e then e [else e] *)
    | EWhile of exp stx * exp stx                       (* while e: e *)
    | EReturn of exp stx                                 (* return e *)
    (* pattern matching *)
    | EMatch of exp stx * (pat stx * exp stx) list      (* match e: p -> e | ... *)

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


  let map_stxs f = List.map (fun s -> f s.pos s.a)

  let typ_map f = function
    | TCon (d, ts) -> TCon (d, map_stxs f ts)
    | atom -> atom

  let exp_map f = function
    | EAnno (e,t) -> EAnno(f e.pos e.a, t)
    | ERef e -> ERef (f e.pos e.a)
    | EMove e -> EMove (f e.pos e.a)
    | EField (e,x) -> EField (f e.pos e.a, x)
    | EApp (e, es) -> EApp (f e.pos e.a, map_stxs f es)
    | ECons (c, es) -> ECons (c, map_stxs f es)
    | ERecord (r, flds) -> ERecord (r, List.map (fun (k, e) -> (k, f e.pos e.a)) flds)
    | ELet (p, e1, e2) -> ELet (p, f e1.pos e1.a, f e2.pos e2.a)
    | ELam (xs, e) -> ELam (xs, f e.pos e.a)
    | ELamAnno (xs, e) -> ELamAnno (xs, f e.pos e.a)
    | EIf (e1, e2, e3) -> EIf (f e1.pos e1.a, f e2.pos e2.a, Option.map (fun e -> f e.pos e.a) e3)
    | EWhile (ec, eb) -> EWhile (f ec.pos ec.a, f eb.pos eb.a)
    | EReturn e -> EReturn (f e.pos e.a)
    | EMatch (e, cs) -> EMatch (f e.pos e.a, List.map (fun (p, e) -> (p, f e.pos e.a)) cs)
    | atom -> atom

  let pat_map f = function
    | PRef p -> PRef (f p.pos p.a)
    | PCons (c, ps) -> PCons (c, map_stxs f ps)
    | PRecord (c, flds) -> PRecord (c, List.map (fun (k, p) -> (k, f p.pos p.a)) flds)
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
  let fresh =
    let next_id = ref 0 in
    (fun x ->
      let id = !next_id in
      next_id := id + 1;
      (x, id))
end

module ScopedExp = ExpAst(ScopedVar)
