open Batteries

type pos = Lexing.position

(* some AST with a source pos attached *)
type 'a stx = {
    pos : pos;
    a : 'a;
  }

(* AST data types: *)
type entire_program = toplevel list

and ident = string stx

(* toplevel definitions *)
and toplevel =
  | TLTypeDefn of ident * generics * type_defn stx     (* type d<X, ...> = *)
  | TLAnno of string * generics * typ stx              (* x : t *)
  | TLDefn of string * expr stx                        (* x = e *)
  | TLExtern of ident * ident * typ stx                (* extern x = f : t *)

and generics = ident list (* <X, ...> *)

(* kinds of type definitions *)
and type_defn =
  | Record of field_defn list                           (* { y : t, ... } *)
  | Union of ctor_defn list                             (* @c(t, ...) | ... *)

and field_defn = { fld_name : ident;
                   fld_type : typ stx }
and ctor_defn = { ctor_name : ident;
                  ctor_args : typ stx list }

(* types *)
and typ =
  | TCon of string * typ stx list                       (* d / d<t, ...> *)
  | TVar of string                                      (* X *)

(* expressions *)
and expr =
  (* atomic forms *)
  | ELit of literal                                     (* c *)
  | EVar of string                                      (* x *)
  (* simple recursive forms *)
  | EAnno of expr stx * typ stx                         (* (e : t) *)
  | ERef of expr stx                                    (* &x *)
  | EMove of expr stx                                   (* *x *)
  | EField of expr stx * ident                          (* x.y *)
  | EApp of expr stx * expr stx list                    (* f(e, ...) *)
  | ECons of string * expr stx list                     (* @C(e, ...) *)
  | ERecord of string * (ident * expr stx) list         (* R { x=e, ... } *)
  (* variable-introducing forms *)
  | ELet of ident * pat stx * expr stx                  (* let p = e; e *)
  | ELetVar of ident * expr stx * expr stx              (* var x = e; e *)
  | ELam of ident list * expr stx                       (* {x, ... -> e} *)
  | ELamAnno of (ident * typ stx) list * expr stx       (* {x:t, ... -> e} *)
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
  | PVar of string                                      (* x *)
  | PWildcard                                           (* _ *)
  (* recursive forms *)
  | PRef of pat                                         (* &p *)
  | PCons of string * pat stx list                      (* @C(e, ...) *)
  | PRecord of string * (ident * pat stx) list          (* R { x=p, ... } *)

(* literals (used by both patterns & expressions) *)
and literal =
  | LInt of Big_int.t                                   (* 123 *)
  | LFloat of Float.t                                   (* 1.23 *)
  | LTrue                                               (* true *)
  | LFalse                                              (* true *)
