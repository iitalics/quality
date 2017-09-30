open Batteries

type pos = Lexing.position
type 'a stx = pos * 'a

type entire_program = toplevel stx list

(* NOT BASE *)
and toplevel
  = TL_Sig of string stx * typ
  | TL_Defn of string stx * exp
  | TL_Type of string stx * type_repr

and typ
  = T_Var of string stx
  | T_Sig of typ list * typ

and exp
  = E_Lit of lit
  | E_Var of string stx
  | E_Ref of exp
  | E_Move of exp
  | E_Fieldof of exp * string stx
  | E_App of exp * exp list
  | E_Lam of string stx list * stmt list

and stmt
  = S_Let of bool * string stx * exp
  | S_Do of exp
  | S_If of exp * stmt list * stmt list
  | S_While of exp * stmt list
  | S_Nop

and lit
  = L_Unit
  | L_Int of int
  | L_True | L_False

and type_repr = fields

and fields = field list
and field = string stx * typ


let rec get_string_typ t =
  let to_print =
    match t with
    | T_Var(v) -> snd v
    | T_Sig(s1,s2) -> (List.iter get_string_typ s1);
                      get_string_typ s2; "sig"
  in print_string to_print
;;

let get_string_lit l =
  let to_print =
    match l with
    | L_Unit -> "Unit"
    | L_Int(i) -> string_of_int i
    | L_True -> "True"
    | L_False -> "False"
  in print_string to_print
;;

let rec get_string_stmt s =
  let to_print =
    match s with
    | S_Let(_,s,e) -> get_string_exp e; snd s
    | S_Do(e) -> get_string_exp e; "do"
    | S_If(e1,s2,s3) -> get_string_exp e1;
                        (List.iter get_string_stmt s2);
                        (List.iter get_string_stmt s3);
                        "if"
    | S_While(e1,s2) -> get_string_exp e1;
                        (List.iter get_string_stmt s2);
                        "while"
    | S_Nop -> "nop"
  in print_string to_print
and get_string_exp e =
  let to_print =
    match e with
    | E_Lit(l) -> get_string_lit l; "lit"
    | E_Var(v) -> snd v
    | E_Ref(e) -> get_string_exp e; "ref"
    | E_Move(e) -> get_string_exp e; "move"
    | E_Fieldof(e,s) -> get_string_exp e; snd s
    | E_App(e,el) -> get_string_exp e;
                     (List.iter get_string_exp el); "App"
    | E_Lam(sl,stl) ->
       (List.iter (fun x -> print_string (snd x)) sl);
       (List.iter get_string_stmt stl); "Lam"
  in print_string to_print
;;

let get_string_typr t =
  (List.iter (fun x -> print_string (snd (fst x));
                       get_string_typ (snd x))
     t)
;;

let rec get_string_top x =
  let to_print =
    match x with
    | TL_Sig(s,t) -> (get_string_typ t); snd s
    | TL_Defn(s,e) -> (get_string_exp e); snd s
    | TL_Type(s,t) -> (get_string_typr t); snd s
  in print_string to_print
;;

let get_string x =
  (List.iter (fun x -> get_string_top (snd x);
     print_int (fst x).Lexing.pos_cnum) x)
;;
