open Batteries

module Ident = struct

  (* number used to discriminate bound variables *)
  type discrim = int

  (* free (FV) or bound (BV) variable type.
     bound variables have additional scope information *)
  type t
    = FV of string
    | BV of string * discrim

  (** generate a unique discriminator **)
  let gen_discrim : unit -> discrim
    = let kk = ref 0 in
      (fun () ->
        let k = !kk in
        kk := (k + 1); k)

  (** generate an identifier with a unique discriminator **)
  let gen x = BV (x, gen_discrim ())

  (** compare if two identifiers are equal **)
  let equal a b =
    match (a,b) with
    | FV x, FV y         -> x = y
    | BV (_,i), BV (_,j) -> i = j
    | _, _ -> false

  let hash = function
    | FV x     -> Hashtbl.hash x
    | BV (_,i) -> i

  (** string representation of identifier (omits discriminator **)
  let to_string = function
    | FV x     -> x
    | BV (x,_) -> x

end

type id = Ident.t

module IdTable = Hashtbl.Make(Ident)
type 'v id_table = 'v IdTable.t


module Resolve = struct
  open Ast

  type context = {
      scope : (string * id) list;
      type_reprs : (string, Type.type_repr) Hashtbl.t;
      global_sigs : (string, Type.t) Hashtbl.t;
    }


  let rec tl_resolve ctx = function
    | TL_Sig (pos, name, t) ->
       TL_Sig (pos, name, typ_resolve { ctx with scope = [] } t)

    | TL_Def (pos, name, e) ->
       TL_Def (pos, name, exp_resolve { ctx with scope = [] } e)

    | TL_Record (pos, name, flds) ->
       let ctx' = { ctx with scope = [] } in
       let flds' = List.map
                     (fun (pos, name, t) ->
                       (pos, name, typ_resolve ctx' t))
                     flds in
       TL_Record (pos, name, flds')


  and typ_resolve ctx = function
    | T_Named (pos, x) ->
       if Hashtbl.mem ctx.type_reprs x then
         T_Named (pos, x)
       else
         raise_ast_error pos
           (Ast.Exn.UndefType x)

    | T_Fun (ts, t_ret) ->
       T_Fun (List.map (typ_resolve ctx) ts,
              typ_resolve ctx t_ret)


  and exp_resolve ctx = function
    | E_Lit (pos, l) -> E_Lit (pos, l)
    | E_Ref pa -> E_Ref (path_resolve ctx pa)
    | E_Move pa -> E_Move (path_resolve ctx pa)
    | E_Copy pa -> E_Copy (path_resolve ctx pa)
    | E_Assn (pa, e) -> E_Assn (path_resolve ctx pa,
                                exp_resolve ctx e)

    | E_Anno (e, t) ->
       E_Anno (exp_resolve ctx e,
               typ_resolve { ctx with scope = [] } t)

    | E_App (i, e_fun, e_args) ->
       E_App (i, exp_resolve ctx e_fun,
              List.map (exp_resolve ctx) e_args)

    | E_Prim (o, es) ->
       E_Prim (o, List.map (exp_resolve ctx) es)

    | E_Do (e_1, e_2) ->
       E_Do (exp_resolve ctx e_1,
             exp_resolve ctx e_2)

    | E_If (pos, e_1, e_2, e_3) ->
       E_If (pos,
             exp_resolve ctx e_1,
             exp_resolve ctx e_2,
             exp_resolve ctx e_3)

    | E_While (pos, e_cond, e_body) ->
       E_While (pos,
                exp_resolve ctx e_cond,
                exp_resolve ctx e_body)

    | E_Let (pos, i, x, e_rhs, e_body) ->
       let x' = Ident.gen x in
       let e_rhs' = exp_resolve ctx e_rhs in
       let e_body' = exp_resolve
                       { ctx with
                         scope = (x,x')::ctx.scope }
                       e_body in
       E_Let (pos, i, x',
              e_rhs', e_body')

    | E_Lam (pos, i, xs, e) ->
       let xs' = List.map Ident.gen xs in
       let e' = exp_resolve
                  { ctx with
                    scope = List.combine xs xs' @ ctx.scope }
                  e in
       E_Lam (pos, i, xs', e')

    | E_Rec (pos, i, flds) ->
       let flds' = List.map
                     (fun (n, e) ->
                       (n, exp_resolve ctx e))
                     flds in
       E_Rec (pos, i, flds')


  and path_resolve ctx = function
    | Pa_Var (pos, x) ->
       let x' =
         try
           List.assoc x ctx.scope
         with Not_found ->
           if Hashtbl.mem ctx.global_sigs x then
             Ident.FV x
           else
             raise_ast_error pos (Exn.UndefVar x)
       in
       Pa_Var (pos, x')

    | Pa_Field (i, pa, x) ->
       Pa_Field (i, path_resolve ctx pa, x)

    | Pa_Expr e ->
       Pa_Expr (exp_resolve ctx e)



end
