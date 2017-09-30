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
      known_types : string Set.t;
      known_globals : string Set.t;
    }

  let context_of_program tls = {
      scope = [];

      known_types =
        List.enum tls
        |> Enum.filter_map (function
               | TL_Record (_, name, _) -> Some name
               | _ -> None)
        |> Set.of_enum;

      known_globals =
        List.enum tls
        |> Enum.filter_map (function
               | TL_Sig (_, name, _) -> Some name
               | _ -> None)
        |> Set.of_enum;
    }


  let rec tl_resolve ctx = function
    | TL_Sig (pos, name, t) ->
       TL_Sig (pos, name, typ_resolve { ctx with scope = [] } t)

    | TL_Def (pos, name, e) ->
       if Set.mem name ctx.known_globals then
         TL_Def (pos, name, exp_resolve { ctx with scope = [] } e)
       else
         raise_ast_error pos
           (Ast.Exn.UndefVar name)

    | TL_Record (pos, name, flds) ->
       let ctx' = { ctx with scope = [] } in
       let flds' = List.map
                     (fun (pos, name, t) ->
                       (pos, name, typ_resolve ctx' t))
                     flds in
       TL_Record (pos, name, flds')


  and typ_resolve ctx = function
    | T_Named (pos, x) ->
       if Set.mem x ctx.known_types then
         T_Named (pos, x)
       else
         raise_ast_error pos
           (Ast.Exn.UndefType x)

    | T_Fun (ts, t_ret) ->
       T_Fun (List.map (typ_resolve ctx) ts,
              typ_resolve ctx t_ret)


  and exp_resolve ctx = function
    | E_Lit (pos, l) -> E_Lit (pos, l)
    | E_Ref (pos, pth) -> E_Ref (pos, path_resolve ctx pth)
    | E_Move (pos, pth) -> E_Move (pos, path_resolve ctx pth)
    | E_Copy (pos, pth) -> E_Copy (pos, path_resolve ctx pth)
    | E_Anno (e, t) ->
       E_Anno (exp_resolve ctx e,
               typ_resolve { ctx with scope = [] } t)

    | E_App (i, e_fun, e_args) ->
       E_App (i, exp_resolve ctx e_fun,
              List.map (exp_resolve ctx) e_args)

    | E_Do (e_1, e_2) ->
       E_Do (exp_resolve ctx e_1,
             exp_resolve ctx e_2)

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

    | E_MakeStruct (pos, i, flds) ->
       let flds' = List.map
                     (fun (n, e) ->
                       (n, exp_resolve ctx e))
                     flds in
       E_MakeStruct (pos, i, flds')


  and path_resolve ctx = function
    | _ -> raise_ast_error Lexing.dummy_pos Exn.Unimplemented

end
