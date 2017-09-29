open Batteries

module Ident = struct

  (* number used to discriminate bound variables *)
  type discrim = int

  (* free (FV) or bound (BV) variable type.
     bound variables have additional scope information *)
  type t
    = FV of string
    | BV of string * discrim

  (** generate an identifier with a unique discriminator **)
  let gen_discrim : unit -> discrim
    = let kk = ref 0 in
      (fun () ->
        let k = !kk in
        kk := (k + 1); k)

  (** generate a unique discriminator **)
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


module Resolve = struct
  open Ast


  let typ_resolve = function
    | pos, T_Named s -> pos, T_Named s


  let rec exp_resolve ctx = function
    | pos, E_Lit l -> pos, E_Lit l
    | pos, E_Ref pth -> pos, E_Ref (path_resolve ctx pth)
    | pos, E_Copy pth -> pos, E_Copy (path_resolve ctx pth)
    | pos, E_Move pth -> pos, E_Move (path_resolve ctx pth)

    | pos, E_App (nfo, e_fn, e_args) ->
       pos, E_App (nfo,
                   exp_resolve ctx e_fn,
                   List.map (exp_resolve ctx) e_args)

    | pos, E_Do (e_1, e_2) ->
       pos, E_Do (exp_resolve ctx e_1,
                  exp_resolve ctx e_2)

    | pos, E_Let ((x_pos, x), e_rhs, e_body) ->
       let e_rhs' = exp_resolve ctx e_rhs in
       let x' = Ident.gen x in
       let e_body' = exp_resolve ((x,x')::ctx) e_body in
       pos, E_Let ((x_pos, x'), e_rhs', e_body')

    | pos, E_Lam (pos_xs, e_body) ->
       let (poss, xs) = List.split pos_xs in
       let xs' = List.map Ident.gen xs in
       let e_body' = exp_resolve
                       (List.append (List.combine xs xs') ctx)
                       e_body in
       pos, E_Lam (List.combine poss xs', e_body')


  and path_resolve ctx = function
    | pos, x ->
       (try
          pos, List.assoc x ctx
        with
        | Not_found ->
           raise (AstError (pos, Exn.Undef_var x)))


  let tl_resolve = function
    | TL_Anno (name, typ) ->
       TL_Anno (name, typ_resolve typ)

    | TL_Defn (name, exp) ->
       TL_Defn (name, exp_resolve [] exp)

    | TL_Record name ->
       TL_Record name

end
