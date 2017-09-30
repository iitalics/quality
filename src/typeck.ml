open Batteries
open Ast

(* this is essentially pierce/turner bidirectional typechecking *)

type context = {
    gamma : Type.t Scope.id_table;
    type_reprs : (string, Type.type_repr) Hashtbl.t;
    global_sigs : (string, Type.t) Hashtbl.t;
  }


(** infer the type of the expression given the context to infer in.
    returns the inferred type and the elaborated form of the expression **)
let rec infer_exp ctx = function
  | E_Lit (pos, l) ->
     infer_lit l, E_Lit (pos, l)

  | E_Ref (pos, pa) ->
     let t, pa' = infer_path ctx pa in
     Type.Ref t, E_Ref (pos, pa')

  | E_Move (pos, pa) ->
     let t, pa' = infer_path ctx pa in
     Type.Ref t, E_Ref (pos, pa')

  | E_Copy (pos, pa) ->
     let t, pa' = infer_path ctx pa in
     Type.Ref t, E_Ref (pos, pa')

  | E_Anno (e, ty) ->
     let t = Type.of_ast ty in
     t, check_exp ctx t e

  | E_App (_, e_fun, e_args) ->
     let t_fun, e_fun' = infer_exp ctx e_fun in
     (match t_fun with
     | Type.Fun (t_args, t_ret) ->
        (* check arg count *)
        if List.length t_args <> List.length e_args then
          raise_ast_error (Ast.pos_of_exp e_fun)
            (Exn.ArgCount (List.length t_args));

        (* check argument types *)
        let e_args' = List.map2 (check_exp ctx) t_args e_args in
        t_ret, E_App (`No_info, e_fun', e_args')

     | _ ->
        raise_ast_error (Ast.pos_of_exp e_fun)
          Exn.TypeNotFunctionApp)

  | E_Do (e_1, e_2) ->
     let e_1' = check_exp ctx Type.builtin_unit e_1 in
     let t, e_2' = infer_exp ctx e_2 in
     t, E_Do (e_1', e_2')

  | E_Let (pos, _, x, e_rhs, e_body) ->
     let t_x, e_rhs' = infer_exp ctx e_rhs in
     Scope.IdTable.add ctx.gamma x t_x;
     let t, e_body' = infer_exp ctx e_body in
     t, E_Let (pos, `No_info, x, e_rhs', e_body')

  | E_Lam (pos, _, xs, e) ->
     raise_ast_error pos
       (Exn.TypeCannotInfer "function arguments")

  | E_MakeStruct (pos, _, flds) ->
     raise_ast_error pos
       (Exn.TypeCannotInfer "struct")


(** check that the given expression has the given type, using the context for inference.
    returns the elaborated form of the expression. **)
and check_exp ctx t = function
  | E_Lam (pos, _, xs, e) ->
     (match t with
      | Type.Fun (t_args, t_ret) ->
         (* check arg count *)
         if List.length t_args <> List.length xs then
           raise_ast_error pos
             (Exn.ArgCount (List.length t_args));

         (* insert arg types *)
         List.iter2 (Scope.IdTable.add ctx.gamma) xs t_args;
         (* check body *)
         check_exp ctx t_ret e

      | _ ->
         raise_ast_error pos
           (Exn.TypeNotFunctionLam (Type.to_string t)))

  | E_MakeStruct (pos, _, flds) ->
     let rec_name, rec_flds = rec_name_and_flds pos Exn.TypeUnexpectRecord ctx t in
     let flds' = List.map
                   (fun (fld, e) ->
                     let t =
                       try
                         List.assoc fld rec_flds
                       with Not_found ->
                         raise_ast_error (pos_of_exp e) (Exn.TypeNoField fld)
                     in
                     fld, check_exp ctx t e)
                   flds
     in
     E_MakeStruct (pos, `Struct_typename rec_name, flds')


  | e ->
     let u, e' = infer_exp ctx e in
     (* infer and then check type == expected *)
     if not (Type.equal t u) then
       raise_ast_error (pos_of_exp e)
         (Exn.TypeMismatch (Type.to_string t,
                            Type.to_string u))
     else
       e'


and rec_name_and_flds pos err ctx = function
  | Type.Con name ->
     (match Hashtbl.find ctx.type_reprs name with
      | Type.TR_Record fs -> name, fs
      | _ ->
         raise_ast_error pos err)
  | _ ->
     raise_ast_error pos err


and infer_path ctx = function
  | Pa_Expr e ->
     let t, e' = infer_exp ctx e in
     t, Pa_Expr e'

  | Pa_Var (pos, id) ->
     let open Scope.Ident in
     let t = match id with
       | FV x -> Hashtbl.find ctx.global_sigs x
       | BV _ -> Scope.IdTable.find ctx.gamma id
     in
     t, Pa_Var (pos, id)

  | Pa_Field (sub_pa, x) ->
     let pos = pos_of_path sub_pa in
     let t, sub_pa' = infer_path ctx sub_pa in
     let _, flds = rec_name_and_flds pos (Exn.TypeNoField x) ctx t in
     let t_fld =
       try
         List.assoc x flds
       with Not_found ->
         raise_ast_error pos (Exn.TypeNoField x)
     in
     t_fld, Pa_Field (sub_pa', x)


and infer_lit = function
  | L_Unit -> Type.builtin_unit
  | L_True -> Type.builtin_bool
  | L_False -> Type.builtin_bool
  | L_Int _ -> Type.builtin_int
