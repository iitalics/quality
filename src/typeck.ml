open Batteries
open Ast

(* this is essentially pierce/turner bidirectional typechecking *)

type info_infer = [ info_none
                  | `Struct_typename of string
                  | `Field_type of Type.t
                  | `Ret_type of Type.t
                  | `Extern_type of Type.t ]


type context = {
    gamma : Type.t Scope.id_table;
    type_reprs : (string, Type.type_repr) Hashtbl.t;
    global_sigs : (string, Type.t) Hashtbl.t;
  }

let of_reprs_and_sigs reprs sigs = {
    gamma = Scope.IdTable.create 20;
    type_reprs = reprs;
    global_sigs = sigs;
  }



(** infer the type of the expression given the context to infer in.
    returns the inferred type and the elaborated form of the expression **)
let rec infer_exp ctx = function
  | E_Lit (pos, l) ->
     infer_lit l, E_Lit (pos, l)

  | E_Extern (pos, _, _) ->
     raise_ast_error pos
       (Exn.TypeCannotInfer "extern")

  | E_Ref pa ->
     let t, pa' = infer_path ctx pa in
     Type.Ref t, E_Ref pa'

  | E_Move pa ->
     let t, pa' = infer_path ctx pa in
     t, E_Move pa'

  | E_Copy pa ->
     let t, pa' = infer_path ctx pa in
     t, E_Copy pa'

  | E_Assn (pa, e) ->
     let t_lhs, pa' = infer_path ctx pa in
     let e' = check_exp ctx t_lhs e in
     Type.builtin_unit, E_Assn (pa', e')

  | E_Anno (e, ty) ->
     let t = Type.of_ast ty in
     t, check_exp ctx t e

  | E_App (i, e_fun, e_args) ->
     let t_fun, e_fun' = infer_exp ctx e_fun in
     (match t_fun with
     | Type.Fun (t_args, t_ret) ->
        (* check arg count *)
        if List.length t_args <> List.length e_args then
          raise_ast_error (Ast.pos_of_exp e_fun)
            (Exn.ArgCount (List.length t_args));

        (* check argument types *)
        let e_args' = List.map2 (check_exp ctx) t_args e_args in

        t_ret, E_App (`Ret_type t_ret, e_fun', e_args')

     | _ ->
        raise_ast_error (Ast.pos_of_exp e_fun)
          (Exn.TypeNotFunctionApp (Type.to_string t_fun)))

  | E_Prim (o, es) as e ->
     (try
        let tn_ret::tn_args = Hashtbl.find Operators.signatures o in
        let t_args = List.map (fun n -> Type.Con n) tn_args in
        let t_ret = Type.Con tn_ret in
        let es' = List.map2 (check_exp ctx) t_args es in
        t_ret, E_Prim (o, es')
      with Not_found ->
        raise_ast_error (Ast.pos_of_exp e)
          Ast.Exn.Unimplemented)

  | E_Do (e_1, e_2) ->
     let e_1' = check_exp ctx Type.builtin_unit e_1 in
     let t, e_2' = infer_exp ctx e_2 in
     t, E_Do (e_1', e_2')

  | E_If (pos, e_1, e_2, e_3) ->
     let e_1' = check_exp ctx Type.builtin_bool e_1 in
     let t, e_2' = infer_exp ctx e_2 in
     let e_3' = check_exp ctx t e_3 in
     t, E_If (pos, e_1', e_2', e_3')

  | E_While (pos, e_cond, e_body) ->
     let e_cond' = check_exp ctx Type.builtin_bool e_cond in
     let e_body' = check_exp ctx Type.builtin_unit e_body in
     Type.builtin_unit, E_While (pos, e_cond', e_body')

  | E_Let (pos, i, x, e_rhs, e_body) ->
     let t_x, e_rhs' = infer_exp ctx e_rhs in
     Scope.IdTable.add ctx.gamma x t_x;
     let t, e_body' = infer_exp ctx e_body in
     t, E_Let (pos, (i :> info_infer), x, e_rhs', e_body')

  | E_Lam (pos, _, xs, e) ->
     raise_ast_error pos
       (Exn.TypeCannotInfer "function arguments")

  | E_Rec (pos, _, flds) ->
     raise_ast_error pos
       (Exn.TypeCannotInfer "struct")


(** check that the given expression has the given type, using the context for inference.
    returns the elaborated form of the expression. **)
and check_exp ctx t = function
  | E_Extern (pos, _, name) ->
     E_Extern (pos, `Extern_type t, name)

  | E_Lam (pos, i, xs, e) ->
     (match t with
      | Type.Fun (t_args, t_ret) ->
         (* check arg count *)
         if List.length t_args <> List.length xs then
           raise_ast_error pos
             (Exn.ArgCount (List.length t_args));
         (* insert arg types *)
         List.iter2 (Scope.IdTable.add ctx.gamma) xs t_args;
         (* check body *)
         let e' = check_exp ctx t_ret e in
         E_Lam (pos, (i :> info_infer), xs, e')

      | _ ->
         raise_ast_error pos
           (Exn.TypeNotFunctionLam (Type.to_string t)))

  | E_Do (e_1, e_2) ->
     let e_1' = check_exp ctx Type.builtin_unit e_1 in
     let e_2' = check_exp ctx t e_2 in
     E_Do (e_1', e_2')

  | E_If (pos, e_1, e_2, e_3) ->
     let e_1' = check_exp ctx Type.builtin_bool e_1 in
     let e_2' = check_exp ctx t e_2 in
     let e_3' = check_exp ctx t e_3 in
     E_If (pos, e_1', e_2', e_3')

  | E_Rec (pos, _, flds) ->
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
     E_Rec (pos, `Struct_typename rec_name, flds')


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

  | Pa_Field (_, sub_pa, x) ->
     let pos = pos_of_path sub_pa in
     let t, sub_pa' = infer_path ctx sub_pa in
     let rec_name, flds = rec_name_and_flds pos (Exn.TypeNoField x) ctx t in
     let t_fld =
       try
         List.assoc x flds
       with Not_found ->
         raise_ast_error pos (Exn.TypeNoField x)
     in
     t_fld, Pa_Field (`Field_type t_fld, sub_pa', x)


and infer_lit = function
  | L_Unit -> Type.builtin_unit
  | L_True -> Type.builtin_bool
  | L_False -> Type.builtin_bool
  | L_Int _ -> Type.builtin_int
  | L_Double _ -> Type.builtin_double
  | L_String _ -> Type.builtin_str
                    


