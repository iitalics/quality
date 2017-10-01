open Batteries
open Ast

type info_llift = [ Typeck.info_infer
                  | `Lifted of string ]


type callbacks = {
    lift_lam : Scope.id list
               -> Scope.id list
               -> (Scope.id, info_llift) exp
               -> string
  }


let rec lam_lift_exp cbs locals = function
  | E_Lit (pos, l) ->
     Set.empty, E_Lit (pos, l)

  | E_Ref pa ->
     let c, pa' = lam_lift_path cbs locals pa in
     c, E_Ref pa'

  | E_Copy pa ->
     let c, pa' = lam_lift_path cbs locals pa in
     c, E_Copy pa'

  | E_Move pa ->
     let c, pa' = lam_lift_path cbs locals pa in
     c, E_Move pa'

  | E_Assn (pa, e) ->
     let c1, pa' = lam_lift_path cbs locals pa in
     let c2, e' = lam_lift_exp cbs locals e in
     Set.union c1 c2,
     E_Assn (pa', e')

  | E_Anno (e, t) ->
     lam_lift_exp cbs locals e

  | E_App (i, e_fun, e_args) ->
     let c, e_fun' = lam_lift_exp cbs locals e_fun in
     let cs, e_args' = List.split (List.map (lam_lift_exp cbs locals) e_args) in
     List.fold_left Set.union c cs,
     E_App ((i :> info_llift), e_fun', e_args')

  | E_Prim (o, es) ->
     let cs, es' = List.split (List.map (lam_lift_exp cbs locals) es) in
     List.fold_left Set.union Set.empty cs,
     E_Prim (o, es')

  | E_Do (e_1, e_2) ->
     let c1, e_1' = lam_lift_exp cbs locals e_1 in
     let c2, e_2' = lam_lift_exp cbs locals e_2 in
     Set.union c1 c2, E_Do (e_1', e_2')

  | E_If (pos, e_1, e_2, e_3) ->
     let c1, e_1' = lam_lift_exp cbs locals e_1 in
     let c2, e_2' = lam_lift_exp cbs locals e_2 in
     let c3, e_3' = lam_lift_exp cbs locals e_3 in
     Set.union (Set.union c1 c2) c3,
     E_If (pos, e_1', e_2', e_3')

  | E_While (pos, e_cond, e_body) ->
     let c1, e_cond' = lam_lift_exp cbs locals e_cond in
     let c2, e_body' = lam_lift_exp cbs locals e_body in
     Set.union c1 c2,
     E_While (pos, e_cond', e_body')

  | E_Let (pos, i, x, e_rhs, e_body) ->
     let c_rhs, e_rhs' = lam_lift_exp cbs locals e_rhs in
     let c_body, e_body' = lam_lift_exp cbs (x::locals) e_body in
     Set.union c_rhs c_body,
     E_Let (pos, (i :> info_llift), x, e_rhs', e_body')

  | E_Lam (pos, i, xs, e) ->
     let c, e' = lam_lift_exp cbs locals e in
     let lifted_name = cbs.lift_lam (Set.to_list c) xs e' in
     Set.diff c (Set.of_list locals),
     E_Lam (pos, `Lifted lifted_name, xs, e')

  | E_Rec (pos, i, flds) ->
     let cs, flds' = List.split
                       (List.map (fun (fld, e) ->
                            let c, e' = lam_lift_exp cbs locals e in
                            c, (fld, e'))
                          flds)
     in
     List.fold_left Set.union Set.empty cs,
     E_Rec (pos, (i :> info_llift), flds')


and lam_lift_path lift locals = function
  | Pa_Var (pos, x) ->
     if List.exists (Scope.Ident.equal x) locals then
       Set.empty, Pa_Var (pos, x)
     else
       Set.singleton x, Pa_Var (pos, x)

  | Pa_Field (i, pa, x) ->
     let c, pa' = lam_lift_path lift locals pa in
     c, Pa_Field ((i :> info_llift), pa', x)

  | Pa_Expr e ->
     let c, e' = lam_lift_exp lift locals e in
     c, Pa_Expr e'
