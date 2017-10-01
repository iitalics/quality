open Batteries

module A = Ast
module S = Ast_surface


let rec conv_toplevels tls = List.map conv_toplevel tls

and conv_toplevel = function
  | S.TL_Defn ((pos,name), e) ->
     A.TL_Def (pos, name, conv_exp e)

  | S.TL_Sig ((pos,name), t) ->
     A.TL_Sig (pos, name, conv_typ t)

  | S.TL_Type ((pos,name), flds) ->
     A.TL_Record (pos, name,
                  List.map (fun ((pos,fld), t) ->
                      (pos, fld, conv_typ t))
                    flds)


and conv_exp = function
  | S.E_Lit (pos, l) ->
     A.E_Lit (pos, conv_lit l)

  | S.E_Ann (e, t) ->
     A.E_Anno (conv_exp e, conv_typ t)

  | S.E_Rec (pos, flds) ->
     A.E_Rec (pos, `No_info,
              List.map (fun ((_,x),e) ->
                  (x, conv_exp e))
                flds)

  | S.E_App (ef, es) ->
     A.E_App (`No_info,
              conv_exp ef,
              List.map conv_exp es)

  | S.E_Lam (pos, xs, body) ->
     let body' = conv_stmts pos body in
     A.E_Lam (A.pos_of_exp body',
              `No_info,
              List.map snd xs,
              body')

  | S.E_BinOp _ -> raise (Failure "bin ops not done")
  | S.E_UniOp _ -> raise (Failure "uni ops not done")

  | S.E_Ref e ->
     A.E_Ref (conv_path e)

  | S.E_Move e ->
     A.E_Move (conv_path e)

  | e ->
     A.E_Copy (conv_path e)


and conv_path = function
  | S.E_Var (pos, x) ->
     A.Pa_Var (pos, x)

  | S.E_Fieldof (e, (pos, fld)) ->
     A.Pa_Field (`No_info,
                 conv_path e,
                 fld)

  | e ->
     A.Pa_Expr (conv_exp e)


and conv_stmts pos = function
  | [] -> A.E_Lit (pos, A.L_Unit)
  | [s] -> conv_stmt s
  | s::ss ->
     match s with
     | S.S_Let ((pos,x),e) ->
        A.E_Let (pos, `No_info, x,
                 conv_exp e,
                 conv_stmts pos ss)

     | S.S_Nop _ ->
        conv_stmts pos ss

     | _ ->
        A.E_Do (conv_stmt s,
                conv_stmts pos ss)


and conv_stmt = function
  | S.S_Let ((pos,x),e) ->
     A.E_Let (pos, `No_info, x,
              conv_exp e,
              A.E_Lit (pos, A.L_Unit))

  | S.S_Reass (lhs, rhs) ->
     A.E_Assn (conv_path lhs,
               conv_exp rhs)

  | S.S_Do e -> conv_exp e

  | S.S_While (e, body) ->
     let e' = conv_exp e in
     let pos = A.pos_of_exp e' in
     A.E_While (pos, e', conv_stmts pos body)

  | S.S_If (outer_pos, conds, final) ->
     List.fold_right
       (fun (pos, cond, body) rest ->
         A.E_If (pos,
                 conv_exp cond,
                 conv_stmts pos body,
                 rest))
       conds
       (conv_stmts outer_pos final)

  | S.S_Nop pos ->
     A.E_Lit (pos, A.L_Unit)


and conv_typ = function
  | S.T_Var (pos, x) ->
     A.T_Named (pos, x)

  | S.T_Sig (args, ret) ->
     A.T_Fun (List.map conv_typ args, conv_typ ret)


and conv_lit = function
  | S.L_True -> A.L_True
  | S.L_False -> A.L_False
  | S.L_Unit -> A.L_Unit
  | S.L_Int n -> A.L_Int n
  | _ ->
     raise (Failure "unimplemented")
