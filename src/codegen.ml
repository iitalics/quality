open Batteries
open Ast


type generate = {
    tyck_ctx : Typeck.context;
    mutable next_id : int;
    mutable regs : (string * Type.t) list;
    mutable out : Text.t;
    mutable vars : string Scope.id_table;
  }


let mangle_fv_prefix = "_quality_g_"
let mangle_type_prefix = "_quality_t_"
let mangle_tmp_prefix = "_q_tmp_"
let mangle_arg_prefix = "_q_arg_"


let emit gen what =
  gen.out <- Text.append gen.out (Text.of_string what)

let gen_name gen prefix =
 let k = gen.next_id in
  let r = Printf.sprintf "%s%d" prefix k in
  gen.next_id <- k + 1;
  r

let gen_reg gen t =
  let r = gen_name gen mangle_tmp_prefix in
  gen.regs <- (r,t)::gen.regs;
  r


let of_typeck_context tyck_ctx = {
    tyck_ctx = tyck_ctx;
    next_id = 0;
    regs = [];
    out = Text.empty;
    vars = Scope.IdTable.create 30;
  }





let rec codegen_procedure
  = fun ~name:name
        ~closure:clos
        ~args:args
        ~body:body
        ~tyck:tyck_ctx
        gen ->
  let gen = of_typeck_context tyck_ctx in
  (* get some type info *)
  let t_ret = simple_exp_type gen body in
  let t_args = List.map (Scope.IdTable.find gen.tyck_ctx.Typeck.gamma) args in
  let mangle_ret = codegen_mangle_type gen t_ret in
  let mangle_args = List.map (codegen_mangle_type gen) t_args in
  (* argument names *)
  let r_args = List.map (fun _ -> gen_name gen mangle_arg_prefix) args in
  List.iter2 (Scope.IdTable.add gen.vars) args r_args;
  (* codegen body *)
  let v_ret = codegen_exp gen body in
  (* put it all together *)
  let open Printf in
  String.concat "\n"
    (["// begin lifted lambda declaration\n";
      sprintf "%s %s(%s) {"
        mangle_ret
        (String.concat ", " mangle_args)
        (List.map2 (fun r t -> sprintf "%s %s" (codegen_mangle_type gen t) r)
           r_args
           t_args
         |> String.concat ", ")]

     (* declare register variables *)
     @ ["// local registers:\n"]
     @ (List.map (fun (r, t) -> sprintf "%s %s;" (codegen_mangle_type gen t) r)
          gen.regs)

     (* body text *)
     @ ["\n// generated contents:\n"]
     @ [Text.to_string gen.out]

     (* epilogue *)
     @ ["\n// epilogue:\n";
        sprintf "return %s;\n}\n\n" v_ret])


and codegen_exp gen = function
  | E_Lit (pos, l) ->
     (match l with
      | L_False -> "0"
      | L_True  -> "1"
      | L_Unit  -> "0"
      | L_Int n -> string_of_int n)

  | E_Ref (pos, pa) ->
     codegen_path_ref gen pa

  | E_Move (pos, pa) ->
     let r_dst = gen_reg gen (simple_path_type gen pa) in
     emit gen (Printf.sprintf "%s = *(%s);\n" r_dst (codegen_path_ref gen pa));
     r_dst

  | E_Copy (pos, pa) ->
     codegen_path_direct gen pa

  | E_Assn (pa, e_rhs) ->
     let r_dst = gen_reg gen (simple_path_type gen pa) in
     let v_src = codegen_exp gen e_rhs in
     emit gen (Printf.sprintf "*(%s) = %s;\n" r_dst v_src);
     "0"

  | E_Anno (e, t) ->
     codegen_exp gen e

  | E_App (i, e_fun, e_args) ->
     (match simple_exp_type gen e_fun with
      | Type.Fun (t_args, t_ret) ->
         let mangle_ret = codegen_mangle_type gen t_ret in
         let mangle_args = List.map (codegen_mangle_type gen) t_args in
         let r_ret = gen_reg gen t_ret in
         let v_fun = codegen_exp gen e_fun in
         let v_args = List.map (codegen_exp gen) e_args in
         emit gen (Printf.sprintf "%s = ((%s (*) (%s)) %s)(%s);\n"
                     mangle_ret
                     (String.concat ", " mangle_args)
                     r_ret
                     v_fun
                     (String.concat ", " v_args));
         r_ret

      | _ -> raise_ast_error (pos_of_exp e_fun)
               Exn.TypeNotFunctionApp)

  | E_Do (e_1, e_2) ->
     let _ = codegen_exp gen e_1 in
     codegen_exp gen e_2

  | E_If (pos, e_1, e_2, e_3) ->
     let r_phi = gen_reg gen (simple_exp_type gen e_2) in
     let v_cond = codegen_exp gen e_1 in
     emit gen (Printf.sprintf "if (%s) {\n" v_cond);
     let v_then = codegen_exp gen e_2 in
     emit gen (Printf.sprintf "%s = %s;\n}\nelse {\n" r_phi v_then);
     let v_else = codegen_exp gen e_3 in
     emit gen (Printf.sprintf "%s = %s;\n}\n" r_phi v_else);
     r_phi

  | E_While (pos, e_cond, e_body) ->
     emit gen "for (;;) {\n";
     let v_cond = codegen_exp gen e_cond in
     emit gen (Printf.sprintf "if ((%s) == 0) { break; }\n" v_cond);
     let _ = codegen_exp gen e_body in
     emit gen "}\n";
     "0"

  | E_Let (pos, i, x, e_rhs, e_body) ->
     let r_x = gen_reg gen (Scope.IdTable.find gen.tyck_ctx.Typeck.gamma x) in
     let v_rhs = codegen_exp gen e_rhs in
     emit gen (Printf.sprintf "%s = %s;\n" r_x v_rhs);
     r_x

  | E_Lam (pos, i, _, _) ->
     (match i with
      | `Lifted k -> k
      | _ -> raise (Failure "invalid tag on lambda in codegen"))

  | E_MakeStruct (pos, i, flds) as e ->
     let r_struct = gen_reg gen (simple_exp_type gen e) in
     flds |> List.iter (fun (fld, e_fld) ->
                 let v_fld = codegen_exp gen e_fld in
                 emit gen (Printf.sprintf "%s.%s = %s;\n" r_struct fld v_fld));
     r_struct


and codegen_path_ref gen pa =
  Printf.sprintf "&(%s)" (codegen_path_direct gen pa)

and codegen_path_direct gen = function
  | Pa_Var (pos, id) ->
     (match id with
      | Scope.Ident.BV _ -> Scope.IdTable.find gen.vars id
      | Scope.Ident.FV x  -> mangle_fv_prefix ^ x)

  | Pa_Field (_, pa, x) ->
     Printf.sprintf "%s.%s" (codegen_path_direct gen pa) x

  | Pa_Expr e ->
     let r = gen_reg gen (simple_exp_type gen e) in
     let v = codegen_exp gen e in
     emit gen (Printf.sprintf "%s = %s;\n" r v);
     r


and codegen_mangle_type gen = function
  | Type.Fun _ -> "void*"

  | Type.Con name ->
     (match Hashtbl.find gen.tyck_ctx.Typeck.type_reprs name with
      | Type.TR_Builtin b -> b
      | Type.TR_Record _ -> mangle_type_prefix ^ name)

  | Type.Ref t ->
     (codegen_mangle_type gen t) ^ "*"


and simple_exp_type gen = function
  | E_Lit (pos, l) -> Typeck.infer_lit l
  | E_Ref (pos, pa) -> simple_path_type gen pa
  | E_Move (pos, pa) -> simple_path_type gen pa
  | E_Copy (pos, pa) -> simple_path_type gen pa
  | E_Assn (pa, e) -> Type.builtin_unit
  | E_Anno (e, t) -> Type.of_ast t
  | E_App (i, e_fun, e_args) ->
     (match i with
      | `Ret_type t -> t
      | _ -> raise (Failure "invalid tag on app in codegen"))

  | E_Do (e_1, e_2) -> simple_exp_type gen e_2
  | E_If (pos, e_1, e_2, e_3) -> simple_exp_type gen e_2
  | E_While (pos, e_cond, e_body) -> Type.builtin_unit
  | E_Let (pos, i, x, e_rhs, e_body) -> simple_exp_type gen e_body
  | E_Lam (pos, i, xs, e) ->
     Type.Fun (List.map (Scope.IdTable.find gen.tyck_ctx.Typeck.gamma) xs,
               simple_exp_type gen e)
  | E_MakeStruct (pos, i, flds) ->
     (match i with
      | `Struct_typename s -> Type.Con s
      | _ -> raise (Failure "invalid tag on makestruct in codegen"))

and simple_path_type gen = function
  | Pa_Var (pos, id) ->
     (match id with
      | Scope.Ident.BV _ -> Scope.IdTable.find (gen.tyck_ctx.Typeck.gamma) id
      | Scope.Ident.FV x -> Hashtbl.find (gen.tyck_ctx.Typeck.global_sigs) x)

  | Pa_Field (i, _, _) ->
     (match i with
      | `Field_type t -> t
      | _ -> raise (Failure "invalid tag on fieldof in codegen"))

  | Pa_Expr e -> simple_exp_type gen e
