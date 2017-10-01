open Batteries
open Ast


type generate = {
    tyck_ctx : Typeck.context;
    mutable next_id : int;
    mutable regs : (string * Type.t) list;
    mutable out : Text.t;
    mutable vars : string Scope.id_table;
  }

let of_typeck_context tyck_ctx = {
    tyck_ctx = tyck_ctx;
    next_id = 0;
    regs = [];
    out = Text.empty;
    vars = Scope.IdTable.create 30;
  }


let mangle_lam_prefix = "_quality_lam_"
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



let rec codegen_proc_fwd_decl
  = fun ~name:name
        ~closure:clos
        ~args:args
        ~body:body
        ~tyck:tyck_ctx ->
  let gen = of_typeck_context tyck_ctx in
  (* get some type info *)
  let t_ret = simple_exp_type gen body in
  let t_args = List.map (Scope.IdTable.find gen.tyck_ctx.Typeck.gamma) args in
  let mangle_ret = mangle_type tyck_ctx t_ret in
  let mangle_args = List.map (mangle_type tyck_ctx) t_args in
  (* argument names *)
  let r_args = List.map (fun _ -> gen_name gen mangle_arg_prefix) args in
  List.iter2 (Scope.IdTable.add gen.vars) args r_args;
  (* put it all together *)
  Printf.sprintf "%s %s(%s);\n"
    mangle_ret
    name
    (String.concat ", " mangle_args)


and codegen_procedure
  = fun ~name:name
        ~closure:clos
        ~args:args
        ~body:body
        ~tyck:tyck_ctx ->
  let gen = of_typeck_context tyck_ctx in
  (* get some type info *)
  let t_ret = simple_exp_type gen body in
  let t_args = List.map (Scope.IdTable.find gen.tyck_ctx.Typeck.gamma) args in
  let mangle_ret = mangle_type tyck_ctx t_ret in
  let mangle_args = List.map (mangle_type tyck_ctx) t_args in
  (* argument names *)
  let r_args = List.map (fun _ -> gen_name gen mangle_arg_prefix) args in
  List.iter2 (Scope.IdTable.add gen.vars) args r_args;
  (* codegen body *)
  let v_ret = codegen_exp gen body in
  (* put it all together *)
  let open Printf in
  String.concat "\n"
    (["//";
      "// BEGIN: lifted lambda declaration";
      sprintf "%s %s(%s) {"
        mangle_ret
        name
        (String.concat ", " (List.map2 (sprintf "%s %s") mangle_args r_args))]

     (* declare register variables *)
     @ ("\n// local registers:"::
          List.map (fun (r, t) -> sprintf "%s %s;" (mangle_type tyck_ctx t) r)
            gen.regs)

     @ [(* body text *)
         "\n// generated contents:";
         Text.to_string gen.out;

         (* epilogue *)
         "\n// epilogue:";
         sprintf "return %s;" v_ret;
         "}\n// END: lifted lambda\n\n"])


and codegen_fwd_global
  = fun (name, tyck_ctx, e) ->
  let open Printf in
  match e with
  | E_Lam (_, `Lifted s, _, _) ->
     sprintf "#define %s%s  ((void*) (%s))\n"
       mangle_fv_prefix name s

  | E_Extern (_, _, ext_name) ->
     sprintf "#define %s%s  ((void*) (%s))\n"
       mangle_fv_prefix name ext_name

  | _ ->
     let gen = of_typeck_context tyck_ctx in
     let t = simple_exp_type gen e in
     sprintf "%s %s%s;\n"
       (mangle_type tyck_ctx t)
       mangle_fv_prefix name


and codegen_fwd_extern
  = fun (name, tyck_ctx, t) ->
  match t with
  | Type.Fun (t_args, t_ret) ->
     let mangle_args = List.map (mangle_type tyck_ctx) t_args in
     let mangle_ret = mangle_type tyck_ctx t_ret in
     Printf.sprintf "extern %s %s(%s);\n"
       mangle_ret
       name
       (String.concat ", " mangle_args)

  | _ ->
     Printf.sprintf "extern %s %s;\n"
       (mangle_type tyck_ctx t)
       name



and codegen_type_repr
  = fun ~name:name
        ~repr:tr
        ~tyck:tyck_ctx ->
  let open Printf in
  match tr with
  | Type.TR_Builtin x ->
     sprintf "// builtin type %s = %s\n" name x

  | Type.TR_Record flds ->
     String.concat "\n"
       (["//";
         "// BEGIN: struct type declaration";
         sprintf "typedef struct {"]

        @ (List.map (fun (fld, t) ->
               let mangled = mangle_type tyck_ctx t in
               sprintf "%s %s;" mangled fld)
             flds)

        @ [sprintf "} %s%s;" mangle_type_prefix name;
           "// END: struct type declaration\n\n"])



and codegen_exp : generate
                  -> (Scope.id, Lam_lift.info_llift) Ast.exp
                  -> string
  = fun gen -> function
  | E_Lit (pos, l) ->
     (match l with
      | L_False -> "0"
      | L_True  -> "1"
      | L_Unit  -> "0"
      | L_Int n -> Printf.sprintf "0x%x" n
      | L_String s ->
         String.enum s
         /@ (fun c -> Printf.sprintf "\\x%02x"
                        (Char.code c))
         |> List.of_enum
         |> String.concat ""
         |> Printf.sprintf "\"%s\"")

  | E_Extern (_, _, name) ->
     Printf.sprintf "((void* ) %s)" name

  | E_Ref pa ->
     codegen_path_ref gen pa

  | E_Move pa ->
     let r_dst = gen_reg gen (simple_path_type gen pa) in
     emit gen (Printf.sprintf "%s = *(%s);\n" r_dst (codegen_path_ref gen pa));
     r_dst

  | E_Copy pa ->
     codegen_path_direct gen pa

  | E_Assn (pa, e_rhs) ->
     let v_dst_path = codegen_path_ref gen pa in
     let v_src = codegen_exp gen e_rhs in
     emit gen (Printf.sprintf "*(%s) = %s;\n" v_dst_path v_src);
     "0"

  | E_Anno (e, t) ->
     codegen_exp gen e

  | E_App (i, e_fun, e_args) ->
     (match simple_exp_type gen e_fun with
      | Type.Fun (t_args, t_ret) ->
         let mangle_ret = mangle_type gen.tyck_ctx t_ret in
         let mangle_args = List.map (mangle_type gen.tyck_ctx) t_args in
         let r_ret = gen_reg gen t_ret in
         let v_fun = codegen_exp gen e_fun in
         let v_args = List.map (codegen_exp gen) e_args in
         emit gen (Printf.sprintf "%s = ((%s (*) (%s)) %s)(%s);\n"
                     r_ret
                     mangle_ret
                     (String.concat ", " mangle_args)
                     v_fun
                     (String.concat ", " v_args));
         r_ret

      | _ -> raise (Failure "applying non-function; this should have been caught in typechecking"))

  | E_Prim (o, es) ->
     (match Hashtbl.find Operators.c_implementations o,
           List.map (codegen_exp gen) es with
      | op,[v] ->
         Printf.sprintf "(%s(%s))" op v
      | op,[v1;v2] ->
         Printf.sprintf "((%s) %s (%s))" v1 op v2
      | _,_ -> raise (Failure "unreachable"))

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
     Scope.IdTable.add gen.vars x r_x;
     codegen_exp gen e_body

  | E_Lam (pos, i, _, _) ->
     (match i with
      | `Lifted k -> k
      | _ -> raise (Failure "invalid tag on lambda in codegen"))

  | E_Rec (pos, i, flds) as e ->
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


and mangle_type tyck_ctx = function
  | Type.Fun _ -> "void*"

  | Type.Con name ->
     (match Hashtbl.find tyck_ctx.Typeck.type_reprs name with
      | Type.TR_Builtin b -> b
      | Type.TR_Record _ -> mangle_type_prefix ^ name)

  | Type.Ref t ->
     (mangle_type tyck_ctx t) ^ "*"


and simple_exp_type gen = function
  | E_Lit (pos, l) -> Typeck.infer_lit l
  | E_Extern (_, i, _) ->
     (match i with
      | `Extern_type t -> t
      | _ -> raise (Failure "invalid tag on extern in codegen"))

  | E_Ref pa -> simple_path_type gen pa
  | E_Move pa -> simple_path_type gen pa
  | E_Copy pa -> simple_path_type gen pa
  | E_Assn (pa, e) -> Type.builtin_unit
  | E_Anno (e, t) -> Type.of_ast t
  | E_App (i, e_fun, e_args) ->
     (match i with
      | `Ret_type t -> t
      | _ -> raise (Failure "invalid tag on app in codegen"))

  | E_Prim (o, _) ->
     Type.Con (List.hd (Hashtbl.find Operators.signatures o))

  | E_Do (e_1, e_2) -> simple_exp_type gen e_2
  | E_If (pos, e_1, e_2, e_3) -> simple_exp_type gen e_2
  | E_While (pos, e_cond, e_body) -> Type.builtin_unit
  | E_Let (pos, i, x, e_rhs, e_body) -> simple_exp_type gen e_body
  | E_Lam (pos, i, xs, e) ->
     Type.Fun (List.map (Scope.IdTable.find gen.tyck_ctx.Typeck.gamma) xs,
               simple_exp_type gen e)
  | E_Rec (pos, i, flds) ->
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










