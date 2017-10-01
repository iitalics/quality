open Batteries

let generate_name : unit -> string
  = let kk = ref 0 in
    fun () ->
    let k = !kk in
    kk := (k + 1);
    Codegen.mangle_lam_prefix ^ string_of_int k


let do_compile tls_surface out =
  let open Format in
  let tls_raw = Surface_to_ast.conv_toplevels tls_surface in
  (** discover **)
  let globs =
    Globals.discover tls_raw in

  let tls_scoped =
    (** scope resolve **)
    let open Scope.Resolve in
    let ctx = { scope = [];
                global_sigs = globs.Globals.global_sigs;
                type_reprs = globs.Globals.type_reprs; } in
    List.map (tl_resolve ctx) tls_raw
  in
  
  let globs = Globals.discover tls_scoped in

  let lifted_lams = ref [] in
  let lifted_externs = ref [] in

  let glob_defs =
    globs.Globals.global_defs
    |> Hashtbl.to_list
    |> List.map
         (fun (name,e) ->
           (** type check **)
           let tyck_ctx = Typeck.of_reprs_and_sigs
                            globs.Globals.type_reprs
                            globs.Globals.global_sigs in
           let ty = Hashtbl.find globs.Globals.global_sigs name in
           let e_tyck = Typeck.check_exp tyck_ctx ty e in
           (** lambda lift **)
           let lift_cbs = {
               Lam_lift.lift_lam = (fun clos args body ->
                 let fn = generate_name () in
                 lifted_lams := (fn, tyck_ctx, clos, args, body)::!lifted_lams;
                 fn);

               Lam_lift.lift_extern = (fun name t_ext ->
                 lifted_externs := (name, tyck_ctx, t_ext)::!lifted_externs);
             } in

           let _, e_lift = Lam_lift.lam_lift_exp lift_cbs [] e_tyck in
           name, tyck_ctx, e_lift)
  in

  set_formatter_output out;
  print_string "// hello world"; print_newline (); print_newline ();

  (* struct types *)
  Hashtbl.iter (fun name tr ->
      print_string
        (Codegen.codegen_type_repr
           ~name:name
           ~repr:tr
           ~tyck:(Typeck.of_reprs_and_sigs
                    globs.Globals.type_reprs
                    globs.Globals.global_sigs)))
    globs.Globals.type_reprs;


  (* forward declarations *)
  List.iter (fun (name, tyck_ctx, clos, args, body) ->
      print_string
        (Codegen.codegen_proc_fwd_decl
           ~name:name
           ~closure:clos
           ~args:args
           ~body:body
           ~tyck:tyck_ctx))
    !lifted_lams;

  List.iter (fun e ->
      print_string (Codegen.codegen_fwd_extern e))
    !lifted_externs;

  List.iter (fun g ->
      print_string (Codegen.codegen_fwd_global g))
    glob_defs;

  (* implementations *)
  List.iter (fun (name, tyck_ctx, clos, args, body) ->
      print_string
        (Codegen.codegen_procedure
           ~name:name
           ~closure:clos
           ~args:args
           ~body:body
           ~tyck:tyck_ctx))
    !lifted_lams;

  (* generate main *)
  print_string "int main(void) {"; print_newline();
  print_string "return ((int ( * )()) ";
  print_string Codegen.mangle_fv_prefix; print_string "main)();"; print_newline();
  print_string "}"; print_newline();
