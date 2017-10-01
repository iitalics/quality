open Batteries

let generate_name : unit -> string
  = let kk = ref 0 in
    fun () ->
    let k = !kk in
    kk := (k + 1);
    Codegen.mangle_lam_prefix ^ string_of_int k


let do_compile tls_surface =
  let open Printf in
  let tls_raw = Surface_to_ast.conv_toplevels tls_surface in
  let globs =
    (** discover **)
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

  let lifted = ref [] in

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
           let _, e_lift = Lam_lift.lam_lift_exp
                             (fun clos args body ->
                               let fn = generate_name () in
                               lifted := (fn, tyck_ctx, clos, args, body)::!lifted;
                               fn)
                             []
                             e_tyck in
           name, tyck_ctx, e_lift)
  in

  printf "// hello world\n\n";

  (* forward declarations *)
  List.iter (fun (name, tyck_ctx, clos, args, body) ->
      print_string
        (Codegen.codegen_proc_fwd_decl
           ~name:name
           ~closure:clos
           ~args:args
           ~body:body
           ~tyck:tyck_ctx))
    !lifted;

  print_string (Codegen.codegen_fwd_globals glob_defs);

  (* implementations *)
  List.iter (fun (name, tyck_ctx, clos, args, body) ->
      print_string
        (Codegen.codegen_procedure
           ~name:name
           ~closure:clos
           ~args:args
           ~body:body
           ~tyck:tyck_ctx))
    !lifted;

  (* generate main *)
  printf "int main(void) {\nreturn ((int ( * )()) %smain)();}\n"
    Codegen.mangle_fv_prefix
