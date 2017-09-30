open Batteries

let generate_name : unit -> string
  = let kk = ref 0 in
    fun () ->
    let k = !kk in
    kk := (k + 1);
    Printf.sprintf "function_%d" k


let do_compile tls_raw =
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
    |> Hashtbl.map
         (fun name e ->
           (** type check **)
           let tyck_ctx =
             let open Typeck in
             { gamma = Scope.IdTable.create 40;
               type_reprs = globs.Globals.type_reprs;
               global_sigs = globs.Globals.global_sigs; } in
           let ty = Hashtbl.find globs.Globals.global_sigs name in
           let e_tyck = Typeck.check_exp tyck_ctx ty e in
           let gamma = tyck_ctx.Typeck.gamma in
           (** lambda lift **)
           let _, e_lift = Lam_lift.lam_lift_exp
                             (fun clos args body ->
                               let fn = generate_name () in
                               lifted := (fn, gamma, clos, args, body)::!lifted;
                               fn)
                             []
                             e_tyck in
           e_lift)
  in

  List.iter (fun (fn, _, _, _, _) ->
      Printf.printf "/* generate procedure: %s */\n" fn)
    !lifted;

  Hashtbl.iter
