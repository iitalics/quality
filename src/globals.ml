open Batteries
open Ast


type 'def globals = {
    type_reprs : (string, Type.type_repr) Hashtbl.t;
    global_sigs : (string, Type.t) Hashtbl.t;
    global_defs : (string, 'def) Hashtbl.t;
  }

let rec discover toplvls =
  let ty_reps = Hashtbl.copy Type.builtin_reprs in
  let sigs = Hashtbl.create 50 in
  let defs = Hashtbl.create 50 in
  List.iter (function
         | TL_Record (pos, name, fields) ->
            if Hashtbl.mem ty_reps name then
              raise_ast_error pos (Exn.Redef name)
            else
              Hashtbl.add ty_reps name
                (Type.TR_Record
                   (List.map (fun (pos,f,ast) ->
                        (f, Type.of_ast ast))
                      fields))

         | TL_Sig (pos, name, typ_ast) ->
            if Hashtbl.mem sigs name then
              raise_ast_error pos (Exn.Redef name)
            else
              Hashtbl.add sigs name (Type.of_ast typ_ast)

         | TL_Def (pos, name, exp) ->
            if not (Hashtbl.mem sigs name) then
              raise_ast_error pos (Exn.SigMissing name)
            else if Hashtbl.mem defs name then
              raise_ast_error pos (Exn.Redef name)
            else
              Hashtbl.add defs name exp)
    toplvls;

  { type_reprs = ty_reps;
    global_sigs = sigs;
    global_defs = defs; }


let map_defs f globs =
  { type_reprs = globs.type_reprs;
    global_sigs = globs.global_sigs;
    global_defs = Hashtbl.map f globs.global_defs; }
