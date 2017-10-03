open Batteries
open Scope
module OP = BatOptParse
module S = BatSys
module STR = BatString


(******************* OPTS PARSING / CONFIG *******************)
           
let out_opt = OP.StdOpt.str_option ~metavar:"FILE" ()
let compile_opt = OP.StdOpt.store_true ()
let macro_opt = OP.StdOpt.store_false ()

let optparser : OP.OptParser.t =
  OP.OptParser.make
    ~prog:"qc"
    ~usage:"%prog - quality's compiler. Output to C or executable"
    ~version:"0.01"
    ()

let () =
  OP.OptParser.add optparser
    ~help:"output a C file with the given name"
    ~short_name:'o'
    ~long_name:"output"
    out_opt;
  OP.OptParser.add optparser
    ~help:"compile the program with cc"
    ~short_name:'c'
    ~long_name:"compile"
    compile_opt;
  OP.OptParser.add optparser
    ~help:"disable m4 macro expansion"
    ~short_name:'m'
    ~long_name:"no-macros"
    macro_opt;
;;

let to_compile =
  try
    BatList.hd (OP.OptParser.parse_argv optparser)
  with
  | Failure(_) -> Printf.printf "No file to compile provided!\n"; exit 1
           
            
let compile_bool = OP.Opt.get compile_opt
let macro_bool = OP.Opt.get macro_opt

exception IncorrectFileName

let generate_name =
  try
    let file_name = STR.rchop ~n:3 to_compile in
    let file_ext = STR.lchop ~n:((STR.length to_compile) - 3) to_compile in
    if STR.equal file_ext ".ql" then STR.concat "." [file_name; "c"]
    else raise IncorrectFileName
  with IncorrectFileName ->
    Printf.printf "Please give a valid source '.ql' file\n"; exit 1
;;

let out_name =
  let s = OP.Opt.opt out_opt in
  match s with
  | None -> generate_name
  | Some s -> s
;;

(******************* OPTS / FILE HANDLING *******************)

let macro_expand =
  let open File in
  let output = open_temporary_out ~prefix:"macro" ~suffix:"tmp.ql" () in
  let path =
    match (STR.rindex_opt to_compile '/') with
    | None -> "./"
    | Some(i) -> STR.left to_compile i in  
  let input = open_in (snd output) in
  
  let result = Sys.command (Printf.sprintf "m4 -I %s %s > %s" path to_compile (snd output)) in

  if result <> 0 then
    (Printf.printf "Error expanding macros! Reported %i\n" result; exit 1;)
  else IO.close_out (fst output);

  input
;;

let get_source_files =
  let open File in
  let out_perm = perm [user_read;user_write;group_read;other_read] in
  let input  = (if macro_bool then macro_expand else (open_in to_compile)) in
  let output = (if compile_bool then
                  open_temporary_out ~prefix:"ir" ~suffix:"tmp.c" ()
                else (open_out ~perm:out_perm out_name),out_name) in
  (input, output)
;;

let compile_cc output =
  let open Printf in
  if compile_bool then begin
      printf "Compiling...\n";
      let result = Sys.command (sprintf "cc %s" output) in
      printf "Completed with code %i\n" result
    end
  else begin
      printf "Writing %s... \n" output;
      printf "Completed!\n"
    end
;;

(******************* MAIN / COMPILING *******************)

let read_surface input =
  let lexbuf = Lexing.from_input input in
  try
    Parser.prog Lexer.token lexbuf
  with
  | Lexer.Error msg ->
     Printf.printf "INVALID TOKEN! @%d\n" msg; exit 1
  | Parser.Error ->
     Printf.printf "ERROR! %s @ %d\n"
       (Lexing.lexeme lexbuf)
       (Lexing.lexeme_start lexbuf); exit 1

     
let main input output_pair =
  try
    Compile.do_compile (read_surface input) (fst output_pair);
    IO.close_in input;
    IO.close_out (fst output_pair);
    compile_cc (snd output_pair);
  with Ast.AstError (pos, ex) ->
    Printf.fprintf IO.stderr "ast error: %s\n" (Ast.Exn.to_string ex)


let () =
  let source = get_source_files in
  main (fst source) (snd source);
;;
  
    
