open Batteries
open Scope
module OP = BatOptParse
module S = BatSys
module STR = BatString

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
    ~help:"expand m4 macros"
    ~short_name:'m'
    ~long_name:"macros"
    macro_opt;
;;

let file =
  try
    BatList.hd (OP.OptParser.parse_argv optparser)
  with
  | Failure(_) -> Printf.printf "No file to compile provided!\n"; exit 1
           
            
let compile_bool = OP.Opt.get compile_opt
let macro_bool = OP.Opt.get macro_opt

exception IncorrectFileName
let out_name =
  let s = OP.Opt.opt out_opt in
  try
    match s with
    | None -> (let file_name = STR.rchop ~n:3 file in
               let file_ext = STR.lchop ~n:((STR.length file) - 3) file in
               if STR.equal file_ext ".ql" then STR.concat "." [file_name; "c"]
               else raise IncorrectFileName)
    | Some s -> s
  with
  | IncorrectFileName -> Printf.printf "Please give a valid source '.ql' file\n"; exit 1
;;


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

let macro_expand file_name =
  let open File in
  let macro_out = open_temporary_out ~prefix:"macro" ~suffix:"tmp.ql" () in
  let file_inp = open_in (snd macro_out) in
  let result = Sys.command (Printf.sprintf "m4 %s > %s" file_name (snd macro_out)) in
  if result <> 0
  then (Printf.printf "Error expanding macros! Reported %i" result; exit 1;)
  else (IO.close_out (fst macro_out); file_inp)
;;

     
let main input output_pair =
  let open Printf in
  try
    Compile.do_compile (read_surface input) (fst output_pair);
    if compile_bool
    then (printf "Compiling...\n";
          printf "Completed with code %i!\n" (Sys.command (sprintf "cc %s" (snd output_pair))))
    else (printf "Writing %s... \n" (snd output_pair);
          printf "Completed!\n")
  with Ast.AstError (pos, ex) ->
    fprintf IO.stderr "ast error: %s\n"
       (Ast.Exn.to_string ex)


let () =
  let open File in
  let out_perm = perm [user_read;user_write;group_read;other_read] in
  let inp = (if macro_bool
             then macro_expand file
             else open_in file) in
  let out = (if compile_bool
             then File.open_temporary_out ~prefix:"ir" ~suffix:"tmp.c" ~mode:[`create;`trunc;`delete_on_exit] ()
             else (open_out ~perm:out_perm out_name),out_name) in
  main inp out;
  IO.close_in inp; IO.close_out (fst out)
;;
  
    
