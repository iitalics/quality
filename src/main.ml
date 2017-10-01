open Batteries
open Scope
module OP = BatOptParse
module S = BatSys
module STR = BatString

let out_opt = OP.StdOpt.str_option ~metavar:"FILE" ()
let compile_opt = OP.StdOpt.store_true ()

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
;;

let file =
  try
    BatList.hd (OP.OptParser.parse_argv optparser)
  with
  | Failure(_) -> Printf.printf "No file to compile provided!\n"; exit 1
           
            
let compile_bool = OP.Opt.get compile_opt

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


     
let main input =
  try
    Compile.do_compile (read_surface input);
    if compile_bool then Printf.printf "Compiling...\n"
    else Printf.printf "Writing source file... \n"
  with Ast.AstError (pos, ex) ->
    Printf.fprintf IO.stderr "ast error: %s\n"
       (Ast.Exn.to_string ex)



let () =
  let file = File.open_in file in
    main(file)
