open Batteries
open Scope
open Typeck
open Codegen

let main input =

  let lexbuf = Lexing.from_input input in
  try
    let result = Parser.prog Lexer.token lexbuf in
    Ast_surface.get_string result; print_newline(); flush stdout;
  with
  | Lexer.Error msg ->
     print_string "INVALID TOKEN! @"; print_int msg; print_newline();
  | Parser.Error ->
     print_string "ERROR! "; print_string (Lexing.lexeme lexbuf); print_string " @ ";
     print_int (Lexing.lexeme_start lexbuf); print_newline();

let () =
  let file = File.open_in Sys.argv.(1) in
    main(file);
