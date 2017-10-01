open Batteries
open Scope

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
    Compile.do_compile (read_surface input)
  with Ast.AstError (pos, ex) ->
    Printf.fprintf IO.stderr "ast error: %s\n"
       (Ast.Exn.to_string ex)



let () =
  let file = File.open_in Sys.argv.(1) in
    main(file)
