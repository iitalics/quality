open Batteries

let main file =
    let lexbuf = Lexing.from_input file in
    let result = Parser.prog Lexer.token lexbuf in
    Ast_surface.get_string result; print_newline(); flush stdout
;;

let () =
  main(File.open_in Sys.argv.(1))
    
