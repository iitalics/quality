open Batteries

let main file =
  let open Ast in
  try
    let lexbuf = Lexing.from_input file in
    while true do
      let result = Lexer.token lexbuf in
      print_string "test"; print_newline(); flush stdout
    done
  with Lexer.Eof ->
    exit 0

let () =
  for i = 1 to Array.length Sys.argv - 1 do
    main(File.open_in Sys.argv.(i))
  done;;
    
