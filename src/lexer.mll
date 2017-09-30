{
   open Batteries
   exception Eof
   type token = item * Lexing.position
   and item =
        (* Built-in literals *)
        | TRUE
        | FALSE
        | INT of int
        | STR of string
        (* Keywords *)
        | TYPE
        | AND
        | WHERE
        | IF
        | ELIF
        | ELSE
        | WHILE
        (* BRACKETS *)
        | LPAREN
        | RPAREN
        | LANGLE
        | RANGLE
        | LSQUARE
        | RSQUARE
        | VERTB
        (* PUNCTUATION *)
        | COMMA
        | COLON
        | ARROW
        | SEMI
        | EOL
        | DSEMI
        (* ASSIGNMENT *)
        | EQUAL
        (* ARITHMETIC *)
        | PLUS
        | MINUS
        | DIV
        | MULTI
        (* VARIABLES *)
        | ID of string
        (* FILE MARKERS *)
        | EOF
   ;;
}

rule token = parse
      [' ' '\t']         { token lexbuf }        (* skip blanks *)

    (* Literals *)
    | "true" | "TRUE" | "True"
                         { TRUE, Lexing.lexeme_start_p lexbuf }
    | "false" | "FALSE" | "False"
                         { FALSE, Lexing.lexeme_start_p lexbuf }               (* Boolean literals *)
    | ['0'-'9']+ as lxm  { INT(int_of_string lxm), Lexing.lexeme_start_p lexbuf }
    | '"' _* '"' as lxm  { STR(lxm), Lexing.lexeme_start_p lexbuf }            (* String literals between any "" *)

    (* Keywords *)
    | "type" | "TYPE" | "Type"
                         { TYPE, Lexing.lexeme_start_p lexbuf }                (* Type declaration *)
    | "and" | "AND" | "And"
                         { AND, Lexing.lexeme_start_p lexbuf }
    | "where" | "WHERE" | "Where"
                         { WHERE, Lexing.lexeme_start_p lexbuf }               (* Information appendage *)
    | "if" | "IF" | "If"
                         { IF, Lexing.lexeme_start_p lexbuf }                  (* If block *)
    | "elif" | "ELIF" | "Elif"
                         { ELIF, Lexing.lexeme_start_p lexbuf }                (* elif block *)
    | "else" | "ELSE" | "Else"
                         { ELSE, Lexing.lexeme_start_p lexbuf }                (* else block *)
    | "while" | "WHILE" | "While"
                         { WHILE, Lexing.lexeme_start_p lexbuf }               (* while block *)

    (* Brackets *)
    | '('                { LPAREN, Lexing.lexeme_start_p lexbuf }              (* Function application/Creation *)
    | ')'                { RPAREN, Lexing.lexeme_start_p lexbuf }
    | '<'                { LANGLE, Lexing.lexeme_start_p lexbuf }              (* 'Generic' braces *)
    | '>'                { RANGLE, Lexing.lexeme_start_p lexbuf }
    | '['                { LANGLE, Lexing.lexeme_start_p lexbuf }              (* Arracy braces *)
    | ']'                { RANGLE, Lexing.lexeme_start_p lexbuf }
    | '|'                { VERTB, Lexing.lexeme_start_p lexbuf }               (* Quality indicator *)

    (* Punctuation *)
    | ','                { COMMA, Lexing.lexeme_start_p lexbuf }
    | ':'                { COLON, Lexing.lexeme_start_p lexbuf }
    | "->"               { ARROW, Lexing.lexeme_start_p lexbuf }
    | ';'                { SEMI, Lexing.lexeme_start_p lexbuf }
    | ['\n']             { EOL, Lexing.lexeme_start_p lexbuf }                 (* new lines *)
    | ";;"               { DSEMI, Lexing.lexeme_start_p lexbuf }               (* end codeblock *)

    (* Assignment *)
    | '='                { EQUAL, Lexing.lexeme_start_p lexbuf }

    (* Arithmetic *)
    | '+'                { PLUS, Lexing.lexeme_start_p lexbuf }
    | '-'                { MINUS, Lexing.lexeme_start_p lexbuf }
    | '/'                { DIV, Lexing.lexeme_start_p lexbuf }
    | '*'                { MULTI, Lexing.lexeme_start_p lexbuf }

    (* Variables *)
    | ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']* as lxm
                         { ID(lxm), Lexing.lexeme_start_p lexbuf }

    (* File marker *)
    | eof                { raise Eof }

