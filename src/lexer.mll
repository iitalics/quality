{
   exception Eof

   type token =
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
                         { TRUE }
    | "false" | "FALSE" | "False"
                         { FALSE }               (* Boolean literals *)
    | ['0'-'9']+ as lxm  { INT(int_of_string lxm) }
    | '"' _* '"' as lxm  { STR(lxm) }            (* String literals between any "" *)

    (* Keywords *)
    | "type" | "TYPE" | "Type"
                         { TYPE }                (* Type declaration *)
    | "and" | "AND" | "And"
                         { AND }
    | "where" | "WHERE" | "Where"
                         { WHERE }               (* Information appendage *)
    | "if" | "IF" | "If"
                         { IF }                  (* If block *)
    | "elif" | "ELIF" | "Elif"
                         { ELIF }                (* elif block *)
    | "else" | "ELSE" | "Else"
                         { ELSE }                (* else block *)
    | "while" | "WHILE" | "While"
                         { WHILE }               (* while block *)

    (* Brackets *)
    | '('                { LPAREN }              (* Function application/Creation *)
    | ')'                { RPAREN }
    | '<'                { LANGLE }              (* 'Generic' braces *)
    | '>'                { RANGLE }
    | '['                { LANGLE }              (* Arracy braces *)
    | ']'                { RANGLE }
    | '|'                { VERTB }               (* Quality indicator *)

    (* Punctuation *)
    | ','                { COMMA }
    | ':'                { COLON }
    | "->"               { ARROW }
    | ';'                { SEMI }
    | ['\n']             { EOL }                 (* new lines *)
    | ";;"               { DSEMI }               (* end codeblock *)

    (* Assignment *)
    | '='                { EQUAL }

    (* Arithmetic *)
    | '+'                { PLUS }
    | '-'                { MINUS }
    | '/'                { DIV }
    | '*'                { MULTI }

    (* Variables *)
    | ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']* as lxm
                         { ID(lxm) }

    (* File marker *)
    | eof                { raise Eof }

