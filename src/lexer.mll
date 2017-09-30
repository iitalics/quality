{
   open Batteries
   exception Eof
}

let white = [' ' '\t']

(* Literals *)
let true  = "true"  | "TRUE"  | "True"
let false = "false" | "FALSE" | "False"
let num   = ['0'-'9']+
let str   = '"' _* '"'

(* Keywords *)
let typ   = "type"  | "TYPE"  | "Type"
let and   = "and"   | "AND"   | "And"
let where = "where" | "WHERE" | "Where"
let if    = "if"    | "IF"    | "If"
let elif  = "elif"  | "ELIF"  | "Elif"
let else  = "else"  | "ELSE"  | "Else"
let while = "while" | "WHILE" | "While"

(* Variables *)

let id    = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']*

(*************************************************************************)

rule token = parse
      [' ' '\t']         { token lexbuf }        (* skip blanks *)

    (* Literals *)
    | true               { TRUE }
    | false              { FALSE }
    | num as lxm         { INT(string_of_int lxm) }
    | str as lxm         { STR(str) }
    
    (* Keywords *)
    | typ                { TYPE }                (* Type declaration *)
    | and                { AND }
    | where              { WHERE }               (* Information appendage *)
    | if                 { IF }                  (* If block *)
    | elif               { ELIF }                (* elif block *)
    | else               { ELSE }                (* else block *)
    | while              { WHILE }               (* while block *)

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
    | id as lxm          { ID(lxm) }

    (* File marker *)
    | eof                { raise Eof }

