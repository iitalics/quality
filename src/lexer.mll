{
   open Batteries
   open Parser

   exception Error of int
}

let white = [' ' '\t']

(* Literals *)
let true  = "true"  | "TRUE"  | "True"
let false = "false" | "FALSE" | "False"
let unit  = "unit"  | "UNIT"  | "Unit"
let num   = ['0'-'9']+
let str   = '"' _* '"'

(* Keywords *)
let typ   = "type"  | "TYPE"  | "Type"
let andy  = "and"   | "AND"   | "And"
let where = "where" | "WHERE" | "Where"
let if    = "if"    | "IF"    | "If"
let elif  = "elif"  | "ELIF"  | "Elif"
let else  = "else"  | "ELSE"  | "Else"
let while = "while" | "WHILE" | "While"
let var   = "var"   | "VAR"   | "Var"
let fun   = "fun"   | "FUN"   | "Fun"
let ass   = "as"    | "AS"    | "As"

(* Variables *)

let id    = ['a'-'z''A'-'Z'] ['a'-'z''A'-'Z''0'-'9']*

(*************************************************************************)

rule token = parse
      [' ' '\t']         { token lexbuf }        (* skip blanks *)

    (* Literals *)
    | true               { TRUE }
    | false              { FALSE }
    (* | unit               { UNIT } *)
    | num as lxm         { INT(int_of_string lxm) }
    | str as lxm         { STR(lxm) }
    
    (* Keywords *)
    | typ                { TYPE }                (* Type declaration *)
    | andy                { AND }
    | where              { WHERE }               (* Information appendage *)
    | if                 { IF }                  (* If block *)
    | elif               { ELIF }                (* elif block *)
    | else               { ELSE }                (* else block *)
    | while              { WHILE }               (* while block *)
    | var                { VAR }
    | fun                { FUN }
    | ass                { AS }

    (* Brackets *)
    | '('                { LPAREN }              (* Function application/Creation *)
    | ')'                { RPAREN }
    | '<'                { LANGLE }              (* 'Generic' braces *)
    | '>'                { RANGLE }
    | '['                { LSQUARE }              (* Arracy braces *)
    | ']'                { RSQUARE }
    | '|'                { VERTB }               (* Quality indicator *)
    | '{'                { LCURL }
    | '}'                { RCURL }

    (* Punctuation *)
    | ','                { COMMA }
    | ':'                { COLON }
    | "->"               { ARROW }
    | '.'                { DOT }
    | '&'                { REF }
    | ';'                { EOL }
    | ['\n']             { EOL }                 (* new lines *)
    | ";;"               { DSEMI }               (* end codeblock *)
    | '!'                { NOT }

    (* Assignment *)
    | '='                { EQUAL }

    (* Arithmetic *)
    | '+'                { PLUS }
    | '-'                { MINUS }
    | '/'                { DIV }
    | '*'                { MULTI }

    (* Comparison *)
    | "=="               { EQL }
    | ">="               { GTE }
    | "<="               { LTE }
    | "!="               { NEQ }
    | "&&"               { LAND }
    | "||"               { LOR }

    (* Variables *)
    | id as lxm          { ID(lxm) }

    (* File marker *)
    | eof                { EOF }

    | _                  { raise (Error (Lexing.lexeme_start lexbuf)) }

