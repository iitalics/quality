{
   open Batteries
   open Parser

   exception Error of int
   exception StringLitError of int
}

let white = [' ' '\t']

(* Literals *)
let true  = "true"  | "TRUE"  | "True"
let false = "false" | "FALSE" | "False"
let unit  = "unit"  | "UNIT"  | "Unit"
let num   = ['0'-'9']+

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
let ext   = "external" | "EXTERNAL" | "External"

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
    | '"'                { read_string (Buffer.create 17) lexbuf }

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
    | ext                { EXT }

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

and read_string buf = parse
    | '"'       { STR (Buffer.contents buf) }
    | '\\' '/'  { Buffer.add_char buf '/'; read_string buf lexbuf }
    | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
    | '\\' 'b'  { Buffer.add_char buf '\b'; read_string buf lexbuf }
    | '\\' 'f'  { Buffer.add_char buf '\012'; read_string buf lexbuf }
    | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
    | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
    | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
    | [^ '"' '\\']+
      { Buffer.add_string buf (Lexing.lexeme lexbuf);
        read_string buf lexbuf
      }
    | _ { raise (StringLitError (Lexing.lexeme_start lexbuf)) }
    | eof { raise (StringLitError (Lexing.lexeme_start lexbuf)) }
