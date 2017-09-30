%{
open Ast_surface
%}


/* Literals */
%token TRUE FALSE UNIT
%token <int> INT
%token <string> STR

/* Keywords */
%token TYPE AND WHERE IF ELIF ELSE WHILE VAR FUN

/* Brackets */
%token LPAREN RPAREN LANGLE RANGLE LSQUARE RSQUARE VERTB LCURL RCURL

/* Punction */
%token COMMA COLON ARROW DOT MOVE EOL DSEMI

/* Assignment */
%token EQUAL

/* Arithmetic */
%token PLUS MINUS DIV MULTI

/* Variables */
%token <string> ID

/* File markers */
%token EOF

/* PRECEDENCE */
/*%left PLUS MINUS*/
/*%left DIV MULTI MOVE*/

%start <Ast_surface.entire_program> prog
%%

prog:
|        EOF                               { [] }
/*|      m=toplevel option(EOL) EOF        { [m] }*/
|        m=toplevel option(EOL) p=prog EOF { m::p }
|        m=toplevel p=prog EOF             { m::p }
;
toplevel:
|        n=name COLON COLON s=sign      { $startpos,TL_Sig(n,s) }
|        n=name EQUAL e=exp             { $startpos,TL_Defn(n,e) }
|        n=name a=delimited(LPAREN,separated_list(COMMA,name),RPAREN);
         EQUAL option(EOL) LCURL b=separated_list(EOL,stmt) RCURL;
                                        { $startpos,TL_Defn(n,E_Lam(a,b)) }
|        TYPE n=name EQUAL f=fields     { $startpos,TL_Type(n,f) }
;

/* Declerations WORKS */
sign:
|        tl=list(typ) ARROW t=typ        { T_Sig(tl,t) }
;
typ:
|        n=name                          { T_Var(n) }
|        LPAREN s=sign RPAREN            { s }
;

/* Definition */
exp:
|        LPAREN e=exp RPAREN               { e }
|        l=lit                             { E_Lit(l) }
|        v=name                            { E_Var(v) }
|        MOVE r=exp                        { E_Ref(r) }
|        MULTI m=exp                       { E_Move(m) }
|        e=exp DOT n=name                  { E_Fieldof(e,n) }
|        a=exp;
         ars=delimited(LPAREN,separated_list(COMMA,exp),RPAREN) { E_App(a,ars) }
|        FUN l=delimited(LPAREN,separated_list(COMMA,name),RPAREN); option(EOL)
             LCURL b=separated_list(EOL,stmt) RCURL      { E_Lam(l,b) }
|        LCURL l=separated_list(COMMA,name);
             EOL b=separated_list(EOL,stmt) RCURL { E_Lam(l,b) }
;

lit:
|        i=INT                               { L_Int(i) }
|        b=TRUE                              { L_True }
|        b=FALSE                             { L_False }
|        u=UNIT                              { L_Unit }
;

stmt:
|        n=exp EQUAL e=exp                   { S_Reass(n,e) }
|        VAR n=name EQUAL e=exp              { S_Let(n,e) }
|        e=exp                               { S_Do(e) }
|        IF e1=exp COLON option(EOL);
         e2=separated_list(EOL,stmt);
         ELSE e3=separated_list(EOL,stmt);
         DSEMI                               { S_If(e1,e2,e3) }
|        WHILE e1=exp COLON option(EOL);
         e2=separated_list(EOL,stmt);
         DSEMI                               { S_While(e1,e2) }
|        option(EOL)                         { S_Nop }
;

/* Type */

fields:
|        f=delimited(LSQUARE,separated_list(COMMA,field),RSQUARE) { f }
;

field:
|        n=name COLON t=typ                { n,t }
;

name:
|        s=ID                         { ($startpos,s) }
;
