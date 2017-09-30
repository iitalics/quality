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
|        list(EOL) EOF                           { [] }
|        m=toplevel list(EOL) p=prog             { m::p }
;
toplevel:
|       s=sigs                          { s }
|       d=defn                         { d }
|       t=typd                         { t }
;
sigs:
|       n=name COLON COLON s=sign       { $startpos,TL_Sig(n,s) }
;
defn:
|       n=name EQUAL e=exp              { $startpos,TL_Defn(n,e) }
|       n=name LPAREN a=separated_list(COMMA,name) RPAREN EQUAL;
        list(EOL) LCURL b=stmtblock;
        RCURL                           { $startpos,TL_Defn(n,E_Lam(a,b)) }
;
typd:
|       TYPE n=name EQUAL f=fields      { $startpos,TL_Type(n,f) }
;

/*******************/
sign:
|        tl=list(typ) ARROW t=typ        { T_Sig(tl,t) }
;
/*******************/
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
         LPAREN ars=separated_list(COMMA,exp) RPAREN { E_App(a,ars) }
|        FUN LPAREN l=separated_list(COMMA,name) RPAREN;
             LCURL b=stmtblock RCURL       { E_Lam(l,b) }
|        LCURL l=separated_list(COMMA,name); EOL
         b=stmtblock RCURL                 { E_Lam(l,b) }
;
stmt:
|        n=exp EQUAL e=exp                   { S_Reass(n,e) }
|        VAR n=name EQUAL e=exp              { S_Let(n,e) }
|        e=exp                               { S_Do(e) }
|        IF e1=exp COLON list(EOL);
         e2=stmtblock;
         ELSE e3=stmtblock;
         DSEMI                               { S_If(e1,e2,e3) }
|        WHILE e1=exp COLON list(EOL);
         e2=stmtblock;
         DSEMI                               { S_While(e1,e2) }
|        EOL                                 { S_Nop }
;
stmtblock:
|         s=stmt; list(EOL);                 { [s] }
|         ss=stmt; list(EOL);
                   sb=stmtblock              { ss::sb }
;

lit:
|        i=INT                               { L_Int(i) }
|        b=TRUE                              { L_True }
|        b=FALSE                             { L_False }
|        LPAREN RPAREN                       { L_Unit }
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

