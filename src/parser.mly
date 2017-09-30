%{
open Ast_surface
%}


/* Literals */
%token TRUE FALSE
%token <int> INT
%token <string> STR

/* Keywords */
%token TYPE AND WHERE IF ELIF ELSE WHILE VAR

/* Brackets */
%token LPAREN RPAREN LANGLE RANGLE LSQUARE RSQUARE VERTB LCURL RCURL

/* Punction */
%token COMMA COLON ARROW MOVE EOL DSEMI

/* Assignment */
%token EQUAL

/* Arithmetic */
%token PLUS MINUS DIV MULTI

/* Variables */
name:
        s=STR                         { $startpos,s }
;
%token <string> ID

/* File markers */
%token EOF

/* PRECEDENCE */
%left PLUS MINUS
%left DIV MULTI

%start main
%type <entire_program> main
%%

main:
        main toplevel                  { $2::$1 }
        toplevel EOF                   { [$1] }
;

toplevel:
        decl n=name t=typ EOL          { $startpos,TL_Decl(n,t) }
        defn n=name e=exp EOL          { $startpos,TL_Defn(n,e) }
        type n=name t=fields EOL        { $startpos,TL_Type(n,($startpos,t)) } 
;
/* Declerations */
typ:
        n=name LANGLE t=typcon RANGLE     { T_Con(n,t) }
        n=name                            { T_Var(n) }
;
typcon:
        t=typ                             { [($startpos,t)] }
        t=typ COMMA r=typcon              { [($startpos,t)::r] }
;

/* Definition */

exp:
        l=lit                             { E_Lit(l) }
        v=STRING                          { E_Var(v) }
        r=exp                             { E_Ref($startpos,r) }
        m=exp                             { E_Move($startpos,m) }
        f=exp n=name                      { E_Fieldof(($startpos,f),n) }
        a=exp args=exps                   { E_App(($startpos,a),args) }
        l=args b=stmts                    { E_Lam(l,b) }
;

stmt:
        i=ID EQUAL e=exp                    { S_Let(false,i,($startpos,e)) }
        e=exp                               { S_Do($startpos,e) }
        IF e1=exp COLON e2=exps ELSE e3=exps {S_If(($startpos,e1),e2,e3) }
        WHILE e1=exp COLON e2=exps          { S_While(($startpos,e1),e2) }
;

lit:
        i=INT                               { L_Int(i) }
        b=TRUE                              { L_True }
        b=FALSE                             { L_False }
;

/* Type */

fields:
        LSQUARE RSQUARE                   { [] }
        LSQUARE f=field fs=fields RSQUARE { f::fs }
        f=field fs=fields                 { f::fs }
;

field:
        n=name COLON t=typ                { name,($startpos,typ) }
;

name:
        s=STR                         { $startpos,s }
;


