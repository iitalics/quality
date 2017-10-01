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
%token COMMA COLON ARROW DOT EOL DSEMI

/* Assignment */
%token EQUAL

/* Arithmetic */
%token PLUS MINUS DIV MULTI
%token UMINUS

/* Comparison */
%token GRT LST GTE LTE EQL NEQ LAND LOR

/* Unary ops */
%token REF MOVE NOT

/* Variables */
%token <string> ID

/* File markers */
%token EOF

/* PRECEDENCE */
%left COMMA
%left LOR
%left LAND
%left NEQ EQL
%left GRT LST GTE LTE
%left PLUS MINUS
%left DIV MULTI
%right REF MOVE UMINUS NOT
%left DOT FUNC


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
|        l=lit                             { E_Lit($startpos,l) }
|        v=name                            { E_Var(v) }
|        r=construct                       { E_Rec($startpos,r) }
|        e=exp DOT n=name                  { E_Fieldof(e,n) }
|        a=exp;
         LPAREN
         ars=separated_list(COMMA,exp)
         RPAREN                %prec FUNC  { E_App(a,ars) }
|        FUN;
         LPAREN;
         l=separated_list(COMMA,name);
         RPAREN;
         LCURL b=stmtblock RCURL           { E_Lam(l,b) }
|        LCURL;
         l=separated_list(COMMA,name);
         EOL; b=stmtblock; RCURL           { E_Lam(l,b) }
|        MINUS e = exp       %prec UMINUS  { E_UniOp(UO_Neg,e) }
|        REF   e = exp                     { E_UniOp(UO_Ref,e) }
|        MULTI e = exp         %prec MOVE  { E_UniOp(UO_Mov,e) }
|        NOT   e = exp                     { E_UniOp(UO_Not,e) }
|        e1=exp PLUS e2=exp                { E_BinOp(BO_Add,e1,e2) }
|        e1=exp MINUS e2=exp               { E_BinOp(BO_Sub,e1,e2) }
|        e1=exp MULTI e2=exp               { E_BinOp(BO_Mul,e1,e2) }
|        e1=exp DIV e2=exp                 { E_BinOp(BO_Div,e1,e2) }
|        e1=exp EQL e2=exp                 { E_BinOp(BO_Eql,e1,e2) }
|        e1=exp NEQ e2=exp                 { E_BinOp(BO_Neq,e1,e2) }
|        e1=exp GRT e2=exp                 { E_BinOp(BO_Grt,e1,e2) }
|        e1=exp LST e2=exp                 { E_BinOp(BO_Lst,e1,e2) }
|        e1=exp GTE e2=exp                 { E_BinOp(BO_Gte,e1,e2) }
|        e1=exp LTE e2=exp                 { E_BinOp(BO_Lte,e1,e2) }
|        e1=exp LAND e2=exp                { E_BinOp(BO_And,e1,e2) }
|        e1=exp LOR e2=exp                 { E_BinOp(BO_Or ,e1,e2) }
;
stmt:
|        n=exp EQUAL e=exp                   { S_Reass(n,e) }
|        VAR n=name EQUAL e=exp              { S_Let(n,e) }
|        e=exp                               { S_Do(e) }

|        IF c=exp COLON list(EOL);
         b=stmtblock;
         el=elif;
         e=els; DSEMI                       { S_If(((c,b)::el),e) }

|        WHILE e1=exp COLON list(EOL);
         e2=stmtblock;
         DSEMI                               { S_While(e1,e2) }
|        EOL                                 { S_Nop }
;

construct:
|       LSQUARE l=separated_list(COMMA,ass) RSQUARE  { l }
;
ass:
|       n=name EQUAL e=exp                   { (n,e) }
;

elif:
|                                            { [] }
|        ELIF cond=exp COLON list(EOL);
         b=stmtblock e=elif                  { (cond,b)::e }
;
els:
|                                            { [] }
|        ELSE COLON b=stmtblock                    { b }
stmtblock:
|         s=stmt; list(EOL);                 { [s] }
|         ss=stmt; list(EOL);
                   sb=stmtblock              { ss::sb }
;

lit:
|        i=INT                               { L_Int(i) }
|        s=STR                               { L_Str(s) }
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

