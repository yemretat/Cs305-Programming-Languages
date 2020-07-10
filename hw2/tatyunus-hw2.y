%{/* C declaration */
#include <stdio.h>
void yyerror (const char *s){
    s = "ERROR";
    printf ("%s\n", s);
    
}
%}
/* Bison declaration starting */
%start prog
/* token part is used to declare tokens used in the grammar.*/
%token   tIF tENDIF tEQ tLT tGT tNE tLTE tGTE tIDENT tINTNUM tREALNUM tDOTPROD tTRANSPOSE tINTTYPE tINTVECTORTYPE tINTMATRIXTYPE tREALTYPE tREALVECTORTYPE tREALMATRIXTYPE
/* left part is used to give a left associative operators */
%left '+' '-'
%left '/' '*'
%left tOR
%left tAND
%left tDOTPROD
%left tTRANSPOSE

%% /* Grammar rules starting  */
prog:   stmtlst /*start symbol of the grammar */
;
stmtlst:   stmt  /*list of statements */
        | stmtlst stmt
;
stmt: asgn    /* each stmt can be assignment,declaration or if stmt */
    | decl    
    |if
;
decl: type vars '=' expr ';' /* a decl followed by a comma variab. equality sign and expression */
;
type: tREALMATRIXTYPE /*4. condition in hw description */
    | tINTVECTORTYPE 
    | tREALVECTORTYPE 
    |  tINTMATRIXTYPE 
    | tREALTYPE 
    | tINTTYPE
;
vars:   tIDENT /*non empty variable list*/
    | vars ',' tIDENT
;
asgn: tIDENT '=' expr ';'/* asignmnet statemnt */
;
if: tIF '(' bool ')' stmtlst tENDIF /* denotes if statement */
;
expr: tINTNUM /* denotes an expression */
    | expr '+' expr
    | expr '-' expr
    | expr '/' expr
    | expr '*' expr
    | tREALNUM
    | tIDENT
    | vectorLit
    | matrixLit
    | expr tDOTPROD expr
    | transpose
    | value
;
transpose:  tTRANSPOSE '(' expr ')' /* denotes a transpose expression */
;
vectorLit:  '[' row ']' /* Denotes a vector literal */
;

matrixLit: '[' row ';' rows ']' /*Denotes a matrix literal*/
;
row: value 
    | value ',' row/* Denotes a non-empty list of comma seperated values */
;
rows: row 
    | rows ';' row/* Denotes a non-empty list of semicolon seperated rows */
;
value: tINTNUM /*Denotes an int ,realnum or a variable reference */
    | tREALNUM
    | tIDENT
;
bool:   comp 
    | bool tOR bool 
    | bool tAND bool 
    | comp tOR bool 
    | comp tAND bool
;/* denotes a boolean expre */

comp: tIDENT relation tIDENT /*simple relation */
;
relation: tNE /* binary relational operators  */
        | tLTE 
        | tGTE 
        | tEQ 
        | tLT 
        | tGT
;
%%

int main (){
    if (yyparse()) {
        return 1;
}
    else {
        printf("OK\n");
        return 0;
    }
}
