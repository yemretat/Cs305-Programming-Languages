%{
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    extern int yylineno;
    void yyerror (const char *s){
        s = "ERROR";
        printf ("%s\n", s);
        
    }
    struct Node{
        char * data;
        struct Node *next;
    };
    struct Node *head=NULL;
    struct Node *tail=NULL;
    struct Node *headfunc=NULL;
    struct Node *tailfunc=NULL;
    void printer()
    {
        struct Node*ptr=head;
        while(ptr != NULL)
        {
            printf(ptr->data);
            ptr=ptr->next;
        }
        printf("\n");
    }
    void printerfunc()
    {
        struct Node*ptr=headfunc;
        while(ptr != NULL)
        {
            printf(ptr->data);
            ptr=ptr->next;
        }
        printf("\n");
    }
    void insertfunc(char *text)
    {
        if(headfunc==NULL)
        {
            headfunc = (struct Node*)malloc(sizeof(struct Node));
            headfunc->data=text;
            tailfunc=headfunc;
        }
        else
        {
            struct Node* temp=(struct Node*)malloc(sizeof(struct Node));
            temp->data=text;
            tailfunc->next=temp;
            tailfunc=tailfunc->next;
        }
    }
    int controllerfunc(char *text)
    {
        struct Node*ptr=headfunc;
        while(ptr != NULL)
        {
            if(strcmp(ptr->data,text)==0)
            {
                return 1;
            }
            else
            {
                ptr=ptr->next;
            }
        }
        return 0;
    }
    void insert(char *text)
    {
        if(head==NULL)
        {
            head = (struct Node*)malloc(sizeof(struct Node));
            head->data=text;
            tail=head;
        }
        else
        {
            struct Node* temp=(struct Node*)malloc(sizeof(struct Node));
            temp->data=text;
            tail->next=temp;
            tail=tail->next;
        }
    }
    int controller(char *text)
    {
        int check=0;
        struct Node*ptr=head;
        while(ptr != NULL)
        {
            if(strcmp(ptr->data,text)==0)
            {
                check=1;
                ptr=ptr->next;
            }
            else
            {
                ptr=ptr->next;
            }
        }
        
        return check;
    }
    void deleteList()
    {
        struct Node* current = headfunc;
        struct Node* next2;
        
        while (current != NULL)
        {
            next2 = current->next;
            free(current);
            current = next2;
        }
        headfunc = NULL;
        tailfunc=NULL;
    }
    %}
%locations
%union{
    char * name;
}


/* Biso declaration starting */
%start prog
/* token part is used to declare tokens used in the grammar.*/
%token  tINT tSTRING tRETURN tPRINT tLPAR tCOMMA tASSIGNM tPLUS tSTAR tLBRAC tRPAR tMOD tMINUS tDIV tSEMI tRBRAC  tINTVAL tSTRINGVAL

%token<name>tIDENT

/* left part is used to give a left associative operators */

%left tPLUS tMINUS tINTVAL tSTRINGVAL
%left tDIV tSTAR tINT tSTRING tRETURN tPRINT tLPAR tCOMMA tASSIGNM tLBRAC tRPAR tMOD tSEMI tRBRAC tIDENT

%%/*Grammer Rules starting */

prog: itemlst /* start symbol of the program */
;
itemlst: item /* list of items*/
| itemlst item
;
item: asgn    /* each item can be assignment,declaration or print item */
|vardecl
|printstam
|funcdef
|functioncall

;
vardecl: type tIDENT tASSIGNM expr tSEMI{if(controller($2)==1){printf("%d Redefinition of variable \n",yylineno);}else{insert($2);}}
;
vardeclfunc: type tIDENT tASSIGNM expr2 tSEMI{if(controllerfunc($2)==1){printf("%d Redefinition of variable \n",yylineno);}else{insertfunc($2);}}

;
type:tINT
| tSTRING
;
expr: value     /* denotes an expression  biz ilk default tanımladığımız şey ney çünkü olup olmadığına bakıcaz*/
| expr tPLUS expr
| expr tMINUS expr
| expr tDIV expr
| expr tSTAR expr
| expr tMOD expr
|functioncall

;
expr2: value2     /* denotes an expression  biz ilk default tanımladığımız şey ney çünkü olup olmadığına bakıcaz*/
| expr2 tPLUS expr2
| expr2 tMINUS expr2
| expr2 tDIV expr2
| expr2 tSTAR expr2
| expr2 tMOD expr2


;
value2: tINTVAL /*Denotes an int ,realnum or a variable reference */
| tSTRINGVAL
| tIDENT {if(controllerfunc($1)==1){;}else {printf("%d Undefined variable \n",yylineno);}}
;


exprList: expr
| exprList tCOMMA expr
|
;
functioncall: tIDENT tLPAR exprList tRPAR tSEMI {if(controller($1)==1){;}else{printf("%d Undefined variable");};}/* Denotes a funccal */
;



value: tINTVAL /*Denotes an int ,realnum or a variable reference */
| tSTRINGVAL
| tIDENT {if(controller($1)==1){;}else {printf("%d Undefined variable \n",yylineno);}}
;



funcdef: type tIDENT tLPAR params tRPAR tLBRAC bodyfunc tRBRAC {if(controller($2)==1){printf("%d Redefinition of variable \n",yylineno);}else{insert($2);}}
;



params: type tIDENT{insertfunc($2);}
|type tIDENT tCOMMA type tIDENT {insertfunc($2);insertfunc($5);}
|type tIDENT tCOMMA type tIDENT tCOMMA type tIDENT {insertfunc($2);insertfunc($5);insertfunc($8);}
|type tIDENT tCOMMA type tIDENT tCOMMA type tIDENT tCOMMA type tIDENT {insertfunc($2);insertfunc($5);insertfunc($8);insertfunc($11);}
|
;
bodyfunc: stltmlist return
|stltmlist
;
stltmlist: stmt stltmlist
|stmt
|
;
stmt: vardeclfunc
| asgn
|printstam
;
asgn: tIDENT tASSIGNM expr2 tSEMI/* asignmnet statemnt */
;

return : tRETURN expr2 tSEMI{deleteList();}
;
printstam:tPRINT tLPAR expr tRPAR tSEMI
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