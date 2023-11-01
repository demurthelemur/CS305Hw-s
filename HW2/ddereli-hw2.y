%{
#include <stdio.h>
void yyerror (const char *s) {return;} /* Called by yyparse on error */
%}

%token tMAIL 
%token tENDMAIL 
%token tSCHEDULE 
%token tENDSCHEDULE 
%token tSEND 
%token tSET 
%token tTO 
%token tFROM 
%token tAT 
%token tCOMMA 
%token tCOLON 
%token tLPR 
%token tRPR 
%token tLBR 
%token tRBR 
%token tIDENT 
%token tSTRING 
%token tADDRESS 
%token tDATE
%token tTIME

%start prog 

%%

prog:  | progComp prog
;

progComp: setStatement | mailBlock ;

mailBlock: tMAIL tFROM tADDRESS tCOLON stmntlist tENDMAIL | tMAIL tFROM tADDRESS tCOLON tENDMAIL;

stmntlist: stmnt | stmnt stmntlist ;

stmnt: setStatement | sendStatement | scheduleStatement ;

setStatement: tSET tIDENT tLPR tSTRING tRPR;

sendStatement: tSEND tLBR tSTRING tRBR tTO recipientList  
      | tSEND tLBR tIDENT tRBR tTO recipientList  ;

recipientList: tLBR multipleRecipient tRBR ;

multipleRecipient: recipient | recipient tCOMMA multipleRecipient ;

recipient: tLPR tADDRESS tRPR
      | tLPR tIDENT tCOMMA tADDRESS tRPR
      | tLPR tSTRING tCOMMA tADDRESS tRPR ;

scheduleStatement: tSCHEDULE tAT tLBR tDATE tCOMMA tTIME tRBR tCOLON sendList tENDSCHEDULE ;

sendList: sendStatement | sendStatement sendList ;

%%

int main() {
    if (yyparse()) {
        printf("ERROR\n");
    }
    else {
        printf("OK\n");
    }
    return 0;
}