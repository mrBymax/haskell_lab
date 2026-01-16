%{
#include <stdio.h>
#include <stdlib.h>

int yylex();
void yyerror(const char* s);
%}

%%

start : S { printf("Valid string\n"); } ;

S : 'a' S
  | P
  ;

P : /* eps is just empty */
  | 'a' P 'a'
  | 'a' P 'b'
  ;
%%

int main () {
    printf("Insert string: ");
    if (yyparse() == 0){
        printf("Analysis completed");
    }
    return 0;
}

void yyerror(const char* s) {
    fprintf(stderr, "Error: %s\n", s);
}
