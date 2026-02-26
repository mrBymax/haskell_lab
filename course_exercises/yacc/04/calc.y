%{
#include <stdio.h>
#include <stdlib.h>
int yylex(void);
void yyerror(char* s);
%}

%token NUM
%left '+' '-'
%left '*' '/'

%%

calc: expr_C '\n'   { printf("Res: %d\n", $1); exit(0); }
    ;

/* { expr } */
expr_C: expr_C '+' expr_C { $$ = $1 + $3; }
      | expr_C '-' expr_C { $$ = $1 - $3; }
      | expr_C '*' expr_C { $$ = $1 * $3; }
      | expr_C '/' expr_C { $$ = $1 / $3; }
      | term_C            { $$ = $1; }
      ;

/* [ expr ] */
expr_S: expr_S '+' expr_S { $$ = $1 + $3; }
      | expr_S '-' expr_S { $$ = $1 - $3; }
      | expr_S '*' expr_S { $$ = $1 * $3; }
      | expr_S '/' expr_S { $$ = $1 / $3; }
      | term_S            { $$ = $1; }
      ;

/* ( expr ) */
expr_P: expr_P '+' expr_P { $$ = $1 + $3; }
      | expr_P '-' expr_P { $$ = $1 - $3; }
      | expr_P '*' expr_P { $$ = $1 * $3; }
      | expr_P '/' expr_P { $$ = $1 / $3; }
      | term_P            { $$ = $1; }
      ;

term_P: NUM               { $$ = $1; }
      | '(' expr_P ')'    { $$ = $2; }
      ;

term_S: NUM               { $$ = $1; }
      | '(' expr_P ')'    { $$ = $2; }
      | '[' expr_S ']'    { $$ = $2; }
      ;

term_C: NUM               { $$ = $1; }
      | '(' expr_P ')'    { $$ = $2; }
      | '[' expr_S ']'    { $$ = $2; }
      | '{' expr_C '}'    { $$ = $2; }
      ;

%%

void yyerror(char* s){
    fprintf(stderr, "Syntax error!");
}

int main(void){
    yyparse();
    return 0;
}
