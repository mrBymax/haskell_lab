%{
#include <stdlib.h>
#include <stdio.h>
#include "y.tab.h"

extern int yylval;
%}

%%
[0-9]+              { yylval = atoi(yytext); return NUM; }
[-+*/()\[\]{}\n]    { return yytext[0]; }
[ \t]+              { /* whitespace */ }
.                   { fprintf(stderr, "Errore lessicale: carattere non riconosciuto '%s'\n", yytext); }

%%

int yywrap(void) {
    return 1;
}
