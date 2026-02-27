%{
#include <stdlib.h>
#include <stdio.h>
#include "y.tab.h"

extern YYSTYPE yylval;
%}

%%

[0-9]+      { yylval.val = atoi(yytext); return NUM; }
"if"        { return IF; }
"case"      { return CASE; }
[-+*=()\n]  { return yytext[0]; }
[ \t]+      { /* whitespace */ }
.           { fprintf(stderr, "Error: character unrecognised '%s'\n", yytext); }

%%

int yywrap(void) {
    return 1;
}
