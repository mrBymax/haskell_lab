/*
    Grammar:
        P ::= eps | aPa | aPb
        S ::= aS | P
*/

%{
#include "es1.tab.h"
%}

%option noyywrap

%%
[ \t\n] ; /* Ignore whitespace, tab and newline */
a   { return 'a'; }
b   { return 'b'; }
.   {
        fprintf(stderr, "Invalid char: %s\n", yytext);
    }
%%
