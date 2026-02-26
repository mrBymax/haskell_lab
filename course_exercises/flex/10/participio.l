%{
#include <stdio.h>
#include <string.h>
%}

%%

"fare"      { printf("facente"); }
"fatto"     { printf("fare"); }

"dire"      { printf("dicente"); }
"detto"     { printf("dire"); }

"leggere"   { printf("leggente"); }
"letto"     { printf("leggere"); }

"vedere"    { printf("vedente"); }
"visto"     { printf("vedere"); }

"correre"   { printf("corrente"); }
"corso"     { printf("correre"); }

[a-zA-Z]+are { yytext[yyleng-3] = '\0'; printf("%sante", yytext);}
[a-zA-Z]+ere { yytext[yyleng-3] = '\0'; printf("%sente", yytext);}
[a-zA-Z]+ire { yytext[yyleng-3] = '\0'; printf("%sente", yytext);}

[a-zA-Z]+ato { yytext[yyleng-3] = '\0'; printf("%sare", yytext);}
[a-zA-Z]+ito { yytext[yyleng-3] = '\0'; printf("%sere", yytext);}
[a-zA-Z]+uto { yytext[yyleng-3] = '\0'; printf("%sire", yytext);}

.  { ECHO; /* spaces, punctuation and other words */ }
\n { ECHO; /* keep formatting */ }

%%

int main(int argc, char** argv){
    yylex();
    return 0;
}

int yywrap(){
    return 1;
}
