%{
#include <stdio.h>
%}

%x COMMENT
%x STRING

%%
<INITIAL>"//".* { } /* Consume comments */

<INITIAL>"/*"  { BEGIN(COMMENT); /* Transition to multi-line comment state */ }
<COMMENT>"*/"  { BEGIN(INITIAL); /* Exit condition */ }
<COMMENT>.     { /* Ignore command chars */ }
<COMMENT>\n    { /* Ignore newlines */ }

<INITIAL>"\""  { ECHO; BEGIN(STRING); /* Catch the beginning of a string */ }
<STRING>"\\\"" { ECHO; /* Escapes with rule */ }
<STRING>"\""   { ECHO; BEGIN(INITIAL); /* Input string ends */ }
<STRING>.      { ECHO; /* Prints string content */ }
<STRING>\n     { ECHO; }

<INITIAL>.     { ECHO; /* Prints every other char from source */ }
<INITIAL>\n    { ECHO; /* Prints the newlines from source */ }

%%
int main (int argc, char **argv) {
    yylex();
    return 0;
}

int yywrap() {
    return 1;
}
