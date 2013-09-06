%{
#include "eea.h"
%}

%option noinput noyywrap bison-bridge bison-locations yylineno
%option never-interactive

%%
[ \t\n]			{ }
.			{ return *yytext; }
