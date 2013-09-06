%{
#include "arith.h"
%}

%option noinput noyywrap bison-bridge bison-locations yylineno
%option never-interactive

%%
[0-9]+			{ *yylval = atoi (yytext); return TOK_NUMBER; }

 /* operators, punctuators */
"+"			{ return TOK_PLUS; }
"-"			{ return TOK_MINUS; }
"*"			{ return TOK_TIMES; }
"/"			{ return TOK_DIVIDE; }
"("			{ return TOK_LPAREN; }
")"			{ return TOK_RPAREN; }
"||"			{ return TOK_OROR; }
"&&"			{ return TOK_ANDAND; }
"="			{ return TOK_EQUALS; }
"if"			{ return TOK_IF; }
"then"			{ return TOK_THEN; }
"else"			{ return TOK_ELSE; }
"end"			{ return TOK_END; }

[ \t\n]			{ }

"#".*"\n"		{ }

.			{ yyerror (yylloc, 0, "illegal character"); }
