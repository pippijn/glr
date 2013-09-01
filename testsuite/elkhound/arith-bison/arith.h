#define YYDEBUG 1

#include "arithParser.h"

void yyerror (YYLTYPE *loc, int *result, char const *msg);
int yylex (YYSTYPE *yylval, YYLTYPE *yylloc);
