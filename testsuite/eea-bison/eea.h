#define YYDEBUG 1

#include "eeaParser.h"

void yyerror (YYLTYPE *loc, int *result, char const *msg);
int yylex (YYSTYPE *yylval, YYLTYPE *yylloc);

extern int yydebug;
