#include <stdio.h>
#include <stdlib.h>

#include "arith.h"

void
yyerror (YYLTYPE *loc, int *result, char const *msg)
{
  printf ("<stdin>:[%d:%d]-[%d:%d]: %s\n",
          loc->first_line,
          loc->first_column,
          loc->last_line,
          loc->last_column,
          msg);
  exit (1);
}

int
main ()
{
  int result;
  yydebug = 0;
  yyparse (&result);
  printf ("%d\n", result);
}
