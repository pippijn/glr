%{
#include "eea.h"
#include <stdio.h>

int
merge (int a, int b)
{
  return a + b;
}

#define YYMAXDEPTH 1000000
#define inline __inline__
%}

%glr-parser
%defines "eeaParser.h"
%error-verbose
%locations
%define api.pure

%parse-param { int *result }

%%

parse
	: E					{ *result = $1; }

E
	: E E %merge<merge>			{ $$ = $1 * $2; }
	| 'a'					{ $$ = 1; }
