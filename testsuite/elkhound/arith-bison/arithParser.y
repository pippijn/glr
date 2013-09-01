%{
#include "arith.h"
#include <stdio.h>
%}

%defines "arithParser.h"
%error-verbose
%locations
%define api.pure

%token TOK_NUMBER
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_EQUALS
%token TOK_IF
%token TOK_THEN
%token TOK_ELSE
%token TOK_END

%left TOK_EQUALS
%left TOK_OROR
%left TOK_ANDAND
%left TOK_PLUS TOK_MINUS
%left TOK_TIMES TOK_DIVIDE

%parse-param { int *result }

%%

parse
	: exp					{ *result = $1; }

exp
	: exp TOK_PLUS exp			{ $$ = $1 + $3; }
	| exp TOK_MINUS exp			{ $$ = $1 - $3; }
	| exp TOK_TIMES exp			{ $$ = $1 * $3; }
	| exp TOK_DIVIDE exp			{ $$ = $1 / $3; }
	| TOK_NUMBER				{ $$ = $1; }
	| TOK_LPAREN exp TOK_RPAREN		{ $$ = $2; }
	| TOK_IF boolean_exp TOK_THEN exp TOK_ELSE exp TOK_END
						{ $$ = $2 ? $4 : $6; }


boolean_exp
	: exp TOK_EQUALS exp			{ $$ = $1 == $3; }
	| boolean_exp TOK_OROR boolean_exp	{ $$ = $1 || $3; }
	| boolean_exp TOK_ANDAND boolean_exp	{ $$ = $1 && $3; }
