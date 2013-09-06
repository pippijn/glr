%token TOK_EOF
%token<int> TOK_NUMBER
%token TOK_PLUS
%token TOK_MINUS
%token TOK_TIMES
%token TOK_DIVIDE
%token TOK_LPAREN
%token TOK_RPAREN
%token TOK_EQUALS
%token TOK_OROR
%token TOK_ANDAND
%token TOK_IF
%token TOK_THEN
%token TOK_ELSE
%token TOK_END

%left TOK_OROR
%left TOK_ANDAND
%left TOK_PLUS TOK_MINUS
%left TOK_TIMES TOK_DIVIDE

%start<int> parse

%%

parse:
	| exp TOK_EOF				{ $1 }

exp:
	| e1=exp TOK_PLUS e2=exp		{ e1 + e2 }
	| e1=exp TOK_MINUS e2=exp		{ e1 - e2 }
	| e1=exp TOK_TIMES e2=exp		{ e1 * e2 }
	| e1=exp TOK_DIVIDE e2=exp		{ e1 / e2 }
	| n=TOK_NUMBER				{ n }
	| p=grouping				{ p }
	| TOK_IF c=boolean_exp TOK_THEN e1=exp TOK_ELSE e2=exp TOK_END
						{ if c then e1 else e2 }


boolean_exp:
	| e1=exp TOK_EQUALS e2=exp			{ e1 = e2 }
	| e1=boolean_exp TOK_OROR e2=boolean_exp	{ e1 || e2 }
	| e1=boolean_exp TOK_ANDAND e2=boolean_exp	{ e1 && e2 }


grouping:
	| TOK_LPAREN e=exp TOK_RPAREN		{ e }
