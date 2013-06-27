%{
  open GrammarAst
%}

%token EOF

/* ===================== tokens ============================ */
/* tokens that have many lexical spellings */
%token <int>			TOK_INTEGER
%token <GrammarAst.uname>	TOK_UNAME
%token <GrammarAst.lname>	TOK_LNAME
%token <GrammarAst.str>		TOK_STRING
%token <GrammarAst.code>	TOK_LIT_CODE

/* punctuators */
%token TOK_LBRACK		/* "["	*/
%token TOK_RBRACK		/* "]"	*/
%token TOK_LBRACE		/* "{"	*/
%token TOK_RBRACE		/* "}"	*/
%token TOK_COLON		/* ":"	*/
%token TOK_SEMICOLON		/* ";"	*/
%token TOK_ARROW		/* "->" */
%token TOK_LPAREN		/* "("	*/
%token TOK_RPAREN		/* ")"	*/
%token TOK_COMMA		/* ","	*/

/* keywords */
%token TOK_FUN			/* "fun"		*/
%token TOK_TERMINALS		/* "terminals"		*/
%token TOK_TOKEN		/* "token"		*/
%token TOK_NONTERM		/* "nonterm"		*/
%token TOK_VERBATIM		/* "verbatim"		*/
%token TOK_IMPL_VERBATIM	/* "impl_verbatim"	*/
%token TOK_PRECEDENCE		/* "precedence"		*/
%token TOK_OPTION		/* "option"		*/
%token TOK_SUBSETS		/* "subsets"		*/
%token TOK_DELETE		/* "delete"		*/
%token TOK_REPLACE		/* "replace"		*/
%token TOK_FORBID_NEXT		/* "forbid_next"	*/


/* ===================== productions ======================= */

%start <GrammarAst.topform list> parse
%%

/* The actions in this file simply build an Abstract Syntax Tree (AST)
 * for later processing. */


/* start symbol */
parse
	: top_form_list EOF		{ List.rev $1 }


top_form_list
	: /*empty*/			{ [] }
	| top_form_list top_form	{ $2 :: $1 }


top_form
	: verbatim			{ $1 }
	| parser_option			{ $1 }
	| terminals			{ $1 }
	| nonterminal			{ $1 }


verbatim
	: TOK_VERBATIM TOK_LIT_CODE	     { TF_verbatim (false, $2) }
	| TOK_IMPL_VERBATIM TOK_LIT_CODE     { TF_verbatim (true , $2) }


/* options without specified values default to a value of 1 */
parser_option
	: TOK_OPTION TOK_LNAME		   TOK_SEMICOLON { TF_option ($2,  1) }
	| TOK_OPTION TOK_LNAME TOK_INTEGER TOK_SEMICOLON { TF_option ($2, $3) }
	| TOK_OPTION TOK_LNAME TOK_LNAME   TOK_SEMICOLON { TF_option ($2, (GrammarAst.int_of_boolstring $3)) }


/* ------ terminals ------ */
/*
 * the terminals are the grammar symbols that appear only on the RHS of
 * forms; they are the output of the lexer; the terminals list declares
 * all of the terminals that will appear in the rules
 */
terminals
	: TOK_TERMINALS TOK_LBRACE terminal_decls term_types precedence TOK_RBRACE
		{ TF_terminals (List.rev $3, List.rev $4, $5) }


terminal_decls
	: /* empty */				{ [] }
	| terminal_decls terminal_decl		{ $2 :: $1 }


/* each terminal has an integer code which is the integer value the
 * lexer uses to represent that terminal.  it is followed by a
 * canonical name, and an optional alias; the name/alias appears in
 * the forms, rather than the integer code itself */
terminal_decl
	: TOK_INTEGER TOK_COLON TOK_UNAME TOK_STRING? TOK_SEMICOLON
		{ TermDecl ($1, $3, $4) }


term_types
	: /* empty */			{ [] }
	| term_types term_type		{ $2 :: $1 }


term_type
	: TOK_TOKEN TOK_LIT_CODE TOK_UNAME TOK_SEMICOLON
		{ TermType ($3, $2, []) }
	| TOK_TOKEN TOK_LIT_CODE TOK_UNAME TOK_LBRACE spec_funcs TOK_RBRACE
		{ TermType ($3, $2, List.rev $5) }


precedence
	: /* empty */						{ [] }
	| TOK_PRECEDENCE TOK_LBRACE prec_specs TOK_RBRACE	{ List.rev $3 }


prec_specs
	: /* empty */
		{ [] }
	| prec_specs TOK_LNAME TOK_INTEGER name_or_string_list TOK_SEMICOLON
		{ PrecSpec (Assoc.of_locstring $2, $3, List.rev $4) :: $1 }


name_or_string_list
	: /* empty */				{ [] }
	| name_or_string_list name_or_string	{ $2 :: $1 }


name_or_string
	: TOK_UNAME		{ $1 }
	| TOK_STRING		{ $1 }



/* ------ specification functions ------ */
spec_funcs
	: /* empty */		      { [] }
	| spec_funcs spec_func	      { $2 :: $1 }


spec_func
	: TOK_FUN TOK_LNAME TOK_LPAREN formals_opt(TOK_LNAME) TOK_RPAREN TOK_LIT_CODE
		{ SpecFunc ($2, $4, $6) }


formals_opt(NAME)
	: /* empty */		      { [] }
	| formals(NAME)		      { List.rev $1 }


formals(NAME)
	: NAME				{ [$1] }
	| formals(NAME) TOK_COMMA NAME	{ $3 :: $1 }



/* ------ nonterminals ------ */
/*
 * a nonterminal is a grammar symbol that appears on the LHS of forms;
 * the body of the nonterminal declaration specifies the the RHS forms,
 * attribute info, etc.
 */
nonterminal
	: TOK_NONTERM type_decl TOK_UNAME production
		{ TF_nonterm ($3, $2, [], [$4], []) }
	| TOK_NONTERM type_decl TOK_UNAME TOK_LBRACE spec_funcs productions subsets TOK_RBRACE
		{ TF_nonterm ($3, $2, List.rev $5, List.rev $6, $7) }


type_decl
	: TOK_LIT_CODE			{ Some $1 }
	| /* empty */			{ None }


productions
	: /* empty */			{ [] }
	| productions production	{ $2 :: $1 }


production
	: TOK_ARROW rhs production_name? action
		{ ProdDecl (PDK_NEW, $3, List.rev $2, $4) }
	| TOK_REPLACE TOK_ARROW rhs action
		{ ProdDecl (PDK_REPLACE, None, List.rev $3, $4) }
	| TOK_DELETE TOK_ARROW rhs TOK_SEMICOLON
		{ ProdDecl (PDK_DELETE, None, List.rev $3, None) }


production_name
	: TOK_LBRACK TOK_UNAME TOK_RBRACK
		{ $2 }


action
	: TOK_LIT_CODE				{ Some $1 }
	| TOK_SEMICOLON				{ None }


rhs
	: rhs_elt				{ [$1] }
	| rhs rhs_elt				{ $2 :: $1 }


/*
 * each element on the RHS of a form can have a tag, which appears before a
 * colon (':') if present; the tag is required if that symbol's attributes
 * are to be referenced anywhere in the actions or conditions for the form
 */
rhs_elt
	: TOK_UNAME
		{ RH_name (None, $1) }
	| TOK_LNAME TOK_COLON TOK_UNAME
		{ RH_name (Some $1, $3) }
	| TOK_STRING
		{ RH_string (None, $1) }
	| TOK_LNAME TOK_COLON TOK_STRING
		{ RH_string (Some $1, $3) }
	| TOK_PRECEDENCE TOK_LPAREN name_or_string TOK_RPAREN
		{ RH_prec ($3) }
	| TOK_FORBID_NEXT TOK_LPAREN name_or_string TOK_RPAREN
		{ RH_forbid ($3) }


subsets
	: /*empty*/					{ [] }
	| TOK_SUBSETS formals(TOK_UNAME) TOK_SEMICOLON	{ List.rev $2 }
