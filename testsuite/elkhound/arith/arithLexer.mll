(* lexical analyzer for arithmetic language *)
{
  open ArithTokens
}


rule token = parse
| ['0'-'9']+ as i	{ TOK_NUMBER (int_of_string i) }

(* operators, punctuators *)
| "+"			{ TOK_PLUS }
| "-"			{ TOK_MINUS }
| "*"			{ TOK_TIMES }
| "/"			{ TOK_DIVIDE }
| "("			{ TOK_LPAREN }
| ")"			{ TOK_RPAREN }
| "||"			{ TOK_OROR }
| "&&"			{ TOK_ANDAND }
| "="			{ TOK_EQUALS }
| "if"			{ TOK_IF }
| "then"		{ TOK_THEN }
| "else"		{ TOK_ELSE }
| "end"			{ TOK_END }

| [' ''\t''\n']		{ token lexbuf }

| "#" _* "\n"		{ token lexbuf }

| _ as c		{ failwith ("illegal character: '" ^ Char.escaped c ^ "'") }

| eof			{ TOK_EOF }
