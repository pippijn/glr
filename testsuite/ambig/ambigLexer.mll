(* lexical analyzer for arithmetic language *)
{
  open AmbigTokens
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

| [' ''\t''\n']		{ token lexbuf }

| "#" _* "\n"		{ token lexbuf }

| _ as c		{ failwith ("illegal character: '" ^ Char.escaped c ^ "'") }

| eof			{ TOK_EOF }
