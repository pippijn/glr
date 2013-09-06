{
  open EeaTokens
}


rule token = parse
| [' ''\n''\t']	{ token lexbuf }
| _		{ A }

| eof		{ TOK_EOF }
