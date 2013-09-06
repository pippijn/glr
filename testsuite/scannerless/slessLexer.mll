{
  open SlessTokens
}


rule token = parse
| '('	{ CHAR_LPAREN }
| ')'	{ CHAR_RPAREN }
| '+'	{ CHAR_PLUS }

| ' '	{ CHAR_SPACE }
| '\n'	{ CHAR_NEWLINE }

| 'a'	{ A }
| 'b'	{ B }
| 'c'	{ C }
| 'd'	{ D }
| 'e'	{ E }
| 'f'	{ F }
| 'g'	{ G }
| 'h'	{ H }
| 'i'	{ I }
| 'j'	{ J }
| 'k'	{ K }
| 'l'	{ L }
| 'm'	{ M }
| 'n'	{ N }
| 'o'	{ O }
| 'p'	{ P }
| 'q'	{ Q }
| 'r'	{ R }
| 's'	{ S }
| 't'	{ T }
| 'u'	{ U }
| 'v'	{ V }
| 'w'	{ W }
| 'x'	{ X }
| 'y'	{ Y }
| 'z'	{ Z }

| _	{ failwith ("illegal character: '" ^ Char.escaped (Lexing.lexeme_char lexbuf 0) ^ "'") }

| eof	{ CHAR_EOF }
