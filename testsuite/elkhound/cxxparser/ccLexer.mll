(* simple lexer for C++ *)

{
  open CcTokens
}


let lower	= ['a'-'z']
let upper	= ['A'-'Z']
let digit	= ['0'-'9']

let alpha	= (lower | upper | '$')
let alnum	= (alpha | digit)

let identifier	= (alpha | '_')(alnum | '_')*


rule token = parse
(* whitespace *)
| ['\n' ' ' '\t' '\r']+						{ token lexbuf }

(* keywords, operators *)
| "<"								{ TOK_LESSTHAN }
| ">"								{ TOK_GREATERTHAN }

(* identifier *)
| identifier as id						{ TOK_NAME id }

| eof								{ TOK_EOF }

| _ as c							{ failwith (Char.escaped c) }
