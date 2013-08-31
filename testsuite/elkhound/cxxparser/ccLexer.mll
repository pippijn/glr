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
| '\n'								{ Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t' '\r']+						{ token lexbuf }

(* identifier *)
| identifier as id						{ TOK_NAME id }

| "#pragma" [^ '\n']+						{ token lexbuf }


| eof								{ TOK_EOF }

| _ as c							{ failwith (Char.escaped c) }
