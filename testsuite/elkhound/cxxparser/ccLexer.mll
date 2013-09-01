(* simple lexer for C++ *)

{
  open CcTokens
  open Cc_keywords
}


let lower    = ['a'-'z']
let upper    = ['A'-'Z']

let digit    = ['0'-'9']

let alpha = (lower | upper | '$')
let alnum = (alpha | digit)

let identifier = (alpha | '_')(alnum | '_')*

let bstring = '`' ('\\' _ | [^ '\n' '\\' '`' ])* '`'
let dstring = '"' ('\\' _ | [^ '\n' '\\' '"' ])* '"'
let sstring = "'" ('\\' _ | [^ '\n' '\\' '\''])* "'"


let d = digit
let o = ['0'-'7']
let h = ['a'-'f' 'A'-'F' '0'-'9']
let xh = ('0'['x''X'])
let b = ['0' '1']
let xb = ('0'['b''B'])
let e = (['E''e']['+''-']?d+)
let p = (['P''p']['+''-']?d+)
let fs = (['i' 'j' 'f' 'F' 'l' 'L' 'q' 'Q' 'd' 'D']+)
let is = (['i' 'j' 'u' 'l' 'U' 'L']+)

let ws = [' ' '\t' '\r']

let u = ['\x80'-'\xbf']


rule token = parse
(* whitespace *)
| '\n'								{ Lexing.new_line lexbuf; token lexbuf }
| [' ' '\t' '\r']+						{ token lexbuf }

(* keywords, operators *)
| "__extension__"						{ token lexbuf }
| "("								{ TOK_LPAREN }
| ")"								{ TOK_RPAREN }
| "[" | "<:"							{ TOK_LBRACKET }
| "]" | ":>"							{ TOK_RBRACKET }
| "{" | "<%"							{ TOK_LBRACE }
| "}" | "%>"							{ TOK_RBRACE }
| "->"								{ TOK_ARROW }
| "::"								{ TOK_COLONCOLON }
| "."								{ TOK_DOT }
| "!"								{ TOK_BANG }
| "~"								{ TOK_TILDE }
| "+"								{ TOK_PLUS }
| "-"								{ TOK_MINUS }
| "++"								{ TOK_PLUSPLUS }
| "--"								{ TOK_MINUSMINUS }
| "&"								{ TOK_AND }
| "*"								{ TOK_STAR }
| ".*"								{ TOK_DOTSTAR }
| "->*"								{ TOK_ARROWSTAR }
| "/"								{ TOK_SLASH }
| "%"								{ TOK_PERCENT }
| "<<"								{ TOK_LEFTSHIFT }
| ">>"								{ TOK_RIGHTSHIFT }
| "<"								{ TOK_LESSTHAN }
| "<="								{ TOK_LESSEQ }
| ">"								{ TOK_GREATERTHAN }
| ">="								{ TOK_GREATEREQ }
| "=="								{ TOK_EQUALEQUAL }
| "!="								{ TOK_NOTEQUAL }
| "^"								{ TOK_XOR }
| "|"								{ TOK_OR }
| "&&"								{ TOK_ANDAND }
| "||"								{ TOK_OROR }
| "?"								{ TOK_QUESTION }
| ":"								{ TOK_COLON }
| "="								{ TOK_EQUAL }
| "*="								{ TOK_STAREQUAL }
| "/="								{ TOK_SLASHEQUAL }
| "%="								{ TOK_PERCENTEQUAL }
| "+="								{ TOK_PLUSEQUAL }
| "-="								{ TOK_MINUSEQUAL }
| "&="								{ TOK_ANDEQUAL }
| "^="								{ TOK_XOREQUAL }
| "|="								{ TOK_OREQUAL }
| "<<="								{ TOK_LEFTSHIFTEQUAL }
| ">>="								{ TOK_RIGHTSHIFTEQUAL }
| ","								{ TOK_COMMA }
| "..."								{ TOK_ELLIPSIS }
| ";"								{ TOK_SEMICOLON }

(* GNU *)
| ">?"								{ TOK_MAX_OP }
| "<?"								{ TOK_MIN_OP }

(* C++ comments *)
| "//" [^ '\n']*						{ token lexbuf }

(* C comments *)
| "/*" ([^ '*'] | "*" [^ '/'])* "*/"				{ token lexbuf }

(* identifier *)
| identifier as id						{ (classify id) }

(* integers *)
| xh h+ is?
| xb b+ is?
| '0'o+ is?
| d+	is? as i						{ (TOK_INT_LITERAL i) }

(* floats *)
| d+e		 fs?
| d*'.'d+e?	 fs?
| d+'.'d*e?	 fs?
| xh h*p h*	 fs?
| xh h*'.'h*p h* fs? as f					{ (TOK_FLOAT_LITERAL f) }

(* strings *)
| 'L'?sstring as c						{ (TOK_CHAR_LITERAL c) }
| 'L'?dstring as s						{ (TOK_STRING_LITERAL s) }

| "#pragma" [^ '\n']+						{ token lexbuf }


| eof								{ TOK_EOF }

| _ as c							{ failwith (Char.escaped c) }
