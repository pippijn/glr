(*+ -auto-loc
 *)
(* simple lexer for C++ *)

{
  open CcTokens
  open Cc_keywords

  let line = ref 1

  let return lexbuf token =
    token(*,
    Lexing.lexeme_start_p lexbuf,
    Lexing.lexeme_end_p lexbuf*)
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
| "("								{ return lexbuf TOK_LPAREN }
| ")"								{ return lexbuf TOK_RPAREN }
| "[" | "<:"							{ return lexbuf TOK_LBRACKET }
| "]" | ":>"							{ return lexbuf TOK_RBRACKET }
| "{" | "<%"							{ return lexbuf TOK_LBRACE }
| "}" | "%>"							{ return lexbuf TOK_RBRACE }
| "->"								{ return lexbuf TOK_ARROW }
| "::"								{ return lexbuf TOK_COLONCOLON }
| "."								{ return lexbuf TOK_DOT }
| "!"								{ return lexbuf TOK_BANG }
| "~"								{ return lexbuf TOK_TILDE }
| "+"								{ return lexbuf TOK_PLUS }
| "-"								{ return lexbuf TOK_MINUS }
| "++"								{ return lexbuf TOK_PLUSPLUS }
| "--"								{ return lexbuf TOK_MINUSMINUS }
| "&"								{ return lexbuf TOK_AND }
| "*"								{ return lexbuf TOK_STAR }
| ".*"								{ return lexbuf TOK_DOTSTAR }
| "->*"								{ return lexbuf TOK_ARROWSTAR }
| "/"								{ return lexbuf TOK_SLASH }
| "%"								{ return lexbuf TOK_PERCENT }
| "<<"								{ return lexbuf TOK_LEFTSHIFT }
| ">>"								{ return lexbuf TOK_RIGHTSHIFT }
| "<"								{ return lexbuf TOK_LESSTHAN }
| "<="								{ return lexbuf TOK_LESSEQ }
| ">"								{ return lexbuf TOK_GREATERTHAN }
| ">="								{ return lexbuf TOK_GREATEREQ }
| "=="								{ return lexbuf TOK_EQUALEQUAL }
| "!="								{ return lexbuf TOK_NOTEQUAL }
| "^"								{ return lexbuf TOK_XOR }
| "|"								{ return lexbuf TOK_OR }
| "&&"								{ return lexbuf TOK_ANDAND }
| "||"								{ return lexbuf TOK_OROR }
| "?"								{ return lexbuf TOK_QUESTION }
| ":"								{ return lexbuf TOK_COLON }
| "="								{ return lexbuf TOK_EQUAL }
| "*="								{ return lexbuf TOK_STAREQUAL }
| "/="								{ return lexbuf TOK_SLASHEQUAL }
| "%="								{ return lexbuf TOK_PERCENTEQUAL }
| "+="								{ return lexbuf TOK_PLUSEQUAL }
| "-="								{ return lexbuf TOK_MINUSEQUAL }
| "&="								{ return lexbuf TOK_ANDEQUAL }
| "^="								{ return lexbuf TOK_XOREQUAL }
| "|="								{ return lexbuf TOK_OREQUAL }
| "<<="								{ return lexbuf TOK_LEFTSHIFTEQUAL }
| ">>="								{ return lexbuf TOK_RIGHTSHIFTEQUAL }
| ","								{ return lexbuf TOK_COMMA }
| "..."								{ return lexbuf TOK_ELLIPSIS }
| ";"								{ return lexbuf TOK_SEMICOLON }

(* GNU *)
| ">?"								{ return lexbuf TOK_MAX_OP }
| "<?"								{ return lexbuf TOK_MIN_OP }

(* C++ comments *)
| "//" [^ '\n']*						{ token lexbuf }

(* C comments *)
| "/*" ([^ '*'] | "*" [^ '/'])* "*/"				{ token lexbuf }

(* identifier *)
| identifier as id						{ return lexbuf (classify id) }

(* integers *)
| xh h+ is?
| xb b+ is?
| '0'o+ is?
| d+	is? as i						{ return lexbuf (TOK_INT_LITERAL i) }

(* floats *)
| d+e		 fs?
| d*'.'d+e?	 fs?
| d+'.'d*e?	 fs?
| xh h*p h*	 fs?
| xh h*'.'h*p h* fs? as f					{ return lexbuf (TOK_FLOAT_LITERAL f) }

(* strings *)
| 'L'?sstring as c						{ return lexbuf (TOK_CHAR_LITERAL c) }
| 'L'?dstring as s						{ return lexbuf (TOK_STRING_LITERAL s) }

| "#pragma" [^ '\n']+						{ token lexbuf }


| eof								{ return lexbuf TOK_EOF }

| _ as c							{ failwith (Char.escaped c) }
