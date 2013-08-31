open CcTokens

(* C89 *)
let c89_keywords = [
  "asm",                        TOK_ASM;
  "auto",                       TOK_AUTO;
  "break",                      TOK_BREAK;
  "case",                       TOK_CASE;
  "char",                       TOK_CHAR;
  "const",                      TOK_CONST;
  "continue",                   TOK_CONTINUE;
  "default",                    TOK_DEFAULT;
  "do",                         TOK_DO;
  "double",                     TOK_DOUBLE;
  "else",                       TOK_ELSE;
  "enum",                       TOK_ENUM;
  "extern",                     TOK_EXTERN;
  "float",                      TOK_FLOAT;
  "for",                        TOK_FOR;
  "goto",                       TOK_GOTO;
  "if",                         TOK_IF;
  "int",                        TOK_INT;
  "long",                       TOK_LONG;
  "register",                   TOK_REGISTER;
  "return",                     TOK_RETURN;
  "short",                      TOK_SHORT;
  "signed",                     TOK_SIGNED;
  "sizeof",                     TOK_SIZEOF;
  "static",                     TOK_STATIC;
  "struct",                     TOK_STRUCT;
  "switch",                     TOK_SWITCH;
  "typedef",                    TOK_TYPEDEF;
  "union",                      TOK_UNION;
  "unsigned",                   TOK_UNSIGNED;
  "void",                       TOK_VOID;
  "volatile",                   TOK_VOLATILE;
  "wchar_t",                    TOK_WCHAR_T;
  "while",                      TOK_WHILE;
]

(* C99 *)
let c99_keywords = [
  "_Bool",			TOK_BOOL;
  "inline",                     TOK_INLINE;
  "restrict",                   TOK_RESTRICT;
  "_Complex",                   TOK_COMPLEX;
  "_Imaginary",                 TOK_IMAGINARY;
]

(* C++ keywords *)
let cxx1998_keywords = [
  "bool",                       TOK_BOOL;
  "catch",                      TOK_CATCH;
  "class",                      TOK_CLASS;
  "const_cast",                 TOK_CONST_CAST;
  "delete",                     TOK_DELETE;
  "dynamic_cast",               TOK_DYNAMIC_CAST;
  "explicit",                   TOK_EXPLICIT;
  "export",                     TOK_EXPORT;
  "false",                      TOK_FALSE;
  "friend",                     TOK_FRIEND;
  "inline",                     TOK_INLINE;
  "mutable",                    TOK_MUTABLE;
  "namespace",                  TOK_NAMESPACE;
  "new",                        TOK_NEW;
  "operator",                   TOK_OPERATOR;
  "private",                    TOK_PRIVATE;
  "protected",                  TOK_PROTECTED;
  "public",                     TOK_PUBLIC;
  "reinterpret_cast",           TOK_REINTERPRET_CAST;
  "static_cast",                TOK_STATIC_CAST;
  "template",                   TOK_TEMPLATE;
  "this",                       TOK_THIS;
  "throw",                      TOK_THROW;
  "true",                       TOK_TRUE;
  "try",                        TOK_TRY;
  "typeid",                     TOK_TYPEID;
  "typename",                   TOK_TYPENAME;
  "using",                      TOK_USING;
  "virtual",                    TOK_VIRTUAL;

  (* operators *)
  "and",			TOK_ANDAND;
  "or",				TOK_OROR;
  "bitor",			TOK_OR;
  "xor",			TOK_XOR;
  "compl",			TOK_TILDE;
  "bitand",			TOK_AND;
  "and_eq",			TOK_ANDEQUAL;
  "or_eq",			TOK_OREQUAL;
  "xor_eq",			TOK_XOREQUAL;
  "not",			TOK_BANG;
  "not_eq",			TOK_NOTEQUAL;

  (* XXX: C99 keywords usable in C++, as they are reserved names *)
  "_Complex",                   TOK_COMPLEX;
  "_Imaginary",                 TOK_IMAGINARY;
]

(* C++11 *)
let cxx2011_keywords = [
  "noexcept",                   TOK_NOEXCEPT;
  "constexpr",                  TOK_CONSTEXPR;
  "decltype",                   TOK_DECLTYPE;
  "nullptr",                    TOK_NULLPTR;
  "static_assert",              TOK_STATIC_ASSERT;
  "char16_t",                   TOK_CHAR16_t;
  "char32_t",                   TOK_CHAR32_t;
]

(* GNU *)
let gnu_keywords = [
  "cdecl",                      TOK_CDECL;
  "pascal",                     TOK_PASCAL;
  "__decltype",                 TOK_DECLTYPE;
  "__alignof",                  TOK_SIZEOF;
  "__alignof__",                TOK_SIZEOF;
  "__volatile__",               TOK_VOLATILE;
  "__asm",                      TOK_ASM;
  "__asm__",                    TOK_ASM;
  "__const",                    TOK_CONST;
  "__const__",                  TOK_CONST;
  "__builtin_va_arg",           TOK___BUILTIN_VA_ARG;
  "__builtin_constant_p",       TOK___BUILTIN_CONSTANT_P;
  "__attribute",                TOK___ATTRIBUTE__;
  "__restrict",                 TOK_RESTRICT;
  "__restrict__",               TOK_RESTRICT;
  "__attribute__",              TOK___ATTRIBUTE__;
  "__offsetof__",               TOK___OFFSETOF__;
  "__builtin_offsetof",         TOK___BUILTIN_OFFSETOF;
  "__label__",                  TOK___LABEL__;
  "__FUNCTION__",               TOK___FUNCTION__;
  "__PRETTY_FUNCTION__",        TOK___PRETTY_FUNCTION__;
  "typeof",                     TOK___TYPEOF__;
  "__typeof",                   TOK___TYPEOF__;
  "__typeof__",                 TOK___TYPEOF__;
  "__real__",                   TOK_REAL;
  "__imag__",                   TOK_IMAG;
  "__complex__",                TOK_COMPLEX;
  "__extension__",              TOK___EXTENSION__;
  "__signed",                   TOK_SIGNED;
  "__signed__",                 TOK_SIGNED;
  "__unsigned",                 TOK_UNSIGNED;
  "__unsigned__",               TOK_UNSIGNED;
  "__inline",                   TOK_INLINE;
  "__inline__",                 TOK_INLINE;
]

let keywords = lazy (List.fold_left (fun map (kw, tok) -> StringMap.add kw tok map) StringMap.empty (
  if Options._xc () then
    c89_keywords @ c99_keywords @ gnu_keywords
  else
    c89_keywords @ gnu_keywords @ cxx1998_keywords @ cxx2011_keywords
))

let string_table = Hashtbl.create 16381

let classify id =
  try
    StringMap.find id (Lazy.force keywords)
  with Not_found ->
    try
      TOK_NAME (Hashtbl.find string_table id)
    with Not_found ->
      Hashtbl.add string_table id id;
      TOK_NAME id
