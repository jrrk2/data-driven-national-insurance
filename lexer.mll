(* file: lexer.mll *)

{
  open Mfcalc
}

let digit = ['0'-'9']+
let flt = ['0'-'9']+['.']*['0'-'9']*
let ident = ['a'-'z' 'A'-'Z']
let ident_num = ['a'-'z' 'A'-'Z' '0'-'9' '_']
rule token = parse
  | [' ' '\t' '\n']	{ token lexbuf }
  | digit as num { NUM num }
  | flt as num { NUM num }
  | '\195' '\151' { MULTIPLY }
  | '<' '='	{ LTEQ }
  | '>' '='	{ GTEQ }
  | '&' '&'	{ AND }
  | '+'		{ PLUS }
  | '-'		{ MINUS }
  | '*'		{ MULTIPLY }
  | '/'		{ DIVIDE }
  | '^'		{ CARET }
  | '?'		{ QUERY }
  | ':'		{ COLON }
  | ';'		{ SEMICOLON }
  | ','		{ COMMA }
  | '='		{ EQUALS }
  | '<'		{ LT }
  | '>'		{ GT }
  | '|'		{ OR }
  | '&'		{ AND }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | "let "      { LET }
  | "max("      { MAX }
  | ident ident_num* as word
  		{ VAR word }
  | eof		{ EOF }
  | _  as l     { failwith ("unrecognised lexeme: "^String.make 1 (l)) }
  