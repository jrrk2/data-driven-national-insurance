%{
  open Yojson.Raw
%}

%token <string> INT
%token <string> FLT
%token <string> STRING
%token TRUE FALSE NULL
%token LBRACE RBRACE LBRACKET RBRACKET COLON COMMA 
%token EOF
%start main
%type <t> main

%%

main:
  | value EOF { $1 }

value:
  | STRING { `Stringlit $1 }
  | INT { `Intlit $1 }
  | FLT { `Floatlit $1 }
  | TRUE { `Bool true }
  | FALSE { `Bool false }
  | NULL { `Null }
  | myobject { `Assoc $1 }
  | myarray { `List $1 }

myobject:
  | LBRACE RBRACE { [] }
  | LBRACE members RBRACE { $2 }

members:
  | member { [$1] }
  | member COMMA members { $1 :: $3 }

member:
  | STRING COLON value { ($1, $3) }

myarray:
  | LBRACKET RBRACKET { [] }
  | LBRACKET elements RBRACKET { $2 }

elements:
  | value { [$1] }
  | value COMMA elements { $1 :: $3 }
