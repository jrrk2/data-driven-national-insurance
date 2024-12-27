
%token EOF
%token LPAREN RPAREN
%token <string> NUM
%token PLUS MINUS MULTIPLY DIVIDE CARET LT LTEQ GTEQ GT AND OR MAX COMMA QUERY COLON EQUALS SEMICOLON LET
%token <string> VAR

%left QUERY COLON
%left OR
%left AND
%left LT LTEQ
%left GT GTEQ
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG	/* negation -- unary minus */
%right CARET	/* exponentiation */

%start tree
%type <Calc.expr> tree

/* Grammar follows */
%%

tree:  stmt_lst EOF	{ Calc.Seq $1 }

stmt_lst: stmt { [ $1 ] }
        | stmt SEMICOLON stmt_lst { $1 :: $3 }

stmt:	  exp { $1 }
        | LET VAR EQUALS exp { Let ($2, $4) }
        | VAR EQUALS VAR { Let ($1, Var $3) }
	
exp:	NUM			{ Num (Expr.tonum $1) }
	| VAR			{ Var $1 }
	| MAX exp COMMA exp RPAREN	{ Max ($2, $4) }
	| exp QUERY exp COLON exp	{ Ternary ($1, $3, $5) }
	| VAR LPAREN exp COMMA exp RPAREN	{ Func2 ($1, $3, $5) }
	| VAR LPAREN exp RPAREN	{ Func ($1, $3) }
	| exp PLUS exp		{ Add ($1, $3) }
	| exp MINUS exp		{ Sub ($1, $3) }
	| exp MULTIPLY exp	{ Mul ($1, $3) }
	| exp DIVIDE exp	{ Div ($1, $3) }
	| exp LT exp		{ Lt ($1, $3) }
	| exp LTEQ exp		{ Lteq ($1, $3) }
	| exp GTEQ exp		{ Gteq ($1, $3) }
	| exp GT exp		{ Gt ($1, $3) }
	| exp AND exp		{ And ($1, $3) }
	| exp OR exp		{ Or ($1, $3) }
	| MINUS exp %prec NEG	{ Neg $2 }
	| exp CARET exp		{ Caret($1, $3) }
	| LPAREN exp RPAREN	{ $2 }
;

%%
