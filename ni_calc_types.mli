
type pay_frequency = Weekly | TwoWeekly | FourWeekly | Monthly [@@deriving yojson]
type ni_category = A | B | C | F | H | J | M | V | Z [@@deriving yojson]

type token =
  | NUMBER of float
  | IDENT of string
  | PLUS | MINUS | TIMES | DIV | POWER | HASH
  | LPAREN | RPAREN
  | EOF

type expr =
  | Num of float
  | Var of string
  | BinOp of expr * token * expr
  | Power of expr * expr  
  | Hash of expr * expr  
  | Paren of expr

type calculation_step = {
  step_number: int;
  name: string;
  equation: string;
  variables: (string * string) list;
}

type earnings_limits = {
  lel: float;
  pt: float;
  st: float;
  uel: float;
  fust: float;
  izust: float;
	
}
	
type step_result = {
  step_num: int;
  name: string;
  result: float;
  variables_used: (string * float) list;
}

type ni_result = {
  steps: step_result list;
  employee_ni: float;
  employer_ni: float;
  total_ni: float;
  bands: band_results;
}

and band_results = {
  up_to_lel: float;
  lel_to_pt: float;
  pt_to_uel: float;
  above_uel: float;
}
