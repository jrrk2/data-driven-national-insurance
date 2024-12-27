open Num

type pay_frequency = Weekly | TwoWeekly | FourWeekly | Monthly [@@deriving yojson]
type ni_category = A | B | C | F | H | J | M | V | Z [@@deriving yojson]

type token =
  | NUMBER of num
  | IDENT of string
  | PLUS | MINUS | TIMES | DIV | POWER | HASH
  | LPAREN | RPAREN
  | EOF

type expr =
  | Num of num
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
  lel: num;
  pt: num;
  st: num;
  uel: num;
  fust: num;
  izust: num;
	
}
	
type step_result = {
  step_num: int;
  name: string;
  result: num;
  variables_used: (string * num) list;
}

type ni_result = {
  steps: step_result list;
  employee_ni: num;
  employer_ni: num;
  total_ni: num;
  bands: band_results;
}

and band_results = {
  up_to_lel: num;
  lel_to_pt: num;
  pt_to_uel: num;
  above_uel: num;
}
