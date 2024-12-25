
(* The type of tokens. *)

type expr =
  | Bool of bool
  | Num of float
  | Neg of expr
  | Var of string
  | Add of expr * expr
  | Sub of expr * expr
  | Mul of expr * expr
  | Div of expr * expr
  | Caret of expr * expr
  | Lt of expr * expr
  | Lteq of expr * expr
  | Gteq of expr * expr
  | Gt of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Func of string * expr
  | Max of expr * expr
  | Func2 of string * expr * expr
  | Ternary of expr * expr * expr
  | Let of string * expr
  | Seq of expr list
  