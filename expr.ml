open Calc

let debug = ref false

let roundP' x = Float.ceil(x)
let roundP x = let pennies = x *. 100.0 in if mod_float pennies 1. <= 0.5 then Float.floor(pennies)/.100.0 else Float.ceil(pennies)/.100.0
let roundPennies x = let pennies = x *. 100.0 in if mod_float pennies 1. <= 0.5 then Float.floor(pennies)/.100.0 else Float.ceil(pennies)/.100.0

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let fun_table = create_hashtable 16 [
    ("sin", sin);
    ("cos", cos);
    ("tan", tan);
    ("asin", asin);
    ("acos", acos);
    ("atan", atan);
    ("log", log);
    ("exp", exp);
    ("sqrt", sqrt);
    ("roundP", roundP);
    ("roundPennies", roundPennies);
  ]

let rec dumpast = let open Calc in function
  | Bool (bool) -> Printf.sprintf "Bool(%b)" bool
  | Num (float) -> Printf.sprintf "Num(%.2f)" float
  | Neg (expr) -> Printf.sprintf "Neg(%s)" (dumpast expr)
  | Var (string) -> Printf.sprintf "Var(%s)" string
  | Add (expr, expr') -> Printf.sprintf "Add(%s, %s)" (dumpast expr) (dumpast expr')
  | Sub (expr, expr') -> Printf.sprintf "Sub(%s, %s)" (dumpast expr) (dumpast expr')
  | Mul (expr, expr') -> Printf.sprintf "Mul(%s, %s)" (dumpast expr) (dumpast expr')
  | Div (expr, expr') -> Printf.sprintf "Div(%s, %s)" (dumpast expr) (dumpast expr')
  | Caret (expr, expr') -> Printf.sprintf "Caret(%s, %s)" (dumpast expr) (dumpast expr')
  | Lt (expr, expr') -> Printf.sprintf "Lt(%s, %s)" (dumpast expr) (dumpast expr')
  | Lteq (expr, expr') -> Printf.sprintf "Lteq(%s, %s)" (dumpast expr) (dumpast expr')
  | Gteq (expr, expr') -> Printf.sprintf "Gteq(%s, %s)" (dumpast expr) (dumpast expr')
  | Gt (expr, expr') -> Printf.sprintf "Gt(%s, %s)" (dumpast expr) (dumpast expr')
  | And (expr, expr') -> Printf.sprintf "And(%s, %s)" (dumpast expr) (dumpast expr')
  | Or (expr, expr') -> Printf.sprintf "Or(%s, %s)" (dumpast expr) (dumpast expr')
  | Func (string, expr') -> Printf.sprintf "Func(%s, %s)" string (dumpast expr')
  | Func2 (string, expr, expr') -> Printf.sprintf "Func2(%s, %s, %s)" string (dumpast expr) (dumpast expr')
  | Max (expr, expr') -> Printf.sprintf "Max(%s, %s)" (dumpast expr) (dumpast expr')
  | Ternary (cond, expr, expr') -> Printf.sprintf "Ternary(%s, %s, %s)" (dumpast cond) (dumpast expr) (dumpast expr')
  | Let (lhs, expr) -> Printf.sprintf "Let(%s, %s)" lhs (dumpast expr)
  | Seq (lst) -> String.concat "; " (List.map dumpast lst)

let dumpacc = function
| Num n -> string_of_float n
| oth -> failwith "dumpacc"

let rec _simplify acclst = function
  | Bool(bool) -> Bool bool
  | Num(float) -> Num float
  | Neg(expr) -> (match simplify acclst expr with Num expr -> Num (-. expr) | expr -> Neg expr)
  | Var(string) -> (if Hashtbl.mem acclst string then let value = Hashtbl.find acclst string in simplify acclst value else failwith ("Var "^string^" not found"))
  | Add(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float +. float2) | _ -> Add(simplify acclst expr, simplify acclst expr2))
  | Sub(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float -. float2) | _ -> Sub(simplify acclst expr, simplify acclst expr2))
  | Mul(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float *. float2) | _ -> Mul(simplify acclst expr, simplify acclst expr2))
  | Div(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float /. float2) | _ -> Div(simplify acclst expr, simplify acclst expr2))
  | Caret(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float ** float2) | _ -> Caret(simplify acclst expr, simplify acclst expr2))
  | Lt(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Bool (float < float2) | _ -> Lt(simplify acclst expr, simplify acclst expr2))
  | Lteq(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Bool (float <= float2) | _ -> Lteq(simplify acclst expr, simplify acclst expr2))
  | Gteq(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Bool (float >= float2) | _ -> Gteq(simplify acclst expr, simplify acclst expr2))
  | Gt(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Bool (float > float2) | _ -> Gt(simplify acclst expr, simplify acclst expr2))
  | And(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Bool bool, Bool bool2 -> Bool (bool && bool2) | _ -> And(simplify acclst expr, simplify acclst expr2))
  | Or(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Bool bool, Bool bool2 -> Bool (bool || bool2) | _ -> Or(simplify acclst expr, simplify acclst expr2))
  | Func(string, expr2) -> (match Hashtbl.find_opt fun_table string, simplify acclst expr2 with
    | None, _ -> failwith ("function "^string^" not found")
    | Some exp, Num float -> Num (exp float)
    | _, exp -> Func(string, exp))
  | Func2(string, expr1, expr2) -> (match string, simplify acclst expr1, simplify acclst expr2 with
    | "roundP1", Num x, Num period -> Num (if period = 1.0 then Float.round(x) else Float.ceil(x))
    | _, exp1, exp2 -> Func2(string, exp1, exp2))
  | Max(expr1, expr2) -> (match simplify acclst expr1, simplify acclst expr2 with
    | Num float1, Num float2 -> Num (if float1 > float2 then float1 else float2)
    | exp1, exp2 -> Max(exp1, exp2))
  | Ternary(cond, expr1, expr2) -> let s1,s2,s3 = simplify acclst cond, simplify acclst expr1, simplify acclst expr2 in (match s1,s2,s3 with
    | Bool cond, num1, num2 -> if cond then num1 else num2
    | cond, exp1, exp2 -> print_endline ("ternary:");
    dump stdout acclst s1;
    dump stdout acclst s2;
    dump stdout acclst s3;
    print_newline();
    Ternary(cond, expr1, expr2))
  | Let (lhs, expr) -> print_string ("*** LET *** "^lhs^" = "); let rslt = simplify acclst expr in dump stdout acclst rslt; print_newline (); Hashtbl.replace acclst lhs rslt; rslt
  | Seq (lst) -> Seq (List.map (function
    | Let(lhs, expr) as l -> print_endline ("SEQ "^lhs^"="^dumpast l); simplify acclst l
    | Var _ as v -> simplify acclst v
    | oth -> failwith "unsupported seq") lst)

and simplify acclst expr =
      let after = _simplify acclst expr in
      if false then print_endline ("simplify variables="^string_of_int (Hashtbl.length acclst));
      let show = function (string, Num v) -> string^" = "^string_of_float v | oth -> "?" in
      let before = dumpast expr in
      let after' = dumpast after in
      if !debug && (before <> after') then
        print_endline ("Partial AST: "^before^" simplifies to "^after');
      after

and dump fd acclst = function
  | Bool(bool) -> Printf.printf "%s " (string_of_bool bool)
  | Num(float) -> Printf.printf "%.2f " float
  | Neg(expr) -> output_string fd "-("; dump fd acclst expr; output_string fd ") "
  | Var(string) -> (if Hashtbl.mem acclst string then dump fd acclst (Hashtbl.find acclst string) else output_string fd string)
  | Add(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " + "; dump fd acclst expr2; output_string fd ") "
  | Sub(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " - "; dump fd acclst expr2; output_string fd ") "
  | Mul(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " * "; dump fd acclst expr2; output_string fd ") "
  | Div(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " / "; dump fd acclst expr2; output_string fd ") "
  | Caret(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " ** "; dump fd acclst expr2; output_string fd ") "
  | Lt(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " < "; dump fd acclst expr2; output_string fd ") "
  | Lteq(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " <= "; dump fd acclst expr2; output_string fd ") "
  | Gteq(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " >= "; dump fd acclst expr2; output_string fd ") "
  | Gt(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " > "; dump fd acclst expr2; output_string fd ") "
  | And(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " & "; dump fd acclst expr2; output_string fd ") "
  | Or(expr, expr2) -> output_string fd "("; dump fd acclst expr; output_string fd " | "; dump fd acclst expr2; output_string fd ") "
  | Func(string, expr2) -> (match Hashtbl.find_opt fun_table string, simplify acclst expr2 with
    | Some exp, Num float -> Printf.printf "%f " (exp float)
    | _ -> output_string fd (string^"("); dump fd acclst expr2; output_string fd ") ")
  | Func2(string, expr1, expr2) -> (match simplify acclst expr1, simplify acclst expr2 with
    | exp1, exp2 -> output_string fd (string^"("); dump fd acclst exp1; output_string fd ", "; dump fd acclst exp2; output_string fd ") ")
  | Max(expr1, expr2) -> (match simplify acclst expr1, simplify acclst expr2 with
    | Num float1, Num float2 -> Printf.printf "%f " (if float1 > float2 then float1 else float2)
    | exp1, exp2 -> output_string fd ("max("); dump fd acclst exp1; output_string fd ", "; dump fd acclst exp2; output_string fd ") ")
  | Ternary(expr1, expr2, expr3) -> (match simplify acclst expr1, simplify acclst expr2, simplify acclst expr3 with
    | Bool cond, Num float1, Num float2 -> Printf.printf "%f " (if cond then float1 else float2)
    | cond, exp1, exp2 -> dump fd acclst cond; output_string fd (" ? "); dump fd acclst exp1; output_string fd " : "; dump fd acclst exp2)
  | Let (lhs, expr) -> output_string fd ("Let "^lhs); dump fd acclst expr
  | Seq (exprlst) -> output_string fd (" [ "); List.iter (dump fd acclst) exprlst; output_string fd (" ] ")

let dump fd acclst x = dump fd acclst x; print_endline ""

(* Assumes the parser file is "mfcalc.mly" and the lexer file is "lexer.mll". *)
let expr str =
    let lexbuf = Lexing.from_string str in
    Mfcalc.tree Lexer.token lexbuf
