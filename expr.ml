open Calc
open Num

let debug = ref false

let tonum x =
try
let ix = String.index x '.' in
let first = String.sub x 0 ix in
let scale = String.length x - ix - 1 in
let second = String.sub x (ix + 1) scale in
if false then Printf.printf "x=%s, ix=%d, first=%s, second=%s, scale=%d\n" x ix first second scale;
div_num (num_of_string (first ^ second)) (power_num (num_of_int 10) (num_of_int scale)) with _ -> num_of_string x

let pass x y = compare_num (abs_num (x -/ y)) (tonum "0.015") < 0

let one = num_of_int 1
let hundred = num_of_int 100
let half = div_num (num_of_int 1) (num_of_int 2)
let roundP1 x period =
  if period =/ one then
    round_num x 
  else
ceiling_num x
let roundP x = ceiling_num x
let roundPennies x = let pennies = x */ hundred in if compare_num (mod_num pennies one) half <= 0 then floor_num(pennies)//hundred else ceiling_num (pennies)//hundred

  let create_hashtable size init =
    let tbl = Hashtbl.create size in
    List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
    tbl

  let fun_table = create_hashtable 16 [
    ("roundP", roundP);
    ("roundPennies", roundPennies);
  ]

let rec dumpast = let open Calc in function
  | Bool (bool) -> Printf.sprintf "Bool(%b)" bool
  | Num (n) -> Printf.sprintf "Num(%s)" (approx_num_fix 2 n)
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
| Num n -> approx_num_fix 2 n
| oth -> failwith "dumpacc"

let rec _simplify acclst = function
  | Bool(bool) -> Bool bool
  | Num(float) -> Num float
  | Neg(expr) -> (match simplify acclst expr with Num expr -> Num (minus_num expr) | expr -> Neg expr)
  | Var(string) -> (if Hashtbl.mem acclst string then let value = Hashtbl.find acclst string in simplify acclst value else failwith ("Var "^string^" not found"))
  | Add(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float +/ float2) | _ -> Add(simplify acclst expr, simplify acclst expr2))
  | Sub(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float -/ float2) | _ -> Sub(simplify acclst expr, simplify acclst expr2))
  | Mul(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float */ float2) | _ -> Mul(simplify acclst expr, simplify acclst expr2))
  | Div(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float // float2) | _ -> Div(simplify acclst expr, simplify acclst expr2))
  | Caret(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Num (float **/ float2) | _ -> Caret(simplify acclst expr, simplify acclst expr2))
  | Lt(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Bool (compare_num float float2 < 0) | _ -> Lt(simplify acclst expr, simplify acclst expr2))
  | Lteq(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Bool (compare_num float float2 <= 0) | _ -> Lteq(simplify acclst expr, simplify acclst expr2))
  | Gteq(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Bool (compare_num float float2 >= 0) | _ -> Gteq(simplify acclst expr, simplify acclst expr2))
  | Gt(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Num float, Num float2 -> Bool (compare_num float float2 > 0) | _ -> Gt(simplify acclst expr, simplify acclst expr2))
  | And(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Bool bool, Bool bool2 -> Bool (bool && bool2) | _ -> And(simplify acclst expr, simplify acclst expr2))
  | Or(expr, expr2) -> (match simplify acclst expr, simplify acclst expr2 with Bool bool, Bool bool2 -> Bool (bool || bool2) | _ -> Or(simplify acclst expr, simplify acclst expr2))
  | Func(string, expr2) -> (match Hashtbl.find_opt fun_table string, simplify acclst expr2 with
    | None, _ -> failwith ("function "^string^" not found")
    | Some exp, Num float -> Num (exp float)
    | _, exp -> Func(string, exp))
  | Func2(string, expr1, expr2) -> (match string, simplify acclst expr1, simplify acclst expr2 with
    | "roundP1", Num x, Num period -> Num (roundP1 x period)
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
      let show = function (string, Num v) -> string^" = "^approx_num_fix 2 v | oth -> "?" in
      let before = dumpast expr in
      let after' = dumpast after in
      if !debug && (before <> after') then
        print_endline ("Partial AST: "^before^" simplifies to "^after');
      after

and dump fd acclst = function
  | Bool(bool) -> Printf.printf "%s " (string_of_bool bool)
  | Num(float) -> Printf.printf "%s " (approx_num_fix 2 float)
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
    | Some exp, Num float -> Printf.printf "%s " (approx_num_fix 2 (exp float))
    | _ -> output_string fd (string^"("); dump fd acclst expr2; output_string fd ") ")
  | Func2(string, expr1, expr2) -> (match simplify acclst expr1, simplify acclst expr2 with
    | exp1, exp2 -> output_string fd (string^"("); dump fd acclst exp1; output_string fd ", "; dump fd acclst exp2; output_string fd ") ")
  | Max(expr1, expr2) -> (match simplify acclst expr1, simplify acclst expr2 with
    | Num float1, Num float2 -> Printf.printf "%s " (approx_num_fix 2 (if compare_num float1 float2 > 0 then float1 else float2))
    | exp1, exp2 -> output_string fd ("max("); dump fd acclst exp1; output_string fd ", "; dump fd acclst exp2; output_string fd ") ")
  | Ternary(expr1, expr2, expr3) -> (match simplify acclst expr1, simplify acclst expr2, simplify acclst expr3 with
    | Bool cond, Num float1, Num float2 -> Printf.printf "%s " (approx_num_fix 2 (if cond then float1 else float2))
    | cond, exp1, exp2 -> dump fd acclst cond; output_string fd (" ? "); dump fd acclst exp1; output_string fd " : "; dump fd acclst exp2)
  | Let (lhs, expr) -> output_string fd ("Let "^lhs); dump fd acclst expr
  | Seq (exprlst) -> output_string fd (" [ "); List.iter (dump fd acclst) exprlst; output_string fd (" ] ")

let dump fd acclst x = dump fd acclst x; print_endline ""

let test_roundp () =

  let cases = [
    (tonum "1.80", tonum "2.00");  (* Always rounds up *)
    (tonum "1.85", tonum "2.00");  (* Always rounds up *)
    (tonum "1.81", tonum "2.00");  (* Always rounds up *)
    (tonum "2.00", tonum "2.00");  (* Whole number stays same *)
    (tonum "1.80", tonum "2.00")   (* Always rounds up *)
  ] in
  
  List.iter (fun (input, expected) ->
    let result = roundP input in
    if result =/ expected then
      Printf.printf "PASS: roundP %s = %s\n" 
        (approx_num_fix 2 input) 
        (approx_num_fix 2 result)
    else
      Printf.printf "FAIL: roundP %s got %s expected %s\n"
        (approx_num_fix 2 input)
        (approx_num_fix 2 result)
        (approx_num_fix 2 expected)
  ) cases

let calculate_steps gp lel st pt fust uel p =
  (* Step 1: up to LEL *)
  let step1_raw = gp -/ lel in
  let step1 = roundP1 step1_raw p in
  
  (* Step 2: LEL to ST *)
  let step2_raw = if gp <=/ lel then tonum "0.0"
    else max_num (tonum "0.0") (step1_raw -/ max_num (tonum "0.0") (gp -/ st)) in
  let step2 = roundP1 step2_raw p in

  (* Step 3: ST to PT *)
  let step3_raw = if gp <=/ st then tonum "0.0"
    else max_num (tonum "0.0") ((gp -/ st) -/ max_num (tonum "0.0") (gp -/ pt)) in
  let step3 = roundP1 step3_raw p in

  (* Step 4: PT to FUST *)
  let step4_raw = if gp <=/ pt then tonum "0.0"
    else if gp <=/ fust then
      max_num (tonum "0.0") ((gp -/ pt) -/ max_num (tonum "0.0") (gp -/ fust))
    else gp -/ pt in
  let step4 = roundP1 step4_raw p in

  (* Step 5: FUST to UEL *)
  let step5_raw = if gp >=/ uel then gp -/ fust
    else max_num (tonum "0.0") ((gp -/ fust) -/ max_num (tonum "0.0") (gp -/ uel)) in
  let step5 = roundP1 step5_raw p in

  (* Step 6: Above UEL *)
  let step6_raw = if gp >=/ uel then gp -/ uel else tonum "0.0" in
  let step6 = roundP1 step6_raw p in

  Printf.printf "Raw values:\n";
  Printf.printf "Step 1 raw: %s\n" (approx_num_fix 2 step1_raw);
  Printf.printf "Step 2 raw: %s\n" (approx_num_fix 2 step2_raw);
  Printf.printf "Step 3 raw: %s\n" (approx_num_fix 2 step3_raw);
  Printf.printf "Step 4 raw: %s\n" (approx_num_fix 2 step4_raw);
  Printf.printf "Step 5 raw: %s\n" (approx_num_fix 2 step5_raw);
  Printf.printf "Step 6 raw: %s\n" (approx_num_fix 2 step6_raw);
  
  Printf.printf "\nRounded values:\n";
  Printf.printf "Step 1: %s\n" (approx_num_fix 2 step1);
  Printf.printf "Step 2: %s\n" (approx_num_fix 2 step2);
  Printf.printf "Step 3: %s\n" (approx_num_fix 2 step3);
  Printf.printf "Step 4: %s\n" (approx_num_fix 2 step4);
  Printf.printf "Step 5: %s\n" (approx_num_fix 2 step5);
  Printf.printf "Step 6: %s\n" (approx_num_fix 2 step6);
  
  let er_band_d = tonum "0.138" in
  let er_nics = if gp >/ st then
    roundPennies((step3 +/ step4) */ er_band_d +/ step5 */ er_band_d +/ step6 */ er_band_d)
  else tonum "0.0" in
  
  Printf.printf "\nNICs:\n";
  Printf.printf "Employer NI: %s\n" (approx_num_fix 2 er_nics)

let test_boundary_case () =
  let lel = tonum "123.00" in
  let st = tonum "175.00" in
  let pt = tonum "242.00" in
  let fust = tonum "481.00" in
  let uel = tonum "967.00" in
  let gp = tonum "123.53" in
  let p = tonum "13.0" in

  calculate_steps gp lel st pt fust uel p
