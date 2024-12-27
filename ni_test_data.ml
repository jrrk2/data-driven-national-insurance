open Ni_calc_types
open Ni_calc_json
open Expr
open Num

type ni_test_case = {
  frequency: pay_frequency;
  gross_pay: num;
  period: int;
  category: ni_category;
  expected_employee: num;
  expected_employer: num;
  expected_82: num;
  expected_82a: num;
  expected_169: num;
  expected_86aa: num;
  expected_86ba: num;
}

let parse_frequency = function
  | "Weekly" -> Weekly
  | "2-weekly" -> TwoWeekly
  | "4-weekly" -> FourWeekly 
  | "Monthly" -> Monthly
  | f -> failwith ("Unknown frequency: " ^ f)

let parse_category = function
  | "A" -> A
  | "B" -> B 
  | "C" -> C
  | "H" -> H
  | "J" -> J
  | "M" -> M
  | "V" -> V
  | "Z" -> Z
  | c -> failwith ("Unknown category: " ^ c)

let flt attr str =
  if false then print_endline ("Converting: " ^ attr ^ " " ^ str);
  try Scanf.sscanf str "£%f" (fun f -> f) with _ ->
  try Scanf.sscanf str "%f" (fun f -> f) with _ ->
  print_endline (attr ^ " " ^ str ^ " converted to 0.0"); 0.0

(* Example calculation for a weekly-paid employee *)
let calculate_example () =
  let result = Calculator.calculate_ni
    ~category:A        (* Category A NI *)
    ~freq:Weekly      (* Weekly paid *)
    ~gross_pay:(tonum "175.03")         (* Gross pay amount *)
    spec                      (* Calculation specification *)
    steps                    (* Earnings limits *)
  in

  (* Access results *)
  Printf.printf "Employee NI: %s\n" (approx_num_fix 2 result.employee_ni);
  Printf.printf "Employer NI: %s\n" (approx_num_fix 2 result.employer_ni);
  Printf.printf "Total NI: %s\n" (approx_num_fix 2 result.total_ni);

  (* Step results are also available *)
  List.iter (fun (step:step_result) ->
    Printf.printf "Step %d: %s = %s\n" 
      step.step_num 
      step.name 
      (approx_num_fix 2 step.result)
  ) result.steps

(* Example for 4-weekly paid employee *)
let calculate_4weekly_example () =
  let result = Calculator.calculate_ni
    ~category:B
    ~freq:FourWeekly
    ~gross_pay:(tonum "760.12")
    spec
    steps
  in
  Printf.printf "4-weekly Employee NI: %s\n" (approx_num_fix 2 result.employee_ni);
  Printf.printf "4-weekly Employer NI: %s\n" (approx_num_fix 2 result.employer_ni)

(* Example for monthly paid employee with Freeports relief *)
let calculate_monthly_freeports () =
  let result = Calculator.calculate_ni
    ~category:F    (* Category F for Freeports *)
    ~freq:Monthly
    ~gross_pay:(tonum "5000.0")
    spec
    steps
  in
  Printf.printf "Monthly Freeports Employee NI: %s\n" (approx_num_fix 2 result.employee_ni);
  Printf.printf "Monthly Freeports Employer NI: %s\n" (approx_num_fix 2 result.employer_ni)

let validate_test_case case =
  let result_json = calculate_ni_json ~category:case.category ~freq:case.frequency ~gross_pay:case.gross_pay in
  Printf.printf "Employee NI JSON: %s\n" (approx_num_fix 2 result_json.employee_ni);
  Printf.printf "Employer NI JSON: %s\n" (approx_num_fix 2 result_json.employer_ni);
  let passes_json = 
    pass result_json.employee_ni case.expected_employee &&
    pass result_json.employer_ni case.expected_employer in

  if not passes_json then
    Printf.printf "FAIL JSON: Cat %s Gross %s\nExpected: emplyee=%s emplyer=%s\nGot: emplyee=%s emplyer=%s\n"
      (show_ni_category case.category)
      (approx_num_fix 2 case.gross_pay)
      (approx_num_fix 2 case.expected_employee) (approx_num_fix 2 case.expected_employer)
      (approx_num_fix 2 result_json.employee_ni) (approx_num_fix 2 result_json.employer_ni);
(*
  let result = calculate_ni ~category:case.category ~freq:case.frequency ~gross_pay:case.gross_pay in
  Printf.printf "Employee NI: %s\n" (approx_num_fix 2 result.employee_ni);
  Printf.printf "Employer NI: %s\n" (approx_num_fix 2 result.employer_ni);

  let passes = 
    pass result.employee_ni case.expected_employee &&
    pass result.employer_ni case.expected_employer &&
    pass result.table_82 case.expected_82 &&
    pass result.table_82a case.expected_82a &&
    pass result.table_169 case.expected_169 &&
    pass result.table_86aa case.expected_86aa &&
    pass result.table_86ba case.expected_86ba in

    if not passes then
    Printf.printf "FAIL: Cat %s Gross %.2f\nExpected: emp=%.2f er=%.2f\nGot: emp=%.2f er=%.2f\n"
      (show_ni_category case.category)
      case.gross_pay
      case.expected_employee case.expected_employer
      result.employee_ni result.employer_ni;
*)      
  passes_json
;;
(* Built-in test cases if no file provided
let test_cases = [|
  (* Cat A - Weekly *)
  {
    frequency = Weekly;
    gross_pay = 175.03;
    period = 1;
    category = A;
    expected_employee = 0.00;
    expected_employer = 0.00;
    expected_82 = 123.00;
    expected_82a = 52.03;
    expected_169 = 0.00;
    expected_86aa = 0.00;
    expected_86ba = 0.00
  };
  {
    frequency = Weekly;
    gross_pay = 967.50;
    period = 1;
    category = A;
    expected_employee = 58.01;
    expected_employer = 109.37;
    expected_82 = 123.00;
    expected_82a = 119.00;
    expected_169 = 725.00;
    expected_86aa = 109.37;
    expected_86ba = 58.01
  };
|]
*)
