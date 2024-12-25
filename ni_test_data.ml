open Ni_calc_types
open Ni_calc_json

type ni_test_case = {
  frequency: pay_frequency;
  gross_pay: float;
  period: int;
  category: ni_category;
  expected_employee: float;
  expected_employer: float;
  expected_82: float;
  expected_82a: float;
  expected_169: float;
  expected_86aa: float;
  expected_86ba: float;
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
    ~gross_pay:175.03         (* Gross pay amount *)
    spec                      (* Calculation specification *)
    steps                    (* Earnings limits *)
  in

  (* Access results *)
  Printf.printf "Employee NI: %.2f\n" result.employee_ni;
  Printf.printf "Employer NI: %.2f\n" result.employer_ni;
  Printf.printf "Total NI: %.2f\n" result.total_ni;

  (* Step results are also available *)
  List.iter (fun (step:step_result) ->
    Printf.printf "Step %d: %s = %.2f\n" 
      step.step_num 
      step.name 
      step.result
  ) result.steps

(* Example for 4-weekly paid employee *)
let calculate_4weekly_example () =
  let result = Calculator.calculate_ni
    ~category:B
    ~freq:FourWeekly
    ~gross_pay:760.12
    spec
    steps
  in
  Printf.printf "4-weekly Employee NI: %.2f\n" result.employee_ni;
  Printf.printf "4-weekly Employer NI: %.2f\n" result.employer_ni

(* Example for monthly paid employee with Freeports relief *)
let calculate_monthly_freeports () =
  let result = Calculator.calculate_ni
    ~category:F    (* Category F for Freeports *)
    ~freq:Monthly
    ~gross_pay:5000.0
    spec
    steps
  in
  Printf.printf "Monthly Freeports Employee NI: %.2f\n" result.employee_ni;
  Printf.printf "Monthly Freeports Employer NI: %.2f\n" result.employer_ni

let validate_test_case case =
  let result_json = calculate_ni_json ~category:case.category ~freq:case.frequency ~gross_pay:case.gross_pay in
  Printf.printf "Employee NI JSON: %.2f\n" result_json.employee_ni;
  Printf.printf "Employer NI JSON: %.2f\n" result_json.employer_ni;
  let passes_json = 
    Float.abs(result_json.employee_ni -. case.expected_employee) < 0.015 &&
    Float.abs(result_json.employer_ni -. case.expected_employer) < 0.015 in

  if not passes_json then
    Printf.printf "FAIL JSON: Cat %s Gross %.2f\nExpected: emplyee=%.2f emplyer=%.2f\nGot: emplyee=%.2f emplyer=%.2f\n"
      (show_ni_category case.category)
      case.gross_pay
      case.expected_employee case.expected_employer
      result_json.employee_ni result_json.employer_ni;
(*
  let result = calculate_ni ~category:case.category ~freq:case.frequency ~gross_pay:case.gross_pay in
  Printf.printf "Employee NI: %.2f\n" result.employee_ni;
  Printf.printf "Employer NI: %.2f\n" result.employer_ni;

  let passes = 
    Float.abs(result.employee_ni -. case.expected_employee) < 0.015 &&
    Float.abs(result.employer_ni -. case.expected_employer) < 0.015 &&
    Float.abs(result.table_82 -. case.expected_82) < 0.015 &&
    Float.abs(result.table_82a -. case.expected_82a) < 0.015 &&
    Float.abs(result.table_169 -. case.expected_169) < 0.015 &&
    Float.abs(result.table_86aa -. case.expected_86aa) < 0.015 &&
    Float.abs(result.table_86ba -. case.expected_86ba) < 0.015 in

    if not passes then
    Printf.printf "FAIL: Cat %s Gross %.2f\nExpected: emp=%.2f er=%.2f\nGot: emp=%.2f er=%.2f\n"
      (show_ni_category case.category)
      case.gross_pay
      case.expected_employee case.expected_employer
      result.employee_ni result.employer_ni;
*)      
  passes_json
;;
(* Built-in test cases if no file provided *)
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
