open Yojson.Basic.Util
open Ni_calc_types

let show_ni_category = function
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | F -> "F"
  | H -> "H"
  | J -> "J"
  | M -> "M"
  | V -> "V"
  | Z -> "Z"

module Formula = struct

(* Helper to print both the expression and its evaluation *)
  
  let evaluate acclst equation variables =
      print_endline ("formula: "^equation);
      List.iter (fun (s,n) -> Hashtbl.replace acclst s (Calc.Num n)) variables;
      let acc = Expr.expr equation in
      print_endline ("AST = "^Expr.dumpast acc);
      match Expr.simplify acclst acc with
           | Calc.Num num -> num
           | Seq lst -> (match List.hd (List.rev lst) with Calc.Num num -> num | oth -> failwith "undecidable evaluation")
           | oth -> failwith "unhandled result"
end

(* Configuration module with safe number parsing *)
module Config = struct

  let load_spec filename =
    (* Entry point *)
    let spec = Json_parser_main.parse_json_file filename in

    let calc_method = spec
      |> member "calculationFormulae" 
      |> member "exactPercentageMethod" in
    let steps = calc_method |> member "steps" |> to_list |> List.map (fun step ->
      {
        step_number = step |> member "stepNumber" |> to_int;
        name = step |> member "name" |> to_string;
        equation = step |> member "equation" |> to_string;
        variables = try step |> member "variables" |> to_assoc 
                   |> List.map (fun (k, v) -> (k, to_string v)) with _ -> []
      } 
    ) in
    spec, steps
end

module Calculator = struct
  let period_multiplier = function
    | Weekly -> 1.0
    | TwoWeekly -> 2.0
    | FourWeekly -> 4.0
    | Monthly -> 13.0 /. 3.0

  let round_to_pennies x =
    let pennies = x *. 100.0 in
    let third_decimal = Float.abs(pennies *. 10.0 -. Float.floor(pennies *. 10.0)) in
    if third_decimal <= 0.5 then
      Float.floor(pennies) /. 100.0
    else
      Float.ceil(pennies) /. 100.0

  (* New rounding functions based on NI guidance *)
  let round_p x =
    Float.ceil x  (* Always round up to nearest pound *)

  let round_p1 ~period x =
    if period = 1.0 then
      Float.round x  (* Round to nearest pound if period = 1 *)
    else 
      Float.ceil x   (* Round up if period > 1 *)

  let contains eq p = try Str.search_forward (Str.regexp p) eq 0 >= 0 with _ -> false

  let execute_step acclst (step: calculation_step) variables period =
    Printf.printf "Executing step %d: %s\n" step.step_number step.equation;
    Printf.printf "Variables: %s\n" 
      (String.concat ", " 
         (List.map (fun (k,v) -> Printf.sprintf "%s=%.2f" k v) variables));
    
    let evaluate_subexpression expr = Formula.evaluate acclst expr variables in

    let result = evaluate_subexpression step.equation in

    Printf.printf "Step %d result: %.2f\n\n" step.step_number result;
        {
          step_num = step.step_number;
          name = step.name;
          result;
          variables_used = variables
        }

let get_rates json category =

  let to_float_safe json =
    try to_float json
    with Type_error _ -> float_of_int (to_int json) in

    let limits = json |> member "earningsLimits" |> member "weekly" in
    let earnings_limits = {
      lel = limits |> member "LEL" |> to_float_safe;
      pt = limits |> member "PT" |> to_float_safe;
      st = limits |> member "ST" |> to_float_safe;
      uel = limits |> member "UEL" |> to_float_safe;
      fust = limits |> member "FUST" |> to_float_safe;
      izust = limits |> member "IZUST" |> to_float_safe;
    } in

  let emp_rates = json 
    |> member "calculationFormulae" 
    |> member "exactPercentageMethod"
    |> member "employeeNICs" 
    |> member "categoryRates"
    |> member (show_ni_category category) in

  print_endline (Yojson.Basic.to_string emp_rates);

  let er_rates = json 
    |> member "calculationFormulae" 
    |> member "exactPercentageMethod"
    |> member "employerNICs" 
    |> member "categoryRates"
    |> member (show_ni_category category) in

  print_endline (Yojson.Basic.to_string er_rates);
        
  let emp_band_d = emp_rates |> member "bandD" |> to_float_safe in
  let emp_band_e = emp_rates |> member "bandE" |> to_float_safe in
  let emp_band_f = emp_rates |> member "bandF" |> to_float_safe in

  let er_band_d = er_rates |> member "bandD" |> to_float_safe in
  let er_band_e = er_rates |> member "bandE" |> to_float_safe in
  let er_band_f = er_rates |> member "bandF" |> to_float_safe in

  [
    ("employeeBandD", emp_band_d);
    ("employeeBandE", emp_band_e); 
    ("employeeBandF", emp_band_f);
    ("employerBandD", er_band_d);
    ("employerBandE", er_band_e);
    ("employerBandF", er_band_f);
  ], earnings_limits

  let calculate_ni ~category ~freq ~gross_pay spec steps =
    let period = period_multiplier freq in
    let rates, limits = get_rates spec category in
    
    let base_variables gross_pay freq = [
      ("GP", gross_pay);
      ("LEL", limits.lel);
      ("PT", limits.pt);
      ("ST", limits.st);
      ("UEL", limits.uel);
      ("FUST", limits.fust); 
      ("IZUST", limits.izust);
      ("p", period);
      ("wm", match freq with 
        | Weekly -> 52.0 
        | Monthly -> 12.0
        | TwoWeekly -> 52.0
        | FourWeekly -> 52.0)
    ] in

    let variables = base_variables gross_pay freq @ rates in

    (* Calculate steps according to spec with proper rounding *)
    let acclst = Hashtbl.create 255 in
    let step_results = List.map 
      (fun step -> execute_step acclst step variables period)
      steps in

  let stepn step' = match Hashtbl.find acclst step' with Num n -> n | oth -> failwith step' in
  
  let final_equations = spec
    |> member "calculationFormulae" 
    |> member "exactPercentageMethod" in
  let employee = final_equations |> member "employeeNICs" |> member "equation" |> to_string in
  let employer = final_equations |> member "employerNICs" |> member "equation" |> to_string in
  let employee_ni = Formula.evaluate acclst employee variables in
  let employer_ni = Formula.evaluate acclst employer variables in

    {
      steps = step_results;
      employee_ni;
      employer_ni;
      total_ni = round_to_pennies (employee_ni +. employer_ni);
      bands = {
        up_to_lel = stepn "step1";
        lel_to_pt = stepn "step2";
        pt_to_uel = stepn "step4" +. stepn "step5";
        above_uel = stepn "step6"
      }
    }
end

(* First load the specification and limits *)
let spec_json = try Sys.getenv "SPEC_JSON" with _ -> "nics-specification.json"
let spec, steps = Config.load_spec spec_json

let calculate_ni_json ~category ~freq ~gross_pay =
  let result = Calculator.calculate_ni 
    ~category 
    ~freq 
    ~gross_pay
    spec
    steps in

  (* Access step results *)
  List.iter (fun (step:step_result) ->
    Printf.printf "Step %d: %s = %.2f\n" 
      step.step_num 
      step.name 
      step.result
  ) result.steps;

  result
