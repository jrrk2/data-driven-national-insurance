open Ni_calc_json
open Printf
open Ni_test_data
open Num

let calc_individual_payment ~category ~freq ~gross_pay =
  let result = calculate_ni_json ~category ~freq ~gross_pay in
  printf "Results for %s with category %s:\n" (approx_num_fix 2 gross_pay) (show_ni_category category);
  printf "Employee NI:   %s\n" (approx_num_fix 2 result.employee_ni);
  printf "Employer NI:   %s\n" (approx_num_fix 2 result.employer_ni);
  printf "Total NI:      %s\n" (approx_num_fix 2 result.total_ni);
(*
  printf "Table 82:      %s\n" (approx_num_fix 2 result.table_82);
  printf "Table 82A:     %s\n" (approx_num_fix 2 result.table_82a);
  printf "Table 169:     %s\n" (approx_num_fix 2 result.table_169)
 *)
  ()

let validate_test_file filename =
  printf "Loading test cases from %s\n" filename;
  match Ni_test_parser.load_test_cases filename with
  | test_cases ->
      let total = List.length test_cases in
      let failures = ref 0 in
      printf "Found %d test cases\n" total;
      
      List.iteri (fun i (case:Ni_test_data.ni_test_case) ->
        printf "\nProcessing test case %d of %d: %s %s..." 
          (i + 1) total 
          (show_ni_category case.category) 
          (approx_num_fix 2 case.gross_pay);
        Out_channel.flush stdout;
        
        if not (Ni_test_data.validate_test_case case) then (
          incr failures;
          printf "\nFAIL: Case %d\n" (i + 1);
          printf "  Category: %s\n" (show_ni_category case.category);
          printf "  Frequency: %s\n" (match case.frequency with
            | Weekly -> "Weekly"
            | TwoWeekly -> "2-weekly"
            | FourWeekly -> "4-weekly"
            | Monthly -> "Monthly");
          printf "  Gross Pay: %s\n" (approx_num_fix 2 case.gross_pay);
          let result = calculate_ni_json
            ~category:case.category 
            ~freq:case.frequency 
            ~gross_pay:case.gross_pay in
          printf "  Employee NI - Expected: %s, Got: %s\n" 
            (approx_num_fix 2 case.expected_employee) (approx_num_fix 2 result.employee_ni);
          printf "  Employer NI - Expected: %s, Got: %s\n"
            (approx_num_fix 2 case.expected_employer) (approx_num_fix 2 result.employer_ni);
        )) test_cases;
      
      printf "\n\nValidation complete:\n";
      printf "Total tests: %d\n" total;
      printf "Passed: %d\n" (total - !failures);
      printf "Failed: %d\n" !failures;
      if !failures = 0 then
        printf "All tests passed!\n"
      else
        printf "Some tests failed - see above for details\n"
        
  | exception e ->
      printf "Error loading test file: %s\n" (Printexc.to_string e)

let _ =
  Expr.test_roundp();
  Expr.test_boundary_case ();
  match Array.to_list (Sys.argv) with
  | [_] ->
      printf "Usage:\n";
      printf "  ni_calc pay <category> <frequency> <gross_pay>\n";
      printf "  ni_calc test <xml_file>\n";
      printf "\nCategories: A, B, C, H, J, M, V, Z\n";
      printf "Frequencies: Weekly, TwoWeekly, FourWeekly, Monthly\n"
  
  | [_; "pay"; category; freq; amount] ->
      let category = Ni_test_data.parse_category category in
      let freq = Ni_test_data.parse_frequency freq in
      let amount = Expr.tonum amount in
      calc_individual_payment ~category ~freq ~gross_pay:amount

  | [_; "test"; filename] ->
      validate_test_file filename

  | _ ->
      printf "Invalid arguments\n"
