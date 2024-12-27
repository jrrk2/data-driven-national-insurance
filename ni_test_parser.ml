open Xml

(* Helper function to safely extract string content from PCData *)
let get_pcdata = function
  | PCData str -> str
  | _ -> ""

(* Helper function to safely extract float from string *)
let parse_float str =
  try Expr.tonum str
  with _ -> Num.num_of_int 0

(* Extract value from Data element *)
let extract_data_value = function
  | Element ("Data", _, [content]) -> get_pcdata content
  | _ -> ""

(* Extract cell value from Cell element *)
let extract_cell_value = function
  | Element ("Cell", _, elements) -> 
      let data_elements = List.filter 
        (function Element ("Data", _, _) -> true | _ -> false) 
        elements
      in
      (match data_elements with
      | [data] -> extract_data_value data
      | _ -> "")
   | _ -> ""

(* Helper to check if a row looks like a header *)
let is_header_row = function
  | Element ("Row", _, cells) ->
      (* Check first 3 cells for header-like content *)
      let values = List.map extract_cell_value cells in
      (match values with
      | "NI Category"::_ 
      | "YEAR: "::_ -> true 
      | x::_ when String.length x = 0 -> true  (* Empty first cell *)
      | "Pay frequency"::"Gross Pay"::"Period"::_ -> true
      | _ -> false)
  | _ -> false

(* Convert row element to test case *)
let row_to_test_case row =
  if is_header_row row then None
  else (match row with
   | Element ("Row", _, cells) ->
       let cell_values = List.map extract_cell_value cells in
       (match cell_values with
        | [freq; gross_pay; period; category; emp_ni; er_ni; _; t82; t82a; t169; t86aa; t86ba] ->
            Some {
              Ni_test_data.frequency = Ni_test_data.parse_frequency freq;
              gross_pay = parse_float gross_pay;
              period = int_of_string period;
              category = Ni_test_data.parse_category category;
              expected_employee = parse_float emp_ni;
              expected_employer = parse_float er_ni;
              expected_82 = parse_float t82;
              expected_82a = parse_float t82a;
              expected_169 = parse_float t169;
              expected_86aa = parse_float t86aa;
              expected_86ba = parse_float t86ba;
            }
        | _ -> None)
   | _ -> None)

(* Process worksheet *)
let process_worksheet worksheet =
  (match worksheet with
   | Element ("Worksheet", _, content) ->
       let rows = 
         List.filter_map 
           (function
             | Element ("Table", _, rows) -> Some rows
             | _ -> None)
           content 
         |> List.flatten 
       in
       (* Filter out header rows and collect test cases *)
       List.filter_map row_to_test_case rows
   | _ -> [])

(* Main function to load test cases *)
let load_test_cases filename =
  let xml = parse_file filename in
  (match xml with
   | Element ("Workbook", _, worksheets) ->
       (* Process each worksheet *)
       let all_test_cases = List.map process_worksheet worksheets |> List.flatten in
       Printf.printf "Found %d test cases\n" (List.length all_test_cases);
       all_test_cases
   | _ -> [])
