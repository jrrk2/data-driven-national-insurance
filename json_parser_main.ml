open Json_parser
open Json_lexer
open Yojson.Basic

let parse_json_file filename =
  print_endline filename;
  let ic = open_in filename in
  let lexbuf = Lexing.from_channel ic in
  let result = Json_parser.main Json_lexer.token lexbuf in
  close_in ic;
  result
