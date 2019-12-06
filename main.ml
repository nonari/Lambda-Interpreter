open Ast
open Format

let _ =
  while true do
    try
      output_string stdout "$ ";flush stdout;
      let input_str = input_line stdin in
      if input_str = "" then () else
        let lexbuf = Lexing.from_string input_str in
        let ast = Parser.main Lexer.token lexbuf in
        let eval = Lambda.eval ast in
          Printf.printf "= %s\n" (Lambda.str_of_term eval);
    with
      | Lexer.Unexpected_token -> prerr_endline "[Error] Unexpected_token";
      | Parsing.Parse_error -> prerr_endline "[Error] Parsing Error";
      | Lambda.No_rules_apply -> prerr_endline "[Error] No rules apply";
      | Lambda.Duplicated_labels -> prerr_endline "[Error] Duplicated labels in record";
      | End_of_file -> print_endline "bye !"; exit(0)
  done