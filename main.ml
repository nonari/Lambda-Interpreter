open Ast
open Format

let _ =
  while true do
    try
      (* Prompt *)
      output_string stdout "$ ";flush stdout;
      let input_str = input_line stdin in
      if input_str = "" then () else
      begin

        let lexbuf = Lexing.from_string input_str in

(*** Read from file.
        let filename =  Sys.argv.(1) in 
        let chnl = open_in filename in
        let lexbuf = Lexing.from_channel stdin in
***)
          let ast = Parser.main Lexer.token lexbuf in
            let ppf = Lambda.ppf in
            let eval = Lambda.eval ast in

            fprintf std_formatter "= %a@." ppf eval;
      end
    with
      | Lexer.Unexpected_token -> prerr_endline "[Error] Unexpected_token";
      | Parsing.Parse_error -> prerr_endline "[Error] Parsing Error";
      | End_of_file -> print_endline "bye !"; exit(1)
  done