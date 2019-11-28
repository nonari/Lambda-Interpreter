{
  open Parser
  exception Eof
  exception Unexpected_token

  let keywords =
    [ "let",    LET;
      "in",     IN; 
      "if",     IF;
      "then",   THEN;
      "else",   ELSE;
      "succ",   SUCC;
      "pred",   PRED;
      "iszero", ISZERO;
      "true",   TRUE;
      "false",  FALSE;
    ]
  
  let find word =
    try List.assoc word keywords
    with Not_found -> VAR(word)
  ;;

}


let space = [' ''\t''\r''\n']
let digit = ['0'-'9']+
let alpha = ['A'-'Z''a'-'z']
let alphanumeric = ['A'-'Z''a'-'z''0'-'9''_']


rule token =  parse
  | eof                 { EOF }
  | space               { token lexbuf }
  | "L"|"lambda"        { LAMBDA }
  | "("                 { L_PAREN }
  | ")"                 { R_PAREN }
  | "."                 { DOT }
  | "="		              { EQ }
  | digit as d          { NUM(int_of_string(d)) }
  | alphanumeric* as s  { find s }
  | _                   { raise Unexpected_token }

