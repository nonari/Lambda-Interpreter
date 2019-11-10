{
  open Parser
  exception Eof
  exception Unexpected_token
}


let space = [' ''\t''\r''\n']
let digit = ['0'-'9']+
let alpha = ['A'-'Z''a'-'z']
let alphanumeric = ['A'-'Z''a'-'z''0'-'9''_']

rule token =  parse
  | eof                 { EOF }
  | space               { token lexbuf } (* skip *)
  | "L"|"lambda"        { LAMBDA }
  | "("                 { L_PAREN }
  | ")"                 { R_PAREN }
  | "."                 { DOT }
  | alphanumeric* as v  { VAR(v) }
  | _                   {raise Unexpected_token}