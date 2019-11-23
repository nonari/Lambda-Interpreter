type token =
  | VAR of (string)
  | NUM of (int)
  | LAMBDA
  | DOT
  | L_PAREN
  | R_PAREN
  | EOF
  | IF
  | THEN
  | ELSE
  | TRUE
  | FALSE
  | SUCC
  | PRED
  | ISZERO
  | LET
  | IN
  | EQ

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ast.term
