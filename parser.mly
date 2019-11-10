%{open Ast %}
%token <string> VAR
%token LAMBDA DOT L_PAREN R_PAREN EOF
%start main
%type <Ast.expr>  main

%left VAR
%left LAMBDA DOT
%left L_PAREN R_PAREN EOF


%%
main:
  expr EOF { $1 }


expr:
  | expr2   { $1 }
  | t var   { App($1,$2) }
  | t expr2 { App($1,$2) }


expr2:
  | LAMBDA VAR DOT t     { Abs($2,$4) }
  | L_PAREN expr R_PAREN { $2 }
  

t:
  | var  { $1 }
  | expr { $1 }


var:
  | VAR { Var($1) }
