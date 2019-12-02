%{open Ast %}
%token <string> VAR
%token <int> NUM
%token LAMBDA
%token DOT
%token L_PAREN
%token R_PAREN
%token EOF
%token IF
%token THEN
%token ELSE
%token TRUE
%token FALSE
%token SUCC
%token PRED
%token ISZERO
%token LET
%token IN
%token EQ

%start main
%type <Ast.term> main

%left VAR
%left NUM
%left LAMBDA DOT
%left L_PAREN R_PAREN EOF


%%
main:
  term EOF { $1 }


term:
  | appTerm                     { $1 }
  | IF term THEN term ELSE term { If($2,$4,$6) }
  | LET VAR EQ term IN term     { Let($2,$4,$6) }
  | LAMBDA VAR DOT term         { Abs($2,$4) }

appTerm:
  | atomTerm         { $1 }
  | SUCC atomTerm    { Succ($2) }
  | PRED atomTerm    { Pred($2) }
  | ISZERO atomTerm  { IsZero($2) }
  | appTerm atomTerm { App($1,$2) }

atomTerm:
  | L_PAREN term R_PAREN { $2 }
  | TRUE                 { True }
  | FALSE                { False }
  | VAR                  { Var($1) }
  | NUM                  { Num($1) }
