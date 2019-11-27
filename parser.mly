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
%type <Ast.term>  main

%left VAR 
%left NUM
%left LAMBDA DOT
%left L_PAREN R_PAREN EOF
%right LET 
%right IF 
%right SUCC 
%right PRED 
%right ISZERO
%nonassoc EQ 
%nonassoc IN 
%nonassoc THEN 
%nonassoc ELSE


%%
main:
  term EOF { $1 }


term:
  | appTerm                     { $1 }
  | atomTerm                    { $1 }
  | term term                   { App($1,$2) }
  | IF term THEN term ELSE term { If($2,$4,$6) }
  | LET VAR EQ term IN term     { Let($2,$4,$6) }


appTerm:
  | SUCC term   { Succ($2) }
  | PRED term   { Pred($2) }
  | ISZERO term { IsZero($2) }


atomTerm:
  | LAMBDA VAR DOT term  { Abs($2,$4) }
  | L_PAREN term R_PAREN { $2 }
  | TRUE                 { True }
  | FALSE                { False }
  | VAR                  { Var($1) }
  | NUM                  { Num($1) }
