%{open Ast %}
%token <string> VAR
%token <int> NUM
%token LAMBDA
%token DOT
%token L_PAREN
%token R_PAREN
%token LCURLY
%token RCURLY
%token COMMA
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
%token LETREC
%token IN
%token EQ

%start main
%type <Ast.term> main

%%
main:
  term EOF { $1 }


term:
  | appTerm                     { $1 }
  | LAMBDA VAR DOT term         { Abs($2,$4) }
  | IF term THEN term ELSE term { If($2,$4,$6) }
  | LET VAR EQ term IN term     { Let($2,$4,$6) }
  | LETREC VAR EQ term IN term  { LetRec($2,$4,$6) }
  | VAR EQ term                 { Free($1,$3) }


appTerm:
  | atomTerm         { $1 }
  | atomTerm DOT VAR { ProjByLab($1, $3) }
  | atomTerm DOT NUM { ProjByPos($1, $3) }
  | SUCC atomTerm    { Succ($2) }
  | PRED atomTerm    { Pred($2) }
  | ISZERO atomTerm  { IsZero($2) }
  | appTerm atomTerm { App($1,$2) }

atomTerm:
  | L_PAREN term R_PAREN { $2 }
  | L_PAREN R_PAREN      { Unit }
  | LCURLY fields RCURLY { Rec($2) }
  | TRUE                 { True }
  | FALSE                { False }
  | VAR                  { Var($1) }
  | NUM                  { Num($1) }

fields :
  | nFields { $1 }

nFields :
    field { [$1] }
  | field COMMA nFields { $1 :: $3 }

field :
    VAR EQ term { ($1, $3) }