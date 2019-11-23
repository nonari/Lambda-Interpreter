type expr = 
    Var of string          (* variable *)
  | App of expr * expr     (* application *)
  | Abs of string * expr   (* abstraction *)
  | Let of string * term * term
  | If of term * term * term
  | True
  | False
  | Num of int
  | IsZero of term
  | Succ of term
  | Pred of term

