type term = 
    Var of string          (* variable *)
  | App of term * term     (* application *)
  | Abs of string * term   (* abstraction *)
  | Let of string * term * term
  | If of term * term * term
  | True
  | False
  | Num of int
  | IsZero of term
  | Succ of term
  | Pred of term

