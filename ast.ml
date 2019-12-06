type term = 
    Var of string          (* variable *)
  | App of term * term     (* application *)
  | Abs of string * term   (* abstraction *)
  | Let of string * term * term
  | If of term * term * term
  | LetRec of string * term * term
  | Free of string * term
  | True
  | False
  | Num of int
  | IsZero of term
  | Succ of term
  | Pred of term
  | Rec of (string * term) list
  | ProjByLab of term * string
  | ProjByPos of term * int
  | Unit

