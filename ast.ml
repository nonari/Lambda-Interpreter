type expr = 
    Var of string          (* variable *)
  | App of expr * expr     (* application *)
  | Abs of string * expr   (* abstraction *)