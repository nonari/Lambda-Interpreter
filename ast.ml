type var = string
type expr
  = Var of var       (* variable *)
  | App of expr * expr  (* application *)
  | Abs of var * expr   (* abstraction *)
  | Paren of expr       (* parenthesis *)
