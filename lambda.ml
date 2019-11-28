open Format
open Ast
open Str
open Char
open String

exception No_rules_apply

let debug = true

module S_set = Set.Make(String);;

let rec str_of_term t =
  match t with
  | True  -> "True"
  | False -> "False"
  | Var a -> a
  | Num n -> string_of_int n
  | App(t1, t2) -> "(" ^ str_of_term t1 ^ " " ^ str_of_term t2 ^ ")"
  | Abs(p, t) -> "(λ" ^ p ^ "." ^ str_of_term t ^ ")"
  | Pred t -> "pred " ^ str_of_term t
  | Succ t -> "succ " ^ str_of_term t
  | Let(p, t1, t2) -> "let " ^ p ^ str_of_term t1 ^ str_of_term t2
  | If(c, t, e) -> "if" ^ str_of_term c ^ "then" ^ str_of_term t ^ "else" ^ str_of_term e
  | IsZero t -> "iszero" ^ str_of_term t

(* Recursively evaluates a term until it can't be reduced *)
let rec eval t =
  let t' = eval0 t in 
    if t' = t
    then t
    else eval t'

(* Evaluates a term one time *)
and eval0 t0 =
  match t0 with
  | App(Abs(p, t1), t2) when isval t2 ->
      Printf.printf "Subst %s by %s in %s\n" p (str_of_term t2) (str_of_term t1);
      subst p t1 t2
  
  | App(v, t) when isval v ->
      App(v, eval t)
  
  | App(t1, t2) ->
      App(eval t1, t2)   

  | Abs(t, e) -> Abs(t, eval e)

  | Let(p, v, t) -> eval (App(Abs(p, t), v))

  | Succ t ->
    (let r = eval t in match r with
      | Num n -> Num(n + 1)
      | t -> t0
    )

  | Pred t ->
    (let r = eval t in match r with
      | Num n -> Num(n - 1)
      | t -> t0
    )

  | IsZero(t) ->
    (let r = eval t in match r with
      | Num 0 -> True
      | Num _ -> False
      | t -> t0 
    )

  | If(c, t, e) ->
    (let r = eval c in match r with
      | True -> eval t
      | False -> eval e
      | c -> t0
    )
  
  | _ -> t0

(* Check if the term is already a value *)
and isval t0 =
  match t0 with
    |Abs _ -> true
    |Var _ -> true
    |Num _ -> true
    |True -> true
    |False -> true
    |Pred t -> isval t
    |Succ t -> isval t
    |IsZero t -> isval t
    |If(_, t, e) -> (isval t) && (isval e) 
    |_ -> false

(* Recursively substitute p0 by s0 in t0 *)
and subst (p0:string) (t0:term) (s0:term) =
  match t0 with
  | Abs(p2, t2) ->
    if p2 = p0 
    then t0
    else let fv = free s0 in
      if not (S_set.mem p0 fv)
      then Abs(p2, subst p0 t2 s0)
      else
        let new_p = new_name fv p2 in
        let sub_t = subst p2 t2 (Var new_p) in
          Abs(new_p, (subst p2 sub_t s0))

  | App(t2, t3) ->
    let r1 = subst p0 t2 s0 in
    let r2 = subst p0 t3 s0 in
      App(r1,r2)

  | Var s -> if p0 = s then s0 else t0
  
  | Succ t -> Succ (subst p0 t s0)

  | Pred t -> Pred (subst p0 t s0)

  | IsZero t -> IsZero (subst p0 t s0)

  | If(c, t, e) -> If ((subst p0 c s0), (subst p0 t s0), (subst p0 e s0))

  | Let(p, t1, t2) -> subst p0 (eval t0) s0

  | _ -> t0

(* Calculates de the free vars of a term *)
and free t =
  match t with
  | Abs(p, t2) -> S_set.diff (free t2) (S_set.add p S_set.empty)
  | App(t1, t2) -> S_set.union (free t1) (free t2)
  | Var v -> S_set.add v S_set.empty
  | Pred t -> free t
  | Succ t -> free t
  | IsZero t -> free t
  | If(c, t, e) -> S_set.union (free c) (S_set.union (free t) (free e))
  | Let(p, t1, t2) -> free (eval t)
  | Num _ -> S_set.empty
  | True -> S_set.empty
  | False -> S_set.empty

(* Finds a new name for a var *)
and new_name vars var =
  let fresh = next_name var in
    if not (S_set.mem fresh vars) 
    then fresh
    else new_name vars fresh

(* Gets the next alphabetical name for a var *)
and next_name var =
  let len = (String.length var) in
    if String.get var (len - 1) = 'z'
    then var ^ "a"
    else
      let last_code = Char.code (String.get var (len - 1)) in
      let next_string = Char.escaped(Char.chr (1 + last_code)) in
        (String.sub var 0 (len - 1)) ^ next_string