open Format
open Ast
open Str
open Char
open String

exception No_rules_apply

let print = fprintf
let debug = true

module S_set = Set.Make(String);;
(*
let rec ppf format t =
  match t with
  | True -> printf "True"
  | False -> printf "False"
  | Var s ->
    print format "%s" s
  | Num n ->
    print format "%d" n
  | App (e1, e2) ->
    print format "%a %a" ppf e1 ppf e2
  | Abs (v, e) ->
    print format "λ%s. %a" v ppf e

let ppdf format t =
  match t with
  | True  -> printf "True"
  | False -> printf "False"
  | Var _ -> print format "Var | %a@." ppf t
  | Num _ -> print format "Num | %a@." ppf t
  | App _ -> print format "App | %a@." ppf t
  | Abs _ -> print format "Abs | %a@." ppf t
  | Pred _ ->print format "Pred | %a@." ppf t
*)
let rec ppdf t =
  let rec ppdf0 t = match t with
  | True  -> "True"
  | False -> "False"
  | Var a -> " " ^ a 
  | Num n -> " " ^ string_of_int n
  | App(t1, t2) -> "(" ^ ppdf0 t1 ^ ppdf0 t2 ^ ")"
  | Abs(p, t) -> "(λ" ^ p ^ "." ^ ppdf0 t ^ ")"
  | Pred t -> "pred " ^ ppdf0 t 
in printf t

let rec eval t =
  let t' = eval0 t in if t' = t then t else eval t'

and eval0 t0:term =
  match t0 with
  | App(Abs(p, t1), t2) ->
    let v = eval t2 in
    eval (subst p t1 v)
  
  | App(App(e1,e2),e3) ->
      eval (App(eval (App(e1,e2)), e3))
  
  | App(_, _) -> t0

  | Abs(t, e) -> Abs(t, eval e)
  
  | Let(p, v, t) -> eval App((Abs p t), v)

  | Var s -> t0
  
  | Num n -> t0

  | Succ t ->
    (let r = eval t in match r with
      | Num n -> Num(n + 1)
      | t -> t0
      | _ -> eval (Succ r))

  | Pred t ->
    (let r = eval t in match r with
      | Num n -> Num(n - 1)
      | t -> t0
      | _ -> eval (Pred r)
    )

  | True -> True

  | False -> False

  | IsZero(t) ->
    (let r = eval t in match r with
      | Num 0 -> True
      | Num _ -> False
      | t -> t0
      | _ -> eval (IsZero r) 
    )

  | If(c, t, e) ->
    (let r = eval c in match r with
      | True -> eval t
      | False -> eval e
      | c -> t0
      | _ -> eval (If (r, t, e))
    )

and free t = 
  match t with
  | Abs(p, t2) -> S_set.diff (free t2) (S_set.add p S_set.empty)
  | App(t1, t2) -> S_set.union (free t1) (free t2)
  | Var v -> S_set.add v S_set.empty
  | Pred t -> free t
  | Succ t -> free t
  | IsZero t -> free t
  | If(c, t, e) -> S_set.union (free c) (S_set.union (free t) (free e))
  | Num _ -> S_set.empty

and subst (p1:string) (t1:term) (s1:term) =
  match t1 with
  | Abs(p2, t2) ->
    if p2 = p1 
    then t1 
    else let fv = free s1 in 
      if not (S_set.mem p1 fv) 
      then Abs(p1, subst p1 t2 s1)
      else let new_p = new_name fv p2 in
        let sub_t = subst p2 t2 (Var new_p) in
          Abs(new_p, (subst p1 sub_t s1))
  | App(t2, t3) ->
    let r1 = subst p1 t2 s1 in
    let r2 = subst p1 t3 s1 in
      App(r1,r2)

  | Var s -> if p1 = s then s1 else t1

  | Num n -> Num n
  
  and new_name vars var = 
    let fresh = next_name var in 
      if not (S_set.mem fresh vars) then
        fresh
      else   
        new_name vars fresh

  and next_name var = 
    let len = (String.length var) in
      if String.get var (len - 1) = 'z' then
        var ^ "a"
      else 
        String.sub var 0 (len - 1) ^ String.sub var (len - 1) len