open Format
open Ast
let fpf = fprintf
let debug = true

let rec ppf fmt t = 
  match t with
  | Var s ->
    fpf fmt "%s" s
  | App (e1, e2) ->
    fpf fmt "%a %a" ppf e1 ppf e2
  | Abs (v, e) ->
    fpf fmt "λ%s. %a" v ppf e
  | Paren e ->
    fpf fmt "( %a )" ppf e

let ppdf fmt t =
  match t with
  | Var _ -> fpf fmt   "Var | %a@." ppf  t
  | App _ -> fpf fmt   "App | %a@." ppf  t
  | Abs _ -> fpf fmt   "Abs | %a@." ppf  t
  | Paren _ -> fpf fmt "Par | %a@." ppf  t

let pp (t:expr) =
  if debug then ppdf std_formatter t
  else fpf std_formatter "%a@." ppf  t

let rec eval e0:expr =
  match e0 with
  (* Application *)
  | App(Var _, _) ->
    e0
  | App(Abs(t,e1), e2) ->
(*** Eager evaluation
    let v = eval e2 in
    eval (sub t e1 v)
***)
    fpf std_formatter "> %a" ppdf e0;
    eval (sub t e1 e2)
  | App(App(e1,e2),e3) ->
    eval (App(eval (App(e1,e2)), e3))
  | App(e1,e2) ->
    eval e0
  (* Abstraction *)
  | Abs(t, App(e1,e2))->
    let v2 = eval e2 in
    if Var(t) = v2 then e1 else Abs(t, App(e1,v2))
  | Abs(t1, Abs(t2,e1)) ->
    (Abs(t1, eval(Abs(t2,e1))))
  | Abs(t,e) ->
    Abs(t,e)
  (* Variable *)
  | Var s ->
    e0
  (* Parenthesis *)
  | Paren e1 ->
    e1

and sub (t:var) (e1:expr) (e2:expr) =
  match e1 with
  | Abs(s, e) ->
    let s1 = sub t e e2 in
    if t = s then e1 else Abs(s,s1)
  | App(e3, e4) ->
    let s1 = sub t e3 e2 in
    let s2 = sub t e4 e2 in
    if Var(t) = s2 then e1 else App(s1,s2)
  | Var s ->
    if t = s then begin 
    fpf std_formatter "- [ %s <- %a ]@." t ppf e2;e2 
    end else e1
  | Paren e ->
    e