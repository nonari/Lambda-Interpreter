open Format
open Ast

let print = fprintf
let debug = true

let rec ppf format t =
  match t with
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
  | Var _ -> print format "Var | %a@." ppf t
  | Num _ -> print format "Num | %a@." ppf t
  | App _ -> print format "App | %a@." ppf t
  | Abs _ -> print format "Abs | %a@." ppf t

let pp (t:term) =
  if debug then ppdf std_formatter t
  else print std_formatter "%a@." ppf t

let rec eval e0:term =
  match e0 with
  | App(Var _, _) -> 
    e0
  
  | App(Abs(t,e1), e2) ->
    let v = eval e2 in
    eval (subst t e1 v)
  
  | App(App(e1,e2),e3) ->
      eval (App(eval (App(e1,e2)), e3))
  
  | Abs(t, App(e1,e2))->
    let v2 = eval e2 in
    if Var(t) = v2 then e1 else Abs(t, App(e1,v2))
  
  | Abs(t1, Abs(t2,e1)) -> 
    (Abs(t1, eval(Abs(t2,e1))))
  
  | Abs(t,e) ->
    Abs(t,e)
  
  | Var s ->
    e0
  
  | Num n ->
    e0

  | Pred t ->
    let r = eval t in match r with
      | Num n -> Num(n - 1)
      | _ -> eval (Pred r)

and subst (t:string) (e1:term) (e2:term) =
  match e1 with
  | Abs(s, e) ->
    let s1 = subst t e e2 in
    if t = s then e1 else Abs(s,s1)
  
  | App(e3, e4) ->
    let s1 = subst t e3 e2 in
    let s2 = subst t e4 e2 in
    if Var(t) = s2 then e1 else App(s1,s2)
  
  | Var s ->
    if t = s then begin 
      print std_formatter "- [ %s <- %a ]@." t ppf e2;
      e2 
    end else 
      e1

  | Num n -> Num n