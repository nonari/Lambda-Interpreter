open Format
open Ast
open Str
open Char
open String
open Hashtbl

exception No_rules_apply
exception Duplicated_labels

module S_set = Set.Make(String);;

let free_context = Hashtbl.create 1024;;

(* FIX combinator definition *)
let fix = 
  let x = Var "x" in 
  let y = Var "y" in
  let f = Var "f" in
  let part = Abs("x", App(f, Abs("y", App(App(x, x) ,y)))) in
    Abs("f", App(part, part))

(* Checks if a var name is in the context *)
let is_in_ctx var =
  let r = Hashtbl.find_opt free_context var in
  match r with
  | Some _ -> true
  | None -> false

(* λ *)
(* Returns the string representation of a term `t` *)
let rec str_of_term t =
  match t with
  | Unit -> "()"
  | True  -> "True"
  | False -> "False"
  | Rec(r) -> "{" ^ str_of_record r 
  | ProjByLab(r, l) -> str_of_term r ^ "." ^ l
  | ProjByPos(r, pos) -> str_of_term r ^ "." ^ string_of_int pos 
  | Var a -> a
  | Num n -> string_of_int n
  | Free(v, t) -> v ^ " = " ^ str_of_term t
  | App(t1, t2) -> "(" ^ str_of_term t1 ^ " " ^ str_of_term t2 ^ ")"
  | Abs(p, t) -> "(L " ^ p ^ "." ^ str_of_term t ^ ")"
  | Pred t -> "pred " ^ str_of_term t
  | Succ t -> "succ " ^ str_of_term t
  | Let(p, t1, t2) -> "let " ^ p ^ " = " ^ str_of_term t1 ^ " in " ^ str_of_term t2
  | LetRec(p, t1, t2) -> "letrec " ^ p ^ " = " ^ str_of_term t1 ^ " in " ^ str_of_term t2
  | If(c, t, e) -> "if " ^ str_of_term c ^ " then " ^ str_of_term t ^ " else " ^ str_of_term e
  | IsZero t -> "iszero " ^ str_of_term t

and str_of_record r =
  match r with
  | [] -> "}"
  | (l,f)::[] -> l ^ "=" ^ str_of_term f ^ "}"
  | (l, f)::t -> l ^ "=" ^ str_of_term f ^ ";" ^ str_of_record t


(* Recursively evaluates a term `t` until it can't be reduced *)
let rec eval t =
  let t' = eval0 t in
    if t' = t
    then t
    else eval t'

(* Rules to evaluate a term `t` *)
and eval0 t0 =
  match t0 with
  | App(Abs(p, t1), t2) when isval t2 ->
      Printf.printf "[%s -> %s]%s\n" p (str_of_term t2) (str_of_term t1);
      eval (subst p t1 t2)

  | App(t1, t2) when isval t1 -> App(t1, eval t2)
  
  | App(t1, t2) -> App(eval t1, t2)

  | Let(name, def, use) -> App((Abs(name, use)), def)

  (*
     Convert letrec into a simple let in substuting the
     recursive call to `name` in `def` by an auxiliar function
     and substituting `name` by fix applied to `name` in `use` 
  *)
  | LetRec(name, def, use) -> 
      let aux_param = new_name (free def) "f" in
      let sub_def = subst name def (Var aux_param) in
      let aux_def = Abs(aux_param, sub_def) in
      let sub_use = subst name use (App(fix, Var name)) in
        Let(name, aux_def, sub_use)

  | Free(name, t) ->
    Hashtbl.add free_context name t;
    Unit

  | Var name -> 
    if is_in_ctx name 
    then Hashtbl.find free_context name
    else t0

  | Succ t ->
    (let r = eval t in match r with
      | Num n -> Num(n + 1)
      | t -> raise No_rules_apply
    )

  | Pred t ->
    (let r = eval t in match r with
      | Num 0 -> Num 0
      | Num n -> Num(n - 1)
      | t -> raise No_rules_apply
    )

  | IsZero(t) ->
    (let r = eval t in match r with
      | Num 0 -> True
      | Num _ -> False
      | t -> raise No_rules_apply
    )

  | If(c, t, e) ->
    (let r = eval c in match r with
      | True -> eval t
      | False -> eval e
      | c -> raise No_rules_apply
    )
  
  | Rec(r) -> 
    if different_labels r S_set.empty
    then t0
    else raise Duplicated_labels

  | ProjByLab(t, label) ->
    (match t with
    | Rec(l) -> proj_by_label l label 
    | _ -> raise No_rules_apply)

  | ProjByPos(t, pos) ->
    (match t with
    | Rec(l) -> proj_by_pos l pos 
    | _ -> raise No_rules_apply)

  | _ -> t0

(* Check if all labels in a record are different *)
and different_labels r accum =
  match r with 
  | [] -> true
  | (labl, f)::t -> 
    if S_set.mem labl accum 
    then false
    else different_labels t (S_set.add labl accum) 

(* Check if the term `t` is already a value *)
and isval t =
  match t with
    | Abs _ -> true 
    | Var v -> not (is_in_ctx v)  
    | Num _ -> true
    | True -> true
    | False -> true
    | Unit -> true
    | Rec r -> true
    | _ -> false

(* Recursively substitute `p0` by `s0` in `t0` *)
and subst (p0:string) (t0:term) (s0:term) =
  match t0 with
  | Abs(p2, t2) ->
    if p2 = p0 
    then t0
    else let fv = free s0 in
      if not (S_set.mem p2 fv) 
      then Abs(p2, subst p0 t2 s0)
      else
        let new_p = new_name (S_set.union (free t2) fv) p2 in
        let sub_t = subst p2 t2 (Var new_p) in
          Abs(new_p, (subst p0 sub_t s0))

  | App(t2, t3) ->
    let r1 = subst p0 t2 s0 in
    let r2 = subst p0 t3 s0 in
      App(r1,r2)

  | Var s -> if p0 = s then s0 else t0
  
  | Succ t -> Succ (subst p0 t s0)

  | Pred t -> Pred (subst p0 t s0)

  | IsZero t -> IsZero (subst p0 t s0)

  | If(c, t, e) -> If ((subst p0 c s0), (subst p0 t s0), (subst p0 e s0))

  | Let(p, t1, t2) -> subst p0 (eval0 t0) s0

  | LetRec(p, t1, t2) -> subst p0 (eval0 t0) s0

  | Rec(r) -> Rec(subst_record r p0 s0) 
  
  | ProjByLab(r, k) -> ProjByLab(subst p0 r s0, k)
  
  | ProjByPos(r, pos) -> ProjByPos(subst p0 r s0, pos)

  | _ -> t0

and subst_record r p0 s0 =
  match r with
  | [] -> []
  | (l,c)::t -> (l, subst p0 c s0) :: subst_record t p0 s0

(* Calculates the free vars of a term `t` *)
and free t =
  match t with
  | Abs(p, t2) -> S_set.diff (free t2) (S_set.add p S_set.empty)
  | App(t1, t2) -> S_set.union (free t1) (free t2)
  | Var v -> S_set.add v S_set.empty
  | Pred t -> free t
  | Succ t -> free t
  | IsZero t -> free t
  | If(c, t, e) -> S_set.union (free c) (S_set.union (free t) (free e))
  | Let(p, t1, t2) -> free (eval0 t)
  | LetRec(p, t1, t2) -> free (eval0 t)
  | Rec((_,v)::t) -> S_set.union (free v) (free (Rec(t))) 
  | ProjByLab(r, k) -> S_set.union (S_set.add k S_set.empty) (free r)
  | ProjByPos(r, pos) -> free r
  | _ -> S_set.empty

(* Select the field with the projecion label *)
and proj_by_label fields label =
  match fields with
  | [] -> raise No_rules_apply
  | (l,c)::t when l = label -> c 
  | (_,c)::t -> proj_by_label t label

(* Select the number projected field *)
and proj_by_pos fields pos =
  match fields with
  | [] -> raise No_rules_apply
  | (l,c)::t when pos = 0 -> c 
  | (_,c)::t -> proj_by_pos t (pos - 1)

(* Finds a new name for a `var` thats not inside `vars` *)
and new_name vars var =
  let fresh = next_name var in
    if not (S_set.mem fresh vars)
    then fresh
    else new_name vars fresh

(* Gets the next alphabetical name for a `var` *)
and next_name var =
  let len = (String.length var) in
    if String.get var (len - 1) = 'z'
    then var ^ "a"
    else
      let last_code = Char.code (String.get var (len - 1)) in
      let next_string = Char.escaped(Char.chr (1 + last_code)) in
        (String.sub var 0 (len - 1)) ^ next_string

and next_name2 var =
  var ^ "'"
