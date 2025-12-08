#use "projet.ml";;


(*3.4.1*)


module type etat = sig
  type t

  val init : t

  val get : t -> var -> int 

  val update : t -> var -> int -> t

end

module Etat : Etat = struct
  type t = var -> int 

  let init = fun _ -> 0

  let get (s:t) (v:var) : int = s v

  let update (s:t) (v: var) (n:int) : t = fun x -> if x = v then n else s x
end

let rec eval_expr_sos (e : wexpr) (s : Etat.t) : int =
  match e with
  | EConst Zero -> 0
  | EConst Un -> 1
  | EVar v -> Etat.get s v
  | EPlus(e1, e2) -> 
      let v1 = eval_expr_sos e1 s in
      let v2 = eval_expr_sos e2 s in
      if v1 = 1 || v2 = 1 then 1 else 0
  | EMult(e1, e2) ->
      let v1 = eval_expr_sos e1 s in
      let v2 = eval_expr_sos e2 s in
      if v1 = 1 && v2 = 1 then 1 else 0
  | ENot e -> 
      if eval_expr_sos e s = 1 then 0 else 1

let exec_assign (v : var) (e : wexpr) (s : Etat.t) : Etat.t =
  let val_e = eval_expr_sos e s in
  Etat.update s v val_e




(* 3.4.2 *)

type config = 
|Inter of winstr * Etat.t 
|Final of Etat.t


let rec faire_un_pas (prog: winstr) (s: Etat.t) : config = 
  match prog with
  |Skip -> Final s
  |Assign (v,e) -> Final (exec_assign v e s)
  |Seq(i1,i2) -> (match faire_un_pas i1 s with
                  |Final s1 -> Inter(i2,s1)
                  |Inter(i1',s1) -> Inter (Seq(i1',i2),s1))
                  
  |If(v,i1,i2) -> let cond = Etat.get s v in 
                    if cond = 1 then Inter(i1,s) else Inter(i2,s)
  
  |While(v,i) -> Inter(If(v,Seq(i,While(v,i)),Skip),s);;


  
(* 3.4.3 *)

let rec executer (prog:winstr) (s: Etat.t) : Etat.t = 
  match faire_un_pas prog s with
  |Final s -> s
  |Inter(i,s) -> executer i s;;



(* Tests*)