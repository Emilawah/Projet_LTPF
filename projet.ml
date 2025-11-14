(* Exercice 1.1.1 *)

type var = A | B | C | D;;

type expr = 
|EConst of int (*0 ou 1*)
|EVar of var;; (*variables a b c ou d*)

type prog = 
|Skip
|Assign of var * expr
|Seq of prog * prog
|If of var * prog * prog
|While of var * prog;;


(*Exercice 1.1.2 *)


