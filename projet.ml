(* Exercice 1.1.1 *)

type var = A | B | C | D
type expr =
  | EConst of int          (* 0 ou 1 *)
  | EVar of var            (* variables a, b, c, d *)
type prog =
  | Skip
  | Assign of var * expr
  | Seq of prog * prog
  | If of var * prog * prog
  | While of var * prog

(* Exercice 1.1.2 *)

(*
Grammaire :

Var ::= 'a' | 'b' | 'c' | 'd'
Cst ::= '0' | '1'
Expr ::= Cst | Var
If ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'
While ::= 'w' '(' Var ')' '{' Prog '}'
Seq ::= Instr ';' Instr
Assign ::= Var ":=" Expr
Instr ::= Skip | Assign | Seq | While | If
Prog ::= Instr
*)

(* Exercice 1.1.3 *)

