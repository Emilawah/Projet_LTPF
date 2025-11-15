#use "analist.ml";;
#use "anacomb.ml";;

(* Exercice 1.1.1 *)

type var = A | B | C | D;;

type expr = 
|EConst of int (*0 ou 1*)
|EVar of var;; (*variables a b c ou d*)

type instr = 
|Skip
|Assign of var * expr
|Seq of instr * instr
|If of var * instr * instr
|While of var * instr;;


(*Exercice 1.1.2 *)
 
(*
  Grammaire pour le langage WHILEb⁻⁻:

  Var ::= 'a' | 'b' | 'c' | 'd'
  Cst ::= 0 | 1
  Expr ::= Cst | Var
  Instr ::= Assign | Seq | If | While | epsilon
  Assign ::= Var ':' '=' Expr
  Seq ::= Instr ';' Instr 
  If ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'
  While ::= 'w' '(' Var ')' '{' Prog '}'
  Prog ::= Prog ';' Instr | Instr


  (*==> Notre grammaire est récursive a gauche : 
        Un programme (qui est une suite d'instruction) peut se répeter 
        à l'infini). 
    *)

*)

(*Exercice 1.1.3 *)

(* Nouvelle grammaire :

  Var ::= 'a' | 'b' | 'c' | 'd'
  Cst ::= 0 | 1
  Expr ::= Cst | Var
  Instr ::=  Assign | If | While
  InstrSuite ::= ';' Instr InstrSuite | epsilon
  Assign ::= Var ':' '=' Expr
  If ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'
  While ::= 'w' '(' Var ')' '{' Prog '}'
  Prog ::= Instr InstrSuite 

*)


(*Exercice 1.1.4*)

(*  Grammaire pour le langage WHILLEb

    Avec la grammaire fournie, nous avons de la récursivité gauche
    - Pour le NT E ->  E ::= E '+' T | T
    - Pour le NT T -> T ::= T '.' F | F
    - Pour le NT F -> F ::= '!' F | A | '(' E ')'

    Une grammaire non récursive à gauche peut s'écrire :

    C ::= '0' | '1'
    V ::= 'a' |'b' | 'c' | 'd'
    A ::= C | V
    F ::= '!' F | A | '(' E ')'

    E ::= T SE
    SE ::= '+' T SE | epsilon

    T ::= F ST
    ST ::= '.' F ST | epsilon 
*)

(* PARTIE PRINCIPALE*)

(*Exercice 2.1.1*)

(*On va utiliser les fichiers analist.ml et anacomb.ml 
  vu en cours pour pouvoir écrire notre analyseur syntaxique
*)

let p_Var = terminal 'a' -|  terminal 'b' -|  terminal 'c' -| terminal 'd';; 
let p_Cst = terminal '0' -| terminal '1';;
let p_Expr = p_Cst -| p_Var;;

(*On utilise la récurrence mutuelle car certains 
  NT ont besoin des autres NT de la grammaire*)
let rec p_Instr l = l|> (p_Assign -| p_If -| p_While)
and p_InstrSuite l = l |> ((terminal ';' --> p_Instr --> p_InstrSuite) -| epsilon)
and p_Assign l = l |> (p_Var --> terminal ':' --> terminal '=' --> p_Expr)
and p_If l = l |> (terminal 'i' --> terminal '(' --> p_Var --> terminal ')' --> terminal '{' --> p_Prog --> terminal '}' --> terminal '{' --> p_Prog --> terminal '}')
and p_While l = l |> (terminal 'w' --> terminal '(' --> p_Var --> terminal ')' --> terminal '{' --> p_Prog --> terminal '}')
and p_Prog l = l |> (p_Instr --> p_InstrSuite);;



