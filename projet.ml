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
  Prog ::= Instr InstrSuite | epsilon

*)


(*Exercice 1.1.4*)

(*  Grammaire pour le langage WHILLEb

    Avec la grammaire fournie, nous avons de la récursivité gauche
    - Pour le NT E ->  E ::= E '+' T | T
    - Pour le NT T -> T ::= T '.' F | F

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
and p_Prog l = l |> (p_Instr --> p_InstrSuite) -| epsilon;;


(*Exercice 2.1.2*)


(*Quelques programmes*)
let prog1 = list_of_string("");;
let prog2 = list_of_string("a");;
let prog3 = list_of_string("a:=0");;
let prog4 = list_of_string("e:=0");;
let prog5 = list_of_string("a:=1;b:=1;a:=b");;
let prog5bis = list_of_string("a:=5;b:=2;a:=b");;
let prog6 = list_of_string("a:=0;b:=1;i(b){c:=3}{d:=0}");;
let prog7 = list_of_string("a:=1;b:=0;i(a){i(b){c:=1}{c:=0}}{d:=1}")
let fibo = list_of_string("a:=1;b:=0;c:=0;w(a){d:=b;b:=a;a:=d}")
let prog8 = list_of_string("a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}");;

(*Tests*)

let _ = assert(p_Prog prog1 = []);;
let _ = try let _ = p_Prog prog2 in assert false with Echec -> ();;
let _ = assert(p_Prog prog3 = []);;
let _ = assert(p_Prog prog4 = []);;
let _ = assert(p_Prog prog5 = []);;
let _ = assert(p_Prog prog6 = [';'; 'i'; '('; 'b'; ')'; '{'; 'c'; ':'; '='; '3'; '}'; '{'; 'd'; ':'; '='; '0'; '}']);;
let _ = assert(p_Prog prog7 = []);;
let _ = assert(p_Prog fibo = []);;
let _ = assert(p_Prog prog8 = []);;


(*Exercice 2.1.3*)

let p_V = terminal 'a' -|  terminal 'b' -|  terminal 'c' -| terminal 'd';;
let p_C = terminal '0' -| terminal '1';;
let p_A = p_V -| p_C;;
let rec  p_E l = l |> (p_T --> p_SE)
and p_SE l = l |> ((terminal '+' --> p_T --> p_SE) -| epsilon)
and p_T l = l |> (p_F --> p_ST)
and p_ST l = l |> ((terminal '.' --> p_F --> p_ST) -| epsilon)
and p_F l = l |> ((terminal '!' --> p_F) -| p_A -| (terminal '(' --> p_E --> terminal ')'));;

(*On reprend le langage du WHILEb⁻⁻ pour pouvoir utilser . + et ! dans les expressions*)
let rec p_Instr l = l|> (p_Assign -| p_If -| p_While)
and p_InstrSuite l = l |> ((terminal ';' --> p_Instr --> p_InstrSuite) -| epsilon)
and p_Assign l = l |> (p_V --> terminal ':' --> terminal '=' --> p_E)
and p_If l = l |> (terminal 'i' --> terminal '(' --> p_V --> terminal ')' --> terminal '{' --> p_Prog --> terminal '}' --> terminal '{' --> p_Prog --> terminal '}')
and p_While l = l |> (terminal 'w' --> terminal '(' --> p_V --> terminal ')' --> terminal '{' --> p_Prog --> terminal '}')
and p_Prog l = l |> (p_Instr --> p_InstrSuite) -| epsilon;;

(*On vérifie*)
p_Prog (list_of_string("b:=(a+(b.0))+(!1)"));;
p_Prog (list_of_string("a:=1;b:=(a+(b.0))+(!1);c:=1;w(a){i(c){c:=0;a:=!b+1.a}{b:=0+1+0.(a+b);c:=!!a}}"));;
p_Prog (list_of_string("b:=0+a;!c:=b"));; (*accepte que b:=0+a et renvoie le reste*)


(*Exercice 2.1.4*)

(*Ajouts de la règle pour avoir du blanc dans le code "Espace / Saut de ligne / Tabulation"*)
let p_esp = star (terminal ' ' -| terminal '\t' -| terminal '\n');;
let p_V = p_esp --> (terminal 'a' -|  terminal 'b' -|  terminal 'c' -| terminal 'd') --> p_esp;;
let p_C = p_esp --> (terminal '0' -| terminal '1') --> p_esp;;
let p_A = p_V -| p_C;;


let rec  p_E l = l |> (p_T --> p_SE)
and p_SE l = l |> ((p_esp --> terminal '+' --> p_esp --> p_T --> p_SE) -| epsilon)
and p_T l = l |> (p_F --> p_ST)
and p_ST l = l |> ((p_esp --> terminal '.' --> p_esp --> p_F --> p_ST) -| epsilon)
and p_F l = l |> ((p_esp --> terminal '!' --> p_esp --> p_F) -| p_A -| (p_esp --> terminal '(' --> p_esp --> p_E --> p_esp --> terminal ')' --> p_esp));;


(*On reprend le langage du WHILEb⁻⁻ pour pouvoir utilser . + et ! dans les expressions*)
let rec p_Instr l = l|> (p_Assign -| p_If -| p_While)
and p_InstrSuite l = l |> ((p_esp --> terminal ';' --> p_esp --> p_Instr --> p_InstrSuite) -| epsilon)
and p_Assign l = l |> (p_V --> p_esp --> terminal ':' --> terminal '=' --> p_esp --> p_E)
and p_If l = l |> (p_esp --> terminal 'i' --> p_esp --> terminal '(' --> p_esp --> p_V --> p_esp --> terminal ')' --> p_esp --> terminal '{' --> p_esp --> p_Prog --> p_esp --> terminal '}' --> p_esp --> terminal '{' --> p_esp--> p_Prog --> p_esp --> terminal '}' --> p_esp)
and p_While l = l |> (p_esp --> terminal 'w' --> p_esp --> terminal '(' --> p_esp --> p_V --> p_esp --> terminal ')' --> p_esp --> terminal '{' --> p_esp --> p_Prog --> p_esp --> terminal '}' --> p_esp)
and p_Prog l = l |> (p_esp --> p_Instr --> p_InstrSuite --> p_esp) -| epsilon;;


(*Test : on voit bien les indentations et espaces*)
let _ = assert (p_Prog (list_of_string("
                        a := 1;
                        b := 0;
                        c := a+(b.0)+(!1);
                        w (a) {
                            i (c) {
                              c := 0+b.(0+c)    ;   a := b
                            }{
                              b := 0+1+1;
                              c := !a
                            }
                        }
")) = []);;

(*------------------------------------------*)
(*version avec une grammaire qui rend un AST*)
(*------------------------------------------*)

(*----------
   Whileb⁻⁻
------------*)

let prVar = terminal_res(function
  |'a' -> Some A
  |'b' -> Some B
  |'c' -> Some C
  |'d' -> Some D
  |_ -> None
);;

let prCst = terminal_res(function
  |'0' -> Some 0
  |'1' -> Some 1
  |_ -> None
);;

let prExpr =
  (prCst ++> fun c -> epsilon_res (EConst c))
  +|
  (prVar ++> fun v -> epsilon_res (EVar v))
;;



let rec prProg l = l|> (
  prInstr ++> fun i -> 
    prInstrSuite ++> fun s ->
      epsilon_res(Seq(i,s))
  +|
  (epsilon_res Skip)
)
and prInstr l = l |> (
      prAssign +| prIf +| prWhile
)
and prInstrSuite l = l|> (
  (terminal ';' -+> prInstr ++> fun i -> 
    prInstrSuite ++> fun s ->
      epsilon_res(Seq(i, s))) 
  +| 
  (epsilon_res Skip)
)
and prAssign l = l |> (
  prVar ++> fun v-> 
    terminal ':' --> terminal '=' -+> prExpr ++> fun e -> 
      epsilon_res(Assign(v,e))
)
and prIf l = l|>  (
  terminal 'i' --> terminal '(' -+> prVar ++> fun v ->
    terminal ')' --> terminal '{' -+> prProg ++> fun p1 ->
      terminal '}' --> terminal '{' -+> prProg ++> fun p2 ->
        terminal '}' -+> epsilon_res(If(v,p1,p2))
)
and prWhile l = l|> (
        terminal 'w' --> terminal '(' -+> prVar ++> fun v -> 
          terminal ')' --> terminal '{' -+> prProg ++> fun p ->
            terminal '}' -+> epsilon_res(While(v,p))
);;


(*--------------
      Whileb
----------------*)
let prV = terminal_res(function
  |'a' -> Some A
  |'b' -> Some B
  |'c' -> Some C
  |'d' -> Some D
  |_ -> None
);;

let prC = terminal_res(function
  |'0' -> Some 0
  |'1' -> Some 1
  |_ -> None
);;

let prA =
  (prC ++> fun c -> epsilon_res (EConst c))
  +|
  (prV ++> fun v -> epsilon_res (EVar v))
;;

let rec prF l = l|> (
  (terminal '!' -+> prF) 
  +|
  (prA)
  +| (terminal '(' -+> prE ++> 
    terminal ')' )
)
and prE l = l|> (
  prT ++> fun t ->
    prSE ++> fun s ->
      epsilon_res(Seq(s,t))
)
and prSE l = l |> (
  (terminal '+' -+> prT ++> fun t -> 
    prSE ++> fun s -> 
      epsilon_res(Seq(s,t)))
  +|
  (epsilon_res Skip)
)
and prT l = l |> (
  prF ++> fun f ->
    prST ++> fun s ->
      epsilon_res(Seq(s,t))
)
and prST l = l |> (
    (terminal '.' -+> prF ++> fun f -> 
      prST ++> fun s -> 
        epsilon_res(Seq(s,f)))
  +|
    (epsilon_res Skip)
);;





