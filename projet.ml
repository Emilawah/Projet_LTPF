#use "anacomb.ml";;

(* Exercice 1.1.1 *)

type var = A | B | C | D;;

type cst = Zero | Un;;

type expr = 
|EConst of cst (*0 ou 1*)
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
let prog5 = list_of_string("a:=0;b:=1;a:=c");;
let prog5bis = list_of_string("a:=5;b:=2;a:=b");;
let prog6 = list_of_string("a:=0;b:=1;i(b){c:=3}{d:=0}");;
let prog7 = list_of_string("a:=1;b:=0;i(a){i(b){c:=1}{c:=0}}{d:=1}")
let fibo = list_of_string("a:=1;b:=0;c:=0;w(a){d:=b;b:=a;a:=d}")
let prog8 = list_of_string("a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}");;

(*Tests*)

let _ = assert(p_Prog prog1 = []);;
let _ = assert(p_Prog prog2 = ['a']);;
let _ = assert(p_Prog prog3 = []);;
let _ = assert(p_Prog prog4 = ['e';':';'=';'0']);;
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
let test1 = p_Prog (list_of_string("b:=(a+(b.0))+(!1)"));;
let test2 = p_Prog (list_of_string("a:=1;b:=(a+(b.0))+(!1);c:=1;w(a){i(c){c:=0;a:=!b+1.a}{b:=0+1+0.(a+b);c:=!!a}}"));;
let test3 = p_Prog (list_of_string("b:=0+a;!c:=b"));; (*accepte que b:=0+a et renvoie le reste*)


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



(*--------------------------------------------
                   Whileb⁻⁻
----------------------------------------------*)



let prVar = terminal_res(function
  |'a' -> Some A
  |'b' -> Some B
  |'c' -> Some C
  |'d' -> Some D
  |_ -> None
);;

let prCst = terminal_res(function
  |'0' -> Some Zero
  |'1' -> Some Un
  |_ -> None
);;

let prExpr =
  (prCst ++> fun c -> epsilon_res (EConst c))
  +|
  (prVar ++> fun v -> epsilon_res (EVar v));;



let rec prProg l = l|> (
  (prInstr ++> fun i -> 
    prInstrSuite ++> fun s ->
      epsilon_res(Seq(i,s)))
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


(*--------------------------------------------
                   Whileb
----------------------------------------------*)



type wexpr = 
| EConst of cst 
| EVar of var
(* ajout de + - et ! *)
| EPlus of wexpr * wexpr
| EMult of wexpr * wexpr
| ENot of wexpr;;

type winstr = 
| Skip
| Assign of var * wexpr
| Seq of winstr * winstr
| If of var * winstr * winstr
| While of var * winstr;;

let prV = terminal_res(function
  |'a' -> Some A
  |'b' -> Some B
  |'c' -> Some C
  |'d' -> Some D
  |_ -> None
);;

let prC = terminal_res(function
  |'0' -> Some Zero
  |'1' -> Some Un
  |_ -> None
);;

let prA =
  (prC ++> fun c -> epsilon_res (EConst c))
  +|
  (prV ++> fun v -> epsilon_res (EVar v))
;;

let rec prF l = l|> (
  (terminal '!' -+> prF ++> fun f ->
    epsilon_res(ENot f)) 
  +|
  (prA)
  +| (terminal '(' -+> prE ++> fun e ->
    terminal ')' -+> epsilon_res e)
)
and prE l = l|> (
  prT ++> fun t ->
    prSE t
)
and prSE e l = l |> (
  (terminal '+' -+> prT ++> fun t -> 
    prSE (EPlus(e,t)))
  +|
  (epsilon_res e)
)
and prT l = l |> (
  prF ++> fun f ->
    prST f
)
and prST e l = l |> (
    (terminal '.' -+> prF ++> fun f -> 
      prST (EMult(e,f)))
  +|
    (epsilon_res e)
);;


let rec pr_Prog l = l|> (
  (pr_Instr ++> fun i -> 
    pr_InstrSuite ++> fun s ->
      epsilon_res(Seq(i,s)))
  +|
  (epsilon_res Skip)
)
and pr_Instr l = l |> (
      pr_Assign +| pr_If +| pr_While
)
and pr_InstrSuite l = l|> (
  (terminal ';' -+> pr_Instr ++> fun i -> 
    pr_InstrSuite ++> fun s ->
      epsilon_res(Seq(i, s))) 
  +| 
  (epsilon_res Skip)
)
and pr_Assign l = l |> (
  prV ++> fun v-> 
    terminal ':' --> terminal '=' -+> prE ++> fun e -> 
      epsilon_res(Assign(v,e))
)
and pr_If l = l|>  (
  terminal 'i' --> terminal '(' -+> prV ++> fun v ->
    terminal ')' --> terminal '{' -+> pr_Prog ++> fun p1 ->
      terminal '}' --> terminal '{' -+> pr_Prog ++> fun p2 ->
        terminal '}' -+> epsilon_res(If(v,p1,p2))
)
and pr_While l = l|> (
        terminal 'w' --> terminal '(' -+> prV ++> fun v -> 
          terminal ')' --> terminal '{' -+> pr_Prog ++> fun p ->
            terminal '}' -+> epsilon_res(While(v,p))
);;



let _ = assert(prE (list_of_string("(a+b).(!c)")) = (EMult(EPlus(EVar A, EVar B), ENot(EVar C)),[]));;
let prog_test = "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}" ;;
let _ = assert(pr_Prog (list_of_string prog_test) = (Seq (Assign (A, EConst Un),
    Seq (Assign (B, EConst Un),
     Seq (Assign (C, EConst Un),
      Seq
       (While (A,
         Seq
          (If (C,
            Seq (Assign (C, EConst Zero), Seq (Assign (A, EVar B), Skip)),
            Seq (Assign (B, EConst Zero), Seq (Assign (C, EVar A), Skip))),
          Skip)),
       Skip)))),
   []));;


(* Partie 2.1.4 version ranalist *)

let p_esp = star (terminal ' ' -| terminal '\t' -| terminal '\n');;
let pr_V = p_esp -+> (terminal_res(function
  |'a' -> Some A
  |'b' -> Some B
  |'c' -> Some C
  |'d' -> Some D
  |_ -> None
)) ++> fun v -> p_esp -+> epsilon_res v;;

let pr_C = p_esp -+> (terminal_res(function
  |'0' -> Some Zero
  |'1' -> Some Un
  |_ -> None
)) ++> fun c -> p_esp -+> epsilon_res c;;

let pr_A =
  (pr_C ++> fun c -> epsilon_res (EConst c))
  +|
  (pr_V ++> fun v -> epsilon_res (EVar v))
;;


let rec pr_F l = l|> (
  (terminal '!' --> p_esp -+> pr_F ++> fun f ->
    epsilon_res(ENot f)) 
  +|
  (pr_A)
  +| (p_esp --> terminal '(' --> p_esp -+> pr_E ++> fun e ->
    p_esp --> terminal ')' --> p_esp -+> epsilon_res e)
)
and pr_E l = l|> (
  pr_T ++> fun t ->
    pr_SE t
)
and pr_SE e l = l |> (
  (p_esp --> terminal '+' --> p_esp -+> pr_T ++> fun t -> 
    pr_SE (EPlus(e,t)))
  +|
  (epsilon_res e)
)
and pr_T l = l |> (
  pr_F ++> fun f ->
    pr_ST f
)
and pr_ST e l = l |> (
    (p_esp --> terminal '.' --> p_esp -+> pr_F ++> fun f -> 
      pr_ST (EMult(e,f)))
  +|
    (epsilon_res e)
);;

let rec pr2_Prog l = l|> (
  p_esp -+> (
    (pr2_Instr ++> fun i -> 
      pr2_InstrSuite ++> fun s ->
        epsilon_res(Seq(i,s))) 
  +|
  (epsilon_res Skip)
  ) 
)
and pr2_Instr l = l |> (
      pr2_Assign +| pr2_If +| pr2_While
)
and pr2_InstrSuite l = l|> (
  (p_esp --> terminal ';' --> p_esp -+> pr2_Instr ++> fun i -> 
    pr2_InstrSuite ++> fun s ->
      epsilon_res(Seq(i, s)))
  +| 
  (epsilon_res Skip)
)
and pr2_Assign l = l |> (
  pr_V ++> fun v-> 
    terminal ':' --> terminal '=' --> p_esp -+> pr_E ++> fun e -> 
      epsilon_res(Assign(v,e))
)
and pr2_If l = l|>  (
  p_esp --> terminal 'i' --> p_esp --> terminal '(' --> p_esp -+> pr_V ++> fun v ->
    p_esp --> terminal ')' --> p_esp --> terminal '{' --> p_esp -+> pr2_Prog ++> fun p1 ->
      p_esp --> terminal '}' --> p_esp --> terminal '{' --> p_esp -+> pr2_Prog ++> fun p2 ->
        p_esp --> terminal '}' -+> epsilon_res(If(v,p1,p2))
)
and pr2_While l = l|> (
        p_esp --> terminal 'w' --> p_esp --> terminal '(' --> p_esp -+> pr_V ++> fun v -> 
          p_esp --> terminal ')' --> p_esp --> terminal '{' --> p_esp -+> pr2_Prog ++> fun p ->
            p_esp --> terminal '}' --> p_esp -+> epsilon_res(While(v,p))
);;


let _ = assert (pr2_Prog (list_of_string("
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
")) = (Seq (Assign (A, EConst Un),
  Seq (Assign (B, EConst Zero),
   Seq
    (Assign (C,
      EPlus (EPlus (EVar A, EMult (EVar B, EConst Zero)), ENot (EConst Un))),
    Seq
     (While (A,
       Seq
        (If (C,
          Seq
           (Assign (C,
             EPlus (EConst Zero, EMult (EVar B, EPlus (EConst Zero, EVar C)))),
           Seq (Assign (A, EVar B), Skip)),
          Seq (Assign (B, EPlus (EPlus (EConst Zero, EConst Un), EConst Un)),
           Seq (Assign (C, ENot (EVar A)), Skip))),
        Skip)),
     Skip)))),
 []));;



(*Exercice 2.2.1 (avec les bons types de WHILEb⁻⁻)*)


type state = var -> int;;

let init_state (v:var) = 0;;

let get (s:state) (v:var) : int = s v;;

let update (s:state) (v:var) (n:int) : state = 
  fun x -> if x = v then n else s x;;


let evalE (e:expr) (s:state) : int = 
  match e with
  |EConst Zero -> 0 
  |EConst Un -> 1
  |EVar v -> get s v 

  
let rec evalI (i:instr) (s:state) : state = 
  match i with
  |Skip -> s
  |Assign(v, e) -> update s v (evalE e s)
  |Seq(i1, i2) -> evalI i2 (evalI i1 s)
  |If(v ,i1, i2) -> if get s v = 1 then evalI i1 s else evalI i2 s
  |While (v, i) -> if get s v = 1 
                   then evalI (While(v, i)) (evalI i s) else s;;   
  
  
(*Tests*)

let run_test progs : state = 
    match prProg progs with
    |(i,[]) -> evalI i init_state;
    |(_,reste) -> failwith ("Echec parsing.");;


let print_res name_test s a' b' c' d' =
  Printf.printf "\n--- %s ---\n" name_test;
  Printf.printf "A = %d (Attendu: %d) -> %s\n" (get s A) a' (if get s A = a' then "OK" else "FAIL");
  Printf.printf "B = %d (Attendu: %d) -> %s\n" (get s B) b' (if get s B = b' then "OK" else "FAIL");
  Printf.printf "C = %d (Attendu: %d) -> %s\n" (get s C) c' (if get s C = c' then "OK" else "FAIL");
  Printf.printf "D = %d (Attendu: %d) -> %s\n" (get s D) d' (if get s D = d' then "OK" else "FAIL");;





(* 
- prog3 = list_of_string("a:=0");;
- prog5 = list_of_string("a:=1;b:=1;a:=b");;
- prog7 = list_of_string("a:=1;b:=0;i(a){i(b){c:=1}{c:=0}}{d:=1}")
- prog7 = list_of_string("a:=1;b:=0;i(a){i(b){c:=1}{c:=0}}{d:=1}")
- prog8 = list_of_string("a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}");;
- fibo = list_of_string("a:=1;b:=0;c:=0;w(a){d:=b;b:=a;a:=d}") 
*)


let test_prog3 =
  let code = prog3 in
  let s = run_test code in 
  print_res "Test prog3 : " s 0 0 0 0;;


let test_prog5 =
  let code = prog5 in
  let s = run_test code in
  print_res "Test prog5 : " s 0 1 0 0;;
  

let test_prog7 =
  let code = prog7 in 
  let s = run_test code in
  print_res "Test prog7 : " s 1 0 0 0;;


let test_prog8 =
  let code = prog8 in
  let s = run_test code in
  print_res "Test prog8 : " s 0 0 0 0;;


let test_prog_fibo =
  let code = fibo in
  let s = run_test code in
  print_res "Test fibo" s 0 1 0 0;; 

(*Exemple trace exec avec fibo :
s1 : A = 1 ; B = 0 ; C = 0 ; D = 0 (a:=1;b:=0;c:=0)
s2 : A = 0 ; B = 1 ; C = 0 ; D = 0  (corps boucle : d:=b;b:=a;a:=d)
s3 : A = 0 ; B = 1 ; C = 0 ; D = 0  (sorti de boucle (a=0) -> fin)
*)




(* Exercice 2.2.2 *)

let rec evalWexpr (e: wexpr) (s:state) : int =
  match e with
  |EConst Zero -> 0
  |EConst Un -> 1
  |EVar v -> get s v
  |EPlus(e1,e2) -> 
    let s1 = evalWexpr e1 s in
    let s2 = evalWexpr e2 s in
    if s1 = 1 || s2 = 1 then 1 else 0
  |EMult(e1,e2) -> 
    let s1 = evalWexpr e1 s in
    let s2 = evalWexpr e2 s in
    if s1 = 1 && s2 = 1 then 1 else 0
  |ENot e -> if evalWexpr e s = 1 then 0 else 1;;


let rec evalWInstr (i:winstr) (s:state) : state = 
  match i with
  |Skip -> s
  |Assign(v,e) -> update s v (evalWexpr e s)
  |Seq(i1,i2) -> evalWInstr i2 (evalWInstr i1 s)
  |If(v,i1,i2) -> if get s v = 1 then evalWInstr i1 s else evalWInstr i2 s
  |While(v,i) -> if get s v = 1 then evalWInstr (While(v,i)) (evalWInstr i s) else s;;
                

(*Tests*)

let run_test_WHILEb progs : state = 
    match pr2_Prog progs with
    |(i,[]) -> evalWInstr i init_state
    |(_,reste) -> failwith ("Echec parsing.");;

    
let _prog1 = list_of_string("
    a := 1
    ");;

(*test des nouvelles expr*)
let _prog2 = list_of_string("
          a := (1+0) ;
          b := 1.0 ;
          c := !b
    ");;
    
(* c'est le XOR ;) *)
let _prog3 = list_of_string("
    a := 1;
    b := 1;
    c := (a + b). !(a . b)
");;    

let _prog4 = (list_of_string("
                        a := 1;
                        b := 0;
                        c := a+(b.0)+(!1);
                        w (a) {
                            i (c) {
                              c := 0+b.( 0 + c )    ;   a := b
                            }{
                              b := 0+1+1;
                              d := !a
                            }
                        }
"));;

(*erreur parsing à cause de '=' ; la valeur 7 et mauvais parenthésages*)
let _prog5 = (list_of_string("
                        a := 1;
                        b := 0;
                        c := a+(b.0)+(!1));
                        w (a) {
                            a = 1;
                            b = 7
                        }
"));;

let _prog6 = list_of_string("");;

let test1 = let s = run_test_WHILEb _prog1 in
  print_res "Test _prog1 : " s 1 0 0 0;;    

let test2 = let s = run_test_WHILEb _prog2 in
  print_res "Test _prog2 : " s 1 0 1 0;;

let test3 = let s = run_test_WHILEb _prog3 in
  print_res "Test _prog3 : " s 1 1 0 0;;

let test4 = let s = run_test_WHILEb _prog4 in
  print_res "Test _prog4 : " s 0 0 0 0;;

(*Trace exec exemple pour _prog4 

  - s1 : A = 1 ; B = 0 ; C = 1 ; D = 0   [ a:=1 b:=0 c := a+(b.0)+(!1) ]


   - Entrée dans While(a) car A=1.
   - Entrée dans If(c) car C=1 (Branche THEN).

  - s2 : A = 1 ; B = 0 ; C = 0 ; D = 0   [ c := 0+b.( 0 + c ) ]

  - s3 : A = 0 ; B = 0 ; C = 0 ; D = 0   [ a := b ]
        
   - Sortie du While(a) car A=0.

   - s4 : A = 0 ; B = 0 ; C = 0 ; D = 0
*)

let test5 = let s = run_test_WHILEb _prog5 in
  print_res "Test _prog5 : " s 0 0 0 0;;

let test6 = let s = run_test_WHILEb _prog6 in
  print_res "Test _prog6 : " s 1 1 1 1;; (* fail car les états sont init a 0 au départ *)