#use "anacomb.ml";;
(* Exercice 1.1.1 *)

type var = A | B | C | D
type expr =
  | EConst of cst          (* 0 ou 1 *)
  | EVar of var            (* variables a, b, c, d *)
type instr =
  | Skip
  | Assign of var * expr
  | Seq of instr * instr
  | If of var * instr * instr
  | While of var * instr

(* Exercice 1.1.2 *)

(* Grammaire : *)

(*
  Grammaire :

  Var ::= 'a' | 'b' | 'c' | 'd'
  Cst ::= 0 | 1
  Expr ::= Cst | Var
  Instr ::= Assign | Seq | If | While
  Assign ::= Var ':' '=' Expr
  Seq ::= Instr ';' Instr 
  If ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'
  While ::= 'w' '(' Var ')' '{' Prog '}'
  Prog ::= Instr
*)

(* Exercice 1.1.3 *)

(* On enlève la récursion gauche *)

(*
  Grammaire sans récursion gauche :

  Var ::= 'a' | 'b' | 'c' | 'd'
  Cst ::= 0 | 1
  Expr ::= Cst | Var
  Instr ::= Assign | If | While
  InstrSuite ::= ';' Instr InstrSuite | ε
  Assign ::= Var ':' '=' Expr
  If ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'
  While ::= 'w' '(' Var ')' '{' Prog '}'
  Prog ::= Instr InstrSuite | ε
*)  

(* Exercice 1.2.1 *)

(*
Notations :
   - s          : état initial
   - s'         : état final
   - [[expr]]_s : valeur de l'expression 'expr' évaluée dans l'état 's'
   - s -P-> s'  : l'exécution du programme P à partir de s conduit à s'

Règle 1 : Cas VRAI (Condition = 1)

        [[expr]]_s = 1       s --P--> s'
      ---------------------------------------
        s --if expr then P else Q--> s'


Règle 2 : Cas FAUX (Condition = 0).

        [[expr]]_s = 0       s --Q--> s'
      ---------------------------------------
        s --if expr then P else Q--> s'
*)

(* 2 Partie principale *)

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
  (prVar ++> fun v -> epsilon_res (EVar v))
;;

let rec prProg l = (
  (prInstr ++> fun i ->
    prInstrSuite ++> fun s ->
      epsilon_res(Seq(i,s)))
+|
  (epsilon_res Skip)
) l

and prInstr l = (
  prAssign 
  +|
  prIf
  +|
  prWhile
) l

and prInstrSuite l = (
  (terminal ';' -+> prInstr ++> fun i ->
    prInstrSuite ++> fun s ->
      epsilon_res(Seq(i, s)))
  +|
  (epsilon_res Skip)
) l

and prAssign l = (
  prVar ++> fun v ->
    terminal ':' --> terminal '=' -+> prExpr ++> fun e ->
      epsilon_res(Assign(v,e))
) l

and prIf l = (
  terminal 'i' --> terminal '(' -+> prVar ++> fun v ->
    terminal ')' --> terminal '{' -+> prProg ++> fun p1 ->
      terminal '}' --> terminal '{' -+> prProg ++> fun p2 ->
        terminal '}' -+> epsilon_res (If(v, p1, p2))
) l

and prWhile l = (
  terminal 'w' --> terminal '(' -+> prVar ++> fun v ->
    terminal ')' --> terminal '{' -+> prProg ++> fun p ->
      terminal '}' -+> epsilon_res (While(v, p))
) l;;

(* Exercice 2.1.2*)

let prog_assign = "a:=1"
let prog_seq    = "a:=1;b:=0"
let prog_cond   = "i(a){b:=1}{c:=0}"
let prog_loop   = "w(a){a:=0}"
let prog_empty_else = "i(a){b:=1}{}" 
let prog_nested = "w(a){b:=1;c:=0}"
let prog_sujet = "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}"

let run_tests () =
  let tests = [
    ("Affectation", prog_assign);
    ("Séquence",    prog_seq);
    ("Condition",   prog_cond);
    ("Boucle",      prog_loop);
    ("Else vide",   prog_empty_else);
    ("Imbrication", prog_nested);
    ("Sujet",       prog_sujet);
  ] in
  
  let executer (nom, code) =
    Printf.printf "[%-15s] : " nom;
    try
      let input = list_of_string code in
      match prProg input with
      | (ast, []) -> print_endline "OK"
      | _ -> print_endline "ECHEC (Incomplet)"
    with Echec -> print_endline "ECHEC (Syntaxe)"
  in
  
  print_endline "\n--- Lancement des tests ---";
  List.iter executer tests;
  print_endline "--- Terminé ---\n"
;;

run_tests ();;

(* Exercice 1.1.4 *)

(*
C ::= '0' | '1'
V ::= 'a' |'b' | 'c' | 'd'
A ::= C | V
F ::= '!' F | A | '(' E ')'
E ::= T SE
SE ::= '+' T SE | ε
T ::= F ST
ST ::= '.' F ST | ε
*)

(* Exercice 2.1.3 *)

