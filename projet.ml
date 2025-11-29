#use "anacomb.ml";;
(* Exercice 1.1.1 *)

type var = A | B | C | D
type cst = Zero | Un
type expr =
  | EConst of cst          (* 0 ou 1 *)
  | EVar of var            (* variables a, b, c, d *)
  | EOr of expr * expr    (* Pour + *)
  | EAnd of expr * expr   (* Pour . *)
  | ENot of expr          (* Pour ! *)
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
A ::= C | V | '(' E ')'
F ::= '!' F | A
E ::= T SE
SE ::= '+' T SE | ε
T ::= F ST
ST ::= '.' F ST | ε
*)

(* Exercice 2.1.3 *)

let prCst = terminal_res (function
  | '0' -> Some Zero 
  | '1' -> Some Un 
  | _ -> None
);;

let prVar = terminal_res(function
  |'a' -> Some A
  |'b' -> Some B
  |'c' -> Some C
  |'d' -> Some D
  |_ -> None
);;

let rec prExpr l = (
  prTerm ++> fun t -> prSE t
) l

and prSE arg l = (
  (terminal '+' -+> prTerm ++> fun t ->
    prSE (EOr(arg,t)))
  +|
  epsilon_res(arg)
) l

and prTerm l = (
  prFactor ++> fun f -> prST f
) l 

and prST arg l = (
  (terminal '.' -+> prFactor ++> fun f ->
    prST(EAnd(arg,f)))
  +|
  epsilon_res(arg)
) l

and prFactor l = (
  (terminal '!' -+> prFactor ++> fun f -> epsilon_res (ENot f))
  +| 
  prAtom
) l

and prAtom l = (
  (prCst ++> fun c -> epsilon_res (EConst c))
  +|
  (prVar ++> fun v -> epsilon_res (EVar v))
  +|
  (terminal '(' -+> prExpr ++> fun e -> terminal ')' -+> epsilon_res e)
) l

and prProg l = (
  (prInstr ++> fun i -> prInstrSuite ++> fun s -> epsilon_res(Seq(i,s)))
  +| (epsilon_res Skip)
) l

and prInstr l = (prAssign +| prIf +| prWhile) l

and prInstrSuite l = (
  (terminal ';' -+> prInstr ++> fun i -> prInstrSuite ++> fun s -> epsilon_res(Seq(i, s)))
  +| (epsilon_res Skip)
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

(* Tests *)

let test s = 
  Printf.printf "Test [%-15s] : " s;
  try match prProg (list_of_string s) with
    | (res, []) -> print_endline "OK"
    | _ -> print_endline "ECHEC (Reste non vide)"
  with Echec -> print_endline "ECHEC (Syntaxe)"
;;

test "a:=b+c";;        (* OU *)
test "a:=b.c";;        (* ET *)
test "a:=!b";;         (* NON *)
test "a:=!(b+c)";;     (* Parenthèses et priorité *)
test "a:=a+b.c";;      (* a + (b.c) *)

(* Exercice 2.2.1 *)

type state = var -> cst

let init_state (v : var) : cst = Zero

let update (s : state) (v : var) (c : cst) : state =
  fun x -> if x = v then c else s x

let plus c1 c2 =
  match c1, c2 with
  | Zero, Zero -> Zero
  | _ -> Un

let mult c1 c2 =
  match c1, c2 with
  | Un, Un -> Un
  | _ -> Zero

let neg c =
  match c with
  | Zero -> Un
  | Un -> Zero

let rec eval (e : expr) (s : state) : cst =
  match e with
  | EConst c -> c
  | EVar v -> s v
  | EOr (e1, e2) -> plus (eval e1 s) (eval e2 s)
  | EAnd (e1, e2) -> mult (eval e1 s) (eval e2 s)
  | ENot e1 -> neg (eval e1 s)

let rec exec (i : instr) (s : state) : state =
  match i with
  | Skip -> s
  | Assign (v, e) -> update s v (eval e s)
  | Seq (i1, i2) -> exec i2 (exec i1 s)
  | If (v, i1, i2) -> 
      if s v = Un then exec i1 s else exec i2 s
  | While (v, body) ->
      if s v = Un then
        let s_after = exec body s in
        exec (While (v, body)) s_after
      else
        s

(* Exercice 2.2.2 *)

let string_of_cst = function
  | Zero -> "0"
  | Un -> "1"

let print_state s =
  Printf.printf "A=%s, B=%s, C=%s, D=%s\n" 
    (string_of_cst (s A)) 
    (string_of_cst (s B)) 
    (string_of_cst (s C)) 
    (string_of_cst (s D))

let run_prog code =
  Printf.printf "Programme : %s\n" code;
  try
    let input = list_of_string code in
    match prProg input with
    | (ast, []) -> 
        let final_state = exec ast init_state in
        print_state final_state
    | _ -> print_endline "Erreur : Reste non vide après analyse"
  with Echec -> print_endline "Erreur : Syntaxe incorrecte"

let () =
  print_endline "--- Tests Exécution (Sans Int) ---";
  run_prog "a:=1";
  run_prog "a:=1;b:=a+0";
  run_prog "a:=1;b:=!a";
  run_prog "a:=1;i(a){b:=1}{b:=0}";
  run_prog "a:=1;w(a){b:=b+1;a:=0}";
  run_prog "a:=1;b:=0;c:=(a+b).!(a.b)";
  run_prog "a:=1;b:=1;c:=(a+b).!(a.b)";
  run_prog "a:=1;b:=0;i(a){i(b){c:=0}{c:=1}}{c:=0}";
  run_prog "a:=1;b:=0;c:=a;a:=b;b:=c";
  run_prog "a:=1;b:=1;c:=0;w(a){c:=c+b;b:=0;i(b){}{a:=0}}";
  run_prog "a:=1;b:=1;c:=1;w(a){i(c){c:=0;a:=b}{b:=0;c:=a}}";