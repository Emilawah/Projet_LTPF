#use "projet.ml";;

(* ============================================================ *)
(* PARTIE 1 : PROGRAMMES VALIDES (Doivent parser sans erreur)   *)
(* ============================================================ *)

(* Cas 1 : Vide (doit renvoyer Skip) *)
let prog1 = "";;

(* Cas 2 : Assignation simple compacte *)
let prog2 = "a:=1";;

(* Cas 3 : Assignation avec beaucoup d'espaces (mais un seul ' ' après := si règle stricte, sinon ok) *)
let prog3 = "   b    :=     0   ";;

(* Cas 4 : Séquence simple *)
let prog4 = "a:=1;b:=0";;

(* Cas 5 : Séquence avec indentation verticale *)
let prog5 = "
    a := 1 ;
    b := 0 ;
    c := 1
";;

(* Cas 6 : If complet avec indentations *)
let prog6 = "
    i ( a ) {
        b := 1
    } {
        b := 0 
    }
";;

(* Cas 6 : If imbriqué *)
let prog6bis = "
    i ( a ) {
        i ( b ) {
          i ( c ) {
            d:=!a
          }{
            d:=!b; a:=0+0+0+0
            }
        }
        {
        i(d){
        }
        {
        } ;
          a:=1.0
        }
    } 
    {
        b := 0 
    }
";;

(* Cas 7 : While compact *)
let prog7 = "w(a){a:=0}";;

(* Cas 8 : While avec corps vide (valide syntaxiquement -> Skip) *)
let prog8 = "w(b){}";;

(* Cas 9 : Expressions complexes (Priorités ! > . > +) *)
let prog9 = "a := !b . c + (1.0)";;

(* Cas 10 : Imbrication If dans While *)
let prog10 = "w(a) { i(b) { c:=1 } { c:=0 } }";;

(* Cas 11 : Imbrication While dans If *)
let prog11 = "i (  a  ) { \n\t w(b) { b:=0 } \n } { \n\t c:=1 \n }";;

(* Cas 12 : Blocs If vides *)
let prog12 = "i(a) { } { }";;

(* Cas 13 : Triple imbrication et expressions *)
let prog13 = "w(a){ i(b){ c := a + b . !d } { d := (a+b).0 } }";;

(* Cas 14 : Programme Fibonacci complet *)
let prog14 = "
    a := 1;
    b := 0;
    c := 0;
    w ( a ) {
        d := b;
        b := a;
        a := d
    }
";;

(* Cas 15 : Tous les cas mélangés *)
let prog15 = "a:=1; i(a){ w(b){ c:=!c } }{ d:=1+0.(!a) } ; b:=0";;

(* --- NOUVEAUX TESTS VALIDES (OPERATEURS & PRIORITES) --- *)

(* Cas 16 : Chaines de négations (!!!!a) *)
let prog16 = "a := ! ! ! ! b";;
(* Cas 17 : Priorité implicite : !a . b doit être (!a).b et pas !(a.b) *)
let prog17 = "a := ! a . b";;

(* Cas 18 : Priorité implicite : a . b + c doit être (a.b) + c *)
let prog18 = "a := a . b + c";;

(* Cas 19 : Parenthèses pour forcer l'ordre inverse *)
let prog19 = "a := ! ( a . b ) + c";;

(* Cas 20 : Expressions longues et mixtes *)
let prog20 = "a := a + b . c + !d ";;






(* ============================================================ *)
(* PARTIE 2 : PROGRAMMES ERRONÉS (Doivent échouer)              *)
(* ============================================================ *)

(* --- ERREURS DE SYNTAXE BASE --- *)

(* Cas 30 : Entier invalide (seuls 0 et 1 sont autorisés) *)
let prog30 = "a := 2";;

(* Cas 31 : Variable invalide *)
let prog31 = "x := 1";;

(* Cas 32 : Opérateur d'assignation incorrect (= au lieu de :=) *)
let prog32 = "a = 1";;


(* Cas 33 : Accolade manquante dans un bloc *)
let prog33 = "w(a) { a:=0";;

(* Cas 34 : Point-virgule en trop à la fin *)
let prog34 = "a := 1 ;";;

(* Cas 35 : Point-virgule manquant entre deux instructions *)
let prog35 = "a := 1 b := 0";;

(* Cas 36 : If mal formé (manque le bloc else) *)
let prog36 = "i(a) { b:=1 }";;

(* Cas 37 : While mal formé (pas de variable de condition) *)
let prog37 = "w() { a:=0 }";;

(* Cas 38 : Caractère illégal inconnu *)
let prog38 = "a := 1 + ?";;

(* --- ERREURS DE PARENTHÉSAGE --- *)

(* Cas 39 : Parenthèse ouvrante non fermée *)
let prog39 = "a := ( 1 + 0";;

(* Cas 40 : Parenthèse fermante en trop *)
let prog40 = "a := 1 + 0 )";;

(* Cas 41 : Parenthèses vides (expression vide interdite) *)
let prog41 = "a := ()";;

(* Cas 42 : Mauvaise imbrication de parenthèses *)
let prog42 = "a := ( 1 + ( 0 . a )";;

(* Cas 43 : Parenthèse au milieu d'un opérateur *)
let prog43 = "a := 1 ( + 0 )";;

(* --- ERREURS D'OPÉRATEURS --- *)

let prog43bis = "a : = 1";;
(* Cas 44 : Négation sans opérande *)
let prog44 = "a := !";;

(* Cas 45 : Négation au mauvais endroit (ex: binaire) *)
let prog45 = "a := a ! b";;

(* Cas 46 : Addition sans membre droit *)
let prog46 = "a := 1 +";;

(* Cas 47 : Addition sans membre gauche *)
let prog47 = "a := + 1";;

(* Cas 48 : Multiplication sans membres *)
let prog48 = "a := .";;
(* Cas 49 : Multiplication sans membres *)
let prog49 = "a := a..b";;
(* Cas 50 : Multiplication sans à droite*)
let prog50 = "a := (a.)";;

let progvertical = "
                w
                (
                a
                )
                {
                b 
                :
                = 
                0
                .
                (
                1
                +
                0
                )
                ;
                a
                :
                =
                !
                b
                }";;






let string_of_var = function
  | A -> "A" | B -> "B" | C -> "C" | D -> "D";;

let string_of_cst = function
  | Zero -> "Zero" | Un -> "Un";;

let rec string_of_expr = function
  | EConst c -> "EConst " ^ string_of_cst c
  | EVar v -> "EVar " ^ string_of_var v
  | EPlus (e1, e2) -> "EPlus(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | EMult (e1, e2) -> "EMult(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | ENot e -> "ENot(" ^ string_of_expr e ^ ")";;

let rec string_of_instr = function
  | Skip -> "Skip"
  | Assign (v, e) -> "Assign(" ^ string_of_var v ^ ", " ^ string_of_expr e ^ ")"
  | Seq (i1, i2) -> "Seq(" ^ string_of_instr i1 ^ ", " ^ string_of_instr i2 ^ ")"
  | If (v, i1, i2) -> "If(" ^ string_of_var v ^ ", " ^ string_of_instr i1 ^ ", " ^ string_of_instr i2 ^ ")"
  | While (v, i) -> "While(" ^ string_of_var v ^ ", " ^ string_of_instr i ^ ")";;


  

let test_valid name prog =
    Printf.printf "[TEST VALIDE]   %-10s : " name;
    try
        let (res, reste) = pr2_Prog (list_of_string prog) in
        if reste = [] then 
            (* ICI : On affiche l'AST si le parsing est réussi *)
            Printf.printf "OK\n    -> AST: %s\n" (string_of_instr res)
        else 
            Printf.printf "FAIL (Reste non parsé: %s)\n" (String.concat "" (List.map (String.make 1) reste))
    with Echec -> Printf.printf "FAIL (Exception Echec levée)\n";;

let test_invalid name prog =
    Printf.printf "[TEST INVALIDE] %-10s : " name;
    try
        let (res, reste) = pr2_Prog (list_of_string prog) in
        if reste <> [] then 
            Printf.printf "OK (Reste détecté)\n"
        else 
            (* Si ça a réussi alors que ça devait rater, on affiche l'AST pour comprendre pourquoi *)
            Printf.printf "FAIL (Accepté à tort !)\n    -> AST généré: %s\n" (string_of_instr res)
    with Echec -> Printf.printf "OK (Exception Echec levée)\n";;


    




let () = 
    print_endline "\n==============================================";
    print_endline "      TESTS DE VALIDITÉ SYNTAXIQUE";
    print_endline "==============================================\n";

    print_endline "--- STRUCTURES & ESPACES ---";
    test_valid "prog1" prog1;
    test_valid "prog2" prog2;
    test_valid "prog3" prog3;
    test_valid "prog4" prog4;
    test_valid "prog5" prog5;
    test_valid "prog6" prog6;
    test_valid "prog6bis" prog6bis;
    test_valid "prog7" prog7;
    test_valid "prog8" prog8;
    test_valid "prog9" prog9;
    test_valid "prog10" prog10;
    test_valid "prog11" prog11;
    test_valid "prog12" prog12;
    test_valid "prog13" prog13;
    test_valid "prog14" prog14;
    test_valid "prog15" prog15;

    print_endline "\n--- OPERATEURS & PRIORITES ---";
    test_valid "prog16 (! chainés)" prog16;
    test_valid "prog17 (! > .)" prog17;
    test_valid "prog18 (. > +)" prog18;
    test_valid "prog19 (Parens)" prog19;
    test_valid "prog20 (Complexe)" prog20;

    print_endline "\n==============================================";
    print_endline "      TESTS D'INVALIDITÉ ";
    print_endline "==============================================\n";

    print_endline "--- SYNTAXE GENERALE ---";
    test_invalid "prog30 (2)" prog30;
    test_invalid "prog31 (var x)" prog31;
    test_invalid "prog32 (=)" prog32;
    test_invalid "prog33 ( { )" prog33;
    test_invalid "prog34 ( ; )" prog34;
    test_invalid "prog35 (no ;)" prog35;
    test_invalid "prog36 (if else)" prog36;
    test_invalid "prog37 (while)" prog37;
    test_invalid "prog38 (?)" prog38;

    print_endline "\n--- PARENTHESES ---";
    test_invalid "prog39 ( ( )" prog39;
    test_invalid "prog40 ( ) )" prog40;
    test_invalid "prog41 ( () )" prog41;
    test_invalid "prog42 ( imbric )" prog42;
    test_invalid "prog43 ( + )" prog43;

    print_endline "\n--- OPERATEURS ---";

    test_invalid "prog43bis ( : = )" prog43bis;
    test_invalid "prog44 ( ! )" prog44;
    test_invalid "prog45 ( a!b )" prog45;
    test_invalid "prog46 ( 1+ )" prog46;
    test_invalid "prog47 ( +1 )" prog47;
    test_invalid "prog48 ( . )" prog48;
    test_invalid "prog49 ( . )" prog49;
    test_invalid "prog50 ( . )" prog50;
    test_invalid "progvertical" progvertical;
    (* du fait de := qui doit être lié *)
    print_endline "\n==============================================\n";;