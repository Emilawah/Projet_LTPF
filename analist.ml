(* Pour les tests *)

let list_of_string s =
  let n = String.length s in
  let rec boucle i =
    if i = n then [] else s.[i] :: boucle (i+1)
  in boucle 0

(* Le type des aspirateurs (fonctions qui aspirent le préfixe d'une liste) *)
type analist = char list -> char list
(* Le type des aspirateurs qui, en plus, rendent un résultat *)
type 'r ranalist = char list -> 'r * char list

(* Idem en plus général (utile lorsqu'on enchaîne analyse lexicale puis syntaxique). *)
type ('token, 'res) ranalist_gen = 'token list -> 'res * 'token list

exception Echec

(* Aspirateur d'un caractère précis en début de liste *)
let terminal c : analist = fun l -> match l with
  | x :: l when x = c -> l
  | _ -> raise Echec

let terminal_cond (p : char -> bool) : analist = fun l -> match l with
  | x :: l when p x -> l
  | _ -> raise Echec

(* Le même avec résultat *)
let terminal_res (f : 'term -> 'res option) : 'res ranalist =
  fun l -> match l with
  | x :: l -> (match f x with Some y -> y, l | None -> raise Echec)
  | _ -> raise Echec

(* Non-terminal vide *)
let epsilon : analist = fun l -> l

let epsilon_res (res : 'r) : 'r ranalist = fun l -> res, l

