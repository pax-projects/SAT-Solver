open List

(** Ce fichier contient les fonctions à compléter. *)

(** Fonctions utilitaires. *)

(** filter_map : ('a -> 'b option) -> 'a list -> 'b list
   disponible depuis la version 4.08.0 de OCaml dans le module List :
   pour chaque élément de `list', appliquer `filter' :
   - si le résultat est `Some e', ajouter `e' au résultat ;
   - si le résultat est `None', ne rien y ajouter.
   Attention, cette implémentation inverse l'ordre de la liste *)
let filter_map filter list =
  let rec aux list ret =
    match list with
    | []   -> ret
    | h::t -> match (filter h) with
      | None   -> aux t ret
      | Some e -> aux t (e::ret)
  in aux list []

(* Ceci est un exemple de test unitaire pour la fonction filter_map. *)
let%test "filter_map" =
  (filter_map (fun x -> if x mod 2 = 0 then Some (x * 2) else None) [1; 2; 3; 4]) = [8; 4]

(* Voici d'autres exemples de tests unitaires dont vous pouvez vous inspirer pour tester vos fonctions. *)
let rec big_list n acc =
  if n <= 0 then acc else big_list (n-1) (n::acc)

(* On teste d'abord sur une petite liste.*)
let %test "length vide" = (length [] = 0)
let %test "length 1" = (length [1] = 1)
let %test "length 3" = (length [1;2;3] = 3)
(* Puis sur une grande liste pour vérifier qu'il n'y a pas de débordement de pile. *)
let %test "length big" = let big = 10000000 in (length (big_list big []) = big)


(** Voici une proposition de types pour une implémentation simple.
    Vous pouvez éventuellement changer les types dans un deuxième temps. *)
type literal = int
type clause = literal list
type cnf = clause list
type interpretation = int list
type resultat = Sat of interpretation | Unsat

(** Avec ce choix de type cette fonction est l'identité. *)
let cnf_of_int_list_list (l: int list list) : cnf = l

(** print_modele : resultat -> unit afficher le résultat *)
let print_modele: resultat -> unit = function
  | Unsat   -> print_string "UNSAT\n"
  | Sat modele -> print_string "SAT\n";
     let modele_trie = sort (fun i j -> (abs i) - (abs j)) modele in
     List.iter (fun i -> print_int i; print_string " ") modele_trie;
     print_string "0\n"

(** simplifie : literal -> cnf -> cnf 
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)
let simplifie l clauses =
  (* à compléter *)
  []

(** solveur_split_rec : cnf -> interpretation -> resultat
   exemple d'utilisation de `simplifie' cette fonction ne doit pas être modifiée, sauf si vous changez 
   le type de la fonction simplifie *)
let rec solveur_split_rec clauses interpretation =
  (* l'ensemble vide de clauses est satisfiable *)
  if clauses = [] then Sat interpretation else
  (* la clause vide n'est jamais satisfiable *)
  if mem [] clauses then Unsat else
  (* branchement *) 
  let l = hd (hd clauses) in
  let branche = solveur_split_rec (simplifie l clauses) (l::interpretation) in
  match branche with
  | Unsat -> solveur_split_rec (simplifie (-l) clauses) ((-l)::interpretation)
  | _    -> branche

let solveur_split clauses = solveur_split_rec clauses []

(** Solveur dpll récursif *)

(** pur : cnf -> literal option
    - si `clauses' contient au moins un littéral pur, retourne
      ce littéral ;
    - sinon renvoie None *)
let pur clauses =
  (* à compléter *)
  0

(** unitaire : cnf -> literal option
    - si `clauses' contient au moins une clause unitaire, retourne
      le littéral de cette clause unitaire ;
    - sinon renvoie None *)
let unitaire clauses =
  (* à compléter *)
  0

(** solveur_dpll_rec : cnf -> interpretation -> resultat *)
let rec solveur_dpll_rec clauses interpretation =
  (* à compléter *)
  Unsat

let solveur_dpll clauses = solveur_dpll_rec clauses []
