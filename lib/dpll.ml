open List

type literal = int
type clause = literal list
type cnf = clause list
type interpretation = int list
type result = Sat of interpretation | Unsat

(** Ce fichier contient les fonctions à compléter. *)

(** Fonctions utilitaires. *)

(** Avec ce choix de type cette fonction est l'identité. *)
let cnf_of_int_list_list (l: int list list) : cnf = l

(** print_modele : result -> unit afficher le résultat *)
let print_result: result -> unit = function
  | Unsat   -> print_string "UNSAT\n"
  | Sat modele -> print_string "SAT\n";
     let modele_trie = sort (fun i j -> (abs i) - (abs j)) modele in
     List.iter (fun i -> print_int i; print_string " ") modele_trie;
     print_string "0\n"

(** simplifie : literal -> cnf -> cnf 
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)

let rec remove_literal_from_clause l clause = match clause with
  | [] -> []
  | hd::tl -> if hd = l then remove_literal_from_clause l tl else hd::remove_literal_from_clause l tl;;

let simplifie l clauses = 
  List.filter (fun clause -> not (List.mem l clause)) clauses
  |> List.map (fun clause -> remove_literal_from_clause (-l) clause)
(** Solveur dpll récursif *)

(** pur : cnf -> literal option
    - si `clauses' contient au moins un littéral pur, retourne
      ce littéral ;
    - sinon renvoie None *)
let rec is_pure l clauses = match clauses with
  | [] -> false
  | hd::tl -> if List.mem (-l) hd then false else is_pure l tl
;;

let rec pur clauses = match clauses with
  | [] -> None
  | hd::tl -> let rec aux = function
      | [] -> pur tl
      | hd'::tl' -> if is_pure hd' clauses then Some(hd') else aux tl'
    in aux hd;;
;;

(** unitaire : cnf -> literal option
    - si `clauses' contient au moins une clause unitaire, retourne
      le littéral de cette clause unitaire ;
    - sinon renvoie None *)
let is_singleton elt = List.length elt = 1;;
let rec unitaire = function
  | [] -> None
  | [x]::tl -> Some x
  | _::tl -> unitaire tl
;;

let is_clause_empty clauses = List.exists (fun c -> c = []) clauses

let rec get_random_literal clauses = match clauses with
  | [] -> None
  | hd::tl ->
    match hd with
    | [] -> get_random_literal tl
    | x::_ -> Some x

let rec solveur_dpll_rec clauses interpretation =
  if clauses = [] then Sat interpretation
  else if is_clause_empty clauses then Unsat
  else
    match unitaire clauses with
    | Some l -> solveur_dpll_rec (simplifie l clauses) (l::interpretation)
    | None ->
      match pur clauses with
      | Some l -> solveur_dpll_rec (simplifie l clauses) (l::interpretation)
      | None ->
        match get_random_literal clauses with
        | None -> Unsat (* plus là par sécurité, mais on a déjà traité [] plus haut *)
        | Some l ->
          match solveur_dpll_rec (simplifie l clauses) (l::interpretation) with
          | Sat _ as s -> s
          | Unsat -> solveur_dpll_rec (simplifie (-l) clauses) ((-l)::interpretation)
;;

(* let dpll_solver clauses = solveur_dpll_rec clauses [];; *)
let dpll_solver clauses = solveur_dpll_rec (Sat_types.Solver_state.create clauses).clauses [];;