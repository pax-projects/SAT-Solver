open List

(* Creating a new set to store literals *)
module LiteralSet = Set.Make(Int);;
module ClauseSet = Set.Make(LiteralSet);;

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
type result = Sat of interpretation | Unsat

let cnf_to_clause_set (clauses: cnf): ClauseSet.t = ClauseSet.of_list (map (LiteralSet.of_list) clauses);;

(** Avec ce choix de type cette fonction est l'identité. *)
let cnf_of_int_list_list (l: int list list) : cnf = l

(** print_model : result -> unit afficher le résultat *)
let print_model: result -> unit = function
  | Unsat   -> print_string "UNSAT\n"
  | Sat modele -> print_string "SAT\n";
     let model_sorted = sort (fun i j -> (abs i) - (abs j)) modele in
     List.iter (fun i -> print_int i; print_string " ") model_sorted;
     print_string "0\n"

(** simplify : literal -> cnf -> cnf 
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)
let simplify l clauses =
  (* Delete every clause where l exists *)
  (* TODO: Moreover, here the program takes list and convert them to Sets; this will be changed *)
  let to_delete = (find_all (mem l) clauses) |> cnf_to_clause_set in
  let full_set = clauses |> cnf_to_clause_set 
  |> (ClauseSet.map (fun clause -> LiteralSet.remove (-l) clause)) (* Remove all opposite literals *)
  |> ClauseSet.filter (fun clause -> not (LiteralSet.is_empty clause)) (* Removes all empty clauses *)
  in (ClauseSet.diff full_set to_delete)
  |> ClauseSet.elements
  |> map (LiteralSet.elements)
;;

(** solver_split_rec : cnf -> interpretation -> result
   exemple d'utilisation de `simplify' cette fonction ne doit pas être modifiée, sauf si vous changez 
   le type de la fonction simplify *)
let rec solver_split_rec clauses interpretation =
  (* l'ensemble vide de clauses est satisfiable *)
  if clauses = [] then Sat interpretation else
  (* la clause vide n'est jamais satisfiable *)
  if mem [] clauses then Unsat else
  (* branchement *) 
  let l = hd (hd clauses) in
  let branch = solver_split_rec (simplify l clauses) (l::interpretation) in
  match branch with
  | Unsat -> solver_split_rec (simplify (-l) clauses) ((-l)::interpretation)
  | _    -> branch

let solver_split clauses = solver_split_rec clauses [];;

(** Solveur dpll récursif *)

(** [flatten_clauses_to_set clauses:cnf] takes an input of type literal list list
  and returns a set of the flatten list.

  Example:
  flatten_clauses_to_set [[1; 2; 3]; [2; -2; -4; 5]] -> A set of (1, 2, -2, 3, -4, 5)

  [Complexity]: O(N) with N the total number of literals

*)
let flatten_clauses_to_set (clauses: cnf): LiteralSet.t = LiteralSet.of_list (flatten clauses);;

(** get_pure : cnf -> literal option
    - si `clauses' contient au moins un littéral pur, retourne
      ce littéral ;
    - sinon renvoie None *)
(**
  [Complexity]:
  - LiteralSet.filter   -> O(N * log(N))
  - LiteralSet.map      -> O(N * log(N))
  - LiteralSet.elements -> O(N)

  - LiteralSet.union    -> O(N + M) ~ O(max(N, M))
  - LiteralSet.diff     -> O(N + M) ~ O(max(N, M)) // Same as union
  - LiteralSet.inter    -> O(N + M) ~ O(max(N, M)) // Also same as union

  The maximum complexity in the worst cases should be about O(3 * [N * log(N)] + 4 * N) ~ O(N*log(N))
 *)
let get_pure (clauses: cnf) = 
  (* Separates neg from pos by using sets *)
  let res = 
    let aux flatten_clauses = 
      let pos = LiteralSet.filter (fun lit -> lit >= 0) flatten_clauses in
      let neg = flatten_clauses
        |> LiteralSet.filter (fun lit -> lit <= 0) 
        |> LiteralSet.map abs
      in LiteralSet.diff (LiteralSet.union pos neg) (LiteralSet.inter pos neg) |> LiteralSet.elements
    in aux (flatten_clauses_to_set clauses)
  in match res with
  | [] -> None
  | _ -> Some (nth res 0) (* TODO: Change to full literal option list later (for optimisations) *)
;;

(** get_unitary : cnf -> literal option
    - si `clauses' contient au moins une clause unitaire, retourne
      le littéral de cette clause unitaire ;
    - sinon renvoie None *)
let rec get_unitary: cnf -> literal option = function 
  | [] -> None
  | hd::tl -> 
      if length hd = 1 then 
        (nth_opt hd 0)
      else 
        get_unitary tl
;;

(** solver_dpll_rec : cnf -> interpretation -> result *)

  let rec solver_dpll_rec clauses interpretation =
    if evaluate clauses interpretation 
    then Sat(interpretation)
    else (
      match get_unitary clauses with
      | Some unit_lit ->
        let new_clauses = simplify unit_lit clauses in
        let new_interpretation = (unit_lit :: interpretation) in
        solver_dpll_rec new_clauses new_interpretation 

      |None ->
        match get_pure clauses with
        | Some pure_lit -> 
          let new_clauses = simplify pure_lit clauses in
          let new_interpretation = (pure_lit :: interpretation ) in
          solver_dpll_rec new_clauses new_interpretation

        | None ->
          match get_all_literal clauses with
          | None -> Sat(interpretation)  
          | Some lit -> 
            let try_interpretation =
              solver_dpll_rec (simplify lit clauses) (lit :: interpretation) 
            in

            match try_interpretation with
            | Sat i -> Sat i
            | Unsat -> 
              solver_dpll_rec (simplify (-lit) clauses) ((-lit) :: interpretation) 

    );;

  Unsat

let solver_dpll clauses = solver_dpll_rec clauses []

module Test_expose = struct 
  let get_unitary = get_unitary
  let get_pure = get_pure
  let simplify = simplify

  let cnf_to_clause_set = cnf_to_clause_set
end;;
