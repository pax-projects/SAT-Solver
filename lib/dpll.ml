open List

open Sat_types

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
;;

(** simplifie : literal -> cnf -> cnf 
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)

let rec remove_literal_from_clause l clause = match clause with
	| [] -> []
	| hd::tl -> if hd = l then remove_literal_from_clause l tl else hd::remove_literal_from_clause l tl;;

let simplifie (state: Solver_state.t) (l: literal) (clauses: cnf) =
	Solver_state.remove_literal state l;
	Solver_state.remove_literal state (-l);
	List.filter (fun clause -> not (List.mem l clause)) clauses
	|> List.map (fun clause -> remove_literal_from_clause (-l) clause)
;;

(* ----------------- Solveur dpll récursif ----------------- *)

(** unitaire : cnf -> literal option
	- si `clauses' contient au moins une clause unitaire, retourne
	  le littéral de cette clause unitaire ;
	- sinon renvoie None *)
let rec unitaire = function
	| [] -> None
	| [x]::_ -> Some x
	| _::tl -> unitaire tl
;;

(** pur : cnf -> literal option
	- si `clauses' contient au moins un littéral pur, retourne
	  ce littéral ;
	- sinon renvoie None *)
let pur (state: Solver_state.t) = 
	match Solver_state.NodeSet.max_elt_opt (state.literals_polarisation) with
	| None -> None
	| Some node ->
		if Solver_state.literal_exists state (-node.literal) 
		then None
		else Some (node.literal)
;;

let is_clause_empty clauses = List.exists (fun c -> c = []) clauses;;

let get_random_literal (state: Solver_state.t) = 
	match Solver_state.NodeSet.max_elt_opt (state.literals_polarisation) with
	| None -> None
	| Some node -> Some (node.literal)
;;

let rec solveur_dpll_rec (state: Solver_state.t) (clauses: cnf) (inter: interpretation) =
	(* Solver_state.dump_memory state; *)
	if clauses = [] then Sat inter
	else 
	if is_clause_empty clauses
	then Unsat
	else
		match unitaire clauses with
		| Some l ->
			let save = Solver_state.make_save state l in
			let res = solveur_dpll_rec state (simplifie state l clauses) (l::inter) in (
				match res with
				| Sat _ -> res
				| Unsat -> Solver_state.restore_save state save; Unsat
			)
		| None -> 
			match pur state with
			| Some l -> 
				let save = Solver_state.make_save state l in
				let res = solveur_dpll_rec state (simplifie state l clauses) (l::inter) in (
					match res with
					| Sat _ -> res
					| Unsat -> Solver_state.restore_save state save; Unsat
				)
			| None ->
				match get_random_literal state with
				| None -> Unsat (* plus là par sécurité, mais on a déjà traité [] plus haut *)
				| Some l ->
					let save = Solver_state.make_save state l in
					let res = solveur_dpll_rec state (simplifie state l clauses) (l::inter) in (
						match res with
						| Sat _ -> res
						| Unsat -> 
							(* (Logger.log Logger.SUCCESS "Touching bottom"); *)
							Solver_state.restore_save state save;
							let save = Solver_state.make_save state l in
							let res = solveur_dpll_rec state (simplifie state (-l) clauses) ((-l)::inter) in (
								match res with
								| Sat _ -> res
								| Unsat -> Solver_state.restore_save state save; Unsat
							)
					)
;;

(* let dpll_solver clauses = solveur_dpll_rec clauses [];; *)
let dpll_solver clauses = 
	let state = Solver_state.create clauses in
	 solveur_dpll_rec state clauses [];;