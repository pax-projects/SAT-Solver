open List

open Sat_types

module LiteralSet = Set.Make(Int)

type literal = int
type clause = LiteralSet.t
type cnf = clause list
type interpretation = LiteralSet.t
type result = Sat of interpretation | Unsat

(** Ce fichier contient les fonctions à compléter. *)

(** Fonctions utilitaires. *)

(** Avec ce choix de type cette fonction est l'identité. *)
let prepare_cnf (l: int list list): cnf = 
	let aux l = List.map (LiteralSet.of_list) l
	in aux l
;;

(** print_modele : result -> unit afficher le résultat *)
let print_result: result -> unit = function
	| Unsat   -> print_string "UNSAT\n"
	| Sat model -> let model = LiteralSet.elements model in
		print_string "SAT\n";
		let sort_model = sort (fun i j -> (abs i) - (abs j)) model in
		List.iter (fun i -> print_int i; print_string " ") sort_model;
		print_string "0\n"
;;

(** simplifie : literal -> cnf -> cnf 
   applique la simplification de l'ensemble des clauses en mettant
   le littéral l à vrai *)
let remove_literals_from_clause (literals: LiteralSet.t) (clause: LiteralSet.t) =
	LiteralSet.filter (fun elt -> not (LiteralSet.mem elt literals)) clause
;;

let simplifie (state: Solver_state.t) (literals: LiteralSet.t) (clauses: cnf): cnf =
	let pos = literals in
	let neg = LiteralSet.map (fun elt -> -elt) pos in

	Solver_state.remove_literal state pos;
	Solver_state.remove_literal state neg;
	(* Removes all clauses where a literal from literals exists *)
	List.filter (fun clause -> LiteralSet.disjoint pos clause) clauses
	|> List.map (fun clause -> remove_literals_from_clause neg clause)
;;

(* ----------------- Solveur dpll récursif ----------------- *)

(** unitaire : cnf -> literal option
	- si `clauses' contient au moins une clause unitaire, retourne
	  le littéral de cette clause unitaire ;
	- sinon renvoie None *)
let unitaire (clauses: cnf): LiteralSet.t option = 
	let res = 
		List.filter (fun clause -> LiteralSet.cardinal clause = 1) clauses
		|> (fun clauses -> List.fold_left (LiteralSet.union) LiteralSet.empty clauses)
	in (if LiteralSet.is_empty res then None else Some res)
;;

(** pur : cnf -> literal option
	- si `clauses' contient au moins un littéral pur, retourne
	  ce littéral ;
	- sinon renvoie None *)
let pur (state: Solver_state.t): LiteralSet.t option = 
	(* Find all where = Max float *)
	let res = Solver_state.get_max_float_polarity state in
	if LiteralSet.is_empty res then None else Some(res)
	(* match Solver_state.NodeSet.max_elt_opt (state.literals_polarisation) with
	| None -> None
	| Some node ->
		if Solver_state.literal_exists state (-node.literal)
		then None
		else Some (LiteralSet.singleton (node.literal)) *)
;;

let is_clause_empty clauses = List.exists (LiteralSet.is_empty) clauses;;

let get_random_literal (state: Solver_state.t): literal option = 
	match Solver_state.NodeSet.max_elt_opt (state.literals_polarisation) with
	| None -> None
	| Some node -> Some (node.literal)
;;

let rec solveur_dpll_rec (state: Solver_state.t) (clauses: cnf) (inter: interpretation): result =
	(* Solver_state.dump_memory state; *)
	if clauses = [] then Sat inter
	else 
	if is_clause_empty clauses
	then Unsat
	else
		match unitaire clauses with
		| Some literals ->
			let save = Solver_state.make_save (state) (literals) in
			let res = solveur_dpll_rec state (simplifie state literals clauses) (LiteralSet.union literals inter) in (
				match res with
				| Sat _ -> res
				| Unsat -> Solver_state.restore_save state save; Unsat
			)
		| None -> 
			match pur state with
			| Some literals -> 
				let save = Solver_state.make_save (state) (literals) in
				let res = solveur_dpll_rec state (simplifie state literals clauses) (LiteralSet.union literals inter) in (
					match res with
					| Sat _ -> res
					| Unsat -> Solver_state.restore_save state save; Unsat
				)
			| None ->
				match get_random_literal state with
				| None -> Unsat (* plus là par sécurité, mais on a déjà traité [] plus haut *)
				| Some l ->
					let l = LiteralSet.singleton l in (* Essentiel, converti l en un singleton *)
					let save = Solver_state.make_save (state) (l) in
					let res = solveur_dpll_rec state (simplifie state l clauses) (LiteralSet.union l inter) in (
						match res with
						| Sat _ -> res
						| Unsat -> 
							(* (Logger.log Logger.SUCCESS "Touching bottom"); *)
							Solver_state.restore_save state save;
							let opposite_l = LiteralSet.map (fun elt -> -elt) l in (* Essentiel, converti {l} en {-l} *)
							let save = Solver_state.make_save (state) (opposite_l) in
							let res = solveur_dpll_rec state (simplifie state opposite_l clauses) (LiteralSet.union opposite_l inter) in (
								match res with
								| Sat _ -> res
								| Unsat -> Solver_state.restore_save state save; Unsat
							)
					)
;;

(* let dpll_solver clauses = solveur_dpll_rec clauses [];; *)
let dpll_solver clauses = 
	let state = Solver_state.create clauses in
	 solveur_dpll_rec (state) (clauses) (LiteralSet.empty);;