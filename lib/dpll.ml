(**
	{1 Module DPLL — A little SAT solver}

	This module implements a SAT solver based on the DPLL (Davis–Putnam–Logemann–Loveland) algorithm.
	It uses the DIMACS format (int list list) where integers represent literals (positive or negated).

	The entry point of this module is {!dpll_solver}, which takes a CNF (see type definitions in {!Sat_types})
	and returns a {!Sat_types.result} — either [Sat model] or [Unsat].

	To build a CNF, use {!prepare_cnf}.  
	To display the result, use {!print_result}.

	When satisfiable, the solver prints the model on standard output (in DIMACS style).  
	Otherwise, it prints the word [UNSAT].

	Have fun with SAT&Lite !
*)

open List

open Sat_types

(** ----------------- Utils Funtions ----------------- *)

(**
	[prepare_cnf: int list list -> Sat_types.cnf]
	> converts a raw DIMACS-style integer list list into a proper
	> {!Sat_types.cnf} (a list of {!Sat_types.LiteralSet.t}).

	This conversion is required for the solver to interpret the CNF correctly.

	@param l A CNF represented as [int list list].
	@return The same CNF represented with literal sets list.
*)
let prepare_cnf (l: int list list): cnf = 
	let aux l = List.map (LiteralSet.of_list) l
	in aux l
;;

(**
	[print_result: result -> unit]
	> displays the solver result on standard output.

	- If the result is [Unsat], prints ["UNSAT"].
	- If the result is [Sat model], prints ["SAT"] followed by the model literals
	  in ascending absolute value order, then ["0"].

	@param result The SAT solver result.
*)
let print_result: result -> unit = function
	| Unsat   -> print_string "UNSAT\n"
	| Sat model -> let model = LiteralSet.elements model in
		print_string "SAT\n";
		let sort_model = sort (fun i j -> (abs i) - (abs j)) model in
		List.iter (fun i -> print_int i; print_string " ") sort_model;
		print_string "0\n"
;;

let print_cnf (c: cnf) = 
	c
	|> List.map (fun sub ->
		sub
		|> LiteralSet.elements
		|> List.map string_of_int
		|> String.concat ", "
		|> Printf.sprintf "[%s]"
	)
	|> String.concat "; "
	|> Logger.log Logger.GRAY
;;


(**
	[remove_literals_from_clause: Sat_types.LiteralSet.t -> Sat_types.LiteralSet.t -> Sat_types.LiteralSet.t] 
	> removes from [clause] all literals present in [literals].

	This function is an auxiliary helper used by {!simplify}.

	@param literals The set of literals to remove.
	@param clause The clause to simplify.
	@return The simplified clause.
*)
let remove_literals_from_clause (literals: LiteralSet.t) (clause: LiteralSet.t): LiteralSet.t =
	LiteralSet.filter (fun elt -> not (LiteralSet.mem elt literals)) clause
;;

let check_contradiction (literals: LiteralSet.t): bool =
	LiteralSet.exists (fun elt -> LiteralSet.exists (fun elt' -> elt = -elt') literals) literals
;;

(**
	[simplify: Sat_types.Solver_state.t -> Sat_types.LiteralSet.t -> Sat_types.cnf -> Sat_types.cnf]
	> applies DPLL simplification for a given set of literals.

	It removes all clauses satisfied by the literals and deletes every negated literal
	from the remaining clauses.

	This function also updates the internal solver state.

	@param state The current solver state.
	@param literals The set of literals being assigned to true.
	@param clauses The CNF formula to simplify.
	@return The simplified CNF.

	@see remove_literals_from_clause for per-clause simplification.
*)
let simplify (state: Solver_state.t) (literals: LiteralSet.t) (clauses: cnf): cnf =
	if check_contradiction literals
	then [LiteralSet.empty]
	else (
		let pos = literals in
		let neg = LiteralSet.map (fun elt -> -elt) pos in

		Solver_state.remove_literal state pos;
		Solver_state.remove_literal state neg;
		(* Removes all clauses where a literal from literals exists *)
		List.filter (fun clause -> LiteralSet.disjoint pos clause) clauses
		|> List.map (fun clause -> remove_literals_from_clause neg clause)
	)
;;

(* ----------------- Solveur dpll récursif ----------------- *)

(**
	[unitaire: Sat_types.cnf -> Sat_types.LiteralSet.t option]
	> returns the set of unit literals found in [clauses], if any.

	A unit clause is a clause containing only one literal.  
	These literals can be directly assigned to true during propagation.

	@param clauses The CNF formula.
	@return [Some literals] if unit clauses exist, [None] otherwise.
*)
let unitaire (clauses: cnf): LiteralSet.t option = (*entry : cnf = clause list, clause : LiteralSet.t -> Somet set si unit, None sinon*) 
	let res = 
		List.filter (fun clause -> LiteralSet.cardinal clause = 1) clauses
		|> (fun clauses -> List.fold_left (LiteralSet.union) LiteralSet.empty clauses) (*Combine unit litterals*)
	in (if LiteralSet.is_empty res then None else Some res)
;;

(**
	[pur: Sat_types.Solver_state.t -> Sat_types.LiteralSet.t option]
	> returns a set of pure literals from the solver state.

	A pure literal is one that appears with only one polarity in the formula
	(either always positive or always negative).

	@param state The current solver state.
	@return [Some literals] if pure literals exist, [None] otherwise.
*)
let pur (state: Solver_state.t): LiteralSet.t option =  
	let res = Solver_state.get_max_float_polarity state (*call auxiliary function get get pure literals*)
	in (if LiteralSet.is_empty res then None else Some(res))
;;

(**
	[is_clause_empty: Sat_types.cnf -> bool]
	> checks if at least one clause in [clauses] is empty.

	@param clauses The CNF to test.
	@return [true] if an empty clause exists, [false] otherwise.
*)
let is_clause_empty (clauses: cnf): bool = List.exists (LiteralSet.is_empty) clauses;;

(**
	[get_random_literal Sat_types.Solver_state.t -> Sat_types.literal option]
	> returns a literal (heuristically chosen) from the solver state.

	Typically, this function returns the literal with the highest polarity score.

	@param state The current solver state.
	@return [Some literal] if available, [None] otherwise.
*)
let get_random_literal (state: Solver_state.t): literal option = 
	match Solver_state.NodeSet.max_elt_opt (state.literals_polarisation) with
	| None -> None
	| Some node -> Some (node.literal)
;;

(**
	[solveur_dpll_rec Sat_types.Solver_state.t -> Sat_types.cnf -> Sat_types.interpretation -> Sat_types.result]
	> is the recursive core of the DPLL algorithm.

	This function applies the recursive DPLL procedure:
	- If the CNF is empty --> [Sat inter].
	- If an empty clause exists --> [Unsat].
	- Otherwise, it selects literals to assign based on unit propagation, pure literals,
	  or heuristic choice.

	Each recursive step:
	1. Saves the solver state.
	2. Simplifies the CNF using {!simplify}.
	3. Recursively calls itself.
	4. Restores the state if the branch fails.

	@param state The solver state (mutable context).
	@param clauses The current CNF.
	@param inter The current partial interpretation.
	@return A {!Sat_types.result} ([Sat model] or [Unsat]).
*)
let rec solveur_dpll_rec (state: Solver_state.t) (clauses: cnf) (inter: interpretation): result =
	(* print_cnf clauses; *)
	(* Solver_state.dump_memory state; *)
	if clauses = [] then Sat inter (* all claused statisfied*)
	else 
	if is_clause_empty clauses
	then Unsat  (* empty clause exists*)
	else
		match unitaire clauses with
		| Some literals ->
			(* Logger.log Logger.VIOLET "Unitairy"; *)
			let save = Solver_state.make_save (state) (literals) in (*save current state, simplify CNF, update intepretation, recurse, restore if branch fails*)
			let res = solveur_dpll_rec state (simplify state literals clauses) (LiteralSet.union literals inter) in (
				match res with
				| Sat _ -> res
				| Unsat -> Solver_state.restore_save state save; Unsat
			)
		| None ->
			match pur state with
			| Some literals -> 
				(* Logger.log Logger.VIOLET "Pure"; *)
				let save = Solver_state.make_save (state) (literals) in
				let res = solveur_dpll_rec state (simplify state literals clauses) (LiteralSet.union literals inter) in (
					match res with
					| Sat _ -> res
					| Unsat -> Solver_state.restore_save state save; Unsat
				)
			| None ->
				match get_random_literal state with
				| None -> Unsat (* For more security, already checked upper *)
				| Some l ->
					(* Logger.log Logger.VIOLET "Try"; *)
					let l = LiteralSet.singleton l in (* Essential, converts l into a singleton *)
					let save = Solver_state.make_save (state) (l) in
					let res = solveur_dpll_rec state (simplify state l clauses) (LiteralSet.union l inter) in (
						match res with
						| Sat _ -> res
						| Unsat ->
							(* Logger.log Logger.VIOLET "TryNeg";  *)
							Solver_state.restore_save state save;
							let opposite_l = LiteralSet.map (fun elt -> -elt) l in (* Essential, converts {l} in {-l} *)
							let save = Solver_state.make_save (state) (opposite_l) in
							let res = solveur_dpll_rec state (simplify state opposite_l clauses) (LiteralSet.union opposite_l inter) in (
								match res with
								| Sat _ -> res
								| Unsat -> Solver_state.restore_save state save; Unsat
							)
					)
;;

(**
	[dpll_solver: Sat_types.cnf -> Sat_types.result]
	> initializes a solver state and starts the recursive DPLL process.

	This is the **main entry point** of the module.

	@param clauses The CNF formula to solve.
	@return The result of the SAT resolution, as a {!Sat_types.result}.
*)
let dpll_solver (clauses: cnf): result = 
	let state = Solver_state.create clauses in
	 solveur_dpll_rec (state) (clauses) (LiteralSet.empty);;