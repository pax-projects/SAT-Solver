(**
	{1 Module Sat_types — Core Types and Solver State for SAT&Lite}

	This module defines the core data structures used by the SAT&Lite solver,
	including literals, clauses, CNF representation, and the internal solver state.
	It also provides utilities for polarity computation and state backtracking.
*)

(** A literal is represented as an integer. Positive numbers denote
    positive literals, while negative numbers denote negated ones. *)
type literal = int

(** A clause is a set of literals. *)
module LiteralSet = Set.Make(Int)

(** A clause is a disjunction of literals. *)
type clause = LiteralSet.t

(** A CNF formula is a conjunction of clauses. *)
type cnf = clause list

(** An interpretation represents a set of literals assumed true. *)
type interpretation = LiteralSet.t

(** The result of the SAT solver: either a satisfiable model or UNSAT. *)
type result = Sat of interpretation | Unsat

(** 
  The [Node] module represents a literal annotated with a polarity score.
  The score is used to choose decision literals in the DPLL search tree.
*)
module Node = struct
	(** Internal record representing a literal and its polarity score. *)
	type t = {
		literal: int;
		polarity_score: float;
	}

	(** Creates a node with a given literal and polarity score. *)
	let create (l: literal) (ps: float) = {literal = l; polarity_score = ps};;

	(** 
		Comparison function for nodes. Ensures that two nodes with the same literal
		are considered equal, and otherwise compares by polarity then by literal.
		@return [0] if literals are identical, otherwise the polarity/literal ordering.
	*)
	let compare (a: t) (b: t) = 
		if a.literal = b.literal (* Avoid having two times the same literal *)
		then 0
		else 
			let res = Float.compare a.polarity_score b.polarity_score in
			if res <> 0
			then res
			else Int.compare a.literal b.literal
	;;
end;;

(** 
	The [Solver_state] module manages the mutable state of the DPLL solver.
	It stores the literal frequency table, the set of active literals
	with polarity information, and provides backtracking support.
*)
module Solver_state = struct
	module NodeSet = Set.Make(Node);;

	(** Internal type: mapping literals to their occurrence count. *)
	type lit_table_type = (literal, int) Hashtbl.t;;

	(** A "save" is a list of triples (literal, count, node) for backtracking. *)
	type save_type = (literal * int * Node.t) list;;

	type save_stack_type = save_type Stack.t;;

	(** Mutable solver state. *)
	type t = {
		lit_table: lit_table_type;					(** Frequency table of literals. *)
		mutable literals_polarisation: NodeSet.t;	(** Active literal nodes. *)
		clauses: cnf;								(** Original CNF. *)
	}

	(* ----------------- Utils functions ----------------- *)
	(** 
		Fills a literal occurrence table with all literals appearing in a CNF.
		@param hash_tbl The target hashtable.
		@param clauses The CNF whose literals will be counted.
	*)
	let append_literals_to_hashtbl (hash_tbl: lit_table_type) (clauses: cnf): unit =
		List.iter (
			fun clause -> (LiteralSet.iter (
				fun literal -> match Hashtbl.find_opt hash_tbl literal with
				| None -> Hashtbl.add hash_tbl literal 1
				| Some occurrences -> Hashtbl.replace hash_tbl literal (occurrences+1)
			) clause)
		) clauses
	;;

	(** 
		Computes the polarity score of a literal based on its occurrences
		compared to its negation.
		@param hash_tbl The literal occurrence table.
		@param l The literal.
		@return A float ratio between occurrences of [l] and [-l].
	*)
	let calculate_polarity_score (hash_tbl: lit_table_type) (l: literal): float = 
		match (Hashtbl.find_opt hash_tbl l, Hashtbl.find_opt hash_tbl (-l)) with
		| None, None -> 0.0
		| None, _ -> 0.0
		| _, None -> Float.max_float
		| Some l_occurences, Some l_occurences' -> (float_of_int l_occurences) /. (float_of_int l_occurences')
	;;

	(** Adds a literal to a node set with an updated polarity score. *)
	let add_literal_to_node_set (set: NodeSet.t) (hash_tbl: lit_table_type) (l: literal): NodeSet.t =
		(* Logger.log Logger.SUCCESS "Adding literal from node set"; *)
		NodeSet.add (Node.create l (calculate_polarity_score hash_tbl l)) set
	;;

	(** Converts all literals from the table into a node set with polarities. *)
	let keys_to_set_with_polarisation (hash_tbl: lit_table_type): NodeSet.t =
		Hashtbl.fold
		(fun key _ acc -> add_literal_to_node_set acc hash_tbl key)
		hash_tbl
		NodeSet.empty
	;;

	(** Removes a literal from a node set. *)
	let remove_literal_of_node_set (set: NodeSet.t) (l: literal): NodeSet.t =
		(* Logger.log Logger.WARNING "Removing literal from node set"; *)
		NodeSet.filter (fun node -> node.literal <> l) set
	;;

	(** Finds a node in a set corresponding to the given literal. *)
	let find_node_by_literal (set: NodeSet.t) (l: literal) : Node.t =
		(* Logger.log Logger.WARNING (Printf.sprintf "Finding node where l = %d in %d elements" l (NodeSet.cardinal set)); *)
		(* Using filter+choose because without knowing why, NodeSet.find_first doesn't work... *)
		NodeSet.choose (
			NodeSet.filter (fun elt -> elt.literal = l) set
		)
	;;
		
	(* ----------------- API Functions ----------------- *)
	(** 
		[dump_memory: t -> unit]
		> dumps the internal solver state to the logger for debugging.

		Prints literal frequencies and polarity scores.

		@param state t
		@return unit
	*)
	let dump_memory (state: t): unit =
		Logger.log Logger.WARNING "----------------------------------------------------";
		Logger.log Logger.WARNING "------------------ DUMPING MEMORY ------------------";
		Logger.log Logger.WARNING "----------------------------------------------------";
		Printf.sprintf "Literal table size: %d" (Hashtbl.length state.lit_table) |> Logger.log Logger.INFO;
		Printf.sprintf "Literal binary heap size: %d" (NodeSet.cardinal state.literals_polarisation) |> Logger.log Logger.INFO;
		Hashtbl.iter (fun key value ->
			Printf.sprintf "| %d | %d |" key value |> Logger.log Logger.INFO
		) state.lit_table;

		Printf.printf "\n";

		NodeSet.iter (fun node ->
			Printf.sprintf "{literal: %d; score: %s }" node.literal (if node.polarity_score = Float.max_float then "+inf" else string_of_float node.polarity_score) |> Logger.log Logger.INFO
		) state.literals_polarisation;

		Logger.log Logger.WARNING "----------------------------------------------------";
		Logger.log Logger.WARNING "----------------------------------------------------"
	;;

	(** 
		[create: cnf -> t]
		> creates an initial solver state from a CNF.

		@param clauses The CNF formula.
		@return t A fully initialized solver state.
	*)
	let create (clauses: cnf): t =
		(* Creates the Hash table *)
		let lit_table = Hashtbl.create 4096 in append_literals_to_hashtbl lit_table clauses;
		(* Creates the sets *)
		let literals_polarisation = keys_to_set_with_polarisation lit_table in
		{
			lit_table = lit_table;
			literals_polarisation = literals_polarisation;
			clauses = clauses;
		}
	;;

	(** 
		[literal_exists: t -> literal -> bool]
		> checks whether a literal exists in the solver’s hash table.

		@param state t
		@param l literal
		@return bool
	*)
	let literal_exists (state: t) (l: literal): bool = 
		match Hashtbl.find_opt state.lit_table l with
		| None -> false
		| Some _ -> true
	;;

	(** 
		[remove_literal: t -> LiteralSet.t -> unit]
		> removes a set of literals from the state (both table and node set).

		Typically used during simplification or propagation.

		@param state t
		@param literals LiteralSet.t
		@return unit
	*)
	let remove_literal (state: t) (literals: LiteralSet.t): unit =
		LiteralSet.iter (fun l ->
			(* Logger.log Logger.WARNING (Printf.sprintf "Simplifying by %d" l); *)
			Hashtbl.remove state.lit_table l;
			state.literals_polarisation <- (remove_literal_of_node_set (state.literals_polarisation) (l))
		) literals
	;;

	(**
		[get_max_float_polarity: t -> LiteralSet.t]
		> returns all literals that have an infinite polarity score (+∞).

		@param state t
		@return LiteralSet.t The set of all pure literals
	*)
	let get_max_float_polarity (state: t): LiteralSet.t =
		NodeSet.fold (fun node acc ->
			if node.polarity_score = Float.max_float
			then LiteralSet.add node.literal acc
			else acc
		)
		state.literals_polarisation
		LiteralSet.empty
	;;

	(* ----------------- Saving functions ----------------- *)
	(** 
		[make_save: t -> LiteralSet.t -> save_type]
		> creates a savepoint of the current state for later restoration.

		@param state The solver state.
		@param literals The literals about to be modified.
		@return A list of triples (literal, count, node) representing the saved state.
	*)
	let make_save (state: t) (literals: LiteralSet.t): save_type =
		(* dump_memory state; *)
		let entry_for k =
			(* Logger.log Logger.WARNING (Printf.sprintf "Making save on %d" k); *)
			(* find and not find_opt because no bugs are supposed to happend except if the Hashtbl is empty*)
			let old_count = Hashtbl.find state.lit_table k in
			let old_node = find_node_by_literal state.literals_polarisation k in
			(k, old_count, old_node)
		in LiteralSet.fold (fun l acc ->
			let neg = -l in
			if literal_exists state neg
			then entry_for l :: entry_for neg :: acc
			else entry_for l :: acc
		) literals []
	;;

	(** 
		[restore_save: t -> save_type -> unit]
		> restores a previously saved state.

		@param state The solver state to modify.
		@param save The saved entries returned by [make_save].
		@return unit
	*)
	let restore_save (state: t) (save: save_type): unit =
		(* Logger.log Logger.ERROR "Restoring save"; *)
		(* dump_memory state; *)
		List.iter (fun (key, old_count, old_node) ->
			Hashtbl.replace (state.lit_table) (key) (old_count);
			state.literals_polarisation <- NodeSet.add (old_node) (state.literals_polarisation)
			(* TODO: Check if it's really slower (add_literal_to_node_set state.literals_polarisation state.lit_table key) *)
		) save;

		(* Logger.log Logger.ERROR "Memory restored"; *)
		(* dump_memory state; *)
	;;
end
