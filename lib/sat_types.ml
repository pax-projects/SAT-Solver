type literal = int
type clause = literal list
type cnf = clause list
type interpretation = int list
type result = Sat of interpretation | Unsat

module LiteralSet = Set.Make(Int)

(* TODO: Keep it private *)
module Node = struct 
	type t = {
		literal: int;
		polarity_score: float;
	}

	let create (l: int) (ps: float) = {literal = l; polarity_score = ps};;

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

module Solver_state = struct
	module NodeSet = Set.Make(Node);;

	type lit_table_type = (int, int) Hashtbl.t

	type save_type = (literal * int * Node.t) list

	type t = {
		lit_table: lit_table_type;
		mutable literals_polarisation: NodeSet.t;
		clauses: cnf;
	}

	(* ----------------- Utils functions ----------------- *)
	let append_literals_to_hashtbl (hash_tbl: lit_table_type) (clauses: cnf): unit =
		List.iter (
			fun clause -> List.iter (
				fun literal -> match Hashtbl.find_opt hash_tbl literal with
				| None -> Hashtbl.add hash_tbl literal 1
				| Some occurrences -> Hashtbl.replace hash_tbl literal (occurrences+1)
			)
			clause
		) clauses
	;;

	let calculate_polarity_score (hash_tbl: lit_table_type) (l: literal): float = 
		match (Hashtbl.find_opt hash_tbl l, Hashtbl.find_opt hash_tbl (-l)) with
		| None, None -> 0.0
		| None, _ -> 0.0
		| _, None -> Float.max_float
		| Some l_occurences, Some l_occurences' -> (float_of_int l_occurences) /. (float_of_int l_occurences')
	;;
	(* let l_occurences = float_of_int l_occurences in 
	let l'_occurences = float_of_int l'_occurences in
	(Float.max l_occurences l'_occurences) /. (Float.min l_occurences l'_occurences)
	*)

	let keys_to_set_with_polarisation (hash_tbl: lit_table_type): NodeSet.t =
		Hashtbl.fold
		(fun key _ acc -> 
			NodeSet.add
			(Node.create key (calculate_polarity_score hash_tbl key))
			acc
		)
		hash_tbl
		NodeSet.empty
	;;

	let add_literal_to_node_set (set: NodeSet.t) (hash_tbl: lit_table_type) (l: literal): NodeSet.t =
		(* Logger.log Logger.SUCCESS "Adding literal from node set"; *)
		NodeSet.add (Node.create l (calculate_polarity_score hash_tbl l)) set
	;;

	let remove_literal_of_node_set (set: NodeSet.t) (l: literal): NodeSet.t =
		(* Logger.log Logger.WARNING "Removing literal from node set"; *)
		NodeSet.filter (fun node -> node.Node.literal <> l) set
	;;

	let find_node_by_literal (set: NodeSet.t) (l: literal) : Node.t =
		(* Logger.log Logger.WARNING (Printf.sprintf "Finding node where l = %d in %d elements" l (NodeSet.cardinal set)); *)
		(* Using filter+choose because without knowing why, NodeSet.find_first doesn't work... *)
		NodeSet.choose (
			NodeSet.filter (
				fun elt -> (
					(* Printf.printf "%d %f\n" elt.literal elt.polarity_score *)
				); 
				elt.literal = l
			) 
			set
		)
	;;
		
	(* ----------------- API Funcions ----------------- *)
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
		) state.literals_polarisation
	;;

	(* TODO: Maybe instead of 64, calculate an approximate table size with [max (length clause) * (length clauses)] * 2 *)
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

	let literal_exists (state: t) (l: literal): bool = 
		match Hashtbl.find_opt state.lit_table l with
		| None -> false
		| Some _ -> true
	;;

	let remove_literal (state: t) (l: literal) =
		(* Logger.log Logger.WARNING (Printf.sprintf "Simplifying by %d" l);  *)
		Hashtbl.remove state.lit_table l;
		state.literals_polarisation <- (remove_literal_of_node_set (state.literals_polarisation) (l))
	;;

	(* ----------------- Saving functions ----------------- *)
	let make_save (state: t) (l: literal): save_type =
		(* dump_memory state; *)
		let entry_for k =
			(* Logger.log Logger.WARNING (Printf.sprintf "Making save on %d" k); *)
			let old_count = Hashtbl.find state.lit_table k in (* find and not find_opt because no bugs are supposed to happend except if the Hashtbl is empty*)
			let old_node = find_node_by_literal state.literals_polarisation k in
			(k, old_count, old_node)
		in
		if (literal_exists state (-l)) then [entry_for l; entry_for (-l)] else [entry_for l]
	;;

	let restore_save (state: t) (save: save_type): unit =
		(* Logger.log Logger.ERROR "Restoring save"; *)
		(* dump_memory state; *)
		List.iter (fun (key, old_count, _) ->
			(* Logger.log Logger.ERROR (string_of_int key);  *)
			Hashtbl.replace state.lit_table key old_count
		)
		save;

		List.iter (fun (key, _, old_node) ->
			state.literals_polarisation <- (add_literal_to_node_set state.literals_polarisation state.lit_table key)
		)
		save;

		(* Logger.log Logger.ERROR "Memory restored"; *)
		(* dump_memory state; *)
	;;
end


(* BUG: On restore l et -l puis on simplifie (au retour du backtrack) sans avoir refait de save avant... *)