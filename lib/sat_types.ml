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
		let res = Float.compare a.polarity_score b.polarity_score in
		if res <> 0
		then res
		else Int.compare a.literal b.literal
	;;
	
end;;

module Solver_state = struct
	module NodeSet = Set.Make(Node);;

	type lit_table_type = (int, int) Hashtbl.t

	type t = {
		lit_table: lit_table_type;
		pure_literals: NodeSet.t;
		clauses: cnf;
	}

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
	
	(* TODO: Maybe instead of 64, calculate an approximate table size with [max (length clause) * (length clauses)] * 2 *)
	let create (clauses: cnf): t =
		(* Creates the Hash table *)
		let lit_table = Hashtbl.create 64 in append_literals_to_hashtbl lit_table clauses;
		(* Creates the sets *)
		let pure_literals = keys_to_set_with_polarisation lit_table in
		{
			lit_table = lit_table;
			pure_literals = pure_literals;
			clauses = clauses;
		}
	;;
	
end
