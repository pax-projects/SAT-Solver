open Sat_types

(** Utils functions *)
let is_opposite_unitary (literal: literal) (unitary_cnf: cnf) = 
	unitary_cnf = [[-literal]]
;;

(** Rules functions *)
let apply_axiom_rule (conclusion: sequent): sequent_tree = 
	let rec aux = function
		| [] -> raise (SequentException ("Axiom rule cannot be applied on: ", conclusion))
		| [[literal]]::tl ->
			if List.exists (is_opposite_unitary literal) tl
			then AxiomRule(conclusion)
			else aux tl
		| _::tl -> aux tl
	in aux conclusion
;;

let apply_or_rule (conclusion: sequent): sequent_tree = 
	let rec aux = function
		| [] -> raise (SequentException ("Disjunction rule cannot be applied on: ", conclusion))
		| [[literal]]::tl -> aux tl
		| [disjunction]::tl -> DisjunctionRule(
				(List.map (fun elt -> [elt]) disjunction)::tl,
				conclusion
			)
		| _::tl -> aux tl
	in aux conclusion
;;

let apply_and_rule (conclusion: sequent): sequent_tree = 
	let rec aux = function
		| [] -> raise (SequentException ("Conjunction rule cannot be applied on: ", conclusion))
		| (disjunction::disjunction'::_ as clause)::tl -> 
			(* disjunction and disjunction' are int (literals), and clause is an int list *)
			let left_branch  = [ [disjunction] ] @ tl in
			let right_branch = [ [disjunction'] ] @ tl in
			ConjunctionRule (left_branch, right_branch, conclusion)
		| _::tl -> aux tl
	in aux conclusion
;;

let prove (conclusion: cnf): unit = 
	[conclusion] |> apply_axiom_rule |> fun _ -> ()
;;

module Test_expose = struct
	let apply_axiom_rule = apply_axiom_rule
	let apply_or_rule = apply_or_rule
	let apply_and_rule = apply_and_rule
end;;
