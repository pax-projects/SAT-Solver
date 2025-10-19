open Sat_types

(** Utils functions *)
let string_of_literal l =
	if l < 0 then "-A" ^ string_of_int (-l)
	else "A" ^ string_of_int l
;;

let string_of_clause clause =
	clause
	|> List.map string_of_literal
	|> String.concat " \\/ "
	|> fun s -> "(" ^ s ^ ")"
;;

let string_of_formula formula =
	formula
	|> List.map string_of_clause
	|> String.concat " /\\ "
;;

let string_of_sequent (seq : sequent) =
	let s =
		seq
		|> List.map string_of_formula
		|> String.concat ", "
	in "{" ^ s ^ "}"
;;

let are_opposite (l: literal) (l': literal) = l = -l'
;;

let rec in_tree (l: literal) (tree: prop_tree): bool = match tree with
	| Literal l' -> l = l'
	| OrNode left, right -> (in_tree l left) || (in_tree l right)
	| AndNode left, right -> (in_tree l left) || (in_tree l right)
;;

(* ----------------- Rules Functions ----------------- *)
(* WARNING: These functions, for simplification reasons, are only working on CNF structures. *)
let rec apply_and_rule (left: prop_tree) (right: prop_tree): sequent_tree = match (left, right) with 
	| Literal _, _ -> failwith "The proposition cannot be proved."
	| _, Literal _ -> failwith "The proposition cannot be proved."
	| OrNode left right, OrNode left' right' -> 
		AndRule (
			prove (OrNode (left, right)),
			prove (OrNode (left', right'))
		)
	| OrNode left right, AndNode left' right' -> 
		AndRule (
			prove (OrNode (left, right)),
			prove (AndNode (left', right'))
		)
	| AndNode left right, OrNode left' right' -> 
		AndRule (
			prove (AndNode (left, right)),
			prove (OrNode (left', right'))
		)
	| AndNode left right, AndNode left' right' ->
		AndRule (
			prove (AndNode (left, right)),
			prove (AndNode (left', right'))
		)
;;

let rec apply_or_rule (left: prop_tree) (right: prop_tree): sequent_tree = match (left, right) with 
	| Literal l, Literal l' -> if are_opposite l l' then AxiomRule([left; right])
	| Literal l, OrNode left right | OrNode left right, Literal l -> 
		if in_tree (-l) (OrNode(left, right)) 
		then OrRule([l], prove (OrNode(left, right)))
		else failwith "The proposition cannot be proved."
;;

(* ----------------- Prove Function ----------------- *)
(* WARNING: This function is only working on CNF structures. *)
let rec prove (tree: prop_tree): sequent_tree = match tree with
	| Literal l -> failwith "Never reached branch."
	| AndNode left right -> apply_and_rule left right
	| OrNode left right -> apply_or_rule left right

(* ----------------- CNF to Propositional Tree ----------------- *)
let rec disjunction_to_tree (clause: clause): prop_tree = match clause with
	| [] -> failwith "Error in cnf_to_tree"
	| hd::[] -> Literal hd
	| hd::tl -> OrNode (Literal hd, disjunction_to_tree tl)
;;

let rec cnf_to_tree (clauses: cnf): prop_tree = match clauses with
	| [] -> failwith "Error in cnf_to_tree"
	| hd::[] -> disjunction_to_tree hd
	| hd::tl -> AndNode (disjunction_to_tree hd, cnf_to_tree tl)
;;

module Test_expose = struct
	let apply_axiom_rule = apply_axiom_rule
	let apply_or_rule = apply_or_rule
	let apply_and_rule = apply_and_rule
end;; 

