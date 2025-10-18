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

let pretty_printing (tree : sequent_tree) : string =
  let rec aux (tree : sequent_tree) (offset : int) : string list =
    match tree with
    | AxiomNode c ->
        let str = string_of_sequent c in
        let len = String.length str in
        [
          (String.make offset ' ') ^ (String.make len '-') ^ "(ax)";
          (String.make (2*offset) ' ') ^ str;
        ]

    | OrNode (h, c) ->
        let str = string_of_sequent c in
        let len = String.length str in
        let h_lines = aux h offset in
        h_lines
        @ [
            (String.make offset ' ') ^ (String.make len '-') ^ "(\\/)";
            (String.make (2*offset) ' ') ^ str;
          ]

    | AndNode (h1, h2, c) ->
        let left_lines = aux h1 offset in
        let right_lines = aux h2 (offset + 4) in

        (* égalise le nombre de lignes *)
        let max_height = max (List.length left_lines) (List.length right_lines) in
        let pad l =
          let diff = max_height - List.length l in
          l @ List.init diff (fun _ -> "")
        in
        let left_lines = pad left_lines in
        let right_lines = pad right_lines in

        (* concatène ligne par ligne *)
        let combined_lines =
          List.map2
            (fun l r -> l ^ "    " ^ r)
            left_lines right_lines
        in

        let str = string_of_sequent c in
        let len = String.length str in
        combined_lines
        @ [
            (String.make offset ' ') ^ (String.make (len+10) '-') ^ "(/\\)";
            (String.make (offset + 5) ' ') ^ str;
          ]
  in

  String.concat "\n" (aux tree 0)
;;


let rec clause_to_neg_clause = function
	| [] -> []
	| hd::tl -> (-hd)::(clause_to_neg_clause tl)
;;

let rec cnf_to_neg_cnf = function
	| [] -> []
	| hd::tl -> (clause_to_neg_clause hd)::(cnf_to_neg_cnf tl)
;;

let is_opposite_unitary (literal: literal) (unitary_cnf: cnf) = 
	unitary_cnf = [[-literal]]
;;

(** Rules functions *)
let apply_axiom_rule (conclusion: sequent): sequent = 
	let rec aux = function
		| [] -> raise (SequentException ("Axiom rule cannot be applied on: ", conclusion))
		| [[literal]]::tl ->
			if List.exists (is_opposite_unitary literal) tl
			then conclusion
			else aux tl
		| _::tl -> aux tl
	in aux conclusion
;;

let apply_or_rule (conclusion: sequent): sequent * sequent = 
	let rec aux = function
		| [] -> raise (SequentException ("Disjunction rule cannot be applied on: ", conclusion))
		| [[literal]]::tl -> aux tl
		| [disjunction]::tl -> ((List.map (fun elt -> [[elt]]) (disjunction)) @ tl, conclusion)
		| _::tl -> aux tl
	in aux conclusion
;;

let apply_and_rule (conclusion: sequent): sequent * sequent * sequent = 
	let rec aux = function
		| [] -> raise (SequentException ("Conjunction rule cannot be applied on: ", conclusion))
		| (disjunction::disjunction'::_)::tl -> 
			(* disjunction and disjunction' are int (literals), and clause is an int list *)
			let left_branch  = [[disjunction']] @ tl in
			let right_branch = [[disjunction]] @ tl in
			(left_branch, right_branch, conclusion)
		| _::tl -> aux tl
	in aux conclusion
;;

let prove (formula: cnf): unit = 
	let rec aux (seq: sequent) = 
		Logger.log Logger.INFO (string_of_sequent seq);
		match seq with
		| [] -> raise (SequentException ("Could not apply more of this rule", seq))
		| hd::tl ->	match try_apply_rules [hd] with
			| Axiom(c) -> (Logger.log Logger.SUCCESS "Axiom"); AxiomNode(c)
			| OrRule(h, c) -> (Logger.log Logger.SUCCESS "OR"); OrNode(aux h, c)
			| AndRule(h, h', c) -> try 
				(Logger.log Logger.SUCCESS ("AND-1::" ^ (string_of_sequent seq))); AndNode(aux h, aux tl, c)
				with SequentException _ -> (Logger.log Logger.SUCCESS "AND-2"); AndNode(aux h, aux h', c)

			and try_apply_rules seq = 
				try Axiom(apply_axiom_rule seq) with SequentException (_, seq_error) ->
				try let (h, c) = (apply_or_rule seq) in OrRule(h, c) with SequentException _ ->
				try let (h, h', c) = (apply_and_rule seq) in AndRule(h, h', c) with SequentException _ -> 
				failwith (
					"Sequent couldn't be proved because of an internal error | " ^ (string_of_sequent seq_error)
				)
	in [formula] |> aux |> pretty_printing |> print_endline
;;

module Test_expose = struct
	let apply_axiom_rule = apply_axiom_rule
	let apply_or_rule = apply_or_rule
	let apply_and_rule = apply_and_rule
end;; 

