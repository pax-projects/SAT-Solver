(** DPLL types *)
type literal = int
type clause = literal list
type cnf = clause list and nnf = literal list list
type interpretation = int list
type result = Sat of interpretation | Unsat

(* Here a cnf list to discribe sequents is enough because of the main project goal (a SAT SOLVER) *)
type sequent = nnf list;;

type sequent_rule =
	| Axiom of sequent
	| OrRule of sequent * sequent
	| AndRule of sequent * sequent * sequent
;;

type sequent_tree = 
	| AxiomNode of sequent
	| OrNode of (sequent_tree * sequent)
	| AndNode of (sequent_tree * sequent_tree * sequent)
;;

type form_tree =
	| Literal of int
	| And of form_tree * form_tree
	| Or of form_tree * form_tree
;;

(* Sequent prover types *)
exception SequentException of string * sequent
