(** DPLL types *)
type literal = int
type clause = literal list
type cnf = clause list
type interpretation = int list
type result = Sat of interpretation | Unsat

(* Here a cnf list to discribe sequents is enough because of the main project goal (a SAT SOLVER) *)
type sequent = cnf list;;

type sequent_tree = 
	| AxiomRule of sequent
	| DisjunctionRule of (sequent * sequent)
	| ConjunctionRule of (sequent * sequent * sequent)
;;

(* Sequent prover types *)
exception SequentException of string * sequent
