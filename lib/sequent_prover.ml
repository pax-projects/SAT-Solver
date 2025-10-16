open Sat_types

exception SequentError of string;;

(* Here a cnf list to discribe sequents is enough because of the main project goal (a SAT SOLVER) *)
type sequent = cnf list;;

type sequent_tree = 
| AxiomRule of sequent
| AndRule of (sequent * sequent * sequent)
| OrRule of (sequent * sequent)
;;

(** Utils functions *)
let cnf_to_formula (expr: cnf) = [[[1; 2]]];;

let apply_axiom_rule (conclusion: sequent): sequent_tree =
	AxiomRule([[[1; 2]]])
;;


let apply_and_rule (conclusion: sequent): sequent_tree = 
	AxiomRule([[[1; 2]]])
;;

let apply_or_rule (conclusion: sequent): sequent_tree = 
	AxiomRule([[[1; 2]]])
;;

let prove (conclusion: cnf): unit = 
	(cnf_to_formula conclusion) |> apply_axiom_rule |> fun _ -> ()
;;

prove [[-1; 1]; [2; 3; 0]];;