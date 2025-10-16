open OUnit2

open Dpll_solver.Sequent_prover
open Dpll_solver.Sat_types

let test_axiom_rule _ =
	let sequent1 = [[1]] in
	let sequent2 = [[-1]] in
	let result = Test_expose.apply_axiom_rule [sequent1; sequent2] in
	match result with
	| AxiomRule _ -> ()
	| _ -> failwith "apply_axiom_rule must only return AxiomRule or raise an SequentException." 
;;

let suite =
	"SequentProver" >::: [
	"axiom_rule" >:: test_axiom_rule
];;

let () = run_test_tt_main suite;;
