(*open OUnit2
open Dpll_solver.Sequent_prover
open Dpll_solver.Sat_types


(* Helpers *)
let assert_unit = assert_equal ();;
let assert_rule_exception = assert_raises (SequentException ("", []));;

let test_axiom_rule seq =
	try
		match Test_expose.apply_axiom_rule seq with
		| Axiom _ -> ()
		| _ -> failwith "apply_axiom_rule must only return AxiomRule or raise a SequentException."
	with SequentException _ -> raise (SequentException ("", []))
;;

let test_or_rule seq =
	try
		match Test_expose.apply_or_rule seq with
		| DisjunctionRule _ -> ()
		| _ -> failwith "apply_or_rule must only return DisjunctionRule or raise a SequentException."
	with SequentException _ -> raise (SequentException ("", []))
;;

let test_and_rule seq =
	try
		match Test_expose.apply_and_rule seq with
		| ConjunctionRule _ -> ()
		| _ -> failwith "apply_and_rule must only return ConjunctionRule or raise a SequentException."
	with SequentException _ -> raise (SequentException ("", []))
;;

(* ----------------- Axiom Rule Tests ----------------- *)
let axiom_rule_tests = "Axiom Rule Tests" >::: [
	"unitary matching pair [-1,1]" >:: (fun _ ->
		assert_unit (test_axiom_rule [[[-1]]; [[1]]])
	);

	"non-unitary clause [-1 ⋁ 2, 1] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_axiom_rule [[[-1;2]]; [[1]]])
	);

	"non-unitary clause [-1, 1 ⋁ 2] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_axiom_rule [[[-1]]; [[1;2]]])
	);

	"conjunction in first clause [-1 ⋀ 2, 1] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_axiom_rule [[[-1]; [2]]; [[1]]])
	);

	"conjunction in second clause [-1, 1 ⋀ 2] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_axiom_rule [[[-1]]; [[1]; [2]]])
	);
]

(* ----------------- Or Rule Tests ----------------- *)
let or_rule_tests = "Disjunction Rule Tests" >::: [
	"simple unitary [-1,1] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_or_rule [[[-1]]; [[1]]])
	);

	"disjunction [-1 ⋁ 2, 1] -> OK" >:: (fun _ ->
		assert_unit (test_or_rule [[[-1;2]]; [[1]]])
	);

	"disjunction [-1, 1 ⋁ 2] -> OK" >:: (fun _ ->
		assert_unit (test_or_rule [[[-1]]; [[1;2]]])
	);

	"conjunction [-1 ⋀ 2, 1] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_or_rule [[[-1]; [2]]; [[1]]])
	);

	"conjunction [-1, 1 ⋀ 2] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_or_rule [[[-1]]; [[1]; [2]]])
	);
]

(* ----------------- And Rule Tests ----------------- *)
let and_rule_tests = "Conjunction Rule Tests" >::: [
	"simple unitary [-1,1] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_and_rule [[[-1]]; [[1]]])
	);

	"disjunction [-1 ⋁ 2, 1] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_and_rule [[[-1;2]]; [[1]]])
	);

	"disjunction [-1, 1 ⋁ 2] -> exception" >:: (fun _ ->
		assert_rule_exception (fun () -> test_and_rule [[[-1]]; [[1;2]]])
	);

	"conjunction [-1 ⋀ 2, 1] -> OK" >:: (fun _ ->
		assert_unit (test_and_rule [[[-1]; [2]]; [[1]]])
	);

	"conjunction [-1, 1 ⋀ 2] -> OK" >:: (fun _ ->
		assert_unit (test_and_rule [[[-1]]; [[1]; [2]]])
	);

	"double conjunction [(-1 ⋀ 2); (1 ⋀ 3)] -> OK" >:: (fun _ ->
		assert_unit (test_and_rule [[[-1]; [2]]; [[1]; [3]]])
	);
]

(* ----------------- Master Suite ----------------- *)
let sequent_prover_test_suite = "Sequent Prover Full Tests" >::: [
	axiom_rule_tests;
	or_rule_tests;
	and_rule_tests;
]

let _ = run_test_tt_main sequent_prover_test_suite;;
*)
