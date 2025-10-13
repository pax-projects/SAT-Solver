open OUnit2
open Dpll_solver.Dpll

let assert_true = assert_equal true;;
let assert_false = assert_equal false;;
let assert_equal_option = (fun a b -> assert_true (Option.equal (=) a b));;

let test_get_unitary = "test suite for is_pure" >::: [
	"empty" >:: (fun _ -> assert_true (Option.is_none(Test_expose.get_unitary [])));
	"mono-clause_true" >:: (fun _ -> assert_equal_option (Some(2)) (Test_expose.get_unitary [[2]]));
	"mono-clause_false" >:: (fun _ -> assert_true (Option.is_none(Test_expose.get_unitary [[-3; 2]])));
	"multi-clause" >:: (fun _ -> assert_equal_option (Some 3) (Test_expose.get_unitary [[1; -2]; [-1; 2; 3]; [3]; [-3]]));
];;

let test_get_pure = "test suite for get_pure" >::: [
	"[empty] expected: None" >:: (fun _ -> assert_true (Option.is_none (Test_expose.get_pure [])));
	"[mono-clause] expected: Some(1)" >:: (fun _ -> assert_equal_option (Some(1)) (Test_expose.get_pure [[2; -2; 1]]));
	"[mono-clause] expected: None" >:: (fun _ -> assert_true (Option.is_none(Test_expose.get_pure [[1; -1]])));
	"[multi-clause] expected: Some(3)" >:: (fun _ -> assert_equal_option (Some(3)) (Test_expose.get_pure [[1; -1]; [-1; 3]]));
	"[multi-clause] expected: None" >:: (fun _ -> assert_true (Option.is_none(Test_expose.get_pure [[1; -2]; [-1; 2]])));
];;

let test_suite = "DPLL internal tests" >::: [
	test_get_unitary;
	test_get_pure;
];;

let _ = run_test_tt_main test_suite;;
