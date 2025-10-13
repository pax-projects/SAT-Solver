open OUnit2
open List
open Dpll_solver

let test_get_unitary = "test suite for is_pure" >::: [
      "empty" >:: (fun _ -> assert_equal None (Dpll.get_unitary []));
      "unitary_true" >:: (fun _ -> assert_equal true (Option.equal (=) (Some(2)) (Dpll.get_unitary [[2]])));
      "unitary_false" >:: (fun _ -> assert_equal true (Option.is_none (Dpll.get_unitary [[-3; 2]])));
      "multiple unitary" >:: (fun _ -> assert_equal (Some 3) (Dpll.get_unitary [[1; -2]; [-1; 2; 3]; [3]; [-3]]));

]

let _ = run_test_tt_main test_get_unitary
(*let _ = run_test_tt_main test_solver_dpll*)
