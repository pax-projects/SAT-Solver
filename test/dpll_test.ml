 open OUnit2
open Dpll_solver.Dpll
open Dpll_solver.Sat_types


let assert_true = assert_equal true

(* Compare deux ClauseSet.t *)

let assert_equal_cnf (expected: LiteralSet.t list) (actual: LiteralSet.t list) =
  assert_true (List.equal (LiteralSet.equal) expected actual)
  

(* Pour construire rapidement un ClauseSet.t depuis int list list *)
let clause_set_of_lists (l: int list list) : LiteralSet.t list =
  l
  |> List.map LiteralSet.of_list


  (* turn an option in a LiteralSet *)
let unwrap_unitaries = function
  | Some s -> s
  | None -> LiteralSet.empty


let test_get_unitary = "test suite for get_unitaries" >::: [
    "empty" >:: (fun _ ->
        let s = Option.is_none (Test_expose.get_unitaries []) in
        assert_true (s)
      );      
    "mono-clause_true" >:: (fun _ ->
        let cs = clause_set_of_lists [[2]] in
        let s = Option.get (Test_expose.get_unitaries cs) in
        assert_true (LiteralSet.equal s (LiteralSet.singleton 2))
      );

    "mono-clause_false" >:: (fun _ ->
        let cs = clause_set_of_lists [[-3; 2]] in
        let s = Option.is_none (Test_expose.get_unitaries cs) in
        assert_true (s)
      );

    "multi-clause" >:: (fun _ ->
        let cs = clause_set_of_lists [[1; -2]; [-1; 2; 3]; [3]; [-3]] in
        let s = unwrap_unitaries (Test_expose.get_unitaries cs) in
        assert_true (LiteralSet.equal s (LiteralSet.of_list [3; -3]))
      )
]

let test_get_pure = "test suite for get_pure" >::: [

    "empty" >:: (fun _ ->
        let state = Solver_state.create (prepare_cnf []) in
        let s = Option.is_none (Test_expose.pure state) in
        assert_bool "Expected no pure literals" s
      );

    "mono-clause_some" >:: (fun _ ->
        let cnf = clause_set_of_lists [[2; -2; 1]] in
        let state = Solver_state.create cnf in
        match Test_expose.pure state with
        | Some lits ->
          assert_true (LiteralSet.equal lits (LiteralSet.singleton 1))
        | None ->
          assert_failure "Expected pure literal {1}, but got None"
      );

    "mono-clause_none" >:: (fun _ ->
        let cnf = clause_set_of_lists [[1; -1]] in
        let state = Solver_state.create cnf in
        let s = Option.is_none (Test_expose.pure state) in
        assert_bool "Expected no pure literals" s
      );

    "multi-clause_some" >:: (fun _ ->
        let cnf = clause_set_of_lists [[1; -1]; [-1; 3]] in
        let state = Solver_state.create cnf in
        match Test_expose.pure state with
        | Some lits ->
          assert_true (LiteralSet.equal lits (LiteralSet.singleton 3))
        | None ->
          assert_failure "Expected pure literal {3}, but got None"
      );

    "multi-clause_none" >:: (fun _ ->
        let cnf = clause_set_of_lists [[1; -2]; [-1; 2]] in
        let state = Solver_state.create cnf in
        let s = Option.is_none (Test_expose.pure state) in
        assert_bool "Expected no pure literals" s
      );
  ]


let test_simplify = "test suite for simplify" >::: [

    "empty CNF" >:: (fun _ ->
        let state = Solver_state.create [] in
        assert_equal_cnf [] (Test_expose.simplify state (LiteralSet.singleton 1) [])
      );

    "mono-clause removed" >:: (fun _ ->
        let cs = clause_set_of_lists [[1; 2; 3; 4]] in
        let state = Solver_state.create cs in
        assert_equal_cnf [] (Test_expose.simplify state (LiteralSet.singleton 1) cs)
      );

    "multi-clause removing literal -1" >:: (fun _ ->
        let cs = clause_set_of_lists [[1;2;3;4]] in
        let expected = clause_set_of_lists [[2;3;4]] in
        let state = Solver_state.create cs in
        assert_equal_cnf expected (Test_expose.simplify state (LiteralSet.singleton (-1)) cs)
      );

    "multi-clause with conflict" >:: (fun _ ->
        let cs = clause_set_of_lists [[-1]; [1;2]] in
        let expected = clause_set_of_lists [[]] in
        let state = Solver_state.create cs in
        assert_equal_cnf expected (Test_expose.simplify state (LiteralSet.singleton 1) cs)
      );
      (*

    "no literal to remove" >:: (fun _ ->
        let cs = clause_set_of_lists [[2;3]; [-1;3;4]] in
        let expected = clause_set_of_lists [[2;3]; [3;4]] in
        let state = Solver_state.create cs in
        assert_equal_cnf expected (Test_expose.simplify state (LiteralSet.singleton 1) cs)
      );
    *)
    
    "all clauses removed" >:: (fun _ ->
        let cs = clause_set_of_lists [[2]; [2;3]] in
        let state = Solver_state.create cs in
        assert_equal_cnf [] (Test_expose.simplify state (LiteralSet.singleton 2) cs)
      );
  ]


let test_suite = "DPLL internal tests" >::: [
  test_get_unitary;
  test_get_pure; 
   test_simplify; 
]

let _ = run_test_tt_main test_suite
 