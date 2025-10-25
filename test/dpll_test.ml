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

(** 
   [test_get_unitary]
   > Test suite for the [get_unitaries] function.

   - Verifies that the function correctly identifies unit clauses 
     (clauses containing exactly one literal) from a given set of clauses.
   - A return value of [None] means no unit clause was found.
   - A return value of [Some s] means the function found one or more unit literals,
     collected in the set [s].

   The following test cases are included:
   - ["empty"]: Tests behavior on an empty clause set — should return [None].
   - ["mono-clause_true"]: Tests a single unit clause [ [2] ] — should return [Some {2}].
   - ["mono-clause_false"]: Tests a clause that is not unit [ [-3; 2] ] — should return [None].
   - ["multi-clause"]: Tests a set containing multiple clauses, 
      including both [ [3] ] and [ [-3] ], which are both unit clauses — 
      should return [Some {3; -3}].
*)

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
        let s = Option.get (Test_expose.get_unitaries cs) in
        assert_true (LiteralSet.equal s (LiteralSet.of_list [3; -3]))
      )
]

(**
  [test_get_pure]
  > Test suite for the [get_pure] function.

  - Verifies that the function correctly identifies *pure literals* 
    within the solver state.
  - A *pure literal* is a variable that appears with only one polarity 
    (either always positive or always negative) across all clauses.
  - The function should return:
      - [None] if there are no pure literals.
      - [Some s] if one or more pure literals are found, grouped in the set [s].

  Test cases:
  - ["empty"]: Checks behavior on an empty CNF — should return [None].
  - ["mono-clause_some"]: Single clause [ [2; -2; 1] ]; only literal [1] is pure — expect [Some {1}].
  - ["mono-clause_none"]: Single clause [ [1; -1] ]; both polarities of 1 appear — expect [None].
  - ["multi-clause_some"]: Multiple clauses [ [1; -1]; [-1; 3] ]; literal [3] is pure — expect [Some {3}].
  - ["multi-clause_none"]: Clauses [ [1; -2]; [-1; 2] ]; each variable appears both positive and negative — expect [None].
*)
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


(**
   [test_simplify]
   > Test suite for the [simplify] function.

   - Verifies that clause simplification behaves correctly after assigning
     certain literals as true or false.
   - The simplification process should:
   - Remove clauses that are satisfied by assigned literals.
   - Remove negated literals (the opposite of assigned ones) 
        from the remaining clauses.
   - Detect and preserve conflicts (empty clauses) when they occur.

   The following test cases are included:
   - ["empty CNF"]: Simplifying an empty CNF should return an empty CNF.
   - ["mono-clause removed"]: A clause satisfied by the assigned literal [1]
      (e.g., [ [1; 2; 3; 4] ]) should be completely removed.
   - ["multi-clause removing literal -1"]: Assigning [-1] should remove the 
      negated literal [1] from all clauses, leaving [ [2; 3; 4] ].
   - ["multi-clause with conflict"]: When a clause becomes empty due to the 
      assignment (e.g., [[-1]; [1; 2]] with [1] assigned), a conflict 
      is represented by an empty clause [[]].
   - ["all clauses removed"]: When all clauses are satisfied by an assignment 
      (e.g., assigning [2] in [[2]; [2; 3]]), the CNF becomes empty.
   - <["no literal to remove"]: When there is nothing to remove, test is revert because of the fold_left function
*)
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
      

      "no literal to remove" >:: (fun _ ->
          let cs = clause_set_of_lists [[2;3]; [-1;3;4]] in
          let expected = clause_set_of_lists [[3;4]; [2;3] ] in
          let state = Solver_state.create cs in
          assert_equal_cnf expected (Test_expose.simplify state (LiteralSet.singleton 1) cs)
        );
      
    
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
 