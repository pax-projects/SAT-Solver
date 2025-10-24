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
(*
let test_get_pure = "test suite for get_pures" >::: [
  "empty" >:: (fun _ ->
    assert_true (LiteralSet.is_empty (Test_expose.get_pures []))
  );
  "mono-clause_some" >:: (fun _ ->
    let cs = clause_set_of_lists [[2; -2; 1]] in
    assert_true (LiteralSet.equal (Test_expose.get_pures cs) (LiteralSet.singleton 1))
  );
  "mono-clause_none" >:: (fun _ ->
    let cs = clause_set_of_lists [[1; -1]] in
    assert_true (LiteralSet.is_empty (Test_expose.get_pures cs))
  );
  "multi-clause_some" >:: (fun _ ->
    let cs = clause_set_of_lists [[1; -1]; [-1; 3]] in
    assert_true (LiteralSet.equal (Test_expose.get_pures cs) (LiteralSet.singleton 3))
  );
  "multi-clause_none" >:: (fun _ ->
    let cs = clause_set_of_lists [[1; -2]; [-1; 2]] in
    assert_true (LiteralSet.is_empty (Test_expose.get_pures cs))
  );
]

let test_simplify = "test suite for simplify" >::: [
  "empty CNF" >:: (fun _ ->
    assert_equal_cnf [] (Test_expose.simplify (LiteralSet.singleton 1) [])
  );
  "mono-clause removed" >:: (fun _ ->
    let cs = clause_set_of_lists [[1; 2; 3; 4]] in
    assert_equal_cnf [] (Test_expose.simplify (LiteralSet.singleton 1) cs)
  );
  "multi-clause removing literal 1" >:: (fun _ ->
    let cs = clause_set_of_lists [[1;2;3;4]] in
    let expected = clause_set_of_lists [[2;3;4]] in
    assert_equal_cnf expected (Test_expose.simplify (LiteralSet.singleton (-1)) cs)
  );
  "multi-clause with conflict" >:: (fun _ ->
    let cs = clause_set_of_lists [[-1]; [1;2]] in
    let expected = clause_set_of_lists [[]] in
    assert_equal_cnf expected (Test_expose.simplify (LiteralSet.singleton 1) cs)
  );
  "no literal to remove" >:: (fun _ ->
    let cs = clause_set_of_lists [[2;3]; [-1;3;4]] in
    let expected = clause_set_of_lists [[2;3]; [3;4]] in
    assert_equal_cnf expected (Test_expose.simplify (LiteralSet.singleton 1) cs)
  );
  "all clauses removed" >:: (fun _ ->
    let cs = clause_set_of_lists [[2]; [2;3]] in
    assert_equal_cnf [] (Test_expose.simplify (LiteralSet.singleton 2) cs)
  );
]
*)
let test_suite = "DPLL internal tests" >::: [
  test_get_unitary;
 (* test_get_pure;
  test_simplify; *)
]

let _ = run_test_tt_main test_suite
 