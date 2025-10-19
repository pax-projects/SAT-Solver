open List

(** Some types helping *)

(* Creating a new set to store literals *)
module LiteralSet = Set.Make(Int);; (* Unsing Int is enough to represent literals with out using any literal module *)
module ClauseSet = Set.Make(LiteralSet);;

type literal = int
type clause = literal list
type cnf = clause list
type interpretation = LiteralSet.t
type result = Sat of interpretation | Unsat


(** Utils functions *)
let cnf_to_clause_set (clauses: int list list): ClauseSet.t = ClauseSet.of_list (map (LiteralSet.of_list) clauses);;

(** val print_model : result -> unit
  *
  * Displays solver result
*)
let print_model: result -> unit = function
  | Unsat   -> print_string "UNSAT\n"
  | Sat model -> let model' = LiteralSet.elements model 
    in let model_sorted = sort (fun i j -> (abs i) - (abs j)) model' 
      in List.iter (fun i -> (print_int i; print_string " ")) model_sorted;
      print_string "0\n"
;;

(** val simplify : literal -> cnf -> cnf 
  *
  * Simplify all clauses setting l to true and -l to false.
  * In other words, it delete every CNF clause containing l and removing -l from every CNF clause.

  * [Complexity]:
  * - TODO
*)
let simplify (literals: LiteralSet.t) (clauses: ClauseSet.t) =
  let neg_literals = LiteralSet.map (fun elt -> -elt) literals in
    (ClauseSet.filter (fun clause -> LiteralSet.disjoint literals clause) clauses)
    |> ClauseSet.map (fun clause -> LiteralSet.diff clause neg_literals)
;;

(** flatten_clauses_to_set clauses: ClauseSet.t -> LiteralSet.t
  * Takes an input of type literal list list
  * and returns a set of the flatten list.

  * [Complexity]: O(N) with N the total number of literals
*)
let flatten_clauses_to_set (clauses: ClauseSet.t): LiteralSet.t = 
  ClauseSet.fold
    (fun clause acc -> LiteralSet.union clause acc)
    clauses
    LiteralSet.empty
;;

(** Dpll solver *)

(** get_pure : ClauseSet.t -> literal option
  * Returns Some(l) if l is the first pure literal in the CNF;
  * Returns None if there is no pure literal in the CNF.

  * [Complexity]:
  * - LiteralSet.filter   -> O(N * log(N))
  * - LiteralSet.map      -> O(N * log(N))
  * - LiteralSet.elements -> O(N)

  * - LiteralSet.union    -> O(N + M) ~ O(max(N, M))
  * - LiteralSet.diff     -> O(N + M) ~ O(max(N, M)) // Same as union
  * - LiteralSet.inter    -> O(N + M) ~ O(max(N, M)) // Also same as union

  * The maximum complexity in the worst cases should be about :
  * O(3 * [N * log(N)] + 4 * N) ~ O(N*log(N))
*)
let get_pures (clauses: ClauseSet.t): LiteralSet.t = 
  (* Separates neg from pos by using sets *)
  let aux (flatten_clauses: LiteralSet.t) = 
    let pos = LiteralSet.filter (fun lit -> lit >= 0) flatten_clauses in
    let neg = flatten_clauses
      |> LiteralSet.filter (fun lit -> lit <= 0) 
      |> LiteralSet.map abs
    in LiteralSet.diff (LiteralSet.union pos neg) (LiteralSet.inter pos neg)
  in flatten_clauses_to_set clauses |> aux 
;;

(** get_unitary : ClauseSet.t -> literal option
  * Returns Some(l) if l is the literal in the first clause containing only one literal;
  * Returns None else.

  * [Complexity]:
  * - TODO
*)
let get_unitaries (clauses: ClauseSet.t): LiteralSet.t =
  (ClauseSet.filter (fun elt -> LiteralSet.cardinal elt = 1)) clauses
  |> flatten_clauses_to_set
;;

(** is_sat : ClauseSet.t -> interpretation -> bool
  * Checks the correctness of an interpretation simplifying the CNF by
  * each interpretaition literal.

  * [Complexity]:
  * - TODO
*)
let is_sat (clauses: ClauseSet.t) (inter: interpretation) = 
  ClauseSet.cardinal (simplify inter clauses) = 0
;;

let get_first_available_literal (clauses: ClauseSet.t): literal option = 
  let clause = ClauseSet.choose_opt clauses in
    match clause with 
    | None -> None
    | Some clause -> LiteralSet.choose_opt clause
;;

(* ----------------- Recursive Solver Functions ----------------- *)
let reduce_unitaries (clauses: ClauseSet.t) = 
  let unitaries = get_unitaries clauses in
  simplify unitaries clauses
;;

let reduce_pures (clauses: ClauseSet.t) =
  let pures = get_pures clauses in
  simplify pures clauses
;;

(** solver_dpll_rec : ClauseSet.t -> interpretation -> result
  * Executes a backtracking algorithm using all the above functions.

  * [Complexity]:
  * - TODO
*)
let rec solver_dpll_rec (clauses: ClauseSet.t) (inter: interpretation) =
  if is_sat clauses inter then
    Sat(inter)
  else (
    clauses
    |> reduce_unitaries
    |> reduce_pures
    |> fun clauses -> match (get_first_available_literal clauses) with
        | None -> if (is_sat clauses inter)
          then Sat(inter)
          else Unsat
        | Some lit -> 
          let try_inter = solver_dpll_rec (simplify (LiteralSet.singleton lit) clauses) (LiteralSet.add lit inter)
          in match try_inter with
            | Sat _ -> try_inter
            | Unsat -> solver_dpll_rec (simplify (LiteralSet.singleton (-lit)) clauses) (LiteralSet.add (-lit) inter) 
  );;

(** solver_dpll: ClauseSet.t -> result
  * Solves the problem by using solver_dpll_rec.
*)
let solver_dpll (clauses: ClauseSet.t) = solver_dpll_rec clauses LiteralSet.empty

module Test_expose = struct 
  let get_unitaries = get_unitaries
  let get_pures = get_pures
  let simplify = simplify
end;;
