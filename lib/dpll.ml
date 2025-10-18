open List
open Sat_types

(* Creating a new set to store literals *)
module LiteralSet = Set.Make(Int);; (* Unsing Int is enough to represent literals with out using any literal module *)
module ClauseSet = Set.Make(LiteralSet);;

(** Utils functions *)
let cnf_to_clause_set (clauses: cnf): ClauseSet.t = ClauseSet.of_list (map (LiteralSet.of_list) clauses);;
let cnf_of_int_list_list (l: int list list) : cnf = l;;

(** val print_model : result -> unit
  *
  * Displays solver result
*)
let print_model: result -> unit = function
  | Unsat   -> print_string "UNSAT\n"
  | Sat modele -> print_string "SAT\n";
     let model_sorted = sort (fun i j -> (abs i) - (abs j)) modele in
     List.iter (fun i -> print_int i; print_string " ") model_sorted;
     print_string "0\n"

(** val simplify : literal -> cnf -> cnf 
  *
  * Simplify all clauses setting l to true and -l to false.
  * In other words, it delete every CNF clause containing l and removing -l from every CNF clause.

  * [Complexity]:
  * - TODO
*)
let simplify (l: literal) (clauses: cnf) =
  (* TODO: Moreover, here the program takes list and convert them to Sets; this will be changed *)
  let to_delete = (find_all (mem l) clauses) |> cnf_to_clause_set in
  let full_set = clauses |> cnf_to_clause_set 
  |> (ClauseSet.map (fun clause -> LiteralSet.remove (-l) clause)) (* Remove all opposite literals *)
  in (ClauseSet.diff full_set to_delete)
  |> ClauseSet.elements
  |> map (LiteralSet.elements)
;;

(** flatten_clauses_to_set clauses: cnf -> LiteralSet.t
  * Takes an input of type literal list list
  * and returns a set of the flatten list.

  * [Complexity]: O(N) with N the total number of literals
*)
let flatten_clauses_to_set (clauses: cnf): LiteralSet.t = LiteralSet.of_list (flatten clauses);;

(** Dpll solver *)

(** get_pure : cnf -> literal option
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
let get_pure (clauses: cnf) = 
  (* Separates neg from pos by using sets *)
  let res = 
    let aux (flatten_clauses: LiteralSet.t) = 
      let pos = LiteralSet.filter (fun lit -> lit >= 0) flatten_clauses in
      let neg = flatten_clauses
        |> LiteralSet.filter (fun lit -> lit <= 0) 
        |> LiteralSet.map abs
      in LiteralSet.diff (LiteralSet.union pos neg) (LiteralSet.inter pos neg) |> LiteralSet.elements
    in aux (flatten_clauses_to_set clauses)
  in match res with
  | [] -> None
  | _ -> Some (nth res 0) (* TODO: Change to full literal option list later (for optimisations) *)
;;

(** get_unitary : cnf -> literal option
  * Returns Some(l) if l is the literal in the first clause containing only one literal;
  * Returns None else.

  * [Complexity]:
  * - TODO
*)
let rec get_unitary: cnf -> literal option = function 
  | [] -> None
  | hd::tl -> 
      if length hd = 1 then 
        (nth_opt hd 0)
      else 
        get_unitary tl
;;

(** is_sat : cnf -> interpretation -> bool
  * Checks the correctness of an interpretation simplifying the CNF by
  * each interpretaition literal.

  * [Complexity]:
  * - TODO
*)
let rec is_sat (clauses: cnf) (inter: interpretation) = match inter with
  | [] -> length clauses = 0
  | hd::tl -> is_sat (simplify hd clauses) tl
;;

let get_first_available_literal (clauses: cnf) = match clauses with
  | [] -> None
  | hd:: _ -> match hd with
    | [] -> None
    | hd'::_ -> (Some hd')
;;

(** solver_dpll_rec : cnf -> interpretation -> result
  * Executes a backtracking algorithm using all the above functions.

  * [Complexity]:
  * - TODO
*)
let rec solver_dpll_rec (clauses: cnf) (inter: interpretation) =
  if is_sat clauses inter then
    Sat(inter)
  else (
    match get_unitary clauses with
    | Some unit_lit ->
      let new_clauses = simplify unit_lit clauses in
      let new_inter = (unit_lit :: inter) in
      solver_dpll_rec new_clauses new_inter 

    | None -> match get_pure clauses with
      | Some pure_lit -> 
        let new_clauses = simplify pure_lit clauses in
        let new_inter = (pure_lit :: inter) in
        solver_dpll_rec new_clauses new_inter

      | None -> match get_first_available_literal clauses with
        | None -> if (is_sat clauses inter)
          then Sat(inter)
          else Unsat
        | Some lit -> let try_inter = 
          solver_dpll_rec (simplify lit clauses) (lit :: inter) in
          match try_inter with
            | Sat _ -> try_inter
            | Unsat -> solver_dpll_rec (simplify (-lit) clauses) ((-lit) :: inter) 
  );;

(** solver_dpll: cnf -> result
  * Solves the problem by using solver_dpll_rec.
*)
let solver_dpll (clauses: cnf) = 
  let res = solver_dpll_rec clauses [] in
  match res with
  | Unsat -> (Sequent_prover.prove [[1; 2; -1]; [-4; -5; 4]; [-2; -7; 4]])
  | _ -> print_model res


module Test_expose = struct 
  let get_unitary = get_unitary
  let get_pure = get_pure
  let simplify = simplify

  let cnf_to_clause_set = cnf_to_clause_set
end;;
