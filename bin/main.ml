open Dpll_solver

let usage () =
    Printf.printf "Usage: %s <input_file.cnf>\n" Sys.argv.(0);
    exit 1

let () =
  if Array.length Sys.argv <> 2 then usage ();
  let clauses = Dpll.prepare_cnf (Dimacs.parse Sys.argv.(1)) in
  let modele = Dpll.dpll_solver clauses in
  Dpll.print_result modele
  

    