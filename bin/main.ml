open Dpll_solver

let usage () =
	Printf.sprintf "Usage: %s <input_file.cnf>\n" Sys.argv.(0)
	|> Logger.log Logger.ERROR;
	exit 1

let () =
	if Array.length Sys.argv <> 2 then usage ();

	let clauses = Dpll.cnf_of_int_list_list (Dimacs.parse Sys.argv.(1)) in
		let modele = Dpll.solver_dpll clauses in
			Dpll.print_model modele
