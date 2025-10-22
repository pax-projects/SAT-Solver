(** Cette fonction prend en argument une liste d'entiers et renvoie une cnf *)
val prepare_cnf : int list list -> Sat_types.cnf

(** Cette fonction prend un argument un objet de type resulat et l'affiche à l'écran. *)
val print_result : Sat_types.result -> unit

(** 
    Cette fonction prend en argument une formule en CNF et renvoie 
    une interprétation qui la satisfait si elle existe, ou Unsat sinon.
    Cette fonction utilise l'algorithme DPLL.
*)
val dpll_solver : Sat_types.cnf -> Sat_types.result