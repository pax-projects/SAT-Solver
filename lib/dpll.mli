
(**  Les literaux sont représentés par des entiers. 
    Un entier positif n représente le littéral n, 
    un entier négatif -n représente le littéral ¬n.
  *)
type literal = int
type clause = literal list
type cnf = clause list
type interpretation = int list
type result = Sat of interpretation | Unsat

(** Cette fonction prend en argument une liste d'entiers et renvoie une cnf *)
val cnf_of_int_list_list : int list list -> cnf

(** Cette fonction prend un argument un objet de type resulat et l'affiche à l'écran. *)
val print_result : result -> unit

(** Cette fonction prend en argument une formule en CNF et renvoie 
    une interprétation qui la satisfait si elle existe, ou Unsat sinon.
    Cette fonction utilise l'algorithme DPLL.
    *)
val dpll_solver : cnf -> result