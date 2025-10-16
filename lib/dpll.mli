
(**  Les literaux sont représentés par des entiers. 
    Un entier positif n représente le littéral n, 
    un entier négatif -n représente le littéral ¬n.
  *)
module LiteralSet : Set.S with type elt = int
module ClauseSet : Set.S with type elt = LiteralSet.t

(** Cette fonction prend en argument une liste d'entiers et renvoie une cnf *)
val cnf_of_int_list_list : int list list -> Sat_types.cnf

(** Cette fonction prend un argument un objet de type resulat et l'affiche à l'écran. *)
val print_model : Sat_types.result -> unit

(** Cette fonction prend en argument une formule en CNF et renvoie 
    une interprétation qui la satisfait si elle existe, ou Unsat sinon.
    Cette fonction utilise l'algorithme DPLL.
    *)
val solver_dpll : Sat_types.cnf -> unit

module Test_expose: sig
    val get_unitary : Sat_types.cnf -> Sat_types.literal option

    val get_pure : Sat_types.cnf -> Sat_types.literal option

    val simplify : Sat_types.literal -> Sat_types.cnf -> Sat_types.cnf

    val cnf_to_clause_set : Sat_types.cnf -> ClauseSet.t
end;;

 