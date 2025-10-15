
(**  Les literaux sont représentés par des entiers. 
    Un entier positif n représente le littéral n, 
    un entier négatif -n représente le littéral ¬n.
  *)
type literal = int
(** Une clause est une disjonction de littéraux. 
    On la représente ici simplement par une liste. *)
type clause = literal list
(** Une formule en CNF est une conjonction de clauses.
    On la représente ici simplement par une liste de clauses. *)
type cnf = clause list
type interpretation 
type result = Sat of interpretation | Unsat

module LiteralSet : Set.S with type elt = int
module ClauseSet : Set.S with type elt = LiteralSet.t

(** Cette fonction prend en argument une liste d'entiers et renvoie une cnf *)
val cnf_of_int_list_list : int list list -> cnf

(** Cette fonction prend un argument un objet de type resulat et l'affiche à l'écran. *)
val print_model : result -> unit

(** Cette fonction prend en argument une formule en CNF et renvoie 
    une interprétation qui la satisfait si elle existe, ou Unsat sinon.
    Cette fonction utilise l'algorithme DPLL.
    *)
val solver_dpll : cnf -> result

module Test_expose: sig
    val get_unitary : cnf -> literal option

    val get_pure : cnf -> literal option

    val simplify : literal -> cnf -> cnf

    val cnf_to_clause_set : cnf -> ClauseSet.t
end;;

 