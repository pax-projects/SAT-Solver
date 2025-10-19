module LiteralSet : Set.S with type elt = int
module ClauseSet : Set.S with type elt = LiteralSet.t

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
type interpretation = LiteralSet.t
type result = Sat of interpretation | Unsat

(** Cette fonction prend en argument une liste d'entiers et renvoie une cnf *)
val cnf_to_clause_set : int list list -> ClauseSet.t

(** Cette fonction prend un argument un objet de type resulat et l'affiche à l'écran. *)
val print_model : result -> unit

(** Cette fonction prend en argument une formule en CNF et renvoie 
    une interprétation qui la satisfait si elle existe, ou Unsat sinon.
    Cette fonction utilise l'algorithme DPLL.
    *)
val solver_dpll : ClauseSet.t -> result

module Test_expose: sig
    val get_unitaries : ClauseSet.t -> LiteralSet.t

    val get_pures : ClauseSet.t -> LiteralSet.t

    val simplify : LiteralSet.t -> ClauseSet.t -> ClauseSet.t
end;;
 