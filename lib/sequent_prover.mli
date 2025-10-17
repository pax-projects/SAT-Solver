val prove : Sat_types.cnf -> unit;;

module Test_expose: sig
	val apply_axiom_rule : Sat_types.sequent -> Sat_types.sequent_tree
	val apply_or_rule : Sat_types.sequent -> Sat_types.sequent_tree
	val apply_and_rule : Sat_types.sequent -> Sat_types.sequent_tree
end;;
