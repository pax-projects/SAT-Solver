(** Some types helping *)
type literal = int
type clause = literal list
type cnf = clause list
type interpretation = int list
type result = Sat of interpretation | Unsat