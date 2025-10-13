type error_level = ERROR | WARNING | INFO | SUCCESS | OK;;

val log : error_level -> string -> unit;;