type error_level = 
| ERROR
| WARNING
| INFO
| SUCCESS
| OK
| VIOLET
| GRAY
| BLUE
;;

val log : error_level -> string -> unit;;