type error_level = ERROR | WARNING | INFO | SUCCESS | OK;;

module Color = struct
	let reset = "\027[0m"

	let apply code s = code ^ s ^ reset

	let red     = apply "\027[31m"
	let green   = apply "\027[32m"
	let yellow  = apply "\027[33m"
	let blue    = apply "\027[34m"
	let magenta = apply "\027[35m"
	let cyan    = apply "\027[36m"
	let gray    = apply "\027[90m"
end;;

let string_of_error_level: error_level -> string = function
| ERROR -> (Color.red "ERROR")
| WARNING -> (Color.yellow "WARNING")
| INFO -> (Color.cyan "INFO")
| SUCCESS | OK -> (Color.green "SUCCESS");;

let log level msg: unit = 
	let formatted_level = string_of_error_level level in
	Printf.printf "[%s] %s\n%!" formatted_level msg
;;