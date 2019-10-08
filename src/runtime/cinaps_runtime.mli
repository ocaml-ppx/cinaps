
val args : (string * Arg.spec * string) list

(* For statged mode only *)
val init : unit -> unit

val exit : unit -> _

val process_file
  :  file_name:string
  -> file_contents:string
  -> (unit -> unit)
  -> unit

val copy_input : int -> int -> unit
val input_sub : int -> int -> string

val read_file : string -> string
