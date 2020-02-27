val run
  :  syntax:Syntax.t
  -> f:(last_text_block:int * int -> code_start:Lexing.position -> code:string -> unit)
  -> file_name:string
  -> file_contents:string
  -> copy_input:(int -> int -> unit)
  -> unit

exception
  Error of
    { pos : Lexing.position
    ; msg : string
    }

val raisef : pos:Lexing.position -> ('a, unit, string, _) format4 -> 'a
