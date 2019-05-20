val run
  :  syntax:Syntax.t
  -> f:(last_text_block:(int * int) ->
        code_start:Lexing.position -> code:string -> unit)
  -> file_name:string
  -> file_contents:string
  -> copy_input:(int -> int -> unit)
  -> unit
