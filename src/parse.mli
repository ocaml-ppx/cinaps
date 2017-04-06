val run
  :  syntax:Syntax.t
  -> f:(last_text_block:string ->
        code_start:Lexing.position -> code:string -> unit)
  -> file_name:string
  -> file_contents:string
  -> unit
