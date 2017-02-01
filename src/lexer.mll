{
open StdLabels
let out = output stdout
}

rule lex f file_contents keep text_start = parse
  | "(*$*)"
      { let ofs_end = Lexing.lexeme_end lexbuf in
        let ofs_start =
          if keep then text_start else Lexing.lexeme_start lexbuf
        in
        out file_contents ofs_start (ofs_end - ofs_start);
        lex f file_contents true ofs_end lexbuf
      }
  | "(*$"
      { let last_text_block =
          String.sub file_contents ~pos:text_start
            ~len:(Lexing.lexeme_start lexbuf - text_start)
        in
        let ofs_start =
          if keep then text_start else Lexing.lexeme_start lexbuf
        in
        let code_start = Lexing.lexeme_end_p lexbuf in
        let code_end, keep_next = code 0 lexbuf in
        let code =
          let code_start = code_start.pos_cnum in
          String.sub file_contents ~pos:code_start ~len:(code_end - code_start)
        in
        out file_contents ofs_start
          (Lexing.lexeme_end lexbuf - ofs_start);
        f ~last_text_block ~code_start ~code;
        lex f file_contents keep_next (Lexing.lexeme_end lexbuf) lexbuf
      }
  | '\n'
      { Lexing.new_line lexbuf;
        lex f file_contents keep text_start lexbuf
      }
  | _
      { lex f file_contents keep text_start lexbuf }
  | eof
      { if keep then
          out file_contents text_start
            (Lexing.lexeme_start lexbuf - text_start)
      }

and code depth = parse
  | "$*)"
      { if depth = 0 then
          (Lexing.lexeme_start lexbuf, true)
        else
          code (depth - 1) lexbuf
      }
  | "*)"
      { if depth = 0 then
          (Lexing.lexeme_start lexbuf, false)
        else
          code (depth - 1) lexbuf
      }
  | "(*"
      { code (depth + 1) lexbuf }
  | '\n'
      { Lexing.new_line lexbuf; code depth lexbuf }
  | _
      { code depth lexbuf }
  | eof
      { let pos = Lexing.lexeme_start_p lexbuf in
        let loc = { Location.loc_ghost = false; loc_start = pos; loc_end = pos } in
        Location.raise_errorf ~loc "End of file reached before end of (*$ block"
      }
