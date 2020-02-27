open StdLabels

let quote_string s =
  let is_substring s ~sub =
    let re = Re.(compile (seq [ rep any; str sub ])) in
    Re.execp re s
  in
  let rec find_tag n =
    let rec make_tag = function
      | 0 -> ""
      | n ->
        Printf.sprintf "%c%s" (Char.chr (Char.code 'a' + (n mod 26))) (make_tag (n / 26))
    in
    let tag = make_tag n in
    if is_substring s ~sub:("|" ^ tag ^ "}") then find_tag (n + 1) else tag
  in
  let tag = find_tag 0 in
  Printf.sprintf "{%s|%s|%s}" tag s tag
;;

type syntax =
  | Auto
  | This of Syntax.t

let syntax_of_string = function
  | "auto" -> Auto
  | "c" -> This Syntax.c
  | "ocaml" -> This Syntax.ocaml
  | "sexp" -> This Syntax.sexp
  | s -> Printf.ksprintf invalid_arg "syntax_of_string (%S)" s
;;

let syntax_of_filename fn =
  let unknown () =
    let pos = { Lexing.pos_fname = fn; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 } in
    Parse.raisef
      ~pos
      "Don't know what syntax to use for this file, pass an explicit [-syntax] option"
  in
  match String.rindex fn '.' with
  | exception _ -> unknown ()
  | i ->
    (match String.sub fn ~pos:(i + 1) ~len:(String.length fn - i - 1) with
     | "ml" | "mli" | "mll" | "mly" | "mld" -> Syntax.ocaml
     | "c" | "h" | "cpp" | "c++" | "cxx" -> Syntax.c
     | "sexp" -> Syntax.sexp
     | _ ->
       (match Filename.basename fn with
        | "jbuild" -> Syntax.sexp
        | _ -> unknown ()))
;;

let process_file ~f ~syntax ~file_name ~file_contents ~copy_input =
  let lexbuf = Lexing.from_string file_contents in
  lexbuf.lex_curr_p <- { pos_fname = file_name; pos_cnum = 0; pos_lnum = 1; pos_bol = 0 };
  Parse.run ~f ~syntax ~file_name ~file_contents ~copy_input
;;

let main () =
  let syntax = ref Auto in
  let staged_output = ref None in
  let init_staged fn =
    let oc = open_out fn in
    staged_output := Some oc;
    Printf.fprintf oc "let () = Cinaps_runtime.init ()\n"
  in
  let args =
    Arg.align
      ([ ( "-syntax"
         , Arg.Symbol ([ "auto"; "c"; "ocaml" ], fun s -> syntax := syntax_of_string s)
         , " Syntax to use (default: auto)" )
       ; ( "-staged"
         , String init_staged
         , "FILE Staged mode: write a .ml file that must be built and executed" )
       ]
       @ Cinaps_runtime.args)
  in
  let usage = Printf.sprintf "%s <options> <files>" Sys.executable_name in
  let process_file fn =
    let syntax =
      match !syntax with
      | Auto -> syntax_of_filename fn
      | This s -> s
    in
    let file_contents = Cinaps_runtime.read_file fn in
    match !staged_output with
    | None ->
      let pos = { Lexing.pos_fname = fn; pos_lnum = 1; pos_cnum = 0; pos_bol = 0 } in
      Parse.raisef ~pos "%s" Non_staged_error.error_message
    | Some oc ->
      let append_code_block
            ~last_text_block:(pos, len)
            ~(code_start : Lexing.position)
            ~code
        =
        Printf.fprintf
          oc
          "let _last_text_block = Cinaps_runtime.input_sub %d %d;;\n# %d %S\n%*s%s\n;;\n"
          pos
          len
          code_start.pos_lnum
          code_start.pos_fname
          (code_start.pos_cnum - code_start.pos_bol)
          ""
          code
      in
      let copy_input pos len =
        Printf.fprintf oc "let () = Cinaps_runtime.copy_input %d %d;;\n" pos len
      in
      Printf.fprintf
        oc
        "let () = Cinaps_runtime.process_file\n\
        \  ~file_name:%S\n\
        \  ~file_contents:%s\n\
        \  (fun () -> let module M = struct\n"
        fn
        (quote_string file_contents);
      process_file ~f:append_code_block ~syntax ~file_contents ~file_name:fn ~copy_input;
      Printf.fprintf oc "end in ());;\n"
  in
  try
    Arg.parse args process_file usage;
    (match !staged_output with
     | None -> ()
     | Some oc ->
       Printf.fprintf oc "Cinaps_runtime.exit ();;\n";
       close_out oc);
    Cinaps_runtime.exit ()
  with
  | Parse.Error { pos; msg } ->
    Printf.eprintf
      "File %S, line %d, character %d:\nError: %s\n"
      pos.pos_fname
      pos.pos_lnum
      (pos.pos_cnum - pos.pos_bol)
      msg;
    exit 1
;;
