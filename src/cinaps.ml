open StdLabels

let quote_string s =
  let is_substring s ~sub =
    let re = Re.(compile (seq [rep any; str sub])) in
    Re.execp re s
  in
  let rec find_tag n =
    let rec make_tag = function
      | 0 -> ""
      | n -> Printf.sprintf "%c%s" (Char.chr (Char.code 'a' + (n mod 26)))
               (make_tag (n / 26))
    in
    let tag = make_tag n in
    if is_substring s ~sub:("|" ^ tag ^ "}") then
      find_tag (n + 1)
    else
      tag
  in
  let tag = find_tag 0 in
  Printf.sprintf "{%s|%s|%s}" tag s tag

let exec_code ~pos code =
  let lexbuf = Lexing.from_string code in
  lexbuf.lex_curr_p <- pos;
  let phrases = !Toploop.parse_use_file lexbuf in
  List.iter phrases ~f:(fun phrase ->
    if not (Toploop.execute_phrase false Format.err_formatter phrase : bool) then
      exit 1)

let execute ~file_contents ~last_text_block:(pos, len) ~code_start ~code =
  exec_code ~pos:{ pos_fname = "<cinaps internal>"
                 ; pos_cnum  = 0
                 ; pos_bol   = 0
                 ; pos_lnum  = 1
                 }
    (Printf.sprintf "let _last_text_block = %s"
       (quote_string (String.sub file_contents ~pos ~len)));
  exec_code ~pos:code_start code

type syntax =
  | Auto
  | This of Syntax.t

let syntax_of_string = function
  | "auto"  -> Auto
  | "c"     -> This Syntax.c
  | "ocaml" -> This Syntax.ocaml
  | "sexp"  -> This Syntax.sexp
  | s       -> Printf.ksprintf invalid_arg "syntax_of_string (%S)" s

let syntax_of_filename fn =
  let unknown () =
    let pos =
      { Lexing.
        pos_fname = fn
      ; pos_lnum  = 1
      ; pos_cnum  = 0
      ; pos_bol   = 0
      }
    in
    let loc = { Location.loc_ghost = false; loc_start = pos; loc_end = pos } in
    Location.raise_errorf ~loc
      "Don't know what syntax to use for this file, pass an explicit [-syntax] option"
  in
  match String.rindex fn '.' with
  | exception _ -> unknown ()
  | i ->
    match String.sub fn ~pos:(i + 1) ~len:(String.length fn - i - 1) with
    | "ml" | "mli" | "mll" | "mly" -> Syntax.ocaml
    | "c"| "h" | "cpp" | "c++" | "cxx" -> Syntax.c
    | "sexp" -> Syntax.sexp
    | _ ->
      match Filename.basename fn with
      | "jbuild" -> Syntax.sexp
      | _ -> unknown ()

let process_file ~f ~syntax ~file_name ~file_contents ~copy_input =
  let lexbuf = Lexing.from_string file_contents in
  lexbuf.lex_curr_p <-
    { pos_fname = file_name
    ; pos_cnum  = 0
    ; pos_lnum  = 1
    ; pos_bol   = 0
    };
  Parse.run ~f ~syntax ~file_name ~file_contents ~copy_input

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
      ([ "-syntax", Arg.Symbol ([ "auto"; "c"; "ocaml" ],
                            fun s -> syntax := syntax_of_string s),
         " Syntax to use (default: auto)"
       ; "-staged", String init_staged,
         "FILE Staged mode: write a .ml file that must be built and executed"
       ] @ Cinaps_runtime.args)
  in
  let usage =
    Printf.sprintf "%s <options> <files>" Sys.executable_name
  in
  Toploop.add_directive "use" (Directive_string (fun name ->
    ignore (Toploop.use_silently Format.err_formatter name : bool)))
    { section = "Loading code"
    ; doc = "Read, compile and execute source phrases from the given file.";
    };
  let process_file fn =
    let syntax =
      match !syntax with
      | Auto -> syntax_of_filename fn
      | This s -> s
    in
    let file_contents = Cinaps_runtime.read_file fn in
    match !staged_output with
    | None ->
      Toploop.initialize_toplevel_env ();
      Location.input_name := fn;
      Cinaps_runtime.process_file ~file_name:fn ~file_contents
        (fun () ->
           process_file ~f:(execute ~file_contents) ~syntax ~file_name:fn ~file_contents
             ~copy_input:Cinaps_runtime.copy_input)
    | Some oc ->
      let append_code_block ~last_text_block:(pos, len) ~(code_start:Lexing.position) ~code =
        Printf.fprintf oc "let _last_text_block = Cinaps_runtime.input_sub %d %d;;\n\
                           # %d %S\n\
                           %*s%s\n\
                           ;;\n"
          pos len
          code_start.pos_lnum code_start.pos_fname
          (code_start.pos_cnum - code_start.pos_bol) ""
          code
      in
      let copy_input pos len =
        Printf.fprintf oc
          "let () = Cinaps_runtime.copy_input %d %d;;\n"
          pos len
      in
      Printf.fprintf oc "let () = Cinaps_runtime.process_file\n\
                        \  ~file_name:%S\n\
                        \  ~file_contents:%s\n\
                        \  (fun () -> let module M = struct\n"
        fn (quote_string file_contents);
      process_file ~f:append_code_block ~syntax ~file_contents ~file_name:fn
        ~copy_input;
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
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
