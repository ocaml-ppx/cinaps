open StdLabels

module Print_diff = struct
  let patdiff_cmd ~use_color =
    let args =
      List.concat [
        ["-keep-whitespace"];
        ["-location-style omake"];
        (if use_color then ["-unrefined"] else ["-ascii"]);
      ]
    in
    String.concat ~sep:" " ("patdiff" :: args)

  let print ?diff_command ?(use_color=false) ~file1 ~file2 () =
    let exec cmd =
      let cmd =
        Printf.sprintf "%s %s %s 1>&2" cmd (Filename.quote file1) (Filename.quote file2)
      in
      match Sys.command cmd with
      | 0 -> true
      | 1 -> false
      | n -> Printf.eprintf "%S exited with code %d\n" cmd n; exit 2
    in
    match diff_command with
    | Some s -> ignore (exec s : bool)
    | None ->
      if exec (patdiff_cmd ~use_color) then (
        Printf.eprintf "File \"%s\", line 1, characters 0-0:\n%!" file1;
        ignore (exec "diff -u" : bool);
      )
end

let exec_code ~pos code =
  let lexbuf = Lexing.from_string code in
  lexbuf.lex_curr_p <- pos;
  let phrases = !Toploop.parse_use_file lexbuf in
  List.iter phrases ~f:(fun phrase ->
    if not (Toploop.execute_phrase false Format.err_formatter phrase : bool) then
      exit 1)

let execute ~last_text_block ~code_start ~code =
  exec_code ~pos:{ pos_fname = "<cinaps internal>"
                 ; pos_cnum  = 0
                 ; pos_bol   = 0
                 ; pos_lnum  = 1
                 }
    (Printf.sprintf "let _last_text_block = %S" last_text_block);
  exec_code ~pos:code_start code

let extension fn =
  match Filename.chop_extension fn with
  | exception _ -> ""
  | s ->
    let len = String.length fn in
    let len_without_ext = String.length s in
    String.sub fn ~pos:len_without_ext ~len:(len - len_without_ext)

let read_file fn =
  let ic = open_in_bin fn in
  let len = in_channel_length ic in
  let s = really_input_string ic len in
  close_in ic;
  s

let write_file fn s =
  let oc = open_out_bin fn in
  output_string oc s;
  close_out oc

let protect ~finally ~f =
  match f () with
  | x -> finally (); x
  | exception e -> finally (); raise e

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

let main () =
  let in_place       = ref false in
  let styler         = ref None  in
  let diff_command   = ref None  in
  let use_color      = ref false in
  let syntax         = ref Auto in
  let args =
    Arg.align
      [ "-i", Set in_place,
        " Update the file in-place"
      ; "-diff-cmd", String (fun s -> diff_command := Some s),
        " Diff command when using code expectations"
      ; "-no-color", Clear use_color,
        " Don't use colors when printing errors"
      ; "-styler", String (fun s -> styler := Some s),
        " Code styler"
      ; "-syntax", Symbol ([ "auto"; "c"; "ocaml" ],
                           fun s -> syntax := syntax_of_string s),
        " Syntax to use (default: auto)"
      ]
  in
  let usage =
    Printf.sprintf "%s <options> <files>" Sys.executable_name
  in
  let failure = ref false in
  let stdout_copy = Unix.dup Unix.stdout in
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
    Toploop.initialize_toplevel_env ();
    Location.input_name := fn;
    let file_contents = read_file fn in
    let tmp_fn, oc = Filename.open_temp_file "cinaps" (extension fn) in
    let expected =
      protect ~finally:(fun () -> Sys.remove tmp_fn) ~f:(fun () ->
        Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stdout;
        close_out oc;
        let lexbuf = Lexing.from_string file_contents in
        lexbuf.lex_curr_p <-
          { pos_fname = fn
          ; pos_cnum  = 0
          ; pos_lnum  = 1
          ; pos_bol   = 0
          };
        Parse.run ~f:execute ~syntax ~file_name:fn ~file_contents;
        flush stdout;
        Unix.close Unix.stdout;
        Unix.dup2 stdout_copy Unix.stdout;
        match !styler with
        | None -> read_file tmp_fn
        | Some cmd ->
          let cmd = Printf.sprintf "%s %s" cmd (Filename.quote tmp_fn) in
          let ic = Unix.open_process_in cmd in
          let s =
            let file_len = String.length file_contents in
            let buf = Buffer.create file_len in
            try
              Buffer.add_channel buf ic file_len;
              while true do Buffer.add_channel buf ic 65536 done;
              assert false
            with End_of_file ->
              Buffer.contents buf
          in
          match Unix.close_process_in ic with
          | WEXITED 0 -> s
          | WEXITED n ->
            Printf.eprintf "command exited with code %d: %s\n" n cmd;
            exit 1
          | WSIGNALED n ->
            Printf.eprintf "command got signal %d: %s\n" n cmd;
            exit 1
          | WSTOPPED _ -> assert false)
    in
    let corrected_fn = fn ^ ".cinaps-corrected" in
    if file_contents = expected then begin
      if Sys.file_exists corrected_fn then Sys.remove corrected_fn
    end else if !in_place then
      write_file fn expected
    else begin
      write_file corrected_fn expected;
      match !diff_command with
      | Some "-" ->
        (* keep the corrected file but do not output the diff *)
        ()
      | diff_command ->
        failure := true;
        Print_diff.print ()
          ?diff_command
          ~use_color:!use_color
          ~file1:fn
          ~file2:corrected_fn
    end
  in
  try
    Arg.parse args process_file usage;
    if !failure then exit 1
  with exn ->
    Location.report_exception Format.err_formatter exn;
    exit 1
