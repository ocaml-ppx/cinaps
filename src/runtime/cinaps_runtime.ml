open StdLabels

let in_place = ref false
let styler = ref None
let diff_command = ref None
let use_color = ref false

let args =
  let open Arg in
  [  "-i", Set in_place,
     " Update the file in-place"
  ;"-diff-cmd", String (fun s -> diff_command := Some s),
    " Diff command when using code expectations"
  ; "-no-color", Clear use_color,
    " Don't use colors when printing errors"
  ; "-styler", String (fun s -> styler := Some s),
    " Code styler"
  ]

let init () =
  let usage =
    Printf.sprintf "%s <options>" Sys.executable_name
  in
  let anon fn =
    raise (Arg.Bad (Printf.sprintf "Don't know what to do with %S." fn))
  in
  Arg.parse (Arg.align args) anon usage

module Print_diff = struct
  let patdiff_cmd () =
    let args =
      List.concat [
        ["-keep-whitespace"];
        ["-location-style omake"];
        (if !use_color then ["-unrefined"] else ["-ascii"]);
      ]
    in
    String.concat ~sep:" " ("patdiff" :: args)

  let print ~file1 ~file2 =
    let exec cmd =
      let cmd =
        Printf.sprintf "%s %s %s 1>&2" cmd (Filename.quote file1) (Filename.quote file2)
      in
      match Sys.command cmd with
      | 0 -> true
      | 1 -> false
      | n -> Printf.eprintf "%S exited with code %d\n" cmd n; exit 2
    in
    match !diff_command with
    | Some s -> ignore (exec s : bool)
    | None ->
      if exec (patdiff_cmd ()) then (
        Printf.eprintf "File \"%s\", line 1, characters 0-0:\n%!" file1;
        ignore (exec "diff -u" : bool);
      )
end

let failure = ref false

let current_file_contents = ref ""

let copy_input pos len = output_substring stdout !current_file_contents pos len

let input_sub pos len = String.sub !current_file_contents ~pos ~len

let protect ~finally ~f =
  match f () with
  | x -> finally (); x
  | exception e -> finally (); raise e

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

let process_file ~file_name ~file_contents f =
  let tmp_fn, oc = Filename.open_temp_file "cinaps" (Filename.extension file_name) in
  let expected =
    protect ~finally:(fun () -> Sys.remove tmp_fn) ~f:(fun () ->
      let stdout_copy = Unix.dup Unix.stdout in
      Unix.dup2 (Unix.descr_of_out_channel oc) Unix.stdout;
      close_out oc;
      current_file_contents := file_contents;
      f ();
      flush stdout;
      Unix.close Unix.stdout;
      Unix.dup2 stdout_copy Unix.stdout;
      Unix.close stdout_copy;
      match Filename.extension file_name, !styler with
      | (".ml" | ".mli"), Some cmd -> begin
        let cmd =
          String.concat ~sep:""
            (match String.split_on_char cmd ~sep:'%' with
             | [] -> assert false
             | x :: l ->
               x :: List.map l ~f:(fun s ->
                 let len = String.length s in
                 if len > 0 && s.[0] = 'i' then
                   (Filename.quote file_name) ^ String.sub s ~pos:1 ~len:(len - 1)
                 else
                   "%" ^ s))
        in
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
        | WSTOPPED _ -> assert false
      end
      | _ -> read_file tmp_fn)
  in
  let corrected_fn = file_name ^ ".cinaps-corrected" in
  if file_contents = expected then begin
    if Sys.file_exists corrected_fn then Sys.remove corrected_fn
  end else if !in_place then
    write_file file_name expected
  else begin
    write_file corrected_fn expected;
    match !diff_command with
    | Some "-" ->
      (* keep the corrected file but do not output the diff *)
      ()
    | _ ->
      failure := true;
      Print_diff.print
        ~file1:file_name
        ~file2:corrected_fn
  end

let exit () =
  exit (if !failure then 1 else 0)
