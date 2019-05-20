(*$ open StdLabels
    open Printf
    let () = printf "\ntoto\n" *)
toto
(*$*)

(*$ printf "\n";;
    List.iter ["unit"; "string"] ~f:(fun s ->
      printf "let foo_%s = foo_%s\n" s s)
*)
let foo_unit = foo_unit
let foo_string = foo_string
(*$*)


(*$ let blah () = printf "42"
    let foo  () = printf "1"
    let bar  () = printf "2" $*)


let f = function
  | (*$blah()*)42(*$*) -> "blah"
  | (*$foo() *)1(*$*)  -> "foo"
  | (*$bar() *)2(*$*)  -> "bar"
