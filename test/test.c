/*$ open StdLabels
    open Printf
    let () = printf "\ntoto\n" */
toto
/*$*/

/*$ printf "\n";;
    List.iter ["int"; "char*"] ~f:(fun s ->
      printf "static %s foo_%s;\n" s s)
*/
static int foo_int;
static char* foo_char*;
/*$*/


/*$ let blah () = printf "42"
    let foo  () = printf "1"
    let bar  () = printf "2" $*/

char* f(int x) {
  switch(x) {
  case /*$blah()*/42/*$*/: return "blah";
  case /*$foo() */1/*$*/:  return "foo";
  case /*$bar() */2/*$*/:  return "bar";
  default: return "other";
  }
}
