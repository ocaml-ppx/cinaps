type t =
  { comment_start_dollar : Re.re
  ; comment_end_or_dollar_comment_end_or_comment_start_if_rec_comment : Re.re
  ; comment_end_anchored : Re.re
  }

val ocaml : t
val c : t
val sexp : t
