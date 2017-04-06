type t =
  { comment_start_dollar : Re.re
  ; comment_end_or_dollar_comment_end_or_comment_start_if_rec_comment : Re.re
  ; comment_end_anchored : Re.re
  }

let make ~comment_start ~comment_end ~is_ocaml =
  { comment_start_dollar = Re.compile (Re.seq [comment_start; Re.char '$'])
  ; comment_end_or_dollar_comment_end_or_comment_start_if_rec_comment =
      Re.compile
        (Re.alt
           (Re.group comment_end
            :: Re.group (Re.seq [Re.char '$'; comment_end])
            :: if is_ocaml then [Re.group comment_start] else []))
  ; comment_end_anchored = Re.compile (Re.seq [Re.start; comment_end])
  }

let ocaml =
  make
    ~comment_start:(Re.str "(*")
    ~comment_end:(Re.str "*)")
    ~is_ocaml:true

let c =
  make
    ~comment_start:(Re.str "/*")
    ~comment_end:(Re.str "*/")
    ~is_ocaml:false

let sexp =
  make
    ~comment_start:(Re.str "#|")
    ~comment_end:(Re.str "*|#")
    ~is_ocaml:false
