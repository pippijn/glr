type kind =
  | AK_LEFT
  | AK_RIGHT
  | AK_NONASSOC
  | AK_NEVERASSOC
  | AK_SPLIT
  with sexp


let of_string = function
  | "left"        -> AK_LEFT
  | "right"       -> AK_RIGHT
  | "nonassoc"    -> AK_NONASSOC
  | "prec"        -> AK_NEVERASSOC
  | "assoc_split" -> AK_SPLIT
  | s             -> failwith s


let to_string = function
  | AK_LEFT       -> "left"
  | AK_RIGHT      -> "right"
  | AK_NONASSOC   -> "nonassoc"
  | AK_NEVERASSOC -> "prec"
  | AK_SPLIT      -> "assoc_split"


let of_locstring s = Sloc.map of_string s
