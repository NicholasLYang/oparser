type span
type 'a spanned

(* Short function for creating spanned values *)
val s : 'a -> start_index:int -> end_index:int -> 'a spanned
val value : 'a spanned -> 'a
val span : 'a spanned -> span
val start_index : span -> int
val end_index : span -> int
