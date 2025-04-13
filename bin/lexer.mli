open Base

type t
type token
type location
type code
type error

val create : string -> t
val get_next : t -> (token Option.t, error) Result.t
val string_of_token : token -> string
val print_error : error -> unit
