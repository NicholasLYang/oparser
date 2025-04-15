open Base
open Span
open Grace
open Token

type t
type code
type error

val create : string -> Source.t -> t
val get_next : t -> (token spanned Option.t, error) Result.t
val print_error : error -> unit
