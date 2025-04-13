open Core

type code = InvalidCharacter
type location = { start_index : int; end_index : int }
type token = Plus | Minus
type error = code Grace.Diagnostic.t
type t = { current_index : int; content : string }

let create content = { current_index = 0; content }

let get_next lexer =
  if Int.equal lexer.current_index (String.length lexer.content) then Ok None
  else Ok (Some Plus)

let string_of_token = function Plus -> "+" | Minus -> "-"

let print_error error =
  Stdlib.Format.printf "%a@." Grace_ansi_renderer.(pp_diagnostic ()) error
