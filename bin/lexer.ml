open Core
open Grace

type code = InvalidCharacter
type token = Plus | Minus
type error = code Grace.Diagnostic.t
type t = { mutable current_index : int; content : string; file_path : string }

let create content file_path = { current_index = 0; content; file_path }

let range source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)

let get_source lexer = `File lexer.file_path

let get_next_char lexer =
  let char = String.get lexer.content lexer.current_index in
  lexer.current_index <- lexer.current_index + 1;
  char

let get_next lexer =
  if Int.equal lexer.current_index (String.length lexer.content) then Ok None
  else
    match get_next_char lexer with
    | '+' -> Ok (Some Plus)
    | '-' -> Ok (Some Minus)
    | c ->
        let message =
          Diagnostic.Message.create
            (Stdlib.Format.sprintf "expected `+`, `-`, found `%c`" c)
        in
        Error
          Diagnostic.(
            createf
              ~labels:
                Label.
                  [
                    primary
                      ~range:
                        (range (get_source lexer) (lexer.current_index - 1)
                           lexer.current_index)
                      message;
                  ]
              ~code:InvalidCharacter Error "Unexpected character found")

let string_of_token = function Plus -> "+" | Minus -> "-"

let print_error error =
  Stdlib.Format.printf "%a@." Grace_ansi_renderer.(pp_diagnostic ()) error
