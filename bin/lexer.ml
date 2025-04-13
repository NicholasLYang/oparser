open Core
open Grace
open Span

type code = InvalidCharacter
type token = Plus | Minus | Ident
type error = code Grace.Diagnostic.t
type t = { mutable current_index : int; content : string; file_path : string }

let create content file_path = { current_index = 0; content; file_path }

let range source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)

let get_source lexer = `File lexer.file_path

let get_next_char lexer =
  if Int.equal lexer.current_index (String.length lexer.content) then None
  else
    let char = String.get lexer.content lexer.current_index in
    let index = lexer.current_index in
    lexer.current_index <- lexer.current_index + 1;
    Some (char, index)

let shift_back lexer =
  lexer.current_index <- lexer.current_index - 1;
  ()

let rec ident lexer start =
  match get_next_char lexer with
  | Some (c, index) ->
      if Char.is_alphanum c || Char.equal c '_' || Char.equal c '\'' then
        ident lexer start
      else (
        (* Don't forget to move index back so we can lex the next token correctly *)
        shift_back lexer;
        Some (s Ident ~start_index:start ~end_index:index))
  | None -> None

let rec get_next lexer =
  match get_next_char lexer with
  | Some (char, index) -> (
      match char with
      | '+' -> Ok (Some (s Plus ~start_index:index ~end_index:index))
      | '-' -> Ok (Some (s Minus ~start_index:index ~end_index:index))
      | ' ' | '\n' | '\t' | '\r' -> get_next lexer
      (* TODO: Add all the unicode edgecases *)
      | c when Char.is_alpha c || Char.equal c '_' -> Ok (ident lexer index)
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
                ~code:InvalidCharacter Error "Unexpected character found"))
  | None -> Ok None

let string_of_token = function Plus -> "+" | Minus -> "-" | Ident -> "<ident>"

let print_error error =
  Stdlib.Format.printf "%a@." Grace_ansi_renderer.(pp_diagnostic ()) error
