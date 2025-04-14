open Core
open Grace
open Span

type code = InvalidCharacter
type token = Plus | Minus | Ident | Number | Char
type error = code Grace.Diagnostic.t
type t = { mutable current_index : int; content : string; source : Source.t }

let create content source = { current_index = 0; content; source }

let range source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)

let get_source lexer = lexer.source

let shift_forward lexer =
  lexer.current_index <- lexer.current_index + 1;
  ()

let shift_back lexer =
  lexer.current_index <- lexer.current_index - 1;
  ()

let peek_next_char lexer =
  if Int.( >= ) lexer.current_index (String.length lexer.content) then None
  else
    let char = String.get lexer.content lexer.current_index in
    let index = lexer.current_index in
    Some (char, index)

let get_next_char lexer =
  let char = peek_next_char lexer in
  shift_forward lexer;
  char

let is_unicode_char c =
  let code = Char.to_int c in
  Int.between code ~low:0xdf ~high:0xf6
  || Int.between code ~low:0xf8 ~high:0xff
  || Int.equal code 0x153 || Int.equal code 0x161 || Int.equal code 0x17e
  || Int.between code ~low:0xc0 ~high:0xd6
  || Int.between code ~low:0xd8 ~high:0xde
  || Int.equal code 0x152 || Int.equal code 0x160 || Int.equal code 0x17d
  || Int.equal code 0x178 || Int.equal code 0x1e9e

let rec ident lexer start =
  match get_next_char lexer with
  | Some (c, _)
    when Char.is_alphanum c || Char.equal c '_' || Char.equal c '\''
         || is_unicode_char c ->
      ident lexer start
  | Some (_, index) ->
      (* Don't forget to move index back so we can lex the next token correctly *)
      shift_back lexer;
      Some (s Ident ~start_index:start ~end_index:index)
  | None when Int.( <> ) start lexer.current_index ->
      Some (s Ident ~start_index:start ~end_index:(lexer.current_index - 1))
  | None -> None

let is_hex_digit c =
  Char.is_digit c
  || Char.between c ~high:'f' ~low:'a'
  || Char.between c ~high:'F' ~low:'A'

let rec hex_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_hex_digit c -> hex_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      Some (s Number ~start_index:start ~end_index:(index - 1))
  | None ->
      Some (s Number ~start_index:start ~end_index:(lexer.current_index - 1))

let is_octal_digit c =
  Char.is_digit c && Int.between (Char.to_int c) ~low:0 ~high:7

let rec octal_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_octal_digit c -> octal_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      Some (s Number ~start_index:start ~end_index:(index - 1))
  | None ->
      Some (s Number ~start_index:start ~end_index:(lexer.current_index - 1))

let is_binary_digit c = Char.equal c '0' || Char.equal c '1'

let rec binary_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_binary_digit c -> binary_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      Some (s Number ~start_index:start ~end_index:(index - 1))
  | None ->
      Some (s Number ~start_index:start ~end_index:(lexer.current_index - 1))

let rec decimal_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when Char.is_digit c -> decimal_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      Some (s Number ~start_index:start ~end_index:(index - 1))
  | None ->
      Some (s Number ~start_index:start ~end_index:(lexer.current_index - 1))

let number first_char lexer start =
  if Char.equal first_char '0' then
    match peek_next_char lexer with
    | Some ('x', _) | Some ('X', _) ->
        (* Account for the `x` *)
        shift_forward lexer;
        Ok (hex_number lexer start)
    | Some ('o', _) | Some ('O', _) ->
        shift_forward lexer;
        Ok (octal_number lexer start)
    | Some ('b', _) | Some ('B', _) ->
        shift_forward lexer;
        Ok (binary_number lexer start)
    | Some (c, _) when Char.is_digit c ->
        shift_back lexer;
        Ok (decimal_number lexer start)
    | Some _ | None -> Ok (Some (s Number ~start_index:start ~end_index:start))
  else Ok (decimal_number lexer start)

(* TODO: Handle escape characters *)
let char_literal lexer =
  shift_forward lexer;
  match get_next_char lexer with
  | Some (c, _) when Char.equal c '\'' ->
      Ok
        (Some
           (s Char ~start_index:lexer.current_index
              ~end_index:(lexer.current_index - 1)))
  | Some (c, index) ->
      let message =
        Diagnostic.Message.create
          (Stdlib.Format.sprintf
             "expected `'` to delimit a character literal, instead found `%c`" c)
      in
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:(range (get_source lexer) index (index + 1))
                  message;
              ]
            ~code:InvalidCharacter Error "Unexpected character found")
  | None ->
      let message =
        Diagnostic.Message.create
          "expected `'`, instead found the end of the file"
      in
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:
                    (range (get_source lexer) (lexer.current_index - 1)
                       lexer.current_index)
                  message;
              ]
            ~code:InvalidCharacter Error "Unexpected character found")

let rec get_next lexer =
  match get_next_char lexer with
  | Some (char, index) -> (
      match char with
      | '+' -> Ok (Some (s Plus ~start_index:index ~end_index:index))
      | '-' -> Ok (Some (s Minus ~start_index:index ~end_index:index))
      | ' ' | '\n' | '\t' | '\r' -> get_next lexer
      | '\'' -> char_literal lexer
      | c when Char.is_alpha c || Char.equal c '_' || is_unicode_char c ->
          Ok (ident lexer index)
      | c when Char.is_digit c -> number c lexer index
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

let string_of_token = function
  | Plus -> "+"
  | Minus -> "-"
  | Ident -> "<ident>"
  | Number -> "<number>"
  | Char -> "<char>"

let print_error error =
  Stdlib.Format.printf "%a@." Grace_ansi_renderer.(pp_diagnostic ()) error
