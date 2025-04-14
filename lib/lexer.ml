open Core
open Grace
open Span

type code = InvalidCharacter | EmptyCharacterLiteral
type token = Plus | Minus | Ident | Number of int | Char
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
  if Int.( >= ) lexer.current_index (String.length lexer.content) then None
  else
    let char = String.get lexer.content lexer.current_index in
    let index = lexer.current_index in
    shift_forward lexer;
    Some (char, index)

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

let make_number lexer ~start_index ~end_index =
  let content =
    String.sub lexer.content ~pos:start_index ~len:(end_index - start_index + 1)
  in
  match int_of_string_opt content with
  | Some number -> Ok (s (Number number) ~start_index ~end_index)
  | None ->
      Error
        Diagnostic.(
          createf ~labels:[] ~code:InvalidCharacter Error "Invalid number %s"
            content)

let is_hex_digit c =
  Char.is_digit c
  || Char.between c ~high:'f' ~low:'a'
  || Char.between c ~high:'F' ~low:'A'

let rec hex_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_hex_digit c -> hex_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      make_number lexer ~start_index:start ~end_index:(index - 1)
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1)

let zero = Char.to_int '0'
let is_octal_digit c = Int.between (Char.to_int c) ~low:zero ~high:(zero + 7)

let rec octal_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_octal_digit c -> octal_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      make_number lexer ~start_index:start ~end_index:(index - 1)
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1)

let is_binary_digit c = Char.equal c '0' || Char.equal c '1'

let rec binary_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_binary_digit c -> binary_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      make_number lexer ~start_index:start ~end_index:(index - 1)
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1)

let rec decimal_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when Char.is_digit c -> decimal_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      make_number lexer ~start_index:start ~end_index:(index - 1)
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1)

let number first_char lexer start =
  if Char.equal first_char '0' then
    match peek_next_char lexer with
    | Some ('x', _) | Some ('X', _) ->
        (* Account for the `x` *)
        shift_forward lexer;
        hex_number lexer start
    | Some ('o', _) | Some ('O', _) ->
        shift_forward lexer;
        octal_number lexer start
    | Some ('b', _) | Some ('B', _) ->
        shift_forward lexer;
        binary_number lexer start
    | Some (c, _) when Char.is_digit c ->
        shift_back lexer;
        decimal_number lexer start
    | Some _ | None -> make_number lexer ~start_index:start ~end_index:start
  else (
    shift_back lexer;
    decimal_number lexer start)

let rec read_decimal_escape lexer count acc =
  if count <= 0 then acc
  else
    match peek_next_char lexer with
    | Some (c, _) when Char.is_digit c ->
        shift_forward lexer;
        read_decimal_escape lexer (count - 1) ((acc * 10) + Char.get_digit_exn c)
    | _ -> acc

let rec read_hex_escape lexer count acc =
  if count <= 0 then acc
  else
    match peek_next_char lexer with
    | Some (c, _)
      when Char.is_digit c
           || (Char.is_alpha c && String.contains "abcdefABCDEF" c) ->
        shift_forward lexer;
        let digit =
          if Char.is_digit c then Char.get_digit_exn c
          else
            let c = Char.lowercase c in
            10 + (Char.to_int c - Char.to_int 'a')
        in
        read_hex_escape lexer (count - 1) ((acc * 16) + digit)
    | _ -> acc

let rec read_octal_escape lexer count acc =
  if count <= 0 then acc
  else
    match peek_next_char lexer with
    | Some (c, _) when Char.is_digit c && Char.get_digit_exn c < 8 ->
        shift_forward lexer;
        read_octal_escape lexer (count - 1) ((acc * 8) + Char.get_digit_exn c)
    | _ -> acc

let char_escape lexer =
  match peek_next_char lexer with
  | Some ('\\', _)
  | Some ('"', _)
  | Some ('\'', _)
  | Some ('n', _)
  | Some ('t', _)
  | Some ('b', _)
  | Some ('r', _)
  | Some (' ', _) ->
      shift_forward lexer;
      Ok ()
  | Some ('x', _) ->
      shift_forward lexer;
      let _ = read_hex_escape lexer 2 0 in
      Ok ()
  | Some ('o', _) ->
      shift_forward lexer;
      let _ = read_octal_escape lexer 3 0 in
      Ok ()
  | Some (c, _) when Char.is_digit c ->
      let _ = read_decimal_escape lexer 3 0 in
      Ok ()
  | Some (c, index) ->
      let message =
        Diagnostic.Message.create
          (Stdlib.Format.sprintf "invalid escape sequence '\\%c'" c)
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
          "Expected a character escape sequence, found the end of the file"
      in
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:
                    (range (get_source lexer) lexer.current_index
                       lexer.current_index)
                  message;
              ]
            ~code:InvalidCharacter Error "Invalid character escape")

let char_literal lexer =
  match peek_next_char lexer with
  | Some ('\\', _) -> (
      match char_escape lexer with
      | Ok () ->
          Ok
            (Some
               (s Char ~start_index:lexer.current_index
                  ~end_index:(lexer.current_index - 1)))
      | Error error -> Error error)
  | Some ('\'', _) ->
      Error
        Diagnostic.(
          createf ~labels:[] ~code:EmptyCharacterLiteral Error
            "Empty character literal, did you forget to include a character?")
  | _ -> (
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
                 "expected `'` to delimit a character literal, instead found \
                  `%c`"
                 c)
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
                ~code:InvalidCharacter Error "Unexpected character found"))

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
      | c when Char.is_digit c ->
          number c lexer index |> Result.map ~f:(fun token -> Some token)
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
  | Number number -> sprintf "<number> %d" number
  | Char -> "<char>"

let print_error error =
  Stdlib.Format.printf "%a@." Grace_ansi_renderer.(pp_diagnostic ()) error
