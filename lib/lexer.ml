open Core
open Grace
open Span

type code = InvalidCharacter
type token = Plus | Minus | Ident | Number
type error = code Grace.Diagnostic.t
type t = { mutable current_index : int; content : string; file_path : string }

let create content file_path = { current_index = 0; content; file_path }

let range source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)

let get_source lexer = `File lexer.file_path


let shift_forward lexer =
  lexer.current_index <- lexer.current_index + 1;
  ()


let shift_back lexer =
  lexer.current_index <- lexer.current_index - 1;
  ()

let peek_next_char lexer = 
  if Int.equal lexer.current_index (String.length lexer.content) then None
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
  Int.between code ~low:0xdf ~high:0xf6 ||
   Int.between code ~low:0xf8 ~high:0xff ||
    Int.equal code 0x153 ||
     Int.equal code 0x161 ||
      Int.equal code 0x17e ||
      Int.between code ~low:0xc0 ~high:0xd6 ||
      Int.between code ~low:0xd8 ~high:0xde ||
      Int.equal code 0x152 ||
      Int.equal code 0x160 ||
      Int.equal code 0x17d ||
      Int.equal code 0x178 ||
      Int.equal code 0x1e9e
      

let rec ident lexer start =
  match get_next_char lexer with
  | Some (c, _) when Char.is_alphanum c || Char.equal c '_' || Char.equal c '\'' || is_unicode_char c -> ident lexer start
  | Some (_, index) -> (
        (* Don't forget to move index back so we can lex the next token correctly *)
        shift_back lexer;
        Some (s Ident ~start_index:start ~end_index:index))
  | None when Int.(<>) start lexer.current_index ->
      Some (s Ident ~start_index:start ~end_index:lexer.current_index)
  | None -> None

let hex_number _lexer start = Ok( Some ( s Number ~start_index: start ~end_index:start))
let octal_number _lexer start = Ok( Some ( s Number ~start_index: start ~end_index:start))
let binary_number _lexer start = Ok( Some ( s Number ~start_index: start ~end_index:start))
let decimal_number _lexer start = Ok( Some ( s Number ~start_index: start ~end_index:start))

let number first_char lexer start = 
    if Char.equal first_char '0' then      
      match peek_next_char lexer with
      | Some ('x', _) | Some('X', _) -> hex_number lexer start
      | Some ('o', _) | Some('O', _) -> octal_number lexer start
      | Some ('b', _) | Some('B', _) -> binary_number lexer start
      | Some(c, _) when Char.is_digit c -> decimal_number lexer start
      | Some(_) | None -> Ok (Some (s Number ~start_index:start ~end_index:start))
  else 
    decimal_number lexer start

let rec get_next lexer =
  match get_next_char lexer with
  | Some (char, index) -> (
      match char with
      | '+' -> Ok (Some (s Plus ~start_index:index ~end_index:index))
      | '-' -> Ok (Some (s Minus ~start_index:index ~end_index:index))
      | ' ' | '\n' | '\t' | '\r' -> get_next lexer
      (* TODO: Add all the unicode edgecases *)
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

let string_of_token = function Plus -> "+" | Minus -> "-" | Ident -> "<ident>" | Number -> "<number>"

let print_error error =
  Stdlib.Format.printf "%a@." Grace_ansi_renderer.(pp_diagnostic ()) error
