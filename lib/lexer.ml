open Core
open Grace
open Span
open Token

type code = InvalidCharacter | EmptyCharacterLiteral
type error = code Grace.Diagnostic.t
type t = { mutable current_index : int; content : string; source : Source.t }

let create content source = { current_index = 0; content; source }

let range source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)

let get_source lexer = lexer.source

(* Moves the current index forward by 1. 
  
  NOTE: should not be used unless you are peeking
  at the next character, otherwise you might be at
  the end of the file *)
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
  | Some (_, index) -> (
      (* Don't forget to move index back so we can lex the next token correctly *)
      shift_back lexer;
      let content = String.sub lexer.content ~pos:start ~len:(index - start) in
      match keyword_of_string content with
      | Some token -> Some (s token ~start_index:start ~end_index:index)
      | None -> Some (s (Ident content) ~start_index:start ~end_index:index))
  | None when Int.( <> ) start lexer.current_index -> (
      let content =
        String.sub lexer.content ~pos:start ~len:(lexer.current_index - start)
      in
      match keyword_of_string content with
      | Some token ->
          Some (s token ~start_index:start ~end_index:(lexer.current_index - 1))
      | None ->
          Some
            (s (Ident content) ~start_index:start
               ~end_index:(lexer.current_index - 1)))
  | None -> None

let rec comment lexer start_index nesting_level =
  match get_next_char lexer with
  | Some ('(', _) -> (
      match peek_next_char lexer with
      | Some ('*', _) ->
          shift_forward lexer;
          comment lexer start_index (nesting_level + 1)
      | _ -> comment lexer start_index nesting_level)
  | Some ('*', _) -> (
      match peek_next_char lexer with
      | Some (')', _) ->
          shift_forward lexer;
          if Int.equal nesting_level 1 then Ok ()
          else comment lexer start_index (nesting_level - 1)
      | _ -> comment lexer start_index nesting_level)
  | Some (_, _) -> comment lexer start_index nesting_level
  | None ->
      let message = Diagnostic.Message.create "Unterminated comment" in
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:
                    (range (get_source lexer) start_index lexer.current_index)
                  message;
              ]
            ~code:InvalidCharacter Error "Unterminated comment")

let make_number lexer ~start_index ~end_index suffix =
  let content =
    String.sub lexer.content ~pos:start_index ~len:(end_index - start_index + 1)
  in
  let number =
    match suffix with
    | `Int -> int_of_string_opt content |> Option.map ~f:(fun n -> Int n)
    | `Int32 -> Int32.of_string_opt content |> Option.map ~f:(fun n -> Int32 n)
    | `Int64 -> Int64.of_string_opt content |> Option.map ~f:(fun n -> Int64 n)
    | `NativeInt ->
        Nativeint.of_string_opt content |> Option.map ~f:(fun n -> NativeInt n)
  in
  match number with
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
  || Char.equal c '_'

let number_suffix lexer =
  match get_next_char lexer with
  | Some ('l', _) -> `Int32
  | Some ('L', _) -> `Int64
  | Some ('n', _) -> `NativeInt
  | Some (_, _) ->
      shift_back lexer;
      `Int
  | None -> `Int

let rec hex_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_hex_digit c -> hex_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      let suffix = number_suffix lexer in
      make_number lexer ~start_index:start ~end_index:(index - 1) suffix
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1)
        `Int

let check_first_digit lexer =
  match peek_next_char lexer with
  | Some ('_', index) ->
      let message =
        Diagnostic.Message.create "`_` found in the first position of a number"
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
            ~code:InvalidCharacter Error "Numbers cannot start with `_`")
  | _ -> Ok ()

let zero = Char.to_int '0'

let is_octal_digit c =
  Int.between (Char.to_int c) ~low:zero ~high:(zero + 7) || Char.equal c '_'

let rec octal_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_octal_digit c -> octal_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      let suffix = number_suffix lexer in
      make_number lexer ~start_index:start ~end_index:(index - 1) suffix
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1)
        `Int

let is_binary_digit c = Char.equal c '0' || Char.equal c '1' || Char.equal c '_'

let rec binary_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_binary_digit c -> binary_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      let suffix = number_suffix lexer in
      make_number lexer ~start_index:start ~end_index:(index - 1) suffix
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1)
        `Int

let is_decimal_digit c = Char.is_digit c || Char.equal c '_'

let rec decimal_number lexer start =
  match get_next_char lexer with
  | Some (c, _) when is_decimal_digit c -> decimal_number lexer start
  | Some (_, index) ->
      shift_back lexer;
      let suffix = number_suffix lexer in
      make_number lexer ~start_index:start ~end_index:(index - 1) suffix
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1)
        `Int

let number first_char lexer start =
  if Char.equal first_char '0' then (
    match peek_next_char lexer with
    | Some ('x', _) | Some ('X', _) ->
        (* Account for the `x` *)
        shift_forward lexer;
        check_first_digit lexer
        |> Result.bind ~f:(fun () -> hex_number lexer start)
    | Some ('o', _) | Some ('O', _) ->
        shift_forward lexer;
        check_first_digit lexer
        |> Result.bind ~f:(fun () -> octal_number lexer start)
    | Some ('b', _) | Some ('B', _) ->
        shift_forward lexer;
        check_first_digit lexer
        |> Result.bind ~f:(fun () -> binary_number lexer start)
    | Some (c, _) when Char.is_digit c ->
        shift_back lexer;
        decimal_number lexer start
    | Some _ ->
        shift_back lexer;
        let suffix = number_suffix lexer in
        make_number lexer ~start_index:start
          ~end_index:(lexer.current_index - 1) suffix
    | None ->
        shift_back lexer;
        make_number lexer ~start_index:start ~end_index:start `Int)
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
  | Some (('\\' as c), _)
  | Some (('"' as c), _)
  | Some (('\'' as c), _)
  | Some (('n' as c), _)
  | Some (('t' as c), _)
  | Some (('b' as c), _)
  | Some (('r' as c), _)
  | Some ((' ' as c), _) ->
      shift_forward lexer;
      Ok c
  | Some ('x', _) ->
      shift_forward lexer;
      let c = read_hex_escape lexer 2 0 in
      Ok (Char.of_int_exn c)
  | Some ('o', _) ->
      shift_forward lexer;
      let c = read_octal_escape lexer 3 0 in
      Ok (Char.of_int_exn c)
  | Some (c, _) when Char.is_digit c ->
      let c = read_decimal_escape lexer 3 0 in
      Ok (Char.of_int_exn c)
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

let end_char_literal lexer start_index char =
  match get_next_char lexer with
  | Some (c2, end_index) when Char.equal c2 '\'' ->
      Ok (Some (s (Char char) ~start_index ~end_index))
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

let char_literal lexer start_index =
  match peek_next_char lexer with
  | Some ('\\', _) ->
      shift_forward lexer;
      char_escape lexer |> Result.bind ~f:(end_char_literal lexer start_index)
  | Some ('\'', index) ->
      let message =
        Diagnostic.Message.create "character literal cannot be empty"
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
            ~code:EmptyCharacterLiteral Error
            "Empty character literal, did you forget to include a character?")
  | _ ->
      (match get_next_char lexer with
      | Some (c, _) -> Ok c
      | None ->
          let message =
            Diagnostic.Message.create
              "expected character literal, instead found the end of the file"
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
                ~code:InvalidCharacter Error "Unexpected character found"))
      |> Result.bind ~f:(end_char_literal lexer start_index)

let rec consume_hex_digits lexer acc =
  match peek_next_char lexer with
  | Some (c, _) when Char.is_hex_digit c ->
      shift_forward lexer;
      consume_hex_digits lexer (acc ^ String.make 1 c)
  | Some ('}', _) ->
      shift_forward lexer;
      Ok acc
  | Some (_, index) ->
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:(range (get_source lexer) index (index + 1))
                  (Diagnostic.Message.create
                     "Invalid hex digit in unicode escape sequence");
              ]
            ~code:InvalidCharacter Error "Invalid unicode escape sequence")
  | None ->
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:
                    (range (get_source lexer) lexer.current_index
                       lexer.current_index)
                  (Diagnostic.Message.create
                     "Unterminated unicode escape sequence");
              ]
            ~code:InvalidCharacter Error "Unterminated unicode escape sequence")

let handle_string_escape lexer start_index buf =
  match peek_next_char lexer with
  | Some ('u', _) -> (
      shift_forward lexer;
      match peek_next_char lexer with
      | Some ('{', _) ->
          shift_forward lexer;
          consume_hex_digits lexer ""
          |> Result.bind ~f:(fun hex ->
                 try
                   let code = int_of_string ("0x" ^ hex) in
                   Stdlib.Buffer.add_utf_8_uchar buf (Uchar.of_scalar_exn code);
                   Ok buf
                 with _ ->
                   Error
                     Diagnostic.(
                       createf
                         ~labels:
                           [
                             Label.primary
                               ~range:
                                 (range (get_source lexer) start_index
                                    lexer.current_index)
                               (Diagnostic.Message.create
                                  "Invalid unicode escape sequence");
                           ]
                         ~code:InvalidCharacter Error
                         "Invalid unicode escape sequence"))
      | _ ->
          Error
            Diagnostic.(
              createf
                ~labels:
                  [
                    Label.primary
                      ~range:
                        (range (get_source lexer) lexer.current_index
                           lexer.current_index)
                      (Diagnostic.Message.create "Expected { after \\u");
                  ]
                ~code:InvalidCharacter Error "Invalid unicode escape sequence"))
  | Some ('n', _) ->
      shift_forward lexer;
      Ok
        (Buffer.add_char buf '\n';
         buf)
  | Some ('r', _) ->
      shift_forward lexer;
      Ok
        (Buffer.add_char buf '\r';
         buf)
  | Some ('t', _) ->
      shift_forward lexer;
      Ok
        (Buffer.add_char buf '\t';
         buf)
  | Some ('b', _) ->
      shift_forward lexer;
      Ok
        (Buffer.add_char buf '\b';
         buf)
  | Some ('"', _) ->
      shift_forward lexer;
      Ok
        (Buffer.add_char buf '"';
         buf)
  | Some ('\\', _) ->
      shift_forward lexer;
      Ok
        (Buffer.add_char buf '\\';
         buf)
  | Some ('\n', _) ->
      shift_forward lexer;
      let rec skip_whitespace () =
        match peek_next_char lexer with
        | Some (' ', _) | Some ('\t', _) ->
            shift_forward lexer;
            skip_whitespace ()
        | _ -> ()
      in
      skip_whitespace ();
      Ok buf
  | Some (_, index) ->
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:(range (get_source lexer) index (index + 1))
                  (Diagnostic.Message.create "Invalid escape sequence");
              ]
            ~code:InvalidCharacter Error "Invalid escape sequence")
  | None ->
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:
                    (range (get_source lexer) lexer.current_index
                       lexer.current_index)
                  (Diagnostic.Message.create "Unterminated string literal");
              ]
            ~code:InvalidCharacter Error "Unterminated string literal")

let string_literal lexer start_index =
  let rec consume_string buf =
    match peek_next_char lexer with
    | Some ('"', end_index) ->
        shift_forward lexer;
        Ok (Some (s (String (Buffer.contents buf)) ~start_index ~end_index))
    | Some ('\\', _) ->
        shift_forward lexer;
        handle_string_escape lexer start_index buf
        |> Result.bind ~f:consume_string
    | Some (c, _) ->
        shift_forward lexer;
        Buffer.add_char buf c;
        consume_string buf
    | None ->
        Error
          Diagnostic.(
            createf
              ~labels:
                [
                  Label.primary
                    ~range:
                      (range (get_source lexer) start_index lexer.current_index)
                    (Diagnostic.Message.create "Unterminated string literal");
                ]
              ~code:InvalidCharacter Error "Unterminated string literal")
  in
  consume_string (Buffer.create 64)

let rec get_next lexer =
  match get_next_char lexer with
  | Some (char, index) -> (
      match char with
      | '(' -> (
          match peek_next_char lexer with
          | Some ('*', _) ->
              shift_forward lexer;
              comment lexer index 1 |> Result.bind ~f:(fun () -> get_next lexer)
          | _ -> Ok (Some (s LParen ~start_index:index ~end_index:index)))
      | ')' -> Ok (Some (s RParen ~start_index:index ~end_index:index))
      | '+' -> Ok (Some (s Plus ~start_index:index ~end_index:index))
      | '-' -> Ok (Some (s Minus ~start_index:index ~end_index:index))
      | ' ' | '\n' | '\t' | '\r' -> get_next lexer
      | '\'' -> char_literal lexer index
      | '"' -> string_literal lexer index
      | c when Char.is_alpha c || Char.equal c '_' || is_unicode_char c ->
          Ok (ident lexer index)
      | c when Char.is_digit c ->
          number c lexer index |> Result.map ~f:(fun token -> Some token)
      | c ->
          let message =
            Diagnostic.Message.create
              (* TODO: add a better message *)
              (Stdlib.Format.sprintf "expected a token, found `%c`" c)
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

let print_error error =
  Stdlib.Format.printf "%a@." Grace_ansi_renderer.(pp_diagnostic ()) error
