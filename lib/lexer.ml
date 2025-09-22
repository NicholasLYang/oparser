open Base
open Grace
open Span
open Token

type code = InvalidCharacter | EmptyCharacterLiteral
type error = code Grace.Diagnostic.t
type t = { mutable current_index : int; content : string; source : Source.t; mutable at_line_start : bool }

let create content source = { current_index = 0; content; source; at_line_start = true }

let range source start stop =
  Range.create ~source (Byte_index.of_int start) (Byte_index.of_int stop)

let get_source lexer = lexer.source

(* Helper functions for error creation *)
let create_diagnostic_error ~source ~start_index ~end_index ~message ~code =
  Diagnostic.(
    createf
      ~labels:
        [
          Label.primary
            ~range:(range source start_index end_index)
            (Diagnostic.Message.create message);
        ]
      ~code Error "%s" message)

let create_point_error ~lexer ~index ~message ~code =
  create_diagnostic_error ~source:(get_source lexer) ~start_index:index ~end_index:(index + 1) ~message ~code

let create_range_error ~lexer ~start_index ~end_index ~message ~code =
  create_diagnostic_error ~source:(get_source lexer) ~start_index ~end_index ~message ~code

let create_current_pos_error ~lexer ~message ~code =
  create_diagnostic_error ~source:(get_source lexer) ~start_index:lexer.current_index ~end_index:lexer.current_index ~message ~code

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

let match_while lexer ~cond =
  let rec match_while_aux () =
    match peek_next_char lexer with
    | Some (c, _) when cond c ->
        shift_forward lexer;
        match_while_aux ()
    | Some (_, index) -> index
    | None -> lexer.current_index
  in
  match_while_aux ()

let is_unicode_char c =
  let code = Char.to_int c in
  Int.between code ~low:0xdf ~high:0xf6
  || Int.between code ~low:0xf8 ~high:0xff
  || Int.equal code 0x153 || Int.equal code 0x161 || Int.equal code 0x17e
  || Int.between code ~low:0xc0 ~high:0xd6
  || Int.between code ~low:0xd8 ~high:0xde
  || Int.equal code 0x152 || Int.equal code 0x160 || Int.equal code 0x17d
  || Int.equal code 0x178 || Int.equal code 0x1e9e

let ident lexer start =
  let end_index =
    match_while lexer ~cond:(fun c ->
        Char.is_alphanum c || Char.equal c '_' || Char.equal c '\''
        || is_unicode_char c)
  in
  let content = String.sub lexer.content ~pos:start ~len:(end_index - start) in
  match keyword_of_string content with
  | Some token -> s token ~start_index:start ~end_index
  | None -> s (Ident content) ~start_index:start ~end_index

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
      let message = "Comment begins here and is not terminated" in
      Error (create_range_error ~lexer ~start_index ~end_index:lexer.current_index ~message ~code:InvalidCharacter)

let make_number lexer ~start_index ~end_index suffix =
  let content =
    String.sub lexer.content ~pos:start_index ~len:(end_index - start_index + 1)
  in
  let number =
    match suffix with
    | `Int -> Int.of_string_opt content |> Option.map ~f:(fun n -> Int n)
    | `Int32 -> Int32.of_string_opt content |> Option.map ~f:(fun n -> Int32 n)
    | `Int64 -> Int64.of_string_opt content |> Option.map ~f:(fun n -> Int64 n)
    | `NativeInt ->
        Nativeint.of_string_opt content |> Option.map ~f:(fun n -> NativeInt n)
    | `Float -> Float.of_string_opt content |> Option.map ~f:(fun f -> Float f)
  in
  match number with
  | Some number -> Ok (s (Number number) ~start_index ~end_index)
  | None ->
      Error (create_current_pos_error ~lexer ~message:("Invalid number " ^ content) ~code:InvalidCharacter)

(* Consolidated digit checking functions *)
let is_hex_digit c =
  Char.is_digit c
  || Char.between c ~high:'f' ~low:'a'
  || Char.between c ~high:'F' ~low:'A'
  || Char.equal c '_'

(* Consolidated hex digit parsing *)
let read_hex_digit c =
  if Char.is_digit c then Char.get_digit_exn c
  else
    let c = Char.lowercase c in
    10 + (Char.to_int c - Char.to_int 'a')

let rec read_hex_escape_n lexer count acc =
  if count <= 0 then acc
  else
    match peek_next_char lexer with
    | Some (c, _) when is_hex_digit c && not (Char.equal c '_') ->
        shift_forward lexer;
        read_hex_escape_n lexer (count - 1) ((acc * 16) + read_hex_digit c)
    | _ -> acc

let number_suffix lexer =
  match get_next_char lexer with
  | Some ('l', _) -> `Int32
  | Some ('L', _) -> `Int64
  | Some ('n', _) -> `NativeInt
  | Some (_, _) ->
      shift_back lexer;
      `Int
  | None -> `Int

(* Consolidated number parsing logic *)
let rec parse_number_with_predicate lexer start predicate =
  match get_next_char lexer with
  | Some (c, _) when predicate c -> parse_number_with_predicate lexer start predicate
  | Some (_, index) ->
      shift_back lexer;
      let suffix = number_suffix lexer in
      make_number lexer ~start_index:start ~end_index:(index - 1) suffix
  | None ->
      make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1) `Int

let hex_number lexer start = parse_number_with_predicate lexer start is_hex_digit

let check_first_digit lexer =
  match peek_next_char lexer with
  | Some ('_', index) ->
      let message = "Numbers cannot start with `_`" in
      Error (create_point_error ~lexer ~index ~message ~code:InvalidCharacter)
  | _ -> Ok ()

let zero = Char.to_int '0'

let is_octal_digit c =
  Int.between (Char.to_int c) ~low:zero ~high:(zero + 7) || Char.equal c '_'

let octal_number lexer start = parse_number_with_predicate lexer start is_octal_digit

let is_binary_digit c = Char.equal c '0' || Char.equal c '1' || Char.equal c '_'

let binary_number lexer start = parse_number_with_predicate lexer start is_binary_digit

let is_decimal_digit c = Char.is_digit c || Char.equal c '_'

(* Enhanced decimal number parsing that handles both integers and floats *)
let decimal_number lexer start =
  (* First parse the integer part *)
  let rec parse_integer_part () =
    match get_next_char lexer with
    | Some (c, _) when is_decimal_digit c -> parse_integer_part ()
    | Some ('.', _) ->
        (* Check if this is really a float (not followed by another .) *)
        (match peek_next_char lexer with
        | Some ('.', _) ->
            (* This is .. so backtrack and treat as integer *)
            shift_back lexer;
            shift_back lexer;
            let suffix = number_suffix lexer in
            make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1) suffix
        | Some (c, _) when Char.is_digit c ->
            (* This is a float - parse fractional part *)
            parse_fractional_part ()
        | _ ->
            (* Dot not followed by digit, treat as integer *)
            shift_back lexer;
            let suffix = number_suffix lexer in
            make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1) suffix)
    | Some (c, _) when Char.equal c 'e' || Char.equal c 'E' ->
        (* Parse exponent *)
        parse_exponent ()
    | Some (_, index) ->
        shift_back lexer;
        let suffix = number_suffix lexer in
        make_number lexer ~start_index:start ~end_index:(index - 1) suffix
    | None ->
        make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1) `Int
  and parse_fractional_part () =
    (* Parse digits after decimal point *)
    let rec parse_frac_digits () =
      match get_next_char lexer with
      | Some (c, _) when Char.is_digit c -> parse_frac_digits ()
      | Some (c, _) when Char.equal c 'e' || Char.equal c 'E' ->
          parse_exponent ()
      | Some (_, _) ->
          shift_back lexer;
          make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1) `Float
      | None ->
          make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1) `Float
    in
    parse_frac_digits ()
  and parse_exponent () =
    (* Parse optional +/- *)
    (match get_next_char lexer with
    | Some ('+', _) | Some ('-', _) -> ()
    | Some (_, _) -> shift_back lexer
    | None -> ());
    (* Parse exponent digits *)
    let rec parse_exp_digits () =
      match get_next_char lexer with
      | Some (c, _) when Char.is_digit c -> parse_exp_digits ()
      | Some (_, _) ->
          shift_back lexer;
          make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1) `Float
      | None ->
          make_number lexer ~start_index:start ~end_index:(lexer.current_index - 1) `Float
    in
    parse_exp_digits ()
  in
  parse_integer_part ()

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

(* Consolidated escape parsing functions *)
let rec read_escape_with_base lexer count acc base digit_predicate digit_converter =
  if count <= 0 then acc
  else
    match peek_next_char lexer with
    | Some (c, _) when digit_predicate c ->
        shift_forward lexer;
        read_escape_with_base lexer (count - 1) ((acc * base) + digit_converter c) base digit_predicate digit_converter
    | _ -> acc

let read_decimal_escape lexer count acc =
  read_escape_with_base lexer count acc 10 Char.is_digit Char.get_digit_exn


let is_octal_digit_strict c = Char.is_digit c && Char.get_digit_exn c < 8

let read_octal_escape lexer count acc =
  read_escape_with_base lexer count acc 8 is_octal_digit_strict Char.get_digit_exn

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
      let c = read_hex_escape_n lexer 2 0 in
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
      let message = "character literal cannot be empty" in
      Error (create_point_error ~lexer ~index ~message ~code:EmptyCharacterLiteral)
  | _ ->
      (match get_next_char lexer with
      | Some (c, _) -> Ok c
      | None ->
          let message = "expected character literal, instead found the end of the file" in
          Error (create_current_pos_error ~lexer ~message ~code:InvalidCharacter))
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
      let message = "Invalid hex digit in unicode escape sequence" in
      Error (create_point_error ~lexer ~index ~message ~code:InvalidCharacter)
  | None ->
      let message = "Unterminated unicode escape sequence" in
      Error (create_current_pos_error ~lexer ~message ~code:InvalidCharacter)

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
                   let code = Int.of_string ("0x" ^ hex) in
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

(* Parse quoted-string-id: sequence of lowercase letters and underscores *)
let parse_quoted_string_id lexer =
  let rec consume_id acc =
    match peek_next_char lexer with
    | Some (c, _) when Char.is_lowercase c || Char.equal c '_' ->
        shift_forward lexer;
        consume_id (acc ^ String.make 1 c)
    | _ -> acc
  in
  consume_id ""

(* Try to parse quoted string literal with { quoted-string-id | ... | quoted-string-id } syntax *)
let try_quoted_string_literal lexer start_index =
  (* Save current position to restore if this isn't a quoted string *)
  let saved_index = lexer.current_index in
  let quoted_id = parse_quoted_string_id lexer in
  match peek_next_char lexer with
  | Some ('|', _) ->
      shift_forward lexer;
      (* Now consume content until we find | quoted_id } *)
      let closing_delimiter = "|" ^ quoted_id ^ "}" in
      let delimiter_len = String.length closing_delimiter in
      let rec consume_quoted_string buf =
        (* Check if we're at the closing delimiter first *)
        let remaining_len = String.length lexer.content - lexer.current_index in
        if remaining_len >= delimiter_len then (
          let remaining_content = String.sub lexer.content ~pos:lexer.current_index ~len:delimiter_len in
          if String.equal remaining_content closing_delimiter then (
            (* Found closing delimiter, consume it and finish *)
            for _ = 1 to delimiter_len do
              match get_next_char lexer with
              | Some _ -> ()
              | None -> ()
            done;
            Ok (Some (s (QuotedString (Buffer.contents buf)) ~start_index ~end_index:(lexer.current_index - 1))))
          else (
            (* Not the closing delimiter, consume one character *)
            match get_next_char lexer with
            | Some (c, _) ->
                Buffer.add_char buf c;
                consume_quoted_string buf
            | None ->
                Error
                  Diagnostic.(
                    createf
                      ~labels:
                        [
                          Label.primary
                            ~range:
                              (range (get_source lexer) start_index lexer.current_index)
                            (Diagnostic.Message.create "Unterminated quoted string literal");
                        ]
                      ~code:InvalidCharacter Error "Unterminated quoted string literal")))
        else (
          (* Not enough characters left for closing delimiter *)
          match get_next_char lexer with
          | Some (c, _) ->
              Buffer.add_char buf c;
              consume_quoted_string buf
          | None ->
              Error
                Diagnostic.(
                  createf
                    ~labels:
                      [
                        Label.primary
                          ~range:
                            (range (get_source lexer) start_index lexer.current_index)
                          (Diagnostic.Message.create "Unterminated quoted string literal");
                      ]
                    ~code:InvalidCharacter Error "Unterminated quoted string literal"))
      in
      consume_quoted_string (Buffer.create 64)
  | _ ->
      (* Not a quoted string, restore position and return LBrace *)
      lexer.current_index <- saved_index;
      Ok (Some (s LBrace ~start_index ~end_index:start_index))

let is_core_operator c =
  match c with
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' -> true
  | _ -> false

let is_operator_char c =
  is_core_operator c
  || match c with '%' | '<' | '!' | '.' | ':' | '?' | '~' -> true | _ -> false

let infix_symbol lexer start_index =
  let rec consume_operator () =
    match peek_next_char lexer with
    | Some (c, _) when is_operator_char c ->
        shift_forward lexer;
        consume_operator ()
    | Some (_, end_index) ->
        let content =
          String.sub lexer.content ~pos:start_index
            ~len:(end_index - start_index)
        in
        s (InfixOp content) ~start_index ~end_index
    | None ->
        let content =
          String.sub lexer.content ~pos:start_index
            ~len:(lexer.current_index - start_index)
        in
        s (InfixOp content) ~start_index ~end_index:lexer.current_index
  in
  match peek_next_char lexer with
  | Some ('#', _) -> (
      shift_forward lexer;
      (* Must have at least one more operator char after # *)
      match peek_next_char lexer with
      | Some (c, _) when is_operator_char c ->
          shift_forward lexer;
          Ok (Some (consume_operator ()))
      | _ ->
          Error
            Diagnostic.(
              createf
                ~labels:
                  Label.
                    [
                      primary
                        ~range:
                          (range (get_source lexer) start_index
                             lexer.current_index)
                        (Message.create "Expected operator character after #");
                    ]
                ~code:InvalidCharacter Error "Invalid operator"))
  | Some (c, _) when is_core_operator c || Char.equal c '%' || Char.equal c '<'
    ->
      shift_forward lexer;
      Ok (Some (consume_operator ()))
  | _ -> assert false (* Should be called only with valid start chars *)

(* We've seen a ~, so we need to check if it's a label, i.e. `~label` or an operator, i.e. `~+` *)
let label_or_operator lexer start_index ~fallback =
  match peek_next_char lexer with
  | Some (c, index) when is_operator_char c ->
      let end_index = match_while lexer ~cond:is_operator_char in
      let content =
        String.sub lexer.content ~pos:index ~len:(end_index - index)
      in
      Ok (Some (s (InfixOp content) ~start_index ~end_index))
  | Some (c, index)
    when Char.is_lowercase c || Char.equal c '_' || is_unicode_char c ->
      let end_index =
        match_while lexer ~cond:(fun c ->
            Char.is_lowercase c || Char.equal c '_' || is_unicode_char c)
      in
      let content =
        String.sub lexer.content ~pos:index ~len:(end_index - index)
      in
      Ok (Some (s (Label content) ~start_index ~end_index))
  | Some (_, index) -> Ok (Some (s fallback ~start_index ~end_index:index))
  | None -> Ok (Some (s fallback ~start_index ~end_index:lexer.current_index))

(* Parse escape sequences in line number directive strings *)
let parse_directive_string_char lexer =
  match peek_next_char lexer with
  | Some ('\\', _) ->
      shift_forward lexer;
      (match peek_next_char lexer with
      | Some (('\\' | '"' | '\'' | 'n' | 't' | 'b' | 'r' | ' ' as c), _) ->
          shift_forward lexer;
          Ok c
      | Some (c, _) when Char.is_digit c ->
          let c1 = Char.get_digit_exn c in
          shift_forward lexer;
          (match peek_next_char lexer with
          | Some (c2, _) when Char.is_digit c2 ->
              let d2 = Char.get_digit_exn c2 in
              shift_forward lexer;
              (match peek_next_char lexer with
              | Some (c3, _) when Char.is_digit c3 ->
                  let d3 = Char.get_digit_exn c3 in
                  shift_forward lexer;
                  Ok (Char.of_int_exn (c1 * 100 + d2 * 10 + d3))
              | _ -> Ok (Char.of_int_exn (c1 * 10 + d2)))
          | _ -> Ok (Char.of_int_exn c1))
      | Some ('x', _) ->
          shift_forward lexer;
          let hex1 = read_hex_escape_n lexer 2 0 in
          Ok (Char.of_int_exn hex1)
      | Some ('o', _) ->
          shift_forward lexer;
          (match peek_next_char lexer with
          | Some (c1, _) when Char.is_digit c1 && Char.get_digit_exn c1 <= 3 ->
              let d1 = Char.get_digit_exn c1 in
              shift_forward lexer;
              (match peek_next_char lexer with
              | Some (c2, _) when Char.is_digit c2 && Char.get_digit_exn c2 <= 7 ->
                  let d2 = Char.get_digit_exn c2 in
                  shift_forward lexer;
                  (match peek_next_char lexer with
                  | Some (c3, _) when Char.is_digit c3 && Char.get_digit_exn c3 <= 7 ->
                      let d3 = Char.get_digit_exn c3 in
                      shift_forward lexer;
                      Ok (Char.of_int_exn (d1 * 64 + d2 * 8 + d3))
                  | _ -> Ok (Char.of_int_exn (d1 * 8 + d2)))
              | _ -> Ok (Char.of_int_exn d1))
          | _ -> 
              Error
                Diagnostic.(
                  createf
                    ~labels:
                      [
                        Label.primary
                          ~range:(range (get_source lexer) lexer.current_index lexer.current_index)
                          (Diagnostic.Message.create "Invalid octal escape sequence");
                      ]
                    ~code:InvalidCharacter Error "Invalid octal escape sequence"))
      | Some ('u', _) ->
          shift_forward lexer;
          (match peek_next_char lexer with
          | Some ('{', _) ->
              shift_forward lexer;
              consume_hex_digits lexer ""
              |> Result.bind ~f:(fun hex ->
                     try
                       let code = Int.of_string ("0x" ^ hex) in
                       Ok (Char.of_int_exn code)
                     with _ -> 
                       Error
                         Diagnostic.(
                           createf
                             ~labels:
                               [
                                 Label.primary
                                   ~range:(range (get_source lexer) lexer.current_index lexer.current_index)
                                   (Diagnostic.Message.create "Invalid unicode escape sequence");
                               ]
                             ~code:InvalidCharacter Error "Invalid unicode escape sequence"))
          | _ -> 
              Error
                Diagnostic.(
                  createf
                    ~labels:
                      [
                        Label.primary
                          ~range:(range (get_source lexer) lexer.current_index lexer.current_index)
                          (Diagnostic.Message.create "Expected { after \\u");
                      ]
                    ~code:InvalidCharacter Error "Expected { after \\u"))
      | _ -> 
          Error
            Diagnostic.(
              createf
                ~labels:
                  [
                    Label.primary
                      ~range:(range (get_source lexer) lexer.current_index lexer.current_index)
                      (Diagnostic.Message.create "Invalid escape sequence");
                  ]
                ~code:InvalidCharacter Error "Invalid escape sequence"))
  | Some ('\n', _) ->
      shift_forward lexer;
      Ok '\n'
  | Some (c, _) ->
      shift_forward lexer;
      Ok c
  | None -> 
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:(range (get_source lexer) lexer.current_index lexer.current_index)
                  (Diagnostic.Message.create "Unexpected end of directive string");
              ]
            ~code:InvalidCharacter Error "Unexpected end of directive string")

(* Parse line number directive: # digits " string " *)
(* Parse polymorphic variant tag: `tag_name *)
let polymorphic_variant_tag lexer start_index =
  (* We've already seen the ` character *)
  let rec consume_tag_chars acc =
    match peek_next_char lexer with
    | Some (c, _) when Char.is_alphanum c || Char.equal c '_' ->
        shift_forward lexer;
        consume_tag_chars (acc ^ String.make 1 c)
    | _ -> acc
  in
  
  match peek_next_char lexer with
  | Some (c, _) when Char.is_alpha c || Char.equal c '_' ->
      let tag_name = consume_tag_chars "" in
      Ok (Some (s (PolymorphicVariantTag tag_name) ~start_index ~end_index:(lexer.current_index - 1)))
  | _ ->
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:(range (get_source lexer) start_index lexer.current_index)
                  (Diagnostic.Message.create "Expected tag name after `");
              ]
            ~code:InvalidCharacter Error "Expected tag name after `")

let parse_linenum_directive lexer start_index =
  (* We've already seen the # character *)
  
  (* Parse digits (line number) *)
  let rec consume_digits () =
    match peek_next_char lexer with
    | Some (c, _) when Char.is_digit c ->
        shift_forward lexer;
        consume_digits ()
    | _ -> ()
  in
  
  (* Must have at least one digit *)
  match peek_next_char lexer with
  | Some (c, _) when Char.is_digit c ->
      consume_digits ();
      
      (* Expect a space *)
      (match peek_next_char lexer with
      | Some (' ', _) ->
          shift_forward lexer;
          
          (* Expect opening quote *)
          (match peek_next_char lexer with
          | Some ('"', _) ->
              shift_forward lexer;
              
              (* Parse string content until closing quote *)
              let rec consume_string_content () =
                match peek_next_char lexer with
                | Some ('"', _) ->
                    shift_forward lexer;
                    Ok ()
                | Some ('\\', _) ->
                    parse_directive_string_char lexer
                    |> Result.bind ~f:(fun _ -> consume_string_content ())
                | Some ('\n', _) ->
                    shift_forward lexer;
                    (* Handle \ newline { space | tab } continuation *)
                    let rec skip_line_whitespace () =
                      match peek_next_char lexer with
                      | Some (' ', _) | Some ('\t', _) ->
                          shift_forward lexer;
                          skip_line_whitespace ()
                      | _ -> ()
                    in
                    skip_line_whitespace ();
                    consume_string_content ()
                | Some (_, _) ->
                    shift_forward lexer;
                    consume_string_content ()
                | None ->
                    Error
                      Diagnostic.(
                        createf
                          ~labels:
                            [
                              Label.primary
                                ~range:(range (get_source lexer) start_index lexer.current_index)
                                (Diagnostic.Message.create "Unterminated line number directive string");
                            ]
                          ~code:InvalidCharacter Error "Unterminated line number directive string")
              in
              consume_string_content ()
          | _ -> 
              Error
                Diagnostic.(
                  createf
                    ~labels:
                      [
                        Label.primary
                          ~range:(range (get_source lexer) lexer.current_index lexer.current_index)
                          (Diagnostic.Message.create "Expected opening quote in line number directive");
                      ]
                    ~code:InvalidCharacter Error "Expected opening quote in line number directive"))
      | _ -> 
          Error
            Diagnostic.(
              createf
                ~labels:
                  [
                    Label.primary
                      ~range:(range (get_source lexer) lexer.current_index lexer.current_index)
                      (Diagnostic.Message.create "Expected space after line number in directive");
                  ]
                ~code:InvalidCharacter Error "Expected space after line number in directive"))
  | _ -> 
      Error
        Diagnostic.(
          createf
            ~labels:
              [
                Label.primary
                  ~range:(range (get_source lexer) lexer.current_index lexer.current_index)
                  (Diagnostic.Message.create "Expected digits after # in line number directive");
              ]
            ~code:InvalidCharacter Error "Expected digits after # in line number directive")

let rec get_next lexer =
  match get_next_char lexer with
  | Some (char, index) -> (
      match char with
      | '(' -> (
          lexer.at_line_start <- false;
          match peek_next_char lexer with
          | Some ('*', _) ->
              shift_forward lexer;
              comment lexer index 1 |> Result.bind ~f:(fun () -> get_next lexer)
          | _ -> Ok (Some (s LParen ~start_index:index ~end_index:index)))
      | ')' -> 
          lexer.at_line_start <- false;
          Ok (Some (s RParen ~start_index:index ~end_index:index))
      | '[' -> 
          lexer.at_line_start <- false;
          Ok (Some (s LBracket ~start_index:index ~end_index:index))
      | ']' -> 
          lexer.at_line_start <- false;
          Ok (Some (s RBracket ~start_index:index ~end_index:index))
      | '}' -> 
          lexer.at_line_start <- false;
          Ok (Some (s RBrace ~start_index:index ~end_index:index))
      | '|' -> 
          lexer.at_line_start <- false;
          Ok (Some (s Bar ~start_index:index ~end_index:index))
      | '+' -> 
          lexer.at_line_start <- false;
          Ok (Some (s Plus ~start_index:index ~end_index:index))
      | ' ' | '\t' | '\r' -> 
          lexer.at_line_start <- false;
          get_next lexer
      | '\n' -> 
          lexer.at_line_start <- true;
          get_next lexer
      | '\'' -> 
          lexer.at_line_start <- false;
          char_literal lexer index
      | '"' -> 
          lexer.at_line_start <- false;
          string_literal lexer index
      | '{' -> 
          lexer.at_line_start <- false;
          try_quoted_string_literal lexer index
      | '~' -> 
          lexer.at_line_start <- false;
          label_or_operator lexer index ~fallback:Tilde
      | '?' -> 
          lexer.at_line_start <- false;
          label_or_operator lexer index ~fallback:Question
      | '#' -> 
          if lexer.at_line_start then (
            (* Parse line number directive and treat as whitespace *)
            parse_linenum_directive lexer index
            |> Result.bind ~f:(fun () -> get_next lexer)
          ) else (
            (* Hash token for method calls *)
            lexer.at_line_start <- false;
            Ok (Some (s Hash ~start_index:index ~end_index:index))
          )
      | '&' -> (
          lexer.at_line_start <- false;
          match peek_next_char lexer with
          | Some ('&', _) ->
              shift_forward lexer;
              Ok (Some (s AndAnd ~start_index:index ~end_index:index))
          | _ -> Ok (Some (s And ~start_index:index ~end_index:index)))
      | ',' -> 
          lexer.at_line_start <- false;
          Ok (Some (s Comma ~start_index:index ~end_index:index))
      | '-' -> (
          lexer.at_line_start <- false;
          match peek_next_char lexer with
          | Some ('.', _) ->
              shift_forward lexer;
              Ok (Some (s MinusDot ~start_index:index ~end_index:(lexer.current_index - 1)))
          | Some ('>', _) ->
              shift_forward lexer;
              Ok (Some (s RightArrow ~start_index:index ~end_index:(lexer.current_index - 1)))
          | _ -> Ok (Some (s Minus ~start_index:index ~end_index:index)))
      | '.' -> (
          lexer.at_line_start <- false;
          match peek_next_char lexer with
          | Some ('.', _) ->
              shift_forward lexer;
              Ok (Some (s DotDot ~start_index:index ~end_index:index))
          | Some ('~', _) ->
              shift_forward lexer;
              Ok (Some (s DotTilde ~start_index:index ~end_index:index))
          | Some ('(', _) ->
              shift_forward lexer;
              Ok (Some (s DotLParen ~start_index:index ~end_index:index))
          | Some ('[', _) ->
              shift_forward lexer;
              Ok (Some (s DotLBracket ~start_index:index ~end_index:index))
          | _ -> Ok (Some (s Dot ~start_index:index ~end_index:index)))
      | ':' -> (
          lexer.at_line_start <- false;
          match peek_next_char lexer with
          | Some (':', _) ->
              shift_forward lexer;
              Ok (Some (s ColonColon ~start_index:index ~end_index:index))
          | Some ('=', _) ->
              shift_forward lexer;
              Ok (Some (s ColonEq ~start_index:index ~end_index:index))
          | Some ('>', _) ->
              shift_forward lexer;
              Ok (Some (s ColonGt ~start_index:index ~end_index:index))
          | _ ->
              Ok (Some (s Colon ~start_index:index ~end_index:index)))
      | ';' -> (
          lexer.at_line_start <- false;
          match peek_next_char lexer with
          | Some (';', _) ->
              shift_forward lexer;
              Ok
                (Some (s SemicolonSemicolon ~start_index:index ~end_index:index))
          | _ -> Ok (Some (s Semicolon ~start_index:index ~end_index:index)))
      | c when is_core_operator c || Char.equal c '%' || Char.equal c '<' ->
          lexer.at_line_start <- false;
          shift_back lexer;
          infix_symbol lexer index
      | c when Char.is_alpha c || Char.equal c '_' || is_unicode_char c ->
          lexer.at_line_start <- false;
          Ok (Some (ident lexer index))
      | c when Char.is_digit c ->
          lexer.at_line_start <- false;
          number c lexer index |> Result.map ~f:(fun token -> Some token)
      | '`' ->
          lexer.at_line_start <- false;
          polymorphic_variant_tag lexer index
      | c ->
          lexer.at_line_start <- false;
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
