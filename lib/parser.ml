open Base
open Token
open Parse_tree

type 'a parse_result = ('a, string) Result.t

(* Check if token is a synchronization point *)
let is_sync_token = function
  | Semicolon | RParen | RBrace | RBracket | In | Then | Else | Done | End
  | With | Or | And | RightArrow | Comma | Dot | Gt ->
      true
  | _ -> false

(* Skip tokens until we find specific tokens *)
let rec skip_until_tokens target_tokens tokens =
  match tokens with
  | [] -> []
  | t :: rest ->
      if
        List.exists target_tokens ~f:(fun target ->
            match (t, target) with
            | Semicolon, Semicolon -> true
            | RParen, RParen -> true
            | RBrace, RBrace -> true
            | RBracket, RBracket -> true
            | In, In -> true
            | Then, Then -> true
            | Else, Else -> true
            | Done, Done -> true
            | End, End -> true
            | With, With -> true
            | Or, Or -> true
            | And, And -> true
            | RightArrow, RightArrow -> true
            | Comma, Comma -> true
            | Dot, Dot -> true
            | Gt, Gt -> true
            | _ -> false)
      then tokens
      else skip_until_tokens target_tokens rest

(* Skip until any synchronization token *)
let _skip_until_any_sync tokens =
  let rec skip = function
    | [] -> []
    | t :: rest as toks -> if is_sync_token t then toks else skip rest
  in
  skip tokens

let is_uppercase_letter c = Char.between c ~low:'A' ~high:'Z'
let is_lowercase_letter c = Char.between c ~low:'a' ~high:'z'
let is_letter c = is_uppercase_letter c || is_lowercase_letter c
let is_digit c = Char.between c ~low:'0' ~high:'9'

let is_core_operator_char = function
  | '$' | '&' | '*' | '+' | '-' | '/' | '=' | '>' | '@' | '^' | '|' -> true
  | _ -> false

let is_operator_char = function
  | '~' | '!' | '?' | '%' | '<' | ':' | '.' -> true
  | c when is_core_operator_char c -> true
  | _ -> false

let ident_char_valid = function
  | c when is_letter c -> true
  | c when is_digit c -> true
  | '_' | '\'' -> true
  | _ -> false

(* Generic identifier validation function *)
let validate_ident_with_predicate ~first_char_pred s =
  if String.is_empty s then false
  else
    let first_char = s.[0] in
    if not (first_char_pred first_char) then false
    else String.for_all s ~f:ident_char_valid

let validate_ident s =
  validate_ident_with_predicate s ~first_char_pred:(fun c ->
      is_letter c || Char.equal c '_')

let validate_capitalized_ident s =
  validate_ident_with_predicate s ~first_char_pred:is_uppercase_letter

let validate_lowercase_ident s =
  validate_ident_with_predicate s ~first_char_pred:(fun c ->
      is_lowercase_letter c || Char.equal c '_')

let validate_prefix_symbol s =
  if String.is_empty s then false
  else
    match s.[0] with
    | '!' -> String.for_all s ~f:is_operator_char
    | ('?' | '~') when String.length s > 1 ->
        String.for_all (String.drop_prefix s 1) ~f:is_operator_char
    | _ -> false

let validate_infix_symbol s = String.for_all s ~f:is_operator_char

let is_predefined_infix_op = function
  | "*" | "+" | "-" | "-." | "=" | "!=" | "<" | ">" | "or" | "||" | "&" | "&&"
  | ":=" | "mod" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" ->
      true
  | _ -> false

let validate_operator_name s =
  validate_prefix_symbol s || validate_infix_symbol s
  || is_predefined_infix_op s

let parse_ident = function
  | Ident s when validate_ident s -> Ok s
  | _ -> Error "Expected identifier"

let parse_capitalized_ident = function
  | Ident s when validate_capitalized_ident s -> Ok s
  | _ -> Error "Expected capitalized identifier"

let parse_lowercase_ident = function
  | Ident s when validate_lowercase_ident s -> Ok s
  | _ -> Error "Expected lowercase identifier"

let parse_value_name = function
  | Ident s when validate_lowercase_ident s -> Ok s
  | LParen -> Error "Parenthesized operator names not yet implemented"
  | _ -> Error "Expected value name"

let parse_operator_name = function
  | Ident s when validate_operator_name s -> Ok s
  | _ -> Error "Expected operator name"

let parse_constr_name = function
  | Ident s when validate_capitalized_ident s -> Ok s
  | _ -> Error "Expected constructor name"

let parse_tag_name = function
  | Ident s when validate_capitalized_ident s -> Ok s
  | _ -> Error "Expected tag name"

let parse_typeconstr_name = function
  | Ident s when validate_lowercase_ident s -> Ok s
  | _ -> Error "Expected type constructor name"

let parse_field_name = function
  | Ident s when validate_lowercase_ident s -> Ok s
  | _ -> Error "Expected field name"

let parse_module_name = function
  | Ident s when validate_capitalized_ident s -> Ok s
  | _ -> Error "Expected module name"

let parse_modtype_name = function
  | Ident s when validate_ident s -> Ok s
  | _ -> Error "Expected module type name"

let parse_class_name = function
  | Ident s when validate_lowercase_ident s -> Ok s
  | _ -> Error "Expected class name"

let parse_inst_var_name = function
  | Ident s when validate_lowercase_ident s -> Ok s
  | _ -> Error "Expected instance variable name"

let parse_method_name = function
  | Ident s when validate_lowercase_ident s -> Ok s
  | _ -> Error "Expected method name"

(* Token stream parser utilities *)

let peek_token = function [] -> None | t :: _ -> Some t
let consume_token = function [] -> (None, []) | t :: rest -> (Some t, rest)

let expect_dot tokens =
  match consume_token tokens with
  | Some Dot, rest -> Ok rest
  | _ -> Error "Expected '.'"

(* Error recovery versions of expect functions *)
let _expect_dot_recover tokens =
  match consume_token tokens with
  | Some Dot, rest -> Ok rest
  | _ ->
      (* Try to recover by skipping to next sync point *)
      let _ =
        skip_until_tokens
          [ Token.Dot; Token.Semicolon; Token.RParen; Token.RBrace ]
          tokens
      in
      Error "Expected '.'"

let expect_lparen tokens =
  match consume_token tokens with
  | Some LParen, rest -> Ok rest
  | _ -> Error "Expected '('"

let _expect_lparen_recover tokens =
  match consume_token tokens with
  | Some LParen, rest -> Ok rest
  | _ ->
      let _ = skip_until_tokens [ RParen; Semicolon ] tokens in
      Error "Expected '('"

let expect_rparen tokens =
  match consume_token tokens with
  | Some RParen, rest -> Ok rest
  | _ -> Error "Expected ')'"

let _expect_rparen_recover tokens =
  match consume_token tokens with
  | Some RParen, rest -> Ok rest
  | _ -> (
      (* Skip until we find a closing paren or other sync point *)
      match skip_until_tokens [ RParen; Semicolon; Comma ] tokens with
      | RParen :: rest -> Ok rest
      | _ -> Error "Expected ')'")

(* Path parsing utilities *)
let rec parse_module_path tokens =
  match consume_token tokens with
  | Some (Ident s), rest when validate_capitalized_ident s ->
      parse_module_path_continuation [ s ] rest
  | _ -> Error "Expected module name"

and parse_module_path_continuation acc tokens =
  match peek_token tokens with
  | Some Dot -> (
      match consume_token tokens with
      | Some Dot, rest -> (
          match consume_token rest with
          | Some (Ident s), rest' when validate_capitalized_ident s ->
              parse_module_path_continuation (s :: acc) rest'
          | _ ->
              Ok (List.rev acc, tokens)
              (* Don't consume the dot if next isn't module name *))
      | _ -> Ok (List.rev acc, tokens))
  | _ -> Ok (List.rev acc, tokens)

let rec parse_extended_module_name tokens =
  match consume_token tokens with
  | Some (Ident s), rest when validate_capitalized_ident s ->
      parse_extended_module_name_continuation s rest
  | _ -> Error "Expected module name"

and parse_extended_module_name_continuation base_name tokens =
  match peek_token tokens with
  | Some LParen -> (
      match expect_lparen tokens with
      | Ok rest -> (
          match parse_extended_module_path rest with
          | Ok (path, rest') -> (
              match expect_rparen rest' with
              | Ok rest'' ->
                  (* Continue parsing for more parenthesized paths *)
                  let extended_name =
                    base_name ^ "(" ^ String.concat ~sep:"." path ^ ")"
                  in
                  parse_extended_module_name_continuation extended_name rest''
              | Error err -> Error err)
          | Error err -> Error err)
      | Error err -> Error err)
  | _ -> Ok (base_name, tokens)

and parse_extended_module_path tokens =
  match parse_extended_module_name tokens with
  | Ok (name, rest) -> parse_extended_module_path_continuation [ name ] rest
  | Error e -> Error e

and parse_extended_module_path_continuation acc tokens =
  match peek_token tokens with
  | Some Dot -> (
      match consume_token tokens with
      | Some Dot, rest -> (
          match parse_extended_module_name rest with
          | Ok (name, rest') ->
              parse_extended_module_path_continuation (name :: acc) rest'
          | Error err -> Error err)
      | _ -> Ok (List.rev acc, tokens))
  | _ -> Ok (List.rev acc, tokens)

(* Helper function to check if we have a module path by looking ahead *)
let has_module_path tokens =
  match tokens with
  | Ident s1 :: Dot :: Ident _ :: _ when validate_capitalized_ident s1 -> true
  | _ -> false

(* Simpler approach: manually parse module.name patterns *)
let try_parse_with_module_path parse_name tokens =
  if has_module_path tokens then
    (* We have Module.Name pattern, parse it manually *)
    match tokens with
    | Ident module_name :: Dot :: Ident name :: rest
      when validate_capitalized_ident module_name -> (
        match parse_name (Ident name) with
        | Ok parsed_name -> Ok ((Some [ module_name ], parsed_name), rest)
        | Error err -> Error err)
    | _ -> Error "Invalid module path pattern"
  else
    (* No module path pattern detected, just parse name *)
    match consume_token tokens with
    | Some token, rest -> (
        match parse_name token with
        | Ok name -> Ok ((None, name), rest)
        | Error err -> Error err)
    | _ -> Error "Expected name"

(* Main path parsers *)
let parse_value_path tokens = try_parse_with_module_path parse_value_name tokens
let parse_constr tokens = try_parse_with_module_path parse_constr_name tokens

let parse_typeconstr tokens =
  (* Use try_parse_with_module_path but with extended_module_path *)
  match consume_token tokens with
  | Some token, rest -> (
      match parse_typeconstr_name token with
      | Ok name -> Ok ((None, name), rest)
      | Error _ -> (
          (* Try with extended module path *)
          match parse_extended_module_path tokens with
          | Ok (path, rest') -> (
              match expect_dot rest' with
              | Ok rest'' -> (
                  match consume_token rest'' with
                  | Some token', rest''' -> (
                      match parse_typeconstr_name token' with
                      | Ok name -> Ok ((Some path, name), rest''')
                      | Error err -> Error err)
                  | _ ->
                      Error "Expected type constructor name after module path")
              | Error _ -> Error "Expected '.' after module path")
          | Error _ -> Error "Expected type constructor name"))
  | _ -> Error "Expected type constructor name"

let parse_field tokens = try_parse_with_module_path parse_field_name tokens

let parse_modtype_path tokens =
  (* Use try_parse_with_module_path but with extended_module_path *)
  match consume_token tokens with
  | Some token, rest -> (
      match parse_modtype_name token with
      | Ok name -> Ok ((None, name), rest)
      | Error _ -> (
          (* Try with extended module path *)
          match parse_extended_module_path tokens with
          | Ok (path, rest') -> (
              match expect_dot rest' with
              | Ok rest'' -> (
                  match consume_token rest'' with
                  | Some token', rest''' -> (
                      match parse_modtype_name token' with
                      | Ok name -> Ok ((Some path, name), rest''')
                      | Error err -> Error err)
                  | _ -> Error "Expected module type name after module path")
              | Error _ -> Error "Expected '.' after module path")
          | Error _ -> Error "Expected module type name"))
  | _ -> Error "Expected module type name"

(* Type expression parsing *)

(* Syntax forms supported by this parser:
 *
 * Primary Types:
 * - Type variables: 'a, 'b, 'var
 * - Wildcards: _
 * - Type constructors: int, string, list
 * - Parenthesized types: (int)
 * - Object types: < method1 : int; method2 : string >, < .. >
 * - Class types: #class_name
 *
 * Type Applications:
 * - Simple application: 'a list
 * - Multi-parameter application: ('a, 'b) result
 * - Class type application: 'a #class
 *
 * Composite Types:
 * - Tuple types: int * string * bool
 * - Arrow types: int -> string
 * - Labeled arrows: label:int -> string
 * - Optional labeled arrows: ?label:int -> string
 * - Type aliases with 'as': 'a as 'x
 *
 * Polymorphic Types:
 * - Universal quantification: 'a 'b. 'a -> 'b
 * - Method types in objects: method_name : poly_typexpr
 *
 * Module-qualified names:
 * - Simple module paths: Module.type_name
 * - Extended module paths: Module(Path).type_name
 * - Nested module access: A.B.C.type_name
 *)

(* Constant parsing *)

let parse_constant = function
  | Number (Int i) -> Ok (make_integer_literal i)
  | Number (Int32 i) -> Ok (make_int32_literal i)
  | Number (Int64 i) -> Ok (make_int64_literal i)
  | Number (NativeInt i) -> Ok (make_nativeint_literal i)
  | Number (Float f) -> Ok (make_float_literal f)
  | Char c -> Ok (make_char_literal c)
  | String s -> Ok (make_string_literal s)
  | Ident name when validate_capitalized_ident name ->
      Ok (make_constructor name)
  | False -> Ok (make_false ())
  | True -> Ok (make_true ())
  | PolymorphicVariantTag tag -> Ok (make_polymorphic_variant_tag tag)
  | _ -> Error "Expected constant"

(* Parse compound constants that require multiple tokens *)
let parse_unit_constant tokens =
  match consume_token tokens with
  | Some LParen, rest -> (
      match consume_token rest with
      | Some RParen, rest' -> Ok (make_unit (), rest')
      | _ -> Error "Expected ')' to complete unit constant")
  | _ -> Error "Expected '(' to start unit constant"

let parse_begin_end_constant tokens =
  match consume_token tokens with
  | Some Begin, rest -> (
      match consume_token rest with
      | Some End, rest' -> Ok (make_begin_end (), rest')
      | _ -> Error "Expected 'end' to complete begin end constant")
  | _ -> Error "Expected 'begin' to start begin end constant"

let parse_empty_list_constant tokens =
  match consume_token tokens with
  | Some LBracket, rest -> (
      match consume_token rest with
      | Some RBracket, rest' -> Ok (make_empty_list (), rest')
      | _ -> Error "Expected ']' to complete empty list constant")
  | _ -> Error "Expected '[' to start empty list constant"

let parse_empty_array_constant tokens =
  match consume_token tokens with
  | Some LBracket, rest -> (
      match consume_token rest with
      | Some Or, rest' -> (
          match consume_token rest' with
          | Some Or, rest'' -> (
              match consume_token rest'' with
              | Some RBracket, rest''' -> Ok (make_empty_array (), rest''')
              | _ -> Error "Expected ']' to complete empty array constant")
          | _ -> Error "Expected second '|' in empty array constant")
      | _ -> Error "Expected '|' after '[' in empty array constant")
  | _ -> Error "Expected '[' to start empty array constant"

(* Parse a complete constant from token list *)
let parse_constant_tokens tokens =
  match peek_token tokens with
  | Some LParen -> parse_unit_constant tokens
  | Some Begin -> parse_begin_end_constant tokens
  | Some LBracket -> (
      (* Could be empty list [] or empty array [||] *)
      match tokens with
      | LBracket :: RBracket :: _ -> parse_empty_list_constant tokens
      | LBracket :: Or :: Or :: RBracket :: _ ->
          parse_empty_array_constant tokens
      | _ -> Error "Invalid list/array constant")
  | Some _ -> (
      match consume_token tokens with
      | Some token, rest -> (
          match parse_constant token with
          | Ok constant -> Ok (constant, rest)
          | Error err -> Error err)
      | _ -> Error "Expected constant token")
  | None -> Error "Expected constant token"

(* Pattern parsing *)

(* Parse patterns with precedence
 * Grammar hierarchy (highest to lowest precedence):
 * 1. Primary patterns (variables, constants, constructors, parentheses, lists, arrays, records)
 * 2. Constructor applications (Constructor pattern)
 * 3. Tuple patterns (pattern, pattern, ...)
 * 4. Cons patterns (pattern :: pattern)
 * 5. Alias patterns (pattern as name)
 * 6. Or patterns (pattern | pattern)
 *)

let rec parse_pattern tokens = parse_or_pattern tokens

(* Parse or patterns (lowest precedence): pattern | pattern *)
and parse_or_pattern tokens =
  match parse_alias_pattern tokens with
  | Ok (left, rest) -> parse_or_continuation left rest
  | Error e -> Error e

and parse_or_continuation left tokens =
  match peek_token tokens with
  | Some Or -> (
      match consume_token tokens with
      | Some Or, rest -> (
          match parse_or_pattern rest with
          | Ok (right, rest') -> Ok (make_or_pattern left right, rest')
          | Error err -> Error err)
      | _ -> Ok (left, tokens))
  | _ -> Ok (left, tokens)

(* Parse alias patterns: pattern as value-name *)
and parse_alias_pattern tokens =
  match parse_cons_pattern tokens with
  | Ok (p, rest) -> (
      match peek_token rest with
      | Some As -> (
          match consume_token rest with
          | Some As, rest' -> (
              match consume_token rest' with
              | Some (Ident name), rest'' when validate_lowercase_ident name ->
                  Ok (make_pattern_alias p name, rest'')
              | _ -> Error "Expected value name after 'as'")
          | _ -> Ok (p, rest))
      | _ -> Ok (p, rest))
  | Error e -> Error e

(* Parse cons patterns: pattern :: pattern *)
and parse_cons_pattern tokens =
  match parse_tuple_pattern tokens with
  | Ok (left, rest) -> parse_cons_continuation left rest
  | Error e -> Error e

and parse_cons_continuation left tokens =
  match peek_token tokens with
  | Some ColonColon -> (
      match consume_token tokens with
      | Some ColonColon, rest -> (
          match parse_cons_pattern rest with
          | Ok (right, rest') -> Ok (make_cons_pattern left right, rest')
          | Error err -> Error err)
      | _ -> Ok (left, tokens))
  | _ -> Ok (left, tokens)

(* Parse tuple patterns: pattern, pattern, ... *)
and parse_tuple_pattern tokens =
  match parse_constructor_app_pattern tokens with
  | Ok (first, rest) -> parse_tuple_continuation [ first ] rest
  | Error e -> Error e

and parse_tuple_continuation acc tokens =
  match peek_token tokens with
  | Some Comma -> (
      match consume_token tokens with
      | Some Comma, rest -> (
          match parse_constructor_app_pattern rest with
          | Ok (p, rest') -> parse_tuple_continuation (p :: acc) rest'
          | Error err -> Error err)
      | _ -> (
          if List.length acc > 1 then
            Ok (make_tuple_pattern (List.rev acc), tokens)
          else
            match acc with
            | [ single ] -> Ok (single, tokens)
            | [] -> Error "Empty tuple list"
            | _ -> Ok (make_tuple_pattern (List.rev acc), tokens)))
  | _ -> (
      if List.length acc > 1 then Ok (make_tuple_pattern (List.rev acc), tokens)
      else
        match acc with
        | [ single ] -> Ok (single, tokens)
        | [] -> Error "Empty tuple list"
        | _ -> Ok (make_tuple_pattern (List.rev acc), tokens))

(* Parse constructor application patterns: Constructor pattern *)
and parse_constructor_app_pattern tokens =
  match parse_primary_pattern tokens with
  | Ok (p, rest) -> (
      match p with
      | PatternConstructor path -> (
          (* Check if next token could be a pattern (constructor application) *)
          match peek_token rest with
          | Some (Ident _)
          | Some LParen
          | Some LBracket
          | Some LBrace
          | Some Quote -> (
              match parse_primary_pattern rest with
              | Ok (arg_pattern, rest') ->
                  Ok (make_constructor_pattern_with_arg path arg_pattern, rest')
              | Error _ -> Ok (p, rest))
          | _ -> Ok (p, rest))
      | _ -> Ok (p, rest))
  | Error e -> Error e

(* Parse primary patterns: atoms and compound structures *)
and parse_primary_pattern tokens =
  match peek_token tokens with
  | Some (Ident "_") -> (
      (* Wildcard pattern *)
      match consume_token tokens with
      | Some (Ident "_"), rest -> Ok (make_wildcard_pattern (), rest)
      | _ -> Error "Expected wildcard")
  | Some (Ident name) when validate_lowercase_ident name -> (
      (* Value name pattern *)
      match consume_token tokens with
      | Some (Ident value_name), rest ->
          Ok (make_value_name_pattern value_name, rest)
      | _ -> Error "Expected value name")
  | Some (Ident name) when validate_capitalized_ident name -> (
      (* Constructor pattern *)
      match parse_constr tokens with
      | Ok (constr_path, rest) -> Ok (make_constructor_pattern constr_path, rest)
      | Error err -> Error err)
  | Some LParen -> parse_parenthesized_pattern tokens
  | Some LBracket -> parse_list_or_array_pattern tokens
  | Some LBrace -> parse_record_pattern tokens
  | Some Quote -> parse_range_or_char_pattern tokens
  | Some Lazy -> parse_lazy_pattern tokens
  | Some Exception -> parse_exception_pattern tokens
  | _ -> (
      (* Try to parse as constant *)
      match parse_constant_tokens tokens with
      | Ok (constant, rest) -> Ok (make_pattern_constant constant, rest)
      | Error err -> Error err)

(* Parse parenthesized patterns: ( pattern ) *)
and parse_parenthesized_pattern tokens =
  match consume_token tokens with
  | Some LParen, rest -> (
      match parse_pattern rest with
      | Ok (p, rest') -> (
          match expect_rparen rest' with
          | Ok rest'' -> Ok (make_parenthesized_pattern p, rest'')
          | Error _ -> (
              (* Try to recover by finding the closing paren *)
              match skip_until_tokens [ RParen ] rest' with
              | RParen :: rest'' -> Ok (make_parenthesized_pattern p, rest'')
              | _ -> Error "Expected ')' after pattern"))
      | Error _ -> (
          (* Skip to closing paren and continue *)
          match skip_until_tokens [ RParen ] rest with
          | RParen :: _ -> Error "Invalid pattern in parentheses"
          | _ -> Error "Expected ')' in pattern"))
  | _ -> Error "Expected '('"

(* Parse list or array patterns: [pattern; ...] or [|pattern; ...|] *)
and parse_list_or_array_pattern tokens =
  match consume_token tokens with
  | Some LBracket, rest -> (
      match peek_token rest with
      | Some Or -> (
          (* Array pattern [|...|] *)
          match consume_token rest with
          | Some Or, rest' -> parse_array_pattern_contents rest'
          | _ -> Error "Expected '|' for array pattern")
      | Some RBracket -> (
          (* Empty list pattern [] *)
          match consume_token rest with
          | Some RBracket, rest' -> Ok (make_list_pattern [], rest')
          | _ -> Error "Expected ']'")
      | _ ->
          (* Non-empty list pattern *)
          parse_list_pattern_contents rest)
  | _ -> Error "Expected '['"

and parse_list_pattern_contents tokens =
  match parse_pattern tokens with
  | Ok (first, rest) -> parse_list_pattern_continuation [ first ] rest
  | Error e -> Error e

and parse_list_pattern_continuation acc tokens =
  match peek_token tokens with
  | Some Semicolon -> (
      match consume_token tokens with
      | Some Semicolon, rest -> (
          match peek_token rest with
          | Some RBracket -> (
              match consume_token rest with
              | Some RBracket, rest' ->
                  Ok (make_list_pattern (List.rev acc), rest')
              | _ -> Error "Expected ']'")
          | _ -> (
              match parse_pattern rest with
              | Ok (p, rest') ->
                  parse_list_pattern_continuation (p :: acc) rest'
              | Error _ -> (
                  (* Skip to next semicolon or closing bracket *)
                  match skip_until_tokens [ Semicolon; RBracket ] rest with
                  | Semicolon :: rest' ->
                      parse_list_pattern_continuation acc rest'
                  | RBracket :: rest' ->
                      Ok (make_list_pattern (List.rev acc), rest')
                  | _ -> Error "Expected ';' or ']' in list pattern")))
      | _ -> Error "Expected ';'")
  | Some RBracket -> (
      match consume_token tokens with
      | Some RBracket, rest -> Ok (make_list_pattern (List.rev acc), rest)
      | _ -> Error "Expected ']'")
  | _ -> Error "Expected ';' or ']'"

and parse_array_pattern_contents tokens =
  match peek_token tokens with
  | Some Or -> (
      (* Empty array [||] *)
      match consume_token tokens with
      | Some Or, rest -> (
          match expect_rbracket rest with
          | Ok rest' -> Ok (make_array_pattern [], rest')
          | Error err -> Error err)
      | _ -> Error "Expected '|'")
  | _ -> (
      (* Non-empty array *)
      match parse_pattern tokens with
      | Ok (first, rest) -> parse_array_pattern_continuation [ first ] rest
      | Error err -> Error err)

and parse_array_pattern_continuation acc tokens =
  match peek_token tokens with
  | Some Semicolon -> (
      match consume_token tokens with
      | Some Semicolon, rest -> (
          match peek_token rest with
          | Some Or -> (
              match consume_token rest with
              | Some Or, rest' -> (
                  match expect_rbracket rest' with
                  | Ok rest'' -> Ok (make_array_pattern (List.rev acc), rest'')
                  | Error err -> Error err)
              | _ -> Error "Expected '|'")
          | _ -> (
              match parse_pattern rest with
              | Ok (p, rest') ->
                  parse_array_pattern_continuation (p :: acc) rest'
              | Error err -> Error err))
      | _ -> Error "Expected ';'")
  | Some Or -> (
      match consume_token tokens with
      | Some Or, rest -> (
          match expect_rbracket rest with
          | Ok rest' -> Ok (make_array_pattern (List.rev acc), rest')
          | Error err -> Error err)
      | _ -> Error "Expected '|'")
  | _ -> Error "Expected ';' or '|'"

and expect_rbracket tokens =
  match consume_token tokens with
  | Some RBracket, rest -> Ok rest
  | _ -> Error "Expected ']'"

(* Parse record patterns: {field = pattern; ...} *)
and parse_record_pattern tokens =
  match consume_token tokens with
  | Some LBrace, rest -> (
      match peek_token rest with
      | Some RBrace -> (
          (* Empty record {} *)
          match consume_token rest with
          | Some RBrace, rest' -> Ok (make_record_pattern [] false, rest')
          | _ -> Error "Expected '}'")
      | _ -> parse_record_pattern_contents rest)
  | _ -> Error "Expected '{'"

and parse_record_pattern_contents tokens =
  match parse_record_pattern_field tokens with
  | Ok (first, rest) -> parse_record_pattern_continuation [ first ] rest
  | Error e -> Error e

and parse_record_pattern_field tokens =
  match parse_field tokens with
  | Ok (field_path, rest) -> (
      match expect_eq rest with
      | Ok rest' -> (
          match parse_pattern rest' with
          | Ok (p, rest'') -> Ok (make_record_pattern_field field_path p, rest'')
          | Error err -> Error err)
      | Error err -> Error err)
  | Error e -> Error e

and expect_eq tokens =
  match consume_token tokens with
  | Some Eq, rest -> Ok rest
  | _ -> Error "Expected '='"

and parse_record_pattern_continuation acc tokens =
  match peek_token tokens with
  | Some Semicolon -> (
      match consume_token tokens with
      | Some Semicolon, rest -> (
          match peek_token rest with
          | Some (Ident "_") -> (
              (* Record with wildcard {field = pattern; _} *)
              match consume_token rest with
              | Some (Ident "_"), rest' -> (
                  match expect_rbrace rest' with
                  | Ok rest'' ->
                      Ok (make_record_pattern (List.rev acc) true, rest'')
                  | Error _ -> (
                      (* Try to recover *)
                      match skip_until_tokens [ RBrace ] rest' with
                      | RBrace :: rest'' ->
                          Ok (make_record_pattern (List.rev acc) true, rest'')
                      | _ -> Error "Expected '}' after record wildcard"))
              | _ -> Error "Expected '_'")
          | Some RBrace -> (
              match consume_token rest with
              | Some RBrace, rest' ->
                  Ok (make_record_pattern (List.rev acc) false, rest')
              | _ -> Error "Expected '}'")
          | _ -> (
              match parse_record_pattern_field rest with
              | Ok (field, rest') ->
                  parse_record_pattern_continuation (field :: acc) rest'
              | Error _ -> (
                  (* Skip to next semicolon or closing brace *)
                  match skip_until_tokens [ Semicolon; RBrace ] rest with
                  | Semicolon :: rest' ->
                      parse_record_pattern_continuation acc rest'
                  | RBrace :: rest' ->
                      Ok (make_record_pattern (List.rev acc) false, rest')
                  | _ -> Error "Expected ';' or '}' in record pattern")))
      | _ -> Error "Expected ';'")
  | Some RBrace -> (
      match consume_token tokens with
      | Some RBrace, rest -> Ok (make_record_pattern (List.rev acc) false, rest)
      | _ -> Error "Expected '}'")
  | _ -> Error "Expected ';' or '}'"

and expect_rbrace tokens =
  match consume_token tokens with
  | Some RBrace, rest -> Ok rest
  | _ -> Error "Expected '}'"

and _expect_rbrace_recover tokens =
  match consume_token tokens with
  | Some RBrace, rest -> Ok rest
  | _ -> (
      match skip_until_tokens [ RBrace; Semicolon ] tokens with
      | RBrace :: rest -> Ok rest
      | _ -> Error "Expected '}'")

(* Parse range or character patterns: 'a'..'z' or 'c' *)
and parse_range_or_char_pattern tokens =
  match consume_token tokens with
  | Some Quote, rest -> (
      match consume_token rest with
      | Some (Char c), rest' -> (
          match peek_token rest' with
          | Some Quote -> (
              match consume_token rest' with
              | Some Quote, rest'' -> (
                  match peek_token rest'' with
                  | Some DotDot -> (
                      (* Range pattern 'a'..'z' *)
                      match consume_token rest'' with
                      | Some DotDot, rest''' -> (
                          match consume_token rest''' with
                          | Some Quote, rest'''' -> (
                              match consume_token rest'''' with
                              | Some (Char end_c), rest''''' -> (
                                  match consume_token rest''''' with
                                  | Some Quote, rest'''''' ->
                                      Ok (make_range_pattern c end_c, rest'''''')
                                  | _ -> Error "Expected closing quote")
                              | _ -> Error "Expected character")
                          | _ -> Error "Expected quote")
                      | _ -> Error "Expected '..'")
                  | _ ->
                      (* Single character pattern 'c' *)
                      Ok (make_pattern_constant (make_char_literal c), rest''))
              | _ -> Error "Expected closing quote")
          | _ -> Error "Expected closing quote")
      | _ -> Error "Expected character")
  | _ -> Error "Expected quote"

(* Parse lazy patterns: lazy pattern *)
and parse_lazy_pattern tokens =
  match consume_token tokens with
  | Some Lazy, rest -> (
      match parse_primary_pattern rest with
      | Ok (p, rest') -> Ok (make_lazy_pattern p, rest')
      | Error err -> Error err)
  | _ -> Error "Expected 'lazy'"

(* Parse exception patterns: exception pattern *)
and parse_exception_pattern tokens =
  match consume_token tokens with
  | Some Exception, rest -> (
      match parse_primary_pattern rest with
      | Ok (p, rest') -> Ok (make_exception_pattern p, rest')
      | Error err -> Error err)
  | _ -> Error "Expected 'exception'"

(* Helper function to parse comma-separated list *)
let rec parse_comma_separated_list parse_fn tokens =
  match parse_fn tokens with
  | Ok (first, rest) ->
      parse_comma_separated_continuation parse_fn [ first ] rest
  | Error e -> Error e

and parse_comma_separated_continuation parse_fn acc tokens =
  match peek_token tokens with
  | Some Comma -> (
      match consume_token tokens with
      | Some Comma, rest -> (
          match parse_fn rest with
          | Ok (item, rest') ->
              parse_comma_separated_continuation parse_fn (item :: acc) rest'
          | Error err -> Error err)
      | _ -> Ok (List.rev acc, tokens))
  | _ -> Ok (List.rev acc, tokens)

(* Parse method type: method-name : poly-typexpr
 * Syntax: method_name : typexpr | method_name : 'a 'b. typexpr
 *)
let rec parse_method_type tokens =
  match consume_token tokens with
  | Some (Ident name), rest when validate_lowercase_ident name -> (
      match expect_colon rest with
      | Ok rest' -> (
          match parse_poly_typexpr rest' with
          | Ok (poly_type, rest'') ->
              Ok (make_method_type name poly_type, rest'')
          | Error err -> Error err)
      | Error err -> Error err)
  | _ -> Error "Expected method name"

and expect_colon tokens =
  match consume_token tokens with
  | Some (InfixOp ":"), rest -> Ok rest
  | _ -> Error "Expected ':'"

(* Parse poly-typexpr: typexpr | { ' ident }+ . typexpr
 * Syntax: 
 * - Simple type: int
 * - Polymorphic type: 'a 'b. 'a -> 'b
 *)
and parse_poly_typexpr tokens =
  (* Check if we have type variables *)
  if has_type_variables tokens then parse_polymorphic_type tokens
  else
    match parse_typexpr tokens with
    | Ok (t, rest) -> Ok (make_mono_type t, rest)
    | Error e -> Error e

and has_type_variables tokens =
  match tokens with
  | Quote :: Ident _ :: Dot :: _ -> true
  | Quote :: Ident _ :: Quote :: _ -> true (* Multiple type variables *)
  | _ -> false

and parse_polymorphic_type tokens =
  match parse_type_variable_list tokens with
  | Ok (vars, rest) -> (
      match expect_dot rest with
      | Ok rest' -> (
          match parse_typexpr rest' with
          | Ok (t, rest'') -> Ok (make_poly_type vars t, rest'')
          | Error err -> Error err)
      | Error err -> Error err)
  | Error e -> Error e

and parse_type_variable_list tokens =
  match consume_token tokens with
  | Some Quote, rest -> (
      match consume_token rest with
      | Some (Ident var), rest' when validate_ident var ->
          parse_type_variable_continuation [ var ] rest'
      | _ -> Error "Expected type variable after '")
  | _ -> Error "Expected type variable"

and parse_type_variable_continuation acc tokens =
  match peek_token tokens with
  | Some Quote -> (
      match consume_token tokens with
      | Some Quote, rest -> (
          match consume_token rest with
          | Some (Ident var), rest' when validate_ident var ->
              parse_type_variable_continuation (var :: acc) rest'
          | _ -> Error "Expected type variable after '")
      | _ -> Ok (List.rev acc, tokens))
  | _ -> Ok (List.rev acc, tokens)

(* Main typexpr parser with precedence
 * Grammar hierarchy (highest to lowest precedence):
 * 1. Primary types (variables, constructors, parentheses, objects, classes)
 * 2. Type applications ('a list)
 * 3. Tuple types (int * string)
 * 4. Type aliases with 'as' ('a as 'x)
 * 5. Arrow types (int -> string, with labeled/optional variants)
 *)
and parse_typexpr tokens = parse_arrow_type tokens

(* Parse arrow types (lowest precedence): typexpr -> typexpr
 * Syntax:
 * - Simple arrow: int -> string
 * - Labeled arrow: label:int -> string  
 * - Optional labeled arrow: ?label:int -> string
 *)
and parse_arrow_type tokens =
  match parse_as_type tokens with
  | Ok (left, rest) -> parse_arrow_continuation left rest
  | Error e -> Error e

and parse_arrow_continuation left tokens =
  match peek_token tokens with
  | Some RightArrow -> (
      match consume_token tokens with
      | Some RightArrow, rest -> (
          match parse_arrow_type rest with
          | Ok (right, rest') -> Ok (make_arrow left right, rest')
          | Error err -> Error err)
      | _ -> Ok (left, tokens))
  | Some (Ident label) when is_label_like label tokens ->
      (* Handle labeled arguments *)
      parse_labeled_arrow left tokens
  | _ -> Ok (left, tokens)

and is_label_like _label tokens =
  match tokens with Ident _ :: InfixOp ":" :: _ -> true | _ -> false

and parse_labeled_arrow left tokens =
  match consume_token tokens with
  | Some (Ident label), rest -> (
      match peek_token rest with
      | Some (InfixOp ":") -> (
          match consume_token rest with
          | Some (InfixOp ":"), rest' -> (
              match parse_arrow_type rest' with
              | Ok (arg_type, rest'') -> (
                  match expect_right_arrow rest'' with
                  | Ok rest''' -> (
                      match parse_arrow_type rest''' with
                      | Ok (return_type, rest'''') ->
                          Ok (make_arrow ~label arg_type return_type, rest'''')
                      | Error err -> Error err)
                  | Error err -> Error err)
              | Error err -> Error err)
          | _ -> Error "Expected ':'")
      | Some Question -> (
          (* Optional labeled argument *)
          match consume_token rest with
          | Some Question, rest' ->
              parse_optional_labeled_arrow label left rest'
          | _ -> Error "Expected '?' for optional argument")
      | _ -> Error "Expected ':' or '?' after label")
  | _ -> Error "Expected label"

and parse_optional_labeled_arrow label _left tokens =
  match expect_colon tokens with
  | Ok rest -> (
      match parse_arrow_type rest with
      | Ok (arg_type, rest') -> (
          match expect_right_arrow rest' with
          | Ok rest'' -> (
              match parse_arrow_type rest'' with
              | Ok (return_type, rest''') ->
                  Ok
                    ( make_arrow ~label ~optional:true arg_type return_type,
                      rest''' )
              | Error err -> Error err)
          | Error err -> Error err)
      | Error err -> Error err)
  | Error e -> Error e

and expect_right_arrow tokens =
  match consume_token tokens with
  | Some RightArrow, rest -> Ok rest
  | _ -> Error "Expected '->'"

(* Parse 'as' types: typexpr as ' ident
 * Syntax: 'a list as 'my_type
 *)
and parse_as_type tokens =
  match parse_tuple_type tokens with
  | Ok (t, rest) -> (
      match peek_token rest with
      | Some As -> (
          match consume_token rest with
          | Some As, rest' -> (
              match consume_token rest' with
              | Some Quote, rest'' -> (
                  match consume_token rest'' with
                  | Some (Ident var), rest''' when validate_ident var ->
                      Ok (make_type_as t var, rest''')
                  | _ -> Error "Expected type variable after 'as '")
              | _ -> Error "Expected ' after 'as'")
          | _ -> Ok (t, rest))
      | _ -> Ok (t, rest))
  | Error e -> Error e

(* Parse tuple types: typexpr { * typexpr }+
 * Syntax: int * string * bool (two or more types separated by *)

and parse_tuple_type tokens =
  match parse_app_type tokens with
  | Ok (first, rest) -> parse_type_tuple_continuation [ first ] rest
  | Error e -> Error e

and parse_type_tuple_continuation acc tokens =
  match peek_token tokens with
  | Some (Token.InfixOp "*") -> (
      match consume_token tokens with
      | Some _, rest -> (
          match parse_app_type rest with
          | Ok (t, rest') -> parse_type_tuple_continuation (t :: acc) rest'
          | Error err -> Error err)
      | _ -> (
          if List.length acc > 1 then Ok (make_tuple (List.rev acc), tokens)
          else
            match acc with
            | [ single ] -> Ok (single, tokens)
            | [] -> Error "Empty tuple list"
            | _ -> Ok (make_tuple (List.rev acc), tokens)))
  | _ -> (
      if List.length acc > 1 then Ok (make_tuple (List.rev acc), tokens)
      else
        match acc with
        | [ single ] -> Ok (single, tokens)
        | [] -> Error "Empty tuple list"
        | _ -> Ok (make_tuple (List.rev acc), tokens))

(* Parse type applications: typexpr typeconstr | ( typexpr { , typexpr } ) typeconstr
 * Syntax:
 * - Single parameter: 'a list
 * - Multiple parameters: ('a, 'b) result
 * - Class type application: 'a #class
 *)
and parse_app_type tokens =
  match parse_primary_type tokens with
  | Ok (t, rest) -> parse_app_continuation t rest
  | Error e -> Error e

and parse_app_continuation t tokens =
  match peek_token tokens with
  | Some (Ident name) when validate_lowercase_ident name -> (
      (* Simple type constructor application *)
      match consume_token tokens with
      | Some (Ident tc_name), rest -> Ok (make_type_app t (None, tc_name), rest)
      | _ -> Ok (t, tokens))
  | Some Hash -> (
      (* Class type application *)
      match consume_token tokens with
      | Some Hash, rest -> (
          match parse_class_path rest with
          | Ok (class_path, rest') -> Ok (make_classtype_app t class_path, rest')
          | Error err -> Error err)
      | _ -> Ok (t, tokens))
  | _ -> Ok (t, tokens)

(* Parse primary types: atoms and parenthesized expressions
 * Syntax:
 * - Type variables: 'a, 'var
 * - Wildcards: _
 * - Type constructors: int, string, Module.type_name
 * - Parenthesized: (int), (int * string)
 * - Object types: < method : int >, < .. >
 * - Class types: #class_name, #Module.class_name
 *)
(* Polymorphic variant type parsing *)
and parse_polymorphic_variant_type tokens =
  match consume_token tokens with
  | Some Token.LBracket, rest -> 
      parse_polymorphic_variant_body rest
  | _ -> Error "Expected '['"

and parse_polymorphic_variant_body tokens =
  match peek_token tokens with
  | Some Token.Gt -> 
      (* [> variant-field { | variant-field } ] *)
      parse_open_variant tokens
  | Some Token.Lt -> 
      (* [< variant-field { | variant-field } [> typexpr { & typexpr } ] ] *)
      parse_less_variant tokens
  | _ -> 
      (* [ variant-field { | variant-field } ] *)
      parse_closed_variant tokens

and parse_open_variant tokens =
  match consume_token tokens with
  | Some Token.Gt, rest -> (
      match parse_variant_fields [] rest with
      | Ok (fields, Token.RBracket :: rest') -> 
          Ok (Parse_tree.PolymorphicVariant (Parse_tree.VariantOpen fields), rest')
      | _ -> Error "Expected ']' after polymorphic variant")
  | _ -> Error "Expected '>'"

and parse_less_variant tokens =
  match consume_token tokens with
  | Some Token.Lt, rest -> (
      match parse_variant_fields [] rest with
      | Ok (fields, rest') -> 
          parse_less_variant_constraint fields rest'
      | Error err -> Error err)
  | _ -> Error "Expected '<'"

and parse_less_variant_constraint fields tokens =
  match peek_token tokens with
  | Some Token.Gt -> (
      match consume_token tokens with
      | Some Token.Gt, rest -> (
          match parse_and_separated_types [] rest with
          | Ok (types, Token.RBracket :: rest') ->
              Ok (Parse_tree.PolymorphicVariant (Parse_tree.VariantLess (fields, types)), rest')
          | _ -> Error "Expected ']' after polymorphic variant")
      | _ -> Error "Expected '>'")
  | Some Token.RBracket -> (
      match consume_token tokens with
      | Some Token.RBracket, rest ->
          Ok (Parse_tree.PolymorphicVariant (Parse_tree.VariantLess (fields, [])), rest)
      | _ -> Error "Expected ']'")
  | _ -> Error "Expected ']' or '>'"

and parse_closed_variant tokens =
  match parse_variant_fields [] tokens with
  | Ok (fields, Token.RBracket :: rest) -> 
      Ok (Parse_tree.PolymorphicVariant (Parse_tree.VariantClosed fields), rest)
  | _ -> Error "Expected ']' after polymorphic variant"

and parse_variant_fields acc tokens =
  match tokens with
  | Token.RBracket :: _ -> Ok (List.rev acc, tokens)
  | Token.Gt :: _ -> Ok (List.rev acc, tokens)
  | _ -> (
      match parse_variant_field tokens with
      | Ok (field, rest) -> (
          match peek_token rest with
          | Some Token.Bar -> (
              match consume_token rest with
              | Some Token.Bar, rest' -> parse_variant_fields (field :: acc) rest'
              | _ -> Error "Expected '|'")
          | _ -> Ok (List.rev (field :: acc), rest))
      | Error err -> Error err)

and parse_variant_field tokens =
  match tokens with
  | Token.PolymorphicVariantTag tag :: rest -> (
      (* `tag-name [ of [&] typexpr { & typexpr } ] *)
      match peek_token rest with
      | Some Token.Of -> (
          match consume_token rest with
          | Some Token.Of, rest' -> (
              (* Check for & at the beginning *)
              let (has_ampersand, rest'') = 
                match peek_token rest' with
                | Some (Token.InfixOp "&") -> (true, snd (consume_token rest'))
                | _ -> (false, rest')
              in
              match parse_and_separated_types [] rest'' with
              | Ok (types, rest''') -> 
                  Ok (Parse_tree.TagField (tag, has_ampersand, types), rest''')
              | Error err -> Error err)
          | _ -> Error "Expected 'of'")
      | _ -> Ok (Parse_tree.TagField (tag, false, []), rest))
  | _ -> (
      (* typexpr - inheritance *)
      match parse_typexpr tokens with
      | Ok (t, rest) -> Ok (Parse_tree.InheritField t, rest)
      | Error err -> Error err)

and parse_and_separated_types acc tokens =
  match parse_typexpr tokens with
  | Ok (t, rest) -> (
      match peek_token rest with
      | Some (Token.InfixOp "&") -> (
          match consume_token rest with
          | Some (Token.InfixOp "&"), rest' -> parse_and_separated_types (t :: acc) rest'
          | _ -> Error "Expected '&'")
      | _ -> Ok (List.rev (t :: acc), rest))
  | Error err -> Error err

and parse_primary_type tokens =
  match peek_token tokens with
  | Some Quote -> (
      (* Type variable: ' ident *)
      match consume_token tokens with
      | Some Quote, rest -> (
          match consume_token rest with
          | Some (Ident var), rest' when validate_ident var ->
              Ok (make_type_var var, rest')
          | _ -> Error "Expected type variable after '")
      | _ -> Error "Expected type variable")
  | Some (Ident "_") -> (
      (* Wildcard: _ *)
      match consume_token tokens with
      | Some (Ident "_"), rest -> Ok (make_wildcard (), rest)
      | _ -> Error "Expected wildcard")
  | Some LParen -> parse_parenthesized_type tokens
  | Some LBracket -> parse_polymorphic_variant_type tokens
  | Some Lt -> parse_object_type tokens
  | Some Hash -> parse_class_type tokens
  | Some (Ident name) when validate_lowercase_ident name -> (
      (* Type constructor *)
      match consume_token tokens with
      | Some (Ident tc_name), rest -> Ok (make_typeconstr (None, tc_name), rest)
      | _ -> Error "Expected type constructor")
  | _ -> Error "Expected type expression"

and parse_parenthesized_type tokens =
  match consume_token tokens with
  | Some LParen, rest -> (
      match parse_typexpr rest with
      | Ok (t, rest') -> (
          match peek_token rest' with
          | Some Comma -> (
              (* Multiple types: ( typexpr { , typexpr } ) typeconstr *)
              match parse_comma_separated_list parse_typexpr rest' with
              | Ok (types, rest'') -> (
                  match expect_rparen rest'' with
                  | Ok rest''' -> (
                      match parse_type_constructor rest''' with
                      | Ok (tc_path, rest'''') ->
                          Ok (make_type_app_multi types tc_path, rest'''')
                      | Error err -> Error err)
                  | Error err -> Error err)
              | Error err -> Error err)
          | Some RParen -> (
              (* Single parenthesized type *)
              match consume_token rest' with
              | Some RParen, rest'' -> Ok (make_parenthesized t, rest'')
              | _ -> Error "Expected ')'")
          | _ -> Error "Expected ')' or ','")
      | Error err -> Error err)
  | _ -> Error "Expected '('"

and parse_type_constructor tokens =
  match consume_token tokens with
  | Some (Ident name), rest when validate_lowercase_ident name ->
      Ok ((None, name), rest)
  | _ -> Error "Expected type constructor"

(* Parse object types: < method-list > | < .. >
 * Syntax:
 * - Empty object: < .. >
 * - Object with methods: < method1 : int; method2 : string >
 * - Object with trailing semicolon: < method : int; >
 * - Object with open type: < method : int; .. >
 *)
and parse_object_type tokens =
  match consume_token tokens with
  | Some Lt, rest -> (
      match peek_token rest with
      | Some DotDot -> (
          (* Empty object: < .. > *)
          match consume_token rest with
          | Some DotDot, rest' -> (
              match expect_gt rest' with
              | Ok rest'' -> Ok (make_object_empty (), rest'')
              | Error err -> Error err)
          | _ -> Error "Expected '..'")
      | _ ->
          (* Object with methods *)
          parse_object_methods rest)
  | _ -> Error "Expected '<'"

and parse_object_methods tokens =
  match parse_method_type tokens with
  | Ok (method_t, rest) -> parse_object_methods_continuation [ method_t ] rest
  | Error _ -> (
      (* No methods, check for .. *)
      match peek_token tokens with
      | Some DotDot -> (
          match consume_token tokens with
          | Some DotDot, rest -> (
              match expect_gt rest with
              | Ok rest' -> Ok (make_object_empty (), rest')
              | Error err -> Error err)
          | _ -> Error "Expected '..'")
      | Some Gt -> (
          match consume_token tokens with
          | Some Gt, rest -> Ok (make_object [] false false, rest)
          | _ -> Error "Expected '>'")
      | _ -> Error "Expected method or '..' or '>'")

and parse_object_methods_continuation methods tokens =
  match peek_token tokens with
  | Some Semicolon -> (
      match consume_token tokens with
      | Some Semicolon, rest -> (
          match peek_token rest with
          | Some DotDot -> (
              (* ; .. > *)
              match consume_token rest with
              | Some DotDot, rest' -> (
                  match expect_gt rest' with
                  | Ok rest'' ->
                      Ok (make_object (List.rev methods) true true, rest'')
                  | Error err -> Error err)
              | _ -> Error "Expected '..'")
          | Some Gt -> (
              (* ; > *)
              match consume_token rest with
              | Some Gt, rest' ->
                  Ok (make_object (List.rev methods) true false, rest')
              | _ -> Error "Expected '>'")
          | _ -> (
              (* ; method-type *)
              match parse_method_type rest with
              | Ok (method_t, rest') ->
                  parse_object_methods_continuation (method_t :: methods) rest'
              | Error err -> Error err))
      | _ -> Error "Expected ';'")
  | Some Gt -> (
      match consume_token tokens with
      | Some Gt, rest -> Ok (make_object (List.rev methods) false false, rest)
      | _ -> Error "Expected '>'")
  | _ -> Error "Expected ';' or '>'"

and expect_gt tokens =
  match consume_token tokens with
  | Some Gt, rest -> Ok rest
  | _ -> Error "Expected '>'"

(* Parse class types: # class-path
 * Syntax: #class_name, #Module.class_name
 *)
and parse_class_type tokens =
  match consume_token tokens with
  | Some Hash, rest -> (
      match parse_classtype_path rest with
      | Ok (ct_path, rest') -> Ok (make_classtype ct_path, rest')
      | Error err -> Error err)
  | _ -> Error "Expected '#'"

and parse_class_path tokens = try_parse_with_module_path parse_class_name tokens

and parse_classtype_path tokens =
  (* Use try_parse_with_module_path but with extended_module_path *)
  match consume_token tokens with
  | Some token, rest -> (
      match parse_class_name token with
      | Ok name -> Ok ((None, name), rest)
      | Error _ -> (
          (* Try with extended module path *)
          match parse_extended_module_path tokens with
          | Ok (path, rest') -> (
              match expect_dot rest' with
              | Ok rest'' -> (
                  let token_opt, rest''' = consume_token rest'' in
                  match token_opt with
                  | Some token' -> (
                      match parse_class_name token' with
                      | Ok name -> Ok ((Some path, name), rest''')
                      | Error err -> Error err)
                  | None -> Error "Expected class name after module path")
              | _ -> Error "Expected class name after module path")
          | Error _ -> Error "Expected class name"))
  | _ -> Error "Expected class name"

(* Full class type parsing functions *)

(* Parse complete class types according to OCaml grammar:
 * class-type ::=
 *   [?label-name:] typexpr -> class-type
 *   | class-body-type
 *)
and parse_full_class_type tokens = parse_class_arrow_type tokens

and parse_class_arrow_type tokens =
  match parse_class_body_type tokens with
  | Ok (class_body, rest) -> (
      match peek_token rest with
      | Some RightArrow -> (
          match consume_token rest with
          | Some RightArrow, rest' -> (
              match parse_full_class_type rest' with
              | Ok (next_class_type, rest'') ->
                  Ok (Parse_tree.ClassArrow (None, false, 
                      Parse_tree.TypeConstr (None, "unit"), next_class_type), rest'')
              | Error err -> Error err)
          | _ -> Error "Expected '->'")
      | _ -> Ok (Parse_tree.ClassBodyType class_body, rest))
  | Error err -> Error err

(* TODO: Implement labeled class arrows when needed *)

(* Parse class body types:
 * class-body-type ::=
 *   object [typexpr] { class-field-spec } end
 *   | [typexpr {, typexpr}] classtype-path
 *   | let open module-path in class-body-type
 *)
and parse_class_body_type tokens =
  match peek_token tokens with
  | Some Object -> parse_class_object_type tokens
  | Some Let -> parse_class_let_open tokens
  | _ -> parse_class_type_application tokens

and parse_class_object_type tokens =
  match consume_token tokens with
  | Some Object, rest -> (
      (* Parse optional self type: object [typexpr] *)
      let (self_type, rest') = 
        match parse_typexpr rest with
        | Ok (t, rest'') -> (Some t, rest'')
        | Error _ -> (None, rest)
      in
      (* Parse class field specs *)
      parse_class_field_specs [] self_type rest')
  | _ -> Error "Expected 'object'"

and parse_class_field_specs acc self_type tokens =
  match peek_token tokens with
  | Some End -> (
      match consume_token tokens with
      | Some End, rest -> 
          Ok (Parse_tree.ClassObject (self_type, List.rev acc), rest)
      | _ -> Error "Expected 'end'")
  | _ -> (
      match parse_class_field_spec tokens with
      | Ok (field_spec, rest) -> 
          parse_class_field_specs (field_spec :: acc) self_type rest
      | Error _ when List.is_empty acc -> 
          (* No field specs, just end *)
          (match consume_token tokens with
          | Some End, rest -> 
              Ok (Parse_tree.ClassObject (self_type, []), rest)
          | _ -> Error "Expected class field spec or 'end'")
      | Error err -> Error err)

and parse_class_field_spec tokens =
  match peek_token tokens with
  | Some Inherit -> parse_inherit_spec tokens
  | Some Val -> parse_val_spec tokens
  | Some Method -> parse_method_spec tokens
  | Some Constraint -> parse_constraint_spec tokens
  | _ -> Error "Expected 'inherit', 'val', 'method', or 'constraint'"

and parse_inherit_spec tokens =
  match consume_token tokens with
  | Some Inherit, rest -> (
      match parse_class_body_type rest with
      | Ok (class_body, rest') ->
          Ok (Parse_tree.InheritSpec class_body, rest')
      | Error err -> Error err)
  | _ -> Error "Expected 'inherit'"

and parse_val_spec tokens =
  match consume_token tokens with
  | Some Val, rest -> (
      let (is_mutable, rest') = 
        match peek_token rest with
        | Some Mutable -> (true, snd (consume_token rest))
        | _ -> (false, rest)
      in
      let (is_virtual, rest'') = 
        match peek_token rest' with
        | Some Virtual -> (true, snd (consume_token rest'))
        | _ -> (false, rest')
      in
      match consume_token rest'' with
      | Some (Ident name), rest''' when validate_lowercase_ident name -> (
          match expect_colon rest''' with
          | Ok rest'''' -> (
              match parse_typexpr rest'''' with
              | Ok (t, rest''''') ->
                  Ok (Parse_tree.ValSpec (is_mutable, is_virtual, name, t), rest''''')
              | Error err -> Error err)
          | Error err -> Error err)
      | _ -> Error "Expected instance variable name")
  | _ -> Error "Expected 'val'"

and parse_method_spec tokens =
  match consume_token tokens with
  | Some Method, rest -> (
      let (is_private, rest') = 
        match peek_token rest with
        | Some Private -> (true, snd (consume_token rest))
        | _ -> (false, rest)
      in
      let (is_virtual, rest'') = 
        match peek_token rest' with
        | Some Virtual -> (true, snd (consume_token rest'))
        | _ -> (false, rest')
      in
      match consume_token rest'' with
      | Some (Ident name), rest''' when validate_lowercase_ident name -> (
          match expect_colon rest''' with
          | Ok rest'''' -> (
              match parse_poly_typexpr rest'''' with
              | Ok (poly_t, rest''''') ->
                  Ok (Parse_tree.MethodSpec (is_private, is_virtual, name, poly_t), rest''''')
              | Error err -> Error err)
          | Error err -> Error err)
      | _ -> Error "Expected method name")
  | _ -> Error "Expected 'method'"

and parse_constraint_spec tokens =
  match consume_token tokens with
  | Some Constraint, rest -> (
      match parse_typexpr rest with
      | Ok (t1, Eq :: rest') -> (
          match parse_typexpr rest' with
          | Ok (t2, rest'') ->
              Ok (Parse_tree.ConstraintSpec (t1, t2), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected type constraint 'type = type'")
  | _ -> Error "Expected 'constraint'"

and parse_class_let_open tokens =
  match consume_token tokens with
  | Some Let, Open :: rest -> (
      match parse_module_path rest with
      | Ok (module_path, In :: rest') -> (
          match parse_class_body_type rest' with
          | Ok (class_body, rest'') ->
              Ok (Parse_tree.ClassOpen (module_path, class_body), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected module path and 'in'")
  | _ -> Error "Expected 'let open'"

and parse_class_type_application tokens =
  (* Parse [typexpr {, typexpr}] classtype-path *)
  match peek_token tokens with
  | Some LParen -> (
      (* Multiple type parameters: (t1, t2, ...) classtype-path *)
      match consume_token tokens with
      | Some LParen, rest -> (
          match parse_comma_separated_list parse_typexpr rest with
          | Ok (types, RParen :: rest') -> (
              match parse_classtype_path rest' with
              | Ok (ct_path, rest'') ->
                  Ok (Parse_tree.ClassApp (types, ct_path), rest'')
              | Error err -> Error err)
          | _ -> Error "Expected ')' after type parameters")
      | _ -> Error "Expected '('")
  | _ -> (
      (* Try single type parameter or no parameters *)
      match parse_typexpr tokens with
      | Ok (t, rest) -> (
          match parse_classtype_path rest with
          | Ok (ct_path, rest') ->
              Ok (Parse_tree.ClassApp ([t], ct_path), rest')
          | Error _ -> 
              (* No classtype path, might just be a standalone classtype path *)
              (match parse_classtype_path tokens with
              | Ok (ct_path, rest') ->
                  Ok (Parse_tree.ClassPath ct_path, rest')
              | Error err -> Error err))
      | Error _ -> 
          (* Try just classtype path *)
          (match parse_classtype_path tokens with
          | Ok (ct_path, rest) ->
              Ok (Parse_tree.ClassPath ct_path, rest)
          | Error err -> Error err))

(* Lexer integration functions *)

(* Helper function to extract tokens from lexer *)
let tokens_from_lexer lexer =
  let rec collect_tokens acc =
    match Lexer.get_next lexer with
    | Ok (Some token_spanned) -> collect_tokens (Span.value token_spanned :: acc)
    | Ok None -> Ok (List.rev acc) (* EOF reached *)
    | Error e ->
        Lexer.print_error e;
        Error "Lexer error"
  in
  collect_tokens []

(* Main parser function that works with lexer - with error recovery *)
let parse_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> (
      (* Track the best parse result and where it got to *)
      let best_result = ref None in
      let best_progress = ref 0 in

      (* Try parsing as type expression *)
      (match parse_typexpr tokens with
      | Ok (ast, remaining) ->
          let progress = List.length tokens - List.length remaining in
          if progress > !best_progress then (
            best_result := Some (TypeExpr ast, remaining);
            best_progress := progress);
          ()
      | Error _ -> ());

      (* Try parsing as pattern - not implemented yet *)
      (*
      (match !best_result with
      | Some (_, []) -> ()
      | _ -> (
          match parse_pattern tokens with
          | Ok (pattern, remaining) ->
              let progress = List.length tokens - List.length remaining in
              if progress > !best_progress then (
                best_result := Some (Pattern pattern, remaining);
                best_progress := progress);
              ()
          | Error _ -> ()));
      *)

      (* Try parsing as expression - not implemented yet *)
      (*
      (match !best_result with
      | Some (_, []) -> ()
      | _ -> (
          match parse_expr tokens with
          | Ok (expr, remaining) ->
              let progress = List.length tokens - List.length remaining in
              if progress > !best_progress then (
                best_result := Some (Expr expr, remaining);
                best_progress := progress);
              ()
          | Error _ -> ()));
      *)

      (* Try parsing as constant *)
      (match !best_result with
      | Some (_, []) -> ()
      | _ -> (
          match parse_constant_tokens tokens with
          | Ok (constant, remaining) ->
              let progress = List.length tokens - List.length remaining in
              if progress > !best_progress then (
                best_result := Some (Constant constant, remaining);
                best_progress := progress);
              ()
          | Error _ -> ()));

      (* Return the best result we found *)
      match !best_result with
      | Some (ast, []) -> Ok ast
      | Some (_, remaining) ->
          (* We made some progress but couldn't parse everything *)
          Error
            (Printf.sprintf "Parsed partially but found unexpected tokens: %s"
               (String.concat ~sep:" " (List.map remaining ~f:string_of_token)))
      | None -> Error "Unable to parse input")
  | Error e -> Error e

(* Convenience function to parse from string *)
let parse_string str source =
  let lexer = Lexer.create str source in
  parse_from_lexer lexer

(* Specific parsing functions for constants *)
let parse_constant_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> (
      match parse_constant_tokens tokens with
      | Ok (constant, []) -> Ok constant (* Successfully parsed all tokens *)
      | Ok (_, remaining) ->
          Error
            ("Unexpected tokens remaining: "
            ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
      | Error err -> Error err)
  | Error e -> Error e

let parse_constant_string str source =
  let lexer = Lexer.create str source in
  parse_constant_from_lexer lexer

(* Specific parsing functions for patterns *)
let parse_pattern_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> (
      match parse_pattern tokens with
      | Ok (pattern, []) -> Ok pattern (* Successfully parsed all tokens *)
      | Ok (_, remaining) ->
          Error
            ("Unexpected tokens remaining: "
            ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
      | Error err -> Error err)
  | Error e -> Error e

let parse_pattern_string str source =
  let lexer = Lexer.create str source in
  parse_pattern_from_lexer lexer

(* Helper function to convert token number to parse tree constant *)
let number_to_constant = function
  | Int i -> Parse_tree.IntegerLiteral i
  | Int32 i -> Parse_tree.Int32Literal i
  | Int64 i -> Parse_tree.Int64Literal i
  | NativeInt i -> Parse_tree.NativeIntLiteral i
  | Token.Float f -> Parse_tree.FloatLiteral f

(* Expression parsing functions *)

(* Parse primary expressions (highest precedence) *)
let rec parse_primary_expr tokens =
  match tokens with
  | [] -> Error "Expected expression"
  | Number n :: rest -> Ok (Parse_tree.ConstantExpr (number_to_constant n), rest)
  | Char c :: rest ->
      Ok (Parse_tree.ConstantExpr (Parse_tree.CharLiteral c), rest)
  | String s :: rest ->
      Ok (Parse_tree.ConstantExpr (Parse_tree.StringLiteral s), rest)
  | True :: rest -> Ok (Parse_tree.ConstantExpr Parse_tree.True, rest)
  | False :: rest -> Ok (Parse_tree.ConstantExpr Parse_tree.False, rest)
  | LParen :: RParen :: rest ->
      Ok (Parse_tree.ConstantExpr Parse_tree.Unit, rest)
  | Begin :: End :: rest ->
      Ok (Parse_tree.ConstantExpr Parse_tree.BeginEnd, rest)
  | LBracket :: RBracket :: rest ->
      Ok (Parse_tree.ConstantExpr Parse_tree.EmptyList, rest)
  | LBracket :: Bar :: RBracket :: rest ->
      Ok (Parse_tree.ConstantExpr Parse_tree.EmptyArray, rest)
  | PolymorphicVariantTag tag :: rest -> (
      match rest with
      | LParen :: _
      | LBracket :: _
      | LBrace :: _
      | Ident _ :: _
      | Number _ :: _
      | String _ :: _
      | Char _ :: _
      | True :: _
      | False :: _ -> (
          (* Has an argument *)
          match parse_primary_expr rest with
          | Ok (arg, rest') ->
              Ok (Parse_tree.PolymorphicVariantExpr (tag, Some arg), rest')
          | Error err -> Error err)
      | _ -> Ok (Parse_tree.PolymorphicVariantExpr (tag, None), rest))
  | LParen :: rest -> (
      (* Could be parenthesized expr, tuple, or type constraint *)
      match parse_expr rest with
      | Ok (e, Colon :: rest') -> (
          (* Type constraint or subtyping coercion *)
          match parse_typexpr rest' with
          | Ok (t, ColonGt :: rest'') -> (
              (* Subtyping coercion (expr : type :> type) *)
              match parse_typexpr rest'' with
              | Ok (t2, RParen :: rest''') ->
                  Ok (Parse_tree.SubtypingCoercion (e, t, t2), rest''')
              | _ -> Error "Expected type and ')' after ':>'")
          | Ok (t, RParen :: rest'') ->
              Ok (Parse_tree.TypeConstraint (e, t), rest'')
          | Ok (_, _) -> Error "Expected ')' or ':>' after type"
          | Error err -> Error err)
      | Ok (e, ColonGt :: rest') -> (
          (* Direct coercion (expr :> type) *)
          match parse_typexpr rest' with
          | Ok (t, RParen :: rest'') ->
              Ok (Parse_tree.Coercion (e, t), rest'')
          | _ -> Error "Expected type and ')' after ':>'")
      | Ok (e, Comma :: rest') ->
          (* Tuple *)
          parse_tuple_elements [ e ] rest'
      | Ok (e, RParen :: rest') -> Ok (Parse_tree.ParenthesizedExpr e, rest')
      | Ok (_, _) ->
          Error "Expected ')', ',' or ':' in parenthesized expression"
      | Error err -> Error err)
  | Begin :: rest -> (
      match parse_expr rest with
      | Ok (e, End :: rest') -> Ok (Parse_tree.BeginEndExpr e, rest')
      | Ok (_, _) -> Error "Expected 'end' after 'begin'"
      | Error err -> Error err)
  | LBracket :: rest -> (
      match rest with
      | Bar :: _ -> parse_array_expr rest
      | _ -> parse_list_expr rest)
  | LBrace :: rest -> parse_record_expr rest
  | Lazy :: rest -> (
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (Parse_tree.Lazy e, rest')
      | Error err -> Error err)
  | Assert :: rest -> (
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (Parse_tree.Assert e, rest')
      | Error err -> Error err)
  | Ident "raise" :: rest -> (
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (Parse_tree.Raise e, rest')
      | Error err -> Error err)
  | New :: rest -> (
      (* New instance creation *)
      match parse_class_path rest with
      | Ok (class_path, rest') ->
          parse_new_instance_args class_path [] rest'
      | Error err -> Error err)
  | Object :: rest -> (
      (* Object expression: object [self-pattern] { class-field } end *)
      parse_object_expression rest)
  | _ :: _ -> (
      (* Try to parse as value path, constructor, or Module.(expr) *)
      match parse_value_path tokens with
      | Ok (path, rest) -> Ok (Parse_tree.ValuePathExpr path, rest)
      | Error _ -> (
          (* Try module path for Module.(expr) syntax *)
          match parse_module_path tokens with
          | Ok (module_path, Dot :: LParen :: rest) -> (
              match parse_expr rest with
              | Ok (e, RParen :: rest') ->
                  Ok (Parse_tree.LocalOpen (module_path, e), rest')
              | _ -> Error "Expected expression and ')' after 'Module.('")
          | _ -> (
              match parse_constr tokens with
              | Ok (path, rest) -> (
              (* Check if constructor has an argument *)
              match rest with
              | LParen :: _
              | LBracket :: _
              | LBrace :: _
              | Ident _ :: _
              | Number _ :: _
              | String _ :: _
              | Char _ :: _ -> (
                  match parse_primary_expr rest with
                  | Ok (arg, rest') ->
                      Ok (Parse_tree.ConstructorExpr (path, Some arg), rest')
                  | Error _ -> Ok (Parse_tree.ConstructorExpr (path, None), rest)
                  )
              | _ -> Ok (Parse_tree.ConstructorExpr (path, None), rest))
          | Error err -> Error err)))

and parse_tuple_elements acc tokens =
  match parse_expr tokens with
  | Ok (e, Comma :: rest) -> parse_tuple_elements (e :: acc) rest
  | Ok (e, RParen :: rest) ->
      Ok (Parse_tree.TupleExpr (List.rev (e :: acc)), rest)
  | Ok (_, _) -> Error "Expected ',' or ')' in tuple"
  | Error e -> Error e

and parse_list_expr tokens =
  match tokens with
  | RBracket :: rest -> Ok (Parse_tree.ListExpr [], rest)
  | _ -> parse_list_elements [] tokens

and parse_list_elements acc tokens =
  match parse_expr tokens with
  | Ok (e, Semicolon :: rest) -> parse_list_elements (e :: acc) rest
  | Ok (e, RBracket :: rest) ->
      Ok (Parse_tree.ListExpr (List.rev (e :: acc)), rest)
  | Ok (_, _) -> Error "Expected ';' or ']' in list"
  | Error e -> Error e

and parse_array_expr tokens =
  match tokens with
  | Bar :: RBracket :: rest -> Ok (Parse_tree.ArrayExpr [], rest)
  | _ -> parse_array_elements [] tokens

and parse_array_elements acc tokens =
  match parse_expr tokens with
  | Ok (e, Semicolon :: rest) -> parse_array_elements (e :: acc) rest
  | Ok (e, Bar :: RBracket :: rest) ->
      Ok (Parse_tree.ArrayExpr (List.rev (e :: acc)), rest)
  | Ok (_, _) -> Error "Expected ';' or '|]' in array"
  | Error e -> Error e

and parse_record_expr tokens =
  match tokens with
  | RBrace :: rest -> Ok (Parse_tree.RecordExpr [], rest)
  | _ -> (
      (* Try to parse fields first *)
      match parse_record_fields [] tokens with
      | Ok (fields, rest) -> Ok (Parse_tree.RecordExpr fields, rest)
      | Error _ -> (
          (* If that fails, try record update syntax *)
          match parse_expr tokens with
          | Ok (e, With :: rest) -> (
              match parse_record_fields [] rest with
              | Ok (fields, rest') ->
                  Ok (Parse_tree.RecordUpdate (e, fields), rest')
              | Error err -> Error err)
          | _ -> Error "Expected record fields or record update expression"))

and parse_record_fields acc tokens =
  match parse_field tokens with
  | Ok (field_path, rest) -> (
      match rest with
      | Colon :: rest' -> (
          match parse_typexpr rest' with
          | Ok (t, Eq :: rest'') -> (
              match parse_expr rest'' with
              | Ok (e, rest''') ->
                  let field =
                    {
                      Parse_tree.field_path;
                      type_constraint = Some t;
                      expr = Some e;
                    }
                  in
                  parse_record_fields_cont (field :: acc) rest'''
              | Error err -> Error err)
          | Ok (t, rest'') ->
              let field =
                { Parse_tree.field_path; type_constraint = Some t; expr = None }
              in
              parse_record_fields_cont (field :: acc) rest''
          | Error err -> Error err)
      | Eq :: rest' -> (
          match parse_expr rest' with
          | Ok (e, rest'') ->
              let field =
                { Parse_tree.field_path; type_constraint = None; expr = Some e }
              in
              parse_record_fields_cont (field :: acc) rest''
          | Error err -> Error err)
      | _ ->
          let field =
            { Parse_tree.field_path; type_constraint = None; expr = None }
          in
          parse_record_fields_cont (field :: acc) rest)
  | Error e -> Error e

and parse_record_fields_cont acc tokens =
  match tokens with
  | Semicolon :: rest -> parse_record_fields acc rest
  | RBrace :: rest -> Ok (List.rev acc, rest)
  | _ -> Error "Expected ';' or '}' in record"

(* Parse field access and function application *)
and parse_postfix_expr tokens =
  match parse_primary_expr tokens with
  | Ok (e, rest) -> parse_postfix_cont e rest
  | Error e -> Error e

and parse_postfix_cont expr tokens =
  match tokens with
  | Dot :: rest -> (
      match rest with
      | LParen :: rest' -> (
          (* Array access *)
          match parse_expr rest' with
          | Ok (idx, RParen :: LeftArrow :: rest'') -> (
              (* Array update *)
              match parse_expr rest'' with
              | Ok (value, rest''') ->
                  parse_postfix_cont
                    (Parse_tree.ArrayUpdate (expr, idx, value))
                    rest'''
              | Error err -> Error err)
          | Ok (idx, RParen :: rest'') ->
              (* Array access *)
              parse_postfix_cont (Parse_tree.ArrayAccess (expr, idx)) rest''
          | _ -> Error "Expected ')' after array index")
      | LBracket :: rest' -> (
          (* String access *)
          match parse_expr rest' with
          | Ok (idx, RBracket :: LeftArrow :: rest'') -> (
              (* String update *)
              match parse_expr rest'' with
              | Ok (value, rest''') ->
                  parse_postfix_cont
                    (Parse_tree.StringUpdate (expr, idx, value))
                    rest'''
              | Error err -> Error err)
          | Ok (idx, RBracket :: rest'') ->
              (* String access *)
              parse_postfix_cont (Parse_tree.StringAccess (expr, idx)) rest''
          | _ -> Error "Expected ']' after string index")
      | _ -> (
          (* Field access *)
          match parse_field rest with
          | Ok (field, LeftArrow :: rest') -> (
              (* Field update *)
              match parse_expr rest' with
              | Ok (value, rest'') ->
                  parse_postfix_cont
                    (Parse_tree.FieldUpdate (expr, field, value))
                    rest''
              | Error err -> Error err)
          | Ok (field, rest') ->
              (* Field access *)
              parse_postfix_cont (Parse_tree.FieldAccess (expr, field)) rest'
          | Error err -> Error err))
  | Hash :: rest -> (
      (* Method call *)
      match rest with
      | token :: rest' -> (
          match parse_method_name token with
          | Ok method_name ->
              parse_method_call_args expr method_name [] rest'
          | Error err -> Error err)
      | [] -> Error "Expected method name after '#'")
  | _ ->
      (* Try function application *)
      parse_function_app expr [] tokens

and parse_function_app func args tokens =
  match parse_argument tokens with
  | Ok (arg, rest) -> parse_function_app func (arg :: args) rest
  | Error _ ->
      if List.is_empty args then Ok (func, tokens)
      else Ok (Parse_tree.FunctionApp (func, List.rev args), tokens)

and parse_method_call_args obj method_name args tokens =
  match parse_argument tokens with
  | Ok (arg, rest) -> parse_method_call_args obj method_name (arg :: args) rest
  | Error _ ->
      if List.is_empty args then
        parse_postfix_cont (Parse_tree.MethodCall (obj, method_name, [])) tokens
      else
        parse_postfix_cont
          (Parse_tree.MethodCall (obj, method_name, List.rev args))
          tokens

and parse_new_instance_args class_path args tokens =
  match parse_argument tokens with
  | Ok (arg, rest) -> parse_new_instance_args class_path (arg :: args) rest
  | Error _ ->
      if List.is_empty args then
        Ok (Parse_tree.NewInstance (class_path, []), tokens)
      else
        Ok (Parse_tree.NewInstance (class_path, List.rev args), tokens)

and parse_object_expression tokens =
  (* Parse object [self-pattern] { class-field } end *)
  let (self_pattern, rest) = parse_optional_self_pattern tokens in
  parse_object_fields [] self_pattern rest

and parse_optional_self_pattern tokens =
  (* Try to parse self pattern, if it fails, assume no self pattern *)
  match parse_pattern tokens with
  | Ok (pattern, rest) -> (Some pattern, rest)
  | Error _ -> (None, tokens)

and parse_object_fields acc self_pattern tokens =
  match peek_token tokens with
  | Some End -> (
      match consume_token tokens with
      | Some End, rest -> 
          Ok (Parse_tree.ObjectExpr { self_pattern; fields = List.rev acc }, rest)
      | _ -> Error "Expected 'end'")
  | _ -> (
      match parse_object_field tokens with
      | Ok (field, rest) -> 
          parse_object_fields (field :: acc) self_pattern rest
      | Error _ when List.is_empty acc -> 
          (* No fields, just end *)
          (match consume_token tokens with
          | Some End, rest -> 
              Ok (Parse_tree.ObjectExpr { self_pattern; fields = [] }, rest)
          | _ -> Error "Expected object field or 'end'")
      | Error err -> Error err)

and parse_object_field tokens =
  match peek_token tokens with
  | Some Inherit -> parse_object_inherit_field tokens
  | Some Val -> parse_object_val_field tokens
  | Some Method -> parse_object_method_field tokens
  | Some Initializer -> parse_object_initializer_field tokens
  | _ -> Error "Expected 'inherit', 'val', 'method', or 'initializer'"

and parse_object_inherit_field tokens =
  match consume_token tokens with
  | Some Inherit, rest -> (
      match parse_class_path rest with
      | Ok (class_path, rest') -> 
          let class_expr : class_expr = ClassPath class_path in
          (* Check for optional 'as' clause *)
          (match peek_token rest' with
          | Some As -> (
              match consume_token rest' with
              | Some As, rest'' -> (
                  match consume_token rest'' with
                  | Some (Ident name), rest''' when validate_lowercase_ident name ->
                      Ok (Parse_tree.InheritField (class_expr, Some name), rest''')
                  | _ -> Error "Expected lowercase identifier after 'as'")
              | _ -> Error "Expected 'as'")
          | _ -> Ok (Parse_tree.InheritField (class_expr, None), rest'))
      | Error err -> Error err)
  | _ -> Error "Expected 'inherit'"

and parse_object_val_field tokens =
  match consume_token tokens with
  | Some Val, rest -> (
      let (is_mutable, rest') = 
        match peek_token rest with
        | Some Mutable -> (true, snd (consume_token rest))
        | _ -> (false, rest)
      in
      match consume_token rest' with
      | Some (Ident name), rest'' when validate_lowercase_ident name -> (
          (* Parse optional type annotation *)
          let (type_opt, rest''') = 
            match peek_token rest'' with
            | Some Colon -> (
                match consume_token rest'' with
                | Some Colon, rest_colon -> (
                    match parse_typexpr rest_colon with
                    | Ok (t, rest_type) -> (Some t, rest_type)
                    | Error _ -> (None, rest''))
                | _ -> (None, rest''))
            | _ -> (None, rest'')
          in
          match consume_token rest''' with
          | Some Eq, rest'''' -> (
              match parse_expr rest'''' with
              | Ok (expr, rest''''') ->
                  Ok (Parse_tree.ValField (is_mutable, name, type_opt, expr), rest''''')
              | Error err -> Error err)
          | _ -> Error "Expected '=' after instance variable")
      | _ -> Error "Expected instance variable name")
  | _ -> Error "Expected 'val'"

and parse_object_method_field tokens =
  match consume_token tokens with
  | Some Method, rest -> (
      (* Check for 'virtual' first *)
      let (is_virtual, rest') = 
        match peek_token rest with
        | Some Virtual -> (true, snd (consume_token rest))
        | _ -> (false, rest)
      in
      if is_virtual then
        parse_virtual_method_field is_virtual rest'
      else
        parse_concrete_method_field rest')
  | _ -> Error "Expected 'method'"

and parse_virtual_method_field _is_virtual tokens =
  let (is_private, rest) = 
    match peek_token tokens with
    | Some Private -> (true, snd (consume_token tokens))
    | _ -> (false, tokens)
  in
  match consume_token rest with
  | Some (Ident name), rest' when validate_lowercase_ident name -> (
      match consume_token rest' with
      | Some Colon, rest'' -> (
          match parse_poly_typexpr rest'' with
          | Ok (poly_t, rest''') ->
              Ok (Parse_tree.VirtualMethod (is_private, name, poly_t), rest''')
          | Error err -> Error err)
      | _ -> Error "Expected ':' after virtual method name")
  | _ -> Error "Expected method name"

and parse_concrete_method_field tokens =
  let (is_private, rest) = 
    match peek_token tokens with
    | Some Private -> (true, snd (consume_token tokens))
    | _ -> (false, tokens)
  in
  match consume_token rest with
  | Some (Ident name), rest' when validate_lowercase_ident name -> (
      (* Parse parameters *)
      parse_method_parameters [] name is_private rest')
  | _ -> Error "Expected method name"

and parse_method_parameters acc method_name is_private tokens =
  match parse_parameter tokens with
  | Ok (param, rest) -> 
      parse_method_parameters (param :: acc) method_name is_private rest
  | Error _ -> 
      (* No more parameters, parse optional type annotation and body *)
      let params = List.rev acc in
      let (type_opt, rest') = 
        match peek_token tokens with
        | Some Colon -> (
            match consume_token tokens with
            | Some Colon, rest_colon -> (
                match parse_typexpr rest_colon with
                | Ok (t, rest_type) -> (Some t, rest_type)
                | Error _ -> (None, tokens))
            | _ -> (None, tokens))
        | _ -> (None, tokens)
      in
      match consume_token rest' with
      | Some Eq, rest'' -> (
          match parse_expr rest'' with
          | Ok (expr, rest''') ->
              Ok (Parse_tree.MethodField (is_private, method_name, params, type_opt, expr), rest''')
          | Error err -> Error err)
      | _ -> Error "Expected '=' after method"

and parse_object_initializer_field tokens =
  match consume_token tokens with
  | Some Initializer, rest -> (
      match parse_expr rest with
      | Ok (expr, rest') ->
          Ok (Parse_tree.InitializerField expr, rest')
      | Error err -> Error err)
  | _ -> Error "Expected 'initializer'"

and parse_argument tokens =
  match tokens with
  | Label label :: Colon :: rest -> (
      (* Labeled argument ~label:expr *)
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (Parse_tree.LabeledArg (label, e), rest')
      | Error err -> Error err)
  | Label label :: rest ->
      (* Punned labeled argument ~label *)
      Ok
        ( Parse_tree.LabeledArg (label, Parse_tree.ValuePathExpr (None, label)),
          rest )
  | OptLabel label :: Colon :: rest -> (
      (* Optional argument ?label:expr *)
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (Parse_tree.OptionalArg (label, Some e), rest')
      | Error err -> Error err)
  | OptLabel label :: rest ->
      (* Optional argument without value ?label *)
      Ok (Parse_tree.OptionalArg (label, None), rest)
  | _ -> (
      (* Simple argument *)
      match parse_primary_expr tokens with
      | Ok (e, rest) -> Ok (Parse_tree.SimpleArg e, rest)
      | Error err -> Error err)

(* Parse prefix operators *)
and parse_prefix_expr tokens =
  match tokens with
  | Minus :: rest -> (
      match parse_postfix_expr rest with
      | Ok (e, rest') -> Ok (Parse_tree.PrefixOp ("-", e), rest')
      | Error err -> Error err)
  | MinusDot :: rest -> (
      match parse_postfix_expr rest with
      | Ok (e, rest') -> Ok (Parse_tree.PrefixOp ("-.", e), rest')
      | Error err -> Error err)
  | Plus :: rest -> (
      match parse_postfix_expr rest with
      | Ok (e, rest') -> Ok (Parse_tree.PrefixOp ("+", e), rest')
      | Error err -> Error err)
  | _ -> parse_postfix_expr tokens

(* Parse infix operators with precedence *)
and parse_infix_expr tokens =
  match parse_prefix_expr tokens with
  | Ok (left, rest) -> parse_infix_cont left rest
  | Error e -> Error e

and parse_infix_cont left tokens =
  match tokens with
  | InfixOp op :: rest ->
      let prec = operator_precedence op in
      parse_infix_right left op prec rest tokens
  | Plus :: rest -> parse_infix_right left "+" 6 rest tokens
  | Minus :: rest -> parse_infix_right left "-" 6 rest tokens
  | Eq :: rest -> parse_infix_right left "=" 4 rest tokens
  | Lt :: rest -> parse_infix_right left "<" 5 rest tokens
  | Gt :: rest -> parse_infix_right left ">" 5 rest tokens
  | AndAnd :: rest -> parse_infix_right left "&&" 3 rest tokens
  | Or :: rest -> parse_infix_right left "||" 2 rest tokens
  | _ -> Ok (left, tokens)

and parse_infix_right left op prec rest original_tokens =
  match parse_prefix_expr rest with
  | Ok (right, rest') -> (
      match rest' with
      | InfixOp op2 :: _ when operator_precedence op2 > prec -> (
          (* Higher precedence operator, parse it first *)
          match parse_infix_cont right rest' with
          | Ok (right', rest'') ->
              parse_infix_cont (Parse_tree.InfixOp (left, op, right')) rest''
          | Error err -> Error err)
      | _ ->
          (* Same or lower precedence, create node *)
          parse_infix_cont (Parse_tree.InfixOp (left, op, right)) rest')
  | Error _ -> Ok (left, original_tokens)

and operator_precedence = function
  | "*" | "/" | "mod" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" -> 7
  | "+" | "-" | "+." | "-." | "^" | "@" -> 6
  | "::" -> 5
  | "=" | "<>" | "<" | ">" | "<=" | ">=" | "==" | "!=" -> 4
  | "&" | "&&" -> 3
  | "or" | "||" -> 2
  | _ -> 1

(* Parse cons expressions *)
and parse_cons_expr tokens =
  match parse_infix_expr tokens with
  | Ok (head, ColonColon :: rest) -> (
      match parse_cons_expr rest with
      | Ok (tail, rest') -> Ok (Parse_tree.ConsExpr (head, tail), rest')
      | Error err -> Error err)
  | result -> result

(* Parse tuple expressions *)
and parse_tuple_expr tokens =
  match parse_cons_expr tokens with
  | Ok (e1, Comma :: rest) -> parse_tuple_rest [ e1 ] rest
  | result -> result

and parse_tuple_rest acc tokens =
  match parse_cons_expr tokens with
  | Ok (e, Comma :: rest) -> parse_tuple_rest (e :: acc) rest
  | Ok (e, rest) -> Ok (Parse_tree.TupleExpr (List.rev (e :: acc)), rest)
  | Error e -> Error e

(* Parse control flow expressions *)
and parse_control_expr tokens =
  match tokens with
  | If :: rest -> (
      match parse_expr rest with
      | Ok (cond, Then :: rest') -> (
          match parse_expr rest' with
          | Ok (then_e, Else :: rest'') -> (
              match parse_expr rest'' with
              | Ok (else_e, rest''') ->
                  Ok (Parse_tree.IfThenElse (cond, then_e, Some else_e), rest''')
              | Error _ -> (
                  (* Try to recover by finding next sync point *)
                  match skip_until_tokens [ Semicolon; In; End ] rest'' with
                  | rest''' ->
                      Ok (Parse_tree.IfThenElse (cond, then_e, None), rest''')))
          | Ok (then_e, rest'') ->
              Ok (Parse_tree.IfThenElse (cond, then_e, None), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected 'then' after 'if' condition")
  | While :: rest -> (
      match parse_expr rest with
      | Ok (cond, Do :: rest') -> (
          match parse_expr rest' with
          | Ok (body, Done :: rest'') ->
              Ok (Parse_tree.While (cond, body), rest'')
          | _ -> Error "Expected 'done' after 'while' body")
      | _ -> Error "Expected 'do' after 'while' condition")
  | For :: Ident var :: Eq :: rest -> (
      match parse_expr rest with
      | Ok (start, To :: rest') -> (
          match parse_expr rest' with
          | Ok (stop, Do :: rest'') -> (
              match parse_expr rest'' with
              | Ok (body, Done :: rest''') ->
                  Ok
                    ( Parse_tree.For (var, start, Parse_tree.To, stop, body),
                      rest''' )
              | _ -> Error "Expected 'done' after 'for' body")
          | _ -> Error "Expected 'do' after 'for' range")
      | Ok (start, Downto :: rest') -> (
          match parse_expr rest' with
          | Ok (stop, Do :: rest'') -> (
              match parse_expr rest'' with
              | Ok (body, Done :: rest''') ->
                  Ok
                    ( Parse_tree.For (var, start, Parse_tree.Downto, stop, body),
                      rest''' )
              | _ -> Error "Expected 'done' after 'for' body")
          | _ -> Error "Expected 'do' after 'for' range")
      | _ -> Error "Expected 'to' or 'downto' in 'for' loop")
  | For :: _ -> Error "Expected identifier after 'for'"
  | Match :: rest -> (
      match parse_expr rest with
      | Ok (e, With :: rest') -> (
          match parse_cases rest' with
          | Ok (cases, rest'') -> Ok (Parse_tree.Match (e, cases), rest'')
          | Error _ -> (
              (* Try to recover by finding next sync point *)
              match
                skip_until_tokens [ Token.Semicolon; Token.In; Token.End ] rest'
              with
              | rest'' -> Ok (Parse_tree.Match (e, []), rest'')))
      | _ -> Error "Expected 'with' after 'match' expression")
  | Function :: rest -> (
      match parse_cases rest with
      | Ok (cases, rest') -> Ok (Parse_tree.Function cases, rest')
      | Error err -> Error err)
  | Try :: rest -> (
      match parse_expr rest with
      | Ok (e, With :: rest') -> (
          match parse_cases rest' with
          | Ok (cases, rest'') -> Ok (Parse_tree.Try (e, cases), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected 'with' after 'try' expression")
  | _ -> parse_tuple_expr tokens

and parse_cases tokens =
  match tokens with
  | Bar :: rest -> parse_case_list [] rest
  | _ -> parse_case_list [] tokens

and parse_case_list acc tokens =
  match parse_pattern tokens with
  | Ok (pattern, rest) -> (
      match rest with
      | When :: rest' -> (
          match parse_expr rest' with
          | Ok (guard, RightArrow :: rest'') -> (
              match parse_expr rest'' with
              | Ok (expr, rest''') ->
                  let case = { Parse_tree.pattern; guard = Some guard; expr } in
                  parse_case_list_cont (case :: acc) rest'''
              | Error _ -> (
                  (* Skip to next bar or sync point *)
                  match skip_until_tokens [ Or; Semicolon; In; End ] rest'' with
                  | Or :: rest''' -> parse_case_list acc rest'''
                  | rest''' -> Ok (List.rev acc, rest''')))
          | _ -> Error "Expected '->' after 'when' clause")
      | RightArrow :: rest' -> (
          match parse_expr rest' with
          | Ok (expr, rest'') ->
              let case = { Parse_tree.pattern; guard = None; expr } in
              parse_case_list_cont (case :: acc) rest''
          | Error _ -> (
              (* Skip to next bar or sync point *)
              match
                skip_until_tokens
                  [ Token.Or; Token.Semicolon; Token.In; Token.End ]
                  rest'
              with
              | Or :: rest'' -> parse_case_list acc rest''
              | rest'' -> Ok (List.rev acc, rest'')))
      | _ -> Error "Expected '->' or 'when' after pattern")
  | Error _ -> (
      (* Skip to next bar and try again *)
      match
        skip_until_tokens
          [ Token.Or; Token.Semicolon; Token.In; Token.End ]
          tokens
      with
      | Token.Or :: rest -> parse_case_list acc rest
      | rest -> Ok (List.rev acc, rest))

and parse_case_list_cont acc tokens =
  match tokens with
  | Bar :: rest -> parse_case_list acc rest
  | _ -> Ok (List.rev acc, tokens)

(* Parse fun expressions *)
and parse_fun_expr tokens =
  match tokens with
  | Fun :: rest -> parse_lambda_params [] rest
  | _ -> parse_control_expr tokens

and parse_lambda_params acc tokens =
  match parse_parameter tokens with
  | Ok (param, rest) -> parse_lambda_params (param :: acc) rest
  | Error _ -> (
      if List.is_empty acc then Error "Expected parameter after 'fun'"
      else
        match tokens with
        | Colon :: rest -> (
            match parse_typexpr rest with
            | Ok (t, RightArrow :: rest') -> (
                match parse_expr rest' with
                | Ok (body, rest'') ->
                    Ok (Parse_tree.Lambda (List.rev acc, Some t, body), rest'')
                | Error err -> Error err)
            | _ -> Error "Expected '->' after return type")
        | RightArrow :: rest -> (
            match parse_expr rest with
            | Ok (body, rest') ->
                Ok (Parse_tree.Lambda (List.rev acc, None, body), rest')
            | Error err -> Error err)
        | _ -> Error "Expected '->' or ':' after parameters")

and parse_parameter tokens =
  match tokens with
  | LParen :: Type :: rest -> (
      (* Type parameter *)
      match parse_typexpr rest with
      | Ok (t, RParen :: rest') -> Ok (Parse_tree.TypeParam t, rest')
      | _ -> Error "Expected ')' after type parameter")
  | Label label :: Colon :: rest -> (
      (* Labeled parameter *)
      match parse_pattern rest with
      | Ok (p, rest') -> Ok (Parse_tree.LabeledParam (label, p), rest')
      | Error err -> Error err)
  | OptLabel label :: Colon :: rest -> (
      (* Optional parameter with pattern *)
      match parse_pattern rest with
      | Ok (p, Eq :: rest') -> (
          match parse_expr rest' with
          | Ok (default, rest'') ->
              Ok (Parse_tree.OptionalParam (label, p, Some default), rest'')
          | Error err -> Error err)
      | Ok (p, rest') -> Ok (Parse_tree.OptionalParam (label, p, None), rest')
      | Error err -> Error err)
  | OptLabel label :: rest ->
      (* Optional parameter without pattern *)
      Ok
        ( Parse_tree.OptionalParam (label, Parse_tree.ValueName label, None),
          rest )
  | _ -> (
      (* Simple parameter *)
      match parse_pattern tokens with
      | Ok (p, rest) -> Ok (Parse_tree.SimpleParam p, rest)
      | Error err -> Error err)

(* Parse let expressions *)
and parse_let_expr tokens =
  match tokens with
  | Token.Let :: Token.Rec :: rest -> (
      match parse_let_bindings [] rest with
      | Ok (bindings, Token.In :: rest') -> (
          match parse_expr rest' with
          | Ok (body, rest'') -> Ok (Parse_tree.LetRec (bindings, body), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected 'in' after let bindings")
  | Token.Let :: Token.Exception :: Token.Ident name :: rest -> (
      (* let exception *)
      match rest with
      | Token.In :: rest' -> (
          match parse_expr rest' with
          | Ok (body, rest'') ->
              Ok (Parse_tree.LetException (name, None, body), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected 'in' after exception declaration")
  | Token.Let :: Token.Open :: rest -> (
      (* let open Module in expr *)
      match parse_module_path rest with
      | Ok (module_path, Token.In :: rest') -> (
          match parse_expr rest' with
          | Ok (body, rest'') ->
              Ok (Parse_tree.LocalOpen (module_path, body), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected module path and 'in' after 'let open'")
  | Token.Let :: Token.Module :: Token.Ident _ :: _ ->
      (* let module - simplified for now *)
      Error "let module not yet implemented"
  | Token.Let :: rest -> (
      match parse_let_bindings [] rest with
      | Ok (bindings, Token.In :: rest') -> (
          match parse_expr rest' with
          | Ok (body, rest'') -> Ok (Parse_tree.Let (bindings, body), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected 'in' after let bindings")
  | _ -> parse_fun_expr tokens

and parse_let_bindings acc tokens =
  match parse_let_binding tokens with
  | Ok (binding, And :: rest) -> parse_let_bindings (binding :: acc) rest
  | Ok (binding, rest) -> Ok (List.rev (binding :: acc), rest)
  | Error _ -> (
      (* Try to recover by skipping to 'in' or 'and' *)
      match skip_until_tokens [ Token.In; Token.And ] tokens with
      | And :: rest -> parse_let_bindings acc rest
      | Token.In :: _ as rest -> Ok (List.rev acc, rest)
      | rest -> Ok (List.rev acc, rest))

and parse_let_binding tokens =
  match parse_pattern tokens with
  | Ok (pattern, rest) -> parse_let_binding_params pattern [] rest
  | Error e -> Error e

and parse_let_binding_params pattern params tokens =
  match parse_parameter tokens with
  | Ok (param, rest) -> parse_let_binding_params pattern (param :: params) rest
  | Error _ -> (
      match tokens with
      | Token.Colon :: rest -> (
          match parse_typexpr rest with
          | Ok (t, Token.Eq :: rest') -> (
              match parse_expr rest' with
              | Ok (expr, rest'') ->
                  Ok
                    ( {
                        Parse_tree.pattern;
                        params = List.rev params;
                        type_constraint = Some t;
                        expr;
                      },
                      rest'' )
              | Error err -> Error err)
          | _ -> Error "Expected '=' after type annotation")
      | Eq :: rest -> (
          match parse_expr rest with
          | Ok (expr, rest') ->
              Ok
                ( {
                    Parse_tree.pattern;
                    params = List.rev params;
                    type_constraint = None;
                    expr;
                  },
                  rest' )
          | Error err -> Error err)
      | _ -> Error "Expected '=' or ':' in let binding")

(* Parse sequence expressions *)
and parse_sequence_expr tokens =
  match parse_let_expr tokens with
  | Ok (e1, Semicolon :: rest) -> (
      match parse_expr rest with
      | Ok (e2, rest') -> Ok (Parse_tree.Sequence (e1, e2), rest')
      | Error err -> Error err)
  | result -> result

(* Main expression parser *)
and parse_expr tokens = parse_sequence_expr tokens

(* Public expression parsing functions *)
let parse_expr_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> (
      match parse_expr tokens with
      | Ok (expr, []) -> Ok expr
      | Ok (_, remaining) ->
          Error
            ("Unexpected tokens remaining: "
            ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
      | Error err -> Error err)
  | Error e -> Error e

let parse_expr_string str source =
  let lexer = Lexer.create str source in
  parse_expr_from_lexer lexer

(* Class type parsing functions *)
let parse_class_type_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> (
      match parse_full_class_type tokens with
      | Ok (class_type, []) -> Ok class_type
      | Ok (_, remaining) ->
          Error
            ("Unexpected tokens remaining: "
            ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
      | Error err -> Error err)
  | Error e -> Error e

let parse_class_type_string str source =
  let lexer = Lexer.create str source in
  parse_class_type_from_lexer lexer

(* Top-level definition parsing functions *)
let rec parse_definition tokens =
  match tokens with
  | Token.Let :: Token.Rec :: rest -> parse_value_rec_def rest
  | Token.Let :: rest -> parse_value_def rest
  | Token.Type :: rest -> parse_type_def rest
  | Token.Exception :: rest -> parse_exception_def rest
  | Token.Module :: Token.Type :: rest -> parse_module_type_def rest
  | Token.Module :: rest -> parse_module_def rest
  | Token.Class :: rest -> parse_class_def rest
  | Token.Open :: rest -> parse_open_def rest
  | Token.Include :: rest -> parse_include_def rest
  | _ -> Error "Expected top-level definition"

and parse_value_def tokens =
  match parse_let_bindings [] tokens with
  | Ok (bindings, rest) -> Ok (Parse_tree.ValueDef bindings, rest)
  | Error err -> Error err

and parse_value_rec_def tokens =
  match parse_let_bindings [] tokens with
  | Ok (bindings, rest) -> Ok (Parse_tree.ValueRecDef bindings, rest)
  | Error err -> Error err

and parse_type_def tokens =
  match parse_type_declarations [] tokens with
  | Ok (decls, rest) -> Ok (Parse_tree.TypeDef decls, rest)
  | Error err -> Error err

and parse_type_declarations acc tokens =
  match parse_type_declaration tokens with
  | Ok (decl, Token.And :: rest) -> parse_type_declarations (decl :: acc) rest
  | Ok (decl, rest) -> Ok (List.rev (decl :: acc), rest)
  | Error err -> Error err

and parse_type_declaration tokens =
  (* Parse type parameters *)
  let (params, rest) = parse_type_params [] tokens in
  match rest with
  | Token.Ident type_name :: rest' when validate_lowercase_ident type_name -> (
      (* Parse private flag *)
      let (is_private, rest'') = 
        match rest' with
        | Token.Private :: rest''' -> (true, rest''')
        | _ -> (false, rest')
      in
      (* Parse manifest (= typexpr) *)
      let (manifest, rest''') = 
        match rest'' with
        | Token.Eq :: rest_eq -> (
            match parse_typexpr rest_eq with
            | Ok (t, rest_type) -> (Some t, rest_type)
            | Error _ -> (None, rest''))
        | _ -> (None, rest'')
      in
      (* Parse type kind and constraints *)
      let (kind, constraints, rest'''') = parse_type_kind_and_constraints rest''' in
      Ok ({
        Parse_tree.type_name;
        type_params = params;
        type_private = is_private;
        type_manifest = manifest;
        type_kind = kind;
        type_constraints = constraints;
      }, rest''''))
  | _ -> Error "Expected type name"

and parse_type_params acc tokens =
  match tokens with
  | Token.Quote :: Token.Ident param :: rest when validate_lowercase_ident param -> 
      parse_type_params ({ Parse_tree.param_name = param; param_variance = None } :: acc) rest
  | Token.LParen :: Token.Quote :: Token.Ident param :: Token.Comma :: rest when validate_lowercase_ident param ->
      (* Multiple parameters: ('a, 'b) *)
      parse_multi_type_params [{ Parse_tree.param_name = param; param_variance = None }] rest
  | _ -> (List.rev acc, tokens)

and parse_multi_type_params acc tokens =
  match tokens with
  | Token.Quote :: Token.Ident param :: Token.Comma :: rest when validate_lowercase_ident param ->
      parse_multi_type_params ({ Parse_tree.param_name = param; param_variance = None } :: acc) rest
  | Token.Quote :: Token.Ident param :: Token.RParen :: rest when validate_lowercase_ident param ->
      (List.rev ({ Parse_tree.param_name = param; param_variance = None } :: acc), rest)
  | _ -> (List.rev acc, tokens)

and parse_type_kind_and_constraints tokens =
  match tokens with
  | Token.Eq :: Token.Bar :: rest -> 
      (* Variant type *)
      let (constrs, rest') = parse_constructor_declarations [] rest in
      (Parse_tree.TypeVariant constrs, [], rest')
  | Token.Eq :: Token.LBrace :: rest ->
      (* Record type *)
      let (fields, rest') = parse_field_declarations [] rest in
      (Parse_tree.TypeRecord fields, [], rest')
  | Token.Eq :: Token.DotDot :: rest ->
      (* Open type *)
      (Parse_tree.TypeOpen, [], rest)
  | _ -> 
      (* Abstract type *)
      (Parse_tree.TypeAbstract, [], tokens)

and parse_constructor_declarations acc tokens =
  match parse_constructor_declaration tokens with
  | Ok (decl, Token.Bar :: rest) -> parse_constructor_declarations (decl :: acc) rest
  | Ok (decl, rest) -> (List.rev (decl :: acc), rest)
  | Error _ -> ([], tokens)

and parse_constructor_declaration tokens =
  match tokens with
  | Token.Ident name :: rest when validate_capitalized_ident name -> (
      match rest with
      | Token.Of :: rest' -> (
          match parse_constructor_args rest' with
          | Ok (args, rest'') -> 
              Ok ({ Parse_tree.constr_name = name; constr_args = args; constr_ret = None }, rest'')
          | Error err -> Error err)
      | _ -> 
          Ok ({ Parse_tree.constr_name = name; constr_args = Parse_tree.NoArgs; constr_ret = None }, rest))
  | _ -> Error "Expected constructor name"

and parse_constructor_args tokens =
  match parse_typexpr tokens with
  | Ok (t, rest) -> (
      match parse_more_constructor_args [t] rest with
      | Ok (types, rest') -> Ok (Parse_tree.TupleArgs types, rest')
      | Error _ -> Ok (Parse_tree.TupleArgs [t], rest))
  | Error err -> Error err

and parse_more_constructor_args acc tokens =
  match tokens with
  | Token.InfixOp "*" :: rest -> (
      match parse_typexpr rest with
      | Ok (t, rest') -> parse_more_constructor_args (t :: acc) rest'
      | Error err -> Error err)
  | _ -> Ok (List.rev acc, tokens)

and parse_field_declarations acc tokens =
  match parse_field_declaration tokens with
  | Ok (decl, Token.Semicolon :: rest) -> parse_field_declarations (decl :: acc) rest
  | Ok (decl, Token.RBrace :: rest) -> (List.rev (decl :: acc), rest)
  | Ok (decl, rest) -> (List.rev (decl :: acc), rest) (* Handle other cases *)
  | Error _ -> ([], tokens)

and parse_field_declaration tokens =
  let (is_mutable, rest) = 
    match tokens with
    | Token.Mutable :: rest' -> (true, rest')
    | _ -> (false, tokens)
  in
  match rest with
  | Token.Ident field_name :: Token.Colon :: rest' when validate_lowercase_ident field_name -> (
      match parse_typexpr rest' with
      | Ok (field_type, rest'') ->
          Ok ({ Parse_tree.field_name; field_mutable = is_mutable; field_type }, rest'')
      | Error err -> Error err)
  | _ -> Error "Expected field declaration"

and parse_exception_def tokens =
  match parse_constructor_declaration tokens with
  | Ok (decl, rest) -> Ok (Parse_tree.ExceptionDef decl, rest)
  | Error err -> Error err

and parse_module_type_def tokens =
  match tokens with
  | Token.Ident name :: Token.Eq :: rest when validate_capitalized_ident name -> (
      match parse_module_type rest with
      | Ok (mt, rest') -> Ok (Parse_tree.ModuleTypeDef (name, mt), rest')
      | Error err -> Error err)
  | _ -> Error "Expected module type definition"

and parse_module_def tokens =
  match tokens with
  | Token.Ident mod_name :: Token.Eq :: rest when validate_capitalized_ident mod_name -> (
      match parse_module_expr rest with
      | Ok (expr, rest') -> Ok (Parse_tree.ModuleDef (mod_name, expr), rest')
      | Error err -> Error err)
  | _ -> Error "Expected module definition"

and parse_module_expr tokens =
  match tokens with
  | Token.Struct :: rest -> parse_module_struct rest
  | Token.Ident mod_name :: rest when validate_capitalized_ident mod_name ->
      Ok (Parse_tree.ModuleExprPath [mod_name], rest)
  | _ -> Error "Expected module expression"

and parse_module_struct tokens =
  let (defs, rest) = parse_module_structure [] tokens in
  match rest with
  | Token.End :: rest' -> Ok (Parse_tree.ModuleExprStruct defs, rest')
  | _ -> Error "Expected 'end' after struct"

and parse_module_structure acc tokens =
  match tokens with
  | Token.End :: _ -> (List.rev acc, tokens)
  | _ -> (
      match parse_definition tokens with
      | Ok (def, rest) -> parse_module_structure (def :: acc) rest
      | Error _ -> (List.rev acc, tokens))

and parse_class_expr tokens =
  match tokens with
  | Token.Object :: rest -> (
      (* object class-body end *)
      match parse_class_field_list [] rest with
      | Ok (fields, Token.End :: rest') ->
          Ok (Parse_tree.ClassObject fields, rest')
      | _ -> Error "Expected 'end' after class object")
  | Token.Ident name :: rest when validate_lowercase_ident name ->
      Ok (Parse_tree.ClassPath (None, name), rest)
  | _ -> Error "Expected class expression"

and parse_class_field_list acc tokens =
  match tokens with
  | Token.End :: _ -> Ok (List.rev acc, tokens)
  | _ -> (
      match parse_class_field tokens with
      | Ok (field, rest) -> parse_class_field_list (field :: acc) rest
      | Error _ -> Ok (List.rev acc, tokens))

and parse_class_field tokens =
  match tokens with
  | Token.Inherit :: rest -> (
      match parse_class_path rest with
      | Ok (path, rest') ->
          let class_expr : Parse_tree.class_expr = Parse_tree.ClassPath path in
          Ok (Parse_tree.InheritField (class_expr, None), rest')
      | Error err -> Error err)
  | Token.Val :: rest -> (
      match rest with
      | Token.Ident name :: Token.Eq :: rest' when validate_lowercase_ident name -> (
          match parse_expr rest' with
          | Ok (expr, rest'') ->
              Ok (Parse_tree.ValField (false, name, None, expr), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected val field")
  | Token.Method :: rest -> (
      match rest with
      | Token.Ident name :: Token.Eq :: rest' when validate_lowercase_ident name -> (
          match parse_expr rest' with
          | Ok (expr, rest'') ->
              Ok (Parse_tree.MethodField (false, name, [], None, expr), rest'')
          | Error err -> Error err)
      | _ -> Error "Expected method field")
  | _ -> Error "Expected class field"

and parse_class_def tokens =
  (* Simplified class definition parsing *)
  match tokens with
  | Token.Ident class_name :: Token.Eq :: rest when validate_lowercase_ident class_name -> (
      match parse_class_expr rest with
      | Ok (expr, rest') -> 
          let def = {
            Parse_tree.class_virtual = false;
            class_params = [];
            class_name;
            class_args = [];
            class_type_constraint = None;
            class_expr = expr;
          } in
          Ok (Parse_tree.ClassDef [def], rest')
      | Error err -> Error err)
  | _ -> Error "Expected class definition"

and parse_open_def tokens =
  match parse_module_path tokens with
  | Ok (path, rest) -> Ok (Parse_tree.OpenDef path, rest)
  | Error err -> Error err

and parse_include_def tokens =
  match parse_module_expr tokens with
  | Ok (expr, rest) -> Ok (Parse_tree.IncludeDef expr, rest)
  | Error err -> Error err

(* Module type parsing functions *)
and parse_module_type tokens =
  match tokens with
  | Token.Sig :: rest -> parse_module_signature rest
  | Token.Ident name :: rest when validate_capitalized_ident name ->
      Ok (Parse_tree.ModuleTypePath (None, name), rest)
  | _ -> Error "Expected module type"

and parse_module_signature tokens =
  let (specs, rest) = parse_specification_list [] tokens in
  match rest with
  | Token.End :: rest' -> Ok (Parse_tree.ModuleTypeSignature specs, rest')
  | _ -> Error "Expected 'end' after signature"

and parse_specification_list acc tokens =
  match tokens with
  | Token.End :: _ -> (List.rev acc, tokens)
  | _ -> (
      match parse_specification tokens with
      | Ok (spec, rest) -> parse_specification_list (spec :: acc) rest
      | Error _ -> (List.rev acc, tokens))

and parse_specification tokens =
  match tokens with
  | Token.Val :: rest -> parse_value_specification rest
  | Token.Type :: rest -> parse_type_specification rest
  | Token.Exception :: rest -> parse_exception_specification rest
  | Token.Module :: rest -> parse_module_specification rest
  | Token.Open :: rest -> parse_open_specification rest
  | _ -> Error "Expected specification"

and parse_value_specification tokens =
  match tokens with
  | Token.Ident name :: Token.Colon :: rest when validate_lowercase_ident name -> (
      match parse_typexpr rest with
      | Ok (t, rest') -> Ok (Parse_tree.ValueSpec (name, t), rest')
      | Error err -> Error err)
  | _ -> Error "Expected value specification"

and parse_type_specification tokens =
  match parse_type_declarations [] tokens with
  | Ok (decls, rest) -> Ok (Parse_tree.TypeSpec decls, rest)
  | Error err -> Error err

and parse_exception_specification tokens =
  match parse_constructor_declaration tokens with
  | Ok (decl, rest) -> Ok (Parse_tree.ExceptionSpec decl, rest)
  | Error err -> Error err

and parse_module_specification tokens =
  match tokens with
  | Token.Ident name :: Token.Colon :: rest when validate_capitalized_ident name -> (
      match parse_module_type rest with
      | Ok (mt, rest') -> Ok (Parse_tree.ModuleSpec (name, mt), rest')
      | Error err -> Error err)
  | _ -> Error "Expected module specification"

and parse_open_specification tokens =
  match parse_module_path tokens with
  | Ok (path, rest) -> Ok (Parse_tree.OpenSpec path, rest)
  | Error err -> Error err

(* Public definition parsing functions *)
let parse_definition_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> (
      match parse_definition tokens with
      | Ok (def, []) -> Ok def
      | Ok (_, remaining) ->
          Error ("Unexpected tokens remaining: " ^
                String.concat ~sep:" " (List.map remaining ~f:string_of_token))
      | Error err -> Error err)
  | Error e -> Error e

let parse_definition_string str source =
  let lexer = Lexer.create str source in
  parse_definition_from_lexer lexer

let parse_module_structure_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> (
      let (defs, remaining) = parse_module_structure [] tokens in
      match remaining with
      | [] -> Ok defs
      | _ -> Error ("Unexpected tokens remaining: " ^
                   String.concat ~sep:" " (List.map remaining ~f:string_of_token)))
  | Error e -> Error e

let parse_module_structure_string str source =
  let lexer = Lexer.create str source in
  parse_module_structure_from_lexer lexer
