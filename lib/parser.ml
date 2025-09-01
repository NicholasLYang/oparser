open Base
open Token
open Parse_tree

type 'a parse_result = ('a, string) Result.t

(* Check if token is a synchronization point *)
let is_sync_token = function
  | Token.Semicolon | Token.RParen | Token.RBrace | Token.RBracket | Token.In
  | Token.Then | Token.Else | Token.Done | Token.End | Token.With | Token.Or
  | Token.And | Token.RightArrow | Token.Comma | Token.Dot | Token.Gt ->
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
            | Token.Semicolon, Token.Semicolon -> true
            | Token.RParen, Token.RParen -> true
            | Token.RBrace, Token.RBrace -> true
            | Token.RBracket, Token.RBracket -> true
            | Token.In, Token.In -> true
            | Token.Then, Token.Then -> true
            | Token.Else, Token.Else -> true
            | Token.Done, Token.Done -> true
            | Token.End, Token.End -> true
            | Token.With, Token.With -> true
            | Token.Or, Token.Or -> true
            | Token.And, Token.And -> true
            | Token.RightArrow, Token.RightArrow -> true
            | Token.Comma, Token.Comma -> true
            | Token.Dot, Token.Dot -> true
            | Token.Gt, Token.Gt -> true
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
  | Token.InfixOp s when validate_operator_name s -> Ok s
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
      let _ = skip_until_tokens [ Token.RParen; Token.Semicolon ] tokens in
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
      match
        skip_until_tokens [ Token.RParen; Token.Semicolon; Token.Comma ] tokens
      with
      | Token.RParen :: rest -> Ok rest
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
              | Error e -> Error e)
          | Error e -> Error e)
      | Error e -> Error e)
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
          | Error e -> Error e)
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
        | Error e -> Error e)
    | _ -> Error "Invalid module path pattern"
  else
    (* No module path pattern detected, just parse name *)
    match consume_token tokens with
    | Some token, rest -> (
        match parse_name token with
        | Ok name -> Ok ((None, name), rest)
        | Error e -> Error e)
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
                      | Error e -> Error e)
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
                      | Error e -> Error e)
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
          | Error e -> Error e)
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
          | Error e -> Error e)
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
          | Error e -> Error e)
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
          | Error e -> Error e)
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
      | Error e -> Error e)
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
      | Error e -> Error e)

(* Parse parenthesized patterns: ( pattern ) *)
and parse_parenthesized_pattern tokens =
  match consume_token tokens with
  | Some LParen, rest -> (
      match parse_pattern rest with
      | Ok (p, rest') -> (
          match expect_rparen rest' with
          | Ok rest'' -> Ok (make_parenthesized_pattern p, rest'')
          | Error e -> (
              (* Try to recover by finding the closing paren *)
              match skip_until_tokens [ Token.RParen ] rest' with
              | Token.RParen :: rest'' ->
                  Ok (make_parenthesized_pattern p, rest'')
              | _ -> Error e))
      | Error e -> (
          (* Skip to closing paren and continue *)
          match skip_until_tokens [ Token.RParen ] rest with
          | Token.RParen :: _ -> Error "Invalid pattern in parentheses"
          | _ -> Error e))
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
              | Error e -> (
                  (* Skip to next semicolon or closing bracket *)
                  match
                    skip_until_tokens [ Token.Semicolon; Token.RBracket ] rest
                  with
                  | Token.Semicolon :: rest' ->
                      parse_list_pattern_continuation acc rest'
                  | Token.RBracket :: rest' ->
                      Ok (make_list_pattern (List.rev acc), rest')
                  | _ -> Error e)))
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
          | Error e -> Error e)
      | _ -> Error "Expected '|'")
  | _ -> (
      (* Non-empty array *)
      match parse_pattern tokens with
      | Ok (first, rest) -> parse_array_pattern_continuation [ first ] rest
      | Error e -> Error e)

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
                  | Error e -> Error e)
              | _ -> Error "Expected '|'")
          | _ -> (
              match parse_pattern rest with
              | Ok (p, rest') ->
                  parse_array_pattern_continuation (p :: acc) rest'
              | Error e -> Error e))
      | _ -> Error "Expected ';'")
  | Some Or -> (
      match consume_token tokens with
      | Some Or, rest -> (
          match expect_rbracket rest with
          | Ok rest' -> Ok (make_array_pattern (List.rev acc), rest')
          | Error e -> Error e)
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
          | Error e -> Error e)
      | Error e -> Error e)
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
                  | Error e -> (
                      (* Try to recover *)
                      match skip_until_tokens [ Token.RBrace ] rest' with
                      | Token.RBrace :: rest'' ->
                          Ok (make_record_pattern (List.rev acc) true, rest'')
                      | _ -> Error e))
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
              | Error e -> (
                  (* Skip to next semicolon or closing brace *)
                  match
                    skip_until_tokens [ Token.Semicolon; Token.RBrace ] rest
                  with
                  | Token.Semicolon :: rest' ->
                      parse_record_pattern_continuation acc rest'
                  | Token.RBrace :: rest' ->
                      Ok (make_record_pattern (List.rev acc) false, rest')
                  | _ -> Error e)))
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
      match skip_until_tokens [ Token.RBrace; Token.Semicolon ] tokens with
      | Token.RBrace :: rest -> Ok rest
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
      | Error e -> Error e)
  | _ -> Error "Expected 'lazy'"

(* Parse exception patterns: exception pattern *)
and parse_exception_pattern tokens =
  match consume_token tokens with
  | Some Exception, rest -> (
      match parse_primary_pattern rest with
      | Ok (p, rest') -> Ok (make_exception_pattern p, rest')
      | Error e -> Error e)
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
          | Error e -> Error e)
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
          | Error e -> Error e)
      | Error e -> Error e)
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
          | Error e -> Error e)
      | Error e -> Error e)
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
          | Error e -> Error e)
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
                      | Error e -> Error e)
                  | Error e -> Error e)
              | Error e -> Error e)
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
              | Error e -> Error e)
          | Error e -> Error e)
      | Error e -> Error e)
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
      | Some (Token.InfixOp "*"), rest -> (
          match parse_app_type rest with
          | Ok (t, rest') -> parse_type_tuple_continuation (t :: acc) rest'
          | Error e -> Error e)
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
          | Error e -> Error e)
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
                      | Error e -> Error e)
                  | Error e -> Error e)
              | Error e -> Error e)
          | Some RParen -> (
              (* Single parenthesized type *)
              match consume_token rest' with
              | Some RParen, rest'' -> Ok (make_parenthesized t, rest'')
              | _ -> Error "Expected ')'")
          | _ -> Error "Expected ')' or ','")
      | Error e -> Error e)
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
              | Error e -> Error e)
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
              | Error e -> Error e)
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
                  | Error e -> Error e)
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
              | Error e -> Error e))
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
      | Error e -> Error e)
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
                      | Error e -> Error e)
                  | None -> Error "Expected class name after module path")
              | _ -> Error "Expected class name after module path")
          | Error _ -> Error "Expected class name"))
  | _ -> Error "Expected class name"

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
      | Error e -> Error e)
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
      | Error e -> Error e)
  | Error e -> Error e

let parse_pattern_string str source =
  let lexer = Lexer.create str source in
  parse_pattern_from_lexer lexer

(* Helper function to convert token number to parse tree constant *)
let _number_to_constant = function
  | Token.Int i -> Parse_tree.IntegerLiteral i
  | Token.Int32 i -> Parse_tree.Int32Literal i
  | Token.Int64 i -> Parse_tree.Int64Literal i
  | Token.NativeInt i -> Parse_tree.NativeIntLiteral i
  | Token.Float f -> Parse_tree.FloatLiteral f

(* Expression parsing functions - NOT IMPLEMENTED YET *)

(*
(* Parse primary expressions (highest precedence) *)
let rec parse_primary_expr tokens =
  match tokens with
  | [] -> Error "Expected expression"
  | Number n :: rest -> Ok (ConstantExpr (number_to_constant n), rest)
  | Char c :: rest -> Ok (ConstantExpr (CharLiteral c), rest)
  | String s :: rest -> Ok (ConstantExpr (StringLiteral s), rest)
  | True :: rest -> Ok (ConstantExpr True, rest)
  | False :: rest -> Ok (ConstantExpr False, rest)
  | LParen :: RParen :: rest -> Ok (ConstantExpr Unit, rest)
  | Begin :: End :: rest -> Ok (ConstantExpr BeginEnd, rest)
  | LBracket :: RBracket :: rest -> Ok (ConstantExpr EmptyList, rest)
  | LBracket :: Bar :: RBracket :: rest -> Ok (ConstantExpr EmptyArray, rest)
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
          | Ok (arg, rest') -> Ok (PolymorphicVariantExpr (tag, Some arg), rest')
          | Error e -> Error e)
      | _ -> Ok (PolymorphicVariantExpr (tag, None), rest))
  | LParen :: rest -> (
      (* Could be parenthesized expr, tuple, or type constraint *)
      match parse_expr rest with
      | Ok (e, Colon :: rest') -> (
          (* Type constraint *)
          match parse_typexpr rest' with
          | Ok (t, RParen :: rest'') -> Ok (TypeConstraint (e, t), rest'')
          | Ok (_, _) -> Error "Expected ')' after type constraint"
          | Error e -> Error e)
      | Ok (e, Comma :: rest') ->
          (* Tuple *)
          parse_tuple_elements [ e ] rest'
      | Ok (e, RParen :: rest') -> Ok (ParenthesizedExpr e, rest')
      | Ok (_, _) ->
          Error "Expected ')', ',' or ':' in parenthesized expression"
      | Error e -> Error e)
  | Begin :: rest -> (
      match parse_expr rest with
      | Ok (e, End :: rest') -> Ok (BeginEndExpr e, rest')
      | Ok (_, _) -> Error "Expected 'end' after 'begin'"
      | Error e -> Error e)
  | LBracket :: rest -> (
      match rest with
      | Bar :: _ -> parse_array_expr rest
      | _ -> parse_list_expr rest)
  | LBrace :: rest -> parse_record_expr rest
  | Lazy :: rest -> (
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (Lazy e, rest')
      | Error e -> Error e)
  | Assert :: rest -> (
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (Assert e, rest')
      | Error e -> Error e)
  | token :: _ -> (
      (* Try to parse as value path or constructor *)
      match parse_value_path tokens with
      | Ok (path, rest) -> Ok (ValuePathExpr path, rest)
      | Error _ -> (
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
                      Ok (ConstructorExpr (path, Some arg), rest')
                  | Error _ -> Ok (ConstructorExpr (path, None), rest))
              | _ -> Ok (ConstructorExpr (path, None), rest))
          | Error e -> Error e))

and parse_tuple_elements acc tokens =
  match parse_expr tokens with
  | Ok (e, Comma :: rest) -> parse_tuple_elements (e :: acc) rest
  | Ok (e, RParen :: rest) -> Ok (TupleExpr (List.rev (e :: acc)), rest)
  | Ok (_, _) -> Error "Expected ',' or ')' in tuple"
  | Error e -> Error e

and parse_list_expr tokens =
  match tokens with
  | RBracket :: rest -> Ok (ListExpr [], rest)
  | _ -> parse_list_elements [] tokens

and parse_list_elements acc tokens =
  match parse_expr tokens with
  | Ok (e, Semicolon :: rest) -> parse_list_elements (e :: acc) rest
  | Ok (e, RBracket :: rest) -> Ok (ListExpr (List.rev (e :: acc)), rest)
  | Ok (_, _) -> Error "Expected ';' or ']' in list"
  | Error e -> Error e

and parse_array_expr tokens =
  match tokens with
  | Bar :: RBracket :: rest -> Ok (ArrayExpr [], rest)
  | _ -> parse_array_elements [] tokens

and parse_array_elements acc tokens =
  match parse_expr tokens with
  | Ok (e, Semicolon :: rest) -> parse_array_elements (e :: acc) rest
  | Ok (e, Bar :: RBracket :: rest) -> Ok (ArrayExpr (List.rev (e :: acc)), rest)
  | Ok (_, _) -> Error "Expected ';' or '|]' in array"
  | Error e -> Error e

and parse_record_expr tokens =
  match tokens with
  | RBrace :: rest -> Ok (RecordExpr [], rest)
  | _ -> (
      match parse_expr tokens with
      | Ok (e, With :: rest) -> (
          (* Record update *)
          match parse_record_fields [] rest with
          | Ok (fields, rest') -> Ok (RecordUpdate (e, fields), rest')
          | Error e -> Error e)
      | Error _ -> (
          (* Regular record *)
          match parse_record_fields [] tokens with
          | Ok (fields, rest) -> Ok (RecordExpr fields, rest)
          | Error e -> Error e))

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
                    { field_path; type_constraint = Some t; expr = Some e }
                  in
                  parse_record_fields_cont (field :: acc) rest'''
              | Error e -> Error e)
          | Ok (t, rest'') ->
              let field =
                { field_path; type_constraint = Some t; expr = None }
              in
              parse_record_fields_cont (field :: acc) rest''
          | Error e -> Error e)
      | Eq :: rest' -> (
          match parse_expr rest' with
          | Ok (e, rest'') ->
              let field =
                { field_path; type_constraint = None; expr = Some e }
              in
              parse_record_fields_cont (field :: acc) rest''
          | Error e -> Error e)
      | _ ->
          let field = { field_path; type_constraint = None; expr = None } in
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
                  parse_postfix_cont (ArrayUpdate (expr, idx, value)) rest'''
              | Error e -> Error e)
          | Ok (idx, RParen :: rest'') ->
              (* Array access *)
              parse_postfix_cont (ArrayAccess (expr, idx)) rest''
          | _ -> Error "Expected ')' after array index")
      | LBracket :: rest' -> (
          (* String access *)
          match parse_expr rest' with
          | Ok (idx, RBracket :: LeftArrow :: rest'') -> (
              (* String update *)
              match parse_expr rest'' with
              | Ok (value, rest''') ->
                  parse_postfix_cont (StringUpdate (expr, idx, value)) rest'''
              | Error e -> Error e)
          | Ok (idx, RBracket :: rest'') ->
              (* String access *)
              parse_postfix_cont (StringAccess (expr, idx)) rest''
          | _ -> Error "Expected ']' after string index")
      | _ -> (
          (* Field access *)
          match parse_field rest with
          | Ok (field, LeftArrow :: rest') -> (
              (* Field update *)
              match parse_expr rest' with
              | Ok (value, rest'') ->
                  parse_postfix_cont (FieldUpdate (expr, field, value)) rest''
              | Error e -> Error e)
          | Ok (field, rest') ->
              (* Field access *)
              parse_postfix_cont (FieldAccess (expr, field)) rest'
          | Error e -> Error e))
  | _ ->
      (* Try function application *)
      parse_function_app expr [] tokens

and parse_function_app func args tokens =
  match parse_argument tokens with
  | Ok (arg, rest) -> parse_function_app func (arg :: args) rest
  | Error _ ->
      if List.is_empty args then Ok (func, tokens)
      else Ok (FunctionApp (func, List.rev args), tokens)

and parse_argument tokens =
  match tokens with
  | Tilde :: Ident label :: Colon :: rest -> (
      (* Labeled argument ~label:expr *)
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (LabeledArg (label, e), rest')
      | Error e -> Error e)
  | Tilde :: Ident label :: rest ->
      (* Punned labeled argument ~label *)
      Ok (LabeledArg (label, ValuePathExpr (None, label)), rest)
  | Question :: Ident label :: Colon :: rest -> (
      (* Optional argument ?label:expr *)
      match parse_primary_expr rest with
      | Ok (e, rest') -> Ok (OptionalArg (label, Some e), rest')
      | Error e -> Error e)
  | Question :: Ident label :: rest ->
      (* Optional argument without value ?label *)
      Ok (OptionalArg (label, None), rest)
  | _ -> (
      (* Simple argument *)
      match parse_primary_expr tokens with
      | Ok (e, rest) -> Ok (SimpleArg e, rest)
      | Error e -> Error e)

(* Parse prefix operators *)
and parse_prefix_expr tokens =
  match tokens with
  | Minus :: rest -> (
      match parse_postfix_expr rest with
      | Ok (e, rest') -> Ok (PrefixOp ("-", e), rest')
      | Error e -> Error e)
  | MinusDot :: rest -> (
      match parse_postfix_expr rest with
      | Ok (e, rest') -> Ok (PrefixOp ("-.", e), rest')
      | Error e -> Error e)
  | Plus :: rest -> (
      match parse_postfix_expr rest with
      | Ok (e, rest') -> Ok (PrefixOp ("+", e), rest')
      | Error e -> Error e)
  | _ -> parse_postfix_expr tokens

(* Parse infix operators with precedence *)
and parse_infix_expr tokens =
  match parse_prefix_expr tokens with
  | Ok (left, rest) -> parse_infix_cont left rest
  | Error e -> Error e

and parse_infix_cont left tokens =
  match tokens with
  | Token.InfixOp op :: rest ->
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
      | Token.InfixOp op2 :: _ when operator_precedence op2 > prec -> (
          (* Higher precedence operator, parse it first *)
          match parse_infix_cont right rest' with
          | Ok (right', rest'') ->
              parse_infix_cont (Parse_tree.InfixOp (left, op, right')) rest''
          | Error e -> Error e)
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
      | Ok (tail, rest') -> Ok (ConsExpr (head, tail), rest')
      | Error e -> Error e)
  | result -> result

(* Parse tuple expressions *)
and parse_tuple_expr tokens =
  match parse_cons_expr tokens with
  | Ok (e1, Comma :: rest) -> parse_tuple_rest [ e1 ] rest
  | result -> result

and parse_tuple_rest acc tokens =
  match parse_cons_expr tokens with
  | Ok (e, Comma :: rest) -> parse_tuple_rest (e :: acc) rest
  | Ok (e, rest) -> Ok (TupleExpr (List.rev (e :: acc)), rest)
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
                  Ok (IfThenElse (cond, then_e, Some else_e), rest''')
              | Error e -> (
                  (* Try to recover by finding next sync point *)
                  match
                    skip_until_tokens
                      [ Token.Semicolon; Token.In; Token.End ]
                      rest''
                  with
                  | rest''' -> Ok (IfThenElse (cond, then_e, None), rest''')))
          | Ok (then_e, rest'') -> Ok (IfThenElse (cond, then_e, None), rest'')
          | Error e -> (
              (* Skip to else or next sync point *)
              match
                skip_until_tokens
                  [ Token.Else; Token.Semicolon; Token.In; Token.End ]
                  rest'
              with
              | Token.Else :: rest'' -> (
                  match parse_expr rest'' with
                  | Ok (else_e, rest''') ->
                      Ok (IfThenElse (cond, then_e, Some else_e), rest''')
                  | Error _ -> Error e)
              | rest'' -> Error e))
      | _ -> Error "Expected 'then' after 'if' condition")
  | While :: rest -> (
      match parse_expr rest with
      | Ok (cond, Do :: rest') -> (
          match parse_expr rest' with
          | Ok (body, Done :: rest'') -> Ok (While (cond, body), rest'')
          | _ -> Error "Expected 'done' after 'while' body")
      | _ -> Error "Expected 'do' after 'while' condition")
  | For :: Ident var :: Eq :: rest -> (
      match parse_expr rest with
      | Ok (start, To :: rest') -> (
          match parse_expr rest' with
          | Ok (stop, Do :: rest'') -> (
              match parse_expr rest'' with
              | Ok (body, Done :: rest''') ->
                  Ok (For (var, start, To, stop, body), rest''')
              | _ -> Error "Expected 'done' after 'for' body")
          | _ -> Error "Expected 'do' after 'for' range")
      | Ok (start, Downto :: rest') -> (
          match parse_expr rest' with
          | Ok (stop, Do :: rest'') -> (
              match parse_expr rest'' with
              | Ok (body, Done :: rest''') ->
                  Ok (For (var, start, Downto, stop, body), rest''')
              | _ -> Error "Expected 'done' after 'for' body")
          | _ -> Error "Expected 'do' after 'for' range")
      | _ -> Error "Expected 'to' or 'downto' in 'for' loop")
  | For :: _ -> Error "Expected identifier after 'for'"
  | Match :: rest -> (
      match parse_expr rest with
      | Ok (e, With :: rest') -> (
          match parse_cases rest' with
          | Ok (cases, rest'') -> Ok (Match (e, cases), rest'')
          | Error e -> (
              (* Try to recover by finding next sync point *)
              match
                skip_until_tokens [ Token.Semicolon; Token.In; Token.End ] rest'
              with
              | rest'' -> Ok (Match (e, []), rest'')))
      | _ -> Error "Expected 'with' after 'match' expression")
  | Function :: rest -> (
      match parse_cases rest with
      | Ok (cases, rest') -> Ok (Function cases, rest')
      | Error e -> Error e)
  | Try :: rest -> (
      match parse_expr rest with
      | Ok (e, With :: rest') -> (
          match parse_cases rest' with
          | Ok (cases, rest'') -> Ok (Try (e, cases), rest'')
          | Error e -> Error e)
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
                  let case = { pattern; guard = Some guard; expr } in
                  parse_case_list_cont (case :: acc) rest'''
              | Error e -> (
                  (* Skip to next bar or sync point *)
                  match
                    skip_until_tokens
                      [ Token.Or; Token.Semicolon; Token.In; Token.End ]
                      rest''
                  with
                  | Token.Or :: rest''' -> parse_case_list acc rest'''
                  | rest''' -> Ok (List.rev acc, rest''')))
          | _ -> Error "Expected '->' after 'when' clause")
      | RightArrow :: rest' -> (
          match parse_expr rest' with
          | Ok (expr, rest'') ->
              let case = { pattern; guard = None; expr } in
              parse_case_list_cont (case :: acc) rest''
          | Error e -> (
              (* Skip to next bar or sync point *)
              match
                skip_until_tokens
                  [ Token.Or; Token.Semicolon; Token.In; Token.End ]
                  rest'
              with
              | Token.Or :: rest'' -> parse_case_list acc rest''
              | rest'' -> Ok (List.rev acc, rest'')))
      | _ -> Error "Expected '->' or 'when' after pattern")
  | Error e -> (
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
                    Ok (Lambda (List.rev acc, Some t, body), rest'')
                | Error e -> Error e)
            | _ -> Error "Expected '->' after return type")
        | RightArrow :: rest -> (
            match parse_expr rest with
            | Ok (body, rest') -> Ok (Lambda (List.rev acc, None, body), rest')
            | Error e -> Error e)
        | _ -> Error "Expected '->' or ':' after parameters")

and parse_parameter tokens =
  match tokens with
  | LParen :: Type :: rest -> (
      (* Type parameter *)
      match parse_typexpr rest with
      | Ok (t, RParen :: rest') -> Ok (TypeParam t, rest')
      | _ -> Error "Expected ')' after type parameter")
  | Tilde :: Ident label :: Colon :: rest -> (
      (* Labeled parameter *)
      match parse_pattern rest with
      | Ok (p, rest') -> Ok (LabeledParam (label, p), rest')
      | Error e -> Error e)
  | Question :: Ident label :: Colon :: rest -> (
      (* Optional parameter with pattern *)
      match parse_pattern rest with
      | Ok (p, Eq :: rest') -> (
          match parse_expr rest' with
          | Ok (default, rest'') ->
              Ok (OptionalParam (label, p, Some default), rest'')
          | Error e -> Error e)
      | Ok (p, rest') -> Ok (OptionalParam (label, p, None), rest')
      | Error e -> Error e)
  | Question :: Ident label :: rest ->
      (* Optional parameter without pattern *)
      Ok (OptionalParam (label, ValueName label, None), rest)
  | _ -> (
      (* Simple parameter *)
      match parse_pattern tokens with
      | Ok (p, rest) -> Ok (SimpleParam p, rest)
      | Error e -> Error e)

(* Parse let expressions *)
and parse_let_expr tokens =
  match tokens with
  | Let :: Rec :: rest -> (
      match parse_let_bindings [] rest with
      | Ok (bindings, In :: rest') -> (
          match parse_expr rest' with
          | Ok (body, rest'') -> Ok (LetRec (bindings, body), rest'')
          | Error e -> Error e)
      | _ -> Error "Expected 'in' after let bindings")
  | Let :: Exception :: Ident name :: rest -> (
      (* let exception *)
      match rest with
      | In :: rest' -> (
          match parse_expr rest' with
          | Ok (body, rest'') -> Ok (LetException (name, None, body), rest'')
          | Error e -> Error e)
      | _ -> Error "Expected 'in' after exception declaration")
  | Let :: Module :: Ident name :: rest ->
      (* let module - simplified for now *)
      Error "let module not yet implemented"
  | Let :: rest -> (
      match parse_let_bindings [] rest with
      | Ok (bindings, In :: rest') -> (
          match parse_expr rest' with
          | Ok (body, rest'') -> Ok (Let (bindings, body), rest'')
          | Error e -> Error e)
      | _ -> Error "Expected 'in' after let bindings")
  | _ -> parse_fun_expr tokens

and parse_let_bindings acc tokens =
  match parse_let_binding tokens with
  | Ok (binding, And :: rest) -> parse_let_bindings (binding :: acc) rest
  | Ok (binding, rest) -> Ok (List.rev (binding :: acc), rest)
  | Error e -> (
      (* Try to recover by skipping to 'in' or 'and' *)
      match skip_until_tokens [ Token.In; Token.And ] tokens with
      | Token.And :: rest -> parse_let_bindings acc rest
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
      | Colon :: rest -> (
          match parse_typexpr rest with
          | Ok (t, Eq :: rest') -> (
              match parse_expr rest' with
              | Ok (expr, rest'') ->
                  Ok
                    ( {
                        pattern;
                        params = List.rev params;
                        type_constraint = Some t;
                        expr;
                      },
                      rest'' )
              | Error e -> Error e)
          | _ -> Error "Expected '=' after type annotation")
      | Eq :: rest -> (
          match parse_expr rest with
          | Ok (expr, rest') ->
              Ok
                ( {
                    pattern;
                    params = List.rev params;
                    type_constraint = None;
                    expr;
                  },
                  rest' )
          | Error e -> Error e)
      | _ -> Error "Expected '=' or ':' in let binding")

(* Parse sequence expressions *)
and parse_sequence_expr tokens =
  match parse_let_expr tokens with
  | Ok (e1, Semicolon :: rest) -> (
      match parse_expr rest with
      | Ok (e2, rest') -> Ok (Sequence (e1, e2), rest')
      | Error e -> Error e)
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
      | Error e -> Error e)
  | Error e -> Error e

let parse_expr_string str source =
  let lexer = Lexer.create str source in
  parse_expr_from_lexer lexer
*)
