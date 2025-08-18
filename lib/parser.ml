open Base
open Token
open Parse_tree

type 'a parse_result = ('a, string) Result.t

let is_uppercase_letter c =
  Char.between c ~low:'A' ~high:'Z'

let is_lowercase_letter c =
  Char.between c ~low:'a' ~high:'z'

let is_letter c =
  is_uppercase_letter c || is_lowercase_letter c

let is_digit c =
  Char.between c ~low:'0' ~high:'9'

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
  validate_ident_with_predicate s ~first_char_pred:(fun c -> is_letter c || Char.equal c '_')

let validate_capitalized_ident s =
  validate_ident_with_predicate s ~first_char_pred:is_uppercase_letter

let validate_lowercase_ident s =
  validate_ident_with_predicate s ~first_char_pred:(fun c -> is_lowercase_letter c || Char.equal c '_')

let validate_prefix_symbol s =
  if String.is_empty s then false
  else
    match s.[0] with
    | '!' -> String.for_all s ~f:is_operator_char
    | '?' | '~' when String.length s > 1 -> 
        String.for_all (String.drop_prefix s 1) ~f:is_operator_char
    | _ -> false

let validate_infix_symbol s =
  String.for_all s ~f:is_operator_char

let is_predefined_infix_op = function
  | "*" | "+" | "-" | "-." | "=" | "!=" | "<" | ">" | "or" | "||" | "&" | "&&" | ":="
  | "mod" | "land" | "lor" | "lxor" | "lsl" | "lsr" | "asr" -> true
  | _ -> false

let validate_operator_name s =
  validate_prefix_symbol s || validate_infix_symbol s || is_predefined_infix_op s

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
  | InfixOp s when validate_operator_name s -> Ok s
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

let peek_token = function
  | [] -> None
  | t :: _ -> Some t

let consume_token = function
  | [] -> (None, [])
  | t :: rest -> (Some t, rest)

let expect_dot tokens =
  match consume_token tokens with
  | (Some Dot, rest) -> Ok rest
  | _ -> Error "Expected '.'"

let expect_lparen tokens =
  match consume_token tokens with
  | (Some LParen, rest) -> Ok rest
  | _ -> Error "Expected '('"

let expect_rparen tokens =
  match consume_token tokens with
  | (Some RParen, rest) -> Ok rest
  | _ -> Error "Expected ')'"

(* Path parsing utilities *)
let rec parse_module_path tokens =
  match consume_token tokens with
  | (Some (Ident s), rest) when validate_capitalized_ident s ->
      parse_module_path_continuation [s] rest
  | _ -> Error "Expected module name"

and parse_module_path_continuation acc tokens =
  match peek_token tokens with
  | Some Dot ->
      (match consume_token tokens with
       | (Some Dot, rest) ->
           (match consume_token rest with
            | (Some (Ident s), rest') when validate_capitalized_ident s ->
                parse_module_path_continuation (s :: acc) rest'
            | _ -> Ok (List.rev acc, tokens))  (* Don't consume the dot if next isn't module name *)
       | _ -> Ok (List.rev acc, tokens))
  | _ -> Ok (List.rev acc, tokens)

let rec parse_extended_module_name tokens =
  match consume_token tokens with
  | (Some (Ident s), rest) when validate_capitalized_ident s ->
      parse_extended_module_name_continuation s rest
  | _ -> Error "Expected module name"

and parse_extended_module_name_continuation base_name tokens =
  match peek_token tokens with
  | Some LParen ->
      (match expect_lparen tokens with
       | Ok rest ->
           (match parse_extended_module_path rest with
            | Ok (path, rest') ->
                (match expect_rparen rest' with
                 | Ok rest'' ->
                     (* Continue parsing for more parenthesized paths *)
                     let extended_name = base_name ^ "(" ^ String.concat ~sep:"." path ^ ")" in
                     parse_extended_module_name_continuation extended_name rest''
                 | Error e -> Error e)
            | Error e -> Error e)
       | Error e -> Error e)
  | _ -> Ok (base_name, tokens)

and parse_extended_module_path tokens =
  match parse_extended_module_name tokens with
  | Ok (name, rest) ->
      parse_extended_module_path_continuation [name] rest
  | Error e -> Error e

and parse_extended_module_path_continuation acc tokens =
  match peek_token tokens with
  | Some Dot ->
      (match consume_token tokens with
       | (Some Dot, rest) ->
           (match parse_extended_module_name rest with
            | Ok (name, rest') ->
                parse_extended_module_path_continuation (name :: acc) rest'
            | Error e -> Error e)
       | _ -> Ok (List.rev acc, tokens))
  | _ -> Ok (List.rev acc, tokens)

(* Helper function to check if we have a module path by looking ahead *)
let has_module_path tokens =
  match tokens with
  | (Ident s1) :: Dot :: (Ident _) :: _ when validate_capitalized_ident s1 -> true
  | _ -> false

(* Simpler approach: manually parse module.name patterns *)
let try_parse_with_module_path parse_name tokens =
  if has_module_path tokens then
    (* We have Module.Name pattern, parse it manually *)
    match tokens with
    | (Ident module_name) :: Dot :: (Ident name) :: rest when validate_capitalized_ident module_name ->
        (match parse_name (Ident name) with
         | Ok parsed_name -> Ok ((Some [module_name], parsed_name), rest)
         | Error e -> Error e)
    | _ -> Error "Invalid module path pattern"
  else
    (* No module path pattern detected, just parse name *)
    (match consume_token tokens with
     | (Some token, rest) ->
         (match parse_name token with
          | Ok name -> Ok ((None, name), rest)
          | Error e -> Error e)
     | _ -> Error "Expected name")

(* Main path parsers *)
let parse_value_path tokens =
  try_parse_with_module_path parse_value_name tokens

let parse_constr tokens =
  try_parse_with_module_path parse_constr_name tokens

let parse_typeconstr tokens =
  (* Use try_parse_with_module_path but with extended_module_path *)
  match consume_token tokens with
  | (Some token, rest) ->
      (match parse_typeconstr_name token with
       | Ok name -> Ok ((None, name), rest)
       | Error _ ->
           (* Try with extended module path *)
           (match parse_extended_module_path tokens with
            | Ok (path, rest') ->
                (match expect_dot rest' with
                 | Ok rest'' ->
                     (match consume_token rest'' with
                      | (Some token', rest''') ->
                          (match parse_typeconstr_name token' with
                           | Ok name -> Ok ((Some path, name), rest''')
                           | Error e -> Error e)
                      | _ -> Error "Expected type constructor name after module path")
                 | Error _ -> Error "Expected '.' after module path")
            | Error _ -> Error "Expected type constructor name"))
  | _ -> Error "Expected type constructor name"

let parse_field tokens =
  try_parse_with_module_path parse_field_name tokens

let parse_modtype_path tokens =
  (* Use try_parse_with_module_path but with extended_module_path *)
  match consume_token tokens with
  | (Some token, rest) ->
      (match parse_modtype_name token with
       | Ok name -> Ok ((None, name), rest)
       | Error _ ->
           (* Try with extended module path *)
           (match parse_extended_module_path tokens with
            | Ok (path, rest') ->
                (match expect_dot rest' with
                 | Ok rest'' ->
                     (match consume_token rest'' with
                      | (Some token', rest''') ->
                          (match parse_modtype_name token' with
                           | Ok name -> Ok ((Some path, name), rest''')
                           | Error e -> Error e)
                      | _ -> Error "Expected module type name after module path")
                 | Error _ -> Error "Expected '.' after module path")
            | Error _ -> Error "Expected module type name"))
  | _ -> Error "Expected module type name"

let parse_class_path tokens =
  try_parse_with_module_path parse_class_name tokens

let parse_classtype_path tokens =
  (* Use try_parse_with_module_path but with extended_module_path *)
  match consume_token tokens with
  | (Some token, rest) ->
      (match parse_class_name token with
       | Ok name -> Ok ((None, name), rest)
       | Error _ ->
           (* Try with extended module path *)
           (match parse_extended_module_path tokens with
            | Ok (path, rest') ->
                (match expect_dot rest' with
                 | Ok rest'' ->
                     (match consume_token rest'' with
                      | (Some token', rest''') ->
                          (match parse_class_name token' with
                           | Ok name -> Ok ((Some path, name), rest''')
                           | Error e -> Error e)
                      | _ -> Error "Expected class name after module path")
                 | Error _ -> Error "Expected '.' after module path")
            | Error _ -> Error "Expected class name"))
  | _ -> Error "Expected class name"


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
 */

(* Constant parsing *)

let parse_constant = function
  | Number (Int i) -> Ok (make_integer_literal i)
  | Number (Int32 i) -> Ok (make_int32_literal i) 
  | Number (Int64 i) -> Ok (make_int64_literal i)
  | Number (NativeInt i) -> Ok (make_nativeint_literal i)
  | Number (Float f) -> Ok (make_float_literal f)
  | Char c -> Ok (make_char_literal c)
  | String s -> Ok (make_string_literal s)
  | Ident name when validate_capitalized_ident name -> Ok (make_constructor name)
  | False -> Ok (make_false ())
  | True -> Ok (make_true ())
  | PolymorphicVariantTag tag -> Ok (make_polymorphic_variant_tag tag)
  | _ -> Error "Expected constant"

(* Parse compound constants that require multiple tokens *)
let parse_unit_constant tokens =
  match consume_token tokens with
  | (Some LParen, rest) ->
      (match consume_token rest with
       | (Some RParen, rest') -> Ok (make_unit (), rest')
       | _ -> Error "Expected ')' to complete unit constant")
  | _ -> Error "Expected '(' to start unit constant"

let parse_begin_end_constant tokens =
  match consume_token tokens with
  | (Some Begin, rest) ->
      (match consume_token rest with
       | (Some End, rest') -> Ok (make_begin_end (), rest')
       | _ -> Error "Expected 'end' to complete begin end constant")
  | _ -> Error "Expected 'begin' to start begin end constant"

let parse_empty_list_constant tokens =
  match consume_token tokens with
  | (Some LBracket, rest) ->
      (match consume_token rest with
       | (Some RBracket, rest') -> Ok (make_empty_list (), rest')
       | _ -> Error "Expected ']' to complete empty list constant")
  | _ -> Error "Expected '[' to start empty list constant"

let parse_empty_array_constant tokens =
  match consume_token tokens with
  | (Some LBracket, rest) ->
      (match consume_token rest with
       | (Some Or, rest') ->
           (match consume_token rest' with
            | (Some Or, rest'') ->
                (match consume_token rest'' with
                 | (Some RBracket, rest''') -> Ok (make_empty_array (), rest''')
                 | _ -> Error "Expected ']' to complete empty array constant")
            | _ -> Error "Expected second '|' in empty array constant")
       | _ -> Error "Expected '|' after '[' in empty array constant")
  | _ -> Error "Expected '[' to start empty array constant"

(* Parse a complete constant from token list *)
let parse_constant_tokens tokens =
  match peek_token tokens with
  | Some LParen ->
      parse_unit_constant tokens
  | Some Begin ->
      parse_begin_end_constant tokens
  | Some LBracket ->
      (* Could be empty list [] or empty array [||] *)
      (match tokens with
       | LBracket :: RBracket :: _ ->
           parse_empty_list_constant tokens
       | LBracket :: Or :: Or :: RBracket :: _ ->
           parse_empty_array_constant tokens
       | _ -> Error "Invalid list/array constant")
  | Some token ->
      (match consume_token tokens with
       | (Some token, rest) ->
           (match parse_constant token with
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

let rec parse_pattern tokens =
  parse_or_pattern tokens

(* Parse or patterns (lowest precedence): pattern | pattern *)
and parse_or_pattern tokens =
  match parse_alias_pattern tokens with
  | Ok (left, rest) ->
      parse_or_continuation left rest
  | Error e -> Error e

and parse_or_continuation left tokens =
  match peek_token tokens with
  | Some Or ->
      (match consume_token tokens with
       | (Some Or, rest) ->
           (match parse_or_pattern rest with
            | Ok (right, rest') ->
                Ok (make_or_pattern left right, rest')
            | Error e -> Error e)
       | _ -> Ok (left, tokens))
  | _ -> Ok (left, tokens)

(* Parse alias patterns: pattern as value-name *)
and parse_alias_pattern tokens =
  match parse_cons_pattern tokens with
  | Ok (p, rest) ->
      (match peek_token rest with
       | Some As ->
           (match consume_token rest with
            | (Some As, rest') ->
                (match consume_token rest' with
                 | (Some (Ident name), rest'') when validate_lowercase_ident name ->
                     Ok (make_pattern_alias p name, rest'')
                 | _ -> Error "Expected value name after 'as'")
            | _ -> Ok (p, rest))
       | _ -> Ok (p, rest))
  | Error e -> Error e

(* Parse cons patterns: pattern :: pattern *)
and parse_cons_pattern tokens =
  match parse_tuple_pattern tokens with
  | Ok (left, rest) ->
      parse_cons_continuation left rest
  | Error e -> Error e

and parse_cons_continuation left tokens =
  match peek_token tokens with
  | Some ColonColon ->
      (match consume_token tokens with
       | (Some ColonColon, rest) ->
           (match parse_cons_pattern rest with
            | Ok (right, rest') ->
                Ok (make_cons_pattern left right, rest')
            | Error e -> Error e)
       | _ -> Ok (left, tokens))
  | _ -> Ok (left, tokens)

(* Parse tuple patterns: pattern, pattern, ... *)
and parse_tuple_pattern tokens =
  match parse_constructor_app_pattern tokens with
  | Ok (first, rest) ->
      parse_tuple_continuation [first] rest
  | Error e -> Error e

and parse_tuple_continuation acc tokens =
  match peek_token tokens with
  | Some Comma ->
      (match consume_token tokens with
       | (Some Comma, rest) ->
           (match parse_constructor_app_pattern rest with
            | Ok (p, rest') ->
                parse_tuple_continuation (p :: acc) rest'
            | Error e -> Error e)
       | _ -> 
           if List.length acc > 1 then
             Ok (make_tuple_pattern (List.rev acc), tokens)
           else
             (match acc with
              | [single] -> Ok (single, tokens)
              | [] -> Error "Empty tuple list"
              | _ -> Ok (make_tuple_pattern (List.rev acc), tokens)))
  | _ -> 
      if List.length acc > 1 then
        Ok (make_tuple_pattern (List.rev acc), tokens)
      else
        (match acc with
         | [single] -> Ok (single, tokens)
         | [] -> Error "Empty tuple list"
         | _ -> Ok (make_tuple_pattern (List.rev acc), tokens))

(* Parse constructor application patterns: Constructor pattern *)
and parse_constructor_app_pattern tokens =
  match parse_primary_pattern tokens with
  | Ok (p, rest) ->
      (match p with
       | PatternConstructor path ->
           (* Check if next token could be a pattern (constructor application) *)
           (match peek_token rest with
            | Some (Ident _) | Some LParen | Some LBracket | Some LBrace | Some Quote ->
                (match parse_primary_pattern rest with
                 | Ok (arg_pattern, rest') ->
                     Ok (make_constructor_pattern_with_arg path arg_pattern, rest')
                 | Error _ -> Ok (p, rest))
            | _ -> Ok (p, rest))
       | _ -> Ok (p, rest))
  | Error e -> Error e

(* Parse primary patterns: atoms and compound structures *)
and parse_primary_pattern tokens =
  match peek_token tokens with
  | Some (Ident "_") ->
      (* Wildcard pattern *)
      (match consume_token tokens with
       | (Some (Ident "_"), rest) ->
           Ok (make_wildcard_pattern (), rest)
       | _ -> Error "Expected wildcard")
  | Some (Ident name) when validate_lowercase_ident name ->
      (* Value name pattern *)
      (match consume_token tokens with
       | (Some (Ident value_name), rest) ->
           Ok (make_value_name_pattern value_name, rest)
       | _ -> Error "Expected value name")
  | Some (Ident name) when validate_capitalized_ident name ->
      (* Constructor pattern *)
      (match parse_constr tokens with
       | Ok (constr_path, rest) ->
           Ok (make_constructor_pattern constr_path, rest)
       | Error e -> Error e)
  | Some LParen ->
      parse_parenthesized_pattern tokens
  | Some LBracket ->
      parse_list_or_array_pattern tokens
  | Some LBrace ->
      parse_record_pattern tokens
  | Some Quote ->
      parse_range_or_char_pattern tokens
  | Some Lazy ->
      parse_lazy_pattern tokens
  | Some Exception ->
      parse_exception_pattern tokens
  | _ ->
      (* Try to parse as constant *)
      (match parse_constant_tokens tokens with
       | Ok (constant, rest) ->
           Ok (make_pattern_constant constant, rest)
       | Error e -> Error e)

(* Parse parenthesized patterns: ( pattern ) *)
and parse_parenthesized_pattern tokens =
  match consume_token tokens with
  | (Some LParen, rest) ->
      (match parse_pattern rest with
       | Ok (p, rest') ->
           (match expect_rparen rest' with
            | Ok rest'' ->
                Ok (make_parenthesized_pattern p, rest'')
            | Error e -> Error e)
       | Error e -> Error e)
  | _ -> Error "Expected '('"

(* Parse list or array patterns: [pattern; ...] or [|pattern; ...|] *)
and parse_list_or_array_pattern tokens =
  match consume_token tokens with
  | (Some LBracket, rest) ->
      (match peek_token rest with
       | Some Or ->
           (* Array pattern [|...|] *)
           (match consume_token rest with
            | (Some Or, rest') ->
                parse_array_pattern_contents rest'
            | _ -> Error "Expected '|' for array pattern")
       | Some RBracket ->
           (* Empty list pattern [] *)
           (match consume_token rest with
            | (Some RBracket, rest') ->
                Ok (make_list_pattern [], rest')
            | _ -> Error "Expected ']'")
       | _ ->
           (* Non-empty list pattern *)
           parse_list_pattern_contents rest)
  | _ -> Error "Expected '['"

and parse_list_pattern_contents tokens =
  match parse_pattern tokens with
  | Ok (first, rest) ->
      parse_list_pattern_continuation [first] rest
  | Error e -> Error e

and parse_list_pattern_continuation acc tokens =
  match peek_token tokens with
  | Some Semicolon ->
      (match consume_token tokens with
       | (Some Semicolon, rest) ->
           (match peek_token rest with
            | Some RBracket ->
                (match consume_token rest with
                 | (Some RBracket, rest') ->
                     Ok (make_list_pattern (List.rev acc), rest')
                 | _ -> Error "Expected ']'")
            | _ ->
                (match parse_pattern rest with
                 | Ok (p, rest') ->
                     parse_list_pattern_continuation (p :: acc) rest'
                 | Error e -> Error e))
       | _ -> Error "Expected ';'")
  | Some RBracket ->
      (match consume_token tokens with
       | (Some RBracket, rest) ->
           Ok (make_list_pattern (List.rev acc), rest)
       | _ -> Error "Expected ']'")
  | _ -> Error "Expected ';' or ']'"

and parse_array_pattern_contents tokens =
  match peek_token tokens with
  | Some Or ->
      (* Empty array [||] *)
      (match consume_token tokens with
       | (Some Or, rest) ->
           (match expect_rbracket rest with
            | Ok rest' ->
                Ok (make_array_pattern [], rest')
            | Error e -> Error e)
       | _ -> Error "Expected '|'")
  | _ ->
      (* Non-empty array *)
      (match parse_pattern tokens with
       | Ok (first, rest) ->
           parse_array_pattern_continuation [first] rest
       | Error e -> Error e)

and parse_array_pattern_continuation acc tokens =
  match peek_token tokens with
  | Some Semicolon ->
      (match consume_token tokens with
       | (Some Semicolon, rest) ->
           (match peek_token rest with
            | Some Or ->
                (match consume_token rest with
                 | (Some Or, rest') ->
                     (match expect_rbracket rest' with
                      | Ok rest'' ->
                          Ok (make_array_pattern (List.rev acc), rest'')
                      | Error e -> Error e)
                 | _ -> Error "Expected '|'")
            | _ ->
                (match parse_pattern rest with
                 | Ok (p, rest') ->
                     parse_array_pattern_continuation (p :: acc) rest'
                 | Error e -> Error e))
       | _ -> Error "Expected ';'")
  | Some Or ->
      (match consume_token tokens with
       | (Some Or, rest) ->
           (match expect_rbracket rest with
            | Ok rest' ->
                Ok (make_array_pattern (List.rev acc), rest')
            | Error e -> Error e)
       | _ -> Error "Expected '|'")
  | _ -> Error "Expected ';' or '|'"

and expect_rbracket tokens =
  match consume_token tokens with
  | (Some RBracket, rest) -> Ok rest
  | _ -> Error "Expected ']'"

(* Parse record patterns: {field = pattern; ...} *)
and parse_record_pattern tokens =
  match consume_token tokens with
  | (Some LBrace, rest) ->
      (match peek_token rest with
       | Some RBrace ->
           (* Empty record {} *)
           (match consume_token rest with
            | (Some RBrace, rest') ->
                Ok (make_record_pattern [] false, rest')
            | _ -> Error "Expected '}'")
       | _ ->
           parse_record_pattern_contents rest)
  | _ -> Error "Expected '{'"

and parse_record_pattern_contents tokens =
  match parse_record_pattern_field tokens with
  | Ok (first, rest) ->
      parse_record_pattern_continuation [first] rest
  | Error e -> Error e

and parse_record_pattern_field tokens =
  match parse_field tokens with
  | Ok (field_path, rest) ->
      (match expect_eq rest with
       | Ok rest' ->
           (match parse_pattern rest' with
            | Ok (p, rest'') ->
                Ok (make_record_pattern_field field_path p, rest'')
            | Error e -> Error e)
       | Error e -> Error e)
  | Error e -> Error e

and expect_eq tokens =
  match consume_token tokens with
  | (Some Eq, rest) -> Ok rest
  | _ -> Error "Expected '='"

and parse_record_pattern_continuation acc tokens =
  match peek_token tokens with
  | Some Semicolon ->
      (match consume_token tokens with
       | (Some Semicolon, rest) ->
           (match peek_token rest with
            | Some (Ident "_") ->
                (* Record with wildcard {field = pattern; _} *)
                (match consume_token rest with
                 | (Some (Ident "_"), rest') ->
                     (match expect_rbrace rest' with
                      | Ok rest'' ->
                          Ok (make_record_pattern (List.rev acc) true, rest'')
                      | Error e -> Error e)
                 | _ -> Error "Expected '_'")
            | Some RBrace ->
                (match consume_token rest with
                 | (Some RBrace, rest') ->
                     Ok (make_record_pattern (List.rev acc) false, rest')
                 | _ -> Error "Expected '}'")
            | _ ->
                (match parse_record_pattern_field rest with
                 | Ok (field, rest') ->
                     parse_record_pattern_continuation (field :: acc) rest'
                 | Error e -> Error e))
       | _ -> Error "Expected ';'")
  | Some RBrace ->
      (match consume_token tokens with
       | (Some RBrace, rest) ->
           Ok (make_record_pattern (List.rev acc) false, rest)
       | _ -> Error "Expected '}'")
  | _ -> Error "Expected ';' or '}'"

and expect_rbrace tokens =
  match consume_token tokens with
  | (Some RBrace, rest) -> Ok rest
  | _ -> Error "Expected '}'"

(* Parse range or character patterns: 'a'..'z' or 'c' *)
and parse_range_or_char_pattern tokens =
  match consume_token tokens with
  | (Some Quote, rest) ->
      (match consume_token rest with
       | (Some (Char c), rest') ->
           (match peek_token rest' with
            | Some Quote ->
                (match consume_token rest' with
                 | (Some Quote, rest'') ->
                     (match peek_token rest'' with
                      | Some DotDot ->
                          (* Range pattern 'a'..'z' *)
                          (match consume_token rest'' with
                           | (Some DotDot, rest''') ->
                               (match consume_token rest''' with
                                | (Some Quote, rest'''') ->
                                    (match consume_token rest'''' with
                                     | (Some (Char end_c), rest''''') ->
                                         (match consume_token rest''''' with
                                          | (Some Quote, rest'''''') ->
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
  | (Some Lazy, rest) ->
      (match parse_primary_pattern rest with
       | Ok (p, rest') ->
           Ok (make_lazy_pattern p, rest')
       | Error e -> Error e)
  | _ -> Error "Expected 'lazy'"

(* Parse exception patterns: exception pattern *)
and parse_exception_pattern tokens =
  match consume_token tokens with
  | (Some Exception, rest) ->
      (match parse_primary_pattern rest with
       | Ok (p, rest') ->
           Ok (make_exception_pattern p, rest')
       | Error e -> Error e)
  | _ -> Error "Expected 'exception'"

(* Helper function to parse comma-separated list *)
let rec parse_comma_separated_list parse_fn tokens =
  match parse_fn tokens with
  | Ok (first, rest) ->
      parse_comma_separated_continuation parse_fn [first] rest
  | Error e -> Error e

and parse_comma_separated_continuation parse_fn acc tokens =
  match peek_token tokens with
  | Some Comma ->
      (match consume_token tokens with
       | (Some Comma, rest) ->
           (match parse_fn rest with
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
  | (Some (Ident name), rest) when validate_lowercase_ident name ->
      (match expect_colon rest with
       | Ok rest' ->
           (match parse_poly_typexpr rest' with
            | Ok (poly_type, rest'') ->
                Ok (make_method_type name poly_type, rest'')
            | Error e -> Error e)
       | Error e -> Error e)
  | _ -> Error "Expected method name"

and expect_colon tokens =
  match consume_token tokens with
  | (Some (InfixOp ":"), rest) -> Ok rest
  | _ -> Error "Expected ':'"

(* Parse poly-typexpr: typexpr | { ' ident }+ . typexpr
 * Syntax: 
 * - Simple type: int
 * - Polymorphic type: 'a 'b. 'a -> 'b
 *)
and parse_poly_typexpr tokens =
  (* Check if we have type variables *)
  if has_type_variables tokens then
    parse_polymorphic_type tokens
  else
    match parse_typexpr tokens with
    | Ok (t, rest) -> Ok (make_mono_type t, rest)
    | Error e -> Error e

and has_type_variables tokens =
  match tokens with
  | Quote :: (Ident _) :: Dot :: _ -> true
  | Quote :: (Ident _) :: Quote :: _ -> true  (* Multiple type variables *)
  | _ -> false

and parse_polymorphic_type tokens =
  match parse_type_variable_list tokens with
  | Ok (vars, rest) ->
      (match expect_dot rest with
       | Ok rest' ->
           (match parse_typexpr rest' with
            | Ok (t, rest'') ->
                Ok (make_poly_type vars t, rest'')
            | Error e -> Error e)
       | Error e -> Error e)
  | Error e -> Error e

and parse_type_variable_list tokens =
  match consume_token tokens with
  | (Some Quote, rest) ->
      (match consume_token rest with
       | (Some (Ident var), rest') when validate_ident var ->
           parse_type_variable_continuation [var] rest'
       | _ -> Error "Expected type variable after '")
  | _ -> Error "Expected type variable"

and parse_type_variable_continuation acc tokens =
  match peek_token tokens with
  | Some Quote ->
      (match consume_token tokens with
       | (Some Quote, rest) ->
           (match consume_token rest with
            | (Some (Ident var), rest') when validate_ident var ->
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
let rec parse_typexpr tokens =
  parse_arrow_type tokens

(* Parse arrow types (lowest precedence): typexpr -> typexpr
 * Syntax:
 * - Simple arrow: int -> string
 * - Labeled arrow: label:int -> string  
 * - Optional labeled arrow: ?label:int -> string
 *)
and parse_arrow_type tokens =
  match parse_as_type tokens with
  | Ok (left, rest) ->
      parse_arrow_continuation left rest
  | Error e -> Error e

and parse_arrow_continuation left tokens =
  match peek_token tokens with
  | Some RightArrow ->
      (match consume_token tokens with
       | (Some RightArrow, rest) ->
           (match parse_arrow_type rest with
            | Ok (right, rest') ->
                Ok (make_arrow left right, rest')
            | Error e -> Error e)
       | _ -> Ok (left, tokens))
  | Some (Ident label) when is_label_like label tokens ->
      (* Handle labeled arguments *)
      parse_labeled_arrow left tokens
  | _ -> Ok (left, tokens)

and is_label_like _label tokens =
  match tokens with
  | (Ident _) :: (InfixOp ":") :: _ -> true
  | _ -> false

and parse_labeled_arrow left tokens =
  match consume_token tokens with
  | (Some (Ident label), rest) ->
      (match peek_token rest with
       | Some (InfixOp ":") ->
           (match consume_token rest with
            | (Some (InfixOp ":"), rest') ->
                (match parse_arrow_type rest' with
                 | Ok (arg_type, rest'') ->
                     (match expect_right_arrow rest'' with
                      | Ok rest''' ->
                          (match parse_arrow_type rest''' with
                           | Ok (return_type, rest'''') ->
                               Ok (make_arrow ~label arg_type return_type, rest'''')
                           | Error e -> Error e)
                      | Error e -> Error e)
                 | Error e -> Error e)
            | _ -> Error "Expected ':'")
       | Some Question ->
           (* Optional labeled argument *)
           (match consume_token rest with
            | (Some Question, rest') ->
                parse_optional_labeled_arrow label left rest'
            | _ -> Error "Expected '?' for optional argument")
       | _ -> Error "Expected ':' or '?' after label")
  | _ -> Error "Expected label"

and parse_optional_labeled_arrow label _left tokens =
  match expect_colon tokens with
  | Ok rest ->
      (match parse_arrow_type rest with
       | Ok (arg_type, rest') ->
           (match expect_right_arrow rest' with
            | Ok rest'' ->
                (match parse_arrow_type rest'' with
                 | Ok (return_type, rest''') ->
                     Ok (make_arrow ~label ~optional:true arg_type return_type, rest''')
                 | Error e -> Error e)
            | Error e -> Error e)
       | Error e -> Error e)
  | Error e -> Error e

and expect_right_arrow tokens =
  match consume_token tokens with
  | (Some RightArrow, rest) -> Ok rest
  | _ -> Error "Expected '->'"

(* Parse 'as' types: typexpr as ' ident
 * Syntax: 'a list as 'my_type
 *)
and parse_as_type tokens =
  match parse_tuple_type tokens with
  | Ok (t, rest) ->
      (match peek_token rest with
       | Some As ->
           (match consume_token rest with
            | (Some As, rest') ->
                (match consume_token rest' with
                 | (Some Quote, rest'') ->
                     (match consume_token rest'' with
                      | (Some (Ident var), rest''') when validate_ident var ->
                          Ok (make_type_as t var, rest''')
                      | _ -> Error "Expected type variable after 'as '")
                 | _ -> Error "Expected ' after 'as'")
            | _ -> Ok (t, rest))
       | _ -> Ok (t, rest))
  | Error e -> Error e

(* Parse tuple types: typexpr { * typexpr }+
 * Syntax: int * string * bool (two or more types separated by *)
 *)
and parse_tuple_type tokens =
  match parse_app_type tokens with
  | Ok (first, rest) ->
      parse_type_tuple_continuation [first] rest
  | Error e -> Error e

and parse_type_tuple_continuation acc tokens =
  match peek_token tokens with
  | Some (InfixOp "*") ->
      (match consume_token tokens with
       | (Some (InfixOp "*"), rest) ->
           (match parse_app_type rest with
            | Ok (t, rest') ->
                parse_type_tuple_continuation (t :: acc) rest'
            | Error e -> Error e)
       | _ -> 
           if List.length acc > 1 then
             Ok (make_tuple (List.rev acc), tokens)
           else
             (match acc with
              | [single] -> Ok (single, tokens)
              | [] -> Error "Empty tuple list"
              | _ -> Ok (make_tuple (List.rev acc), tokens)))
  | _ -> 
      if List.length acc > 1 then
        Ok (make_tuple (List.rev acc), tokens)
      else
        (match acc with
         | [single] -> Ok (single, tokens)
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
  | Ok (t, rest) ->
      parse_app_continuation t rest
  | Error e -> Error e

and parse_app_continuation t tokens =
  match peek_token tokens with
  | Some (Ident name) when validate_lowercase_ident name ->
      (* Simple type constructor application *)
      (match consume_token tokens with
       | (Some (Ident tc_name), rest) ->
           Ok (make_type_app t (None, tc_name), rest)
       | _ -> Ok (t, tokens))
  | Some Hash ->
      (* Class type application *)
      (match consume_token tokens with
       | (Some Hash, rest) ->
           (match parse_class_path rest with
            | Ok (class_path, rest') ->
                Ok (make_classtype_app t class_path, rest')
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
  | Some Quote ->
      (* Type variable: ' ident *)
      (match consume_token tokens with
       | (Some Quote, rest) ->
           (match consume_token rest with
            | (Some (Ident var), rest') when validate_ident var ->
                Ok (make_type_var var, rest')
            | _ -> Error "Expected type variable after '")
       | _ -> Error "Expected type variable")
  | Some (Ident "_") ->
      (* Wildcard: _ *)
      (match consume_token tokens with
       | (Some (Ident "_"), rest) ->
           Ok (make_wildcard (), rest)
       | _ -> Error "Expected wildcard")
  | Some LParen ->
      parse_parenthesized_type tokens
  | Some Lt ->
      parse_object_type tokens
  | Some Hash ->
      parse_class_type tokens
  | Some (Ident name) when validate_lowercase_ident name ->
      (* Type constructor *)
      (match consume_token tokens with
       | (Some (Ident tc_name), rest) ->
           Ok (make_typeconstr (None, tc_name), rest)
       | _ -> Error "Expected type constructor")
  | _ -> Error "Expected type expression"

and parse_parenthesized_type tokens =
  match consume_token tokens with
  | (Some LParen, rest) ->
      (match parse_typexpr rest with
       | Ok (t, rest') ->
           (match peek_token rest' with
            | Some Comma ->
                (* Multiple types: ( typexpr { , typexpr } ) typeconstr *)
                (match parse_comma_separated_list parse_typexpr rest' with
                 | Ok (types, rest'') ->
                     (match expect_rparen rest'' with
                      | Ok rest''' ->
                          (match parse_type_constructor rest''' with
                           | Ok (tc_path, rest'''') ->
                               Ok (make_type_app_multi types tc_path, rest'''')
                           | Error e -> Error e)
                      | Error e -> Error e)
                 | Error e -> Error e)
            | Some RParen ->
                (* Single parenthesized type *)
                (match consume_token rest' with
                 | (Some RParen, rest'') ->
                     Ok (make_parenthesized t, rest'')
                 | _ -> Error "Expected ')'")
            | _ -> Error "Expected ')' or ','")
       | Error e -> Error e)
  | _ -> Error "Expected '('"

and parse_type_constructor tokens =
  match consume_token tokens with
  | (Some (Ident name), rest) when validate_lowercase_ident name ->
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
  | (Some Lt, rest) ->
      (match peek_token rest with
       | Some DotDot ->
           (* Empty object: < .. > *)
           (match consume_token rest with
            | (Some DotDot, rest') ->
                (match expect_gt rest' with
                 | Ok rest'' ->
                     Ok (make_object_empty (), rest'')
                 | Error e -> Error e)
            | _ -> Error "Expected '..'")
       | _ ->
           (* Object with methods *)
           parse_object_methods rest)
  | _ -> Error "Expected '<'"

and parse_object_methods tokens =
  match parse_method_type tokens with
  | Ok (method_t, rest) ->
      parse_object_methods_continuation [method_t] rest
  | Error _ ->
      (* No methods, check for .. *)
      (match peek_token tokens with
       | Some DotDot ->
           (match consume_token tokens with
            | (Some DotDot, rest) ->
                (match expect_gt rest with
                 | Ok rest' ->
                     Ok (make_object_empty (), rest')
                 | Error e -> Error e)
            | _ -> Error "Expected '..'")
       | Some Gt ->
           (match consume_token tokens with
            | (Some Gt, rest) ->
                Ok (make_object [] false false, rest)
            | _ -> Error "Expected '>'")
       | _ -> Error "Expected method or '..' or '>'")

and parse_object_methods_continuation methods tokens =
  match peek_token tokens with
  | Some Semicolon ->
      (match consume_token tokens with
       | (Some Semicolon, rest) ->
           (match peek_token rest with
            | Some DotDot ->
                (* ; .. > *)
                (match consume_token rest with
                 | (Some DotDot, rest') ->
                     (match expect_gt rest' with
                      | Ok rest'' ->
                          Ok (make_object (List.rev methods) true true, rest'')
                      | Error e -> Error e)
                 | _ -> Error "Expected '..'")
            | Some Gt ->
                (* ; > *)
                (match consume_token rest with
                 | (Some Gt, rest') ->
                     Ok (make_object (List.rev methods) true false, rest')
                 | _ -> Error "Expected '>'")
            | _ ->
                (* ; method-type *)
                (match parse_method_type rest with
                 | Ok (method_t, rest') ->
                     parse_object_methods_continuation (method_t :: methods) rest'
                 | Error e -> Error e))
       | _ -> Error "Expected ';'")
  | Some Gt ->
      (match consume_token tokens with
       | (Some Gt, rest) ->
           Ok (make_object (List.rev methods) false false, rest)
       | _ -> Error "Expected '>'")
  | _ -> Error "Expected ';' or '>'"

and expect_gt tokens =
  match consume_token tokens with
  | (Some Gt, rest) -> Ok rest
  | _ -> Error "Expected '>'"

(* Parse class types: # class-path
 * Syntax: #class_name, #Module.class_name
 *)
and parse_class_type tokens =
  match consume_token tokens with
  | (Some Hash, rest) ->
      (match parse_classtype_path rest with
       | Ok (ct_path, rest') ->
           Ok (make_classtype ct_path, rest')
       | Error e -> Error e)
  | _ -> Error "Expected '#'"

(* Lexer integration functions *)

(* Helper function to extract tokens from lexer *)
let tokens_from_lexer lexer =
  let rec collect_tokens acc =
    match Lexer.get_next lexer with
    | Ok (Some token_spanned) -> collect_tokens (Span.value token_spanned :: acc)
    | Ok None -> Ok (List.rev acc)  (* EOF reached *)
    | Error e -> 
        Lexer.print_error e;
        Error "Lexer error"
  in
  collect_tokens []

(* Main parser function that works with lexer *)
let parse_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> 
      (match parse_typexpr tokens with
       | Ok (ast, []) -> Ok (TypeExpr ast)  (* Successfully parsed all tokens as type expression *)
       | Ok (_, remaining) -> Error ("Unexpected tokens remaining: " ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
       | Error _ ->
           (* Try parsing as pattern if type expression parsing failed *)
           (match parse_pattern tokens with
            | Ok (pattern, []) -> Ok (Pattern pattern)  (* Successfully parsed all tokens as pattern *)
            | Ok (_, remaining) -> Error ("Unexpected tokens remaining: " ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
            | Error _ ->
                (* Try parsing as constant if pattern parsing failed *)
                (match parse_constant_tokens tokens with
                 | Ok (constant, []) -> Ok (Constant constant)  (* Successfully parsed all tokens as constant *)
                 | Ok (_, remaining) -> Error ("Unexpected tokens remaining: " ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
                 | Error e -> Error e)))
  | Error e -> Error e

(* Convenience function to parse from string *)
let parse_string str source =
  let lexer = Lexer.create str source in
  parse_from_lexer lexer

(* Specific parsing functions for constants *)
let parse_constant_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> 
      (match parse_constant_tokens tokens with
       | Ok (constant, []) -> Ok constant  (* Successfully parsed all tokens *)
       | Ok (_, remaining) -> Error ("Unexpected tokens remaining: " ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
       | Error e -> Error e)
  | Error e -> Error e

let parse_constant_string str source =
  let lexer = Lexer.create str source in
  parse_constant_from_lexer lexer

(* Specific parsing functions for patterns *)
let parse_pattern_from_lexer lexer =
  match tokens_from_lexer lexer with
  | Ok tokens -> 
      (match parse_pattern tokens with
       | Ok (pattern, []) -> Ok pattern  (* Successfully parsed all tokens *)
       | Ok (_, remaining) -> Error ("Unexpected tokens remaining: " ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
       | Error e -> Error e)
  | Error e -> Error e

let parse_pattern_string str source =
  let lexer = Lexer.create str source in
  parse_pattern_from_lexer lexer