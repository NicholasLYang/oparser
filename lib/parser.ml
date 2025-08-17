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

(* Parse method type: method-name : poly-typexpr *)
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

(* Parse poly-typexpr: typexpr | { ' ident }+ . typexpr *)
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

(* Main typexpr parser with precedence *)
and parse_typexpr tokens =
  parse_arrow_type tokens

(* Parse arrow types (lowest precedence): typexpr -> typexpr *)
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

(* Parse 'as' types: typexpr as ' ident *)
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

(* Parse tuple types: typexpr { * typexpr }+ *)
and parse_tuple_type tokens =
  match parse_app_type tokens with
  | Ok (first, rest) ->
      parse_tuple_continuation [first] rest
  | Error e -> Error e

and parse_tuple_continuation acc tokens =
  match peek_token tokens with
  | Some (InfixOp "*") ->
      (match consume_token tokens with
       | (Some (InfixOp "*"), rest) ->
           (match parse_app_type rest with
            | Ok (t, rest') ->
                parse_tuple_continuation (t :: acc) rest'
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

(* Parse type applications: typexpr typeconstr | ( typexpr { , typexpr } ) typeconstr *)
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

(* Parse primary types: atoms and parenthesized expressions *)
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
       | Ok (ast, []) -> Ok ast  (* Successfully parsed all tokens *)
       | Ok (_, remaining) -> Error ("Unexpected tokens remaining: " ^ String.concat ~sep:" " (List.map remaining ~f:string_of_token))
       | Error e -> Error e)
  | Error e -> Error e

(* Convenience function to parse from string *)
let parse_string str source =
  let lexer = Lexer.create str source in
  parse_from_lexer lexer