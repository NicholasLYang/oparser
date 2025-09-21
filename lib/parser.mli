(** Parser interface for OCaml type expressions *)

open Base
open Token
open Parse_tree

(** Parser result type *)
type 'a parse_result = ('a, string) Result.t

(** Extract tokens from a lexer *)
val tokens_from_lexer : Lexer.t -> token list parse_result

(** Parse from a lexer directly *)
val parse_from_lexer : Lexer.t -> parse_tree parse_result

(** Parse from a string using the lexer *)
val parse_string : string -> Grace.Source.t -> parse_tree parse_result

(** Parse constants specifically *)
val parse_constant_from_lexer : Lexer.t -> constant parse_result

(** Parse constant from string *)
val parse_constant_string : string -> Grace.Source.t -> constant parse_result

(** Low-level token parsing functions *)

(** Parse identifier *)
val parse_ident : token -> string parse_result

(** Parse capitalized identifier *)
val parse_capitalized_ident : token -> string parse_result

(** Parse lowercase identifier *)
val parse_lowercase_ident : token -> string parse_result

(** Parse value name *)
val parse_value_name : token -> string parse_result

(** Parse operator name *)
val parse_operator_name : token -> string parse_result

(** Parse constructor name *)
val parse_constr_name : token -> string parse_result

(** Parse tag name *)
val parse_tag_name : token -> string parse_result

(** Parse type constructor name *)
val parse_typeconstr_name : token -> string parse_result

(** Parse field name *)
val parse_field_name : token -> string parse_result

(** Parse module name *)
val parse_module_name : token -> string parse_result

(** Parse module type name *)
val parse_modtype_name : token -> string parse_result

(** Parse class name *)
val parse_class_name : token -> string parse_result

(** Parse instance variable name *)
val parse_inst_var_name : token -> string parse_result

(** Parse method name *)
val parse_method_name : token -> string parse_result

(** Path parsing functions *)

(** Parse module path *)
val parse_module_path : token list -> (module_path * token list) parse_result

(** Parse value path *)
val parse_value_path : token list -> (value_path * token list) parse_result

(** Parse constructor path *)
val parse_constr : token list -> (constr_path * token list) parse_result

(** Parse type constructor path *)
val parse_typeconstr : token list -> (typeconstr_path * token list) parse_result

(** Parse field path *)
val parse_field : token list -> (field_path * token list) parse_result

(** Parse module type path *)
val parse_modtype_path : token list -> (modtype_path * token list) parse_result

(** Parse class path *)
val parse_class_path : token list -> (class_path * token list) parse_result

(** Parse class type path *)
val parse_classtype_path : token list -> (classtype_path * token list) parse_result

(** Type expression parsing functions *)

(** Parse type expression *)
val parse_typexpr : token list -> (typexpr * token list) parse_result

(** Parse polymorphic type expression *)
val parse_poly_typexpr : token list -> (poly_typexpr * token list) parse_result

(** Parse method type *)
val parse_method_type : token list -> (method_type * token list) parse_result

(** Constant parsing functions *)

(** Parse single constant token *)
val parse_constant : token -> constant parse_result

(** Parse constant from token list *)
val parse_constant_tokens : token list -> (constant * token list) parse_result

(** Pattern parsing functions *)

(** Parse pattern from lexer *)
val parse_pattern_from_lexer : Lexer.t -> pattern parse_result

(** Parse pattern from string *)
val parse_pattern_string : string -> Grace.Source.t -> pattern parse_result

(** Parse pattern from token list *)
val parse_pattern : token list -> (pattern * token list) parse_result

(** Expression parsing functions *)

(** Parse expression from lexer *)
val parse_expr_from_lexer : Lexer.t -> expr parse_result

(** Parse expression from string *)
val parse_expr_string : string -> Grace.Source.t -> expr parse_result

(** Parse expression from token list *)
val parse_expr : token list -> (expr * token list) parse_result