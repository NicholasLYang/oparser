(** Parse tree types for OCaml grammar *)

(** {1 Basic identifier and name types} *)

type ident = string
type value_name = string
type operator_name = string
type constr_name = string
type tag_name = string
type typeconstr_name = string
type field_name = string
type module_name = string
type modtype_name = string
type class_name = string
type inst_var_name = string
type method_name = string

(** {1 Path types} *)

type module_path = module_name list
type extended_module_path = string list
type value_path = module_path option * value_name
type constr_path = module_path option * constr_name
type typeconstr_path = extended_module_path option * typeconstr_name
type field_path = module_path option * field_name
type modtype_path = extended_module_path option * modtype_name
type class_path = module_path option * class_name
type classtype_path = extended_module_path option * class_name

(** {1 Type expression grammar} *)

type label_name = string

(** {1 Constants} *)

type constant =
  | IntegerLiteral of int
  | Int32Literal of int32
  | Int64Literal of int64
  | NativeIntLiteral of nativeint
  | FloatLiteral of float
  | CharLiteral of char
  | StringLiteral of string
  | Constructor of constr_name
  | False
  | True
  | Unit
  | BeginEnd
  | EmptyList
  | EmptyArray
  | PolymorphicVariantTag of tag_name
[@@deriving sexp]

type method_type = { method_name : method_name; poly_typexpr : poly_typexpr }
(** Method type for object types *)

and polymorphic_variant_type = unit
(** Polymorphic variant type (placeholder) *)

(** Type expressions *)
and typexpr =
  | TypeVar of ident  (** ' ident *)
  | Wildcard  (** _ *)
  | Parenthesized of typexpr  (** ( typexpr ) *)
  | Arrow of label_name option * bool * typexpr * typexpr
      (** [[?]label-name:] typexpr -> typexpr *)
  | Tuple of typexpr list  (** typexpr { * typexpr }+ *)
  | TypeConstr of typeconstr_path  (** typeconstr *)
  | TypeApp of typexpr * typeconstr_path  (** typexpr typeconstr *)
  | TypeAppMulti of typexpr list * typeconstr_path
      (** ( typexpr { , typexpr } ) typeconstr *)
  | TypeAs of typexpr * ident  (** typexpr as ' ident *)
  | PolymorphicVariant of polymorphic_variant_type
      (** polymorphic-variant-type *)
  | ObjectEmpty  (** < [..] > *)
  | Object of method_type list * bool * bool
      (** < method-type { ; method-type } [; | ; ..] > *)
  | ClassType of classtype_path  (** # classtype-path *)
  | ClassTypeApp of typexpr * class_path  (** typexpr # class-path *)
  | ClassTypeAppMulti of typexpr list * class_path
      (** ( typexpr { , typexpr } ) # class-path *)
[@@deriving sexp]

(** Polymorphic type expressions *)
and poly_typexpr =
  | MonoType of typexpr  (** typexpr *)
  | PolyType of ident list * typexpr  (** { ' ident }+ . typexpr *)

(** {1 Pattern expressions} *)

type pattern =
  | ValueName of value_name  (** value-name *)
  | PatternWildcard  (** _ *)
  | PatternConstant of constant  (** constant *)
  | PatternAlias of pattern * value_name  (** pattern as value-name *)
  | PatternConstructor of constr_path  (** constr *)
  | ConstructorPattern of constr_path * pattern  (** constr pattern *)
  | TuplePattern of pattern list  (** pattern { , pattern }+ *)
  | ConsPattern of pattern * pattern  (** pattern :: pattern *)
  | ListPattern of pattern list  (** [pattern {; pattern}*] *)
  | RecordPattern of record_pattern_field list * bool  (** {field-name=pattern; ...} [; _] *)
  | ArrayPattern of pattern list  (** [|pattern {; pattern}*|] *)
  | RangePattern of char * char  (** 'a'..'z' *)
  | LazyPattern of pattern  (** lazy pattern *)
  | ExceptionPattern of pattern  (** exception pattern *)
  | OrPattern of pattern * pattern  (** pattern | pattern *)
  | ParenthesizedPattern of pattern  (** ( pattern ) *)
[@@deriving sexp]

and record_pattern_field = {
  field_name : field_path;
  pattern : pattern
}
[@@deriving sexp]

(** {1 Complete parse tree type} *)

type parse_tree =
  | TypeExpr of typexpr
  | PolyTypeExpr of poly_typexpr
  | MethodType of method_type
  | ValuePath of value_path
  | ConstrPath of constr_path
  | TypeConstrPath of typeconstr_path
  | FieldPath of field_path
  | ModTypePath of modtype_path
  | ClassPath of class_path
  | ClassTypePath of classtype_path
  | Constant of constant
  | Pattern of pattern
[@@deriving sexp]

(** {1 Pretty printing functions} *)

val string_of_typexpr : typexpr -> string
val string_of_poly_typexpr : poly_typexpr -> string
val string_of_method_type : method_type -> string
val string_of_value_path : value_path -> string
val string_of_constr_path : constr_path -> string
val string_of_typeconstr_path : typeconstr_path -> string
val string_of_field_path : field_path -> string
val string_of_modtype_path : modtype_path -> string
val string_of_class_path : class_path -> string
val string_of_classtype_path : classtype_path -> string
val string_of_parse_tree : parse_tree -> string
val string_of_constant : constant -> string
val string_of_pattern : pattern -> string
val string_of_record_pattern_field : record_pattern_field -> string

(** {1 Helper functions for creating parse tree nodes} *)

val make_type_var : ident -> typexpr
val make_wildcard : unit -> typexpr
val make_parenthesized : typexpr -> typexpr

val make_arrow :
  ?label:label_name -> ?optional:bool -> typexpr -> typexpr -> typexpr

val make_tuple : typexpr list -> typexpr
val make_typeconstr : typeconstr_path -> typexpr
val make_type_app : typexpr -> typeconstr_path -> typexpr
val make_type_app_multi : typexpr list -> typeconstr_path -> typexpr
val make_type_as : typexpr -> ident -> typexpr
val make_object_empty : unit -> typexpr
val make_object : method_type list -> bool -> bool -> typexpr
val make_classtype : classtype_path -> typexpr
val make_classtype_app : typexpr -> class_path -> typexpr
val make_classtype_app_multi : typexpr list -> class_path -> typexpr
val make_mono_type : typexpr -> poly_typexpr
val make_poly_type : ident list -> typexpr -> poly_typexpr
val make_method_type : method_name -> poly_typexpr -> method_type

(** {1 Helper functions for creating constants} *)

val make_integer_literal : int -> constant
val make_int32_literal : int32 -> constant
val make_int64_literal : int64 -> constant
val make_nativeint_literal : nativeint -> constant
val make_float_literal : float -> constant
val make_char_literal : char -> constant
val make_string_literal : string -> constant
val make_constructor : constr_name -> constant
val make_false : unit -> constant
val make_true : unit -> constant
val make_unit : unit -> constant
val make_begin_end : unit -> constant
val make_empty_list : unit -> constant
val make_empty_array : unit -> constant
val make_polymorphic_variant_tag : tag_name -> constant

(** {1 Helper functions for creating pattern nodes} *)

val make_value_name_pattern : value_name -> pattern
val make_wildcard_pattern : unit -> pattern
val make_pattern_constant : constant -> pattern
val make_pattern_alias : pattern -> value_name -> pattern
val make_constructor_pattern : constr_path -> pattern
val make_constructor_pattern_with_arg : constr_path -> pattern -> pattern
val make_tuple_pattern : pattern list -> pattern
val make_cons_pattern : pattern -> pattern -> pattern
val make_list_pattern : pattern list -> pattern
val make_record_pattern : record_pattern_field list -> bool -> pattern
val make_array_pattern : pattern list -> pattern
val make_range_pattern : char -> char -> pattern
val make_lazy_pattern : pattern -> pattern
val make_exception_pattern : pattern -> pattern
val make_or_pattern : pattern -> pattern -> pattern
val make_parenthesized_pattern : pattern -> pattern
val make_record_pattern_field : field_path -> pattern -> record_pattern_field
