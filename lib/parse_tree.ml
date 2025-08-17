open Base

(* Parse tree types for OCaml grammar *)

(* Identifier and name types from previous grammar *)
type ident = string [@@deriving sexp]
type value_name = string [@@deriving sexp]
type operator_name = string [@@deriving sexp]
type constr_name = string [@@deriving sexp]
type tag_name = string [@@deriving sexp]
type typeconstr_name = string [@@deriving sexp]
type field_name = string [@@deriving sexp]
type module_name = string [@@deriving sexp]
type modtype_name = string [@@deriving sexp]
type class_name = string [@@deriving sexp]
type inst_var_name = string [@@deriving sexp]
type method_name = string [@@deriving sexp]

(* Path types *)
type module_path = module_name list [@@deriving sexp]
type extended_module_path = string list [@@deriving sexp]

(* Can include functor applications *)
type value_path = module_path option * value_name [@@deriving sexp]
type constr_path = module_path option * constr_name [@@deriving sexp]

type typeconstr_path = extended_module_path option * typeconstr_name
[@@deriving sexp]

type field_path = module_path option * field_name [@@deriving sexp]
type modtype_path = extended_module_path option * modtype_name [@@deriving sexp]
type class_path = module_path option * class_name [@@deriving sexp]
type classtype_path = extended_module_path option * class_name [@@deriving sexp]

(* Label types for type expressions *)
type label_name = string [@@deriving sexp]

(* Constants *)
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

(* Type expressions *)
type typexpr =
  | TypeVar of ident (* ' ident *)
  | Wildcard (* _ *)
  | Parenthesized of typexpr (* ( typexpr ) *)
  | Arrow of label_name option * bool * typexpr * typexpr
  (* [[?]label-name:] typexpr -> typexpr *)
  (* bool indicates if label is optional (?) *)
  | Tuple of typexpr list (* typexpr { * typexpr }+ *)
  | TypeConstr of typeconstr_path (* typeconstr *)
  | TypeApp of typexpr * typeconstr_path (* typexpr typeconstr *)
  | TypeAppMulti of
      typexpr list * typeconstr_path (* ( typexpr { , typexpr } ) typeconstr *)
  | TypeAs of typexpr * ident (* typexpr as ' ident *)
  | PolymorphicVariant of
      polymorphic_variant_type (* polymorphic-variant-type *)
  | ObjectEmpty (* < [..] > *)
  | Object of method_type list * bool * bool
  (* < method-type { ; method-type } [; | ; ..] > *)
  (* first bool: has semicolon at end, second bool: has .. *)
  | ClassType of classtype_path (* # classtype-path *)
  | ClassTypeApp of typexpr * class_path (* typexpr # class-path *)
  | ClassTypeAppMulti of
      typexpr list * class_path (* ( typexpr { , typexpr } ) # class-path *)
[@@deriving sexp]

(* Polymorphic type expressions *)
and poly_typexpr =
  | MonoType of typexpr (* typexpr *)
  | PolyType of ident list * typexpr (* { ' ident }+ . typexpr *)

(* Polymorphic variant type (placeholder for now) *)
and polymorphic_variant_type =
  unit (* TODO: Define when we encounter the grammar *)

(* Method type for object types *)
and method_type = { method_name : method_name; poly_typexpr : poly_typexpr }
[@@deriving sexp]

(* Parse tree for complete constructs *)
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
[@@deriving sexp]

(* Pretty printing functions *)
let rec string_of_typexpr = function
  | TypeVar id -> "'" ^ id
  | Wildcard -> "_"
  | Parenthesized t -> "(" ^ string_of_typexpr t ^ ")"
  | Arrow (None, false, t1, t2) ->
      string_of_typexpr t1 ^ " -> " ^ string_of_typexpr t2
  | Arrow (Some label, false, t1, t2) ->
      label ^ ":" ^ string_of_typexpr t1 ^ " -> " ^ string_of_typexpr t2
  | Arrow (Some label, true, t1, t2) ->
      "?" ^ label ^ ":" ^ string_of_typexpr t1 ^ " -> " ^ string_of_typexpr t2
  | Arrow (None, true, t1, t2) ->
      "?" ^ string_of_typexpr t1 ^ " -> " ^ string_of_typexpr t2
  | Tuple types ->
      String.concat ~sep:" * " (List.map types ~f:string_of_typexpr)
  | TypeConstr (None, name) -> name
  | TypeConstr (Some path, name) -> String.concat ~sep:"." path ^ "." ^ name
  | TypeApp (t, (None, name)) -> string_of_typexpr t ^ " " ^ name
  | TypeApp (t, (Some path, name)) ->
      string_of_typexpr t ^ " " ^ String.concat ~sep:"." path ^ "." ^ name
  | TypeAppMulti (types, (None, name)) ->
      "("
      ^ String.concat ~sep:", " (List.map types ~f:string_of_typexpr)
      ^ ") " ^ name
  | TypeAppMulti (types, (Some path, name)) ->
      "("
      ^ String.concat ~sep:", " (List.map types ~f:string_of_typexpr)
      ^ ") "
      ^ String.concat ~sep:"." path
      ^ "." ^ name
  | TypeAs (t, id) -> string_of_typexpr t ^ " as '" ^ id
  | PolymorphicVariant _ -> "[polymorphic-variant]"
  | ObjectEmpty -> "< .. >"
  | Object (methods, has_semi, has_dots) ->
      let method_strs = List.map methods ~f:string_of_method_type in
      let methods_str = String.concat ~sep:"; " method_strs in
      let ending =
        match (has_semi, has_dots) with
        | true, true -> "; .."
        | true, false -> ";"
        | false, true -> "; .."
        | false, false -> ""
      in
      "< " ^ methods_str ^ ending ^ " >"
  | ClassType (None, name) -> "#" ^ name
  | ClassType (Some path, name) ->
      "#" ^ String.concat ~sep:"." path ^ "." ^ name
  | ClassTypeApp (t, (None, name)) -> string_of_typexpr t ^ " #" ^ name
  | ClassTypeApp (t, (Some path, name)) ->
      string_of_typexpr t ^ " #" ^ String.concat ~sep:"." path ^ "." ^ name
  | ClassTypeAppMulti (types, (None, name)) ->
      "("
      ^ String.concat ~sep:", " (List.map types ~f:string_of_typexpr)
      ^ ") #" ^ name
  | ClassTypeAppMulti (types, (Some path, name)) ->
      "("
      ^ String.concat ~sep:", " (List.map types ~f:string_of_typexpr)
      ^ ") #"
      ^ String.concat ~sep:"." path
      ^ "." ^ name

and string_of_poly_typexpr = function
  | MonoType t -> string_of_typexpr t
  | PolyType (vars, t) ->
      String.concat ~sep:" " (List.map vars ~f:(fun v -> "'" ^ v))
      ^ ". " ^ string_of_typexpr t

and string_of_method_type { method_name; poly_typexpr } =
  method_name ^ " : " ^ string_of_poly_typexpr poly_typexpr

let string_of_constant = function
  | IntegerLiteral i -> Int.to_string i
  | Int32Literal i -> Int32.to_string i ^ "l"
  | Int64Literal i -> Int64.to_string i ^ "L"
  | NativeIntLiteral i -> Nativeint.to_string i ^ "n"
  | FloatLiteral f -> Float.to_string f
  | CharLiteral c -> "'" ^ String.make 1 c ^ "'"
  | StringLiteral s -> "\"" ^ s ^ "\""
  | Constructor name -> name
  | False -> "false"
  | True -> "true"
  | Unit -> "()"
  | BeginEnd -> "begin end"
  | EmptyList -> "[]"
  | EmptyArray -> "[||]"
  | PolymorphicVariantTag tag -> "`" ^ tag

(* Generic helper for path string conversion *)
let string_of_path (path_opt, name) =
  match path_opt with
  | None -> name
  | Some path -> String.concat ~sep:"." path ^ "." ^ name

let string_of_value_path = string_of_path
let string_of_constr_path = string_of_path
let string_of_typeconstr_path = string_of_path
let string_of_field_path = string_of_path
let string_of_modtype_path = string_of_path
let string_of_class_path = string_of_path
let string_of_classtype_path = string_of_path

let string_of_parse_tree = function
  | TypeExpr t -> "TypeExpr: " ^ string_of_typexpr t
  | PolyTypeExpr pt -> "PolyTypeExpr: " ^ string_of_poly_typexpr pt
  | MethodType mt -> "MethodType: " ^ string_of_method_type mt
  | ValuePath vp -> "ValuePath: " ^ string_of_value_path vp
  | ConstrPath cp -> "ConstrPath: " ^ string_of_constr_path cp
  | TypeConstrPath tcp -> "TypeConstrPath: " ^ string_of_typeconstr_path tcp
  | FieldPath fp -> "FieldPath: " ^ string_of_field_path fp
  | ModTypePath mtp -> "ModTypePath: " ^ string_of_modtype_path mtp
  | ClassPath cp -> "ClassPath: " ^ string_of_class_path cp
  | ClassTypePath ctp -> "ClassTypePath: " ^ string_of_classtype_path ctp
  | Constant c -> "Constant: " ^ string_of_constant c

(* Helper functions for creating parse tree nodes *)
let make_type_var id = TypeVar id
let make_wildcard () = Wildcard
let make_parenthesized t = Parenthesized t
let make_arrow ?label ?(optional = false) t1 t2 = Arrow (label, optional, t1, t2)
let make_tuple types = Tuple types
let make_typeconstr path = TypeConstr path
let make_type_app t path = TypeApp (t, path)
let make_type_app_multi types path = TypeAppMulti (types, path)
let make_type_as t id = TypeAs (t, id)
let make_object_empty () = ObjectEmpty
let make_object methods has_semi has_dots = Object (methods, has_semi, has_dots)
let make_classtype path = ClassType path
let make_classtype_app t path = ClassTypeApp (t, path)
let make_classtype_app_multi types path = ClassTypeAppMulti (types, path)
let make_mono_type t = MonoType t
let make_poly_type vars t = PolyType (vars, t)
let make_method_type method_name poly_typexpr = { method_name; poly_typexpr }

(* Helper functions for creating constants *)
let make_integer_literal i = IntegerLiteral i
let make_int32_literal i = Int32Literal i
let make_int64_literal i = Int64Literal i
let make_nativeint_literal i = NativeIntLiteral i
let make_float_literal f = FloatLiteral f
let make_char_literal c = CharLiteral c
let make_string_literal s = StringLiteral s
let make_constructor name = Constructor name
let make_false () = False
let make_true () = True
let make_unit () = Unit
let make_begin_end () = BeginEnd
let make_empty_list () = EmptyList
let make_empty_array () = EmptyArray
let make_polymorphic_variant_tag tag = PolymorphicVariantTag tag
