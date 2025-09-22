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

(* Polymorphic variant type *)
and polymorphic_variant_type =
  | VariantClosed of variant_field list (* [ variant-field { | variant-field } ] *)
  | VariantOpen of variant_field list (* [> variant-field { | variant-field } ] *)
  | VariantLess of variant_field list * typexpr list (* [< variant-field { | variant-field } [> typexpr { & typexpr } ] ] *)

and variant_field =
  | TagField of tag_name * bool * typexpr list (* `tag-name [ of [&] typexpr { & typexpr } ] *)
  | InheritField of typexpr (* typexpr *)

(* Method type for object types *)
and method_type = { method_name : method_name; poly_typexpr : poly_typexpr }
[@@deriving sexp]

(* Pattern expressions *)
type pattern =
  | ValueName of value_name (* value-name *)
  | PatternWildcard (* _ *)
  | PatternConstant of constant (* constant *)
  | PatternAlias of pattern * value_name (* pattern as value-name *)
  | PatternConstructor of constr_path (* constr *)
  | ConstructorPattern of constr_path * pattern (* constr pattern *)
  | TuplePattern of pattern list (* pattern { , pattern }+ *)
  | ConsPattern of pattern * pattern (* pattern :: pattern *)
  | ListPattern of pattern list (* [pattern {; pattern}*] *)
  | RecordPattern of record_pattern_field list * bool (* {field-name=pattern; ...} [; _] *)
  | ArrayPattern of pattern list (* [|pattern {; pattern}*|] *)
  | RangePattern of char * char (* 'a'..'z' *)
  | LazyPattern of pattern (* lazy pattern *)
  | ExceptionPattern of pattern (* exception pattern *)
  | OrPattern of pattern * pattern (* pattern | pattern *)
  | ParenthesizedPattern of pattern (* ( pattern ) *)
[@@deriving sexp]

and record_pattern_field = {
  field_name : field_path;
  pattern : pattern
}
[@@deriving sexp]

(* Forward declarations needed for mutual recursion *)
type constr_decl = {
  constr_name: constr_name;
  constr_args: constr_args;
  constr_ret: typexpr option;
}

and constr_args =
  | NoArgs
  | TupleArgs of typexpr list
  | RecordArgs of field_decl list

and field_decl = {
  field_name: field_name;
  field_mutable: bool;
  field_type: typexpr;
}

and module_params = parameter list

and module_type =
  | ModuleTypePath of modtype_path
  | ModuleTypeSignature of module_signature
  | ModuleTypeFunctor of module_name * module_type option * module_type
  | ModuleTypeWith of module_type * with_constraint list
  | ModuleTypeOf of module_expr

and module_signature = specification list

and specification =
  | ValueSpec of value_name * typexpr
  | TypeSpec of type_declaration list
  | ExceptionSpec of constr_decl
  | ModuleSpec of module_name * module_type
  | ModuleTypeSpec of modtype_name * module_type option
  | ClassSpec of class_specification list
  | ClassTypeSpec of class_type_specification list
  | IncludeSpec of module_type
  | OpenSpec of module_path

and type_declaration = {
  type_name: typeconstr_name;
  type_params: type_param list;
  type_private: bool;
  type_manifest: typexpr option;
  type_kind: type_kind;
  type_constraints: (typexpr * typexpr) list;
}

and type_param = {
  param_name: ident;
  param_variance: variance option;
}

and variance = Covariant | Contravariant

and type_kind =
  | TypeAbstract
  | TypeVariant of constr_decl list
  | TypeRecord of field_decl list
  | TypeOpen

and class_specification = {
  class_virtual: bool;
  class_params: type_param list;
  class_name: class_name;
  class_type: class_type;
}

and class_type_specification = {
  classtype_virtual: bool;
  classtype_params: type_param list;
  classtype_name: class_name;
  classtype_type: class_type;
}

and with_constraint =
  | WithType of typeconstr_path * type_declaration
  | WithModule of module_path * module_path

and module_expr =
  | ModuleExprPath of module_path
  | ModuleExprStruct of module_structure
  | ModuleExprFunctor of module_name * module_type option * module_expr
  | ModuleExprApply of module_expr * module_expr
  | ModuleExprConstraint of module_expr * module_type

and module_structure = definition list

and definition =
  | ValueDef of let_binding list
  | ValueRecDef of let_binding list
  | TypeDef of type_declaration list
  | ExceptionDef of constr_decl
  | ModuleDef of module_name * module_expr
  | ModuleTypeDef of modtype_name * module_type
  | ClassDef of class_definition list
  | ClassTypeDef of class_type_definition list
  | IncludeDef of module_expr
  | OpenDef of module_path

and class_definition = {
  class_virtual: bool;
  class_params: type_param list;
  class_name: class_name;
  class_args: parameter list;
  class_type_constraint: class_type option;
  class_expr: class_expr;
}

and class_type_definition = {
  classtype_virtual: bool;
  classtype_params: type_param list;
  classtype_name: class_name;
  classtype_type: class_type;
}

and class_type =
  | ClassBodyType of class_body_type
  | ClassArrow of label_name option * bool * typexpr * class_type

and class_body_type =
  | ClassObject of typexpr option * class_field_spec list
  | ClassPath of classtype_path
  | ClassApp of typexpr list * classtype_path
  | ClassOpen of module_path * class_body_type

and class_field_spec =
  | InheritSpec of class_body_type
  | ValSpec of bool * bool * inst_var_name * typexpr (* mutable, virtual, name, type *)
  | MethodSpec of bool * bool * method_name * poly_typexpr (* private, virtual, name, type *)
  | ConstraintSpec of typexpr * typexpr

and class_expr =
  | ClassPath of class_path
  | ClassApp of typexpr list * class_path
  | ClassParenthesized of class_expr
  | ClassConstrained of class_expr * class_type
  | ClassAppl of class_expr * argument list
  | ClassFun of parameter list * class_expr
  | ClassLet of let_binding list * class_expr
  | ClassLetRec of let_binding list * class_expr
  | ClassObject of class_field list
  | ClassOpen of module_path * class_expr

and class_field =
  | InheritField of class_expr * value_name option
  | ValField of bool * inst_var_name * typexpr option * expr (* mutable, name, type, expr *)
  | MethodField of bool * method_name * parameter list * typexpr option * expr (* private, name, params, type, expr *)
  | VirtualMethod of bool * method_name * poly_typexpr (* private, name, type *)
  | InitializerField of expr

and object_expr = {
  self_pattern: pattern option;
  fields: class_field list;
}

(* Expression types *)
and expr =
  | ConstantExpr of constant (* constant *)
  | ValuePathExpr of value_path (* value-path *)
  | ParenthesizedExpr of expr (* ( expr ) *)
  | BeginEndExpr of expr (* begin expr end *)
  | TypeConstraint of expr * typexpr (* ( expr : typexpr ) *)
  | TupleExpr of expr list (* expr { , expr }+ *)
  | ConstructorExpr of constr_path * expr option (* constr [expr] *)
  | PolymorphicVariantExpr of tag_name * expr option (* `tag-name [expr] *)
  | ConsExpr of expr * expr (* expr :: expr *)
  | ListExpr of expr list (* [ expr { ; expr } [;] ] *)
  | ArrayExpr of expr list (* [| expr { ; expr } [;] |] *)
  | RecordExpr of record_field list (* { field [: typexpr] [= expr] { ; field [: typexpr] [= expr] } [;] } *)
  | RecordUpdate of expr * record_field list (* { expr with field [: typexpr] [= expr] { ; field [: typexpr] [= expr] } [;] } *)
  | FunctionApp of expr * argument list (* expr { argument }+ *)
  | PrefixOp of string * expr (* prefix-symbol expr *)
  | InfixOp of expr * string * expr (* expr infix-op expr *)
  | FieldAccess of expr * field_path (* expr . field *)
  | FieldUpdate of expr * field_path * expr (* expr . field <- expr *)
  | ArrayAccess of expr * expr (* expr .( expr ) *)
  | ArrayUpdate of expr * expr * expr (* expr .( expr ) <- expr *)
  | StringAccess of expr * expr (* expr .[ expr ] *)
  | StringUpdate of expr * expr * expr (* expr .[ expr ] <- expr *)
  | IfThenElse of expr * expr * expr option (* if expr then expr [ else expr ] *)
  | While of expr * expr (* while expr do expr done *)
  | For of value_name * expr * for_direction * expr * expr (* for value-name = expr ( to | downto ) expr do expr done *)
  | Sequence of expr * expr (* expr ; expr *)
  | Match of expr * case list (* match expr with pattern-matching *)
  | Function of case list (* function pattern-matching *)
  | Lambda of parameter list * typexpr option * expr (* fun { parameter }+ [ : typexpr ] -> expr *)
  | Try of expr * case list (* try expr with pattern-matching *)
  | Let of let_binding list * expr (* let [rec] let-binding { and let-binding } in expr *)
  | LetRec of let_binding list * expr (* let rec let-binding { and let-binding } in expr *)
  | LetException of constr_name * constr_decl option * expr (* let exception constr-decl in expr *)
  | LetModule of module_name * module_params * module_type option * module_expr * expr (* let module module-name { ( module-name : module-type ) } [ : module-type ] = module-expr in expr *)
  | Coercion of expr * typexpr (* ( expr :> typexpr ) *)
  | SubtypingCoercion of expr * typexpr * typexpr (* ( expr : typexpr :> typexpr ) *)
  | Assert of expr (* assert expr *)
  | Lazy of expr (* lazy expr *)
  | Raise of expr (* raise expr *)
  | LocalOpen of module_path * expr (* let open module-path in expr OR module-path.( expr ) *)
  | ObjectExpr of object_expr (* object class-body end *)
  | MethodCall of expr * method_name * argument list (* expr#method {argument}* *)
  | NewInstance of class_path * argument list (* new class-path {argument}* *)
[@@deriving sexp]

and for_direction = 
  | To
  | Downto
[@@deriving sexp]

and argument =
  | SimpleArg of expr
  | LabeledArg of label_name * expr (* label:expr *)
  | OptionalArg of label_name * expr option (* ?label[:expr] *)
[@@deriving sexp]

and parameter =
  | SimpleParam of pattern
  | LabeledParam of label_name * pattern (* label:pattern *)
  | OptionalParam of label_name * pattern * expr option (* ?label[:pattern] [= expr] *)
  | TypeParam of typexpr (* ( type typeconstr-name ) *)
[@@deriving sexp]

and let_binding = {
  pattern: pattern;
  params: parameter list;
  type_constraint: typexpr option;
  expr: expr;
}
[@@deriving sexp]

and case = {
  pattern: pattern;
  guard: expr option; (* when expr *)
  expr: expr;
}
[@@deriving sexp]

and record_field = {
  field_path: field_path;
  type_constraint: typexpr option;
  expr: expr option;
}
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
  | Pattern of pattern
  | Expr of expr
  | ClassTypeTree of class_type
  | ClassExpr of class_expr
  | ClassField of class_field
  | ClassDefinition of class_definition
  | ModuleType of module_type
  | ModuleExpr of module_expr
  | Definition of definition
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

let rec string_of_pattern = function
  | ValueName name -> name
  | PatternWildcard -> "_"
  | PatternConstant c -> string_of_constant c
  | PatternAlias (p, name) -> string_of_pattern p ^ " as " ^ name
  | PatternConstructor path -> string_of_constr_path path
  | ConstructorPattern (path, p) -> string_of_constr_path path ^ " " ^ string_of_pattern p
  | TuplePattern patterns -> "(" ^ String.concat ~sep:", " (List.map patterns ~f:string_of_pattern) ^ ")"
  | ConsPattern (head, tail) -> string_of_pattern head ^ " :: " ^ string_of_pattern tail
  | ListPattern patterns -> "[" ^ String.concat ~sep:"; " (List.map patterns ~f:string_of_pattern) ^ "]"
  | RecordPattern (fields, has_wildcard) ->
      let field_strs = List.map fields ~f:string_of_record_pattern_field in
      let fields_str = String.concat ~sep:"; " field_strs in
      let wildcard_str = if has_wildcard then "; _" else "" in
      "{" ^ fields_str ^ wildcard_str ^ "}"
  | ArrayPattern patterns -> "[|" ^ String.concat ~sep:"; " (List.map patterns ~f:string_of_pattern) ^ "|]"
  | RangePattern (start, stop) -> "'" ^ String.make 1 start ^ "'.." ^ "'" ^ String.make 1 stop ^ "'"
  | LazyPattern p -> "lazy " ^ string_of_pattern p
  | ExceptionPattern p -> "exception " ^ string_of_pattern p
  | OrPattern (p1, p2) -> string_of_pattern p1 ^ " | " ^ string_of_pattern p2
  | ParenthesizedPattern p -> "(" ^ string_of_pattern p ^ ")"

and string_of_record_pattern_field { field_name; pattern } =
  string_of_field_path field_name ^ " = " ^ string_of_pattern pattern

let rec string_of_expr = function
  | ConstantExpr c -> string_of_constant c
  | ValuePathExpr path -> string_of_value_path path
  | ParenthesizedExpr e -> "(" ^ string_of_expr e ^ ")"
  | BeginEndExpr e -> "begin " ^ string_of_expr e ^ " end"
  | TypeConstraint (e, t) -> "(" ^ string_of_expr e ^ " : " ^ string_of_typexpr t ^ ")"
  | TupleExpr exprs -> "(" ^ String.concat ~sep:", " (List.map exprs ~f:string_of_expr) ^ ")"
  | ConstructorExpr (path, None) -> string_of_constr_path path
  | ConstructorExpr (path, Some e) -> string_of_constr_path path ^ " " ^ string_of_expr e
  | PolymorphicVariantExpr (tag, None) -> "`" ^ tag
  | PolymorphicVariantExpr (tag, Some e) -> "`" ^ tag ^ " " ^ string_of_expr e
  | ConsExpr (head, tail) -> string_of_expr head ^ " :: " ^ string_of_expr tail
  | ListExpr exprs -> "[" ^ String.concat ~sep:"; " (List.map exprs ~f:string_of_expr) ^ "]"
  | ArrayExpr exprs -> "[|" ^ String.concat ~sep:"; " (List.map exprs ~f:string_of_expr) ^ "|]"
  | RecordExpr fields -> "{" ^ String.concat ~sep:"; " (List.map fields ~f:string_of_record_field) ^ "}"
  | RecordUpdate (e, fields) -> "{" ^ string_of_expr e ^ " with " ^ String.concat ~sep:"; " (List.map fields ~f:string_of_record_field) ^ "}"
  | FunctionApp (f, args) -> string_of_expr f ^ " " ^ String.concat ~sep:" " (List.map args ~f:string_of_argument)
  | PrefixOp (op, e) -> op ^ " " ^ string_of_expr e
  | InfixOp (e1, op, e2) -> string_of_expr e1 ^ " " ^ op ^ " " ^ string_of_expr e2
  | FieldAccess (e, field) -> string_of_expr e ^ "." ^ string_of_field_path field
  | FieldUpdate (e, field, value) -> string_of_expr e ^ "." ^ string_of_field_path field ^ " <- " ^ string_of_expr value
  | ArrayAccess (e, idx) -> string_of_expr e ^ ".(" ^ string_of_expr idx ^ ")"
  | ArrayUpdate (e, idx, value) -> string_of_expr e ^ ".(" ^ string_of_expr idx ^ ") <- " ^ string_of_expr value
  | StringAccess (e, idx) -> string_of_expr e ^ ".[" ^ string_of_expr idx ^ "]"
  | StringUpdate (e, idx, value) -> string_of_expr e ^ ".[" ^ string_of_expr idx ^ "] <- " ^ string_of_expr value
  | IfThenElse (cond, then_e, None) -> "if " ^ string_of_expr cond ^ " then " ^ string_of_expr then_e
  | IfThenElse (cond, then_e, Some else_e) -> "if " ^ string_of_expr cond ^ " then " ^ string_of_expr then_e ^ " else " ^ string_of_expr else_e
  | While (cond, body) -> "while " ^ string_of_expr cond ^ " do " ^ string_of_expr body ^ " done"
  | For (var, start, dir, stop, body) -> 
      "for " ^ var ^ " = " ^ string_of_expr start ^ 
      (match dir with To -> " to " | Downto -> " downto ") ^
      string_of_expr stop ^ " do " ^ string_of_expr body ^ " done"
  | Sequence (e1, e2) -> string_of_expr e1 ^ "; " ^ string_of_expr e2
  | Match (e, cases) -> "match " ^ string_of_expr e ^ " with " ^ String.concat ~sep:" | " (List.map cases ~f:string_of_case)
  | Function cases -> "function " ^ String.concat ~sep:" | " (List.map cases ~f:string_of_case)
  | Lambda (params, None, body) -> "fun " ^ String.concat ~sep:" " (List.map params ~f:string_of_parameter) ^ " -> " ^ string_of_expr body
  | Lambda (params, Some t, body) -> "fun " ^ String.concat ~sep:" " (List.map params ~f:string_of_parameter) ^ " : " ^ string_of_typexpr t ^ " -> " ^ string_of_expr body
  | Try (e, cases) -> "try " ^ string_of_expr e ^ " with " ^ String.concat ~sep:" | " (List.map cases ~f:string_of_case)
  | Let (bindings, body) -> "let " ^ String.concat ~sep:" and " (List.map bindings ~f:string_of_let_binding) ^ " in " ^ string_of_expr body
  | LetRec (bindings, body) -> "let rec " ^ String.concat ~sep:" and " (List.map bindings ~f:string_of_let_binding) ^ " in " ^ string_of_expr body
  | LetException (name, _, body) -> "let exception " ^ name ^ " in " ^ string_of_expr body
  | LetModule (name, _, _, _, body) -> "let module " ^ name ^ " = ... in " ^ string_of_expr body
  | Coercion (e, t) -> "(" ^ string_of_expr e ^ " :> " ^ string_of_typexpr t ^ ")"
  | SubtypingCoercion (e, t1, t2) -> "(" ^ string_of_expr e ^ " : " ^ string_of_typexpr t1 ^ " :> " ^ string_of_typexpr t2 ^ ")"
  | Assert e -> "assert " ^ string_of_expr e
  | Lazy e -> "lazy " ^ string_of_expr e
  | Raise e -> "raise " ^ string_of_expr e
  | LocalOpen (path, e) -> "let open " ^ String.concat ~sep:"." path ^ " in " ^ string_of_expr e
  | ObjectExpr _ -> "object ... end"
  | MethodCall (obj, method_name, _) -> string_of_expr obj ^ "#" ^ method_name
  | NewInstance (class_path, _) -> "new " ^ string_of_class_path class_path

and string_of_argument = function
  | SimpleArg e -> string_of_expr e
  | LabeledArg (label, e) -> label ^ ":" ^ string_of_expr e
  | OptionalArg (label, None) -> "?" ^ label
  | OptionalArg (label, Some e) -> "?" ^ label ^ ":" ^ string_of_expr e

and string_of_parameter = function
  | SimpleParam p -> string_of_pattern p
  | LabeledParam (label, p) -> label ^ ":" ^ string_of_pattern p
  | OptionalParam (label, p, None) -> "?" ^ label ^ ":" ^ string_of_pattern p
  | OptionalParam (label, p, Some e) -> "?" ^ label ^ ":" ^ string_of_pattern p ^ " = " ^ string_of_expr e
  | TypeParam t -> "(type " ^ string_of_typexpr t ^ ")"

and string_of_let_binding { pattern; params; type_constraint; expr } =
  string_of_pattern pattern ^ 
  (if List.is_empty params then "" else " " ^ String.concat ~sep:" " (List.map params ~f:string_of_parameter)) ^
  (match type_constraint with None -> "" | Some t -> " : " ^ string_of_typexpr t) ^
  " = " ^ string_of_expr expr

and string_of_case { pattern; guard; expr } =
  string_of_pattern pattern ^
  (match guard with None -> "" | Some g -> " when " ^ string_of_expr g) ^
  " -> " ^ string_of_expr expr

and string_of_record_field { field_path; type_constraint; expr } =
  string_of_field_path field_path ^
  (match type_constraint with None -> "" | Some t -> " : " ^ string_of_typexpr t) ^
  (match expr with None -> "" | Some e -> " = " ^ string_of_expr e)

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
  | Pattern p -> "Pattern: " ^ string_of_pattern p
  | Expr e -> "Expr: " ^ string_of_expr e
  | ClassTypeTree _ -> "ClassType: [class-type]"
  | ClassExpr _ -> "ClassExpr: [class-expr]"
  | ClassField _ -> "ClassField: [class-field]"
  | ClassDefinition _ -> "ClassDefinition: [class-definition]"
  | ModuleType _ -> "ModuleType: [module-type]"
  | ModuleExpr _ -> "ModuleExpr: [module-expr]"
  | Definition _ -> "Definition: [definition]"

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
let make_classtype path = ClassType path (* typexpr constructor *)
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

(* Helper functions for creating pattern nodes *)
let make_value_name_pattern name = ValueName name
let make_wildcard_pattern () = PatternWildcard
let make_pattern_constant c = PatternConstant c
let make_pattern_alias p name = PatternAlias (p, name)
let make_constructor_pattern path = PatternConstructor path
let make_constructor_pattern_with_arg path p = ConstructorPattern (path, p)
let make_tuple_pattern patterns = TuplePattern patterns
let make_cons_pattern head tail = ConsPattern (head, tail)
let make_list_pattern patterns = ListPattern patterns
let make_record_pattern fields has_wildcard = RecordPattern (fields, has_wildcard)
let make_array_pattern patterns = ArrayPattern patterns
let make_range_pattern start stop = RangePattern (start, stop)
let make_lazy_pattern p = LazyPattern p
let make_exception_pattern p = ExceptionPattern p
let make_or_pattern p1 p2 = OrPattern (p1, p2)
let make_parenthesized_pattern p = ParenthesizedPattern p
let make_record_pattern_field field_name pattern = { field_name; pattern }
