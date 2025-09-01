(* Test expression AST construction and string conversion without relying on parsing functions *)

open Oparser.Parse_tree

let%expect_test "expression_constants" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Constants ===\n";
  
  (* Test various constant expressions *)
  let int_expr = ConstantExpr (IntegerLiteral 42) in
  let str_expr = ConstantExpr (StringLiteral "hello") in
  let bool_expr = ConstantExpr True in
  let unit_expr = ConstantExpr Unit in
  
  Printf.printf "Integer: %s\n" (string_of_expr int_expr);
  Printf.printf "String: %s\n" (string_of_expr str_expr);
  Printf.printf "Boolean: %s\n" (string_of_expr bool_expr);
  Printf.printf "Unit: %s\n" (string_of_expr unit_expr);
  
  [%expect {|
    === Testing Expression Constants ===
    Integer: 42
    String: "hello"
    Boolean: true
    Unit: ()
  |}]

let%expect_test "expression_variables" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Variables ===\n";
  
  (* Test variable path expressions *)
  let simple_var = ValuePathExpr (None, "x") in
  let module_var = ValuePathExpr (Some ["List"], "map") in
  
  Printf.printf "Simple variable: %s\n" (string_of_expr simple_var);
  Printf.printf "Module variable: %s\n" (string_of_expr module_var);
  
  [%expect {|
    === Testing Expression Variables ===
    Simple variable: x
    Module variable: List.map
  |}]

let%expect_test "expression_operators" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Operators ===\n";
  
  (* Test infix and prefix operators *)
  let left = ConstantExpr (IntegerLiteral 1) in
  let right = ConstantExpr (IntegerLiteral 2) in
  let add_expr = InfixOp (left, "+", right) in
  let mul_expr = InfixOp (left, "*", right) in
  let neg_expr = PrefixOp ("-", left) in
  
  Printf.printf "Addition: %s\n" (string_of_expr add_expr);
  Printf.printf "Multiplication: %s\n" (string_of_expr mul_expr);
  Printf.printf "Negation: %s\n" (string_of_expr neg_expr);
  
  [%expect {|
    === Testing Expression Operators ===
    Addition: 1 + 2
    Multiplication: 1 * 2
    Negation: - 1
  |}]

let%expect_test "expression_data_structures" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Data Structures ===\n";
  
  (* Test tuples, lists, and arrays *)
  let elem1 = ConstantExpr (IntegerLiteral 1) in
  let elem2 = ConstantExpr (IntegerLiteral 2) in
  let elem3 = ConstantExpr (IntegerLiteral 3) in
  
  let tuple_expr = TupleExpr [elem1; elem2; elem3] in
  let list_expr = ListExpr [elem1; elem2] in
  let array_expr = ArrayExpr [elem1; elem2] in
  let empty_list = ListExpr [] in
  
  Printf.printf "Tuple: %s\n" (string_of_expr tuple_expr);
  Printf.printf "List: %s\n" (string_of_expr list_expr);
  Printf.printf "Array: %s\n" (string_of_expr array_expr);
  Printf.printf "Empty list: %s\n" (string_of_expr empty_list);
  
  [%expect {|
    === Testing Expression Data Structures ===
    Tuple: (1, 2, 3)
    List: [1; 2]
    Array: [|1; 2|]
    Empty list: []
  |}]

let%expect_test "expression_constructors" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Constructors ===\n";
  
  (* Test constructor expressions *)
  let none_expr = ConstructorExpr ((None, "None"), None) in
  let some_expr = ConstructorExpr ((None, "Some"), Some (ConstantExpr (IntegerLiteral 42))) in
  let variant_expr = PolymorphicVariantExpr ("Red", None) in
  let variant_with_arg = PolymorphicVariantExpr ("Point", Some (ConstantExpr (IntegerLiteral 10))) in
  
  Printf.printf "None: %s\n" (string_of_expr none_expr);
  Printf.printf "Some: %s\n" (string_of_expr some_expr);
  Printf.printf "Variant: %s\n" (string_of_expr variant_expr);
  Printf.printf "Variant with arg: %s\n" (string_of_expr variant_with_arg);
  
  [%expect {|
    === Testing Expression Constructors ===
    None: None
    Some: Some 42
    Variant: `Red
    Variant with arg: `Point 10
  |}]

let%expect_test "expression_control_flow" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Control Flow ===\n";
  
  (* Test control flow expressions *)
  let cond = ConstantExpr True in
  let then_expr = ConstantExpr (IntegerLiteral 1) in
  let else_expr = ConstantExpr (IntegerLiteral 2) in
  
  let if_else = IfThenElse (cond, then_expr, Some else_expr) in
  let if_only = IfThenElse (cond, then_expr, None) in
  
  let seq_expr = Sequence (then_expr, else_expr) in
  
  Printf.printf "If-else: %s\n" (string_of_expr if_else);
  Printf.printf "If only: %s\n" (string_of_expr if_only);
  Printf.printf "Sequence: %s\n" (string_of_expr seq_expr);
  
  [%expect {|
    === Testing Expression Control Flow ===
    If-else: if true then 1 else 2
    If only: if true then 1
    Sequence: 1; 2
  |}]

let%expect_test "expression_functions" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Functions ===\n";
  
  (* Test function expressions *)
  let param = SimpleParam (ValueName "x") in
  let body = ValuePathExpr (None, "x") in
  let lambda = Lambda ([param], None, body) in
  
  let func_app = FunctionApp (lambda, [SimpleArg (ConstantExpr (IntegerLiteral 42))]) in
  
  Printf.printf "Lambda: %s\n" (string_of_expr lambda);
  Printf.printf "Function application: %s\n" (string_of_expr func_app);
  
  [%expect {|
    === Testing Expression Functions ===
    Lambda: fun x -> x
    Function application: (fun x -> x) 42
  |}]

let%expect_test "expression_let_bindings" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Let Bindings ===\n";
  
  (* Test let expressions *)
  let pattern = ValueName "x" in
  let binding_expr = ConstantExpr (IntegerLiteral 42) in
  let body_expr = ValuePathExpr (None, "x") in
  
  let binding = { pattern; params = []; type_constraint = None; expr = binding_expr } in
  let let_expr = Let ([binding], body_expr) in
  
  Printf.printf "Let expression: %s\n" (string_of_expr let_expr);
  
  [%expect {|
    === Testing Expression Let Bindings ===
    Let expression: let x = 42 in x
  |}]

let%expect_test "expression_parentheses" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Parentheses ===\n";
  
  (* Test parenthesized and begin-end expressions *)
  let inner = ConstantExpr (IntegerLiteral 42) in
  let paren_expr = ParenthesizedExpr inner in
  let begin_expr = BeginEndExpr inner in
  
  Printf.printf "Parenthesized: %s\n" (string_of_expr paren_expr);
  Printf.printf "Begin-end: %s\n" (string_of_expr begin_expr);
  
  [%expect {|
    === Testing Expression Parentheses ===
    Parenthesized: (42)
    Begin-end: begin 42 end
  |}]

let%expect_test "expression_field_access" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Field Access ===\n";
  
  (* Test field access and updates *)
  let record = ValuePathExpr (None, "record") in
  let field_path = (None, "field") in
  let new_value = ConstantExpr (IntegerLiteral 42) in
  
  let field_access = FieldAccess (record, field_path) in
  let field_update = FieldUpdate (record, field_path, new_value) in
  
  Printf.printf "Field access: %s\n" (string_of_expr field_access);
  Printf.printf "Field update: %s\n" (string_of_expr field_update);
  
  [%expect {|
    === Testing Expression Field Access ===
    Field access: record.field
    Field update: record.field <- 42
  |}]

let%expect_test "expression_array_access" =
  let open Oparser.Parse_tree in
  
  Printf.printf "=== Testing Expression Array Access ===\n";
  
  (* Test array and string access *)
  let arr = ValuePathExpr (None, "arr") in
  let index = ConstantExpr (IntegerLiteral 0) in
  let new_value = ConstantExpr (IntegerLiteral 42) in
  
  let array_access = ArrayAccess (arr, index) in
  let array_update = ArrayUpdate (arr, index, new_value) in
  let string_access = StringAccess (arr, index) in
  
  Printf.printf "Array access: %s\n" (string_of_expr array_access);
  Printf.printf "Array update: %s\n" (string_of_expr array_update);
  Printf.printf "String access: %s\n" (string_of_expr string_access);
  
  [%expect {|
    === Testing Expression Array Access ===
    Array access: arr.(0)
    Array update: arr.(0) <- 42
    String access: arr.[0]
  |}]