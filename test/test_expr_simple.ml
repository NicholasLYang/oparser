open Oparser.Token
open Oparser.Parse_tree

(* Test basic expression parsing functionality without relying on complex parser functions *)

let%expect_test "basic_expression_tokens" =
  Printf.printf "=== Testing Basic Expression Token Recognition ===\n";
  
  (* Test that our new tokens are recognized *)
  let test_token token expected =
    Printf.printf "%s -> %s\n" (Token.string_of_token token) expected
  in
  
  test_token Bar "|";
  test_token (Token.InfixOp "+") "<infixop> +";
  test_token DotLParen ".(";
  test_token DotLBracket ".[";
  test_token ColonGt ":>";
  test_token Function "function";
  
  [%expect {|
    === Testing Basic Expression Token Recognition ===
    | -> |
    <infixop> + -> <infixop> +
    .( -> .(
    .[ -> .[
    :> -> :>
    function -> function
  |}]

let%expect_test "expression_ast_types" =
  Printf.printf "=== Testing Expression AST Construction ===\n";
  
  (* Test creating basic expression AST nodes *)
  let const_expr = ConstantExpr (IntegerLiteral 42) in
  let var_expr = ValuePathExpr (None, "x") in
  let paren_expr = ParenthesizedExpr const_expr in
  
  Printf.printf "Constant: %s\n" (string_of_expr const_expr);
  Printf.printf "Variable: %s\n" (string_of_expr var_expr);
  Printf.printf "Parenthesized: %s\n" (string_of_expr paren_expr);
  
  [%expect {|
    === Testing Expression AST Construction ===
    Constant: 42
    Variable: x
    Parenthesized: (42)
  |}]

let%expect_test "infix_expression_ast" =
  Printf.printf "=== Testing Infix Expression AST ===\n";
  
  (* Test creating infix expressions *)
  let left = ConstantExpr (IntegerLiteral 1) in
  let right = ConstantExpr (IntegerLiteral 2) in
  let infix_expr = InfixOp (left, "+", right) in
  
  Printf.printf "Infix: %s\n" (string_of_expr infix_expr);
  
  [%expect {|
    === Testing Infix Expression AST ===
    Infix: 1 + 2
  |}]

let%expect_test "tuple_expression_ast" =
  Printf.printf "=== Testing Tuple Expression AST ===\n";
  
  (* Test creating tuple expressions *)
  let elem1 = ConstantExpr (IntegerLiteral 1) in
  let elem2 = ConstantExpr (IntegerLiteral 2) in
  let elem3 = ConstantExpr (IntegerLiteral 3) in
  let tuple_expr = TupleExpr [elem1; elem2; elem3] in
  
  Printf.printf "Tuple: %s\n" (string_of_expr tuple_expr);
  
  [%expect {|
    === Testing Tuple Expression AST ===
    Tuple: (1, 2, 3)
  |}]

let%expect_test "list_expression_ast" =
  Printf.printf "=== Testing List Expression AST ===\n";
  
  (* Test creating list expressions *)
  let elem1 = ConstantExpr (IntegerLiteral 1) in
  let elem2 = ConstantExpr (IntegerLiteral 2) in
  let list_expr = ListExpr [elem1; elem2] in
  let empty_list = ListExpr [] in
  
  Printf.printf "List: %s\n" (string_of_expr list_expr);
  Printf.printf "Empty list: %s\n" (string_of_expr empty_list);
  
  [%expect {|
    === Testing List Expression AST ===
    List: [1; 2]
    Empty list: []
  |}]

let%expect_test "constructor_expression_ast" =
  Printf.printf "=== Testing Constructor Expression AST ===\n";
  
  (* Test creating constructor expressions *)
  let simple_constr = ConstructorExpr ((None, "None"), None) in
  let arg_expr = ConstantExpr (IntegerLiteral 42) in
  let constr_with_arg = ConstructorExpr ((None, "Some"), Some arg_expr) in
  
  Printf.printf "Simple constructor: %s\n" (string_of_expr simple_constr);
  Printf.printf "Constructor with arg: %s\n" (string_of_expr constr_with_arg);
  
  [%expect {|
    === Testing Constructor Expression AST ===
    Simple constructor: None
    Constructor with arg: Some 42
  |}]

let%expect_test "polymorphic_variant_expression_ast" =
  Printf.printf "=== Testing Polymorphic Variant Expression AST ===\n";
  
  (* Test creating polymorphic variant expressions *)
  let simple_variant = PolymorphicVariantExpr ("Red", None) in
  let arg_expr = ConstantExpr (IntegerLiteral 42) in
  let variant_with_arg = PolymorphicVariantExpr ("Value", Some arg_expr) in
  
  Printf.printf "Simple variant: %s\n" (string_of_expr simple_variant);
  Printf.printf "Variant with arg: %s\n" (string_of_expr variant_with_arg);
  
  [%expect {|
    === Testing Polymorphic Variant Expression AST ===
    Simple variant: `Red
    Variant with arg: `Value 42
  |}]

let%expect_test "control_flow_expression_ast" =
  Printf.printf "=== Testing Control Flow Expression AST ===\n";
  
  (* Test creating if-then-else expressions *)
  let cond = ConstantExpr True in
  let then_expr = ConstantExpr (IntegerLiteral 1) in
  let else_expr = ConstantExpr (IntegerLiteral 2) in
  let if_expr = IfThenElse (cond, then_expr, Some else_expr) in
  let if_no_else = IfThenElse (cond, then_expr, None) in
  
  Printf.printf "If-then-else: %s\n" (string_of_expr if_expr);
  Printf.printf "If-then: %s\n" (string_of_expr if_no_else);
  
  [%expect {|
    === Testing Control Flow Expression AST ===
    If-then-else: if true then 1 else 2
    If-then: if true then 1
  |}]

let%expect_test "function_expression_ast" =
  Printf.printf "=== Testing Function Expression AST ===\n";
  
  (* Test creating lambda expressions *)
  let param = SimpleParam (ValueName "x") in
  let body = ValuePathExpr (None, "x") in
  let lambda = Lambda ([param], None, body) in
  
  Printf.printf "Lambda: %s\n" (string_of_expr lambda);
  
  [%expect {|
    === Testing Function Expression AST ===
    Lambda: fun x -> x
  |}]