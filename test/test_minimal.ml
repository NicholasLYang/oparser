open Oparser.Parse_tree

let%expect_test "minimal_expression_test" =
  let expr = ConstantExpr (IntegerLiteral 42) in
  let result = string_of_expr expr in
  Stdio.printf "%s\n" result;
  [%expect {| 42 |}]

let%expect_test "minimal_parse_tree_test" =
  let expr = ConstantExpr (IntegerLiteral 42) in
  let parse_tree = Expr expr in
  let result = string_of_parse_tree parse_tree in
  Stdio.printf "%s\n" result;
  [%expect {| Expr: 42 |}]