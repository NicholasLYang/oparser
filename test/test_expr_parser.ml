open Oparser.Token
open Oparser.Parse_tree

(* Helper function to test expression parsing *)
let test_expr_parse tokens expected_desc =
  try
    match Oparser.Parser.parse_expr tokens with
    | Ok (expr, []) -> 
        Printf.printf "✓ %s: %s\n" expected_desc (string_of_expr expr)
    | Ok (_, remaining) -> 
        Printf.printf "✗ %s: Unexpected tokens remaining: %s\n" 
          expected_desc 
          (String.concat " " (List.map Token.string_of_token remaining))
    | Error msg -> 
        Printf.printf "✗ %s: Error: %s\n" expected_desc msg
  with
  | exn -> Printf.printf "✗ %s: Exception: %s\n" expected_desc (Printexc.to_string exn)

(* Simple constant expressions *)
let%expect_test "constants" =
  Printf.printf "=== Testing Constant Expressions ===\n";
  
  (* Integer literal *)
  test_expr_parse [Number (Int 42)] "integer literal";
  
  (* String literal *)
  test_expr_parse [String "hello"] "string literal";
  
  (* Boolean literals *)
  test_expr_parse [True] "true literal";
  test_expr_parse [False] "false literal";
  
  (* Unit literal *)
  test_expr_parse [LParen; RParen] "unit literal";
  
  [%expect {|
    === Testing Constant Expressions ===
    ✓ integer literal: 42
    ✓ string literal: "hello"
    ✓ true literal: true
    ✓ false literal: false
    ✓ unit literal: ()
  |}]

(* Variable and path expressions *)
let%expect_test "variables_and_paths" =
  Printf.printf "=== Testing Variables and Paths ===\n";
  
  (* Simple variable *)
  test_expr_parse [Ident "x"] "simple variable";
  
  (* Module qualified variable *)
  test_expr_parse [Ident "List"; Dot; Ident "map"] "module qualified variable";
  
  [%expect {|
    === Testing Variables and Paths ===
    ✓ simple variable: x
    ✓ module qualified variable: List.map
  |}]

(* Parenthesized expressions *)
let%expect_test "parenthesized" =
  Printf.printf "=== Testing Parenthesized Expressions ===\n";
  
  (* Parenthesized variable *)
  test_expr_parse [LParen; Ident "x"; RParen] "parenthesized variable";
  
  (* Nested parentheses *)
  test_expr_parse [LParen; LParen; Number (Int 42); RParen; RParen] "nested parentheses";
  
  [%expect {|
    === Testing Parenthesized Expressions ===
    ✓ parenthesized variable: (x)
    ✓ nested parentheses: ((42))
  |}]

(* Tuple expressions *)
let%expect_test "tuples" =
  Printf.printf "=== Testing Tuple Expressions ===\n";
  
  (* Two-element tuple *)
  test_expr_parse [LParen; Number (Int 1); Comma; Number (Int 2); RParen] "two-element tuple";
  
  (* Three-element tuple *)
  test_expr_parse [LParen; Number (Int 1); Comma; Number (Int 2); Comma; Number (Int 3); RParen] "three-element tuple";
  
  [%expect {|
    === Testing Tuple Expressions ===
    ✓ two-element tuple: (1, 2)
    ✓ three-element tuple: (1, 2, 3)
  |}]

(* List expressions *)
let%expect_test "lists" =
  Printf.printf "=== Testing List Expressions ===\n";
  
  (* Empty list *)
  test_expr_parse [LBracket; RBracket] "empty list";
  
  (* Single element list *)
  test_expr_parse [LBracket; Number (Int 1); RBracket] "single element list";
  
  (* Multi-element list *)
  test_expr_parse [LBracket; Number (Int 1); Semicolon; Number (Int 2); Semicolon; Number (Int 3); RBracket] "multi-element list";
  
  [%expect {|
    === Testing List Expressions ===
    ✓ empty list: []
    ✓ single element list: [1]
    ✓ multi-element list: [1; 2; 3]
  |}]

(* Array expressions *)
let%expect_test "arrays" =
  Printf.printf "=== Testing Array Expressions ===\n";
  
  (* Empty array *)
  test_expr_parse [LBracket; Bar; RBracket] "empty array";
  
  (* Single element array *)
  test_expr_parse [LBracket; Bar; Number (Int 1); Bar; RBracket] "single element array";
  
  [%expect {|
    === Testing Array Expressions ===
    ✓ empty array: [||]
    ✓ single element array: [|1|]
  |}]

(* Constructor expressions *)
let%expect_test "constructors" =
  Printf.printf "=== Testing Constructor Expressions ===\n";
  
  (* Simple constructor *)
  test_expr_parse [Ident "Some"] "simple constructor";
  
  (* Constructor with argument *)
  test_expr_parse [Ident "Some"; Number (Int 42)] "constructor with argument";
  
  [%expect {|
    === Testing Constructor Expressions ===
    ✓ simple constructor: Some
    ✓ constructor with argument: Some 42
  |}]

(* Polymorphic variant expressions *)
let%expect_test "polymorphic_variants" =
  Printf.printf "=== Testing Polymorphic Variant Expressions ===\n";
  
  (* Simple polymorphic variant *)
  test_expr_parse [PolymorphicVariantTag "Red"] "simple polymorphic variant";
  
  (* Polymorphic variant with argument *)
  test_expr_parse [PolymorphicVariantTag "Point"; LParen; Number (Int 1); Comma; Number (Int 2); RParen] "polymorphic variant with argument";
  
  [%expect {|
    === Testing Polymorphic Variant Expressions ===
    ✓ simple polymorphic variant: `Red
    ✓ polymorphic variant with argument: `Point (1, 2)
  |}]

(* Infix operators *)
let%expect_test "infix_operators" =
  Printf.printf "=== Testing Infix Operators ===\n";
  
  (* Addition *)
  test_expr_parse [Number (Int 1); Plus; Number (Int 2)] "addition";
  
  (* Subtraction *)
  test_expr_parse [Number (Int 5); Minus; Number (Int 3)] "subtraction";
  
  (* Comparison *)
  test_expr_parse [Number (Int 1); Lt; Number (Int 2)] "less than";
  
  [%expect {|
    === Testing Infix Operators ===
    ✓ addition: 1 + 2
    ✓ subtraction: 5 - 3
    ✓ less than: 1 < 2
  |}]

(* Prefix operators *)
let%expect_test "prefix_operators" =
  Printf.printf "=== Testing Prefix Operators ===\n";
  
  (* Unary minus *)
  test_expr_parse [Minus; Number (Int 42)] "unary minus";
  
  (* Unary plus *)
  test_expr_parse [Plus; Number (Int 42)] "unary plus";
  
  [%expect {|
    === Testing Prefix Operators ===
    ✓ unary minus: - 42
    ✓ unary plus: + 42
  |}]

(* If-then-else expressions *)
let%expect_test "if_then_else" =
  Printf.printf "=== Testing If-Then-Else Expressions ===\n";
  
  (* If-then-else *)
  test_expr_parse [
    If; True; Then; Number (Int 1); Else; Number (Int 2)
  ] "if-then-else";
  
  (* If-then (no else) *)
  test_expr_parse [
    If; True; Then; Number (Int 1)
  ] "if-then";
  
  [%expect {|
    === Testing If-Then-Else Expressions ===
    ✓ if-then-else: if true then 1 else 2
    ✓ if-then: if true then 1
  |}]

(* Function expressions *)
let%expect_test "functions" =
  Printf.printf "=== Testing Function Expressions ===\n";
  
  (* Simple lambda *)
  test_expr_parse [
    Fun; Ident "x"; RightArrow; Ident "x"
  ] "simple lambda";
  
  (* Multi-parameter lambda *)
  test_expr_parse [
    Fun; Ident "x"; Ident "y"; RightArrow; Ident "x"
  ] "multi-parameter lambda";
  
  [%expect {|
    === Testing Function Expressions ===
    ✓ simple lambda: fun x -> x
    ✓ multi-parameter lambda: fun x y -> x
  |}]

(* Let expressions *)
let%expect_test "let_expressions" =
  Printf.printf "=== Testing Let Expressions ===\n";
  
  (* Simple let *)
  test_expr_parse [
    Let; Ident "x"; Eq; Number (Int 42); In; Ident "x"
  ] "simple let";
  
  [%expect {|
    === Testing Let Expressions ===
    ✓ simple let: let x = 42 in x
  |}]

(* Sequence expressions *)
let%expect_test "sequences" =
  Printf.printf "=== Testing Sequence Expressions ===\n";
  
  (* Simple sequence *)
  test_expr_parse [
    Number (Int 1); Semicolon; Number (Int 2)
  ] "simple sequence";
  
  [%expect {|
    === Testing Sequence Expressions ===
    ✓ simple sequence: 1; 2
  |}]

(* Complex expressions *)
let%expect_test "complex_expressions" =
  Printf.printf "=== Testing Complex Expressions ===\n";
  
  (* Operator precedence: 1 + 2 * 3 should be 1 + (2 * 3) *)
  test_expr_parse [
    Number (Int 1); Plus; Number (Int 2); InfixOp "*"; Number (Int 3)
  ] "operator precedence";
  
  [%expect {|
    === Testing Complex Expressions ===
    ✓ operator precedence: 1 + 2 * 3
  |}]