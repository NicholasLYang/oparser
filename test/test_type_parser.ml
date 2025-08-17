open Oparser.Parser
open Oparser.Token
open Oparser.Parse_tree

(* Helper function to create tokens for testing *)
let make_tokens tokens = tokens

let print_typexpr_result = function
  | Ok (expr, _) -> Printf.printf "Ok: %s\n" (string_of_typexpr expr)
  | Error msg -> Printf.printf "Error: %s\n" msg

let print_poly_typexpr_result = function
  | Ok (expr, _) -> Printf.printf "Ok: %s\n" (string_of_poly_typexpr expr)
  | Error msg -> Printf.printf "Error: %s\n" msg

(* Tests for type variables *)
let%expect_test "parse_type_var" =
  let tokens = [Quote; Ident "a"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| 'a |}]

let%expect_test "parse_wildcard" =
  let tokens = [Ident "_"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| _ |}]

let%expect_test "parse_type_constructor" =
  let tokens = [Ident "int"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| int |}]

let%expect_test "parse_arrow_type" =
  let tokens = [Ident "int"; RightArrow; Ident "string"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| int -> string |}]

let%expect_test "parse_tuple_type" =
  let tokens = [Ident "int"; InfixOp "*"; Ident "string"; InfixOp "*"; Ident "bool"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| int * string * bool |}]

let%expect_test "parse_type_application" =
  let tokens = [Quote; Ident "a"; Ident "list"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| 'a list |}]

let%expect_test "parse_parenthesized_type" =
  let tokens = [LParen; Quote; Ident "a"; RightArrow; Quote; Ident "b"; RParen] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| ('a -> 'b) |}]

let%expect_test "parse_type_as" =
  let tokens = [Quote; Ident "a"; As; Quote; Ident "x"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| 'a as 'x |}]

let%expect_test "parse_object_empty" =
  let tokens = [Lt; DotDot; Gt] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| < .. > |}]

let%expect_test "parse_object_with_method" =
  let tokens = [Lt; Ident "foo"; InfixOp ":"; Quote; Ident "a"; Gt] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| < foo : 'a > |}]

let%expect_test "parse_object_with_methods" =
  let tokens = [Lt; Ident "foo"; InfixOp ":"; Quote; Ident "a"; Semicolon; Ident "bar"; InfixOp ":"; Quote; Ident "b"; Gt] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| < foo : 'a; bar : 'b > |}]

let%expect_test "parse_classtype" =
  let tokens = [Hash; Ident "widget"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| #widget |}]

let%expect_test "parse_classtype_app" =
  let tokens = [Quote; Ident "a"; Hash; Ident "comparable"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| 'a #comparable |}]

let%expect_test "parse_poly_type" =
  let tokens = [Quote; Ident "a"; Dot; Quote; Ident "a"; RightArrow; Quote; Ident "a"] in
  (match parse_poly_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_poly_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| 'a. 'a -> 'a |}]

let%expect_test "parse_multi_poly_type" =
  let tokens = [Quote; Ident "a"; Quote; Ident "b"; Dot; Quote; Ident "a"; RightArrow; Quote; Ident "b"] in
  (match parse_poly_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_poly_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| 'a 'b. 'a -> 'b |}]

let%expect_test "parse_mono_poly_type" =
  let tokens = [Ident "int"; RightArrow; Ident "string"] in
  (match parse_poly_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_poly_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| int -> string |}]

(* Complex type expressions *)
let%expect_test "parse_complex_arrow" =
  let tokens = [Ident "int"; RightArrow; Quote; Ident "a"; Ident "list"; RightArrow; Ident "string"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| int -> 'a list -> string |}]

let%expect_test "parse_tuple_with_arrows" =
  let tokens = [LParen; Quote; Ident "a"; RightArrow; Quote; Ident "b"; RParen; InfixOp "*"; Ident "int"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| ('a -> 'b) * int |}]

let%expect_test "parse_nested_tuples" =
  let tokens = [LParen; Quote; Ident "a"; InfixOp "*"; Quote; Ident "b"; RParen; InfixOp "*"; Quote; Ident "c"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| ('a * 'b) * 'c |}]

(* Error cases *)
let%expect_test "parse_incomplete_arrow" =
  let tokens = [Ident "int"; RightArrow] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected type expression |}]

let%expect_test "parse_incomplete_tuple" =
  let tokens = [Ident "int"; InfixOp "*"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected type expression |}]

let%expect_test "parse_unclosed_paren" =
  let tokens = [LParen; Quote; Ident "a"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected ')' or ',' |}]

let%expect_test "parse_invalid_type_var" =
  let tokens = [Quote; Ident "123"] in
  (match parse_typexpr tokens with
   | Ok (expr, _) -> Printf.printf "%s\n" (string_of_typexpr expr)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected type variable after ' |}]

(* Debug test for method parsing *)
let%expect_test "debug_method_parsing" =
  let tokens = [Ident "foo"; InfixOp ":"; Quote; Ident "a"] in
  (match parse_method_type tokens with
   | Ok (method_t, _) -> Printf.printf "%s\n" (string_of_method_type method_t)
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| foo : 'a |}]