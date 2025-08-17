open Oparser.Token
open Oparser.Parse_tree

(* Simple tests for constant parsing that don't require the full parser to work *)

let print_constant_result = function
  | Ok constant -> Printf.printf "Ok: %s\n" (string_of_constant constant)
  | Error msg -> Printf.printf "Error: %s\n" msg

(* Test individual constant token parsing with working parser functions *)
let%expect_test "parse_constant - integer literals" =
  let test_cases = [
    Number (Int 42);
    Number (Int 0);
    Number (Int (-123));
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: 42
    Ok: 0
    Ok: -123
  |}]

let%expect_test "parse_constant - int32 literals" =
  let test_cases = [
    Number (Int32 42l);
    Number (Int32 0l);
    Number (Int32 (-123l));
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: 42l
    Ok: 0l
    Ok: -123l
  |}]

let%expect_test "parse_constant - int64 literals" =
  let test_cases = [
    Number (Int64 42L);
    Number (Int64 0L);
    Number (Int64 (-123L));
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: 42L
    Ok: 0L
    Ok: -123L
  |}]

let%expect_test "parse_constant - nativeint literals" =
  let test_cases = [
    Number (NativeInt 42n);
    Number (NativeInt 0n);
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: 42n
    Ok: 0n
  |}]

let%expect_test "parse_constant - float literals" =
  let test_cases = [
    Number (Float 3.14);
    Number (Float 0.0);
    Number (Float (-2.5));
    Number (Float 1e6);
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: 3.14
    Ok: 0
    Ok: -2.5
    Ok: 1000000
  |}]

let%expect_test "parse_constant - char literals" =
  let test_cases = [
    Char 'a';
    Char 'Z';
    Char '0';
    Char ' ';
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: 'a'
    Ok: 'Z'
    Ok: '0'
    Ok: ' '
  |}]

let%expect_test "parse_constant - string literals" =
  let test_cases = [
    String "hello";
    String "";
    String "world with spaces";
    String "unicode: é";
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: "hello"
    Ok: ""
    Ok: "world with spaces"
    Ok: "unicode: é"
  |}]

let%expect_test "parse_constant - constructor names" =
  let test_cases = [
    Ident "Some";
    Ident "None";
    Ident "Ok";
    Ident "Error";
    Ident "CustomConstructor";
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: Some
    Ok: None
    Ok: Ok
    Ok: Error
    Ok: CustomConstructor
  |}]

let%expect_test "parse_constant - boolean literals" =
  let test_cases = [
    False;
    True;
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: false
    Ok: true
  |}]

let%expect_test "parse_constant - polymorphic variant tags" =
  let test_cases = [
    PolymorphicVariantTag "Red";
    PolymorphicVariantTag "Blue";
    PolymorphicVariantTag "tag";
    PolymorphicVariantTag "Another_Tag";
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Ok: `Red
    Ok: `Blue
    Ok: `tag
    Ok: `Another_Tag
  |}]

let%expect_test "parse_constant - invalid tokens" =
  let test_cases = [
    Plus;
    Dot;
    LParen;
    Ident "lowercase"; (* invalid constructor name *)
  ] in
  List.iter (fun token ->
    print_constant_result (Oparser.Parser.parse_constant token)
  ) test_cases;
  [%expect {|
    Error: Expected constant
    Error: Expected constant
    Error: Expected constant
    Error: Expected constant
  |}]