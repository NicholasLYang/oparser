open Base
open Oparser.Token
open Oparser.Parse_tree

(* Tests for constants as they appear in OCaml patterns *)

let print_constant_from_string_result = function
  | Ok constant -> Printf.printf "Ok: %s\n" (string_of_constant constant)
  | Error msg -> Printf.printf "Error: %s\n" msg

(* Test parsing constants from string input (like the REPL would) *)
let%expect_test "pattern constants - integers from string" =
  let test_cases = [
    "42";
    "0";
    "-123";
    "0x2a";  (* hex *)
    "0o52";  (* octal *)
    "0b101010";  (* binary *)
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: 42
    Ok: 0
    Ok: -123
    Ok: 42
    Ok: 42
    Ok: 42
  |}]

let%expect_test "pattern constants - typed integers from string" =
  let test_cases = [
    "42l";    (* int32 *)
    "42L";    (* int64 *)
    "42n";    (* nativeint *)
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: 42l
    Ok: 42L
    Ok: 42n
  |}]

let%expect_test "pattern constants - floats from string" =
  let test_cases = [
    "3.14";
    "0.0";
    "-2.5";
    "1e6";
    "1.5e-3";
    "6.02e23";
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: 3.14
    Ok: 0
    Ok: -2.5
    Ok: 1000000
    Ok: 0.0015
    Ok: 6.02e+23
  |}]

let%expect_test "pattern constants - characters from string" =
  let test_cases = [
    "'a'";
    "'Z'";
    "'0'";
    "' '";
    "'\\n'";   (* newline *)
    "'\\t'";   (* tab *)
    "'\\''";   (* single quote *)
    "'\\\\'";  (* backslash *)
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: 'a'
    Ok: 'Z'
    Ok: '0'
    Ok: ' '
    Ok: '
'
    Ok: '	'
    Ok: '''
    Ok: '\'
  |}]

let%expect_test "pattern constants - strings from string" =
  let test_cases = [
    "\"hello\"";
    "\"\"";
    "\"world with spaces\"";
    "\"unicode: é\"";
    "\"escaped \\\"quotes\\\"\"";
    "\"newline\\ntest\"";
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: "hello"
    Ok: ""
    Ok: "world with spaces"
    Ok: "unicode: é"
    Ok: "escaped \"quotes\""
    Ok: "newline
test"
  |}]

let%expect_test "pattern constants - booleans from string" =
  let test_cases = [
    "true";
    "false";
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: true
    Ok: false
  |}]

let%expect_test "pattern constants - unit from string" =
  let test_cases = [
    "()";
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: ()
  |}]

let%expect_test "pattern constants - constructors from string" =
  let test_cases = [
    "None";
    "Some";
    "Ok";
    "Error";
    "CustomConstructor";
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: None
    Ok: Some
    Ok: Ok
    Ok: Error
    Ok: CustomConstructor
  |}]

let%expect_test "pattern constants - polymorphic variants from string" =
  let test_cases = [
    "`Red";
    "`Blue";
    "`Custom_Tag";
    "`tag123";
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: `Red
    Ok: `Blue
    Ok: `Custom_Tag
    Ok: `tag123
  |}]

let%expect_test "pattern constants - empty collections from string" =
  let test_cases = [
    "[]";      (* empty list *)
    "[||]";    (* empty array *)
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: []
    Ok: [||]
  |}]

let%expect_test "pattern constants - begin end from string" =
  let test_cases = [
    "begin end";
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Ok: begin end
  |}]

let%expect_test "pattern constants - invalid cases" =
  let test_cases = [
    "lowercase";  (* not a constructor *)
    "123abc";     (* invalid number *)
    "'unclosed";  (* unclosed char *)
    "\"unclosed"; (* unclosed string *)
    "++";         (* invalid operator *)
  ] in
  List.iter (fun str ->
    print_constant_from_string_result (Oparser.Parser.parse_constant_string str "test")
  ) test_cases;
  [%expect {|
    Error: Expected constant
    Error: Lexer error
    Error: Lexer error
    Error: Lexer error
    Error: Expected constant
  |}]