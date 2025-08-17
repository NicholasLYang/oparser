open Oparser
open Parser
open Token

(* Helper function to create tokens for testing *)
let make_tokens tokens = tokens

let print_parse_result = function
  | Ok ((Some path, name), _) -> Printf.printf "Ok (Some [%s], %s)" (String.concat "." path) name
  | Ok ((None, name), _) -> Printf.printf "Ok (None, %s)" name
  | Error msg -> Printf.printf "Error: %s" msg

(* Tests for basic identifier parsers *)
let%expect_test "parse_ident - valid identifiers" =
  let test_cases = [
    Ident "foo";
    Ident "_bar";
    Ident "baz123";
    Ident "hello_world";
    Ident "test'";
  ] in
  List.iter (fun token ->
    match parse_ident token with
    | Ok s -> Printf.printf "Ok: %s\n" s
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) test_cases;
  [%expect {|
    Ok: foo
    Ok: _bar
    Ok: baz123
    Ok: hello_world
    Ok: test'
  |}]

let%expect_test "parse_ident - invalid identifiers" =
  let test_cases = [
    String "not_ident";
    Number (Int 42);
    Plus;
  ] in
  List.iter (fun token ->
    match parse_ident token with
    | Ok s -> Printf.printf "Ok: %s\n" s
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) test_cases;
  [%expect {|
    Error: Expected identifier
    Error: Expected identifier
    Error: Expected identifier
  |}]

let%expect_test "parse_capitalized_ident - valid" =
  let test_cases = [
    Ident "Foo";
    Ident "Bar123";
    Ident "Hello_world";
    Ident "TEST'";
  ] in
  List.iter (fun token ->
    match parse_capitalized_ident token with
    | Ok s -> Printf.printf "Ok: %s\n" s
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) test_cases;
  [%expect {|
    Ok: Foo
    Ok: Bar123
    Ok: Hello_world
    Ok: TEST'
  |}]

let%expect_test "parse_capitalized_ident - invalid" =
  let test_cases = [
    Ident "foo";
    Ident "_bar";
    Ident "123abc";
  ] in
  List.iter (fun token ->
    match parse_capitalized_ident token with
    | Ok s -> Printf.printf "Ok: %s\n" s
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) test_cases;
  [%expect {|
    Error: Expected capitalized identifier
    Error: Expected capitalized identifier
    Error: Expected capitalized identifier
  |}]

let%expect_test "parse_lowercase_ident - valid" =
  let test_cases = [
    Ident "foo";
    Ident "_bar";
    Ident "baz123";
    Ident "hello_world";
  ] in
  List.iter (fun token ->
    match parse_lowercase_ident token with
    | Ok s -> Printf.printf "Ok: %s\n" s
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) test_cases;
  [%expect {|
    Ok: foo
    Ok: _bar
    Ok: baz123
    Ok: hello_world
  |}]

let%expect_test "parse_lowercase_ident - invalid" =
  let test_cases = [
    Ident "Foo";
    Ident "Bar123";
  ] in
  List.iter (fun token ->
    match parse_lowercase_ident token with
    | Ok s -> Printf.printf "Ok: %s\n" s
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) test_cases;
  [%expect {|
    Error: Expected lowercase identifier
    Error: Expected lowercase identifier
  |}]

(* Tests for path parsers *)
let%expect_test "parse_value_path - simple value name" =
  let tokens = [Ident "foo"] in
  (match parse_value_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=None, name=foo
  |}]

let%expect_test "parse_value_path - with module path" =
  let tokens = [Ident "Module"; Dot; Ident "value"] in
  (match parse_value_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=Module, name=value
  |}]

let%expect_test "parse_value_path - nested module path" =
  let tokens = [Ident "Outer"; Dot; Ident "Inner"; Dot; Ident "value"] in
  (match parse_value_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected value name |}]

let%expect_test "parse_constr - simple constructor" =
  let tokens = [Ident "Some"] in
  (match parse_constr tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=None, name=Some
  |}]

let%expect_test "parse_constr - with module path" =
  let tokens = [Ident "Option"; Dot; Ident "Some"] in
  (match parse_constr tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Ok: path=Option, name=Some |}]

let%expect_test "parse_typeconstr - simple type" =
  let tokens = [Ident "int"] in
  (match parse_typeconstr tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=None, name=int
  |}]

let%expect_test "parse_typeconstr - with extended module path" =
  let tokens = [Ident "Core"; Dot; Ident "int"] in
  (match parse_typeconstr tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected type constructor name |}]

let%expect_test "parse_field - simple field" =
  let tokens = [Ident "name"] in
  (match parse_field tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=None, name=name
  |}]

let%expect_test "parse_field - with module path" =
  let tokens = [Ident "Person"; Dot; Ident "name"] in
  (match parse_field tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=Person, name=name
  |}]

let%expect_test "parse_module_name - valid" =
  let test_cases = [
    Ident "List";
    Ident "String";
    Ident "MyModule";
  ] in
  List.iter (fun token ->
    match parse_module_name token with
    | Ok s -> Printf.printf "Ok: %s\n" s
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) test_cases;
  [%expect {|
    Ok: List
    Ok: String
    Ok: MyModule
  |}]

let%expect_test "parse_module_name - invalid" =
  let test_cases = [
    Ident "list";
    Ident "_module";
  ] in
  List.iter (fun token ->
    match parse_module_name token with
    | Ok s -> Printf.printf "Ok: %s\n" s
    | Error msg -> Printf.printf "Error: %s\n" msg
  ) test_cases;
  [%expect {|
    Error: Expected module name
    Error: Expected module name
  |}]

let%expect_test "parse_class_path - simple class" =
  let tokens = [Ident "widget"] in
  (match parse_class_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=None, name=widget
  |}]

let%expect_test "parse_class_path - with module path" =
  let tokens = [Ident "Ui"; Dot; Ident "Widget"; Dot; Ident "button"] in
  (match parse_class_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected class name |}]

let%expect_test "parse_modtype_path - simple module type" =
  let tokens = [Ident "COMPARABLE"] in
  (match parse_modtype_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=None, name=COMPARABLE
  |}]

let%expect_test "parse_modtype_path - with extended module path" =
  let tokens = [Ident "Base"; Dot; Ident "COMPARABLE"] in
  (match parse_modtype_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Ok: path=None, name=Base |}]

let%expect_test "parse_classtype_path - simple classtype" =
  let tokens = [Ident "comparable"] in
  (match parse_classtype_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {|
    Ok: path=None, name=comparable
  |}]

let%expect_test "parse_classtype_path - with extended module path" =
  let tokens = [Ident "Core"; Dot; Ident "comparable"] in
  (match parse_classtype_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected class name |}]

(* Test error cases *)
let%expect_test "parse_value_path - invalid value name" =
  let tokens = [Ident "Foo"] in  (* capitalized, not valid for value *)
  (match parse_value_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected value name |}]

let%expect_test "parse_constr - invalid constructor name" =
  let tokens = [Ident "foo"] in  (* lowercase, not valid for constructor *)
  (match parse_constr tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected constructor name |}]

let%expect_test "parse_value_path - incomplete module path" =
  let tokens = [Ident "Module"; Dot] in  (* missing value name after dot *)
  (match parse_value_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Error: Expected value name |}]

(* Additional tests to verify working module paths *)
let%expect_test "parse_value_path - working module path case" =
  let tokens = [Ident "List"; Dot; Ident "map"] in
  (match parse_value_path tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Ok: path=List, name=map |}]

let%expect_test "parse_constr - working module path case" =
  let tokens = [Ident "Option"; Dot; Ident "Some"] in
  (match parse_constr tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Ok: path=Option, name=Some |}]

let%expect_test "parse_field - working module path case" =
  let tokens = [Ident "Person"; Dot; Ident "name"] in
  (match parse_field tokens with
   | Ok ((path, name), _) -> 
       let path_str = match path with Some p -> String.concat "." p | None -> "None" in
       Printf.printf "Ok: path=%s, name=%s\n" path_str name
   | Error msg -> Printf.printf "Error: %s\n" msg);
  [%expect {| Ok: path=Person, name=name |}]

(* Debug test to understand module path parsing *)
let%expect_test "debug_module_path_parsing" =
  let tokens = [Ident "Option"; Dot; Ident "Some"] in
  (match parse_module_path tokens with
   | Ok (path, rest) -> 
       Printf.printf "Module path: [%s], remaining: %d tokens\n" 
         (String.concat "." path) (List.length rest)
   | Error msg -> Printf.printf "Module path error: %s\n" msg);
  [%expect {| Module path: [Option.Some], remaining: 0 tokens |}]