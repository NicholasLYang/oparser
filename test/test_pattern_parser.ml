open Base
open Oparser.Parser
open Oparser.Parse_tree

(* Test basic pattern parsing *)
let test_basic_patterns () =
  (* Test wildcard pattern *)
  (match parse_pattern_string "_" "test" with
   | Ok (PatternWildcard) -> print_endline "✓ Wildcard pattern"
   | Ok p -> print_endline ("✗ Expected wildcard, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Wildcard parsing failed: " ^ e));

  (* Test value name pattern *)
  (match parse_pattern_string "x" "test" with
   | Ok (ValueName "x") -> print_endline "✓ Value name pattern"
   | Ok p -> print_endline ("✗ Expected value name, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Value name parsing failed: " ^ e));

  (* Test constructor pattern *)
  (match parse_pattern_string "None" "test" with
   | Ok (PatternConstructor (None, "None")) -> print_endline "✓ Constructor pattern"
   | Ok p -> print_endline ("✗ Expected constructor, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Constructor parsing failed: " ^ e))

(* Test constant patterns *)
let test_constant_patterns () =
  (* Test integer constant *)
  (match parse_pattern_string "42" "test" with
   | Ok (PatternConstant (IntegerLiteral 42)) -> print_endline "✓ Integer constant pattern"
   | Ok p -> print_endline ("✗ Expected integer constant, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Integer constant parsing failed: " ^ e));

  (* Test string constant *)
  (match parse_pattern_string "\"hello\"" "test" with
   | Ok (PatternConstant (StringLiteral "hello")) -> print_endline "✓ String constant pattern"
   | Ok p -> print_endline ("✗ Expected string constant, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ String constant parsing failed: " ^ e));

  (* Test character constant *)
  (match parse_pattern_string "'a'" "test" with
   | Ok (PatternConstant (CharLiteral 'a')) -> print_endline "✓ Character constant pattern"
   | Ok p -> print_endline ("✗ Expected character constant, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Character constant parsing failed: " ^ e));

  (* Test boolean constants *)
  (match parse_pattern_string "true" "test" with
   | Ok (PatternConstant True) -> print_endline "✓ Boolean true constant pattern"
   | Ok p -> print_endline ("✗ Expected true constant, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ True constant parsing failed: " ^ e));

  (match parse_pattern_string "false" "test" with
   | Ok (PatternConstant False) -> print_endline "✓ Boolean false constant pattern"
   | Ok p -> print_endline ("✗ Expected false constant, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ False constant parsing failed: " ^ e))

(* Test tuple patterns *)
let test_tuple_patterns () =
  (* Test simple tuple *)
  (match parse_pattern_string "x, y" "test" with
   | Ok (TuplePattern [ValueName "x"; ValueName "y"]) -> print_endline "✓ Simple tuple pattern"
   | Ok p -> print_endline ("✗ Expected tuple pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Tuple parsing failed: " ^ e));

  (* Test nested tuple *)
  (match parse_pattern_string "x, (y, z)" "test" with
   | Ok (TuplePattern [ValueName "x"; ParenthesizedPattern (TuplePattern [ValueName "y"; ValueName "z"])]) ->
       print_endline "✓ Nested tuple pattern"
   | Ok p -> print_endline ("✗ Expected nested tuple, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Nested tuple parsing failed: " ^ e));

  (* Test tuple with wildcard *)
  (match parse_pattern_string "_, x, _" "test" with
   | Ok (TuplePattern [PatternWildcard; ValueName "x"; PatternWildcard]) -> print_endline "✓ Tuple with wildcard pattern"
   | Ok p -> print_endline ("✗ Expected tuple with wildcard, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Tuple with wildcard parsing failed: " ^ e))

(* Test list patterns *)
let test_list_patterns () =
  (* Test empty list *)
  (match parse_pattern_string "[]" "test" with
   | Ok (ListPattern []) -> print_endline "✓ Empty list pattern"
   | Ok p -> print_endline ("✗ Expected empty list, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Empty list parsing failed: " ^ e));

  (* Test single element list *)
  (match parse_pattern_string "[x]" "test" with
   | Ok (ListPattern [ValueName "x"]) -> print_endline "✓ Single element list pattern"
   | Ok p -> print_endline ("✗ Expected single element list, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Single element list parsing failed: " ^ e));

  (* Test multiple element list *)
  (match parse_pattern_string "[x; y; z]" "test" with
   | Ok (ListPattern [ValueName "x"; ValueName "y"; ValueName "z"]) -> print_endline "✓ Multiple element list pattern"
   | Ok p -> print_endline ("✗ Expected multiple element list, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Multiple element list parsing failed: " ^ e))

(* Test cons patterns *)
let test_cons_patterns () =
  (* Test simple cons *)
  (match parse_pattern_string "x :: xs" "test" with
   | Ok (ConsPattern (ValueName "x", ValueName "xs")) -> print_endline "✓ Simple cons pattern"
   | Ok p -> print_endline ("✗ Expected cons pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Cons parsing failed: " ^ e));

  (* Test multiple cons *)
  (match parse_pattern_string "x :: y :: xs" "test" with
   | Ok (ConsPattern (ValueName "x", ConsPattern (ValueName "y", ValueName "xs"))) -> print_endline "✓ Multiple cons pattern"
   | Ok p -> print_endline ("✗ Expected multiple cons, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Multiple cons parsing failed: " ^ e))

(* Test constructor application patterns *)
let test_constructor_app_patterns () =
  (* Test Some constructor *)
  (match parse_pattern_string "Some x" "test" with
   | Ok (ConstructorPattern ((None, "Some"), ValueName "x")) -> print_endline "✓ Some constructor pattern"
   | Ok p -> print_endline ("✗ Expected Some constructor, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Some constructor parsing failed: " ^ e));

  (* Test nested constructor *)
  (match parse_pattern_string "Some (Some x)" "test" with
   | Ok (ConstructorPattern ((None, "Some"), ParenthesizedPattern (ConstructorPattern ((None, "Some"), ValueName "x")))) ->
       print_endline "✓ Nested constructor pattern"
   | Ok p -> print_endline ("✗ Expected nested constructor, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Nested constructor parsing failed: " ^ e))

(* Test array patterns *)
let test_array_patterns () =
  (* Test empty array *)
  (match parse_pattern_string "[||]" "test" with
   | Ok (ArrayPattern []) -> print_endline "✓ Empty array pattern"
   | Ok p -> print_endline ("✗ Expected empty array, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Empty array parsing failed: " ^ e));

  (* Test single element array *)
  (match parse_pattern_string "[|x|]" "test" with
   | Ok (ArrayPattern [ValueName "x"]) -> print_endline "✓ Single element array pattern"
   | Ok p -> print_endline ("✗ Expected single element array, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Single element array parsing failed: " ^ e));

  (* Test multiple element array *)
  (match parse_pattern_string "[|x; y; z|]" "test" with
   | Ok (ArrayPattern [ValueName "x"; ValueName "y"; ValueName "z"]) -> print_endline "✓ Multiple element array pattern"
   | Ok p -> print_endline ("✗ Expected multiple element array, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Multiple element array parsing failed: " ^ e))

(* Test record patterns *)
let test_record_patterns () =
  (* Test simple record *)
  (match parse_pattern_string "{x = a; y = b}" "test" with
   | Ok (RecordPattern ([{field_name = (None, "x"); pattern = ValueName "a"}; 
                         {field_name = (None, "y"); pattern = ValueName "b"}], false)) ->
       print_endline "✓ Simple record pattern"
   | Ok p -> print_endline ("✗ Expected record pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Record parsing failed: " ^ e));

  (* Test record with wildcard *)
  (match parse_pattern_string "{x = a; _}" "test" with
   | Ok (RecordPattern ([{field_name = (None, "x"); pattern = ValueName "a"}], true)) ->
       print_endline "✓ Record with wildcard pattern"
   | Ok p -> print_endline ("✗ Expected record with wildcard, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Record with wildcard parsing failed: " ^ e))

(* Test alias patterns *)
let test_alias_patterns () =
  (* Test simple alias *)
  (match parse_pattern_string "x as y" "test" with
   | Ok (PatternAlias (ValueName "x", "y")) -> print_endline "✓ Simple alias pattern"
   | Ok p -> print_endline ("✗ Expected alias pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Alias parsing failed: " ^ e));

  (* Test complex alias *)
  (match parse_pattern_string "(x, y) as pair" "test" with
   | Ok (PatternAlias (ParenthesizedPattern (TuplePattern [ValueName "x"; ValueName "y"]), "pair")) ->
       print_endline "✓ Complex alias pattern"
   | Ok p -> print_endline ("✗ Expected complex alias, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Complex alias parsing failed: " ^ e))

(* Test or patterns *)
let test_or_patterns () =
  (* Test simple or *)
  (match parse_pattern_string "x | y" "test" with
   | Ok (OrPattern (ValueName "x", ValueName "y")) -> print_endline "✓ Simple or pattern"
   | Ok p -> print_endline ("✗ Expected or pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Or parsing failed: " ^ e));

  (* Test multiple or *)
  (match parse_pattern_string "x | y | z" "test" with
   | Ok (OrPattern (ValueName "x", OrPattern (ValueName "y", ValueName "z"))) -> print_endline "✓ Multiple or pattern"
   | Ok p -> print_endline ("✗ Expected multiple or, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Multiple or parsing failed: " ^ e))

(* Test range patterns *)
let test_range_patterns () =
  (* Test character range *)
  (match parse_pattern_string "'a'..'z'" "test" with
   | Ok (RangePattern ('a', 'z')) -> print_endline "✓ Character range pattern"
   | Ok p -> print_endline ("✗ Expected range pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Range parsing failed: " ^ e))

(* Test lazy patterns *)
let test_lazy_patterns () =
  (* Test lazy pattern *)
  (match parse_pattern_string "lazy x" "test" with
   | Ok (LazyPattern (ValueName "x")) -> print_endline "✓ Lazy pattern"
   | Ok p -> print_endline ("✗ Expected lazy pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Lazy parsing failed: " ^ e))

(* Test exception patterns *)
let test_exception_patterns () =
  (* Test exception pattern *)
  (match parse_pattern_string "exception Exit" "test" with
   | Ok (ExceptionPattern (PatternConstructor (None, "Exit"))) -> print_endline "✓ Exception pattern"
   | Ok p -> print_endline ("✗ Expected exception pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Exception parsing failed: " ^ e))

(* Test parenthesized patterns *)
let test_parenthesized_patterns () =
  (* Test simple parenthesized *)
  (match parse_pattern_string "(x)" "test" with
   | Ok (ParenthesizedPattern (ValueName "x")) -> print_endline "✓ Parenthesized pattern"
   | Ok p -> print_endline ("✗ Expected parenthesized pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Parenthesized parsing failed: " ^ e))

(* Test complex patterns *)
let test_complex_patterns () =
  (* Test complex nested pattern *)
  (match parse_pattern_string "Some (x, [y; z]) as result" "test" with
   | Ok (PatternAlias (ConstructorPattern ((None, "Some"), 
                                          ParenthesizedPattern (TuplePattern [ValueName "x"; 
                                                                              ListPattern [ValueName "y"; ValueName "z"]])), 
                       "result")) ->
       print_endline "✓ Complex nested pattern"
   | Ok p -> print_endline ("✗ Expected complex pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Complex pattern parsing failed: " ^ e));

  (* Test pattern with or and cons *)
  (match parse_pattern_string "[] | x :: xs" "test" with
   | Ok (OrPattern (ListPattern [], ConsPattern (ValueName "x", ValueName "xs"))) ->
       print_endline "✓ Pattern with or and cons"
   | Ok p -> print_endline ("✗ Expected or/cons pattern, got: " ^ string_of_pattern p)
   | Error e -> print_endline ("✗ Or/cons pattern parsing failed: " ^ e))

(* Main test runner *)
let () =
  print_endline "=== Testing Pattern Parsing ===";
  print_endline "\n--- Basic Patterns ---";
  test_basic_patterns ();
  print_endline "\n--- Constant Patterns ---";
  test_constant_patterns ();
  print_endline "\n--- Tuple Patterns ---";
  test_tuple_patterns ();
  print_endline "\n--- List Patterns ---";
  test_list_patterns ();
  print_endline "\n--- Cons Patterns ---";
  test_cons_patterns ();
  print_endline "\n--- Constructor Application Patterns ---";
  test_constructor_app_patterns ();
  print_endline "\n--- Array Patterns ---";
  test_array_patterns ();
  print_endline "\n--- Record Patterns ---";
  test_record_patterns ();
  print_endline "\n--- Alias Patterns ---";
  test_alias_patterns ();
  print_endline "\n--- Or Patterns ---";
  test_or_patterns ();
  print_endline "\n--- Range Patterns ---";
  test_range_patterns ();
  print_endline "\n--- Lazy Patterns ---";
  test_lazy_patterns ();
  print_endline "\n--- Exception Patterns ---";
  test_exception_patterns ();
  print_endline "\n--- Parenthesized Patterns ---";
  test_parenthesized_patterns ();
  print_endline "\n--- Complex Patterns ---";
  test_complex_patterns ();
  print_endline "\n=== Pattern Testing Complete ==="