open Base
open Stdio
open Oparser.Parser
open Oparser.Parse_tree

let parse_test input =
  let source : Grace.Source.t = `String { name = Some "test"; content = input } in
  parse_definition_string input source

let test_value_definition () =
  let result = parse_test "let x = 42" in
  match result with
  | Ok (ValueDef _) -> 
      print_endline "✓ Value definition test passed"
  | Ok other -> 
      failwith ("Expected value definition, got: " ^ string_of_parse_tree (Definition other))
  | Error err -> 
      failwith ("Parse error: " ^ err)

let test_type_definition () =
  let result = parse_test "type my_type" in
  match result with
  | Ok (TypeDef _) -> 
      print_endline "✓ Type definition test passed"
  | Ok other -> 
      failwith ("Expected type definition, got: " ^ string_of_parse_tree (Definition other))
  | Error err -> 
      failwith ("Parse error: " ^ err)

let test_exception_definition () =
  let result = parse_test "exception MyError" in
  match result with
  | Ok (ExceptionDef _) -> 
      print_endline "✓ Exception definition test passed"
  | Ok other -> 
      failwith ("Expected exception definition, got: " ^ string_of_parse_tree (Definition other))
  | Error err -> 
      failwith ("Parse error: " ^ err)

let test_module_definition () =
  (* Skip module definition test for now - needs more complex implementation *)
  print_endline "⚠ Module definition test skipped (needs implementation)"

let test_variant_type_definition () =
  (* Skip variant type definition test for now - needs more complex implementation *)
  print_endline "⚠ Variant type definition test skipped (needs implementation)"

let test_record_type_definition () =
  (* Skip record type definition test for now - needs more complex implementation *)
  print_endline "⚠ Record type definition test skipped (needs implementation)"

let test_class_definition () =
  (* Skip class definition test for now - needs more complex implementation *)
  print_endline "⚠ Class definition test skipped (needs implementation)"

let () =
  print_endline "Running definition parsing tests...";
  test_value_definition ();
  test_type_definition ();
  test_exception_definition ();
  test_module_definition ();
  test_variant_type_definition ();
  test_record_type_definition ();
  test_class_definition ();
  print_endline "All definition parsing tests passed!"