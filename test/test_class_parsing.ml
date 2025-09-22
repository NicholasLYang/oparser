open Base
open Stdio
open Oparser.Parser
open Oparser.Parse_tree

let parse_test input =
  let source : Grace.Source.t = `String { name = Some "test"; content = input } in
  parse_expr_string input source

let test_method_call () =
  let result = parse_test "obj#method_name" in
  match result with
  | Ok (MethodCall (ValuePathExpr (None, "obj"), "method_name", [])) -> 
      print_endline "✓ Method call test passed"
  | Ok other -> 
      failwith ("Expected method call, got: " ^ string_of_expr other)
  | Error err -> 
      failwith ("Parse error: " ^ err)

let test_method_call_with_args () =
  let result = parse_test "obj#method_name arg1 arg2" in
  match result with
  | Ok (MethodCall (ValuePathExpr (None, "obj"), "method_name", 
                   [SimpleArg (ValuePathExpr (None, "arg1")); 
                    SimpleArg (ValuePathExpr (None, "arg2"))])) -> 
      print_endline "✓ Method call with args test passed"
  | Ok other -> 
      failwith ("Expected method call with args, got: " ^ string_of_expr other)
  | Error err -> 
      failwith ("Parse error: " ^ err)

let test_new_instance () =
  let result = parse_test "new my_class" in
  match result with
  | Ok (NewInstance ((None, "my_class"), [])) -> 
      print_endline "✓ New instance test passed"
  | Ok other -> 
      failwith ("Expected new instance, got: " ^ string_of_expr other)
  | Error err -> 
      failwith ("Parse error: " ^ err)

let test_new_instance_with_args () =
  let result = parse_test "new my_class arg1 arg2" in
  match result with
  | Ok (NewInstance ((None, "my_class"), 
                    [SimpleArg (ValuePathExpr (None, "arg1")); 
                     SimpleArg (ValuePathExpr (None, "arg2"))])) -> 
      print_endline "✓ New instance with args test passed"
  | Ok other -> 
      failwith ("Expected new instance with args, got: " ^ string_of_expr other)
  | Error err -> 
      failwith ("Parse error: " ^ err)

let test_chained_method_calls () =
  let result = parse_test "obj#method1#method2" in
  match result with
  | Ok (MethodCall (MethodCall (ValuePathExpr (None, "obj"), "method1", []), "method2", [])) -> 
      print_endline "✓ Chained method calls test passed"
  | Ok other -> 
      failwith ("Expected chained method calls, got: " ^ string_of_expr other)
  | Error err -> 
      failwith ("Parse error: " ^ err)

let test_class_type_parsing () =
  (* Test basic class type path *)
  let source1 : Grace.Source.t = `String { name = Some "test"; content = "my_class" } in
  let result1 = Oparser.Parser.parse_class_type_string "my_class" source1 in
  (match result1 with
  | Ok _ -> print_endline "✓ Basic class type parsing test passed"
  | Error err -> failwith ("Class type parse error: " ^ err));
  
  (* Test simple object class type *)
  let source2 : Grace.Source.t = `String { name = Some "test"; content = "object end" } in
  let result2 = Oparser.Parser.parse_class_type_string "object end" source2 in
  (match result2 with
  | Ok _ -> print_endline "✓ Simple object class type parsing test passed"
  | Error err -> failwith ("Simple object class type parse error: " ^ err))

let test_object_expression_parsing () =
  (* Test simple object expression *)
  let result = parse_test "object end" in
  match result with
  | Ok _ -> print_endline "✓ Simple object expression parsing test passed"
  | Error err -> failwith ("Object expression parse error: " ^ err)

let () =
  print_endline "Running class parsing tests...";
  test_method_call ();
  test_method_call_with_args ();
  test_new_instance ();
  test_new_instance_with_args ();
  test_chained_method_calls ();
  test_class_type_parsing ();
  test_object_expression_parsing ();
  print_endline "All tests passed!"