open Oparser.Parse_tree

let%expect_test "type_var_string" =
  let t = make_type_var "a" in
  Printf.printf "%s\n" (string_of_typexpr t);
  [%expect {| 'a |}]

let%expect_test "wildcard_string" =
  let t = make_wildcard () in
  Printf.printf "%s\n" (string_of_typexpr t);
  [%expect {| _ |}]

let%expect_test "arrow_type_string" =
  let t1 = make_type_var "a" in
  let t2 = make_type_var "b" in
  let arrow = make_arrow t1 t2 in
  Printf.printf "%s\n" (string_of_typexpr arrow);
  [%expect {| 'a -> 'b |}]

let%expect_test "labeled_arrow_type_string" =
  let t1 = make_type_var "a" in
  let t2 = make_type_var "b" in
  let arrow = make_arrow ~label:"x" t1 t2 in
  Printf.printf "%s\n" (string_of_typexpr arrow);
  [%expect {| x:'a -> 'b |}]

let%expect_test "optional_arrow_type_string" =
  let t1 = make_type_var "a" in
  let t2 = make_type_var "b" in
  let arrow = make_arrow ~label:"x" ~optional:true t1 t2 in
  Printf.printf "%s\n" (string_of_typexpr arrow);
  [%expect {| ?x:'a -> 'b |}]

let%expect_test "tuple_type_string" =
  let t1 = make_type_var "a" in
  let t2 = make_type_var "b" in
  let t3 = make_type_var "c" in
  let tuple = make_tuple [t1; t2; t3] in
  Printf.printf "%s\n" (string_of_typexpr tuple);
  [%expect {| 'a * 'b * 'c |}]

let%expect_test "typeconstr_string" =
  let tc = make_typeconstr (None, "int") in
  Printf.printf "%s\n" (string_of_typexpr tc);
  [%expect {| int |}]

let%expect_test "qualified_typeconstr_string" =
  let tc = make_typeconstr (Some ["Core"], "int") in
  Printf.printf "%s\n" (string_of_typexpr tc);
  [%expect {| Core.int |}]

let%expect_test "type_application_string" =
  let t = make_type_var "a" in
  let tc = make_type_app t (None, "list") in
  Printf.printf "%s\n" (string_of_typexpr tc);
  [%expect {| 'a list |}]

let%expect_test "multi_type_application_string" =
  let t1 = make_type_var "a" in
  let t2 = make_type_var "b" in
  let tc = make_type_app_multi [t1; t2] (None, "map") in
  Printf.printf "%s\n" (string_of_typexpr tc);
  [%expect {| ('a, 'b) map |}]

let%expect_test "type_as_string" =
  let t = make_type_var "a" in
  let as_type = make_type_as t "x" in
  Printf.printf "%s\n" (string_of_typexpr as_type);
  [%expect {| 'a as 'x |}]

let%expect_test "object_empty_string" =
  let obj = make_object_empty () in
  Printf.printf "%s\n" (string_of_typexpr obj);
  [%expect {| < .. > |}]

let%expect_test "object_with_methods_string" =
  let method1 = make_method_type "foo" (make_mono_type (make_type_var "a")) in
  let method2 = make_method_type "bar" (make_mono_type (make_type_var "b")) in
  let obj = make_object [method1; method2] false false in
  Printf.printf "%s\n" (string_of_typexpr obj);
  [%expect {| < foo : 'a; bar : 'b > |}]

let%expect_test "object_with_semicolon_string" =
  let method1 = make_method_type "foo" (make_mono_type (make_type_var "a")) in
  let obj = make_object [method1] true false in
  Printf.printf "%s\n" (string_of_typexpr obj);
  [%expect {| < foo : 'a; > |}]

let%expect_test "object_with_dots_string" =
  let method1 = make_method_type "foo" (make_mono_type (make_type_var "a")) in
  let obj = make_object [method1] false true in
  Printf.printf "%s\n" (string_of_typexpr obj);
  [%expect {| < foo : 'a; .. > |}]

let%expect_test "classtype_string" =
  let ct = make_classtype (None, "widget") in
  Printf.printf "%s\n" (string_of_typexpr ct);
  [%expect {| #widget |}]

let%expect_test "qualified_classtype_string" =
  let ct = make_classtype (Some ["Ui"], "widget") in
  Printf.printf "%s\n" (string_of_typexpr ct);
  [%expect {| #Ui.widget |}]

let%expect_test "classtype_app_string" =
  let t = make_type_var "a" in
  let ct = make_classtype_app t (None, "comparable") in
  Printf.printf "%s\n" (string_of_typexpr ct);
  [%expect {| 'a #comparable |}]

let%expect_test "poly_type_string" =
  let t = make_arrow (make_type_var "a") (make_type_var "a") in
  let poly = make_poly_type ["a"] t in
  Printf.printf "%s\n" (string_of_poly_typexpr poly);
  [%expect {| 'a. 'a -> 'a |}]

let%expect_test "multi_poly_type_string" =
  let t = make_arrow (make_type_var "a") (make_type_var "b") in
  let poly = make_poly_type ["a"; "b"] t in
  Printf.printf "%s\n" (string_of_poly_typexpr poly);
  [%expect {| 'a 'b. 'a -> 'b |}]

let%expect_test "parenthesized_type_string" =
  let t1 = make_type_var "a" in
  let t2 = make_type_var "b" in
  let arrow = make_arrow t1 t2 in
  let paren = make_parenthesized arrow in
  Printf.printf "%s\n" (string_of_typexpr paren);
  [%expect {| ('a -> 'b) |}]

let%expect_test "complex_type_string" =
  (* int -> ('a * 'b) list *)
  let int_type = make_typeconstr (None, "int") in
  let a_type = make_type_var "a" in
  let b_type = make_type_var "b" in
  let tuple_type = make_tuple [a_type; b_type] in
  let paren_tuple = make_parenthesized tuple_type in
  let list_type = make_type_app paren_tuple (None, "list") in
  let arrow_type = make_arrow int_type list_type in
  Printf.printf "%s\n" (string_of_typexpr arrow_type);
  [%expect {| int -> ('a * 'b) list |}]

(* Test path string functions *)
let%expect_test "value_path_string" =
  let simple_path = (None, "map") in
  let qualified_path = (Some ["List"], "map") in
  Printf.printf "Simple: %s\n" (string_of_value_path simple_path);
  Printf.printf "Qualified: %s\n" (string_of_value_path qualified_path);
  [%expect {|
    Simple: map
    Qualified: List.map
  |}]