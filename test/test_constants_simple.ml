open Base
open Stdio

(* Simple test to verify constant functionality without using the full parser *)

let test_constant_creation () =
  let open Oparser.Parse_tree in
  
  (* Test creating different types of constants *)
  let int_const = make_integer_literal 42 in
  let int32_const = make_int32_literal 42l in
  let int64_const = make_int64_literal 42L in
  let nativeint_const = make_nativeint_literal 42n in
  let float_const = make_float_literal 3.14 in
  let char_const = make_char_literal 'a' in
  let string_const = make_string_literal "hello" in
  let constr_const = make_constructor "Some" in
  let bool_false = make_false () in
  let bool_true = make_true () in
  let unit_const = make_unit () in
  let begin_end_const = make_begin_end () in
  let empty_list_const = make_empty_list () in
  let empty_array_const = make_empty_array () in
  let poly_variant_const = make_polymorphic_variant_tag "Red" in
  
  (* Test string representations *)
  printf "Integer: %s\n" (string_of_constant int_const);
  printf "Int32: %s\n" (string_of_constant int32_const);
  printf "Int64: %s\n" (string_of_constant int64_const);
  printf "NativeInt: %s\n" (string_of_constant nativeint_const);
  printf "Float: %s\n" (string_of_constant float_const);
  printf "Char: %s\n" (string_of_constant char_const);
  printf "String: %s\n" (string_of_constant string_const);
  printf "Constructor: %s\n" (string_of_constant constr_const);
  printf "False: %s\n" (string_of_constant bool_false);
  printf "True: %s\n" (string_of_constant bool_true);
  printf "Unit: %s\n" (string_of_constant unit_const);
  printf "Begin End: %s\n" (string_of_constant begin_end_const);
  printf "Empty List: %s\n" (string_of_constant empty_list_const);
  printf "Empty Array: %s\n" (string_of_constant empty_array_const);
  printf "Polymorphic Variant: %s\n" (string_of_constant poly_variant_const);
  
  (* Test S-expression serialization *)
  printf "\nS-expressions:\n";
  printf "Integer: %s\n" (Sexp.to_string_hum (sexp_of_constant int_const));
  printf "Constructor: %s\n" (Sexp.to_string_hum (sexp_of_constant constr_const));
  printf "Polymorphic Variant: %s\n" (Sexp.to_string_hum (sexp_of_constant poly_variant_const));

let () = test_constant_creation ()