open Base
open Stdio
open Oparser

let test_expr input expected_description =
  let source : Grace.Source.t = 
    `String { name = Some "test"; content = input } in
  match Parser.parse_expr_string input source with
  | Ok expr ->
      printf "%s\n  Input: %s\n  AST: %s\n\n" expected_description input
        (Sexp.to_string_hum (Parse_tree.sexp_of_expr expr))
  | Error err -> printf "ERROR parsing '%s': %s\n\n" input err

let () =
  printf "Testing Expression Parsing\n";
  printf "==========================\n\n";

  (* Basic expressions *)
  test_expr "42" "Integer literal";
  test_expr "true" "Boolean literal";
  test_expr "\"hello\"" "String literal";
  test_expr "'a'" "Character literal";
  test_expr "x" "Variable";
  test_expr "()" "Unit";
  test_expr "[]" "Empty list";
  test_expr "[||]" "Empty array";

  (* Function application *)
  test_expr "f x" "Simple function application";
  test_expr "f x y" "Multiple argument application";
  test_expr "f ~label:x" "Labeled argument";
  test_expr "f ?opt" "Optional argument";

  (* Operators *)
  test_expr "x + y" "Addition";
  test_expr "x * y + z" "Operator precedence";
  test_expr "x :: y :: []" "List construction";

  (* Control flow *)
  test_expr "if x then y else z" "If-then-else";
  test_expr "while x do y done" "While loop";
  (* test_expr "for i = 0 to 10 do print_int i done" "For loop"; *)

  (* Pattern matching *)
  (* test_expr "match x with | 0 -> \"zero\" | _ -> \"other\"" "Match expression"; *)
  test_expr "function | [] -> 0 | _ -> 1" "Function expression";

  (* Let bindings *)
  test_expr "let x = 42 in x + 1" "Simple let binding";
  test_expr "let rec fact n = if n = 0 then 1 else n * fact (n-1) in fact 5"
    "Recursive let binding";

  (* Sequences *)
  test_expr "print_string \"hello\"; 42" "Sequence expression";

  (* Arrays and records *)
  test_expr "[| 1; 2; 3 |]" "Array literal";
  test_expr "{ x = 1; y = 2 }" "Record literal";
  test_expr "{ p with x = 3 }" "Record update";
  test_expr "arr.(i)" "Array access";
  test_expr "str.[i]" "String access";

  (* Advanced features *)
  test_expr "lazy (expensive_computation ())" "Lazy expression";
  test_expr "assert (x > 0)" "Assert expression";
  test_expr "raise Not_found" "Raise expression";
  test_expr "let open List in map" "Local open";
  test_expr "List.(map)" "Module open syntax";
  test_expr "(x : int)" "Type constraint";
  test_expr "(x :> num)" "Coercion";
  test_expr "(x : int :> num)" "Subtyping coercion";

  (* Tuples *)
  test_expr "(x, y)" "Pair";
  test_expr "(x, y, z)" "Triple";

  (* Polymorphic variants *)
  test_expr "`Red" "Polymorphic variant without argument";
  test_expr "`RGB (255, 0, 0)" "Polymorphic variant with argument";

  (* Complex expressions *)
  test_expr "List.fold_left ( + ) 0 [1; 2; 3]" "Complex function application";
  test_expr "fun x y -> x + y" "Lambda expression";
  test_expr "try Some (List.hd xs) with _ -> None" "Try-with expression"