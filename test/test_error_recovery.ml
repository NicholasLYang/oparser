open Base
open Stdio
open Oparser

let test_partial_parse input expected_type =
  let source : Grace.Source.t =
    `String { name = Some "<test>"; content = input }
  in
  match Parser.parse_string input source with
  | Ok result ->
      printf "Input: %s\n" input;
      printf "Successfully parsed as: ";
      (match result with
       | Parse_tree.TypeExpr _ -> printf "TypeExpr\n"
       | Parse_tree.Pattern _ -> printf "Pattern\n"
       | Parse_tree.Expr _ -> printf "Expr\n"
       | Parse_tree.Constant _ -> printf "Constant\n"
       | _ -> printf "Other\n");
      printf "Expected: %s\n" expected_type;
      printf "S-expression: %s\n\n" (Sexp.to_string_hum (Parse_tree.sexp_of_parse_tree result))
  | Error e ->
      printf "Input: %s\n" input;
      printf "Error: %s\n\n" e

let () =
  printf "Testing error recovery in parser...\n\n";
  
  (* Test recovery in type expressions *)
  printf "=== Type Expression Recovery ===\n";
  test_partial_parse "int -> )" "TypeExpr";
  test_partial_parse "< method : int; error >" "TypeExpr";
  test_partial_parse "('a, 'b" "TypeExpr";
  
  (* Test recovery in patterns *)
  printf "\n=== Pattern Recovery ===\n";
  test_partial_parse "{ field = value; error" "Pattern";
  test_partial_parse "[1; 2; error" "Pattern";
  test_partial_parse "(pattern" "Pattern";
  
  (* Test recovery in expressions *)
  printf "\n=== Expression Recovery ===\n";
  test_partial_parse "if x then y" "Expr";
  test_partial_parse "match x with | A -> 1 | error" "Expr";
  test_partial_parse "let x = 5 in" "Expr";
  test_partial_parse "fun x ->" "Expr";
  
  (* Test cases that should partially parse *)
  printf "\n=== Partial Parse Recovery ===\n";
  test_partial_parse "int -> string garbage" "TypeExpr";
  test_partial_parse "let x = 5 in x + error" "Expr";
  test_partial_parse "match x with | A -> 1 | B ->" "Expr";
  
  printf "\nDone!\n"