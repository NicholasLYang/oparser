open Base
open Stdio
open Oparser
open Parse_tree

(* REPL for OCaml expression parsing *)

type parse_mode = TypeMode | ExprMode

let mode = ref TypeMode

let print_prompt () =
  let prompt = match !mode with
    | TypeMode -> "type# "
    | ExprMode -> "expr# "
  in
  Out_channel.output_string stdout prompt;
  Out_channel.flush stdout

let parse_and_print_sexp input =
  let source : Grace.Source.t =
    `String { name = Some "<file>"; content = input }
  in
  match !mode with
  | TypeMode ->
      let lexer = Lexer.create input source in
      (match Parser.parse_from_lexer lexer with
      | Ok ast ->
          let sexp = sexp_of_parse_tree ast in
          Stdio.printf "%s\n" (Sexp.to_string_hum sexp)
      | Error error_msg -> Stdio.printf "Parse error: %s\n" error_msg)
  | ExprMode ->
      (match Parser.parse_expr_string input source with
      | Ok expr ->
          let ast = Expr expr in
          let sexp = sexp_of_parse_tree ast in
          Stdio.printf "%s\n" (Sexp.to_string_hum sexp)
      | Error error_msg -> Stdio.printf "Parse error: %s\n" error_msg)

let rec repl_loop () =
  print_prompt ();
  match In_channel.input_line stdin with
  | None ->
      Stdio.printf "\nBye!\n";
      ()
  | Some line ->
      let trimmed = String.strip line in
      if String.is_empty trimmed then repl_loop ()
      else if String.equal trimmed ":quit" || String.equal trimmed ":q" then (
        Stdio.printf "Bye!\n";
        ())
      else if String.equal trimmed ":type" then (
        mode := TypeMode;
        Stdio.printf "Switched to type expression mode\n";
        repl_loop ())
      else if String.equal trimmed ":expr" then (
        mode := ExprMode;
        Stdio.printf "Switched to expression mode\n";
        repl_loop ())
      else (
        parse_and_print_sexp trimmed;
        repl_loop ())

let () =
  Stdio.printf "OCaml Expression Parser REPL\n";
  Stdio.printf
    "Enter OCaml type expressions or expressions to see their parse trees as S-expressions.\n";
  Stdio.printf "Commands:\n";
  Stdio.printf "  :type - Switch to type expression parsing mode\n";
  Stdio.printf "  :expr - Switch to expression parsing mode\n";
  Stdio.printf "  :quit or :q - Exit\n\n";
  repl_loop ()
