open Base
open Stdio
open Oparser
open Parse_tree

(* REPL for OCaml expression parsing *)

let print_prompt () =
  Out_channel.output_string stdout "# ";
  Out_channel.flush stdout

let parse_and_print_sexp input =
  let source : Grace.Source.t =
    `String { name = Some "<file>"; content = input }
  in
  let lexer = Lexer.create input source in
  match Parser.parse_from_lexer lexer with
  | Ok ast ->
      let sexp = sexp_of_parse_tree ast in
      Stdio.printf "%s\n" (Sexp.to_string_hum sexp)
  | Error error_msg -> Stdio.printf "Parse error: %s\n" error_msg

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
      else (
        parse_and_print_sexp trimmed;
        repl_loop ())

let () =
  Stdio.printf "OCaml Expression Parser REPL\n";
  Stdio.printf
    "Enter OCaml type expressions or constants to see their parse trees as S-expressions.\n";
  Stdio.printf "Use :quit or :q to exit.\n\n";
  repl_loop ()
