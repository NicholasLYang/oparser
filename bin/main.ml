open Core

let print_tokens str file_path =
  let lexer = Lexer.create str file_path in

  let rec print_next () =
    match Lexer.get_next lexer with
    | Ok (Some token) ->
        let token_string = Lexer.string_of_token (Span.value token) in
        let span = Span.span token in
        let start_index = Span.start_index span in
        let end_index = Span.end_index span in
        Stdlib.Format.printf " [%s (%i-%i)] " token_string start_index end_index;
        print_next ()
    | Ok None -> Stdlib.Format.printf "\n"
    | Error diagnostic -> Lexer.print_error diagnostic
  in

  print_next ()

let print_tokens_from_file filename =
  try
    let content = Stdio.In_channel.read_all filename in
    print_tokens content filename
  with Sys_error msg -> Stdlib.Format.printf "Error reading file: %s\n" msg
;;

print_tokens_from_file "test.txt"
