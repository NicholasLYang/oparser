open Core

let print_tokens str =
  let lexer = Lexer.create str in

  let rec print_next () =
    match Lexer.get_next lexer with
    | Ok (Some token) ->
        Stdlib.Format.printf " [%s] " (Lexer.string_of_token token);
        print_next ()
    | Ok None -> Stdlib.Format.printf "end"
    | Error diagnostic -> Lexer.print_error diagnostic
  in

  print_next ()

let print_tokens_from_file filename =
  try
    let content = Stdio.In_channel.read_all filename in
    print_tokens content
  with Sys_error msg -> Stdlib.Format.printf "Error reading file: %s\n" msg
;;

print_tokens_from_file "test.txt"
