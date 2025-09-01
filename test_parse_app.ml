open Oparser
let test_tokens = [Token.Ident "int"]
let result = Parser.parse_app_type test_tokens
let () = match result with
  | Ok _ -> print_endline "Success"
  | Error e -> print_endline ("Error: " ^ e)