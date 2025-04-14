open Oparser

let () =
  Print.print_tokens_from_string "foo _foo _ foo_bar foo1";
  Print.print_tokens_from_string "0xabc 10 0o10 0b10";
  Print.print_tokens_from_string "'a' 'b' 'c'";
  Print.print_tokens_from_string "'a "
