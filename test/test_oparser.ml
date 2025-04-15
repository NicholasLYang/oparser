open Oparser

let () =
  (* Print.print_tokens_from_string "foo _foo _ foo_bar foo1";
  Print.print_tokens_from_string "0xabc 10 0o10 0b10";
  Print.print_tokens_from_string "0xa_bc 1_0 0o10 0b_10";*)
  Print.print_tokens_from_string "0xabcl 1_0n 0o10L 0b10n";
  (* Print.print_tokens_from_string "'a' 'b' 'c'";
  Print.print_tokens_from_string "'a ";
  Print.print_tokens_from_string "'\\a'" *)
