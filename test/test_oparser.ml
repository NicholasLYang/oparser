open Oparser


let () = (
  Print.print_tokens "foo _foo _ foo_bar foo1" "test";
  Print.print_tokens "10 101" "test"
)