open Oparser

let%expect_test _ =
  Print.print_tokens_from_string
    {| '\"' '\'' '\\' '\n' '\t' '\b' '\r' '\ ' '\x61' |};
  [%expect
    {| [<char> " (1-4)]  [<char> ' (6-9)]  [<char> \ (11-14)]  [<char> n (16-19)]  [<char> t (21-24)]  [<char> b (26-29)]  [<char> r (31-34)]  [<char>   (36-39)]  [<char> a (41-46)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "foo _foo _ foo_bar foo1";
  [%expect
    {| [<ident> foo (0-3)]  [<ident> _foo (4-8)]  [<ident> _ (9-10)]  [<ident> foo_bar (11-18)]  [<ident> foo1 (19-22)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "0xabc 10 0o10 0b10";
  [%expect
    {| [<int> 2748 (0-4)]  [<int> 10 (6-7)]  [<int> 8 (9-12)]  [<int> 2 (14-17)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "if let for open or";
  [%expect
    {| [if (0-2)]  [let (3-6)]  [for (7-10)]  [open (11-15)]  [or (16-17)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "(* comment *)";
  [%expect {| |}]

let%expect_test _ =
  Print.print_tokens_from_string "(* comment (* nested *) *)";
  [%expect {| |}]

let%expect_test _ =
  Print.print_tokens_from_string "\"foo\"";
  [%expect {| [<string> foo (0-4)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "\"\\u{1f600}\"";
  [%expect {| [<string> 😀 (0-10)] |}]

let%expect_test _ =
  Print.print_tokens_from_string " \" \\\" \"";
  [%expect {| [<string>  "  (1-6)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "\"\\n\"";
  [%expect {|
    [<string>
    (0-3)]
    |}]

let%expect_test _ =
  Print.print_tokens_from_string "0xa_bc 1_0 0o10 0b_10";
  [%expect
    {|
    [1m[91merror[0;1m[0m[1m[91m[E????][0;1m[0m: [1mNumbers cannot start with `_`[0m
        [36m┌─[0m 0xa_bc 1_0 0o10 0b_10:1:19
    [36m  1[0m [36m│[0m  0xa_bc 1_0 0o10 0b[31m_[0m10
        [36m│[0m                    [31m^[0m [31m`_` found in the first position of a number[0m
    |}]

let%expect_test _ =
  Print.print_tokens_from_string "0xabcl 1_0n 0o10L 0b10n";
  [%expect
    {| [<int32> 2748 (0-4)]  [<nativeint> 10 (7-9)]  [<int64> 8 (12-15)]  [<nativeint> 2 (18-21)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "'a' 'b' 'c'";
  [%expect {| [<char> a (0-2)]  [<char> b (4-6)]  [<char> c (8-10)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "'a ";
  [%expect
    {|
    [1m[91merror[0;1m[0m[1m[91m[E????][0;1m[0m: [1mUnexpected character found[0m
        [36m┌─[0m 'a :1:3
    [36m  1[0m [36m│[0m  'a[31m [0m
        [36m│[0m    [31m^[0m [31mexpected `'` to delimit a character literal, instead found ` `[0m
    |}]

let%expect_test _ =
  Print.print_tokens_from_string "a <%> b";
  [%expect {| [<ident> a (0-1)]  [<infixop> <%> (2-5)]  [<ident> b (6-6)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "a >>= b";
  [%expect {| [<ident> a (0-1)]  [<infixop> >>= (2-5)]  [<ident> b (6-6)] |}]
