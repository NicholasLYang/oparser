open Oparser

let%expect_test _ =
  Print.print_tokens_from_string
    {| '\"' '\'' '\\' '\n' '\t' '\b' '\r' '\ ' '\x61' |};
  [%expect
    {| [<char> " (1-4)]  [<char> ' (6-9)]  [<char> \ (11-14)]  [<char> n (16-19)]  [<char> t (21-24)]  [<char> b (26-29)]  [<char> r (31-34)]  [<char>   (36-39)]  [<char> a (41-46)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "foo _foo _ foo_bar foo1";
  [%expect
    {| [<ident> foo (0-3)]  [<ident> _foo (4-8)]  [<ident> _ (9-10)]  [<ident> foo_bar (11-18)]  [<ident> foo1 (19-23)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "0xabc 10 0o10 0b10";
  [%expect
    {| [<int> 2748 (0-4)]  [<int> 10 (6-7)]  [<int> 8 (9-12)]  [<int> 2 (14-17)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "if let for open or";
  [%expect
    {| [if (0-2)]  [let (3-6)]  [for (7-10)]  [open (11-15)]  [or (16-18)] |}]

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
  [%expect {| [<ident> a (0-1)]  [<infixop> <%> (2-5)]  [<ident> b (6-7)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "a >>= b";
  [%expect {| [<ident> a (0-1)]  [<infixop> >>= (2-5)]  [<ident> b (6-7)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "{|||}";
  [%expect {| [<quotedstring> | (0-4)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "{abc|hello world|abc}";
  [%expect {| [<quotedstring> hello world (0-20)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "{test|\"quotes\" and \\escapes\\ and newlines|test}";
  [%expect {| [<quotedstring> "quotes" and \escapes\ and newlines (0-46)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "{foo_bar|content with | pipe|foo_bar}";
  [%expect {| [<quotedstring> content with | pipe (0-36)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "{|regular brace";
  [%expect {xxx|
    [1m[91merror[0;1m[0m[1m[91m[E????][0;1m[0m: [1mUnterminated quoted string literal[0m
        [36m┌─[0m {|regular brace:1:1
    [36m  1[0m [36m│[0m  [31m{|regular brace[0m
        [36m│[0m  [31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m[31m^[0m [31mUnterminated quoted string literal[0m
    |xxx}]

let%expect_test _ =
  Print.print_tokens_from_string "{ foo }";
  [%expect {| [{ (0-0)]  [<ident> foo (2-5)]  [} (6-6)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "{123|not valid";
  [%expect {| [{ (0-0)]  [<int> 123 (1-3)]  [<infixop> | (4-5)]  [<ident> not (5-8)]  [<ident> valid (9-14)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "{||}";
  [%expect {| [<quotedstring>  (0-3)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "{_underscore|content with newlines\nand symbols!@#$%|_underscore}";
  [%expect {|
     [<quotedstring> content with newlines
    and symbols!@#$% (0-63)]
    |}]

let%expect_test _ =
  Print.print_tokens_from_string "#123 \"test.ml\"\nlet x = 42";
  [%expect {| [let (15-18)]  [<ident> x (19-20)]  [<infixop> = (21-22)]  [<int> 42 (23-24)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "#1 \"file.ml\" let x = 1";
  [%expect {| [let (13-16)]  [<ident> x (17-18)]  [<infixop> = (19-20)]  [<int> 1 (21-21)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "#42 \"source\\nfile.ml\"\n(* comment *) y";
  [%expect {| [<ident> y (36-37)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "#1 \"test\\\"quote.ml\"\nfun x -> x";
  [%expect {| [fun (20-23)]  [<ident> x (24-25)]  [-> (26-27)]  [<ident> x (29-30)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "#999 \"file with\\x20spaces.ml\"\ntype t = int";
  [%expect {| [type (30-34)]  [<ident> t (35-36)]  [<infixop> = (37-38)]  [<ident> int (39-42)] |}]

let%expect_test _ =
  Print.print_tokens_from_string "x + y\n#42 \"newfile.ml\"\nz * w";
  [%expect {| [<ident> x (0-1)]  [+ (2-2)]  [<ident> y (4-5)]  [<ident> z (23-24)]  [<infixop> * (25-26)]  [<ident> w (27-28)] |}]
