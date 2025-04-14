# OParser

An attempt to write an OCaml parser in OCaml by hand. Partially a learning exercise, partially a genuine attempt to make a nicer lexer/parser for OCaml with good error messages.

## Notes
- Gotta figure out unicode "correctly". Is there an easy way to turn a string into unicode?
- Seems like uutf works for that
- Weird that integers are lexed with `-`