# OCaml Parser Project (oparser)

## Overview

This project is an OCaml parser that can lex and parse OCaml type expressions, converting them into S-expressions for analysis. It includes a REPL (Read-Eval-Print Loop) for interactive parsing and exploration.

## Project Structure

```
oparser/
   lib/                    # Core parsing library
      lexer.ml/.mli      # Lexical analyzer for OCaml tokens
      parser.ml/.mli     # Parser for OCaml type expressions
      parse_tree.ml/.mli # AST definitions with S-expression support
      token.ml           # Token definitions
      span.ml/.mli       # Source location tracking
      print.ml           # Pretty printing utilities
   bin/                   # Executable REPL
      main.ml           # REPL implementation
      dune              # Build configuration for executable
   test/                 # Test suite
      test_*.ml         # Various test files
   dune-project          # Main project configuration
```

## Features

### Lexer (`lib/lexer.ml`)
- Tokenizes OCaml source code with full support for:
  - Identifiers and keywords
  - Numbers (decimal, hex, octal, binary) with suffixes (l, L, n)
  - Character literals with escape sequences
  - String literals with Unicode support
  - Quoted string literals `{id|content|id}`
  - Comments (including nested comments)
  - Operators and special symbols
  - Line number directives (`# line "file"`)

### Parser (`lib/parser.ml`)
- Parses both OCaml type expressions and full OCaml expressions
- Supports two modes: type expressions only, or full expressions

#### Type Expression Parsing:

##### Primary Types:
- **Type variables**: `'a`, `'b`, `'var`
- **Wildcards**: `_`
- **Type constructors**: `int`, `string`, `list`
- **Parenthesized types**: `(int)`, `(int * string)`
- **Object types**: 
  - With methods: `< method1 : int; method2 : string >`
  - Empty objects: `< .. >`
  - With trailing semicolons: `< method : int; >`
  - With open types: `< method : int; .. >`
- **Class types**: `#class_name`

##### Type Applications:
- **Single parameter**: `'a list`
- **Multi-parameter**: `('a, 'b) result`
- **Class type application**: `'a #class`

##### Composite Types:
- **Tuple types**: `int * string * bool` (two or more types)
- **Arrow types**: `int -> string`
- **Labeled arrows**: `label:int -> string`
- **Optional labeled arrows**: `?label:int -> string`
- **Type aliases with 'as'**: `'a as 'x`

##### Polymorphic Types:
- **Universal quantification**: `'a 'b. 'a -> 'b`
- **Method types in objects**: `method_name : poly_typexpr`

##### Module-qualified Names:
- **Simple module paths**: `Module.type_name`
- **Extended module paths**: `Module(Path).type_name`
- **Nested module access**: `A.B.C.type_name`

##### Type Operator Precedence (highest to lowest):
1. Primary types (variables, constructors, parentheses, objects, classes)
2. Type applications (`'a list`)
3. Tuple types (`int * string`)
4. Type aliases with 'as' (`'a as 'x`)
5. Arrow types (`int -> string`, with labeled/optional variants)

#### Expression Parsing:

##### Basic Expressions:
- **Constants**: integers, floats, characters, strings, booleans, unit
- **Variables and paths**: `x`, `Module.value`
- **Parenthesized expressions**: `(expr)`
- **Begin-end blocks**: `begin expr end`
- **Type constraints**: `(expr : type)`
- **Type coercions**: `(expr :> type)`, `(expr : type1 :> type2)`

##### Data Structures:
- **Tuples**: `(x, y, z)`
- **Lists**: `[1; 2; 3]`, `[]`
- **Arrays**: `[|1; 2; 3|]`, `[||]`
- **Records**: `{ field1 = value1; field2 = value2 }`
- **Record updates**: `{ record with field = new_value }`
- **Variant constructors**: `Some x`, `None`
- **Polymorphic variants**: `` `Red``, `` `RGB (255, 0, 0)``

##### Function-related:
- **Function application**: `f x y`, `f ~label:x`, `f ?opt`
- **Lambda expressions**: `fun x -> x + 1`, `fun ~label:x -> x`
- **Function with pattern matching**: `function | [] -> 0 | _ -> 1`
- **Partial application**: `(+) 1`, `List.map f`

##### Control Flow:
- **Conditionals**: `if cond then expr1 else expr2`
- **Pattern matching**: `match expr with | pattern -> expr`
- **Loops**: `while cond do expr done`, `for i = 0 to 10 do expr done`
- **Sequences**: `expr1; expr2`
- **Exception handling**: `try expr with pattern -> handler`
- **Raising exceptions**: `raise Not_found`

##### Bindings:
- **Let bindings**: `let x = 42 in expr`
- **Recursive bindings**: `let rec f x = ... in expr`
- **Mutually recursive**: `let rec f x = ... and g y = ... in expr`
- **Local exceptions**: `let exception E in expr`
- **Local opens**: `let open Module in expr`, `Module.(expr)`

##### Operations:
- **Arithmetic**: `+`, `-`, `*`, `/`, `mod`
- **Floating-point**: `+.`, `-.`, `*.`, `/.`
- **Comparison**: `=`, `<>`, `<`, `>`, `<=`, `>=`
- **Boolean**: `&&`, `||`, `not`
- **String concatenation**: `^`
- **List cons**: `::`
- **Reference operations**: `!`, `:=`
- **Array/String access**: `arr.(i)`, `str.[i]`
- **Field access**: `record.field`
- **Assignment**: `arr.(i) <- value`, `record.field <- value`

##### Advanced Features:
- **Lazy evaluation**: `lazy expr`
- **Assertions**: `assert expr`
- **Local module bindings**: `let module M = ... in expr` (partially supported)
- **Object creation**: `new class_name` (not yet implemented)
- **Method calls**: `obj#method` (not yet implemented)

##### Expression Operator Precedence:
1. Prefix operators (`-`, `+`, `-.`, `+.`)
2. Field access, array/string indexing
3. Function application
4. Constructor application
5. Multiplication/division operators (`*`, `/`, `mod`, etc.)
6. Addition/subtraction operators (`+`, `-`, `^`, etc.)
7. Cons operator (`::`)
8. Comparison operators (`=`, `<`, etc.)
9. Boolean AND (`&&`)
10. Boolean OR (`||`)
11. Comma (tuple construction)
12. Assignment operators (`<-`, `:=`)
13. Conditional (`if-then-else`)
14. Semicolon (sequencing)

### AST with S-expression Support (`lib/parse_tree.ml`)
- Comprehensive type definitions for OCaml constructs:
  - Type expressions (`typexpr`)
  - Expressions (`expr`)
  - Patterns (`pattern`)
  - Constants, paths, and identifiers
- Full S-expression derivation using `ppx_sexp_conv`
- Support for pretty printing parse trees
- Path types for module-qualified names
- Rich expression AST supporting all core OCaml features

### REPL (`bin/main.ml`)
- Interactive command-line interface
- Supports two modes:
  - Type expression mode (default): Parse OCaml type expressions
  - Expression mode: Parse full OCaml expressions (use `:expr` to switch)
- Outputs parse trees as formatted S-expressions
- Commands:
  - `:quit` or `:q` to exit
  - `:type` to switch to type expression mode
  - `:expr` to switch to expression mode
  - Any OCaml expression or type expression to parse and display

## Building the Project

Build the entire project:
```bash
dune build
```

Build just the library:
```bash
dune build lib/
```

Build and run the REPL:
```bash
dune exec ./bin/main.exe
```

## Dependencies

- **Core libraries**: `base`, `stdio`, `sexplib`
- **Parsing**: `grace` (for diagnostics), `sedlex` (for lexing)
- **Preprocessors**: `ppx_sexp_conv`, `ppx_jane`

## Usage Examples

### Running the REPL
```bash
$ dune exec ./bin/main.exe
OCaml Expression Parser REPL
Enter OCaml type expressions to see their parse trees as S-expressions.
Use :quit or :q to exit.

# int
(TypeConstr (() int))

# int -> string
(Arrow () false (TypeConstr (() int)) (TypeConstr (() string)))

# 'a list
(TypeApp (TypeVar a) (() list))

# int * string * bool
(Tuple ((TypeConstr (() int)) (TypeConstr (() string)) (TypeConstr (() bool))))

# :q
Bye!
```

### Testing
Run the test suite:
```bash
dune runtest
```

## Current Status

**Completed Tasks:**
- Lexer implementation with comprehensive OCaml token support
- Parser for OCaml type expressions with proper precedence and full syntax coverage
- Full expression parser supporting most OCaml expression constructs
- Pattern matching support for match, function, try-with, and let bindings
- AST definitions with S-expression derivation for types, expressions, and patterns
- REPL interface for interactive parsing (needs mode switching implementation)
- Build system configuration with proper dependencies
- Comprehensive inline documentation of all supported syntax forms

**Known Issues:**
- REPL has a Grace source configuration issue that needs to be resolved
- Some edge cases in complex expressions may need refinement
- Empty array syntax `[||]` parsing needs investigation
- Object-oriented features (new, method calls) not yet implemented
- Local module bindings (`let module`) partially supported

## Development Notes

- The project uses `ppx_sexp_conv` to automatically generate S-expression converters
- Error handling uses the Grace diagnostic system for user-friendly error messages
- The lexer supports OCaml's full lexical syntax including recent additions
- Parser uses recursive descent with proper operator precedence

## Future Enhancements

- Complete object-oriented features (object expressions, method calls)
- Full module system parsing (module types, signatures, functors)
- Remaining expression features (extensible variants, attributes)
- Class definitions and class types
- Improved error recovery in the parser
- Better error messages with source locations
- Integration with OCaml's type system for semantic analysis

## Architecture and Design

- Expression parsing uses recursive descent with careful precedence handling
- Modular parser design with separate functions for each precedence level
- Extensive use of parser combinators for clean, maintainable code
- Architecture supports incremental parsing and S-expression conversion
- Clear separation between lexing, parsing, and AST representation
- Supports advanced language features like polymorphism, GADTs, and first-class modules
- Context-sensitive parsing for handling ambiguous constructs
- Error recovery mechanisms to continue parsing after syntax errors