open Base

type number =
  | Int of int
  | Int32 of int32
  | Int64 of int64
  | NativeInt of nativeint
  | Float of float

type token =
  | MinusDot
  | NotEq
  | Hash
  | AndAnd
  | Quote
  | Comma
  | Colon
  | ColonColon
  | ColonEq
  | ColonRightArrow
  | Semicolon
  | SemicolonSemicolon
  | Dot
  | DotDot
  | DotTilde
  | DotLParen
  | DotLBracket
  | Eq
  | Gt
  | Lt
  | LeftArrow
  | RightArrow
  | ColonGt
  | Label of string
  | OptLabel of string
  | InfixOp of string
  | String of string
  | QuotedString of string
  | Tilde
  | Question
  | LParen
  | RParen
  | LBracket
  | RBracket
  | LBrace
  | RBrace
  | Bar
  | Plus
  | Minus
  | Ident of string
  | Number of number
  | Char of char
  | PolymorphicVariantTag of string
  | And
  | As
  | Assert
  | Asr
  | Begin
  | Class
  | Constraint
  | Do
  | Done
  | Downto
  | Else
  | End
  | Exception
  | External
  | False
  | For
  | Fun
  | Function
  | Functor
  | If
  | In
  | Include
  | Inherit
  | Initializer
  | Land
  | Lazy
  | Let
  | Lor
  | Lsl
  | Lsr
  | Lxor
  | Match
  | Method
  | Mod
  | Module
  | Mutable
  | New
  | Nonrec
  | Object
  | Of
  | Open
  | Or
  | Private
  | Rec
  | Sig
  | Struct
  | Then
  | To
  | True
  | Try
  | Type
  | Val
  | Virtual
  | When
  | While
  | With

let keyword_of_string = function
  | "and" -> Some And
  | "as" -> Some As
  | "assert" -> Some Assert
  | "asr" -> Some Asr
  | "begin" -> Some Begin
  | "class" -> Some Class
  | "constraint" -> Some Constraint
  | "do" -> Some Do
  | "done" -> Some Done
  | "downto" -> Some Downto
  | "else" -> Some Else
  | "end" -> Some End
  | "exception" -> Some Exception
  | "external" -> Some External
  | "false" -> Some False
  | "for" -> Some For
  | "fun" -> Some Fun
  | "function" -> Some Function
  | "functor" -> Some Functor
  | "if" -> Some If
  | "in" -> Some In
  | "include" -> Some Include
  | "inherit" -> Some Inherit
  | "initializer" -> Some Initializer
  | "land" -> Some Land
  | "lazy" -> Some Lazy
  | "let" -> Some Let
  | "lor" -> Some Lor
  | "lsl" -> Some Lsl
  | "lsr" -> Some Lsr
  | "lxor" -> Some Lxor
  | "match" -> Some Match
  | "method" -> Some Method
  | "mod" -> Some Mod
  | "module" -> Some Module
  | "mutable" -> Some Mutable
  | "new" -> Some New
  | "nonrec" -> Some Nonrec
  | "object" -> Some Object
  | "of" -> Some Of
  | "open" -> Some Open
  | "or" -> Some Or
  | "private" -> Some Private
  | "rec" -> Some Rec
  | "sig" -> Some Sig
  | "struct" -> Some Struct
  | "then" -> Some Then
  | "to" -> Some To
  | "true" -> Some True
  | "try" -> Some Try
  | "type" -> Some Type
  | "val" -> Some Val
  | "virtual" -> Some Virtual
  | "when" -> Some When
  | "while" -> Some While
  | "with" -> Some With
  | _ -> None

let string_of_number = function
  | Int n -> Printf.sprintf "<int> %d" n
  | Int32 n -> Printf.sprintf "<int32> %ld" n
  | Int64 n -> Printf.sprintf "<int64> %Ld" n
  | NativeInt n -> Printf.sprintf "<nativeint> %s" (Nativeint.to_string n)
  | Float f -> Printf.sprintf "<float> %g" f

let string_of_token = function
  | MinusDot -> "-."
  | NotEq -> "!="
  | Hash -> "#"
  | AndAnd -> "&&"
  | Quote -> "'"
  | Comma -> ","
  | Colon -> ":"
  | Semicolon -> ";"
  | SemicolonSemicolon -> ";;"
  | ColonColon -> "::"
  | ColonEq -> ":="
  | ColonRightArrow -> "=>"
  | Dot -> "."
  | DotDot -> ".."
  | DotTilde -> ".~"
  | DotLParen -> ".("
  | DotLBracket -> ".["
  | Eq -> "="
  | Gt -> ">"
  | Lt -> "<"
  | LeftArrow -> "<-"
  | RightArrow -> "->"
  | ColonGt -> ":>"
  | Tilde -> "~"
  | Question -> "?"
  | Label content -> Printf.sprintf "<label> %s" content
  | OptLabel content -> Printf.sprintf "<optlabel> %s" content
  | InfixOp content -> Printf.sprintf "<infixop> %s" content
  | String content -> Printf.sprintf "<string> %s" content
  | QuotedString content -> Printf.sprintf "<quotedstring> %s" content
  | LParen -> "("
  | RParen -> ")"
  | LBracket -> "["
  | RBracket -> "]"
  | LBrace -> "{"
  | RBrace -> "}"
  | Bar -> "|"
  | Plus -> "+"
  | Minus -> "-"
  | Ident content -> Printf.sprintf "<ident> %s" content
  | Number number -> string_of_number number
  | Char c -> Printf.sprintf "<char> %c" c
  | PolymorphicVariantTag tag -> Printf.sprintf "<polymorphicvarianttag> `%s" tag
  | And -> "and"
  | As -> "as"
  | Assert -> "assert"
  | Asr -> "asr"
  | Begin -> "begin"
  | Class -> "class"
  | Constraint -> "constraint"
  | Do -> "do"
  | Done -> "done"
  | Downto -> "downto"
  | Else -> "else"
  | End -> "end"
  | Exception -> "exception"
  | External -> "external"
  | False -> "false"
  | For -> "for"
  | Fun -> "fun"
  | Function -> "function"
  | Functor -> "functor"
  | If -> "if"
  | In -> "in"
  | Include -> "include"
  | Inherit -> "inherit"
  | Initializer -> "initializer"
  | Land -> "land"
  | Lazy -> "lazy"
  | Let -> "let"
  | Lor -> "lor"
  | Lsl -> "lsl"
  | Lsr -> "lsr"
  | Lxor -> "lxor"
  | Match -> "match"
  | Method -> "method"
  | Mod -> "mod"
  | Module -> "module"
  | Mutable -> "mutable"
  | New -> "new"
  | Nonrec -> "nonrec"
  | Object -> "object"
  | Of -> "of"
  | Open -> "open"
  | Or -> "or"
  | Private -> "private"
  | Rec -> "rec"
  | Sig -> "sig"
  | Struct -> "struct"
  | Then -> "then"
  | To -> "to"
  | True -> "true"
  | Try -> "try"
  | Type -> "type"
  | Val -> "val"
  | Virtual -> "virtual"
  | When -> "when"
  | While -> "while"
  | With -> "with"
