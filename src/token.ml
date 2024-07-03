open! Core

type t =
  | Illegal of Error.t
  (* Items *)
  | Int of string
  | Bool of bool
  | Float of string
  | String of string
  | Ident of string
  (* Symbols *)
  | Left_paren
  | Right_paren
  | Plus
  | Plus_dot
  | Minus
  | Minus_dot
  | Minus_greater
  | Asterisk
  | Asterisk_dot
  | Slash
  | Slash_dot
  | Equal
  | Less
  | Less_greater
  | Less_equal
  | Less_minus
  | Greater
  | Greater_equal
  | Comma
  | Dot
  | Quote
  | Colon
  | Semicolon
  (* Keywords *)
  | Not
  | If
  | Then
  | Else
  | Let
  | In
  | Rec
  | Fun
  (* End of file *)
  | Eof
[@@deriving sexp_of, compare, equal, hash]

let to_string = function
  | Illegal _ -> ""
  | Int s -> s
  | Bool bool -> Bool.to_string bool
  | Float s -> s
  | Ident ident -> ident
  | String string -> [%string {|"%{string}"|}]
  | Left_paren -> "("
  | Right_paren -> ")"
  | Plus -> "+"
  | Plus_dot -> "+."
  | Minus -> "-"
  | Minus_dot -> "-."
  | Minus_greater -> "->"
  | Asterisk -> "*"
  | Asterisk_dot -> "*."
  | Slash -> "/"
  | Slash_dot -> "/."
  | Equal -> "="
  | Less -> "<"
  | Less_greater -> "<>"
  | Less_equal -> "<="
  | Less_minus -> "<-"
  | Greater -> ">"
  | Greater_equal -> ">="
  | Comma -> ","
  | Dot -> "."
  | Quote -> "'"
  | Colon -> ":"
  | Semicolon -> ";"
  | Not -> "not"
  | If -> "if"
  | Then -> "then"
  | Else -> "else"
  | Let -> "let"
  | In -> "in"
  | Rec -> "rec"
  | Fun -> "fun"
  | Eof -> ""
;;

let length = Fn.compose String.length to_string