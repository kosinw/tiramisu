open! Core

(** [>>] is the forward composition operator. *)
let ( >> ) f g x = x |> f |> g

type t =
  | Illegal of Error.t
  (* Items *)
  | Int of string
  | Float of string
  | String of string
  | Char of string
  | Ident of string
  (* Symbols *)
  | Quote
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
  | True
  | False
  (* End of file *)
  | Eof
[@@deriving sexp_of, compare, equal, hash]

let to_string = function
  | Illegal _ -> ""
  | Int s -> s
  | Float s -> s
  | Ident ident -> ident
  | Quote -> "'"
  | String s -> s
  | Char s -> s
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
  | True -> "true"
  | False -> "false"
  | Eof -> ""
;;

let describe = function
  | Illegal _ -> "illegal token"
  | Int _ -> "integer literal"
  | Float _ -> "float literal"
  | Ident _ -> "identifier"
  | Quote -> "'"
  | String _ -> "string literal"
  | Char _ -> "character literal"
  | Left_paren -> "("
  | Right_paren -> ")"
  | Plus -> "+"
  | Plus_dot -> "+."
  | Minus -> "-"
  | Minus_dot -> "-"
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
  | True -> "true"
  | False -> "false"
  | Eof -> "end of file"
;;

let length = to_string >> String.length
