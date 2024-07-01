open! Core

type t =
    | Int of int
    | Bool of bool
    | Float of float
    | Left_paren
    | Right_paren
    | Not
    | Plus
    | Minus
    | Asterisk
    | Slash
    | Plus_dot
    | Minus_dot
    | Asterisk_dot
    | Slash_dot
    | Equal
    | Less_greater
    | Less_equal
    | Greater_equal
    | Less
    | Greater
    | If
    | Then
    | Else
    | Let
    | In
    | Rec
    | Coma
    | Dot
    | Less_minus
    | Semicolon
    | Ident of string
    | Eof
[@@deriving sexp_of, compare, equal]
