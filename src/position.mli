open! Core

type t [@@deriving sexp_of, compare, equal]

val create : filename:string -> line_number:int -> column_number:int -> t
val filename : t -> string
val line_number : t -> int
val column_number : t -> int
