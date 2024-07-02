open! Core

type t [@@deriving compare, hash, equal, sexp]

val to_string : t -> string
val create : ?base:string -> unit -> t

include Comparable.S with type t := t
