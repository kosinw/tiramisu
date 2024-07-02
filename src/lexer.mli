open! Core

type t

module Result : sig
  type t = Token.t Or_error.t * Position.t [@@deriving sexp_of, compare, equal]
end

val from_string : ?filename:string -> string -> t
val from_channel : ?filename:string -> In_channel.t -> t
val all : t -> Result.t list
val current : t -> Result.t
val advance : t -> t
