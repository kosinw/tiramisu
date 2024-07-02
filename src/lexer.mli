open! Core

type t

val from_string : ?filename:string -> string -> t
val from_channel : ?filename:string -> In_channel.t -> t
val all : t -> Token.With_position.t Or_error.t list
val current : t -> Token.With_position.t Or_error.t
val advance : t -> t
