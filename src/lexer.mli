open! Core

type t

val from_string : ?filename:string -> string -> t
val from_file : filename:string -> t
val to_position : t -> Position.t
val token : t -> Token.t Or_error.t * t
val token_all : t -> Token.t list
val token_with_position : t -> Token.With_position.t Or_error.t * t
val token_with_position_all : t -> Token.With_position.t list
val current : t -> Token.With_position.t Or_error.t
val advance : t -> t
