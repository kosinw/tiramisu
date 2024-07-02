(** Lexical analyzer for Tiramisu.

    This module implements the lexical analyzer (lexer) for the Tiramisu compiler.
    It transforms the input source code into a sequence of tokens that can be
    processed by the parser.

    Some examples of token types that the lexer recognizes:
    - Keywords (e.g., 'let', 'in', 'if', 'then', 'else')
    - Identifiers
    - Literals (integers, floats, strings)
    - Operators and punctuation *)
open! Core

type t

module Result : sig
  type t = Token.t Or_error.t * Position.t [@@deriving sexp_of, compare, equal]

  (** [token t] returns the token if this result is valid, otherwise returns [None]. *)
  val token : t -> Token.t option

  (** [position t] returns the position that this lexer result occured at. *)
  val position : t -> Position.t
end

(** [from_string ?filename contents] creates a new [t] given the source code as a string. *)
val from_string : ?filename:string -> string -> t

(** [from_channel ?filename channel] creates a new [t] given the input channel. *)
val from_channel : ?filename:string -> In_channel.t -> t

(** [all t] returns the entire list of tokens from the lexer. *)
val all : t -> Result.t list

(** [current t] returns the current token and the next state of the lexer. *)
val current : t -> Result.t * t

(** [peek t] returns the next token to be produced by the lexer. *)
val peek : t -> Result.t

(** [advance t] moves the lexer, forward by one token. *)
val advance : t -> t
