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

(** [from_string ?filename contents] creates a new [t] given the source code as a string. *)
val from_string : ?filename:string -> string -> t

(** [from_channel ?filename channel] creates a new [t] given the input channel. *)
val from_channel : ?filename:string -> In_channel.t -> t

(** [all t] returns the entire list of tokens (with positions) from the lexer. *)
val all : t -> (Token.t * Position.t) list

(** [position t] returns the current position of the lexer, skips whitespace. *)
val position : t -> Position.t

(** [token t] returns the current token and the next state of the lexer. *)
val token : t -> Token.t * t

(** [peek t] returns the next token to be produced by the lexer. *)
val peek : t -> Token.t

(** [advance t] moves the lexer, forward by one token. *)
val advance : t -> t
