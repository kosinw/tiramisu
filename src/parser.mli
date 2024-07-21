open! Core

type t

(** [parse t] takes in a lexer and returns the parse result which can either be
    an abstract syntax tree or a list of errors encountered while parsing. *)
val parse : Lexer.t -> t

(** [result t] returns the parsing result which is either a syntax tree or a list of errors. *)
val result : t -> (Syntax.t, (Error.t * Position.t) list) Result.t

(** [spans t] returns the captured spans of syntax tree elements. *)
val spans : t -> Span.t Id.Map.t
