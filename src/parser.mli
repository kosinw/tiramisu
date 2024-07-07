open! Core

type t

(** [parse t] takes in a lexer and returns the parse result which can either be
    an abstract syntax tree or a list of errors encountered while parsing. *)
val parse : Lexer.t -> t

(** [syntax t] returns the resulting syntax tree form the parse. If any errors occured,
    while parsing the syntax tree will be [Syntax.Illegal].*)
val syntax : t -> Syntax.t

(** [errors t] returns the list of errors encountered during the parse. *)
val errors : t -> (Error.t * Position.t) list

(** [spans t] returns the spans of each syntax tree element. *)
val spans : t -> Span.t Id.Map.t
