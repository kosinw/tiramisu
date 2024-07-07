open! Core

(** Represents an interval of characters in a source file. *)
type t [@@deriving sexp_of, compare, equal]

(** [create ~fst ~snd token] creates a new span starting from [fst] and ending
    with [snd]. *)
val create : fst:Position.t -> snd:Position.t -> t

(** [fst t] returns the starting position of the span. *)
val fst : t -> Position.t

(** [snd t] returns the ending position (inclusive) of the span. *)
val snd : t -> Position.t
