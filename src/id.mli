open! Core

type t [@@deriving compare, hash, equal, sexp]

(** [to_string id] returns the string representation of the id. *)
val to_string : t -> string

(** [create ()] creates a new id.*)
val create : unit -> t

(** [rename t name] renames the prefix of the id to [name]. *)
val rename : t -> string -> t

module For_test : sig
  (** [reset ()] resets the global counter for unit tests. *)
  val reset : unit -> unit
end

include Comparable.S with type t := t
include Hashable.S with type t := t
