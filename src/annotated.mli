open! Core

type 'a t =
  { id : Id.t
  ; contents : 'a
  }
[@@deriving sexp]

(** [create contents] creates a new annotation. *)
val create : 'a -> 'a t

(** [id t] reads the annotation id. *)
val id : 'a t -> Id.t

(** [contents t] reads the value. *)
val contents : 'a t -> 'a

include Monad.S with type 'a t := 'a t
