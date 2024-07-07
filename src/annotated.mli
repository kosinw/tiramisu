open! Core

type 'a t [@@deriving sexp]

(** [id t] reads the annotation id. *)
val id : 'a t -> Id.t

(** [contents t] reads the value. *)
val contents : 'a t -> 'a

include Monad.S with type 'a t := 'a t
