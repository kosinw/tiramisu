open! Core

type t [@@deriving sexp_of, compare, equal]

(** [create ~filename ~line_number ~column_number] represents position
    [line_number, column_number] in file [filename].*)
val create : filename:string -> line_number:int -> column_number:int -> t

(** [filename t] returns the file of the position. *)
val filename : t -> string

(** [line_number t] returns which line the position is on. *)
val line_number : t -> int

(** [column_number t] returns which column the position is on. *)
val column_number : t -> int

(** [pred t] returns the previous position to this one. *)
val pred : t -> t
