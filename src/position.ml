open! Core

type t =
  { filename : string
  ; line_number : int
  ; column_number : int
  }
[@@deriving sexp_of, compare, equal, fields ~getters ~iterators:(create)]

let create = Fields.create