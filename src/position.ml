open! Core

type t =
  { filename : string
  ; line_number : int
  ; column_number : int
  }
[@@deriving sexp_of, compare, equal, fields ~getters ~iterators:create]

let create = Fields.create

let pred { filename; line_number; column_number } =
  { filename; line_number; column_number = Int.max (Int.pred column_number) 1 }
;;

let to_string { filename; line_number; column_number } =
  [%string "file %{filename}, line %{line_number#Int}, col %{column_number#Int}"]
;;
