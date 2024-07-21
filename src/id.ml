open! Core

let counter = ref 0

module T = struct
  type t = string * int [@@deriving compare, hash, equal, sexp]

  let to_string ((s, i) : t) = [%string {|%{s}/%{i#Int}|}]
  let compare (_, n1) (_, n2) = Int.compare n1 n2
end

let create () =
  let t = "id", !counter in
  counter := !counter + 1;
  t
;;

let rename (_, id) name = name, id

module For_test = struct
  let reset () = counter := 0
end

include Comparable.Make (T)
include Hashable.Make (T)
include T
