open! Core

let counter = ref 0

module T = struct
  type t = int * string [@@deriving compare, hash, equal, sexp]
end

let to_string (i, s) = [%string {|%{s}/%{i#Int}|}]

let create () =
  let t = !counter, "id" in
  counter := !counter + 1;
  t
;;

let rename (id, _) name = id, name

module For_test = struct
  let reset () = counter := 0
end

include Comparable.Make (T)
include Hashable.Make (T)
include T
