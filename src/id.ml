open! Core

let global_counter = ref 0

module T = struct
  type t = int * string [@@deriving compare, hash, equal, sexp]
end

let to_string (i, s) = [%string {|%{s}/%{i#Int}|}]

let create ?base () =
  let t = !global_counter, Option.value ~default:"id" base in
  global_counter := !global_counter + 1;
  t
;;

include Comparable.Make (T)
include T
