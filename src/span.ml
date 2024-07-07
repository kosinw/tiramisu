open! Core

type t =
  { fst : Position.t
  ; snd : Position.t
  }
[@@deriving sexp_of, compare, equal]

let create ~fst ~snd = { fst; snd = Position.pred snd }
let fst t = t.fst
let snd t = t.snd
