open! Core

module T = struct
  type 'a t =
    { id : Id.t
    ; contents : 'a
    }
  [@@deriving sexp, fields ~getters]

  let return contents = { id = Id.create (); contents }
  let bind t ~f = { (f t.contents) with id = t.id }
  let map = `Custom (fun a ~f -> { a with contents = f a.contents })
  let create = return
end

include T
include Monad.Make (T)
