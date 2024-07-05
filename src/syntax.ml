open! Core

(** Untyped abstract syntax tree for Tiramisu.

    This module implements the untyped, abstract syntax tree IR for the Tiramisu compiler.
    It represents the abstract syntax of a Tiramisu program before desugaring.

    Some examples of abstract syntax expressions includes:
    - Let expressions (e.g. let x = M in N)
    - Constants (e.g. "hello", 3.14, false)
    - Variables references (e.g. x)
    - Tuple construction (e.g. (1, 2, 3) or (4, "pi", true))
    - Typed expressions (e.g. (5 : bool) or (x : string))

    Abstract syntax expressions can optionally have type annotations as
    well, which include:
    - Primitive types (e.g. bool, int)
    - Function types (e.g. int -> int -> bool)
    - Tuple types (e.g. bool * string * int)
    - Type variables (e.g. 'a)
    - Type functions (e.g. string list) *)

module Annotated = struct
  type 'a t =
    { id : Id.t
    ; t : 'a
    }
  [@@deriving sexp_of]
end

type expr =
  | Constant of constant Annotated.t

and constant =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
[@@deriving sexp_of]

type t = expr [@@deriving sexp_of]
