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

open! Core

type expr =
  (* Constants *)
  | Unit of Id.t
  | Bool of Id.t * bool
  | Int of Id.t * int
  | Float of Id.t * float
  (* Unary expressions *)
  | UnaryOp of Id.t * unary_op * expr
  (* Binary expressions *)
  | BinaryOp of Id.t * binary_op * expr * expr
  (* Complex expressions *)
  | Typed of Id.t * expr * Type.t
  | Let of Id.t * string * Type.t * expr
  | Conditional of Id.t * expr * expr * expr

and unary_op =
  | Not
  | Neg
  | Neg_float

and binary_op =
  | Add
  | Sub
  | Add_float
  | Sub_float
  | Mul_float
  | Div_float
  | Eq
  | Le
[@@deriving sexp, compare, equal, hash]

type t = expr [@@deriving sexp, compare, equal, hash]
