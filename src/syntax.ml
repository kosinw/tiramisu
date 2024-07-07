open! Core

(** Untyped abstract syntax tree for Tiramisu.

    Some examples of abstract syntax expressions includes:
    - Let expressions (e.g. let x = M in N)
    - Constants (e.g. "hello", 3.14, false)
    - Variables references (e.g. x)
    - Tuple construction (e.g. (1, 2, 3) or (4, "pi", true))
    - Constraint expressions (e.g. (5 : bool) or (x : string)) *)

type expr = expr' Annotated.t

and expr' =
  | Illegal
  (* Simple expressions *)
  | Unit
  | Int of string
  | Float of string
  | Char of string
  | Bool of string
  | String of string
  | Variable of string
  (* Prefix expressions *)
  | Not of expr
  | Neg of expr
  | Neg_float of expr
  (* Infix expressions *)
  | Add of expr * expr
  | Sub of expr * expr
  | Add_float of expr * expr
  | Sub_float of expr * expr
  | Div_float of expr * expr
  | Mul_float of expr * expr
  | Equals of expr * expr
  | Less_than of expr * expr
  | Sequence of expr * expr
  (* Mixfix expressions *)
  | If of expr * expr * expr
  | Array_set of expr * expr * expr
  | Let of pattern * expr * expr
  | Let_rec of pattern * pattern list * expr * expr
  (* Complex expressions *)
  | Constraint of expr * typexpr
  | Apply of expr list
  | Tuple of expr list
  | Array_get of expr * expr

and pattern = pattern' Annotated.t

and pattern' =
  | Illegal_pattern
  | Var_pattern of string
  | Tuple_pattern of pattern list
  | Constraint_pattern of pattern * typexpr

and typexpr = typexpr' Annotated.t

and typexpr' =
  | Illegal_typexpr
  | Var_typexpr of string
  | Arrow_typexpr of typexpr * typexpr
  | Tuple_typexpr of typexpr list
  | Constr_typexpr of typexpr list
[@@deriving sexp]

type t = expr [@@deriving sexp]
