open! Core

type t =
  (* Primitive types *)
  | Unit
  | Bool
  | Int
  | Float
  | String
  (* Function Types *)
  | Function of t * t
  (* Tuple Types *)
  | Tuple of t list
  (* Array types *)
  | Array of t
  (* Type variable *)
  | TypeVar of string
[@@deriving sexp, compare, equal, hash]

let rec to_string = function
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | String -> "string"
  | Function (l, r) -> [%string {|%{to_string l} -> %{to_string r}|}]
  | Tuple ts -> String.concat ~sep:" * " (List.map ts ~f:to_string)
  | Array t -> [%string {|%{to_string t} array|}]
  | TypeVar ident -> [%string {|'%{ident}|}]
;;
