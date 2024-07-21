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
  (* Atomic expressions *)
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
  | Apply of expr * expr list
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

let rec pp t =
  match Annotated.contents t with
  | Illegal -> Sexp.Atom "illegal"
  | Unit -> Sexp.Atom "()"
  | Int x -> Sexp.List [ Sexp.Atom "int"; Sexp.Atom x ]
  | Float x -> Sexp.List [ Sexp.Atom "float"; Sexp.Atom x ]
  | Char x -> Sexp.List [ Sexp.Atom "char"; Sexp.Atom x ]
  | Bool x -> Sexp.List [ Sexp.Atom "bool"; Sexp.Atom x ]
  | String x -> Sexp.List [ Sexp.Atom "string"; Sexp.Atom x ]
  | Variable x -> Sexp.List [ Sexp.Atom "variable"; Sexp.Atom x ]
  | Not x -> Sexp.List [ Sexp.Atom "not"; pp x ]
  | Neg x -> Sexp.List [ Sexp.Atom "neg"; pp x ]
  | Neg_float x -> Sexp.List [ Sexp.Atom "neg_float"; pp x ]
  | Add (x, y) -> Sexp.List [ Sexp.Atom "add"; pp x; pp y ]
  | Sub (x, y) -> Sexp.List [ Sexp.Atom "sub"; pp x; pp y ]
  | Add_float (x, y) -> Sexp.List [ Sexp.Atom "add_float"; pp x; pp y ]
  | Sub_float (x, y) -> Sexp.List [ Sexp.Atom "sub_float"; pp x; pp y ]
  | Div_float (x, y) -> Sexp.List [ Sexp.Atom "div_float"; pp x; pp y ]
  | Mul_float (x, y) -> Sexp.List [ Sexp.Atom "mul_float"; pp x; pp y ]
  | Equals (x, y) -> Sexp.List [ Sexp.Atom "equals"; pp x; pp y ]
  | Less_than (x, y) -> Sexp.List [ Sexp.Atom "less_than"; pp x; pp y ]
  | Sequence (x, y) -> Sexp.List [ Sexp.Atom "sequence"; pp x; pp y ]
  | If (a, b, c) -> Sexp.List [ Sexp.Atom "if"; pp a; pp b; pp c ]
  | Array_set (a, b, c) -> Sexp.List [ Sexp.Atom "array_set"; pp a; pp b; pp c ]
  | Let (a, b, c) -> Sexp.List [ Sexp.Atom "let"; pp_pattern a; pp b; pp c ]
  | Let_rec (a, bs, c, d) ->
    Sexp.List
      ([ Sexp.Atom "let_rec"; pp_pattern a ] @ List.map ~f:pp_pattern bs @ [ pp c; pp d ])
  | Constraint (a, b) -> Sexp.List [ Sexp.Atom "constraint"; pp a; pp_typexpr b ]
  | Apply (a, bs) -> Sexp.List ([ Sexp.Atom "apply"; pp a ] @ List.map ~f:pp bs)
  | Tuple xs -> Sexp.List ([ Sexp.Atom "tuple" ] @ List.map ~f:pp xs)
  | Array_get (a, b) -> Sexp.List [ Sexp.Atom "array_get"; pp a; pp b ]

and pp_pattern t =
  match Annotated.contents t with
  | Illegal_pattern -> Sexp.Atom "illegal_pattern"
  | Var_pattern x -> Sexp.List [ Sexp.Atom "var_pattern"; Sexp.Atom x ]
  | Tuple_pattern xs ->
    Sexp.List ([ Sexp.Atom "tuple_pattern" ] @ List.map xs ~f:pp_pattern)
  | Constraint_pattern (x, y) ->
    Sexp.List [ Sexp.Atom "constraint_pattern"; pp_pattern x; pp_typexpr y ]

and pp_typexpr t =
  match Annotated.contents t with
  | Illegal_typexpr -> Sexp.Atom "illegal_typexpr"
  | Var_typexpr x -> Sexp.List [ Sexp.Atom "var_typexpr"; Sexp.Atom x ]
  | Arrow_typexpr (x, y) ->
    Sexp.List [ Sexp.Atom "arrow_typexpr"; pp_typexpr x; pp_typexpr y ]
  | Tuple_typexpr xs ->
    Sexp.List ([ Sexp.Atom "tuple_typexpr" ] @ List.map xs ~f:pp_typexpr)
  | Constr_typexpr xs ->
    Sexp.List ([ Sexp.Atom "constr_typexpr" ] @ List.map xs ~f:pp_typexpr)
;;
