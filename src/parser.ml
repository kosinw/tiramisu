open! Core

(* TODO kosinw: Modify parser state to cache token peeks + currents.
   Adding this will probably help with parser performance if bad.*)

module State = struct
  type t =
    { lexer : Lexer.t
    ; locations : (Position.t * Position.t) Id.Map.t
    ; errors : (Error.t * Position.t) list
    }

  let create ~lexer = { lexer; locations = Id.Map.empty; errors = [] }
  let position t = Lexer.position t.lexer
  let advance t = { t with lexer = Lexer.advance t.lexer }
  let current t = Lexer.current t.lexer
  let peek t = Lexer.current (Lexer.advance t.lexer)

  let locate t ~id:key ~location:data =
    { t with locations = Map.add_exn t.locations ~key ~data }
  ;;

  let error t ~error = { t with errors = (error, position t) :: t.errors }
  let errors t = t.errors
  let locations t = t.locations

  (* TODO kosinw: Implement parser recovery *)
  let recover _t = failwith ""

  let expect ~expected t =
    let token = current t in
    if Int.equal (Token.Variants.to_rank token) (Token.Variants.to_rank expected)
    then advance t
    else (
      let error =
        Error.of_string
          [%string
            "Expected %{Token.describe expected}, instead got %{Token.describe token}"]
      in
      { (recover t) with errors = (error, position t) :: t.errors })
  ;;
end

type t =
  { syntax : Syntax.t
  ; state : State.t
  }

open Syntax
open State

(* TODO kosinw: Implement tracking of locations *)
let rec program state =
  let e, state = expr state in
  match current state with
  | Token.Eof -> e, state
  | _ -> failwith ""

and expr state =
  let left, state = prefix_expr state in
  left, state

and prefix_expr state =
  match current state with
  | Token.Int i -> int_expr i state
  | Token.Float f -> float_expr f state
  | Token.Char c -> char_expr c state
  | Token.String s -> string_expr s state
  | Token.True -> bool_expr "true" state
  | Token.False -> bool_expr "false" state
  | Token.Left_paren -> paren_expr state
  | _ -> failwith ""

(* TODO kosinw: Refactor this simple logic into helper function *)
and int_expr i state = Annotated.return (Syntax.Int i), advance state
and float_expr f state = Annotated.return (Syntax.Float f), advance state
and char_expr c state = Annotated.return (Syntax.Char c), advance state
and string_expr s state = Annotated.return (Syntax.String s), advance state
and bool_expr b state = Annotated.return (Syntax.Bool b), advance state
and unit_expr state = Annotated.return Syntax.Unit, advance state

and paren_expr state =
  let state = expect ~expected:Token.Left_paren state in
  match current state with
  | Token.Right_paren -> unit_expr state
  | _ ->
    let expr', state = expr state in
    let state = expect ~expected:Token.Right_paren state in
    expr', state
;;

let parse lexer =
  let state = State.create ~lexer in
  let syntax, state = program state in
  { syntax; state }
;;

let syntax t = t.syntax
let errors t = errors t.state
let locations t = locations t.state

let run_parser string =
  Id.For_test.reset ();
  let parse_result = string |> Lexer.from_string |> parse in
  let syntax = syntax parse_result in
  let errors = errors parse_result in
  let locations = locations parse_result in
  print_s [%message (syntax : Syntax.t)];
  print_s [%message (errors : (Error.t * Position.t) list)];
  print_s [%message (locations : (Position.t * Position.t) Id.Map.t)]
;;

let%expect_test "should parse simple expressions" =
  print_endline "------";
  run_parser {| 15 |};
  print_endline "------";
  run_parser {| 3.14 |};
  print_endline "------";
  run_parser {| true |};
  print_endline "------";
  run_parser {| "hello" |};
  print_endline "------";
  run_parser {| '\n' |};
  print_endline "------";
  run_parser {| () |};
  print_endline "------";
  run_parser {| ( true ) |};
  print_endline "------";
  [%expect
    {|
    ------
    (syntax ((id (0 id)) (value (Int 15))))
    (errors ())
    (locations ())
    ------
    (syntax ((id (0 id)) (value (Float 3.14))))
    (errors ())
    (locations ())
    ------
    (syntax ((id (0 id)) (value (Bool true))))
    (errors ())
    (locations ())
    ------
    (syntax ((id (0 id)) (value (String "\"hello\""))))
    (errors ())
    (locations ())
    ------
    (syntax ((id (0 id)) (value (Char "'\\n'"))))
    (errors ())
    (locations ())
    ------
    (syntax ((id (0 id)) (value Unit)))
    (errors ())
    (locations ())
    ------
    (syntax ((id (0 id)) (value (Bool true))))
    (errors ())
    (locations ())
    ------
    |}]
;;
