open! Core
open Annotated

(* TODO kosinw: Modify parser state to cache token peeks + currents.
   Adding this will probably help with parser performance if bad.*)

module State = struct
  type t =
    { lexer : Lexer.t
    ; spans : Span.t Id.Map.t
    ; errors : (Error.t * Position.t) list
    }

  let create ~lexer = { lexer; spans = Id.Map.empty; errors = [] }
  let position t = Lexer.position t.lexer
  let advance t = { t with lexer = Lexer.advance t.lexer }
  let current t = Lexer.current t.lexer
  let errors t = t.errors
  let spans t = t.spans

  let lex t =
    let token, position, lexer = Lexer.lex t.lexer in
    token, position, { t with lexer }
  ;;

  (* TODO kosinw: Implement proper token-dependent parser recovery *)
  let rec recover t =
    let token, _, state = lex t in
    match token with
    | Token.Eof -> state
    | _ -> recover state
  ;;

  let fail ~error t = { (recover t) with errors = (error, position t) :: t.errors }

  let expect ~expected t =
    let token = current t in
    if Int.equal (Token.Variants.to_rank token) (Token.Variants.to_rank expected)
    then t
    else
      fail
        ~error:
          (Error.of_string
             [%string
               "Expected %{Token.describe expected}, instead got %{Token.describe token}"])
        t
  ;;

  let register_span ~id:key ~span:data t = { t with spans = Map.set t.spans ~key ~data }

  (** [annotate ~fst contents state] generates a new AST node given starting position
      [~fst] with the ast node contents being [contents]. *)
  let annotate ~fst contents t =
    let t = advance t in
    let snd = Lexer.position' t.lexer in
    let span = Span.create ~fst ~snd in
    let node = return contents in
    let t = register_span ~id:(id node) ~span t in
    node, t
  ;;
end

open State

type t = Syntax.t * State.t

let rec program state =
  let e, state = expr 0 state in
  let state = expect ~expected:Token.Eof state in
  e, state

and expr prec state =
  let left, state = prefix_expr prec state in
  left, state

and prefix_expr prec state =
  ignore prec;
  match current state with
  | Token.Int i -> int_expr i state
  | Token.Float f -> float_expr f state
  | Token.Char c -> char_expr c state
  | Token.String s -> string_expr s state
  | Token.True -> bool_expr "true" state
  | Token.False -> bool_expr "false" state
  | Token.Ident v -> var_expr v state
  | Token.Left_paren -> paren_expr state
  | _ -> failwith ""

and simple_expr e state = annotate ~fst:(position state) e state
and int_expr i state = simple_expr (Syntax.Int i) state
and float_expr f state = simple_expr (Syntax.Float f) state
and char_expr c state = simple_expr (Syntax.Char c) state
and string_expr s state = simple_expr (Syntax.String s) state
and bool_expr b state = simple_expr (Syntax.Bool b) state
and unit_expr state = simple_expr Syntax.Unit state
and var_expr v state = simple_expr (Syntax.Variable v) state

and paren_expr state =
  let state = state |> expect ~expected:Token.Left_paren |> advance in
  match current state with
  | Token.Right_paren -> unit_expr state
  | _ ->
    let inner, state = expr 0 state in
    let state = advance (expect ~expected:Token.Right_paren state) in
    inner, state
;;

let parse lexer =
  let state = State.create ~lexer in
  let syntax, state = program state in
  let syntax =
    match List.length (errors state) with
    | 0 -> syntax
    | _ -> return Syntax.Illegal
  in
  syntax, state
;;

let syntax (syntax, _) = syntax
let errors (_, state) = errors state
let spans (_, state) = spans state

let run_parser string =
  Id.For_test.reset ();
  let lexer = Lexer.from_string string in
  let parse_result = parse lexer in
  let syntax = syntax parse_result in
  let spans = spans parse_result in
  let errors = errors parse_result in
  print_s [%message (syntax : Syntax.t)];
  print_s [%message (spans : Span.t Id.Map.t)];
  print_s [%message (errors : (Error.t * Position.t) list)]
;;

let%expect_test "should parse integer expressions" =
  run_parser {| 0xdeadbeef |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Int 0xdeadbeef))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 11)))))))
    (errors ())
    |}]
;;

let%expect_test "should parse float expressions" =
  run_parser {| 3.14 |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Float 3.14))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 5)))))))
    (errors ())
    |}]
;;

let%expect_test "should parse char expressions" =
  run_parser {| '\n' |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Char "'\\n'"))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 5)))))))
    (errors ())
    |}]
;;

let%expect_test "should parse string expressions" =
  run_parser {| (* Here is a comment*)
"multiple lines with comments
"|};
  [%expect
    {|
    (syntax
     ((id (0 id)) (contents (String  "\"multiple lines with comments\
                                    \n\""))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 2) (column_number 1)))
        (snd ((filename -) (line_number 3) (column_number 1)))))))
    (errors ())
    |}]
;;

let%expect_test "should parse boolean expressions" =
  run_parser {| true |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Bool true))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 5)))))))
    (errors ())
    |}];
  run_parser {| false |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Bool false))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 6)))))))
    (errors ())
    |}]
;;

let%expect_test "should parse unit expressions" =
  run_parser {| () |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents Unit)))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 3)))
        (snd ((filename -) (line_number 1) (column_number 3)))))))
    (errors ())
    |}]
;;

let%expect_test "should parse simple variable expressions" =
  run_parser {| x |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Variable x))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 2)))))))
    (errors ())
    |}];
  run_parser {| long_function_name' |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Variable long_function_name'))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 20)))))))
    (errors ())
    |}]
;;

let%expect_test "should parse parenthesized expressions" =
  run_parser {| ( 15 ) |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Int 15))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 4)))
        (snd ((filename -) (line_number 1) (column_number 5)))))))
    (errors ())
    |}];
  run_parser {| (0o534.676) |};
  [%expect
    {|
    (syntax ((id (0 id)) (contents (Float 0o534.676))))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 3)))
        (snd ((filename -) (line_number 1) (column_number 11)))))))
    (errors ())
    |}]
;;

let%expect_test "[temporary]: should not parse multiple expressions" =
  run_parser {| hello () |};
  [%expect
    {|
    (syntax ((id (1 id)) (contents Illegal)))
    (spans
     (((0 id)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 6)))))))
    (errors
     (("Expected end of file, instead got ("
       ((filename -) (line_number 1) (column_number 8)))))
    |}]
;;
