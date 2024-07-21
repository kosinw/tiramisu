open! Core

module State = struct
  type t =
    { lexer : Lexer.t
    ; spans : Span.t Id.Map.t
    ; errors : (Error.t * Position.t) list
    }

  let create ~lexer = { lexer; spans = Id.Map.empty; errors = [] }
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

  let fail ~error t =
    { (recover t) with errors = (error, Lexer.position t.lexer) :: t.errors }
  ;;

  let expect ~expected t =
    let token = current t in
    if Token.equal token expected
    then advance t
    else
      fail
        ~error:
          (Error.of_string
             [%string
               "Expected %{Token.describe expected}, instead got %{Token.describe token}"])
        t
  ;;

  let annotate ~f t =
    let fst = Lexer.position t.lexer in
    let syntax, t = f () in
    let snd = Lexer.position' t.lexer in
    let span = Span.create ~fst ~snd in
    let syntax = Annotated.create syntax in
    syntax, { t with spans = Map.set t.spans ~key:(Annotated.id syntax) ~data:span }
  ;;

  let apply2 ~left ~right t =
    let left_id = Annotated.id left in
    let span = Map.find_exn t.spans left_id in
    let fst = Span.fst span in
    let snd = Lexer.position' t.lexer in
    let span = Span.create ~fst ~snd in
    let syntax =
      match right with
      | { Annotated.contents = Syntax.Apply (left', right'); id = _ } ->
        Syntax.Apply (left, left' :: right')
      | _ -> Syntax.Apply (left, [ right ])
    in
    let syntax = Annotated.create syntax in
    syntax, { t with spans = Map.set t.spans ~key:(Annotated.id syntax) ~data:span }
  ;;
end

type t = Syntax.t * State.t

module Prec = struct
  type t =
    | Lowest
    | Prefix_let
    | Infix_semicolon_right
    | Infix_semicolon_left
    | Prefix_if
    | Infix_comma_left
    | Infix_comma_right
    | Infix_compare_left
    | Infix_compare_right
    | Infix_add_left
    | Infix_add_right
    | Infix_mul_left
    | Infix_mul_right
    | Prefix_minus
    | Apply
    | Infix_dot_right
    | Infix_dot_left
  [@@deriving compare, equal]

  let prefix = function
    | Token.Let -> Some ((), Prefix_let)
    | Token.If -> Some ((), Prefix_if)
    | Token.Minus_dot | Token.Minus | Token.Not -> Some ((), Prefix_minus)
    | _ -> None
  ;;

  let infix = function
    | Token.Plus | Token.Minus | Token.Plus_dot | Token.Minus_dot ->
      Some (Infix_add_left, Infix_add_right)
    | Token.Asterisk | Token.Asterisk_dot | Token.Slash | Token.Slash_dot ->
      Some (Infix_mul_left, Infix_mul_right)
    | Token.Equal
    | Token.Less
    | Token.Less_greater
    | Token.Less_equal
    | Token.Less_minus
    | Token.Greater
    | Token.Greater_equal -> Some (Infix_compare_left, Infix_compare_right)
    | Token.Comma -> Some (Infix_comma_left, Infix_comma_right)
    | Token.Semicolon -> Some (Infix_semicolon_left, Infix_semicolon_right)
    | Token.Dot -> Some (Infix_dot_left, Infix_dot_right)
    | _ -> None
  ;;

  let prefix_exn x = x |> prefix |> Option.value_exn
  let is_infix x = x |> infix |> Option.is_some

  let is_argument = function
    | Token.Int _
    | Token.Float _
    | Token.String _
    | Token.Char _
    | Token.Ident _
    | Token.Left_paren -> true
    | _ -> false
  ;;
end

let fail ~error state = Annotated.create Syntax.Illegal, State.fail ~error state

let rec expr prec state =
  let left, state = prefix state in
  apply left prec state

and apply left prec state =
  match State.current state with
  | tok when Prec.is_infix tok -> infix left prec state
  | tok when Prec.is_argument tok ->
    let right, state = expr Prec.Apply state in
    let left, state = State.apply2 ~left ~right state in
    apply left prec state
  | _ -> left, state

and infix left prec state =
  ignore left;
  ignore prec;
  fail ~error:(Error.of_string "Infix operators not implemented yet") state

and prefix state =
  let current = State.current state in
  match current with
  | Token.Left_paren -> with_left_paren state
  | Token.Not -> with_prefix_expr ~f:(fun x -> Syntax.Not x) ~e:Token.Not state
  | Token.Minus -> with_prefix_expr ~f:(fun x -> Syntax.Neg x) ~e:Token.Minus state
  | Token.Minus_dot ->
    with_prefix_expr ~f:(fun x -> Syntax.Neg_float x) ~e:Token.Minus_dot state
  | Token.Int x -> with_atom (Syntax.Int x) state
  | Token.Float x -> with_atom (Syntax.Float x) state
  | Token.Char x -> with_atom (Syntax.Char x) state
  | Token.String x -> with_atom (Syntax.String x) state
  | Token.True -> with_atom (Syntax.Bool "true") state
  | Token.False -> with_atom (Syntax.Bool "false") state
  | Token.Ident x -> with_atom (Syntax.Variable x) state
  | Token.If -> with_conditional state
  | _ ->
    fail
      ~error:
        (Error.of_string
           [%string "Expected start of expression, instead saw %{Token.describe current}"])
      state

and with_left_paren state =
  State.annotate state ~f:(fun () ->
    let state = State.expect ~expected:Token.Left_paren state in
    match State.current state with
    | Token.Right_paren -> Syntax.Unit, State.advance state
    | _ ->
      let syntax, state = expr Prec.Lowest state in
      let state = State.expect ~expected:Token.Right_paren state in
      Annotated.contents syntax, state)

and with_conditional state =
  State.annotate state ~f:(fun () ->
    let state = State.expect ~expected:Token.If state in
    let (), right_bp = Prec.prefix_exn Token.If in
    let condition, state = expr right_bp state in
    let state = State.expect ~expected:Token.Then state in
    let consequent, state = expr right_bp state in
    let state = State.expect ~expected:Token.Else state in
    let alternative, state = expr right_bp state in
    Syntax.If (condition, consequent, alternative), state)

and with_prefix_expr ~f ~e:expected state =
  State.annotate state ~f:(fun () ->
    let state = State.expect ~expected state in
    let (), right_bp = Prec.prefix_exn expected in
    let syntax, state = expr right_bp state in
    f syntax, state)

and with_atom syntax state =
  State.annotate state ~f:(fun () -> syntax, State.advance state)
;;

let parse lexer =
  let syntax, state = expr Prec.Lowest (State.create ~lexer) in
  syntax, State.expect ~expected:Token.Eof state
;;

let result (syntax, state) =
  let errors = State.errors state in
  match List.is_empty errors with
  | true -> Ok syntax
  | false -> Error errors
;;

let spans (_, state) = State.spans state

let run_parser ?(verbose = false) string =
  Id.For_test.reset ();
  let lexer = Lexer.from_string string in
  let parse_result = parse lexer in
  (match result parse_result with
   | Ok syntax ->
     if verbose
     then print_s [%message "" ~_:(syntax : Syntax.t)]
     else print_s (Syntax.pp syntax)
   | Error errors -> print_s [%message "" (errors : (Error.t * Position.t) list)]);
  if verbose
  then (
    let spans = spans parse_result in
    print_s [%message "" (spans : Span.t Id.Map.t)])
;;

let%expect_test "should parse integer expressions" =
  run_parser {| 0xdeadbeef |};
  [%expect {| (int 0xdeadbeef) |}]
;;

let%expect_test "should parse float expressions" =
  run_parser {| 3.14 |};
  [%expect {| (float 3.14) |}]
;;

let%expect_test "should parse char expressions" =
  run_parser {| '\n' |};
  [%expect {| (char "'\\n'") |}]
;;

let%expect_test "[verbose]: should parse string expressions" =
  run_parser ~verbose:true {| (* Here is a comment*)
"multiple lines with comments
"|};
  [%expect
    {|
    ((id (id 0)) (contents (String  "\"multiple lines with comments\
                                   \n\"")))
    (spans
     (((id 0)
       ((fst ((filename -) (line_number 2) (column_number 1)))
        (snd ((filename -) (line_number 3) (column_number 1)))))))
    |}]
;;

let%expect_test "should parse boolean expressions" =
  run_parser {| true |};
  [%expect {| (bool true) |}];
  run_parser {| false |};
  [%expect {| (bool false) |}]
;;

let%expect_test "[verbose]: should parse unit expressions" =
  run_parser ~verbose:true {|(               )|};
  [%expect
    {|
    ((id (id 0)) (contents Unit))
    (spans
     (((id 0)
       ((fst ((filename -) (line_number 1) (column_number 1)))
        (snd ((filename -) (line_number 1) (column_number 17)))))))
    |}]
;;

let%expect_test "should parse simple variable expressions" =
  run_parser {| x |};
  [%expect {| (variable x) |}];
  run_parser {| long_function_name' |};
  [%expect {| (variable long_function_name') |}]
;;

let%expect_test "should parse parenthesized expressions" =
  run_parser ~verbose:true {| ( 15 ) |};
  [%expect
    {|
    ((id (id 1)) (contents (Int 15)))
    (spans
     (((id 0)
       ((fst ((filename -) (line_number 1) (column_number 4)))
        (snd ((filename -) (line_number 1) (column_number 5)))))
      ((id 1)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 7)))))))
    |}];
  run_parser ~verbose:true {| (0o534.676) |};
  [%expect
    {|
    ((id (id 1)) (contents (Float 0o534.676)))
    (spans
     (((id 0)
       ((fst ((filename -) (line_number 1) (column_number 3)))
        (snd ((filename -) (line_number 1) (column_number 11)))))
      ((id 1)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 12)))))))
    |}]
;;

let%expect_test "should parse logical not expressions" =
  run_parser {| not true |};
  [%expect {| (not (bool true)) |}];
  run_parser {| not (not true) |};
  [%expect {| (not (not (bool true))) |}]
;;

let%expect_test "should parse floating and integer negation" =
  run_parser {| ---5 |};
  [%expect {| (neg (neg (neg (int 5)))) |}];
  run_parser {|-.0x15.af|};
  [%expect {| (neg_float (float 0x15.af)) |}]
;;

let%expect_test "should parse basic if expression" =
  run_parser {| if true then 1 else 2 |};
  [%expect {| (if (bool true) (int 1) (int 2)) |}]
;;

let%expect_test "should parse more annoying if expressions" =
  run_parser {| if a then if b then c else d else e |};
  [%expect
    {| (if (variable a) (if (variable b) (variable c) (variable d)) (variable e)) |}];
  run_parser {| if a then b else if c then d else e |};
  [%expect
    {| (if (variable a) (variable b) (if (variable c) (variable d) (variable e))) |}];
  run_parser {| if if a then b else c then d else e |};
  [%expect
    {| (if (if (variable a) (variable b) (variable c)) (variable d) (variable e)) |}]
;;

let%expect_test "should parse application expressions" =
  run_parser {| map a b c d |};
  [%expect
    {| (apply (variable map) (variable a) (variable b) (variable c) (variable d)) |}];
  run_parser ~verbose:true {| (not a) b c d |};
  [%expect
    {|
    ((id (id 8))
     (contents
      (Apply ((id (id 2)) (contents (Not ((id (id 0)) (contents (Variable a))))))
       (((id (id 3)) (contents (Variable b)))
        ((id (id 4)) (contents (Variable c)))
        ((id (id 5)) (contents (Variable d)))))))
    (spans
     (((id 0)
       ((fst ((filename -) (line_number 1) (column_number 7)))
        (snd ((filename -) (line_number 1) (column_number 7)))))
      ((id 1)
       ((fst ((filename -) (line_number 1) (column_number 3)))
        (snd ((filename -) (line_number 1) (column_number 7)))))
      ((id 2)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 8)))))
      ((id 3)
       ((fst ((filename -) (line_number 1) (column_number 10)))
        (snd ((filename -) (line_number 1) (column_number 10)))))
      ((id 4)
       ((fst ((filename -) (line_number 1) (column_number 12)))
        (snd ((filename -) (line_number 1) (column_number 12)))))
      ((id 5)
       ((fst ((filename -) (line_number 1) (column_number 14)))
        (snd ((filename -) (line_number 1) (column_number 14)))))
      ((id 6)
       ((fst ((filename -) (line_number 1) (column_number 12)))
        (snd ((filename -) (line_number 1) (column_number 14)))))
      ((id 7)
       ((fst ((filename -) (line_number 1) (column_number 10)))
        (snd ((filename -) (line_number 1) (column_number 14)))))
      ((id 8)
       ((fst ((filename -) (line_number 1) (column_number 2)))
        (snd ((filename -) (line_number 1) (column_number 14)))))))
    |}]
;;

let%expect_test "should parse basic infix expressions" =
  run_parser {| a + b |};
  [%expect
    {|
    (errors
     (("Not implemented yet" ((filename -) (line_number 1) (column_number 4)))))
    |}]
;;
