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

  let rec recover t =
    match current t with
    | Token.Eof -> t
    | _ -> recover (advance t)
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

  let prefix ~f t =
    let fst = Lexer.position t.lexer in
    let syntax, t = f () in
    let snd = Lexer.position' t.lexer in
    let span = Span.create ~fst ~snd in
    let syntax = Annotated.create syntax in
    syntax, { t with spans = Map.set t.spans ~key:(Annotated.id syntax) ~data:span }
  ;;

  let infix ~left ~f t =
    let fst = left |> Annotated.id |> Map.find_exn t.spans |> Span.fst in
    let syntax, t = f () in
    let snd = Lexer.position' t.lexer in
    let span = Span.create ~fst ~snd in
    let syntax = Annotated.create syntax in
    syntax, { t with spans = Map.set t.spans ~key:(Annotated.id syntax) ~data:span }
  ;;
end

module Precedence = struct
  type t =
    | Lowest
    | Prefix_let
    | Infix_semicolon_right
    | Infix_semicolon_left
    | Prefix_if
    | Infix_arrow_right
    | Infix_arrow_left
    | Infix_comma_left
    | Infix_comma_right
    | Infix_or_right
    | Infix_or_left
    | Infix_and_right
    | Infix_and_left
    | Infix_compare_left
    | Infix_compare_right
    | Infix_add_left
    | Infix_add_right
    | Infix_mul_left
    | Infix_mul_right
    | Prefix_minus
    | Apply_left
    | Apply_right
    | Infix_dot_left
    | Infix_dot_right
    | Highest
  [@@deriving compare, equal]

  let prefix = function
    | Token.Fun | Token.Let -> Some ((), Prefix_let)
    | Token.If -> Some ((), Prefix_if)
    | Token.Minus_dot | Token.Minus | Token.Not -> Some ((), Prefix_minus)
    | _ -> None
  ;;

  let infix = function
    | Token.Less_minus -> Some (Infix_arrow_left, Infix_arrow_right)
    | Token.Plus | Token.Minus | Token.Plus_dot | Token.Minus_dot ->
      Some (Infix_add_left, Infix_add_right)
    | Token.Asterisk | Token.Asterisk_dot | Token.Slash | Token.Slash_dot ->
      Some (Infix_mul_left, Infix_mul_right)
    | Token.Double_pipe -> Some (Infix_or_left, Infix_or_right)
    | Token.Double_ampersand -> Some (Infix_and_left, Infix_and_right)
    | Token.Equal
    | Token.Less
    | Token.Less_greater
    | Token.Less_equal
    | Token.Greater
    | Token.Greater_equal -> Some (Infix_compare_left, Infix_compare_right)
    | Token.Comma -> Some (Infix_comma_left, Infix_comma_right)
    | Token.Semicolon -> Some (Infix_semicolon_left, Infix_semicolon_right)
    | Token.Dot_paren | Token.Dot -> Some (Infix_dot_left, Infix_dot_right)
    | Token.Int _
    | Token.True
    | Token.False
    | Token.Float _
    | Token.String _
    | Token.Char _
    | Token.Ident _
    | Token.Left_paren
    | Token.Not -> Some (Apply_left, Apply_right)
    | _ -> None
  ;;

  let prefix_exn x = Option.value_exn (prefix x)
end

type t = Syntax.t * State.t

let rec expr prec state =
  let left, state = prefix state in
  infix left prec state

and infix left prec state =
  let op = State.current state in
  let error_state =
    State.fail
      ~error:
        (Error.of_string [%string "Did not expect infix operator %{Token.describe op}"])
      state
  in
  match Precedence.infix op with
  | Some (Precedence.Apply_left, Precedence.Apply_right) ->
    if Precedence.compare Precedence.Apply_left prec <= 0
    then left, state
    else (
      let left, state =
        State.infix state ~left ~f:(fun () ->
          let right, state = expr Precedence.Apply_right state in
          Syntax.Apply (left, right), state)
      in
      infix left prec state)
  | Some (left_binding_power, right_binding_power) ->
    if Precedence.compare left_binding_power prec <= 0
    then left, state
    else (
      let state = State.advance state in
      let left, state =
        State.infix state ~left ~f:(fun () ->
          let right, state = expr right_binding_power state in
          match op with
          | Token.Plus -> Syntax.Add (left, right), state
          | Token.Plus_dot -> Syntax.Add_float (left, right), state
          | Token.Asterisk -> Syntax.Mul (left, right), state
          | Token.Asterisk_dot -> Syntax.Mul_float (left, right), state
          | Token.Minus -> Syntax.Sub (left, right), state
          | Token.Minus_dot -> Syntax.Sub_float (left, right), state
          | Token.Slash -> Syntax.Div (left, right), state
          | Token.Slash_dot -> Syntax.Div_float (left, right), state
          | Token.Double_ampersand -> Syntax.And (left, right), state
          | Token.Double_pipe -> Syntax.Or (left, right), state
          | Token.Equal -> Syntax.Equals (left, right), state
          | Token.Less -> Syntax.Less_than (left, right), state
          | Token.Greater -> Syntax.Greater_than (left, right), state
          | Token.Less_equal -> Syntax.Less_equals (left, right), state
          | Token.Greater_equal -> Syntax.Greater_equals (left, right), state
          | Token.Less_greater -> Syntax.Not_equals (left, right), state
          | Token.Semicolon -> Syntax.Sequence (left, right), state
          | Token.Comma -> Syntax.Tuple (left, right), state
          | Token.Dot -> dot_expr left right state
          | Token.Dot_paren -> array_expr left right state
          | _ -> Syntax.Illegal, error_state)
      in
      infix left prec state)
  | None -> left, state

and array_expr left right state =
  let state = State.expect state ~expected:Token.Right_paren in
  match State.current state with
  | Token.Less_minus ->
    let state = State.advance state in
    let after, state = expr Precedence.Infix_arrow_right state in
    Syntax.Array_set (left, right, after), state
  | _ -> Syntax.Array_get (left, right), state

and dot_expr left right state =
  match Annotated.contents left, Annotated.contents right with
  | Syntax.Variable x, Syntax.Variable y -> Syntax.Variable (x ^ "." ^ y), state
  | _ ->
    ( Syntax.Illegal
    , State.fail
        ~error:
          (Error.of_string "Dot infix operator can only be used between two variables")
        state )

and prefix state =
  let current = State.current state in
  match current with
  | Token.Left_paren -> paren_expr state
  | Token.Not -> with_prefix ~f:(fun x -> Syntax.Not x) ~e:Token.Not state
  | Token.Minus -> with_prefix ~f:(fun x -> Syntax.Neg x) ~e:Token.Minus state
  | Token.Minus_dot ->
    with_prefix ~f:(fun x -> Syntax.Neg_float x) ~e:Token.Minus_dot state
  | Token.Int x -> atom (Syntax.Int x) state
  | Token.Float x -> atom (Syntax.Float x) state
  | Token.Char x -> atom (Syntax.Char x) state
  | Token.String x -> atom (Syntax.String x) state
  | Token.True -> atom (Syntax.Bool "true") state
  | Token.False -> atom (Syntax.Bool "false") state
  | Token.Ident x -> atom (Syntax.Variable x) state
  | Token.If -> if_expr state
  | Token.Let -> let_expr state
  | Token.Fun -> fun_expr state
  | _ ->
    State.prefix state ~f:(fun () ->
      ( Syntax.Illegal
      , State.fail
          ~error:
            (Error.of_string
               [%string
                 "Expected start of expression, instead saw %{Token.describe current}"])
          state ))

and paren_expr state =
  State.prefix state ~f:(fun () ->
    let state = State.expect ~expected:Token.Left_paren state in
    match State.current state with
    | Token.Right_paren -> Syntax.Unit, State.advance state
    | _ ->
      let syntax, state = expr Precedence.Lowest state in
      let state = State.expect ~expected:Token.Right_paren state in
      Annotated.contents syntax, state)

and if_expr state =
  State.prefix state ~f:(fun () ->
    let state = State.expect ~expected:Token.If state in
    let (), right_binding_power = Precedence.prefix_exn Token.If in
    let condition, state = expr right_binding_power state in
    let state = State.expect ~expected:Token.Then state in
    let consequent, state = expr right_binding_power state in
    let state = State.expect ~expected:Token.Else state in
    let alternative, state = expr right_binding_power state in
    Syntax.If (condition, consequent, alternative), state)

and let_expr state =
  State.prefix state ~f:(fun () ->
    let state = State.expect ~expected:Token.Let state in
    let (), right_binding_power = Precedence.prefix_exn Token.Let in
    match State.current state with
    | Token.Rec ->
      let state = State.advance state in
      let binding, state = pattern Precedence.Highest state in
      let arguments, state = pattern_list Precedence.Highest state in
      let state = State.expect ~expected:Token.Equal state in
      let expression, state =
        State.prefix state ~f:(fun () ->
          let expression, state = expr right_binding_power state in
          Syntax.Fix (binding, arguments, expression), state)
      in
      let state = State.expect ~expected:Token.In state in
      let body, state = expr right_binding_power state in
      Syntax.Let (binding, expression, body), state
    | _ ->
      let binding, state = pattern Precedence.Highest state in
      let arguments, state = pattern_list Precedence.Highest state in
      let state = State.expect ~expected:Token.Equal state in
      (match arguments with
       | [] ->
         let expression, state = expr right_binding_power state in
         let state = State.expect ~expected:Token.In state in
         let body, state = expr right_binding_power state in
         Syntax.Let (binding, expression, body), state
       | _ ->
         let expression, state =
           State.prefix state ~f:(fun () ->
             let expression, state = expr right_binding_power state in
             Syntax.Fun (arguments, expression), state)
         in
         let state = State.expect ~expected:Token.In state in
         let body, state = expr right_binding_power state in
         Syntax.Let (binding, expression, body), state))

and fun_expr state =
  State.prefix state ~f:(fun () ->
    let state = State.expect ~expected:Token.Fun state in
    let (), right_binding_power = Precedence.prefix_exn Token.Fun in
    let arguments, state = pattern_list Precedence.Highest state in
    match arguments with
    | [] ->
      ( Syntax.Illegal
      , State.fail
          state
          ~error:
            (Error.of_string
               [%string "Anonymous functions require at least one formal paramter"]) )
    | _ ->
      let state = State.expect ~expected:Token.Minus_greater state in
      let expression, state = expr right_binding_power state in
      Syntax.Fun (arguments, expression), state)

and with_prefix ~f ~e:expected state =
  State.prefix state ~f:(fun () ->
    let state = State.expect ~expected state in
    let (), right_binding_power = Precedence.prefix_exn expected in
    let syntax, state = expr right_binding_power state in
    f syntax, state)

and atom : 'a. 'a -> State.t -> 'a Annotated.t * State.t =
  fun syntax state -> State.prefix state ~f:(fun () -> syntax, State.advance state)

and pattern prec state =
  let left, state = prefix_pattern state in
  infix_pattern left prec state

and pattern_list prec state =
  let rec pattern_list' left prec state =
    match State.current state with
    | Token.Ident _ | Token.Left_paren ->
      let right, state = pattern prec state in
      pattern_list' (right :: left) prec state
    | _ -> List.rev left, state
  in
  pattern_list' [] prec state

and infix_pattern left prec state =
  let op = State.current state in
  match Precedence.infix op with
  | Some (left_binding_power, right_binding_power) ->
    if Precedence.compare left_binding_power prec <= 0
    then left, state
    else (
      let state = State.advance state in
      let left, state =
        State.infix state ~left ~f:(fun () ->
          let right, state = pattern right_binding_power state in
          match op with
          | Token.Comma -> Syntax.Tuple_pattern (left, right), state
          | _ ->
            ( Syntax.Illegal_pattern
            , State.fail
                ~error:
                  (Error.of_string
                     [%string "Unknown infix operator %{Token.describe op}"])
                state ))
      in
      infix_pattern left prec state)
  | None -> left, state

and prefix_pattern state =
  let current = State.current state in
  match current with
  | Token.Left_paren -> paren_expr_pattern state
  | Token.Ident x -> atom (Syntax.Var_pattern x) state
  | _ ->
    State.prefix state ~f:(fun () ->
      ( Syntax.Illegal_pattern
      , State.fail
          ~error:
            (Error.of_string
               [%string
                 "Expected start of pattern, instead saw %{Token.describe current}"])
          state ))

and paren_expr_pattern state =
  State.prefix state ~f:(fun () ->
    let state = State.expect ~expected:Token.Left_paren state in
    match State.current state with
    | Token.Right_paren -> Syntax.Unit_pattern, State.advance state
    | _ ->
      let syntax, state = pattern Precedence.Lowest state in
      let state = State.expect ~expected:Token.Right_paren state in
      Annotated.contents syntax, state)
;;

let parse lexer =
  let syntax, state = expr Precedence.Lowest (State.create ~lexer) in
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
   | Error errors ->
     List.iter errors ~f:(fun (error, position) ->
       print_s [%message (Position.to_string position) ~_:(error : Error.t)]));
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

let%expect_test "should parse string expressions" =
  run_parser {| (* Here is a comment*) "a string literal" |};
  [%expect {| (string "\"a string literal\"") |}]
;;

let%expect_test "should parse boolean expressions" =
  run_parser {| true |};
  [%expect {| (bool true) |}];
  run_parser {| false |};
  [%expect {| (bool false) |}]
;;

let%expect_test "should parse unit expressions" =
  run_parser {|(               )|};
  [%expect {| unit |}]
;;

let%expect_test "should parse simple variable expressions" =
  run_parser {| x |};
  [%expect {| (variable x) |}];
  run_parser {| long_function_name' |};
  [%expect {| (variable long_function_name') |}]
;;

let%expect_test "should parse parenthesized expressions" =
  run_parser {| ( 15 ) |};
  [%expect {| (int 15) |}];
  run_parser {| (0o534.676) |};
  [%expect {| (float 0o534.676) |}]
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
    {|
    (apply
     (apply (apply (apply (variable map) (variable a)) (variable b))
      (variable c))
     (variable d))
    |}];
  run_parser {| (not a) b c d |};
  [%expect
    {|
    (apply (apply (apply (not (variable a)) (variable b)) (variable c))
     (variable d))
    |}];
  run_parser {| a not b |};
  [%expect {| (apply (variable a) (not (variable b))) |}]
;;

let%expect_test "should parse basic infix expressions" =
  run_parser {| a + b + c + d |};
  [%expect {| (add (add (add (variable a) (variable b)) (variable c)) (variable d)) |}];
  run_parser {| a * b + c * d |};
  [%expect {| (add (mul (variable a) (variable b)) (mul (variable c) (variable d))) |}];
  run_parser {| a * (b + c) * d |};
  [%expect {| (mul (mul (variable a) (add (variable b) (variable c))) (variable d)) |}]
;;

let%expect_test "should parse sequence with right assosciativity" =
  run_parser {| a; b; c; d; e |};
  [%expect
    {|
    (sequence (variable a)
     (sequence (variable b)
      (sequence (variable c) (sequence (variable d) (variable e)))))
    |}]
;;

let%expect_test "should parse complex expression 1" =
  run_parser {|
    if a <> b || c <= d
    then max x y
    else min x y
  |};
  [%expect
    {|
    (if
     (or (not_equals (variable a) (variable b))
      (less_equals (variable c) (variable d)))
     (apply (apply (variable max) (variable x)) (variable y))
     (apply (apply (variable min) (variable x)) (variable y)))
    |}]
;;

let%expect_test "should not parse syntax errors" =
  run_parser {||};
  [%expect
    {|
    ("File \"-\", line 1, character 1"
     "Expected start of expression, instead saw end of file")
    |}];
  run_parser {|a++b|};
  [%expect
    {|
    ("File \"-\", line 1, character 3"
     "Expected start of expression, instead saw +")
    |}]
;;

let%expect_test "should parse let expressions" =
  run_parser {| let x= 3. in x +. y |};
  [%expect {| (let (var_pattern x) (float 3.) (add_float (variable x) (variable y))) |}];
  run_parser {| let x = 5 * 15 + 23 - 22 in add_all x y z (5 - 4) (8. /. 2.)|};
  [%expect
    {|
    (let (var_pattern x) (sub (add (mul (int 5) (int 15)) (int 23)) (int 22))
     (apply
      (apply
       (apply (apply (apply (variable add_all) (variable x)) (variable y))
        (variable z))
       (sub (int 5) (int 4)))
      (div_float (float 8.) (float 2.))))
    |}]
;;

let%expect_test "should parse function definitions" =
  run_parser {| let rec fac n = if n < 1 then 0 else n * fac (n - 1) in fac 6 |};
  [%expect
    {|
    (let (var_pattern fac)
     (fix (var_pattern fac) (var_pattern n)
      (if (less_than (variable n) (int 1)) (int 0)
       (mul (variable n) (apply (variable fac) (sub (variable n) (int 1))))))
     (apply (variable fac) (int 6)))
    |}];
  run_parser {| let max x y = if x >= y then x else y in max -1 22 |};
  [%expect
    {|
    (let (var_pattern max)
     (fun (var_pattern x) (var_pattern y)
      (if (greater_equals (variable x) (variable y)) (variable x) (variable y)))
     (sub (variable max) (apply (int 1) (int 22))))
    |}]
;;

let%expect_test "should parse anonymous function definitions" =
  run_parser {| (fun x -> not x) false |};
  [%expect {| (apply (fun (var_pattern x) (not (variable x))) (bool false)) |}];
  run_parser {| let curry f = fun x y -> f (x, y) in curry (fun (x, y) -> x * y) 3 4 |};
  [%expect
    {|
    (let (var_pattern curry)
     (fun (var_pattern f)
      (fun (var_pattern x) (var_pattern y)
       (apply (variable f) (tuple (variable x) (variable y)))))
     (apply
      (apply
       (apply (variable curry)
        (fun (tuple_pattern (var_pattern x) (var_pattern y))
         (mul (variable x) (variable y))))
       (int 3))
      (int 4)))
    |}]
;;

let%expect_test "should parse dot expressions" =
  run_parser {| Array.create 0 10 |};
  [%expect {| (apply (apply (variable Array.create) (int 0)) (int 10)) |}]
;;

let%expect_test "should parse array expressions" =
  run_parser {| a.(x) |};
  [%expect {| (array_get (variable a) (variable x)) |}];
  run_parser {| a.(x) <- if true then 1 else 2; a.(x) |};
  [%expect
    {|
    (sequence
     (array_set (variable a) (variable x) (if (bool true) (int 1) (int 2)))
     (array_get (variable a) (variable x)))
    |}]
;;
