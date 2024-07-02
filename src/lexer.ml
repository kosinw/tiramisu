open! Core

type t =
  { filename : string
  ; contents : string
  ; abs_pos : int
  ; abs_lnum : int
  ; abs_cnum : int
  }
[@@deriving sexp_of]

let from_string ?filename contents =
  { filename = Option.value ~default:"[anon]" filename
  ; contents
  ; abs_pos = 0
  ; abs_lnum = 0
  ; abs_cnum = 0
  }
;;

let from_channel ?filename channel =
  channel |> In_channel.input_all |> from_string ?filename
;;

let is_end_of_stream t = t.abs_pos >= String.length t.contents

let peek_char t =
  match is_end_of_stream t with
  | true -> None
  | false -> Some (String.get t.contents t.abs_pos)
;;

let advance_char t =
  let { abs_pos; abs_lnum; abs_cnum; _ } = t in
  match peek_char t with
  | Some '\n' -> { t with abs_pos = abs_pos + 1; abs_lnum = abs_lnum + 1; abs_cnum = 0 }
  | Some _ -> { t with abs_pos = abs_pos + 1; abs_cnum = abs_cnum + 1 }
  | None -> t
;;

let rec skip_whitespace t =
  match peek_char t with
  | Some c ->
    (match Char.is_whitespace c with
     | true -> skip_whitespace (advance_char t)
     | false -> t)
  | None -> t
;;

let to_position { filename; abs_lnum; abs_cnum; _ } =
  let line_number = succ abs_lnum in
  let column_number = succ abs_cnum in
  Position.create ~filename ~line_number ~column_number
;;

let consume_string t =
  let rec consume_string' l t =
    match peek_char t with
    | Some c ->
      (match c with
       | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' ->
         consume_string' (c :: l) (advance_char t)
       | _ -> t, l)
    | None -> t, l
  in
  let t, l = consume_string' [] t in
  t, String.of_char_list (List.rev l)
;;

let parse_numeric ~floating s =
  let open Option.Let_syntax in
  match floating with
  | true ->
    let%map f = Float.of_string_opt s in
    `Float f
  | false ->
    let%map i = Int.of_string_opt s in
    `Int i
;;

let rec token t =
  match peek_char t with
  | Some ')' -> Ok Token.Right_paren, advance_char t
  | Some '=' -> Ok Token.Equal, advance_char t
  | Some '.' -> Ok Token.Dot, advance_char t
  | Some '\'' -> Ok Token.Quote, advance_char t
  | Some ':' -> Ok Token.Colon, advance_char t
  | Some ';' -> Ok Token.Semicolon, advance_char t
  | Some ',' -> Ok Token.Comma, advance_char t
  | Some '(' -> with_left_paren (advance_char t)
  | Some '+' -> with_plus (advance_char t)
  | Some '-' -> with_minus (advance_char t)
  | Some '*' -> with_ast (advance_char t)
  | Some '/' -> with_slash (advance_char t)
  | Some '<' -> with_less (advance_char t)
  | Some '>' -> with_greater (advance_char t)
  | Some '"' -> with_quote [] (advance_char t)
  | Some '0' .. '9' -> with_numeric [] t
  | Some ('a' .. 'z' | 'A' .. 'Z' | '_') -> keyword t
  | Some x when Char.is_whitespace x -> token (skip_whitespace t)
  | Some x -> Or_error.error_string [%string "Unknown start of token, %{x#Char}"], t
  | None -> Ok Token.Eof, t

and with_numeric ?(negative = false) ?(floating = false) l t =
  let err =
    [%string
      {|Invalid %{if floating then "float" else "integer"} literal, %{String.of_list (List.rev l)}|}]
  in
  match peek_char t with
  | Some
      (('0' .. '9' | 'x' | 'X' | 'o' | 'O' | 'b' | 'B' | '_' | 'a' .. 'f' | 'A' .. 'F') as
       x) -> with_numeric ~negative ~floating (x :: l) (advance_char t)
  | Some '.' ->
    (match floating with
     | true -> Or_error.error_string err, t
     | false -> with_numeric ~negative ~floating:true ('.' :: l) (advance_char t))
  | _ ->
    let s = String.of_list (List.rev l) in
    let s = if negative then "-" ^ s else s in
    (match parse_numeric ~floating s with
     | Some (`Float _) -> Ok (Token.Float s), t
     | Some (`Int _) -> Ok (Token.Int s), t
     | None -> Or_error.error_string err, t)

and with_left_paren t =
  match peek_char t with
  | Some '*' -> comment 0 (advance_char t)
  | _ -> Ok Token.Left_paren, t

and with_plus t =
  match peek_char t with
  | Some '.' -> Ok Token.Plus_dot, advance_char t
  | _ -> Ok Token.Plus, t

and with_minus t =
  match peek_char t with
  | Some '.' -> Ok Token.Minus_dot, advance_char t
  | Some '>' -> Ok Token.Minus_greater, advance_char t
  | Some '0' .. '9' -> with_numeric ~negative:true [] t
  | _ -> Ok Token.Minus, t

and with_ast t =
  match peek_char t with
  | Some '.' -> Ok Token.Asterisk_dot, advance_char t
  | _ -> Ok Token.Asterisk, t

and with_slash t =
  match peek_char t with
  | Some '.' -> Ok Token.Slash_dot, advance_char t
  | _ -> Ok Token.Slash, t

and with_less t =
  match peek_char t with
  | Some '=' -> Ok Token.Less_equal, advance_char t
  | Some '>' -> Ok Token.Less_greater, advance_char t
  | _ -> Ok Token.Less, t

and with_greater t =
  match peek_char t with
  | Some '=' -> Ok Token.Greater_equal, advance_char t
  | _ -> Ok Token.Greater, t

and keyword t =
  let t, s = consume_string t in
  match s with
  | "not" -> Ok Token.Not, t
  | "if" -> Ok Token.If, t
  | "then" -> Ok Token.Then, t
  | "else" -> Ok Token.Else, t
  | "let" -> Ok Token.Let, t
  | "in" -> Ok Token.In, t
  | "rec" -> Ok Token.Rec, t
  | "_" -> Ok (Token.Ident "_"), t
  | "true" -> Ok (Token.Bool true), t
  | "false" -> Ok (Token.Bool false), t
  | "fun" -> Ok Token.Fun, t
  | _ -> identifier t s

and identifier t s =
  match
    String.for_all s ~f:(function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
      | _ -> false)
  with
  | true -> Ok (Token.Ident s), t
  | false -> Or_error.error_string [%string "Invalid identifier, %{s}"], t

and comment n t =
  match peek_char t with
  | Some '(' ->
    let t = advance_char t in
    (match peek_char t with
     | Some '*' -> comment (succ n) (advance_char t)
     | _ -> comment n (advance_char t))
  | Some '*' ->
    let t = advance_char t in
    (match peek_char t with
     | Some ')' ->
       (match n with
        | 0 -> token (advance_char t)
        | n' -> comment (pred n') (advance_char t))
     | _ -> comment n (advance_char t))
  | Some _ -> comment n (advance_char t)
  | None -> Or_error.error_string "Comment not terminated", t

and with_quote l t =
  match peek_char t with
  | Some '"' -> Ok (Token.String (String.of_char_list (List.rev l))), advance_char t
  | Some '\\' ->
    let t = advance_char t in
    (match peek_char t with
     | Some '\\' -> with_quote ('\\' :: l) (advance_char t)
     | Some '"' -> with_quote ('"' :: l) (advance_char t)
     | Some '\'' -> with_quote ('\'' :: l) (advance_char t)
     | Some 'n' -> with_quote ('\n' :: l) (advance_char t)
     | Some 'r' -> with_quote ('\r' :: l) (advance_char t)
     | Some 't' -> with_quote ('\t' :: l) (advance_char t)
     | Some 'b' -> with_quote ('\b' :: l) (advance_char t)
     | Some ' ' -> with_quote (' ' :: l) (advance_char t)
     | Some x ->
       Or_error.error_string [%string "String escape sequence unknown, \\%{x#Char}"], t
     | None -> Or_error.error_string "String escape sequence not terminated", t)
  | Some c -> with_quote (c :: l) (advance_char t)
  | None -> Or_error.error_string "String literal not terminated", t
;;

let token_with_position t =
  let t = skip_whitespace t in
  let position = to_position t in
  match token t with
  | Ok token, t -> Ok (Token.With_position.create ~token ~position), t
  | Error err, t -> Error err, t
;;

let all t =
  let rec all' l t =
    match token_with_position t with
    | Ok token, t ->
      (match Token.With_position.token token with
       | Token.Eof -> List.rev l
       | _ -> all' (Ok token :: l) t)
    | Error e, _ -> List.rev (Error e :: l)
  in
  all' [] t
;;

let current = Fn.compose fst token_with_position
let advance = Fn.compose snd token_with_position

let%expect_test "should work with basic program" =
  let lexer = from_string {| let hello = 3 in |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {|
    (tokens
     ((Ok
       ((token Let)
        (position ((filename [anon]) (line_number 1) (column_number 2)))))
      (Ok
       ((token (Ident hello))
        (position ((filename [anon]) (line_number 1) (column_number 6)))))
      (Ok
       ((token Equal)
        (position ((filename [anon]) (line_number 1) (column_number 12)))))
      (Ok
       ((token (Int 3))
        (position ((filename [anon]) (line_number 1) (column_number 14)))))
      (Ok
       ((token In)
        (position ((filename [anon]) (line_number 1) (column_number 16)))))))
    |}]
;;

let%expect_test "should work with positions" =
  let lexer = from_string {| let hello = 3 in |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {|
    (tokens
     ((Ok
       ((token Let)
        (position ((filename [anon]) (line_number 1) (column_number 2)))))
      (Ok
       ((token (Ident hello))
        (position ((filename [anon]) (line_number 1) (column_number 6)))))
      (Ok
       ((token Equal)
        (position ((filename [anon]) (line_number 1) (column_number 12)))))
      (Ok
       ((token (Int 3))
        (position ((filename [anon]) (line_number 1) (column_number 14)))))
      (Ok
       ((token In)
        (position ((filename [anon]) (line_number 1) (column_number 16)))))))
    |}]
;;

let%expect_test "should work with comments" =
  let lexer = from_string {| (* Hello, new world! *) let hello = 3 in |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {|
    (tokens
     ((Ok
       ((token Let)
        (position ((filename [anon]) (line_number 1) (column_number 2)))))
      (Ok
       ((token (Ident hello))
        (position ((filename [anon]) (line_number 1) (column_number 30)))))
      (Ok
       ((token Equal)
        (position ((filename [anon]) (line_number 1) (column_number 36)))))
      (Ok
       ((token (Int 3))
        (position ((filename [anon]) (line_number 1) (column_number 38)))))
      (Ok
       ((token In)
        (position ((filename [anon]) (line_number 1) (column_number 40)))))))
    |}]
;;

let%expect_test "should work with nested comments" =
  let lexer =
    from_string
      {| (* (* Hello, new world! *) (* Multiple nested comments *) *) let hello = 3 in |}
  in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {|
    (tokens
     ((Ok
       ((token Let)
        (position ((filename [anon]) (line_number 1) (column_number 2)))))
      (Ok
       ((token (Ident hello))
        (position ((filename [anon]) (line_number 1) (column_number 67)))))
      (Ok
       ((token Equal)
        (position ((filename [anon]) (line_number 1) (column_number 73)))))
      (Ok
       ((token (Int 3))
        (position ((filename [anon]) (line_number 1) (column_number 75)))))
      (Ok
       ((token In)
        (position ((filename [anon]) (line_number 1) (column_number 77)))))))
    |}]
;;

let%expect_test "should work with comments on new lines" =
  let lexer =
    from_string {|
(*
  (* Hello, new world! *)
  (* Multiple nested comments *)
*)
  |}
  in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {| (tokens ()) |}]
;;

let%expect_test "should throw error with unterminated comments" =
  let lexer = from_string {| (* |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {| (tokens ((Error "Comment not terminated"))) |}]
;;

let%expect_test "should throw error with bad token" =
  let lexer = from_string {| 1ab |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {| (tokens ((Error "Invalid integer literal, 1ab"))) |}]
;;

let%expect_test "should throw error with bad token (2)" =
  let lexer = from_string {| ! |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {| (tokens ((Error "Unknown start of token, !"))) |}]
;;

let%expect_test "should throw error with bad token (3)" =
  let lexer = from_string {| 3.o |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {| (tokens ((Error "Invalid float literal, 3.o"))) |}]
;;

let%expect_test "should work with weird tokens" =
  let lexer = from_string {| let x = 3.4 +. 5. in let y = -3 + 4 in -z |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {|
    (tokens
     ((Ok
       ((token Let)
        (position ((filename [anon]) (line_number 1) (column_number 2)))))
      (Ok
       ((token (Ident x))
        (position ((filename [anon]) (line_number 1) (column_number 6)))))
      (Ok
       ((token Equal)
        (position ((filename [anon]) (line_number 1) (column_number 8)))))
      (Ok
       ((token (Float 3.4))
        (position ((filename [anon]) (line_number 1) (column_number 10)))))
      (Ok
       ((token Plus_dot)
        (position ((filename [anon]) (line_number 1) (column_number 14)))))
      (Ok
       ((token (Float 5.))
        (position ((filename [anon]) (line_number 1) (column_number 17)))))
      (Ok
       ((token In)
        (position ((filename [anon]) (line_number 1) (column_number 20)))))
      (Ok
       ((token Let)
        (position ((filename [anon]) (line_number 1) (column_number 23)))))
      (Ok
       ((token (Ident y))
        (position ((filename [anon]) (line_number 1) (column_number 27)))))
      (Ok
       ((token Equal)
        (position ((filename [anon]) (line_number 1) (column_number 29)))))
      (Ok
       ((token (Int -3))
        (position ((filename [anon]) (line_number 1) (column_number 31)))))
      (Ok
       ((token Plus)
        (position ((filename [anon]) (line_number 1) (column_number 34)))))
      (Ok
       ((token (Int 4))
        (position ((filename [anon]) (line_number 1) (column_number 36)))))
      (Ok
       ((token In)
        (position ((filename [anon]) (line_number 1) (column_number 38)))))
      (Ok
       ((token Minus)
        (position ((filename [anon]) (line_number 1) (column_number 41)))))
      (Ok
       ((token (Ident z))
        (position ((filename [anon]) (line_number 1) (column_number 42)))))))
    |}]
;;

let%expect_test "should work with parsing string literals" =
  let lexer = from_string {| "\\hello" "hello" "hell\"o" "foo" |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {|
    (tokens
     ((Ok
       ((token (String "\\hello"))
        (position ((filename [anon]) (line_number 1) (column_number 2)))))
      (Ok
       ((token (String hello))
        (position ((filename [anon]) (line_number 1) (column_number 12)))))
      (Ok
       ((token (String "hell\"o"))
        (position ((filename [anon]) (line_number 1) (column_number 20)))))
      (Ok
       ((token (String foo))
        (position ((filename [anon]) (line_number 1) (column_number 30)))))))
    |}]
;;

let%expect_test "testing Array.create" =
  let lexer = from_string {| Array.create |} in
  let tokens = all lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {|
    (tokens
     ((Ok
       ((token (Ident Array))
        (position ((filename [anon]) (line_number 1) (column_number 2)))))
      (Ok
       ((token Dot)
        (position ((filename [anon]) (line_number 1) (column_number 7)))))
      (Ok
       ((token (Ident create))
        (position ((filename [anon]) (line_number 1) (column_number 8)))))))
    |}]
;;
