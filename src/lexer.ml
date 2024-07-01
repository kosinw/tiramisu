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
  { filename = Option.value ~default:"[unnamed]" filename
  ; contents
  ; abs_pos = 0
  ; abs_lnum = 0
  ; abs_cnum = 0
  }
;;

let from_file ~filename =
  filename |> In_channel.create |> In_channel.input_all |> from_string ~filename
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
      (match Char.is_whitespace c with
       | false -> consume_string' (c :: l) (advance_char t)
       | true -> t, l)
    | None -> t, l
  in
  let t, l = consume_string' [] t in
  t, String.of_char_list (List.rev l)
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
  | Some x ->
    (match Char.is_whitespace x with
     | true -> token (skip_whitespace t)
     | false -> Tuple2.uncurry non_symbol (consume_string t))
  | None -> Ok Token.Eof, t

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

and non_symbol t = function
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
  | s ->
    (match Int.of_string_opt s with
     | Some i -> Ok (Token.Int i), t
     | None ->
       (match Float.of_string_opt s with
        | Some f -> Ok (Token.Float f), t
        | None -> identifier t s))

and identifier t s =
  match String.to_list s with
  | ('a' .. 'z' | 'A' .. 'Z' | '_') :: tl ->
    if List.for_all tl ~f:(function
         | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
         | _ -> false)
    then Ok (Token.Ident s), t
    else Or_error.error_string [%string "Unknown token, %{s}"], t
  | _ -> Or_error.error_string [%string "Unknown token, %{s}"], t

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
;;

let token_with_position t =
  let t = skip_whitespace t in
  let position = to_position t in
  match token t with
  | Ok token, t -> Ok (Token.With_position.create ~token ~position), t
  | Error err, t -> Error err, t
;;

let token_with_position_all t =
  let rec token_all' l t =
    match token_with_position t with
    | Ok token, t ->
      (match Token.With_position.token token with
       | Token.Eof -> List.rev l
       | _ -> token_all' (token :: l) t)
    | Error e, _ -> raise (Error.to_exn e)
  in
  token_all' [] t
;;

let token_all_err t =
  let rec token_all' l t =
    match token_with_position t with
    | Ok token, t ->
      (match Token.With_position.token token with
       | Token.Eof -> List.rev l
       | _ -> token_all' (Ok token :: l) t)
    | Error e, _ -> List.rev (Error e :: l)
  in
  token_all' [] t
;;

let token_all = Fn.compose (List.map ~f:Token.With_position.token) token_with_position_all
let current = Fn.compose fst token_with_position
let advance = Fn.compose snd token_with_position

let%expect_test "should work with basic program" =
  let lexer = from_string {| let hello = 3 in |} in
  let tokens = token_all lexer in
  print_s [%message (tokens : Token.t list)];
  [%expect {| (tokens (Let (Ident hello) Equal (Int 3) In)) |}]
;;

let%expect_test "should work with positions" =
  let lexer = from_string ~filename:"[for testing]" {| let hello = 3 in |} in
  let tokens = token_with_position_all lexer in
  print_s [%message (tokens : Token.With_position.t list)];
  [%expect
    {|
    (tokens
     (((token Let)
       (position ((filename "[for testing]") (line_number 1) (column_number 2))))
      ((token (Ident hello))
       (position ((filename "[for testing]") (line_number 1) (column_number 6))))
      ((token Equal)
       (position ((filename "[for testing]") (line_number 1) (column_number 12))))
      ((token (Int 3))
       (position ((filename "[for testing]") (line_number 1) (column_number 14))))
      ((token In)
       (position ((filename "[for testing]") (line_number 1) (column_number 16))))))
    |}]
;;

let%expect_test "should work with comments" =
  let lexer = from_string {| (* Hello, new world! *) let hello = 3 in |} in
  let tokens = token_all lexer in
  print_s [%message (tokens : Token.t list)];
  [%expect {| (tokens (Let (Ident hello) Equal (Int 3) In)) |}]
;;

let%expect_test "should work with nested comments" =
  let lexer =
    from_string
      {| (* (* Hello, new world! *) (* Multiple nested comments *) *) let hello = 3 in |}
  in
  let tokens = token_all lexer in
  print_s [%message (tokens : Token.t list)];
  [%expect {| (tokens (Let (Ident hello) Equal (Int 3) In)) |}]
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
  let tokens = token_all lexer in
  print_s [%message (tokens : Token.t list)];
  [%expect {| (tokens ()) |}]
;;

let%expect_test "should throw error with unterminated comments" =
  let lexer = from_string {| (* |} in
  let tokens = token_all_err lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {| (tokens ((Error "Comment not terminated"))) |}]
;;

let%expect_test "should throw error with bad token" =
  let lexer = from_string {| 1ab |} in
  let tokens = token_all_err lexer in
  print_s [%message (tokens : Token.With_position.t Or_error.t list)];
  [%expect {| (tokens ((Error "Unknown token, 1ab"))) |}]
;;

let%expect_test "should work with weird tokens" =
  let lexer = from_string {| let x = 3.4 +. 5. in let y = -3 + 4 in -z |} in
  let tokens = token_all lexer in
  print_s [%message (tokens : Token.t list)];
  [%expect
    {|
    (tokens
     (Let (Ident x) Equal (Float 3.4) Plus_dot (Float 5) In Let (Ident y) Equal
      Minus (Int 3) Plus (Int 4) In Minus (Ident z)))
    |}]
;;
