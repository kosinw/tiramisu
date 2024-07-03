open! Core

(** [>>] is the forward composition operator. *)
let ( >> ) f g x = x |> f |> g

type t =
  { filename : string
  ; contents : string
  ; abs_pos : int
  ; abs_lnum : int
  ; abs_cnum : int
  }
[@@deriving sexp_of]

let from_string ?filename contents =
  { filename = Option.value ~default:"-" filename
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

let is_identifier = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' -> true
  | _ -> false
;;

let is_binary_digit = function
  | '0' | '1' -> true
  | _ -> false
;;

let is_octal_digit = function
  | '0' .. '7' -> true
  | _ -> false
;;

let consume_while ~f t =
  let rec consume_while' l t =
    match peek_char t with
    | Some c ->
      (match f c with
       | true -> consume_while' (c :: l) (advance_char t)
       | false -> l, t)
    | None -> l, t
  in
  let l, t = consume_while' [] t in
  String.of_char_list (List.rev l), t
;;

let skip_whitespace = consume_while ~f:Char.is_whitespace >> snd
let consume_until ~f = consume_while ~f:(Fn.non f)
let advance_until ~f = consume_until ~f >> snd

let position t =
  let { filename; abs_lnum; abs_cnum; _ } = skip_whitespace t in
  let line_number = succ abs_lnum in
  let column_number = succ abs_cnum in
  Position.create ~filename ~line_number ~column_number
;;

let rec token t =
  match peek_char t with
  | Some ')' -> Token.Right_paren, advance_char t
  | Some '=' -> Token.Equal, advance_char t
  | Some '.' -> Token.Dot, advance_char t
  | Some '\'' -> Token.Quote, advance_char t
  | Some ':' -> Token.Colon, advance_char t
  | Some ',' -> Token.Comma, advance_char t
  | Some ';' -> Token.Semicolon, advance_char t
  | Some '(' -> with_left_paren (advance_char t)
  | Some '+' -> with_plus (advance_char t)
  | Some '-' -> with_minus (advance_char t)
  | Some '*' -> with_ast (advance_char t)
  | Some '/' -> with_slash (advance_char t)
  | Some '<' -> with_less (advance_char t)
  | Some '>' -> with_greater (advance_char t)
  | Some '"' -> with_quote [] (advance_char t)
  | Some '0' .. '9' -> with_numeric t
  | Some x when is_identifier x -> keyword t
  | Some x when Char.is_whitespace x -> token (skip_whitespace t)
  | Some x ->
    ( Token.Illegal (Error.of_string [%string "Unknown start of token, %{x#Char}"])
    , advance_char t )
  | None -> Token.Eof, t

and with_numeric t =
  match peek_char t with
  | Some '0' ->
    let t = advance_char t in
    (match peek_char t with
     | Some 'x' | Some 'X' -> with_hex (advance_char t)
     | Some 'b' | Some 'B' -> with_binary (advance_char t)
     | Some 'o' | Some 'O' -> with_octal (advance_char t)
     | _ -> with_decimal t)
  | Some '1' .. '9' -> with_decimal t
  | _ -> Token.Illegal (Error.of_string "Invalid integer literal"), advance_char t

and with_digit ~prefix ~f l t =
  match peek_char t with
  | Some '.' -> with_float ~prefix ~f ('.' :: l) (advance_char t)
  | Some '_' -> with_digit ~prefix ~f ('_' :: l) (advance_char t)
  | Some x when f x -> with_digit ~prefix ~f (x :: l) (advance_char t)
  | Some x when Char.is_whitespace x ->
    Token.Int (prefix ^ String.of_list (List.rev l)), t
  | None -> Token.Int (prefix ^ String.of_list (List.rev l)), t
  | _ ->
    ( Token.Illegal (Error.of_string [%string "Invalid integer literal"])
    , advance_until ~f:Char.is_whitespace t )

and with_float ~prefix ~f l t =
  match peek_char t with
  | Some '_' -> with_float ~prefix ~f ('_' :: l) (advance_char t)
  | Some x when f x -> with_float ~prefix ~f (x :: l) (advance_char t)
  | Some x when Char.is_whitespace x -> Token.Float (String.of_list (List.rev l)), t
  | None -> Token.Float (prefix ^ String.of_list (List.rev l)), t
  | _ ->
    ( Token.Illegal (Error.of_string [%string "Invalid float literal"])
    , advance_until ~f:Char.is_whitespace t )

and with_hex t = with_digit ~prefix:"0x" ~f:is_binary_digit [] t
and with_binary t = with_digit ~prefix:"0b" ~f:is_binary_digit [] t
and with_octal t = with_digit ~prefix:"0o" ~f:is_octal_digit [] t
and with_decimal t = with_digit ~prefix:"" ~f:Char.is_digit [] t

and with_left_paren t =
  match peek_char t with
  | Some '*' -> comment 0 (advance_char t)
  | _ -> Token.Left_paren, t

and with_plus t =
  match peek_char t with
  | Some '.' -> Token.Plus_dot, advance_char t
  | _ -> Token.Plus, t

and with_minus t =
  match peek_char t with
  | Some '.' -> Token.Minus_dot, advance_char t
  | Some '>' -> Token.Minus_greater, advance_char t
  | _ -> Token.Minus, t

and with_ast t =
  match peek_char t with
  | Some '.' -> Token.Asterisk_dot, advance_char t
  | _ -> Token.Asterisk, t

and with_slash t =
  match peek_char t with
  | Some '.' -> Token.Slash_dot, advance_char t
  | _ -> Token.Slash, t

and with_less t =
  match peek_char t with
  | Some '=' -> Token.Less_equal, advance_char t
  | Some '>' -> Token.Less_greater, advance_char t
  | _ -> Token.Less, t

and with_greater t =
  match peek_char t with
  | Some '=' -> Token.Greater_equal, advance_char t
  | _ -> Token.Greater, t

and keyword t =
  match consume_while ~f:is_identifier t with
  | "not", t -> Token.Not, t
  | "if", t -> Token.If, t
  | "then", t -> Token.Then, t
  | "else", t -> Token.Else, t
  | "let", t -> Token.Let, t
  | "in", t -> Token.In, t
  | "rec", t -> Token.Rec, t
  | "true", t -> Token.Bool true, t
  | "false", t -> Token.Bool false, t
  | "fun", t -> Token.Fun, t
  | s, t -> Token.Ident s, t

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
  | None -> Token.Illegal (Error.of_string "Comment not terminated"), t

and with_quote l t =
  match peek_char t with
  | Some '"' -> Token.String (String.of_char_list (List.rev l)), advance_char t
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
       ( Token.Illegal
           (Error.of_string [%string "String escape sequence unknown, \\%{x#Char}"])
       , advance_until ~f:(Char.equal '"') t )
     | None -> Token.Illegal (Error.of_string "String escape sequence not terminated"), t)
  | Some c -> with_quote (c :: l) (advance_char t)
  | None -> Token.Illegal (Error.of_string "String literal not terminated"), t
;;

let all t =
  let rec all' l t =
    let t = skip_whitespace t in
    let position = position t in
    let token, t' = token t in
    match token with
    | Token.Eof -> List.rev ((token, position) :: l)
    | _ -> all' ((token, position) :: l) t'
  in
  all' [] t
;;

let peek = token >> fst
let advance = token >> snd

let%expect_test "should work with basic program" =
  let lexer = from_string {| let hello = 3 in |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     ((Let ((filename -) (line_number 1) (column_number 2)))
      ((Ident hello) ((filename -) (line_number 1) (column_number 6)))
      (Equal ((filename -) (line_number 1) (column_number 12)))
      ((Int 3) ((filename -) (line_number 1) (column_number 14)))
      (In ((filename -) (line_number 1) (column_number 16)))
      (Eof ((filename -) (line_number 1) (column_number 19)))))
    |}]
;;

let%expect_test "should work with comments" =
  let lexer = from_string {| (* Hello, new world! *) let hello = 3 in |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     ((Let ((filename -) (line_number 1) (column_number 2)))
      ((Ident hello) ((filename -) (line_number 1) (column_number 30)))
      (Equal ((filename -) (line_number 1) (column_number 36)))
      ((Int 3) ((filename -) (line_number 1) (column_number 38)))
      (In ((filename -) (line_number 1) (column_number 40)))
      (Eof ((filename -) (line_number 1) (column_number 43)))))
    |}]
;;

let%expect_test "should work with nested comments" =
  let lexer =
    from_string
      {| (* (* Hello, new world! *) (* Multiple nested comments *) *) let hello = 3 in |}
  in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     ((Let ((filename -) (line_number 1) (column_number 2)))
      ((Ident hello) ((filename -) (line_number 1) (column_number 67)))
      (Equal ((filename -) (line_number 1) (column_number 73)))
      ((Int 3) ((filename -) (line_number 1) (column_number 75)))
      (In ((filename -) (line_number 1) (column_number 77)))
      (Eof ((filename -) (line_number 1) (column_number 80)))))
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
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect {| (tokens ((Eof ((filename -) (line_number 2) (column_number 1))))) |}]
;;

let%expect_test "should throw error with unterminated comments" =
  let lexer = from_string {| (* |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     (((Illegal "Comment not terminated")
       ((filename -) (line_number 1) (column_number 2)))
      (Eof ((filename -) (line_number 1) (column_number 5)))))
    |}]
;;

let%expect_test "should throw error with bad token" =
  let lexer = from_string {| 1ab |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     (((Illegal "Invalid integer literal")
       ((filename -) (line_number 1) (column_number 2)))
      (Eof ((filename -) (line_number 1) (column_number 6)))))
    |}]
;;

let%expect_test "should throw error with bad token (2)" =
  let lexer = from_string {| ! |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     (((Illegal "Unknown start of token, !")
       ((filename -) (line_number 1) (column_number 2)))
      (Eof ((filename -) (line_number 1) (column_number 4)))))
    |}]
;;

let%expect_test "should throw error with bad token (3)" =
  let lexer = from_string {| 3.o |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     (((Illegal "Invalid float literal")
       ((filename -) (line_number 1) (column_number 2)))
      (Eof ((filename -) (line_number 1) (column_number 6)))))
    |}]
;;

let%expect_test "should work with weird tokens" =
  let lexer = from_string {| let x = 3.4 +. 5. in let y = -3 + 4 in -z |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     ((Let ((filename -) (line_number 1) (column_number 2)))
      ((Ident x) ((filename -) (line_number 1) (column_number 6)))
      (Equal ((filename -) (line_number 1) (column_number 8)))
      ((Float 3.4) ((filename -) (line_number 1) (column_number 10)))
      (Plus_dot ((filename -) (line_number 1) (column_number 14)))
      ((Float 5.) ((filename -) (line_number 1) (column_number 17)))
      (In ((filename -) (line_number 1) (column_number 20)))
      (Let ((filename -) (line_number 1) (column_number 23)))
      ((Ident y) ((filename -) (line_number 1) (column_number 27)))
      (Equal ((filename -) (line_number 1) (column_number 29)))
      (Minus ((filename -) (line_number 1) (column_number 31)))
      ((Int 3) ((filename -) (line_number 1) (column_number 32)))
      (Plus ((filename -) (line_number 1) (column_number 34)))
      ((Int 4) ((filename -) (line_number 1) (column_number 36)))
      (In ((filename -) (line_number 1) (column_number 38)))
      (Minus ((filename -) (line_number 1) (column_number 41)))
      ((Ident z) ((filename -) (line_number 1) (column_number 42)))
      (Eof ((filename -) (line_number 1) (column_number 44)))))
    |}]
;;

let%expect_test "should work with parsing string literals" =
  let lexer = from_string {| "\\hello" "hello" "hell\"o" "foo" |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     (((String "\\hello") ((filename -) (line_number 1) (column_number 2)))
      ((String hello) ((filename -) (line_number 1) (column_number 12)))
      ((String "hell\"o") ((filename -) (line_number 1) (column_number 20)))
      ((String foo) ((filename -) (line_number 1) (column_number 30)))
      (Eof ((filename -) (line_number 1) (column_number 36)))))
    |}]
;;

let%expect_test "testing Array.create" =
  let lexer = from_string {| Array.create |} in
  let tokens = all lexer in
  print_s [%message (tokens : (Token.t * Position.t) list)];
  [%expect
    {|
    (tokens
     (((Ident Array) ((filename -) (line_number 1) (column_number 2)))
      (Dot ((filename -) (line_number 1) (column_number 7)))
      ((Ident create) ((filename -) (line_number 1) (column_number 8)))
      (Eof ((filename -) (line_number 1) (column_number 15)))))
    |}]
;;
