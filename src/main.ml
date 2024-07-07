open! Core
open! Tiramisu

let lex =
  Command.basic
    ~summary:"runs the lexer and prints out token stream"
    (let open Command.Let_syntax in
     let%map_open filename =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       let channel =
         match filename with
         | "-" -> In_channel.stdin
         | _ -> In_channel.create filename
       in
       let lexer = Lexer.from_channel ~filename channel in
       let tokens = Lexer.all lexer in
       List.iter tokens ~f:(fun (token, position) ->
         print_s [%message "" ~_:(token : Token.t) ~_:(position : Position.t)]);
       In_channel.close channel)
;;

let parse =
  Command.basic
    ~summary:"runs the parse and prints out the syntax tree"
    (let open Command.Let_syntax in
     let%map_open filename =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       let channel =
         match filename with
         | "-" -> In_channel.stdin
         | _ -> In_channel.create filename
       in
       let lexer = Lexer.from_channel ~filename channel in
       let parser = Parser.parse lexer in
       let syntax = Parser.syntax parser in
       let spans = Parser.spans parser in
       let errors = Parser.errors parser in
       print_s [%message (syntax : Syntax.t)];
       print_s [%message (spans : Span.t Id.Map.t)];
       print_s [%message (errors : (Error.t * Position.t) list)];
       In_channel.close channel)
;;

let readme () =
  "Tiramisu is a compiler for an OCaml subset which targets RISC-V assembly. It can also \
   run individual compilation stages for analysis or debugging purposes."
;;

let () =
  Command_unix.run
    ~version:"1.0"
    (Command.group ~summary:"" [ "lex", lex; "parse", parse ] ~readme)
;;
