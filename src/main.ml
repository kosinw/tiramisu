open! Core
open! Tiramisu

let lex =
  Command.basic
    ~summary:"runs the lexer and prints out tokens to stdout"
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
         print_s [%message "" ~token:(token : Token.t) ~position:(position : Position.t)]);
       In_channel.close channel)
;;

let readme () =
  "Tiramisu is a compiler for an OCaml subset which targets RISC-V assembly. It can also \
   run individual compilation stages for analysis or debugging purposes."
;;

let () =
  Command_unix.run ~version:"1.0" (Command.group ~summary:"" [ "lex", lex ] ~readme)
;;
