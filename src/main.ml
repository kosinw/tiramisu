open! Core
open! Tiramisu

let lexer =
  Command.basic
    ~summary:"Runs lexer and outputs token stream to stdout."
    (let open Command.Let_syntax in
     let%map_open filename =
       anon (maybe_with_default "-" ("filename" %: Filename_unix.arg_type))
     in
     fun () ->
       let lexer =
         if String.equal filename "-"
         then Lexer.from_stdin ()
         else Lexer.from_file ~filename
       in
       let tokens = Lexer.token_all lexer in
       print_s [%message (tokens : Token.t list)])
;;

let group = Command.group ~summary:"The Tiramisu compiler" [ "lexer", lexer ]
let () = Command_unix.run group
