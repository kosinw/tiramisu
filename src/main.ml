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
       let channel =
         if String.equal filename "-"
         then In_channel.stdin
         else In_channel.create filename
       in
       let lexer = Lexer.from_channel ~filename channel in
       let tokens = Lexer.all lexer in
       print_s [%message (tokens : Token.With_position.t Or_error.t list)];
       In_channel.close channel)
;;

let group = Command.group ~summary:"The Tiramisu compiler" [ "lexer", lexer ]
let () = Command_unix.run group
