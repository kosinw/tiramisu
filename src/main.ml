open! Core
open! Tiramisu

let lex =
  Command.basic
    ~summary:"Runs the lexical analyzer and prints out token stream to stdout."
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
       print_s [%message (tokens : Lexer.Result.t list)];
       In_channel.close channel)
;;

let readme () =
  "Tiramisu is a compiler for an ML language which targets to RISC-V assembly. It can \
   also run individual compilation stages for analysis or debugging purposes."
;;

let () =
  Command_unix.run
    ~version:"1.0"
    (Command.group ~summary:"Tiramisu" [ "lex", lex ] ~readme)
;;
