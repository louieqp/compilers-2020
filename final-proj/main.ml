(* driver for parser *)

let parse s =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
  Bool.compile_prog ast

(* commented out so you don't have to rewrite the pretty printer, use parse instead.
let ast_test infile =
   let lexbuf = Lexing.from_channel infile in
   let loop () =
     let prog  = Parser.main Lexer.token lexbuf in
     Pretty.print_prog prog
   in
   loop ()

let _ = 
   if Array.length Sys.argv <> 2 then
      Printf.fprintf stderr "usage: %s input_filename\n" Sys.argv.(0)
   else
      let infile = open_in Sys.argv.(1) in
         try
            ast_test infile
         with (Failure f) ->
            Printf.fprintf stderr "\nERROR: %s\n" f;
         close_in infile
*)
