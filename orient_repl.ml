open Lexing
open Printf

type parse_result = Complete of Interpreter.cst | Incomplete | Error of string

let try_parse_complete (buf : string) =
  let lexbuf = from_string buf in
  try
    let result = Parser.complete_expression Lexer.read lexbuf in
    Complete result
  with
  | Lexer.SyntaxError msg -> Error msg
  | Parser.Error -> Incomplete

let repl () =
  try
    let rec loop acc_lines =
      printf (if acc_lines = [] then "> " else "  ");
      flush stdout;
      let line = read_line () in
      let current_input = String.concat "\n" (List.rev (line :: acc_lines)) in

      match try_parse_complete current_input with
      | Complete ast ->
          let result = Interpreter.eval ast [] in
          printf "= %s\n" (Interpreter.string_of_expr result);
          loop []
      | Incomplete -> loop (line :: acc_lines)
      | Error msg ->
          printf "Error: %s\n" msg;
          loop []
    in
    loop []
  with End_of_file -> ()

let () = repl ()
