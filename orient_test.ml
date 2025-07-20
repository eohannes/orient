(* massively overcomplicated *)

open Interpreter

type fail = [ `Parse | `Eval ]

type result =
  [ `Pass of string * Interpreter.cst | `Fail of fail * string * string ]

let is_test f = Filename.check_suffix f ".test"

(* checks if the parse is correct *)
let check_parse f =
  match Lexer.parse_file f with
  | Some expr ->
      let expected_file = open_in (Filename.remove_extension f ^ ".parse") in
      let expected = input_line expected_file in
      let parse = string_of_expr expr in
      if expected <> parse then
        `Fail (`Parse, f, "   parse: " ^ parse ^ "\nexpected: " ^ expected)
      else `Pass expr
  | None -> `Fail (`Parse, f, "parser returned None")

(* checks if the actual result is correct *)
let check_test f =
  match check_parse f with
  | `Fail (c, f', r) -> `Fail (c, f', r)
  | `Pass expr -> (
      let expected_file = open_in (Filename.remove_extension f ^ ".result") in
      let expected = input_line expected_file in
      try
        let result = string_of_expr (Interpreter.eval expr []) in
        if result <> expected then
          `Fail (`Eval, f, "  result: " ^ result ^ "\nexpected: " ^ expected)
        else `Pass (f, result)
      with InterpreterError msg ->
        `Fail (`Eval, f, "evaluator threw InterpreterError: " ^ msg ^ "\n"))

(* pretty-prints results *)
let print_result result =
  match result with
  | `Pass (f, _) -> print_string (f ^ "...pass\n")
  | `Fail (c, f, r) ->
      print_string
        (f ^ "..."
        ^ (match c with
          | `Parse -> "parse failed:\n"
          | `Eval -> "eval failed:\n")
        ^ r ^ "\n")

let any_failed results =
  List.exists
    (fun r -> match r with `Fail (_, _, _) -> true | `Pass (_, _) -> false)
    results

let () =
  let results =
    Sys.readdir "test" |> Array.to_list
    |> List.map (fun s -> "test/" ^ s)
    |> List.filter is_test |> List.map check_test
  in
  List.iter print_result results;
  if any_failed results then exit (-1) else ()
