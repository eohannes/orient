open Core

let exec_file f =
    let parse = Lexer.parse_file f in
        match parse with
            | Some expr ->
                print_string (Interpreter.string_of_expr expr);
                Out_channel.newline stdout;
                Some (Interpreter.eval expr [("x",`Var "y")])
            | None -> None

let exec_and_print f () =
    match exec_file f with
        | Some result -> print_string ("==> " ^ (Interpreter.string_of_expr result)); Out_channel.newline stdout
        | None -> ()

let () =
    Command.basic_spec ~summary: "Execute a file"
    Command.Spec.(empty +> anon ("filename" %: string))
    exec_and_print
    |> Command.run