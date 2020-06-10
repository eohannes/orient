exception InterpreterError of string

(* AST type *)
type expression = [
    | `App of expression * expression
    | `Lambda of expression * expression
    | `Var of string
]

type environment = (string * expression) list

(* whatever the ugly version of pretty-printing is called *)
let rec string_of_expr (e : expression) =
    match e with
        | `App (lhs, rhs) -> "App (" ^ string_of_expr lhs ^ ") (" ^ string_of_expr rhs ^ ")"
        | `Lambda (var, body) -> "Lambda (" ^ string_of_expr var ^ ") (" ^ string_of_expr body ^ ")" 
        | `Var var -> "Var (" ^ var ^ ")"


(* evaluator *)
let rec eval (e : expression) (env : environment) =
    match e with
        (* apply a lambda to an argument *)
        | `App ((`Lambda (`Var var, body)), rhs) ->
            let newEnv = (var, (eval rhs env)) :: env in
                eval body newEnv
        (* apply other things to an argument *)
        | `App (lhs, rhs) -> eval (`App (eval lhs env, eval rhs env)) env
        (* a lambda *)
        | `Lambda (`Var var, body) -> `Lambda (`Var var, body)
        (* substitute for a variable if a binding exists *)
        | `Var var ->
                (match (List.assoc_opt var env) with
                    | Some v -> v 
                    | None -> `Var var)
        (* anything else is an invalid expression *)
        | _ -> raise (InterpreterError "bad lambda expr")