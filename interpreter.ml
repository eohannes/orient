exception InterpreterError of string

(* CST type *)
type cst =
  [ `Declaration of string * cst * cst
  | `Abstraction of string * cst
  | `Application of cst * cst
  | `Variable of string ]

let rec string_of_expr (c : cst) =
  match c with
  | `Declaration (var, binding, body) ->
      var ^ " = " ^ string_of_expr binding ^ " in " ^ string_of_expr body
  | `Abstraction (var, body) -> "\\" ^ var ^ ". " ^ string_of_expr body
  | `Application (lhs, rhs) ->
      "(" ^ string_of_expr lhs ^ ") (" ^ string_of_expr rhs ^ ")"
  | `Variable var -> var

let string_of_env (env : (string * cst) list) =
  if List.length env = 0 then "<empty>"
  else
    List.fold_left
      (fun acc (var, expr) -> acc ^ var ^ " -> " ^ string_of_expr expr ^ "\n")
      "" env

let rec eval (e : cst) (env : (string * cst) list) =
  match e with
  | `Declaration (v, binding, body) ->
      let newEnv = (v, eval binding env) :: env in
      eval body newEnv
  | `Application (m, n) -> (
      let lhs = eval m env and rhs = eval n env in
      match lhs with
      | `Abstraction (v, body) ->
          let newEnv = (v, rhs) :: env in
          eval body newEnv
      | _ -> `Application (lhs, rhs))
  | `Abstraction (v, body) -> `Abstraction (v, eval body env)
  | `Variable var -> (
      match List.assoc_opt var env with Some v -> v | None -> e)
