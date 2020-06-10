%token LAMBDA
%token DOT
%token LPAREN
%token RPAREN
%token <string> ID
%token EOF

%start <Interpreter.expression option> prog
%%

prog: EOF      { None }
    | expr EOF { Some $1 }
    ;

expr: ID                 { `Var $1 }               (* a variable *)
    | expr expr          { `App ($1, $2) }         (* an application *)
    | LAMBDA ID DOT expr { `Lambda (`Var $2, $4) } (* a lambda *)
    | LPAREN expr RPAREN { $2 }                    (* a parenthesized expr *)
    ;