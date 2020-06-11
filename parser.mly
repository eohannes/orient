%token LAMBDA
%token DOT
%token LPAREN
%token RPAREN
%token <string> ID
%token EOF

%start <Interpreter.expression option> prog
%%

prog: EOF      { None }
    | expr EOF { Some  $1 }
    ;

expr: LAMBDA ID DOT expr { `Lambda (`Var $2, $4) }
    | appl               { $1 }

appl: appl item { `App ($1, $2) }
    | item      { $1 }

item: ID                 { `Var $1 }
    | LPAREN expr RPAREN { $2 }