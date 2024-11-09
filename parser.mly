%token LAMBDA
%token DOT
%token LPAREN
%token RPAREN
%token <string> ID
%token EOF

%start <Interpreter.cst option> maybe_expression

%%

maybe_expression : term EOF { Some $1 }
                 | EOF { None }

term : application { $1 }
     | abstraction { $1 }

abstraction : LAMBDA ID DOT term { `Abstraction ($2, $4) }

variable : ID { `Variable $1 }

element : variable { $1 }
        | LPAREN term RPAREN { $2 }

application : application element { `Application ($1, $2) }
            | element { $1 }
