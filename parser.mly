%token LAMBDA
%token DOT
%token LPAREN
%token RPAREN
%token EQ
%token IN
%token LET
%token <string> ID
%token EOF

%start <Interpreter.cst option> maybe_expression
%start <Interpreter.cst> complete_expression

%%

maybe_expression : expression EOF { Some $1 }
                 | EOF        { None }

complete_expression : expression EOF { $1 }

expression : declaration { $1 }
           | term { $1 }

declaration : LET ID EQ term IN expression { `Declaration ($2, $4, $6)}

term : application { $1 }
     | abstraction { $1 }

abstraction : LAMBDA ID DOT term { `Abstraction ($2, $4) }

variable : ID { `Variable $1 }

element : variable { $1 }
        | LPAREN term RPAREN { $2 }

application : application element { `Application ($1, $2) }
            | element { $1 }
