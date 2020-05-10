/* Tokens for add-sub */
%token <string> ID
%token <int64> INT64
%token LET
%token IN
%token LPAREN RPAREN
%token ADD1 SUB1
%token ASSING
%token EOF
%token EOL
%nonassoc UMINUS        /* highest precedence */

%start <Syntax.expr> main
%{ open Syntax %}
%%

main:
|e = expr EOF  { e }
;

expr:
| i = INT64 { Num i }
| id = ID { Id id }
| LPAREN e = expr RPAREN { e }
| ADD1 e = expr { Add1 e }
| SUB1 e = expr { Sub1 e }
| LET id = ID ASSING e1 = expr IN e2 = expr {Let (id,e1,e2)}
;