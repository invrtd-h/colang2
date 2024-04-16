%{
open L1lang
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE
%token LEQ
%token TIMES
%token PLUS
%token LPAREN
%token RPAREN
%token LET
%token EQUALS
%token IN
%token IF
%token THEN
%token ELSE
%token EOF

%nonassoc IN
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES

%start <L1lang.ast_node> prog

%%
prog:
  | e = expr; EOF { e }
  ;
  
expr:
  | i = INT { IntE i |> fresh_node }
  | x = ID { Id x |> fresh_node }
  | TRUE { BoolE true |> fresh_node }
  | FALSE { BoolE false |> fresh_node }
  | IF e1 = expr THEN e2 = expr ELSE e3 = expr { IfE (e1, e2, e3) |> fresh_node }
  | LPAREN e = expr RPAREN { e }
  ;
  
