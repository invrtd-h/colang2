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

%token LET1
%token JAGIGA
%token RANUN
%token IRANUN
%token SARAMINDE
%token EQUALS
%token HETE

%token EUL
%token REUL

%token IF
%token THEN
%token ELSE

%token QUESTION
%token EXCLAMATION

%token EOF

%nonassoc HETE
%nonassoc ELSE
%left LEQ
%left PLUS
%left TIMES

%start <L1lang.ast_node> prog

%%
prog:
  | e = expr EOF { e }
  ;
  
let_expr:
  | LET1 JAGIGA x = ID RANUN SARAMINDE body = expr EUL HETE next = expr
    { let_untyped x body next |> make ($startpos, $endpos) }
  | LET1 JAGIGA x = ID RANUN SARAMINDE body = expr REUL HETE next = expr
    { let_untyped x body next |> make ($startpos, $endpos) }
  | LET1 JAGIGA x = ID IRANUN SARAMINDE body = expr EUL HETE next = expr
    { let_untyped x body next |> make ($startpos, $endpos) }
  | LET1 JAGIGA x = ID IRANUN SARAMINDE body = expr REUL HETE next = expr
    { let_untyped x body next |> make ($startpos, $endpos) }
  
expr:
  | i = INT { int_e i |> make ($startpos, $endpos) }
  | x = ID { id_e x |> make ($startpos, $endpos) }
  | TRUE { bool_e true |> make ($startpos, $endpos) }
  | FALSE { bool_e false |> make ($startpos, $endpos) }
  | IF QUESTION LPAREN flag = expr RPAREN t = expr ELSE f = expr 
    { IfE (flag, t, f) |> make ($startpos, $endpos) }
  | LPAREN e = expr RPAREN { e }
  | e = let_expr { e }
  ;
  
