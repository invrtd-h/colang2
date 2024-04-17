%{
open L1lang
%}

%token <int> INT
%token <string> ID
%token TRUE
%token FALSE

%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE

%token UNDERSCORE

%token LET1
%token JAGIGA
%token IRANUN
%token SARAMINDE
%token HETE
%token REUL

%token IN
%token JUNGENEUN
%token AMURI
%token IRADO
%token HAL
%token SUGA
%token UPDANDA

%token COLON

%token AH
%token MEOGEORA

%token IF
%token ELSE

%token QUESTION
%token QUESQUES
%token EXCLAMATION
%token EXCLAMEXCLAM

%token INTTYPE
%token BOOLTYPE

%token EOF

%nonassoc ELSE
%nonassoc AH

%start <L1lang.ast_node> prog

%%
prog:
  | t = toplevel* EOF { toplevel_join t }
  ;
  
toplevel:
  | LET1 JAGIGA pat = let_pattern IRANUN SARAMINDE body = expr REUL EXCLAMATION? HETE
    { fun next ->
      let_e pat body next |> make ($startpos, $endpos) }
  | IN fn_name = ID JUNGENEUN EXCLAMEXCLAM AMURI pat = arg_pattern IRADO ret_t = type_expr 
    LBRACE body = expr RBRACE HAL SUGA UPDANDA EXCLAMEXCLAM
    { fun next ->
      let_rec_e fn_name pat body ret_t next |> make ($startpos, $endpos) }
  ;
  
let_pattern:
  | UNDERSCORE { underscore_lp }
  | var_id = ID { var_id_lp var_id None }
  | var_id = ID COLON te = type_expr { var_id_lp var_id (Some te) }
  | LPAREN l = let_pattern RPAREN { l }
  ;
  
arg_pattern:
  | arg_id = ID COLON te = type_expr { var_id_ap arg_id te }
  ;
  
expr:
  | i = INT { int_e i |> make ($startpos, $endpos) }
  | x = ID { id_e x |> make ($startpos, $endpos) }
  | TRUE { bool_e true |> make ($startpos, $endpos) }
  | FALSE { bool_e false |> make ($startpos, $endpos) }
  | IF QUESTION LPAREN flag = expr RPAREN t = expr ELSE f = expr 
    { if_e flag t f |> make ($startpos, $endpos) }
  | LPAREN e = expr RPAREN { e }
  | f = expr AH arg = expr MEOGEORA QUESQUES %prec AH
    { apply_e f arg |> make ($startpos, $endpos)}
  ;

type_expr:
  | INTTYPE { int_te }
  | BOOLTYPE { bool_te }