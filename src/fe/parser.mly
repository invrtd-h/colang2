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
%token LBRACK
%token RBRACK

%token UNDERSCORE

%token CHUNJAT

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
%token SEMICOLON
%token COMMA

%token AH
%token MEOGEORA

%token IF
%token ELSE

%token QUESTION
%token QUESQUES
%token EXCLAMATION
%token EXCLAMEXCLAM

%token SKIVIA
%token INTTYPE
%token BOOLTYPE
%token ARROW
%token VECTOR
%token JOYGO
%token MTE

%token EOF

%nonassoc ELSE
%nonassoc AH
%left ARROW
%left JOYGO

%start <L1lang.ast_node> prog

%%
prog:
  | t = toplevel* CHUNJAT EXCLAMATION e = expr EOF { toplevel_join t e }
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
  | id = var_id { var_id_lp id None }
  | id = var_id COLON te = type_expr { var_id_lp id (Some te) }
  | LPAREN l = let_pattern RPAREN { l }
  ;
  
var_id:
  | x = ID { x }
  | MTE { "\u{bb49}\u{d0f1}\u{c774}" }
  
arg_pattern:
  | arg_id = ID COLON te = type_expr { var_id_ap arg_id te }
  ;
  
expr:
  | i = INT { int_e i |> make ($startpos, $endpos) }
  | x = var_id { id_e x |> make ($startpos, $endpos) }
  | SKIVIA { unit_e |> make ($startpos, $endpos) }
  | TRUE { bool_e true |> make ($startpos, $endpos) }
  | FALSE { bool_e false |> make ($startpos, $endpos) }
  | IF QUESTION LPAREN flag = expr RPAREN t = expr ELSE f = expr 
    { if_e flag t f |> make ($startpos, $endpos) }
  | LPAREN e = expr RPAREN { e }
  | f = expr AH arg = expr MEOGEORA QUESQUES
    { apply_e f arg |> make ($startpos, $endpos)}
  ;

type_expr:
  | x = ID { typeid_te x }
  | SKIVIA { unit_te }
  | INTTYPE { int_te }
  | BOOLTYPE { bool_te }
  | t1 = type_expr ARROW t2 = type_expr { fun_te t1 t2 }
  | LPAREN t = type_expr RPAREN { t }
  | t = type_expr_tuple { TupleBuilder.rev t }
  | MTE LBRACE r = record_fields RBRACE { record_te r }
  ;
  
type_expr_tuple:
  | tl = type_expr JOYGO tr = type_expr { TupleBuilder.join_rev tl tr }
  ;
  
record_fields:
  | l = record_field { [l] }
  | l = record_field SEMICOLON { [l] }
  | l = record_field SEMICOLON r = record_fields { l :: r }
  ;
  
record_field:
  | name = ID COLON te = type_expr { name, te }