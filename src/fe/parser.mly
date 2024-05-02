%{
open Lang.L1lang
open Lang.L1type
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
%token EUL
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

%token JA
%token TILDE

%token MUKM

%token IF
%token ELSE

%token QUESTION
%token QUESQUES
%token EXCLAMATION
%token EXCLAMEXCLAM

%token SKIVIA
%token INTTYPE
%token ILKA
%token ANILKA
%token ARROW
%token VECTOR
%token JOYGO
%token MTE
%token YEOREOBUN
%token NEUN
%token ILKAYO

%token EOF

%left COMMA
%nonassoc ELSE
%nonassoc AH
%left ARROW
%left JOYGO

%start <l1expr> prog

%%
prog:
  | t = toplevel* CHUNJAT EXCLAMATION LBRACE e = expr RBRACE EOF 
    { toplevel_join t e }
  ;
  
toplevel:
  | LET1 JAGIGA pat = let_pattern IRANUN SARAMINDE body = expr eul HETE
    { E.let' pat body }
  | IN fn_name = ID JUNGENEUN EXCLAMEXCLAM AMURI pat = arg_pattern IRADO ret_t = type_expr 
    LBRACE body = expr RBRACE HAL SUGA UPDANDA EXCLAMEXCLAM
    { E.let_rec fn_name pat body ret_t }
  | YEOREOBUN name = ID NEUN defs = variant_def* QUESQUES
    { E.variant_def name defs }
  ;
  
eul:
  | EUL { () }
  | REUL EXCLAMATION? { () }
  ;
  
variant_def:
  | cons_name = ID ILKAYO { cons_name, TE.unit }
  | cons_name = ID COLON tinfo = type_expr ILKAYO { cons_name, tinfo }
  ;
  
let_pattern:
  | UNDERSCORE { Lp.underscore }
  | id = var_id { Lp.var_id id None }
  | id = var_id COLON te = type_expr { Lp.var_id id (Some te) }
  | LPAREN l = separated_nonempty_list(COMMA, let_pattern) RPAREN
    { Lp.tuple l }
  ;
  
var_id:
  | x = ID { x }
  | MTE { "\u{bb49}\u{d0f1}\u{c774}" }
  
arg_pattern:
  | arg_id = ID COLON te = type_expr { Ap.var_id arg_id te }
  ;
  
expr:
  | e = aexpr { e }
  | e = aexpr SEMICOLON { E.seqn e E.unit }
  | e1 = aexpr SEMICOLON e2 = expr { E.seqn e1 e2 }
  | hd = exprexpr SEMICOLON e = expr { hd e }
  ;

exprexpr: (* expr -> expr *)
  | JA TILDE pat = let_pattern REUL EXCLAMATION body = aexpr
    { E.let' pat body }
  ;
  
aexpr: (* atomic expr *)
  | i = INT { E.int i }
  | x = var_id { E.id x }
  | SKIVIA { E.unit }
  | TRUE { E.bool true }
  | FALSE { E.bool false }
  | IF QUESTION LPAREN flag = aexpr RPAREN t = aexpr ELSE f = aexpr 
    { E.if' flag t f }
  | LPAREN e = aexpr RPAREN { e }
  | CHUNJAT EXCLAMATION LBRACE e = expr RBRACE { e }
  | f = aexpr AH arg = aexpr MEOGEORA QUESQUES (* apply *)
    { E.apply f arg }
  | lhs = aexpr JOYGO rhs = aexpr { Op.mul lhs rhs } (* mult *)
  | MUKM EXCLAMEXCLAM LPAREN l = separated_nonempty_list(COMMA, aexpr) RPAREN (* tuple *)
    { E.tuple l }
  ;

type_expr:
  | x = ID { TE.typeid x }
  | SKIVIA { TE.unit }
  | INTTYPE { TE.int }
  | ILKA ANILKA QUESQUES { TE.bool }
  | t1 = type_expr ARROW t2 = type_expr { TE.fn t1 t2 }
  | LPAREN t = type_expr RPAREN { t }
  | t = type_expr_tuple { TupleBuilder.rev t }
  | MTE LBRACE r = record_fields RBRACE { TE.record r }
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