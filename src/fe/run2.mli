exception ParserError of string
exception LexerError of string

val parse : string -> L1lang.l1expr
val typecheck : string -> unit
val compile : string -> Expr.expr
val run : string -> Expr.value
