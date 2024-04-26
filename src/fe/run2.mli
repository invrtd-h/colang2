exception ParserError of string
exception LexerError of string

val parse : string -> Lang.L1lang.l1expr
val typecheck : string -> unit
val compile : string -> Lang.Expr.expr
val run : string -> Lang.Expr.value
