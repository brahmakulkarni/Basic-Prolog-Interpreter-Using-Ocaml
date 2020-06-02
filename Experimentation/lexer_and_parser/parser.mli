type token =
  | EOL
  | EOF
  | LPAREN
  | RPAREN
  | COMMA
  | EMPTY
  | VAR of (string)
  | IDEN of (string)
  | NUM of (int)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr_type.expr
