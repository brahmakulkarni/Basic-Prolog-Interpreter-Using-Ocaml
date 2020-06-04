type token =
  | EOL
  | EOF
  | LPAREN
  | RPAREN
  | COMMA
  | EMPTY
  | OR
  | NOT
  | IMPLIES
  | VAR of (string)
  | IDEN of (string)
  | NUM of (int)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Expr_type.rule