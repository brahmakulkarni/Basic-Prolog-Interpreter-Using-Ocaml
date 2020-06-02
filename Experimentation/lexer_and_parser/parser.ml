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

open Parsing;;
let _ = parse_error;;
# 2 "parser.mly"
open Expr_type
# 17 "parser.ml"
let yytransl_const = [|
  257 (* EOL *);
    0 (* EOF *);
  258 (* LPAREN *);
  259 (* RPAREN *);
  260 (* COMMA *);
  261 (* EMPTY *);
    0|]

let yytransl_block = [|
  262 (* VAR *);
  263 (* IDEN *);
  264 (* NUM *);
    0|]

let yylhs = "\255\255\
\001\000\003\000\003\000\002\000\002\000\002\000\004\000\004\000\
\000\000"

let yylen = "\002\000\
\002\000\001\000\001\000\001\000\001\000\004\000\001\000\003\000\
\002\000"

let yydefred = "\000\000\
\000\000\000\000\004\000\000\000\002\000\009\000\000\000\005\000\
\000\000\001\000\000\000\000\000\000\000\006\000\008\000"

let yydgoto = "\002\000\
\006\000\011\000\008\000\012\000"

let yysindex = "\006\000\
\254\254\000\000\000\000\006\255\000\000\000\000\000\255\000\000\
\254\254\000\000\005\255\007\255\254\254\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\255\254\000\000\000\000\000\000\000\000\
\000\000\000\000\008\255\000\000\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\011\000\000\000\001\000"

let yytablesize = 14
let yytable = "\003\000\
\010\000\003\000\003\000\003\000\004\000\005\000\001\000\009\000\
\013\000\014\000\007\000\007\000\000\000\015\000"

let yycheck = "\001\001\
\001\001\003\001\004\001\006\001\007\001\008\001\001\000\002\001\
\004\001\003\001\003\001\001\000\255\255\013\000"

let yynames_const = "\
  EOL\000\
  EOF\000\
  LPAREN\000\
  RPAREN\000\
  COMMA\000\
  EMPTY\000\
  "

let yynames_block = "\
  VAR\000\
  IDEN\000\
  NUM\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'expr) in
    Obj.repr(
# 16 "parser.mly"
                  ( _1						)
# 90 "parser.ml"
               : Expr_type.expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 19 "parser.mly"
              ( CONST(NUM(_1)) 			)
# 97 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 20 "parser.mly"
               ( CONST(IDEN(_1)) 			)
# 104 "parser.ml"
               : 'const))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 23 "parser.mly"
              ( VAR(_1)					)
# 111 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'const) in
    Obj.repr(
# 24 "parser.mly"
                (_1)
# 118 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 3 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 1 : 'expr_list) in
    Obj.repr(
# 25 "parser.mly"
                                 ( FUNC(_1, _3) 				)
# 126 "parser.ml"
               : 'expr))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'expr) in
    Obj.repr(
# 28 "parser.mly"
               ( [_1]						)
# 133 "parser.ml"
               : 'expr_list))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'expr) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'expr_list) in
    Obj.repr(
# 29 "parser.mly"
                            ( _1 :: _3				)
# 141 "parser.ml"
               : 'expr_list))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Expr_type.expr)
