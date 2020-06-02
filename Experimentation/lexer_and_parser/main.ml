let tss = [
  "1"        ;
  "hello"    ;
  "X";
  "man(brahma)" ; (* <error> *)
  "loves(rathin,ram)"    ;
]

let test_parser s =
  try
    let lexbuf = Lexing.from_string s in
    let result = (Parser.main Lexer.scan lexbuf) in
    Printf.printf "%s\n" (Expr_type.string_of_expr result)
  with Parsing.Parse_error ->
    Printf.printf "%s -> false\n" s


let test_all () =
  List.iter test_parser tss

let _ = test_all ()
