let tss = [
  "1"        ;
  "hello"    ;
  "X";
  "man(brahma)" ; (* <error> *)
  "loves(rathin,ram)"    ;
  "vertical(line(point(1, 2), point(1, 3)))";
  "jealous(X,Y) :- loves(X,Z), loves(Y,Z)"
]

let test_parser s =
  try
    let lexbuf = Lexing.from_string s in
    let result = (Parser.main Lexer.scan lexbuf) in
    Printf.printf "%s\n" (Expr_type.string_of_rule result)
  with Parsing.Parse_error ->
    Printf.printf "%s -> false\n" s


let test_all () =
  List.iter test_parser tss

let _ = test_all ()
