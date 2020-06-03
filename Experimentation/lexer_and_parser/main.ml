open Printf

let extract_filename s =
	let len = String.length s in  
        let extract_within = String.sub s 1 (len-2) in
        let filename = (extract_within ^ ".pl") in
	filename

let load_file s =
	let input_file = open_in s in 
	let file_content = really_input_string input_file (in_channel_length input_file) in file_content


let tss = [
  "1"        ; 
  "hello"    ;
  "X";
  "man(brahma)" ; (* <error> *)
  "loves(rathin,ram)"    ;
  "vertical(line(point(1, 2), point(1, 3)))";
  "jealous(X,Y) :- loves(X,Z), loves(Y,Z)";
  "brahma :- kulkarni";
  "happy(X) :- !sad(X)";
]

let test_parser s =
  try
    let lexbuf = Lexing.from_string s in
    let result = (Parser.rule Lexer.scan lexbuf) in
    Printf.printf "%s\n" (Expr_type.string_of_rule result)
  with Parsing.Parse_error ->
    Printf.printf "%s -> false\n" s


let test_all () =
  List.iter test_parser tss

let test_file () =
  let input = print_string "> ";read_line () in 
  let filename = extract_filename input in 
  let readcontent = load_file filename in 
  let test_list = String.split_on_char '\n' readcontent in 
  List.iter print_endline test_list; 
  print_endline " ";
  List.iter test_parser test_list

(* let _ = test_all () *)
let _ = test_file ()
