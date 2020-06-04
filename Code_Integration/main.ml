open Printf
open Expr_type

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

let global_arr = []

let test_parser s =
  try
    let lexbuf = Lexing.from_string s in
    let result = (Parser.rule Lexer.scan lexbuf) in
    Printf.printf "%s\n" (Expr_type.string_of_rule result)
  with Parsing.Parse_error ->
    Printf.printf "%s -> false\n" s

let rec make_list_of_rules arr =
	match arr with
		[] -> []
	|	h::t ->
		begin
			let lexbuf = Lexing.from_string h in
			let result = (Parser.rule Lexer.scan lexbuf) in
			let _ = result::global_arr in
			make_list_of_rules t
		end 

let test_all () =
  List.iter test_parser tss

let query = FUNC("k", [VAR("Y")])

let test_file () =
  let input = print_string "> ";read_line () in 
  let filename = extract_filename input in 
  let readcontent = load_file filename in 
  let test_list = String.split_on_char '\n' readcontent in 
  let rule_list = List.rev (make_list_of_rules test_list) in
  let flag, hash = (Search.search_rule_list query [] rule_list rule_list) in
  Hashtbl.iter (fun x y -> if((Search.has_var y)) then () else Printf.printf "%s = %s\n" x (string_of_expr y)) hash;
  Printf.printf "%b\n" flag
(*  List.iter print_endline test_list; 
  print_endline " ";
  List.iter test_parser test_list *)

(* let _ = test_all () *)
let _ = test_file ()
