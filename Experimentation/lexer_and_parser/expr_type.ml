type constant =
  | NUM of int
  | IDEN of string

type expr = 
  | VAR of string
  | CONST of constant
  | FUNC of string * expr list

let rec string_of_expr expr =
    match expr with 
    	CONST(IDEN(x)) -> x
    |   CONST(NUM(x)) -> (string_of_int x)
    |   VAR(x) -> x
    |   FUNC(name,list1) -> name ^ "(" ^ (string_of_list list1) ^ ")"
and string_of_list list1 = 
  	match list1 with
  		[] -> ""
  	|   [h] -> (string_of_expr h)
  	|   h :: t -> (string_of_expr h) ^ ", " ^ (string_of_list t)

(* let _ = print_endline (string_of_exp "Hello") *)
