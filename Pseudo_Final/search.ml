(* type constant =
  | NUM of int
  | IDEN of string

type Unified.expr = 
  | VAR of string
  | CONST of constant
  | FUNC of string * Unified.expr list

type pair_expr = Unified.expr * Unified.expr *)

type right = 
  | LEAF of Unified.expr
  | OR of right * right
  | AND of right * right
  | NOT of right

type rule = 
  | HEAD of Unified.expr
  | NODE of Unified.expr * right

let rec string_of_right right = 
  match right with
  | LEAF(x) -> (Unified.string_of_expr x)
  | OR(x,y) -> (string_of_right x) ^ " | " ^ (string_of_right y)
  | AND(x,y) -> (string_of_right x) ^ " , " ^ (string_of_right y)
  | NOT(x) -> "!"^(string_of_right x) ^ " "

let rec string_of_rule rule = 
  match rule with
  | HEAD(x) -> (Unified.string_of_expr x)
  | NODE(x,y) ->
    (Unified.string_of_expr x) ^ " :- " ^ (string_of_right y) 

let debug () = 
  let a = (Unified.unifier (VAR("X")) (VAR("X"))) in 
  let b,c = a in
  Printf.printf "Value: %b\n" b

let _ = debug ()

let rules = [
  HEAD(FUNC("f",[CONST(IDEN("a"))])) ;
  HEAD(FUNC("f",[CONST(IDEN("b"))])) ;

  HEAD(FUNC("g",[CONST(IDEN("a"))])) ;
  HEAD(FUNC("f",[CONST(IDEN("b"))])) ;

  HEAD(FUNC("h",[CONST(IDEN("b"))])) ;

  NODE(FUNC("h",[VAR("X")]),AND(AND(LEAF(FUNC("f",[VAR("X")])),LEAF(FUNC("g",[VAR("X")]))),LEAF(FUNC("h",[VAR("X")])))) ;
]

let query = FUNC("k",[VAR("Y")])


let print_rule x = 
  Printf.printf "%s\n" ((string_of_rule x) ^ ".")

let test_all () = 
  List.iter print_rule rules


let _  = test_all ()
