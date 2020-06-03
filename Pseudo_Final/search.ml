type constant =
  | NUM of int
  | IDEN of string

type expr = 
  | VAR of string
  | CONST of constant
  | FUNC of string * expr list

type pair_expr = expr * expr

let rec string_of_expr expr =
  match expr with 
  | CONST(IDEN(x)) | VAR(x) -> x
  | CONST(NUM(x)) -> (string_of_int x)
  | FUNC(name,list1) -> name ^ "(" ^ (string_of_list list1) ^ ")"
and string_of_list list1 = 
match list1 with
| [] -> ""
| [h] -> (string_of_expr h)
| h :: t -> (string_of_expr h) ^ ", " ^ (string_of_list t)


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

(* let search_rules query rules =  *)


let query = FUNC("k",[VAR("Y")])

(* Variable mapping (Comes from unification)
Query variables <-> Head variables
Transfer the mapping onto the tail *)

let rec replace_var_expr orig var value = 
  match orig with 
  | VAR(x) -> if(x = var) then value else VAR(x)
  | CONST(x) -> CONST(x)
  | FUNC(x,y) -> FUNC(x,(replace_list y var value))
and replace_list list1 var value =
  match list1 with 
  | [] -> []
  | h::t -> 
    (replace_var_expr h var value) :: (replace_list t var value)
  
(* let rec solve_query query rules = 


and iter_unify query rules = 
  let var_exp = Hashtbl.create 1000 in
  match rules with 
  | [] -> false,var_exp
  | h::t -> 
    match h with 
    | HEAD(x) -> 

    |    
    let x,y = Unified.unifier query  *)

let testcases_replace = [
  FUNC("a",[FUNC("f",[VAR("Y")])]),"Y",CONST(NUM(1)) ;
  FUNC("a",[FUNC("f",[VAR("Y")])]),"Y",FUNC("g",[CONST(NUM(1))])  ;
]

let print_rule x = 
  Printf.printf "%s\n" ((string_of_rule x) ^ ".")

let test_all () = 
  List.iter print_rule rules

let tester_replace a =
  let x,y,z = a in
  Printf.printf "Expr:%s Var:%s Value:%s Final:%s\n" (string_of_expr x) y (string_of_expr z) (string_of_expr (replace_var_expr x y z))

let test_replace () = 
  List.iter tester_replace testcases_replace


(* let _  = test_all () *)
let _ = test_replace ()
