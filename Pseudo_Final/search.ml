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

let rec right_to_list right = 
  match right with 
  | LEAF(x) -> [x]
  | OR(x,y) -> (List.append x y)
  | AND(x,y) -> (List.append x y)
  | NOT(x) -> [x]

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

let rec replace_var_hash expr var_expr = 
  match expr with
  | VAR(x) -> 
    if((Hashtbl.find_opt x) = None) then VAR(x) else (Hashtbl.find x)
  | CONST(x) -> CONST(x)
  | FUNC(x,y) -> FUNC(x,(replace_var_hash_list y var_expr))
and replace_var_hash_list list1 var_expr = 
  match list1 with
  | [] -> []
  | h::t -> 
    (replace_var_hash h) :: (replace_var_hash_list t var_expr)

let rec has_var expr = 
  match expr with 
  | VAR(x) -> true
  | CONST(x) -> false
  | FUNC(x,y) -> (has_list_var expr y)
and has_list_var expr list1 = 
  match list1 with
  | [] -> false
  | h :: t -> 
    if (has_var h) true
    else (has_list_var expr t)



let search_rule_list query goal_list rules = 
  let search_hash = Hashtbl.create 1000 in
  match rules with 
  | [] -> false,search_hash
  | h :: t ->
    match h with
    | HEAD(x) ->
      let flag,unified_hash = (Unified.unifier x query) in
      if (flag = false) then (search_list query t) 
      else 
        let new_goal_list = (replace_var_hash_list goal_list unified_hash) in
        (search_goal_list new_goal_list rules )
        
      (* else flag,unified_hash *)
    | NODE(x,y) ->
      let flag,unified_hash = (Unified.unifier x query) in
      if (flag = false) then (search_list query t) 
      else 
        let flag_tail,tail_hash = (search_right right rules) in
        if(flag_tail = false) then (search_list query t)
        else
          let new_head = (replace_var_hash x tail_hash) in
          let new_flag,new_unified_hash = (Unified.unifier query new_head) in
          if(new_flag = false) then Printf.printf "DUMB SHITE ERROR. THIS SHOULD NEVER OCCUR\n";
          new_flag,new_unified_hash

and search_goal_list goal_list rules = 
  match goal_list with 
  | [] -> 
  | h :: t -> (search_rule_list h t rules)
and search_right right rules = 
  let goal_list = (right_to_list right) in
  search_goal_list goal_list rules

(* search_list query rules 
seasrc *)
(* let rec search_tail right rules = 
  match rules with
   *)
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
