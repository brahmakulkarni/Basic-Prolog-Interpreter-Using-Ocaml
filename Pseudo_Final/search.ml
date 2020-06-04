open Unified 

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
  | OR(x,y) -> (List.append (right_to_list x) (right_to_list y))
  | AND(x,y) -> (List.append (right_to_list x) (right_to_list y))
  | NOT(x) -> (right_to_list x)

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

let rec string_list_of_rule rules = 
  match rules with 
  | [] -> ""
  | h :: t -> (string_of_rule h) ^ "\n" ^ (string_list_of_rule t)

let rules1 = [
  HEAD(FUNC("f",[CONST(IDEN("a"))])) ;
  HEAD(FUNC("f",[CONST(IDEN("b"))])) ;

  HEAD(FUNC("g",[CONST(IDEN("a"))])) ;
  HEAD(FUNC("g",[CONST(IDEN("b"))])) ;

  HEAD(FUNC("h",[CONST(IDEN("b"))])) ;

  NODE(FUNC("k",[VAR("X")]),AND(AND(LEAF(FUNC("f",[VAR("X")])),LEAF(FUNC("g",[VAR("X")]))),LEAF(FUNC("h",[VAR("X")])))) ;
]

let rules2 = [
  HEAD(FUNC("loves",[CONST(IDEN("vincent"));CONST(IDEN("mia"))])) ;
  HEAD(FUNC("loves",[CONST(IDEN("marcellus"));CONST(IDEN("mia"))])) ;

  NODE(FUNC("jealous",[VAR("A");VAR("B")]),AND(LEAF(FUNC("loves",[VAR("A");VAR("C")])),LEAF(FUNC("loves",[VAR("B");VAR("C")])))) ;
]

(* let search_rules query rules =  *)

let queries1 = [
  FUNC("k",[VAR("Y")]) ;
  FUNC("g",[CONST(IDEN("C"))]) ;
  VAR("Y") ;
]

let queries2 = [
  FUNC("jealous",[VAR("X");VAR("Y")]) ;
]

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
    if((Hashtbl.find_opt var_expr x) = None) then VAR(x) else (Hashtbl.find var_expr x)
  | CONST(x) -> CONST(x)
  | FUNC(x,y) -> FUNC(x,(replace_var_hash_list y var_expr))
and replace_var_hash_list list1 var_expr = 
  match list1 with
  | [] -> []
  | h::t -> 
    (replace_var_hash h var_expr) :: (replace_var_hash_list t var_expr)

let rec has_var expr = 
  match expr with 
  | VAR(x) -> true
  | CONST(x) -> false
  | FUNC(x,y) -> (has_list_var expr y)
and has_list_var expr list1 = 
  match list1 with
  | [] -> false
  | h :: t -> 
    if (has_var h) then true
    else (has_list_var expr t)


let rec search_rule_list query goal_list sub_rules rules = 
  Printf.printf "Query: %s Goals: %s\n" (string_of_expr query) (string_of_list goal_list);
  let empty_hash = Hashtbl.create 1000 in
  match sub_rules with 
  | [] -> false,empty_hash
  | h :: t ->
    match h with
    | HEAD(x) ->
      let flag,unified_hash = (Unified.unifier x query) in
      if (flag = false) then (search_rule_list query goal_list t rules) 
      else 
        let new_goal_list = (replace_var_hash_list goal_list unified_hash) in
        let flag_goal,goal_hash = (search_goal_list new_goal_list unified_hash rules rules) in
        if(flag_goal = false) then (search_rule_list query goal_list t rules)
        else flag_goal,goal_hash 
        
    | NODE(head,tail) ->
      let flag,unified_hash = (Unified.unifier head query) in
      if (flag = false) then (search_rule_list query goal_list t rules) 
      else 
        let flag_tail,tail_hash = (search_right tail empty_hash rules rules) in
        (* Printf.printf "Query: %s Head: %s Tail: %s flag_tail: %b\n" (string_of_expr query) (string_of_expr head) (string_of_right tail) flag_tail; *)
        if(flag_tail = false) then (search_rule_list query goal_list t rules)
        else
          let new_head = (replace_var_hash head tail_hash) in
          let new_flag,new_unified_hash = (Unified.unifier query new_head) in
          (* if(new_flag = false) then Printf.printf "DUMB SHITE ERROR. THIS SHOULD NEVER OCCUR\n"; *)
          (*new_flag is true because flag is true*)
          let new_goal_list = (replace_var_hash_list goal_list new_unified_hash) in
          let new_flag_goal, new_goal_hash = (search_goal_list new_goal_list new_unified_hash rules rules) in
          if(new_flag_goal = false) then (search_rule_list query goal_list t rules)
          else new_flag_goal,new_goal_hash 
          

and search_goal_list goal_list orig_hash sub_rules rules = 
  match goal_list with 
  | [] -> true,orig_hash
  | h :: t -> 
    let flag_tail,tail_hash = (search_rule_list h t rules rules) in
    (*Unionify the hash tables*)
    if(flag_tail = true) then 
    begin
      Hashtbl.iter (fun x y -> Hashtbl.add orig_hash x y) tail_hash;
      true,orig_hash
    end
    (*Doesn't really matter which hash_table we return*)
    else flag_tail,tail_hash 

and search_right right empty_hash sub_rules rules = 
  let goal_list = (right_to_list right) in
  search_goal_list goal_list empty_hash rules rules

let print_test query = 
  Printf.printf "Query: %s\n" (string_of_expr query);
  let flag,hash = (search_rule_list query [] rules1 rules1) in 
  Hashtbl.iter (fun x y -> if((has_var y)) then () else Printf.printf "%s = %s\n" x (string_of_expr y)) hash;
  Printf.printf "%b\n" flag;
  Printf.printf "\n\n-------------------------\n\n"

let test_rule_list () = 
  Printf.printf "RULES\n%s-------------------------\n\n" (string_list_of_rule rules1);
  List.iter print_test queries1
  


(* let testcases_replace = [
  FUNC("a",[FUNC("f",[VAR("Y")])]),"Y",CONST(NUM(1)) ;
  FUNC("a",[FUNC("f",[VAR("Y")])]),"Y",FUNC("g",[CONST(NUM(1))])  ;
] *)

(* let print_rule x = 
  Printf.printf "%s\n" ((string_of_rule x) ^ ".")

let test_all () = 
  List.iter print_rule rules

let tester_replace a =
  let x,y,z = a in
  Printf.printf "Expr:%s Var:%s Value:%s Final:%s\n" (string_of_expr x) y (string_of_expr z) (string_of_expr (replace_var_expr x y z))

let test_replace () = 
  List.iter tester_replace testcases_replace *)

(* let debug () = 
  let a = (Unified.unifier (VAR("X")) (VAR("X"))) in 
  let b,c = a in
  Printf.printf "Value: %b\n" b

let _ = debug () *)

(* let _  = test_all () *)
(* let _ = test_replace () *)
let _ = test_rule_list ()
