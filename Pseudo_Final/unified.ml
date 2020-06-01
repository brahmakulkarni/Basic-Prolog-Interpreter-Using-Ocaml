type constant =
  | NUM of int
  | IDEN of string

type expr = 
  | VAR of string
  | CONST of constant
  | FUNC of string * expr list

type pair_expr = expr * expr

let tests = [
  CONST ( IDEN ( "mia" ) ) , CONST ( IDEN ( "mia" ) ) ;
  CONST ( IDEN ( "mia" ) ) , CONST ( IDEN ( "vincent" ) ) ;
  VAR ( "X" ) , VAR ( "Y" ) ;
  VAR ( "X" ) , CONST ( IDEN ( "mia" ) ) ;
  FUNC ("f",[VAR ("X");VAR("Y")]) , VAR ("X") ;
  (* vertical(line(point(1,1),point(1,3)))
  vertical(line(point(X,Y),point(X,Z))) *)
  FUNC ("vertical",[FUNC("line",[FUNC("point",[CONST(NUM(1));CONST(NUM(1))]);FUNC("point",[CONST(NUM(1));CONST(NUM(3))]) ]) ]),
  FUNC ("vertical",[FUNC("line",[FUNC("point",[VAR("X");VAR("Y")]);FUNC("point",[VAR("X");VAR("Z")])])]) ;
  FUNC ("vertical",[FUNC("line",[FUNC("point",[VAR("X");VAR("Y")]);FUNC("point",[VAR("X");VAR("Z")])])]),
  FUNC ("vertical",[FUNC("line",[FUNC("point",[CONST(NUM(1));CONST(NUM(1))]);FUNC("point",[CONST(NUM(2));CONST(NUM(3))]) ]) ]) ;

  FUNC("food",[CONST(IDEN("bread"));VAR("X")]),
  FUNC("food",[VAR("Y");CONST(IDEN("sausage"))]);

  FUNC("food",[CONST(IDEN("bread"));VAR("X")]),
  FUNC("food",[VAR("Y");CONST(IDEN("bread"))]);

  FUNC("meal",[FUNC("food",[FUNC("f1",[FUNC("f2",[FUNC("f3",[VAR("Y")])])])]);VAR("X")]),
  FUNC("meal",[VAR("X");FUNC("food",[FUNC("f1",[FUNC("f2",[FUNC("f3",[VAR("Y")])])])])]) ; 

  FUNC("meal",[FUNC("food",[FUNC("f1",[FUNC("f2",[FUNC("f3",[CONST(NUM(100))])])])]);VAR("X")]),
  FUNC("meal",[VAR("X");FUNC("food",[FUNC("f1",[FUNC("f2",[FUNC("f3",[CONST(NUM(10))])])])])]) ; 
]

let rec string_of_expr expr =
    match expr with 
    CONST(IDEN(x)) -> x
    | CONST(NUM(x)) -> (string_of_int x)
    | VAR(x) -> x
    | FUNC(name,list1) -> name ^ "(" ^ (string_of_list list1) ^ ")"
and string_of_list list1 = 
  match list1 with
  [] -> ""
  | [h] -> (string_of_expr h)
  | h :: t -> (string_of_expr h) ^ ", " ^ (string_of_list t)


let matcher s t = 
  let var_exp = Hashtbl.create 1000 in
  let rec rec_match s t = 
    (* Printf.printf "Debug: %s and %s\n" (string_of_expr s) (string_of_expr t); *)
    match (s,t) with 
    CONST(a),CONST(b) -> a = b
    | VAR(x) , value -> 
        let prev_instant = Hashtbl.find_opt var_exp x in
        if(prev_instant = None) then 
        begin
          Hashtbl.add var_exp x value;
          true
        end
        else
        begin
          (* Printf.printf "Debug: x: %s %s %s\n" x (string_of_expr value) (string_of_expr (Hashtbl.find var_exp x)); *)
          let hash_value = (Hashtbl.find var_exp x) in
          if (value = hash_value) then true
          else false
        end
    | value , VAR(x) ->
      begin
        let prev_instant = Hashtbl.find_opt var_exp x in
        if(prev_instant = None) then 
        begin
          Hashtbl.add var_exp x value;
          true
        end
        (* else if (value = (Hashtbl.find var_exp x)) then true
        else false *)
        else
        begin
          let hash_value = (Hashtbl.find var_exp x) in
          if (value = hash_value) then true
          else false
        end
      end
    | FUNC(f1,list1), FUNC(f2,list2) ->
    begin

      if(((compare f1 f2)!= 0) || ((List.length list1) != (List.length list2))) then false
      else
      begin
        (* Printf.printf "Debug: here\n"; *)
        let flag = ref true in 
        let flag_true = ref true in 
        let rec compute_list l m = 
          match l,m with
          [],[] -> ()
          | [],_ -> ()
          | _,[] -> ()
          | h1::t1,h2::t2 -> 
          begin
            (* Printf.printf "Debug_list: %s %s\n" (string_of_expr h1) (string_of_expr h2); *)
            flag := !flag&&(rec_match h1 h2);
            compute_list t1 t2
          end
        in compute_list list1 list2;
        if(flag = flag_true) then true
        else false 
      end
    end
    | _ -> false
  in 
  let flag = rec_match s t in
  Printf.printf "?- %s = %s\n" (string_of_expr s) (string_of_expr t);  
  (* Printf.printf "Hashtable size: %d\n" (Hashtbl.length var_exp); *)
  if(flag) then
  begin
    Hashtbl.iter (fun x y -> Printf.printf "%s = %s\n" x (string_of_expr y)) var_exp ;
    Printf.printf "true\n\n"
  end
  else Printf.printf "false\n\n"

let match_pair a = match a with x,y -> matcher x y;;

let test_all () = 
  List.iter match_pair tests 

let _ = test_all ()
(* 
let d = Hashtbl.create 1000 in 
let rec f counter  = 
  if(counter > 5) then ()
  else
  begin
    Hashtbl.add d counter (counter+1);
    f (counter+1)
  end
in f 0;
Hashtbl.iter (fun x y -> Printf.printf "%d -> %d \n" x y) d *)
