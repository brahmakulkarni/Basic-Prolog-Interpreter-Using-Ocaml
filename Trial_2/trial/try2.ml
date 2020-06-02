type constant =
  | NUM of int
  | IDEN of string

type expr = 
  | VAR of string
  | CONST of constant
  | FUNC of string * expr list

type pair_expr = expr * expr

type rule = 
  | LEAF of expr
  | NODE of expr*expr

let a = (Try.unifier (VAR("X")) (VAR("X"))) in a
(* let b,c = a in
Printf.printf "Value: %b\n" b *)
