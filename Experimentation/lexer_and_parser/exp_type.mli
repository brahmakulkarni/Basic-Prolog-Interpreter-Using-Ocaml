(*let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*
let upperCase = ['A'-'Z']
let lowerCase = ['a'-'z']
let underScore = '_'

let alphaNumeric = upperCase | lowerCase | underScore | digit *)

type exp_type = 
	Var of string 
  |	Const of string
  |	Functor of string * (exp_type list)	 

val string_of_exp : exp_type -> string
