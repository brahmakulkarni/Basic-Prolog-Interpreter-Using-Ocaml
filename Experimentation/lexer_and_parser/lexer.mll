{
exception Lexer_exception of string
}

let digit = ['0'-'9']
let integer = ['0'-'9']['0'-'9']*
let upperCase = ['A'-'Z']
let lowerCase = ['a'-'z']
let underScore = '_'

let alphaNumeric = upperCase | lowerCase | underScore | digit

let variable = upperCase alphaNumeric*
let constant = lowerCase alphaNumeric*
let lparen = '('
let rparen = ')'
let comma = ','
let arg = (variable|constant)
let withinParen = arg (comma arg)+
let functor = constant lparen withinParen rparen

rule scan = parse
  |	[' ' '\t']	{scan lexbuf} (*skips blanks*)
  | 	['\n']		{EOL}
(*  |	integer as i 	{Parser.NUM(int_of_string i)} *)
  | 	variable as v 	{Parser.VAR(v)}
  | 	constant as c 	{Parser.CONST(c)}
  |	'('		{LPAREN}
  | 	')'		{RPAREN}
  | 	eof		{Parser.EOF}
  |	functor		{Parser.FUNC}
  |	_		{scan lexbuf}

{
}
