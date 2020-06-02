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
let constant = (lowerCase alphaNumeric*) | integer
let lparen = '('
let rparen = ')'
let comma = ','
let empty = ''
let arg = (variable|constant)
let withinParen = arg (comma arg)+
let functor = constant lparen withinParen rparen

rule scan = parse
  |	[' ' '\t']		{scan lexbuf} (*skips blanks*)
  | ['\n']			{Parser.EOL}
  | comma 			{Parser.COMMA}
  | empty			{Parser.EMPTY}
  | variable as v 	{Parser.VAR(v)}
  | constant as c 	{Parser.CONST(c)}
  |	lparen			{Parser.LPAREN}
  | rparen			{Praser.RPAREN}
  | eof				{Parser.EOF}
  |	_				{scan lexbuf}
(*| integer as i    {Parser.NUM(int_of_string i)} *)
(*  |   functor     {Parser.FUNC} *)

{
}
