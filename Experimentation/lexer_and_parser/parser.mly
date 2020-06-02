%{
open Expr_type
%}

%token EOL EOF LPAREN RPAREN COMMA EMPTY
%token <string> VAR
%token <string> IDEN
%token <int> NUM

%start main
%type <Expr_type.expr> main

%% /* Grammar rules and actions follow */
;
  main: 	
		| expr EOL						{ $1						}

	const:		
		| NUM							{ CONST(NUM($1)) 			}
		| IDEN							{ CONST(IDEN($1)) 			}

  expr: 
		| VAR							{ VAR($1)					}
		| const							{$1}
		| IDEN LPAREN expr_list RPAREN { FUNC($1, $3) 				}

	expr_list: 
		| expr							{ [$1]						}
		| expr COMMA expr_list 			{ $1 :: $3				}






