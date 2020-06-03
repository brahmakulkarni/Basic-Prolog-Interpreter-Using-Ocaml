%{
open Expr_type
%}

%token EOL EOF LPAREN RPAREN COMMA EMPTY OR NOT IMPLIES
%token <string> VAR
%token <string> IDEN
%token <int> NUM

%start main
%type <Expr_type.rule> main

%% /* Grammar rules and actions follow */
;
  	main: 	
		| expr							{ HEAD($1)					}

	const:		
		| NUM							{ NUM($1) 					}
		| IDEN							{ IDEN($1)					}		

	right:
        | expr                          { LEAF($1)                  }
        | right OR right                { Expr_type.OR($1, $3)      }   
        | right COMMA right             { AND($1, $3)               }
        | NOT right                     { Expr_type.NOT($2)         }

    rule:
        | expr                          { HEAD($1)                  }
        | expr IMPLIES right            { print_endline "implies encountered"; NODE($1,$3)               }

  	expr: 
		| VAR							{ VAR($1)					}
		| const							{ CONST($1)					}
		| IDEN LPAREN expr_list RPAREN 	{ print_endline "Functor encountered"; FUNC($1, $3) 				}

	expr_list: 
		| expr							{ [$1]						}
		| expr COMMA expr_list 			{ $1 :: $3					}








