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
		| NUM							{ CONST(NUM($1)) 			}
		| IDEN							{ CONST(IDEN($1)) 			}

  	expr: 
		| VAR							{ VAR($1)					}
		| const							{ $1						}
		| IDEN LPAREN expr_list RPAREN 	{ FUNC($1, $3) 				}

	expr_list: 
		| expr							{ [$1]						}
		| expr COMMA expr_list 			{ $1 :: $3					}

	right:
        | expr                          { LEAF($1)                  }
        | right OR right                { Expr_type.OR($1, $3)      }
        | right COMMA right             { AND($1, $3)               }
        | NOT right                     { Expr_type.NOT($2)         }

    rule:
        | expr                          { HEAD($1)                  }
        | expr IMPLIES right            { NODE($1,$3)               }








