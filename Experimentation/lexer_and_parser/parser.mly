%{
%}

(* %token FUNC *)
%token EOL EOF LPAREN RPAREN COMMA EMPTY
%token <string> VAR
%token <string> IDEN
%token <int> NUM

%start main
%typr <Exp_type.exp_type> main

%% /* Grammar rules and actions follow */
;
  	main 		: expr EOL		{ $1						}

	const		: NUM			{ Expr_type.CONST(NUM($1)) 	}
				| IDEN			{ Expr_type.CONST(IDEN($1)) }

	expr_list 	: expr
				| expr COMMA expr_list 

  	expr 		: VAR
		 		| const
		 		| const LPAREN   RPAREN





