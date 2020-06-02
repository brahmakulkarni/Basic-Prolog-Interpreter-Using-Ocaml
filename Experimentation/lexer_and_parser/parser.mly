%{
%}

(* %token FUNC *)
%token EOL EOF LPAREN RPAREN COMMA EMPTY
%token <string> VAR
%token <string> CONST

%start main
%typr <Exp_type.exp_type> main

%% /* Grammar rules and actions follow */
;
  	main 		: expr EOL		{ $1	}

	expr_list 	: expr COMMA 

  	expr 		: VAR
		 		| CONST
		 		| CONST LPAREN   RPAREN





