%{
%}

%token FUNC
%token <string> VAR
%token <string> CONST

%start expr
%typr <Exp_type.exp_type> exp

%% /* Grammar rules and actions follow */
;
  exp :
