parser : lexer.cmx expr_type.cmx parser.cmx main.cmx
	ocamlopt -o parser lexer.cmx expr_type.cmx parser.cmx search.cmx unified.cmx main.cmx

lexer.cmx : parser.cmi lexer.ml
	ocamlopt -c lexer.ml

# lexer.cmi : parser.cmi lexer.ml
# 	ocamlopt -c lexer.ml

lexer.ml : lexer.mll
	ocamllex lexer.mll

parser.ml : expr_type.cmi parser.mly
	ocamlyacc parser.mly

parser.cmi : parser.mli
	ocamlc -c parser.mli

parser.mli : expr_type.cmi parser.mly
	ocamlyacc parser.mly

parser.cmx : lexer.cmx expr_type.cmi parser.ml
	ocamlopt -c parser.ml

expr_type.cmx : expr_type.cmi expr_type.ml
	ocamlopt -c expr_type.ml

expr_type.cmi : expr_type.mli
	ocamlc -c expr_type.mli

main.cmx : parser.cmi lexer.cmx search.cmi main.ml
	ocamlopt -c main.ml

search.cmx : unified.cmi search.ml
	ocamlopt -c search.ml

search.cmi : unified.cmi search.ml
	ocamlopt -c search.ml

unified.cmx : unified.ml
	ocamlopt -c unified.ml

unified.cmi : unified.ml
	ocamlopt -c unified.ml

clean:
	rm *.cm[ix] lexer.ml parser.ml parser.mli *.o parser 
