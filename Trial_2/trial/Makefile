search: try.cmx try2.cmx
	ocamlopt -o search try.cmx try2.cmx

try2.cmx: try.cmi 
	ocamlopt -c try2.ml

try.cmx: try.ml
	ocamlopt -c try.ml

try.cmi: try.ml
	ocamlopt -c try.ml

clean: 
	rm *.cm[ix] *.o search