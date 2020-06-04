search: unified.cmx search.cmx
	ocamlopt -o search unified.cmx search.cmx

search.cmx: unified.cmi search.ml
	ocamlopt -c search.ml

unified.cmx: unified.ml
	ocamlopt -c unified.ml

unified.cmi: unified.ml
	ocamlopt -c unified.ml

clean: 
	rm *.cm[ix] *.o search
