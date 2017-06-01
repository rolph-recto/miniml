all:
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -quiet -pkg core,sexplib,ppx_sexp_conv prog.native

clean:
	rm *.native
	rm -r _build
