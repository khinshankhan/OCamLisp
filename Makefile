all:
	ocamlbuild -use-menhir -use-ocamlfind -I src/ -I src/reader -I src/syntax -I src/eval main.native

clean:
	@rm -f main.native
	@rm -f src/*.cm[iox]
	@rm -f src/parser.ml src/parser.mli
	@rm -f src/lexer.ml
	@rm -rf _build
