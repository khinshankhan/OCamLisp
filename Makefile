all:
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -I src/ -quiet -pkg core main.native

clean:
	@rm -f main.native
	@rm -f src/*.cm[iox]
	@rm -f src/parser.ml src/parser.mli
	@rm -f src/lexer.ml
	@rm -rf _build
