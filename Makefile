.PHONY: all clean

OCB_FLAGS = -use-menhir -use-ocamlfind
SRC = -I src/ -I src/reader -I src/syntax -I src/eval -I src/error
PKG =
OCB = ocamlbuild $(OCB_FLAGS) $(SRC) $(PKG)

all:
	 @$(OCB) main.native > output.log

explicit:
	$(OCB) main.native

clean:
	@rm -f main.native
	@rm -f src/*.cm[iox]
	@rm -f src/parser.ml src/parser.mli
	@rm -f src/lexer.ml
	@rm -rf _build
	@rm -f output.log
	clear
