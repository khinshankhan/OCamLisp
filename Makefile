.PHONY: all clean

OCB_FLAGS = -use-menhir -use-ocamlfind
SRCS = -I src/ -I src/reader -I src/syntax -I src/eval
PKGS =
OCB = ocamlbuild $(OCB_FLAGS) $(SRCS) $(PKG)

all:
	 $(OCB) main.native

clean:
	@rm -f main.native
	@rm -f src/*.cm[iox]
	@rm -f src/parser.ml src/parser.mli
	@rm -f src/lexer.ml
	@rm -rf _build
	clear
