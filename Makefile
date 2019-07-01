all:
	ocamlbuild -use-menhir -tag thread -use-ocamlfind -I src/  -quiet -pkg core main.native
