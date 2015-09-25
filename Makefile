-include Makefile.locale

all:
	ocamlbuild -no-hygiene -j 0 main.native

clean:
	ocamlbuild -clean;