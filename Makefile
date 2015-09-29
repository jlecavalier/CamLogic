-include Makefile.locale

all:
	ocamlbuild -no-hygiene -j 0 main.native
	ocamlbuild -no-hygiene -j 0 tdpll.native

clean:
	ocamlbuild -clean;