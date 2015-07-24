-include Makefile.locale

OCAML_FLAGS= \
	-pp "camlp4o pa_extend.cmo" \
	-cflags -cc,$(CXX) \
	-lflags -cc,$(CXX) \
	-cflags -w,Ae \
	-cflag -g \
	-lflag -g \

LIBS = unix
OCAMLC=ocamlopt

all:
	ocamlbuild -no-hygiene -j 0 $(OCAML_FLAGS) -libs $(LIBS) main.native

clean:
	ocamlbuild -clean;