LIB_FILES=gedcom.a gedcom.cmxa gedcom.cma gedcom.cmi

OCAMLBUILD=ocamlbuild -use-ocamlfind -tags rectypes -pkgs str

all:

build:
	 $(OCAMLBUILD) $(LIB_FILES)

install:
	ocamlfind install gedcom META $(addprefix _build/, $(LIB_FILES))

uninstall:
	ocamlfind remove gedcom

doc:
	$(OCAMLBUILD) gedcom.docdir/index.html

clean:
	ocamlbuild -clean
