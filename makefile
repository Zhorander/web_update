OCAML=ocamlopt
FIND=ocamlfind
JSON=-package yojson

all: web_update

web_update: web_update.ml
	$(FIND) $(OCAML) $(JSON) $@.ml -o $@ -linkpkg