SOURCES = position.ml game.ml
PACKS = universeJsLocal
RESULT = game
OCAMLMAKEFILE = $(OPAM_SWITCH_PREFIX)/lib/ocaml-makefile/OCamlMakefile
include $(OCAMLMAKEFILE)

$(RESULT).js : byte-code
	js_of_ocaml compile $(RESULT)
