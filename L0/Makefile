TOPFILE = L0
OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
SOURCES = L0.ml LibSerialize.ml
CAMLP5  = -syntax camlp5o -package ostap.syntax,GT.syntax,GT.syntax.all 
PXFLAGS = $(CAMLP5)
BFLAGS = -rectypes -g -w -13-58 -package GT,ostap,unix,yojson
OFLAGS = $(BFLAGS)
PPX_EXE=ppx.exe
PPX_MAIN=ppx_rewriter_main.ml

all: depend $(TOPFILE)

depend: $(SOURCES)
	$(OCAMLDEP) $(PXFLAGS) *.ml > .depend

$(TOPFILE): $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(TOPFILE) $(OFLAGS) -linkpkg $(SOURCES:.ml=.cmx)

$(TOPFILE).cma: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -a -o $(TOPFILE).cma $(BFLAGS) -linkpkg $(SOURCES:.ml=.cmo)

clean:
	$(RM) $(TOPFILE) $(TOPFILE).bc.js *.cm[ioxa] *.annot *.map *.o *.opt *.byte *~ .depend

-include .depend
# generic rules

###############
%.cmi: %.mli
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

# Note: cmi <- mli should go first
%.cmi: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.cmo: %.ml
	$(OCAMLC) -c $(BFLAGS) $(PXFLAGS) $<

%.o: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

%.cmx: %.ml
	$(OCAMLOPT) -c $(OFLAGS) $(STATIC) $(PXFLAGS) $<

$(TOPFILE).bc.js: $(TOPFILE).byte
	js_of_ocaml --debug-info --no-inline --source-map --pretty $< -o $@

jsL0.cmo: BFLAGS += -I .
jsL0.cmo: L0.cma
jsL0.byte: BFLAGS += -package js_of_ocaml
jsL0.byte: jsL0.cmo 
	$(OCAMLC) -o $@ $(BFLAGS) -linkpkg L0.cmo $<

jsL0.bc.js: jsL0.byte
	js_of_ocaml --debug-info --no-inline --source-map --pretty $< -o $@

client.cmo: BFLAGS += -I . -package js_of_ocaml
client.cmo: PXFLAGS = -pp "./$(PPX_EXE)"
client.cmo: L0.cma ppx.exe
client.byte: BFLAGS += -package js_of_ocaml
client.byte: client.cmo 
	$(OCAMLC) -o $@ $(BFLAGS) -linkpkg L0.cmo LibSerialize.cmo $<

client.bc.js: client.byte
	js_of_ocaml --debug-info --no-inline --source-map --pretty $< -o $@

ppx.exe:
	@echo "let () = Ppxlib.Driver.standalone ()" > $(PPX_MAIN)
	@ocamlfind opt $(PPX_MAIN) -o $@ \
		-predicates custom_ppx -predicates ppx_driver \
		-package GT.ppx,GT.ppx_all,js_of_ocaml-ppx \
		-package ppxlib.runner \
		-linkpkg -linkall
	@$(RM) $(PPX_MAIN)

