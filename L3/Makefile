TOPFILE = L3
OCAMLC = ocamlfind c
OCAMLOPT = ocamlfind opt
OCAMLDEP = ocamlfind dep
SOURCES = L3.ml
CAMLP5  = -syntax camlp5o -package ostap.syntax,GT.syntax,GT.syntax.all 
PXFLAGS = $(CAMLP5)
BFLAGS = -rectypes -g -w -13-58 -package GT,ostap,unix,yojson
OFLAGS = $(BFLAGS)

all: depend $(TOPFILE)

depend: $(SOURCES)
	$(OCAMLDEP) $(PXFLAGS) *.ml > .depend

$(TOPFILE): $(SOURCES:.ml=.cmx)
	$(OCAMLOPT) -o $(TOPFILE) $(OFLAGS) -linkpkg $(SOURCES:.ml=.cmx)

$(TOPFILE).byte: $(SOURCES:.ml=.cmo)
	$(OCAMLC) -o $(TOPFILE).byte $(BFLAGS) -linkpkg $(SOURCES:.ml=.cmo)

clean:
	$(RM) $(TOPFILE) *.cm[ioxa] *.annot *.o *.opt *.byte *~ .depend

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
