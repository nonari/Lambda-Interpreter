OCAMLC=ocamlc
OCAMLOPT=ocamlopt
OCAMLDEP=ocamldep

OBJ= ast.cmo parser.cmo lexer.cmo lambda.cmo main.cmo

all: $(OBJ)
	$(OCAMLC) -o run $(OBJ)


opt: $(OBJ:.cmo=.cmx)
	$(OCAMLOPT) -o fly $(OBJ:.cmo=.cmx)

optclean: $(OBJ:.cmo=.cmx)
	# rm -f *.o *.cm*
	$(OCAMLOPT) -o fly $(OBJ:.cmo=.cmx)

depend: parser.ml parser.mli lexer.ml
	ocamldep \
	$(patsubst %.cmo,%.ml,$(filter %.cmo,$(OBJ))) \
	$(patsubst %.cmi,%.mli,$(filter %.cmi,$(OBJ))) \
	 > .depend

.SUFFIXES: .ml .mli .cmo .cmx .cmi .mly .mll
.ml.cmx:
	$(OCAMLOPT) -c $<
.ml.cmo:
	$(OCAMLC) -c $<
.mli.cmi:
	$(OCAMLC) -c $<
.mll.ml:
	ocamllex $<
.mly.ml:
	ocamlyacc $<

clean:
	rm -f *.o *.cm* parser.ml parser.mli lexer.ml fly run



include .depend 
