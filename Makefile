# Various commands and dir
##########################
CAMLRUN= ocamlrun
OCAMLC   = ocamlfind ocamlc -I +compiler-libs -g -annot -bin-annot -w A-4-9 -warn-error A-4-9-10-27-32-33-34-39-45
OCAMLOPT = ocamlfind ocamlopt -I +compiler-libs -annot -bin-annot -w A-4-9 -warn-error A-4-9-32-33-34-45
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC= ocamlyacc
OCAMLLIB = $(LIBDIR)
OCAMLBIN = $(BINDIR)

# Compilation
#############
OCAMLSRCDIR=../ocaml-4.01.0
INCLUDES_DEP=-I +compiler-libs -package cryptokit

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP) -I +unix

MODULES= ttmap untypeast pprintast mod optcompile optmain

OBJS=		$(addsuffix .cmo, $(MODULES))

XOBJS=		$(addsuffix .cmx, $(MODULES))

PROF= -I examples #-p -g

poorman: $(OBJS)
	ocamlfind ocamlc -package cryptokit -g -o $@ -I +compiler-libs -linkpkg ocamlcommon.cma ocamloptcomp.cma $(OBJS)

redblack: 
	ocamlfind ocamlopt -package batteries,sha $(PROF) -c examples/merkle.ml
#	prover
	MODE=prover ./poorman $(PROF) -c examples/redblack.ml
	ocamlfind ocamlopt $(PROF) -package benchmark,batteries,sha -c examples/driver_redblack.ml
	ocamlfind ocamlopt $(PROF) -linkpkg -package benchmark,batteries,sha -o redblack_prv merkle.cmx redblack.cmx examples/driver_redblack.ml 
#	verifier
	MODE=verifier ./poorman $(PROF) -c examples/redblack.ml
	ocamlfind ocamlopt $(PROF) -package benchmark,batteries,sha -c examples/driver_redblack.ml
	ocamlfind ocamlopt $(PROF) -linkpkg -package benchmark,batteries,sha -o redblack_vrf merkle.cmx redblack.cmx examples/driver_redblack.ml 

prover:
	ocamlfind ocamlopt -package batteries,sha $(PROF) -c examples/merkle.ml
	MODE=prover ./poorman $(PROF) -c examples/bintree.ml
	MODE=prover ./poorman $(PROF) -c examples/skiplist.ml
	MODE=prover ./poorman $(PROF) -c examples/redblack.ml
	MODE=prover ./poorman $(PROF) -c examples/mtree.ml
	MODE=prover ./poorman $(PROF) -c examples/blockchain.ml
	ocamlfind ocamlopt $(PROF) -package benchmark,batteries,sha -c examples/driver.ml
	ocamlfind ocamlopt $(PROF) -linkpkg -package benchmark,batteries,sha -o driver_prv merkle.cmx skiplist.cmx bintree.cmx redblack.cmx mtree.cmx blockchain.cmx examples/driver.ml 

verifier:
	ocamlfind ocamlopt -package batteries,sha $(PROF) -c examples/merkle.ml
	MODE=verifier ./poorman $(PROF) -c examples/bintree.ml
	MODE=verifier ./poorman $(PROF) -c examples/skiplist.ml
	MODE=verifier ./poorman $(PROF) -c examples/redblack.ml
	MODE=verifier ./poorman $(PROF) -c examples/mtree.ml
	ocamlfind ocamlopt $(PROF) -package benchmark,batteries,sha -c  examples/driver.ml
	ocamlfind ocamlopt $(PROF) -linkpkg -package benchmark,batteries,sha  -o driver_vrf merkle.cmx skiplist.cmx bintree.cmx redblack.cmx mtree.cmx examples/driver.ml

ideal:
	ocamlfind ocamlopt -package batteries,sha $(PROF) -c examples/merkle.ml
	MODE=ideal ./poorman $(PROF) -c examples/bintree.ml
	MODE=ideal ./poorman $(PROF) -c examples/skiplist.ml
	MODE=ideal ./poorman $(PROF) -c examples/redblack.ml
	MODE=ideal ./poorman $(PROF) -c examples/mtree.ml
	ocamlfind ocamlopt $(PROF) -package benchmark,batteries,sha -c examples/driver.ml
	ocamlfind ocamlopt $(PROF) -linkpkg -package benchmark,batteries,sha -o driver_idl merkle.cmx skiplist.cmx bintree.cmx redblack.cmx mtree.cmx examples/driver.ml

# abstraction:
# #	 For creating the interface files, compile only without linking
# 	ocamlc -c examples/merkle.mli
# 	ocamlfind ocamlopt -package batteries,sha -c examples/merkle.ml
# 	ocamlc -c examples/bintree.ml
# 	ocamlc -c examples/skiplist.ml
# 	ocamlc -c examples/redblack.ml
# 	ocamlc -c examples/mtree.ml
# 	ocamlc -c examples/blockchain.ml
# 	ocamlfind ocamlopt -package benchmark,batteries,sha -c examples/driver.ml

cmerkle::
	gcc -o cmerkle -O2 -pg -std=c99 cmerkle.c -lcrypto

modes: prover verifier ideal

clean::
	rm -f poorman

main.ml: $(OCAMLSRCDIR)/driver/main.ml
	cp $< $@

optmain.ml: $(OCAMLSRCDIR)/driver/optmain.ml
	cp $< $@

untypeast.ml: $(OCAMLSRCDIR)/tools/untypeast.ml
	cp $< $@

untypeast.mli: $(OCAMLSRCDIR)/tools/untypeast.mli
	cp $< $@

pprintast.ml: $(OCAMLSRCDIR)/parsing/pprintast.ml
	cp $< $@

beforedepend:: optmain.ml main.ml untypeast.ml untypeast.mli pprintast.ml

clean::
	rm -f optmain.ml main.ml untypeast.ml untypeast.mli pprintast.ml

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	ocamldep $(INCLUDES_DEP) *.mli *.ml  > .depend

clean::
	rm -f *.cm* *.annot *.o examples/*.cm*

include .depend
