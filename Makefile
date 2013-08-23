# Modified by <anonymous> July 2013

#######################################################################
#                                                                     #
#                            OCamlSpotter                             #
#                                                                     #
#                             Jun FURUSE                              #
#                                                                     #
#   Copyright 2008-2012 Jun Furuse. All rights reserved.              #
#   This file is distributed under the terms of the GNU Library       #
#   General Public License, with the special exception on linking     #
#   described in file LICENSE.                                        #
#                                                                     #
#######################################################################


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
OCAMLSRCDIR=..
INCLUDES_DEP=-I +compiler-libs -package cryptokit

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP) -I +unix

MODULES= ttmap untypeast pprintast mod optcompile optmain

OBJS=		$(addsuffix .cmo, $(MODULES))

XOBJS=		$(addsuffix .cmx, $(MODULES))

PROF= #-p -g

poorman: $(OBJS)
	ocamlfind ocamlc -package cryptokit -g -o $@ -I +compiler-libs -linkpkg ocamlcommon.cma ocamloptcomp.cma $(OBJS)

abstraction:
	ocamlc -c merkle.mli
	ocamlfind ocamlopt -package cryptokit,batteries,sha -c merkle.ml
	ocamlc -c bintree.ml
	ocamlc -c skiplist.ml
	ocamlc -c redblack.ml
	ocamlc -c mtree.ml
	ocamlc -c blockchain.ml
	ocamlfind ocamlopt -package benchmark,batteries,sha -c driver.ml

prover:
	ocamlfind ocamlopt -package cryptokit,batteries,sha -c merkle.ml
	MODE=prover ./poorman $(PROF) -c bintree.ml
	MODE=prover ./poorman $(PROF) -c skiplist.ml
	MODE=prover ./poorman $(PROF) -c redblack.ml
	MODE=prover ./poorman $(PROF) -c mtree.ml
	MODE=prover ./poorman $(PROF) -c blockchain.ml
	ocamlfind ocamlopt $(PROF) -package benchmark,batteries,sha -c driver.ml
	ocamlfind ocamlopt $(PROF) -linkpkg -package cryptokit,benchmark,batteries,sha -o driver_prv merkle.cmx skiplist.cmx bintree.cmx redblack.cmx mtree.cmx blockchain.cmx driver.ml 

verifier:
	ocamlfind ocamlopt -package cryptokit,batteries,sha $(PROF) -c merkle.ml
	MODE=verifier ./poorman $(PROF) -c bintree.ml
	MODE=verifier ./poorman $(PROF) -c skiplist.ml
	MODE=verifier ./poorman $(PROF) -c redblack.ml
	MODE=verifier ./poorman $(PROF) -c mtree.ml
	ocamlfind ocamlopt $(PROF) -package benchmark,batteries,sha -c  driver.ml
	ocamlfind ocamlopt $(PROF) -linkpkg -package cryptokit,benchmark,batteries,sha  -o driver_vrf merkle.cmx skiplist.cmx bintree.cmx redblack.cmx mtree.cmx driver.ml

ideal:
	ocamlfind ocamlopt -package cryptokit,batteries,sha $(PROF) -c merkle.ml
	MODE=ideal ./poorman $(PROF) -c bintree.ml
	MODE=ideal ./poorman $(PROF) -c skiplist.ml
	MODE=ideal ./poorman $(PROF) -c redblack.ml
	MODE=ideal ./poorman $(PROF) -c mtree.ml
	ocamlfind ocamlopt $(PROF) -package benchmark,batteries,sha -c driver.ml
	ocamlfind ocamlopt $(PROF) -linkpkg -package cryptokit,benchmark,batteries,sha -o driver_idl merkle.cmx skiplist.cmx bintree.cmx redblack.cmx mtree.cmx driver.ml

cmerkle::
	gcc -o cmerkle -O2 -pg -std=c99 cmerkle.c -lcrypto

modes: prover verifier ideal

clean::
	rm -f poorman

main.ml: ../ocaml-trunk/driver/main.ml
	cp $< $@

optmain.ml: ../ocaml-trunk/driver/optmain.ml
	cp $< $@

untypeast.ml: ../ocaml-trunk/tools/untypeast.ml
	cp $< $@

untypeast.mli: ../ocaml-trunk/tools/untypeast.mli
	cp $< $@

pprintast.ml: ../ocaml-trunk/parsing/pprintast.ml
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
	ocamldep $(INCLUDES) *.mli *.ml > .depend

clean::
	rm -f *.cm* *.annot *.o

include .depend
