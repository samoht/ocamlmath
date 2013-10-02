#!/bin/sh
#@(#)Compilation de ferdinand.o à partir de ferdinand-bytecode.ml
#@(#)Copyright Stéphane Grognet 2012, 2013
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4


cat ../util/util.ml ../util/bary.ml ../data/data.ml ../sparse/hash.ml ../sparse/sparse_vector.ml ../sparse/sparse_tensor.ml ../sparse/sparse_matrix.ml ../matrix/mat.ml ../trans/fft.ml ../matrix/matrix.ml ../infinitesimal/infinitesimal.ml ../readwrite/readwrite.ml ../sci/sci.ml ../reduc/reduc.ml ../data/data2.ml ../draw/draw.ml ../widget/widget.ml analogic.ml ferdinand_bytecode.ml > compilation_source.ml

ocamlc.opt -linkall -o ferdinand.cmo /usr/local/lib/ocaml/graphics.cma /usr/local/lib/ocaml/nums.cma /usr/local/lib/ocaml/str.cma /usr/local/lib/ocaml/unix.cma compilation_source.ml

# The compilation is assumed to be done on a BSD system, where the path to ocamlrun is "/usr/local/bin/ocamlrun"
# and the shebang is "#!/usr/local/bin/ocamlrun", consisting in 25 charactgers.

echo -n "#!/usr/bin/ocamlrun" > nonBSDferdinand.cmo
tail -c +26 ferdinand.cmo >> nonBSDferdinand.cmo

rm -f compilation_source.*

exit 0
