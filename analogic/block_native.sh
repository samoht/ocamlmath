#!/bin/sh
#@(#)Compilation of ferdinand from ferdinand.ml
#@(#)Copyright Stéphane Grognet 2012, 2013
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4
#@(#)The compilation uses around 1,4 gigabyte of random access memory.

cat ../util/util.ml ../util/bary.ml ../data/data.ml ../sparse/hash.ml ../sparse/sparse_vector.ml ../sparse/sparse_tensor.ml ../sparse/sparse_matrix.ml ../matrix/mat.ml ../trans/fft.ml ../matrix/matrix.ml ../infinitesimal/infinitesimal.ml ../readwrite/readwrite.ml ../sci/sci.ml ../reduc/reduc.ml ../data/data2.ml ../draw/draw.ml ../widget/widget.ml analogic.ml ferdinand_native.ml > compilation_source.ml

ocamlopt.opt -linkall -o ferdinand /usr/local/lib/ocaml/graphics.cmxa /usr/local/lib/ocaml/nums.cmxa /usr/local/lib/ocaml/str.cmxa /usr/local/lib/ocaml/unix.cmxa compilation_source.ml

rm -f compilation_source.*

exit 0
