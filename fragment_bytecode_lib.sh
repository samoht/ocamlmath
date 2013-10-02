#!/bin/sh
#@(#)Fragmented bytecode compilation of the calculation modules for Ocaml.
#@(#)Copyright Stéphane Grognet 2012
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4

OCAMLBIN=`which ocaml`
BINDIR=`dirname $OCAMLBIN`
LIBDIR=`dirname $BINDIR`"/lib/ocaml"

cd util
ocamlc.opt -c -o util.cmo util.ml
ocamlc.opt -a -linkall -o util.cma ${LIBDIR}/nums.cma util.cmo
ocamlc.opt -c -o bary.cmo bary.ml
ocamlc.opt -a -linkall -o bary.cma bary.cmo
ocamlc.opt -c -o deg.cmo deg.ml
ocamlc.opt -a -linkall -o deg.cma ${LIBDIR}/nums.cma deg.cmo
cd ../data
ocamlc.opt -c -o data.cmo -I ../util data.ml
ocamlc.opt -a -linkall -o data.cma ${LIBDIR}/nums.cma data.cmo
cd ../sparse
ocamlc.opt -c -o hash.cmo -I ../util -I ../data hash.ml
ocamlc.opt -a -linkall -o hash.cma hash.cmo
ocamlc.opt -c -o sparse_vector.cmo -I ../util -I ../data sparse_vector.ml
ocamlc.opt -a -linkall -o sparse_vector.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma sparse_vector.cmo
ocamlc.opt -c -o sparse_tensor.cmo -I ../util -I ../data sparse_tensor.ml
ocamlc.opt -a -linkall -o sparse_tensor.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma sparse_tensor.cmo
ocamlc.opt -c -o sparse_matrix.cmo -I ../util -I ../data sparse_matrix.ml
ocamlc.opt -a -linkall -o sparse_matrix.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma sparse_matrix.cmo
cd ../matrix
ocamlc.opt -c -o mat.cmo -I ../util -I ../data -I ../sparse mat.ml
ocamlc.opt -a -linkall -o mat.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma mat.cmo
ocamlc.opt -c -o matrix.cmo -I ../util matrix.ml
ocamlc.opt -a -linkall -o matrix.cma matrix.cmo
cd ../trans
ocamlc.opt -c -o fft.cmo -I ../util -I ../data -I ../sparse -I ../matrix fft.ml
ocamlc.opt -a -linkall -o fft.cma fft.cmo
cd ../readwrite
ocamlc.opt -c -o readwrite.cmo -I ../util -I ../matrix readwrite.ml
ocamlc.opt -a -linkall -o readwrite.cma ${LIBDIR}/str.cma readwrite.cmo
cd ../infinitesimal
ocamlc.opt -c -o infinitesimal.cmo -I ../util -I ../matrix infinitesimal.ml
ocamlc.opt -a -linkall -o infinitesimal.cma infinitesimal.cmo
cd ../sci
ocamlc.opt -c -o sci.cmo -I ../util -I ../sparse -I ../data -I ../matrix -I ../readwrite sci.ml
ocamlc.opt -a -linkall -o sci.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma sci.cmo
cd ../reduc
ocamlc.opt -c -o reduc.cmo -I ../util -I ../matrix -I ../sci reduc.ml
ocamlc.opt -a -linkall -o reduc.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma reduc.cmo
cd ../data
ocamlc.opt -c -o data2.cmo -I ../util -I ../matrix -I ../sci -I ../reduc data2.ml
ocamlc.opt -a -linkall -o data2.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma data2.cmo
cd ../draw
ocamlc.opt -c -o draw.cmo -I ../util -I ../matrix -I ../infinitesimal -I ../readwrite -I ../data -I ../sparse draw.ml
ocamlc.opt -a -linkall -o draw.cma ${LIBDIR}/graphics.cma draw.cmo
cd ../widget
ocamlc.opt -c -o widget.cmo -I ../util -I ../matrix -I ../readwrite -I ../infinitesimal widget.ml
ocamlc.opt -a -linkall -o widget.cma ${LIBDIR}/graphics.cma widget.cmo
cd ../analogic
ocamlc.opt -c -o analogic.cmo -I ../util -I ../data -I ../sparse -I ../matrix -I ../trans -I ../readwrite -I ../infinitesimal -I ../sci -I ../reduc -I ../draw -I ../widget analogic.ml
ocamlc.opt -a -linkall -o analogic.cma ${LIBDIR}/graphics.cma ${LIBDIR}/unix.cma analogic.cmo
cd ../spec
ocamlc.opt -c -o spec.cmo -I ../util -I ../readwrite spec.ml
ocamlc.opt -a -linkall -o spec.cma ${LIBDIR}/str.cma ${LIBDIR}/unix.cma spec.cmo
cd ..

exit 0

