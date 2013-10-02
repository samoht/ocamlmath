#!/bin/sh
#@(#)Fragmented bytecode compilation of the calculation modules for Ocaml.
#@(#)Copyright Stéphane Grognet 2012
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4
#@(#)The compilation uses around 700 megabyte of random access memory.

OCAMLBIN=`which ocaml`
BINDIR=`dirname $OCAMLBIN`
LIBDIR=`dirname $BINDIR`"/lib/ocaml"

cd util
ocamlopt.opt -c -o util.cmx ${LIBDIR}/nums.cmxa util.ml
ocamlopt.opt -a -linkall -o util.cmxa util.cmx
ocamlopt.opt -c -o bary.cmx bary.ml
ocamlopt.opt -a -linkall -o bary.cmxa bary.cmx
ocamlopt.opt -c -o deg.cmx ${LIBDIR}/nums.cmxa deg.ml
ocamlopt.opt -a -linkall -o deg.cmxa deg.cmx
cd ../data
ocamlopt.opt -c -o data.cmx -I ../util ${LIBDIR}/nums.cmxa data.ml
ocamlopt.opt -a -linkall -o data.cmxa data.cmx
cd ../sparse
ocamlopt.opt -c -o hash.cmx -I ../util -I ../data hash.ml
ocamlopt.opt -a -linkall -o hash.cmxa hash.cmx
ocamlopt.opt -c -o sparse_vector.cmx -I ../util -I ../data ${LIBDIR}/nums.cmxa ${LIBDIR}/str.cmxa sparse_vector.ml
ocamlopt.opt -a -linkall -o sparse_vector.cmxa sparse_vector.cmx
ocamlopt.opt -c -o sparse_tensor.cmx -I ../util -I ../data ${LIBDIR}/nums.cmxa ${LIBDIR}/str.cmxa sparse_tensor.ml
ocamlopt.opt -a -linkall -o sparse_tensor.cmxa sparse_tensor.cmx
ocamlopt.opt -c -o sparse_matrix.cmx -I ../util -I ../data ${LIBDIR}/nums.cmxa ${LIBDIR}/str.cmxa sparse_matrix.ml
ocamlopt.opt -a -linkall -o sparse_matrix.cmxa sparse_matrix.cmx
cd ../matrix
ocamlopt.opt -c -o mat.cmx -I ../util -I ../data -I ../sparse ${LIBDIR}/nums.cmxa ${LIBDIR}/str.cmxa mat.ml
ocamlopt.opt -a -linkall -o mat.cmxa mat.cmx
ocamlopt.opt -c -o matrix.cmx -I ../util matrix.ml
ocamlopt.opt -a -linkall -o matrix.cmxa matrix.cmx
cd ../trans
ocamlopt.opt -c -o fft.cmx -I ../util -I ../data -I ../sparse -I ../matrix fft.ml
ocamlopt.opt -a -linkall -o fft.cma fft.cmx
cd ../readwrite
ocamlopt.opt -c -o readwrite.cmx -I ../util -I ../matrix ${LIBDIR}/str.cmxa readwrite.ml
ocamlopt.opt -a -linkall -o readwrite.cmxa readwrite.cmx
cd ../infinitesimal
ocamlopt.opt -c -o infinitesimal.cmx -I ../util -I ../matrix infinitesimal.ml
ocamlopt.opt -a -linkall -o infinitesimal.cmxa infinitesimal.cmx
cd ../sci
ocamlopt.opt -c -o sci.cmx -I ../util -I ../sparse -I ../data -I ../matrix -I ../readwrite ${LIBDIR}/nums.cmxa ${LIBDIR}/str.cmxa sci.ml
ocamlopt.opt -a -linkall -o sci.cmxa sci.cmx
cd ../reduc
ocamlopt.opt -c -o reduc.cmx -I ../util -I ../matrix -I ../sci ${LIBDIR}/nums.cmxa ${LIBDIR}/str.cmxa reduc.ml
ocamlopt.opt -a -linkall -o reduc.cmxa reduc.cmx
cd ../data
ocamlopt.opt -c -o data2.cmx -I ../util -I ../matrix -I ../sci -I ../reduc ${LIBDIR}/nums.cmxa ${LIBDIR}/str.cmxa data2.ml
ocamlopt.opt -a -linkall -o data2.cmxa data2.cmx
cd ../draw
ocamlopt.opt -c -o draw.cmx -I ../util -I ../matrix -I ../infinitesimal -I ../readwrite -I ../data -I ../sparse ${LIBDIR}/graphics.cmxa draw.ml
ocamlopt.opt -a -linkall -o draw.cmxa draw.cmx
cd ../widget
ocamlopt.opt -c -o widget.cmx -I ../util -I ../matrix -I ../readwrite -I ../infinitesimal ${LIBDIR}/graphics.cmxa widget.ml
ocamlopt.opt -a -linkall -o widget.cmxa widget.cmx
cd ../analogic
ocamlopt.opt -c -o analogic.cmx -I ../util -I ../data -I ../sparse -I ../matrix -I ../trans -I ../readwrite -I ../infinitesimal -I ../sci -I ../reduc -I ../draw -I ../widget ${LIBDIR}/graphics.cmxa ${LIBDIR}/unix.cmxa analogic.ml
ocamlopt.opt -a -linkall -o analogic.cmxa analogic.cmx
cd ../spec
ocamlopt.opt -c -o spec.cmx -I ../util -I ../readwrite ${LIBDIR}/str.cmxa ${LIBDIR}/unix.cmxa spec.ml
ocamlopt.opt -a -linkall -o spec.cmxa spec.cmx
cd ..

exit 0

