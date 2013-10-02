#!/bin/sh
#@(#)Block bytecode compilation of the calculation modules for Ocaml.
#@(#)Copyright Stéphane Grognet 2012, 2013
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4

OCAMLBIN=`which ocaml`
BINDIR=`dirname $OCAMLBIN`
LIBDIR=`dirname $BINDIR`"/lib/ocaml"

ocamlc.opt -c -o math.cmo math.ml &
ocamlc.opt -c -o graphicmath/graphicmath.cmo graphicmath/graphicmath.ml

ocamlc.opt -a -linkall -o math.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma math.cmo &
ocamlc.opt -a -linkall -o graphicmath/graphicmath.cma ${LIBDIR}/graphics.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma ${LIBDIR}/unix.cma graphicmath/graphicmath.cmo

#rm -f math.cmo &
#rm -f graphicmath/graphicmath.cmo

if ( ! test -h spec/graphicmath.cma )
 then
  ln -s graphicmath/graphicmath.cma spec/graphicmath.cma
fi

if ( ! test -h spec/graphicmath.cmi )
 then
  ln -s graphicmath/graphicmath.cmi spec/graphicmath.cmi
fi

exit 0
