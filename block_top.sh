#!/bin/sh
#@(#)Block bytecode compilation for the calculation pseudo-interpreters.
#@(#)Copyright Stéphane Grognet 2012, 2013
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4

OCAMLBIN=`which ocaml`
BINDIR=`dirname $OCAMLBIN`
LIBDIR=`dirname $BINDIR`"/lib/ocaml"

echo "open Math ;;" > .ocamlinit &
ocamlmktop -o topmath ${LIBDIR}/nums.cma ${LIBDIR}/str.cma math.cma &

echo "open Graphicmath ;;\
open Spec ;;" > graphicmath/.ocamlinit &
ocamlmktop -o graphicmath/topgraphicmath ${LIBDIR}/graphics.cma ${LIBDIR}/nums.cma ${LIBDIR}/str.cma ${LIBDIR}/unix.cma graphicmath/graphicmath.cma ;

# The compilation is assumed to be done on a BSD system, where the path to ocamlrun is "/usr/local/bin/ocamlrun"
# and the shebang is "#!/usr/local/bin/ocamlrun", consisting in 25 charactgers.

echo -n "#!/usr/bin/ocamlrun" | tee graphicmath/nonBSDtopgraphicmath > nonBSDtopmath ;
tail -c +26 graphicmath/topgraphicmath >> graphicmath/nonBSDtopgraphicmath &
tail -c +26 topmath >> nonBSDtopmath
chmod +x nonBSDtopmath graphicmath/nonBSDtopgraphicmath


exit 0

