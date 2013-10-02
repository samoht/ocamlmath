#!/bin/sh
#@(#)Compilation d'ocamlmath
#@(#)Copyright Stéphane Grognet 2012
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4


echo "-> Making the specifications"
if ( ! test -e MathSpec )
 then
  ( mkdir MathSpec )
 else
  ( rm -r MathSpec ; mkdir MathSpec )
fi
if ( ! test -e GraphicmathSpec )
 then
  ( mkdir GraphicmathSpec )
 else
  ( rm -r GraphicmathSpec ; mkdir GraphicmathSpec )
fi
ocamldoc.opt -v -html -d MathSpec -colorize-code -all-params -keep-code -intro foreword_math.ml math.ml &
ocamldoc.opt -v -html -d GraphicmathSpec -colorize-code -all-params -keep-code -intro foreword_graphicmath.ml graphicmath/graphicmath.ml
if ( ! test -e ../../doc/ocamlmath )
 then
  ( mkdir ../../doc/ocamlmath )
fi
mv MathSpec GraphicmathSpec ../../doc/ocamlmath
cp README_opam ../../doc/ocamlmath/README
cp depend.txt licence.html ../../doc/ocamlmath

echo "-> Making the bytecode libraries"
if ( ! test -e ../../share/ocamlmath )
 then
  ( mkdir ../../share/ocamlmath )
fi
cp misc.ml test_misc.ml ../../share/ocamlmath
sh block_bytecode_lib.sh
sh fragment_bytecode_lib.sh

echo "-> Making the native libraries"
sh fragment_native_lib.sh

echo "-> Making the toplevels"
sh block_top.sh
if ( ! test -e ../../lib/ocamlmath )
 then
  ( mkdir ../../lib/ocamlmath )
fi
cp math.cm? topmath nonBSDtopmath *.sh *.ml .ocamlinit depend.* ../../lib/ocamlmath
cp -R analogic data draw graphicmath infinitesimal matrix readwrite reduc sci sparse spec trans util widget ../../lib/ocamlmath
chmod +x opam_topmath.sh opam_topgraphicmath.sh

echo "The end"

exit 0
