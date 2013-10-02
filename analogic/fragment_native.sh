#!/bin/sh
#@(#)Compilation de ferdinand à partir de ferdinand_fragment_native.ml
#@(#)Copyright Stéphane Grognet 2012, 2013
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4

ocamlopt.opt -linkall -o ferdinand -I ../util -I ../matrix -I ../readwrite -I ../infinitesimal -I ../draw -I ../widget /usr/local/lib/ocaml/graphics.cmxa /usr/local/lib/ocaml/nums.cmxa /usr/local/lib/ocaml/str.cmxa /usr/local/lib/ocaml/unix.cmxa ../util/util.cmxa ../matrix/matrix.cmxa ../readwrite/readwrite.cmxa ../infinitesimal/infinitesimal.cmxa ../draw/draw.cmxa ../widget/widget.cmxa analogic.cmxa ferdinand_native.ml

exit 0

