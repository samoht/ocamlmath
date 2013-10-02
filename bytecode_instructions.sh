#!/bin/sh
#@(#)Prepare bytecode compilation for the calculation modules for Ocaml.
#@(#)Copyright Stéphane Grognet 2012
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4

#################
# Préliminaires #
#################

if [ $# -lt 2 ];
    then
    echo "Error: missing filename. Usage: sh $0 source_file target_file"
    exit 1
fi

if [ $# -gt 2 ];
    then
    echo "Error: more than two filenames. Usage: sh $0 source_file target_file"
    exit 1
fi

echo "--> Source file: "$1
echo "  The order of the files '*.ml' in the output instruction"
echo "     is approximative and may be changed by hand."
echo "  The list of dependencies may be incomplete"
echo "     since it is guessed by the 'depend.sh' script."
echo "--> Target file: "$2

#################################
# Building the dependencies     #
# Établissement des dépendances #
#################################

sh depend.sh $1 > tmp_hints

########################################################################################
# Making of the list of the non-standard libraries of the standard distribution.       #
# Fabrication de la liste des bibliothèques non standards de la distribution standard. #
########################################################################################

OCAMLBIN=`which ocaml`
BINDIR=`dirname $OCAMLBIN`
BASENAME=`dirname $BINDIR`"/lib/ocaml"

grep ".*[.]cma" tmp_hints | sed "s?^?${BASENAME}?p" > tmp_std_lib

#################################################################
# Organization of needed calculation modules.                   #
# Ordonnancement de la liste des modules de calcul nécessaires. #
#################################################################

ORDER=`grep "[-][>]" depend.txt | cut -d "-" -f 1`

order ( ) {
 ARGUMENT=`cat $1`
 for I in $ORDER; do
     echo $ARGUMENT | tr " " "\n" | grep -s $I
 done
}

#############################################################
# Making of the list of needed calculation modules          #
# Fabrication de la liste des modules de calcul nécessaires #
#############################################################

grep "level.*[.]ml" tmp_hints | cut -d":" -f 2 | awk '{ print $NF }' | sed -n "s/^/-I /p" | order > tmp_lib

################################
# Making of the instructions   #
# Fabrication des instructions #
################################

OPTIONS=`sort -u tmp_std_lib | paste -s - | expand | tr -s " "`
LIST=`grep -v $1 tmp_lib | paste -s - | expand | tr -s " "`
echo "cat "${LIST}" "$1" > compilation_source.ml"
echo "ocamlc.opt -linkall -o "$2" "${OPTIONS}" compilation_source.ml"

#############
# Cleaning  #
# Nettoyage #
#############

rm -f tmp_lib
rm -f tmp_std_lib
rm -f tmp_hints

exit 0

