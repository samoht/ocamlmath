#!/bin/sh
#@(#)Approximate search of dependencies for the calculation modules for Ocaml.
#@(#)Copyright Stéphane Grognet 2012
#@(#)Laboratoire de mathématiques Jean Leray UMR 6629 CNRS
#@(#)Fédération de recherche mathématique des Pays de la Loire
#@(#)IREM des Pays de la Loire - Université de Nantes
#@(#)version 0.4

#################
# Préliminaires #
#################

if [ $# -lt 1 ];
    then
    echo "Error: missing filename."
    exit 1
fi

if [ $# -gt 1 ];
    then
    echo "Error: more than one filename."
    exit 1
fi

echo "File inspected: "$1

##########################
# Making of the patterns #
# Fabrication des motifs #
##########################

awk 'BEGIN { FS = "[ \t]+" } { print $1 }' depend.defaults > inputs-defaults
awk 'BEGIN { FS = "[ \t]+" } { print $2 }' depend.defaults > outputs-byte
awk 'BEGIN { FS = "[ \t]+" } { print $3 }' depend.defaults > outputs-opt
awk 'BEGIN { FS = "[ \t]+" } { print $1 }' depend.conf > inputs-conf
awk 'BEGIN { FS = "[ \t]+" } { print $2 }' depend.conf > outputs-ml

####################################
# Functions of superficial parsing #
# Fonctions d'examen superficiel   #
####################################

first_depend_cma ( ) {
 COUNT="0"
# Required modules avoiding confusion between Str and String
# Modules requis en évitant la confusion entre Str et String
 TRY=`ocamldep.opt -modules $1 | grep -v String`
 for I in `cat inputs-defaults` ; do
    COUNT=$(( $COUNT + 1 ))
# Getting the modules of the standard distribution which are not part of the standard library.
# Récuperation des modules de la distribution standard hors de la bibliothèque standard.
    J=`sed -n "${COUNT}p" outputs-byte`
    sed -n "s?.*${I}.*?${J}?p" $1
# Similar work done otherwise.
# Travail similaire realise autrement.
    K=`echo $I | cut -d"[" -f 1`
    echo $TRY | sed -n "s?.*${K}.*?${J}?p"
 done
}

first_depend_cmxa ( ) {
 COUNT="0"
# Required modules avoiding confusion between Str and String
# Modules requis en évitant la confusion entre Str et String
 TRY=`ocamldep.opt -modules $1 | grep -v String`
 for II in `cat inputs-defaults` ; do
    COUNT=$(( $COUNT + 1 ))
# Getting the modules of the standard distribution which are not part of the standard library.
# Récuperation des modules de la distribution standard hors de la bibliothèque standard.
    JJ=`sed -n "${COUNT}p" outputs-opt`
    sed -n "s?.*${II}.*?${JJ}?p" $1
# Similar work done otherwise.
# Travail similaire réalisé autrement.
    KK=`echo $II | cut -d"[" -f 1`
    echo $TRY | sed -n "s?.*${KK}.*?${JJ}?p"
 done
}

second_depend ( ) {
 COUNT="0"
 TRY=`ocamldep.opt -I analogic -I deg -I draw -I infinitesimal -I matrix -I reduc -I readwrite -I sci -I sparse -I util -I widget $1`
 for I in `cat inputs-conf` ; do
    COUNT=$(( $COUNT + 1 ))
    J=`sed -n "${COUNT}p" outputs-ml`
    L=`echo $I | cut -d"[" -f 1`
# Éviter les messages d'exceptions, les liens html, les exemples d'instructions.
# Avoid exception messages, html links, instruction examples.
    grep -v "failwith" $1 | grep -v "[{][{][\!]" | grep -v "[\"][^\"]*${I}[^\"]*[\"]" | sed -n "s?.*${I}.*?${J}?p"
    if grep -qm1 "include.*$L" $1 
     then echo $J
    fi
# Similar work done otherwise but more safely ?
# Travail similaire réalisé autrement, mais plus sûrement ?
    K=`echo $J | cut -d"/" -f 2 | cut -d"." -f 1`
    IDEM=`basename $1`
    DIRECTORY=`dirname $1`
    MASTER=$DIRECTORY"/"$DIRECTORY".ml"
    echo $TRY | sed -n "s?.*${K}.*?${J}?p" | grep -v "$IDEM" | grep -v "$MASTER"
 done
}

#####################
# Recursive parsing #
# Examen recursif   #
#####################

# Results are approximate and can miss some information.
# On the concatenation modules "math.ml" and "graphicmath.ml", the results are not significant.
# Les résultats sont approximatifs et peuvent rater des informations.
# Sur les modules de concaténation "math.ml" et "graphicmath.ml", les résultats ne sont pas significatifs.

# OUTPUT INITIATION FOR cma
first_depend_cma $1 | sort -u > tmp-byte
# OUTPUT INITIATION FOR cmxa
first_depend_cmxa $1 | sort -u > tmp-opt

second_depend $1 | sort -u > tmp-ml
LEVEL=0
cat tmp-ml > tmp-depend-ml
# RECURSION INITIATION FOR ml
awk -vlevel=$LEVEL '{ print "level " level ": "$0 }' tmp-ml

condition ( ) {
 grep -qm 1 "[.]ml" $1
}

while condition tmp-ml ; do
    LEVEL=$(( $LEVEL + 1 ))
    echo -n "" > tmp-tmp
    echo -n "" > tmpdep
    for M in `cat tmp-ml` ; do
	second_depend $M | sort -u | grep -vf tmp-depend-ml >> tmp-tmp
	grep -vf tmp-depend-ml tmp-tmp > temporary
	cat temporary >> tmpdep
# OUTPUT FOR ml
	awk -v level=$LEVEL -v demand=$M '{ print "level "level": "demand" -> "$0 }' temporary
	cat tmp-tmp >> tmp-depend-ml
	first_depend_cma $M | sort -u >> tmp-byte
	first_depend_cmxa $M | sort -u >> tmp-opt
    done
    cat tmpdep > tmp-tmp
    sort -u tmp-tmp > tmp-ml
done

# OUTPUT FOR cma
sort -u tmp-byte
# OUTPUT FOR cmxa
sort -u tmp-opt

#############
# Cleaning  #
# Nettoyage #
#############

rm -f temporary
rm -f tmpdep
rm -f tmp-tmp
rm -f tmp-ml
rm -f tmp-byte
rm -f tmp-opt
rm -f tmp-depend-ml
rm -f outputs-byte
rm -f outputs-opt
rm -f outputs-ml
rm -f inputs-conf
rm -f inputs-defaults

exit 0

