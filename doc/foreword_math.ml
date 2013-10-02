



{!indexlist}



{C § }
{1 Foreword}
{C  }




{2 Aim}


These modules may be used as interactive calculation constructions when loaded in the [ocaml] pseudo_interpreter,
or as calculation libraries. 
They are designed to function in layers. The dispositions of layers may be modified.
The aim is neither to design a standard nor to deliver a software,
but to prove that any mathematician can program in  a few hundreds of hours
an ordinary chapter of a mathematics lecture.

Everything is written in the Ocaml language, the same language as that of utilization.
All the definitions are in fact macro-definitions.
The mathematician may thus modify up to her or his will the definitions
through any text editor and without having to learn any further language.

The principles of writing follow the following hierarchy in decreasing order:
+ the functions must be written in the same language as their utilization,
+ comfort of use is more important than speed,
+ the ease of transition between the different tradeoffs is more important than the tradeoffs themselves,
+ the person who is using the text must have the possibility to redefine or increase any part of it,
+ different tradeoffs between calculation time and accuracy must be available when the situation deserves it,
+ most of the parameters must be chosen at the time of eventual use,
+ if a calculation is difficultied by the crossing of a singularity, it is better to raise an exception rather than silently yield wacky results.


{2 Content}


The file [math.ml] is the concatenation of the following modules :
{{:./Math.Util.html} util.ml},
{{:./Math.Bary.html} bary.ml},
{{:./Math.Data.html} data.ml},
{{:./Math.Deg.html} deg.ml},
{{:./Math.Hash.html} hash.ml},
{{:./Math.Sparse_vector.html} sparse_vector.ml},
{{:./Math.Sparse_tensor.html} sparse_tensor.ml},
{{:./Math.Sparse_matrix.html} sparse_matrix.ml},
{{:./Math.Mat.html} mat.ml},
{{:./Math.Fft.html} fft.ml},
{{:./Math.Matrix.html} matrix.ml},
{{:./Math.Readwrite.html} readwrite.ml},
{{:./Math.Infinitesimal.html} infinitesimal.ml},
{{:./Math.Sci.html} sci.ml},
{{:./Math.Reduc.html} reduc.ml},
{{:./Math.Data2.html} data2.ml}.

There is no need for a graphic interface.

The file {{:../README} README} takes up on the differents uses of the archive set of mathematical tools for Ocaml
and describes different shell scripts found in this archive in order to help compilation.

The mathematician will find in these modules:

- constructions to practice real and integer matrix calculations,
{{:./Math.Matrix.html} matrix.ml},

- constructions to practice differential and integral calculations,
{{:./Math.Infinitesimal.html} infinitesimal.ml},

- constructions to approximate solutions of ordinary differential equations of first order,
{{:./Math.Infinitesimal.html} infinitesimal.ml},

- constructions to practice complex matrix calculations,
{{:./Math.Reduc.html} reduc.ml},

- constructions to practice calculations on univariate polynomials with real or complex coefficients,
{{:./Math.Reduc.html} reduc.ml},

- constructions to practice complex calculations in extended precision,
{{:./Math.Sci.html} sci.ml},

- usual constants,
{{:./Math.Data.Classical.html} module Classical of data.ml},
{{:./Math.Sci.html} sci.ml},

- modules for the coefficients of matrices (usual algebraic objects) - and for the indices:
{{:./Math.Data.html} data.ml}, {{:./Math.Data2.html} data2.ml}, 

- common convergence acceleration methods,
{{:./Math.Matrix.html} matrix.ml},
{{:./Math.Sci.html} sci.ml},

- simple random generators with values in common Lie groups,
{{:./Math.Matrix.html} matrix.ml},
{{:./Math.Reduc.html} reduc.ml},

- utilities to exchange vectors, lists and matrices with text files, binary files, some picture files and some sound files (in the Audacity format),
{{:./Math.Readwrite.html} readwrite.ml},

- calculation utilities for polynomial degrees,
{{:./Math.Deg.html} deg.ml},

- tools for handling sparse vectors, matrices and tensors,
the coefficients {e and indices} being polymorphic:
{{:./Math.Sparse_vector.html} sparse_vector.ml},
{{:./Math.Sparse_tensor.html} sparse_tensor.ml},
{{:./Math.Sparse_matrix.html} sparse_matrix.ml},

- tools for handling polymorphic vectors and tensors and matrices, indifferently full or sparse,
{{:./Math.Mat.html} mat.ml},

- tools for fast Fourier transorms,
{{:./Math.Fft.html} fft.ml},

- basic tools necessary to several modules,
{{:./Math.Util.html} util.ml},

- tools for handling weighted sets,
{{:./Math.Bary.html} bary.ml},

- tools for handling weighted hash tables,
{{:./Math.Hash.html} hash.ml},

- prefabricated pieces to quickly make other constructions,

- complete source codes permitting to resize everything to any use,

- a motley wildlife of various mathematical calculations; for example,
different function are presented to calculate the inverse of a real matrix,
to approximate the roots of a polynomial,
offering more or less slowlyness of execution and different levels of precision.


{2 Conventions}


The interdependency of the modules is described in {{:../depend.txt} depend.txt}

All the needed informations are in the present specifications documents
and the source codes. The need for further documentation can only be
the symptom of a weakness of the specification or of the program; 
a correction is thus necessary.

The calculations discussed here deal with general mathematics; unless otherwise,
the specifications and proofs of the algorithms are either immediately visible or
contained in the classical literature.
In case the specification of an algorithm is unknown, a warning is explicitly stated.

A function is {e sealed} if there is no sharing between the input variables and the output value.
This is the expected behavior of usual mathematical functions.
The present specification must allow in every case to decide if the programmed functions are sealed or not.
Enough copy functions are provided in order to seal any function, at the expense of increased calculation time and memory footprint.


{2 Comments}


The delivered functions are rough from workshop and require the usual care of use.

The plasticity of the Ocaml language allows to get as closed to the hardware as to
insure a moderate time of execution, as well as to reach high abstraction 
through mutually recursive definitions for some functions.

The rigidity of the typing allows a quick and easy setting of the programs.


These modules are distributed under the same licence as Ocaml.



{C § }
{1 Avant-propos}


{2 But}


Ces modules peuvent être utilisés comme constructions interactives de calcul quand chargés 
dans le pseudo_interpreteur [ocaml], ou comme bibliothèques de calcul.
Ils sont conçus pour fonctionner en couches. La disposition des couches peut être modifiée.
Le but n'est ni de rédiger une norme ni de fournir un logiciel,
mais de démontrer que n'importe quel mathématicien peut programmer en quelques centaines d'heures 
un chapitre courant d'un cours de mathématiques.

Tout est rédigé en langage Ocaml, le même langage que celui d'utilisation.
Toutes les définitions sont en fait des macro-définitions.
La mathématicienne ou le mathématicien peut ainsi modifier à sa guise les définitions 
à l'aide de n'importe quel éditeur de texte et sans avoir besoin d'apprendre un langage supplémentaire.

Les principes de rédaction suivent la hiérarchie suivante dans l'ordre décroissant :
+ les fonctions doivent être écrites dans le même langage que leur utilisation,
+ le confort d'utilisation est plus important que la vitesse,
+ la fluidité de passage entre les différents compromis est plus importante que les compromis eux-mêmes,
+ la personne qui utilise le texte doit pouvoir en redéfinir ou augmenter aisément n'importe quelle partie,
+ différents compromis entre temps de calcul et précision doivent être disponibles quand la situation le mérite,
+ la plupart des paramètres doivent être choisis au moment de l'utilisation finale,
+ si un calcul est mis en difficulté par la traversée d'une singularité, il vaut mieux lever une exception plutôt que de produire silencieusement des résultats farfelus.


{2 Contenu}


Le fichier [math.ml] est la concaténation des modules suivants :
{{:./Math.Util.html} util.ml},
{{:./Math.Bary.html} bary.ml},
{{:./Math.Data.html} data.ml},
{{:./Math.Deg.html} deg.ml},
{{:./Math.Hash.html} hash.ml},
{{:./Math.Sparse_vector.html} sparse_vector.ml},
{{:./Math.Sparse_tensor.html} sparse_tensor.ml},
{{:./Math.Sparse_matrix.html} sparse_matrix.ml},
{{:./Math.Mat.html} mat.ml},
{{:./Math.Fft.html} fft.ml},
{{:./Math.Matrix.html} matrix.ml},
{{:./Math.Readwrite.html} readwrite.ml},
{{:./Math.Infinitesimal.html} infinitesimal.ml},
{{:./Math.Sci.html} sci.ml},
{{:./Math.Reduc.html} reduc.ml},
{{:./Math.Data2.html} data2.ml}.

Il n'est nul besoin d'interface graphique.

Le fichier {{:../README} README} aborde les différents usages de l'ensemble de l'archive d'outils mathématiques pour Ocaml
et décrit différents scripts shell présents dans l'archive pour faciliter la compilation.

La mathématicienne ou le mathématicien trouvera dans ces modules :

- des outils permettant de pratiquer le calcul matriciel réel et entier,
{{:./Math.Matrix.html} matrix.ml},

- des outils permettant de pratiquer le calcul différentiel et intégral,
{{:./Math.Infinitesimal.html} infinitesimal.ml},

- des outils permettant d'approcher des solutions d'équations différentielles ordinaires du premier ordre,
{{:./Math.Infinitesimal.html} infinitesimal.ml},

- des outils permettant de pratiquer le calcul matriciel complexe,
{{:./Math.Reduc.html} reduc.ml},

- des outils permettant de pratiquer le calcul sur les polynômes à une variable à coefficients réels et complexes,
{{:./Math.Reduc.html} reduc.ml},

- des outils permettant de pratiquer le calcul complexe en précision étendue,
{{:./Math.Sci.html} sci.ml},

- des constantes usuelles,
{{:./Math.Data.Classical.html} module Classical de data.ml},
{{:./Math.Sci.html} sci.ml},

- des modules pour les coefficients des matrices (objets algébriques usuels) - et pour les indices :
{{:./Math.Data.html} data.ml}, {{:./Math.Data2.html} data2.ml}, 

- des méthodes usuelles d'accélération de convergence,
{{:./Math.Matrix.html} matrix.ml},
{{:./Math.Sci.html} sci.ml},

- des générateurs aléatoires simples à valeurs dans des groupes de Lie usuels,
{{:./Math.Matrix.html} matrix.ml},
{{:./Math.Reduc.html} reduc.ml},

- des utilitaires pour échanger vecteurs, listes et matrices avec des fichiers textes, des fichiers binaires, quelques fichiers d'images et des fichiers sons (au format Audacity),
{{:./Math.Readwrite.html} readwrite.ml},

- des outils de calcul pour les degrés des polynômes,
{{:./Math.Deg.html} deg.ml},

- des foncteurs pour la manipulation des vecteurs, matrices et tenseurs creux,
les coefficients {e et indices} étant polymorphes :
{{:./Math.Sparse_vector.html} sparse_vector.ml},
{{:./Math.Sparse_tensor.html} sparse_tensor.ml},
{{:./Math.Sparse_matrix.html} sparse_matrix.ml},

- des outils pour traiter les vecteurs, tenseur et matrices polymorphes, indifféremment pleins ou creux,
{{:./Math.Mat.html} mat.ml},

- des outils pour les transformées de Fourier rapides,
{{:./Math.Fft.html} fft.ml},

- des outils de bases nécessaires à plusieurs modules,
{{:./Math.Util.html} util.ml},

- des outils pour manipuler les ensembles à poids,
{{:./Math.Bary.html} bary.ml},

- des outils pour manipuler les tables de hachage à poids,
{{:./Math.Hash.html} hash.ml},

- des pièces préfabriquées permettant de construire rapidement d'autres outils,

- les codes sources complets permettant de tout retailler à la mesure d'une utilisation quelconque,

- une faune bigarrée de calculs mathématiques divers ; par exemple, 
différentes fonctions sont présentées pour calculer l'inverse d'une matrice réelle,
pour approcher les racines d'un polynôme,
offrant des exécutions plus ou moins lentes et différents niveaux de précision.


{2 Conventions}


L'interdépendance des modules est décrite dans {{:../depend.txt} depend.txt}

Toutes les informations nécessaires sont dans les présents documents de spécifications
et les codes-sources. Le besoin d'une documentation supplémentaire ne peut être 
que le symptôme de l'insuffisance de la spécification ou du programme ; 
une correction est alors nécessaire.

Les calculs abordés ici concernent les mathématiques générales ; sauf avis contraire,
les spécifications et démonstrations des algorithmes sont soit immédiatement visibles 
soit contenues dans la littérature classique.
Dans le cas où la spécification d'un algorithme est inconnue, 
un avertissement est explicitement marqué.

Une fonction est {e étanche} quand il n'y a aucun partage entre les variables fournies en entrée et la valeur obtenue en sortie.
C'est le comportement attendu des fonctions mathématiques habituelles. 
La présente spécification doit permettre de décider dans tous les cas si les fonctions programmées sont étanches ou pas.
Suffisamment de fonctions de recopies sont fournies pour étanchéifier n'importe quelle fonction, au prix d'un temps de calcul et d'un encombrement de mémoire augmentés.


{2 Commentaires}


Les fonctions délivrées sont brutes de fonderie et réclament les précautions habituelles d'utilisation.

La plasticité du langage Ocaml permet aussi bien de coller au matériel d'assez près 
pour assurer une durée d'exécution raisonnable, que de planer vers de hautes sphères d'abstraction
grâce à des définitions mutuellement récursives pour certaines fonctions.

La rigidité du typage permet une mise au point rapide et facile des programmes.


Ces modules sont distribués selon la même licence qu'Ocaml.


{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.4}
@version 0.4
@author Stéphane Grognet
@since septembre 2011, 2012




