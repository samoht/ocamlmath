



module Matrix = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module:

- constructions to practice matrix calculus with integer and real coefficients,

- common convergence acceleration methods,

- methods of block matrix calculus through variant types [float_or_array] and [int_or_array].


{2 Conventions}


Vectors are rows of scalars ([float] or [int]), of type [float array] or [int array].

A (bidimensional) matrix is a row vector,
each element of which being a row of the matrix.
Matrices are of type [float array array] or [int array array].


{2 Comments}


The delivered functions are rough from workshop and require the usual care of use.

The iterative refinement of the inverse of a matrix may be stopped by testing the difference
between the product and the identity or by detecting a too weak variation of the candidate, as suggested by William Kahan in :

[http://www.cs.berkeley.edu/~wkahan/Cantilever.pdf]

This author confesses to be the author of the HP15C calulator's high level mathematical functions manual 
in the page 5 of :

[http://www.cs.berkeley.edu/~wkahan/MktgMath.pdf]

This manual is a good introduction to error analysis.

The block calculus allows to replace blocks of matrices by scalars ([float] or [int]).
Such a simplification implies necessary ambiguities: in a multiplication,
the block reduced to a scalar behaves like a scalar matrix; in an addition,
it behaves like a saturated matrix. Only the scalar [0] behaves without ambiguity.

The matrices may be cut in blocks in a recursive way;
the obtained thickness is not limited: a square matrix of order 2 ^ n cut in a recursive way
in two parts (vertically and horizontally) would climb up to the thickness n - 1.

Increment the thickness slows down the elementary calculi on matrices.

A matrix can be invertible by block only if it is generic enough (random enough).
In this case, the calculus of the inverse with a block cutting up to the first level
may be quicker than usual inverse, for an order greater than about 1250;
the rows and columns are cut in 20 to 22 pieces between 1250 and 10000.
The increase in speed is due to the replacement of many blocks by the real [0],
which compensates the slow of block matrix operations.

On the laptop described below, the inversion of a big random matrix is about five times slower
than under [scilab] compilated with [atlas], about twenty percent slower than under [scilab] 
compilated without [atlas].


This module is distributed under the same licence as Ocaml.



{C § }



La mathématicienne ou le mathématicien trouvera dans ce module :

- des constructions permettant de pratiquer le calcul matriciel à coefficients entiers et réels,

- des méthodes usuelles d'accélération de convergence,

- des méthodes de calcul matriciel par blocs, à l'aide de types variants [float_or_array] et [int_or_array].


{2 Conventions}


Les vecteurs sont des lignes de scalaires ([float] ou [int]), de type [float array] ou [int array].

Une matrice (bidimensionnelle) est un vecteur ligne 
dont chaque élément est une ligne de la matrice. 
Les matrices sont de type [float array array] ou [int array array].


{2 Commentaires}


Les fonctions délivrées sont brutes de fonderie et réclament les précautions habituelles d'utilisation.

Le raffinement itératif de l'inverse d'une matrice peut être arrêté par test de l'écart entre 
le produit et l'identité ou par variation du candidat trop faible, comme proposé par William Kahan dans :

[http://www.cs.berkeley.edu/~wkahan/Cantilever.pdf]

Cet auteur avoue être l'auteur du manuel des fonctions mathématiques de haut niveau 
de la calculette HP15C à la page 5 de :

[http://www.cs.berkeley.edu/~wkahan/MktgMath.pdf]

Ce manuel est une bonne introduction à l'analyse d'erreur.

Le calcul par blocs autorise de remplacer des blocs de matrices par des scalaires ([float] ou [int]).
Une telle simplification entraîne d'obligatoires ambiguïtés : dans une multiplication, 
le bloc réduit à un scalaire se comporte comme une matrice scalaire ; dans une addition, 
il se comporte comme une matrice saturée. Seul le scalaire [0] se comporte sans ambiguïté.

Les matrices peuvent être découpées en blocs de manière récursive ;
l'épaisseur obtenue n'est pas limitée : une matrice carrée d'ordre 2 ^ n découpée récursivement 
en deux (verticalement et horizontalement) monterait jusqu'à l'épaisseur n - 1.

Incrémenter l'épaisseur ralentit les calculs élémentaires sur les matrices.

Une matrice ne peut être bloc-inversible que si elle est suffisamment générique (assez aléatoire).
Dans ce cas, le calcul de l'inverse avec un découpage par blocs au premier niveau peut
être plus rapide que l'inverse ordinaire, pour un ordre supérieur à 1250 environ ; 
les lignes et colonnes sont découpées en 20 à 22 morceaux entre 1250 et 10000.
L'augmentation de vitesse est due au remplacement de beaucoup de blocs par le réel [0],
ce qui compense la lenteur des opérations sur les matrices par blocs.

Sur la machine portable :

{[
CPU: AMD Athlon(tm) II Dual-Core M300 (1994.94-MHz K8-class CPU)
  Origin = "AuthenticAMD"  Id = 0x100f62  Stepping = 2
  Features=0x178bfbff<FPU,VME,DE,PSE,TSC,MSR,PAE,MCE,CX8,APIC,SEP,MTRR,PGE,MCA,CMOV,PAT,PSE36,CLFLUSH,MMX,FXSR,SSE,SSE2,HTT>
  Features2=0x802009<SSE3,MON,CX16,POPCNT>
  AMD Features=0xee500800<SYSCALL,NX,MMX+,FFXSR,Page1GB,RDTSCP,LM,3DNow!+,3DNow!>
  AMD Features2=0x377f<LAHF,CMP,SVM,ExtAPIC,CR8,ABM,SSE4A,Prefetch,OSVW,IBS,SKINIT,WDT>
  TSC: P-state invariant
real memory  = 4294967296 (4096 MB)
avail memory = 4111618048 (3921 MB)
ACPI APIC Table: <HP     3652    >
FreeBSD/SMP: Multiprocessor System Detected: 2 CPUs
FreeBSD/SMP: 1 package(s) x 2 core(s)
 cpu0 (BSP): APIC ID:  0
 cpu1 (AP): APIC ID:  1
]}

l'inversion d'une grande matrice aléatoire est environ cinq fois plus lente que sous [scilab] compilé 
avec [atlas], environ vingt pour cent plus lente que sous [scilab] compilé sans [atlas].


Ce module est distribué selon la même licence qu'Ocaml.

{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.5}
*)
(**
@version 0.5
*)
(**
@author Stéphane Grognet
*)
(**
@since 2011, 2012, 2013
*)




(** {C § } *)
(** 
{1 Constructions minimales}
{1 Minimal constructions}
*)
(** {C  } *)




open Util ;;




(** {C § } *)
(** 
{1 Constructions polymorphes}
{1 Polymorphic constructions}
*)
(** {C  } *)




(** Polymorphic functions are reputed to be slow.

Les fonctions polymorphes sont réputés lentes.*)
(** {C § } *)


(** {v matrix_eq equality matrix1 matrix2 v} *)
let matrix_eq = fun eq m n ->
 let p = Array.mapi ( fun i x -> Util.array_eq eq m.(i) x ) n in
  Array.fold_left ( && ) true p ;;

(** {v insert matrix row column element v} *)
let insert = fun m i j x ->
 (m.(i).(j) <- x) ;;

(** {v numrows matrix v} *)
let numrows = function m ->
 Array.length m ;;

(** {v numcolumns matrix v} *)
let numcolumns = function m ->
 if Array.length m = 0 then 0 else Array.length m.(0) ;;

(** {v slow_numcolumns matrix v} slow_numcolumns gives the number of columns even for lacunar matrices:

slow_numcolumns donne le nombre des colonnes même pour les matrices lacunaires. *)
let slow_numcolumns = function m ->
 let long = numrows m in
  if long = 0 then 0 else 
   let accu = ref 0 in
    for i = 0 to long - 1 do
     accu := Util.int_max !accu ( Array.length m.(i) )
    done ;
    !accu ;;


(** {v matrix_max_by_row matrix v} *)
let matrix_max_by_row = function m ->
 Array.map Util.vector_max m ;;

(** {v matrix_max_by_column matrix v} *)
let matrix_max_by_column = function m ->
 matrix_max_by_row ( Util.transpose m ) ;;

(** {v matrix_max matrix v} *)
let matrix_max = function m ->
 Util.vector_max (Array.map Util.vector_max m) ;;

(** {v matrix_min_by_row matrix v} *)
let matrix_min_by_row = function m ->
 Array.map Util.vector_min m ;;

(** {v matrix_min_by_column matrix v} *)
let matrix_min_by_column = function m ->
 matrix_min_by_row ( Util.transpose m ) ;;

(** {v matrix_min matrix v} *)
let matrix_min = function m ->
 Util.vector_min (Array.map Util.vector_min m) ;;

(** {v vector_copy copy_function vector v} *)
let vector_copy = fun copy v ->
 if Array.length v = 0 then [| |]
 else
  begin
   let r = Array.length v in
    let vv = Array.make r v.(0) in
     for i = 0 to r - 1 do
      vv.(i) <- copy v.(i)
     done ;
     vv
  end ;;

(** {v matrix_copy copy_function matrix v} *)
let matrix_copy = fun copy m ->
 let r = numrows m and c = numcolumns m in
  if ( r <= 0 ) or ( c <= 0 ) then [| [| |] |]
  else
   begin
    let mm = Array.make_matrix r c m.(0).(0) in
     for i = 0 to r - 1 do
      mm.(i) <- vector_copy copy m.(i)
     done ;
     mm
   end ;;


(** {v matrix_find_last equality matrix v} matrix_find_last returns [[|-1;-1|]] if it does not find:

matrix_find_last retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_find_last = fun eq x m ->
 let r = numrows m and index = ref (-1) and indice = ref (-1) in
  let i = ref (r - 1) in
   while !i >= 0 do
    indice := Util.vector_find_last eq x m.(!i) ;
    if !indice <> -1 then (index := !i ; i := -1) else i := !i - 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_find_last_first equality matrix v} matrix_find_last returns [[|-1;-1|]] if it does not find:

matrix_find_last_first retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_find_last_first = fun eq x m ->
 let r = numrows m and index = ref (-1) and indice = ref (-1) in
  let i = ref (r - 1) in
   while !i >= 0 do
    indice := Util.vector_find_first eq x m.(!i) ;
    if !indice <> -1 then (index := !i ; i := -1) else i := !i - 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_find_first equality matrix v} matrix_find_first returns [[|-1;-1|]] if it does not find:

matrix_find_first retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_find_first = fun eq x m ->
 let r = numrows m and index = ref (-1) and indice = ref (-1) and i = ref 0 in
  while  !i < r do
   indice := Util.vector_find_first eq x m.(!i) ;
   if !indice <> -1 then (index := !i ; i := r) else i := !i + 1 ; 
  done ;
  [| !index ; !indice |] ;;


(** {v matrix_find_first_last equality matrix v} matrix_find_first returns [[|-1;-1|]] if it does not find:

matrix_find_first_last retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_find_first_last = fun eq x m ->
 let r = numrows m and index = ref (-1) and indice = ref (-1) and i = ref 0 in
  while  !i < r do
   indice := Util.vector_find_last eq x m.(!i) ;
   if !indice <> -1 then (index := !i ; i := r) else i := !i + 1 ; 
  done ;
  [| !index ; !indice |] ;;


(** {v matrix_find_twin equality matrix v} matrix_find_first returns [[|-1;-1|]] if it does not find:

matrix_find_twin retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_find_twin = fun eq x m ->
 let r = numrows m and index = ref (-1) and indice = ref (-1) and i = ref 0 in
  while  !i <= min (r - 1) ( int_of_float ( ceil ( (float r) /. 2. ) ) ) do
   indice := Util.vector_find_twin eq x m.(!i) ;
   if !indice <> -1 then (index := !i ; i := 1 + r) else
    begin
     let j = r - 1 - !i in
      indice := Util.vector_find_twin eq x m.(j) ;
      if !indice <> -1 then (index := j ; i := 1 + r) else i := !i + 1 ; 
    end
  done ;
  [| !index ; !indice |] ;;


(** {v matrix_find_all equality matrix v} matrix_find_all returns [[||]] if it does not find:

matrix_find_all retourne [[||]] s'il ne trouve pas.*)
let matrix_find_all = fun eq x m ->
 let r = numrows m and result = ref [||] and index = ref [||] and indice = ref [||] in
  for i = 0 to r - 1 do
   indice := Util.vector_find_all eq x m.(i) ;
   if !indice <> [||] then 
    begin
     index := Array.map (function a -> [| i ; a|]) !indice ; 
     result := Array.append !result !index 
    end
  done ;
  !result ;;

(** {v extract_column column matrix v} *)
let extract_column = fun (i:int) m ->
 let mm = ref[||] in
  mm := Array.append !mm (Array.map (function a -> List.nth (Array.to_list a) i) m) ;
 !mm ;;

(** {v float_extract_column column matrix v} *)
let float_extract_column = fun (j:int) (m:float array array) ->
 let n = Array.length m in
  let v = Array.make n 0. in
   for i = 0 to n - 1 do
    v.(i) <- m.(i).(j) ;
   done ;
   v ;;


(** {v affect_column column_index vector matrix v} The size of the vector may be bigger than the number of rows of the matrix. The modification takes place on the input matrix.

La taille du vecteur peut dépassr le nombre de lignes de la matrice.
La modification a lieu sur la matrice entrée. *)
let affect_column = fun (j:int) v m ->
 let r = numrows m in
  for i = 0 to r - 1 do
   m.(i).(j) <- List.nth (Array.to_list v) i
  done ;;


(** {v float_affect_column column_index vector matrix v} The size of the vector may be bigger than the number of rows of the matrix. The modification takes place on the input matrix.

La taille du vecteur peut dépassr le nombre de lignes de la matrice. La modification a lieu sur la matrice entrée. *)
let float_affect_column = fun (j:int) (v:float array) (m:float array array) ->
 let r = numrows m in
  for i = 0 to r - 1 do
   m.(i).(j) <- v.(i)
  done ;;

(** {v extract_diag matrix v} *)
let extract_diag = function m ->
 let mm = ref[||] 
 and r = min (numrows m) (numcolumns m) in
  for i = 0 to r - 1 do
   mm := Array.append !mm [|m.(i).(i)|]
  done ;
 !mm ;;


(** {v exchange_row index1 index2 matrix v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let exchange_row = fun i j m ->
 let aux = m.(i) in
  m.(i) <- m.(j) ;
  m.(j) <- aux ;
 m ;;

(** {v exchange_column index1 index2 matrix v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let exchange_column = fun i j m ->
 let aux = extract_column i m
 and auxil = extract_column j m in
  affect_column j aux m ;
  affect_column i auxil m ;
  m ;;


(** {v sub_matrix matrix beg-row end-row beg-col end-col v} *)
let sub_matrix = fun m i ii j jj ->
(** i et j : beginning: début ; ii et jj : end: fin *)
 let mm = Array.make_matrix (ii - i + 1) (jj - j + 1) m.(0).(0) in
  for row = i to ii do
   mm.(row-i) <- Array.sub m.(row) j (jj - j + 1) 
  done ;
 mm ;;


(** The functions [inline] and [slice] are reciprocal of one another.

Les fonctions [inline] et [slice] sont réciproques l'une de l'autre. *)

(** {v inline matrix v} *)
let inline = function m ->
 let r = Array.length m
 and x = m.(0) in
  let c = Array.length x in
   let v = Array.make ( r * c ) x.(0)
   and cc = pred c in
    for i = 0 to pred r do
     let ir = i * r
     and row = m.(i) in
      for j = 0 to cc do
       v.( ir + j ) <- row.(j)
      done 
    done ;
    v ;;

(** {v slice vector v} *)
let slice = fun (n:int) v ->
 let l = Array.length v in
  if l mod n != 0 then failwith "The number of lines must divide the length of the vector in Matrix.slice." ;
   let c = l / n in
    let m = Array.make_matrix n c v.(0)
    and cc =pred c in
     for i = 0 to pred n do
      let row = m.(i)
      and ni = n * i in
       for j = 0 to cc do
        row.(j) <- v.( ni + j )
       done
     done ;
     m ;;





(** {C § } *)
(**
{1 Constructions}
*)
(** {C  } *)




(** The variants bis and ter introduce different behaviours for non square matrices.

Les variantes bis et ter introduisent des comportements différents pour les matrices non carrées. *)

(** {C § } *)


(** {v zeros_float matrix v} *)
let zeros_float = function (m:float array array) ->
 let r = numrows m and c = numcolumns m in
 Array.make_matrix r c 0. ;;

(** {v null_float numrows numcolumns v} *)
let null_float = fun (r:int) (c:int) ->
 Array.make_matrix r c 0. ;;

(** {v ones_float matrix v} *)
let ones_float = function (m:float array array) ->
 let r = numrows m and c = numcolumns m in
 Array.make_matrix r c 1. ;;

(** {v units_float matrix v} *)
let units_float = fun (r:int) (c:int) ->
 Array.make_matrix r c 1. ;;

(** {v fill_float matrix v} *)
let fill_float = fun (m:float array array) (x:float) ->
 Array.make_matrix (Array.length m) (Array.length m.(0)) x ;;

(** {v saturate_float numrows numcolumns real v} *)
let saturate_float = fun (r:int) (c:int) (x:float) ->
 Array.make_matrix r c x ;;


(** {v identity_float numrows numcolumns v} *)
let identity_float = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0. in
  for i = 0 to ( min r c ) - 1 do
   m.(i).(i) <- 1.
  done ;
  m ;;

(** {v identity_float_bis numrows numcolumns v} *)
let identity_float_bis = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0.
 and delta = Util.int_max 0 (c - r) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).( i + delta ) <- 1.
  done ;
  m ;;

(** {v identity_float_ter numrows numcolumns v} *)
let identity_float_ter = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0.
 and delta = Util.int_max 0 (r - c) in
  for i = 0 to ( min r c ) - 1 do
   m.( i + delta ).(i) <- 1.
  done ;
  m ;;


(** {v rank_float_matrix numrows numcolumns rank v} *)
let rank_float_matrix = fun (r:int) (c:int) (rank:int) ->
 let m = Array.make_matrix r c 0. in
  for i = 0 to rank - 1 do
   m.(i).(i) <- 1.
  done ;
  m ;;


(** {v eye_float matrix v} *)
let eye_float = function (m:float array array) ->
 identity_float (Array.length m) (Array.length m.(0)) ;;

(** {v eye_float_bis matrix v} *)
let eye_float_bis = function (m:float array array) ->
 identity_float_bis (Array.length m) (Array.length m.(0)) ;;

(** {v eye_float_ter matrix v} *)
let eye_float_ter = function (m:float array array) ->
 identity_float_ter (Array.length m) (Array.length m.(0)) ;;


(** {v scal_float numrows numcolumns real v} *)
let scal_float = fun (r:int) (c:int) (x:float) ->
  let m = Array.make_matrix r c 0. in
   for i = 0 to ( min r c ) - 1 do
    m.(i).(i) <- x
   done ;
 m ;;

(** {v scal_float_bis numrows numcolumns real v} *)
let scal_float_bis = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and delta = Util.int_max 0 (c - r) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).( i + delta ) <- x
  done ;
  m ;;

(** {v scal_float_ter numrows numcolumns real v} *)
let scal_float_ter = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and delta = Util.int_max 0 (r - c) in
  for i = 0 to ( min r c ) - 1 do
   m.( i + delta ).(i) <- x
  done ;
  m ;;


(** {v diag_float vector v} *)
let diag_float = function (v:float array) ->
 let r = Array.length v in
  let m = Array.make_matrix r r 0. in
   for i = 0 to r - 1 do
    m.(i).(i) <- v.(i)
   done ;
 m ;;

(** {v diag_inv_float vector v} *)
let diag_inv_float = function (v:float array) ->
 let r = Array.length v in
  let m = Array.make_matrix r r 0. in
   for i = 0 to r - 1 do
    begin
     let x = v.(i) in
      if x == 0. then failwith "Division by zero in Matrix.diag_inv_float." ;
      m.(i).(i) <- 1. /. x
    end
   done ;
 m ;;

(** {v matrix_float_permu size index1 index2 v} *)
let matrix_float_permu = fun n i j ->
 let m = identity_float n n in
   m.(i).(i) <- 0. ;
   m.(j).(j) <- 0. ;
   m.(i).(j) <- 1. ;
   m.(j).(i) <- 1. ;
 m ;;

(** {v oblique_float numrows numcolumns v} *)
let oblique_float = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0.
 and s = ( min r c ) - 1 in
  for i = 0 to s do
   m.(i).(s - i) <- 1.
  done ;
  m ;;

(** {v oblique_float_bis numrows numcolumns v} *)
let oblique_float_bis = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0.
 and s = ( min r c ) - 1
 and delta = Util.int_max 0 (c - r) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).( s - i + delta ) <- 1.
  done ;
  m ;;

(** {v oblique_float_ter numrows numcolumns v} *)
let oblique_float_ter = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0.
 and s = ( min r c ) - 1
 and delta = Util.int_max 0 (r - c) in
  for i = 0 to ( min r c ) - 1 do
   m.( i + delta ).(s - i) <- 1.
  done ;
  m ;;


(** {v antiscal_float numrows numcolumns real v} *)
let antiscal_float = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and s = ( min r c ) - 1 in
  for i = 0 to s do
   m.(i).(s-i) <- x
  done ;
  m ;;

(** {v antiscal_float_bis numrows numcolumns real v} *)
let antiscal_float_bis = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and s = ( min r c ) - 1
 and delta = Util.int_max 0 (c - r) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).( s - i + delta ) <- x
  done ;
  m ;;

(** {v antiscal_float_ter numrows numcolumns real v} *)
let antiscal_float_ter = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and s = ( min r c ) - 1
 and delta = Util.int_max 0 (r - c) in
  for i = 0 to ( min r c ) - 1 do
   m.( i + delta ).(s-i) <- x
  done ;
  m ;;


(** {v antidiag_float numrows numcolumns v} *)
let antidiag_float = function (v:float array) ->
 let r = Array.length v in
  let m = Array.make_matrix r r 0. in
   for i = 0 to r - 1 do
    m.(i).(r - 1 - i) <- v.(i)
   done ;
   m ;;

(** {v antidiag_inv_float numrows numcolumns v} *)
let antidiag_inv_float = function (v:float array) ->
 let r = Array.length v in
  let m = Array.make_matrix r r 0. in
   for i = 0 to r - 1 do
    begin
     let x = v.(i) in
      if x == 0. then failwith "Division by zero in Matrix.antidiag_inv_float." ;
      m.(i).(r - 1 - i) <- 1. /. x
    end
   done ;
   m ;;


(** {v matrix_float_serret_frenet vector v} *)
let matrix_float_serret_frenet = function (v:float array) ->
 let r = Array.length v in
  let rr = r + 1
  and r_r = r - 1 in
   let m = Array.make_matrix rr rr 0. in
    m.(0).(1) <- v.(0) ;
    for i = 1 to r_r do
     let row = m.(i) in
      row.( i - 1 ) <- -. v.( i - 1 ) ;
      row.( i + 1 ) <- v.(i) ;
    done ;
    m.(r).(r_r) <- -. v.(r_r) ;
    m ;;

(** {v vector_float_random size range v} *)
let vector_float_random = fun n x ->
 let v = Array.make n 0. in
  for i = 0 to n - 1 do
   v.(i) <- Random.float x
 done ;
 v ;;

(** {v vector_float_sign_random size v} *)
let vector_float_sign_random = function n ->
 let v = Array.make n 0. in
  for i = 0 to n - 1 do
   v.(i) <- if Random.bool() then 1. else -1.
 done ;
 v ;;

(** {v vector_float_random_progression size range v} *)
let vector_float_random_progression = fun n x ->
 let v = vector_float_random n x in
  for i = 1 to n - 1 do
   v.(i) <- v.(i - 1) +. v.(i)
  done ;
  v ;;

(** {v vector_float_bal_random size range v} *)
let vector_float_bal_random = fun n x ->
 let v = Array.make n 0.
 and y = x *. 2. in
  for i = 0 to n - 1 do
   v.(i) <- (Random.float y) -. x
 done ;
 v ;;



(** {v float_open_equal_subdivision beginning number_of_steps end v} *)
let float_open_equal_subdivision = fun (a:float) (n:int) (b:float) ->
 let x = (b -. a) /. (1. +. float n) in
  let v = Array.make n (a +. x) in
   for i = 1 to n - 1 do
    v.(i) <- v.(i - 1) +. x
   done ;
   v ;;

(** {v float_closed_equal_subdivision beginning number_of_steps end v} *)
let float_closed_equal_subdivision = fun (a:float) (n:int) (b:float) ->
 let x = (b -. a) /. ( (float n) -. 1. )
 and nn = n / 2 in
  let v = Array.make n a in
   for i = 1 to nn do
    v.(i) <- v.(i - 1) +. x
   done ;
   v.(n - 1) <- b ;
   for i = n - 2 downto nn + 1 do
    v.(i) <- v.(i + 1) -. x
   done ;
   v ;;


(** {v float_closed_open_equal_subdivision beginning number_of_steps end v} *)
let float_closed_open_equal_subdivision = fun (a:float) (n:int) (b:float) ->
 let x = (b -. a) /. (float n) in
  let v = Array.make n a in
   for i = 1 to n - 1 do
    v.(i) <- v.(i - 1) +. x
   done ;
   v ;;

(** {v float_open_closed_equal_subdivision beginning number_of_steps end v} *)
let float_open_closed_equal_subdivision = fun (a:float) (n:int) (b:float) ->
 let x = (b -. a) /. (float n) in
  let v = Array.make n b in
   for i = n - 2 downto 0 do
    v.(i) <- v.(i + 1) -. x
   done ;
   v ;;


(** {v float_open_equal_range beginning step end v} *)
let float_open_equal_range = fun (a:float) (x:float) (b:float) ->
 let n = Util.round ( (b -. a) /. x ) - 1 in
  let v = Array.make n (a +. x) in
   for i = 1 to n - 1 do
    v.(i) <- v.(i - 1) +. x
   done ;
   v ;;

(** {v float_closed_equal_range beginning step end v} *)
let float_closed_equal_range = fun (a:float) (x:float) (b:float) ->
 let n = Util.round ( (b -. a) /. x ) + 1 in
  let nn = n / 2 in
   let v = Array.make n a in
    for i = 1 to nn do
     v.(i) <- v.(i - 1) +. x
    done ;
    v.(n - 1) <- b ;
    for i = n - 2 downto nn + 1 do
     v.(i) <- v.(i + 1) -. x
    done ;
    v ;;

(** {v float_closed_open_equal_range beginning step end v} *)
let float_closed_open_equal_range = fun (a:float) (x:float) (b:float) ->
 let n = Util.round ( (b -. a) /. x ) in
  let v = Array.make n a in
   for i = 1 to n - 1 do
    v.(i) <- v.(i - 1) +. x
   done ;
   v ;;

(** {v float_open_closed_equal_range beginning step end v} *)
let float_open_closed_equal_range = fun (a:float) (x:float) (b:float) ->
 let n = Util.round ( (b -. a) /. x ) in
  let v = Array.make n b in
   for i = n - 2 downto 0 do
    v.(i) <- v.(i + 1) -. x
   done ;
   v ;;


(** {v float_open_random_subdivision beginning number_of_steps end v} *)
let float_open_random_subdivision = fun (a:float) (n:int) (b:float) ->
 let v = vector_float_random_progression (n + 1) 1. in
  let x = (b -. a) /. v.(n) in
   for i = 0 to n - 1 do
    v.(i) <- a +. x *. v.(i)
   done ;
   Array.sub v 0 n ;;

(** {v float_closed_random_subdivision beginning number_of_steps end v} *)
let float_closed_random_subdivision = fun (a:float) (n:int) (b:float) ->
 let v = vector_float_random_progression (n - 1) 1. in
  let x = (b -. a) /. v.(n - 2) in
   for i = n - 2 downto 1 do
    v.(i) <- a +. x *. v.(i - 1)
   done ;
   v.(0) <- a ;
   Array.append v [|b|] ;;

(** {v float_closed_open_random_subdivision beginning number_of_steps end v} *)
let float_closed_open_random_subdivision = fun (a:float) (n:int) (b:float) ->
 let v = vector_float_random_progression n 1. in
  let x = (b -. a) /. v.(n - 1) in
   for i = n - 1 downto 1 do
    v.(i) <- a +. x *. v.(i - 1)
   done ;
   v.(0) <- a ;
   v ;;

(** {v float_open_closed_random_subdivision beginning number_of_steps end v} *)
let float_open_closed_random_subdivision = fun (a:float) (n:int) (b:float) ->
 let v = vector_float_random_progression n 1. in
  let x = (b -. a) /. v.(n - 1) in
   for i = 0 to n - 2 do
    v.(i) <- a +. x *. v.(i)
   done ;
   v.(n - 1) <- b ;
   v ;;



(** {v matrix_float_random rows columns range v} *)
let matrix_float_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and cc = c - 1 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = 0 to cc do
     row.(j) <- Random.float x
    done
  done ;
  m ;;

(** {v matrix_float_bal_random rows columns range v} *)
let matrix_float_bal_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. 
 and y = x *. 2.
 and cc = c - 1 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = 0 to cc do
     row.(j) <- (Random.float y) -. x
    done
  done ;
  m ;;

(** {v upper_trig_float_random rows columns range v} *)
let upper_trig_float_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and cc = c - 1 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = i to cc do
     row.(j) <- Random.float x
    done
  done ;
  m ;;

(** {v upper_trig_float_bal_random rows columns range v} *)
let upper_trig_float_bal_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. 
 and y = x *. 2.
 and cc = c - 1 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = i to cc do
     row.(j) <- (Random.float y) -. x
    done
  done ;
  m ;;

(** {v lower_trig_float_random rows columns range v} *)
let lower_trig_float_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = 0 to i do
     row.(j) <- Random.float x
    done
  done ;
  m ;;

(** {v lower_trig_float_bal_random rows columns range v} *)
let lower_trig_float_bal_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. 
 and y = x *. 2. in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = 0 to i do
     row.(j) <- (Random.float y) -. x
    done
  done ;
  m ;;

(** {v upper_nil_float_random rows columns range v} *)
let upper_nil_float_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and cc = c - 1 in
  for i = 0 to r - 2 do
   let row = m.(i) in
    for j = i + 1 to cc do
     row.(j) <- Random.float x
    done
  done ;
  m ;;

(** {v upper_nil_float_bal_random rows columns range v} *)
let upper_nil_float_bal_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. 
 and y = x *. 2.
 and cc = c - 1 in
  for i = 0 to r - 2 do
   let row = m.(i) in
    for j = i + 1 to cc do
     row.(j) <- (Random.float y) -. x
    done
  done ;
  m ;;

(** {v lower_nil_float_random rows columns range v} *)
let lower_nil_float_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. in
  for i = 1 to r - 1 do
   let row = m.(i) in
    for j = 0 to i - 1 do
     row.(j) <- Random.float x
    done
  done ;
  m ;;

(** {v lower_nil_float_bal_random rows columns range v} *)
let lower_nil_float_bal_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. 
 and y = x *. 2. in
  for i = 1 to r - 1 do
   let row = m.(i) in
    for j = 0 to i - 1 do
     row.(j) <- (Random.float y) -. x
    done
  done ;
  m ;;


(** {v upper_unip_float_random rows columns range v} *)
let upper_unip_float_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0.
 and cc = c - 1 in
  for i = 0 to r - 2 do
   let row = m.(i) in
    row.(i) <- 1. ;
    for j = i + 1 to cc do
     row.(j) <- Random.float x
    done
  done ;
  let rr = pred r in
   m.(rr).(rr) <- 1. ;
   m ;;

(** {v upper_unip_float_bal_random rows columns range v} *)
let upper_unip_float_bal_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. 
 and y = x *. 2.
 and cc = c - 1 in
  for i = 0 to r - 2 do
   let row = m.(i) in
    row.(i) <- 1. ;
    for j = i + 1 to cc do
     row.(j) <- (Random.float y) -. x
    done
  done ;
  let rr = pred r in
   m.(rr).(rr) <- 1. ;
   m ;;

(** {v lower_unip_float_random rows columns range v} *)
let lower_unip_float_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. in
  m.(0).(0) <- 1. ;
  for i = 1 to r - 1 do
   let row = m.(i) in
    for j = 0 to i - 1 do
     row.(j) <- Random.float x
    done ;
    row.(i) <- 1.
  done ;
  m ;;

(** {v lower_unip_float_bal_random rows columns range v} *)
let lower_unip_float_bal_random = fun (r:int) (c:int) (x:float) ->
 let m = Array.make_matrix r c 0. 
 and y = x *. 2. in
  m.(0).(0) <- 1. ;
  for i = 1 to r - 1 do
   let row = m.(i) in
    for j = 0 to i - 1 do
     row.(j) <- (Random.float y) -. x
    done ;
    row.(i) <- 1.
  done ;
  m ;;


(** {v matrix_float_binome rank v} The Pascal triangle in floating point output.

Le triangle de Pascal en sortie réelle. *)
let rec matrix_float_binome = function n ->
 match n with
 | 0 -> [|[|1.|]|]
 | 1 -> [| [|1.|] ; [| 1. ; 1. |] |]
 | _ -> let m = matrix_float_binome (n - 1)
  and v = Array.make ( n + 1 ) 1. in
   let row = m.( n - 1 ) in
    for i = 1 to n - 1 do
     v.(i) <- row.(i - 1) +. row.(i)
    done ;
    Array.append m [| v |] ;;


(** {v matrix_float_Vandermonde vector v} The real Vandermonde matrix.

La matrice de Vandermonde réelle. *)
let matrix_float_Vandermonde = function v ->
 let n = Array.length v in
  let m = Array.make_matrix n n 1. in
   for i = 0 to n - 1 do
    let row = m.(i)
    and xi = v.(i) in
     for j = 1 to n - 1 do
      row.(j) <- row.(j - 1) *. xi
     done
   done ;
   m ;;


(** {v matrix_float_nil order v} The model nilpotent real matrix of order n and rank n - 1.

La matrice nilpotente modèle réelle d'ordre n et de rang n - 1. *)
let matrix_float_nil = function n ->
 let m = Array.make_matrix n n 0. in
  for i = 0 to n - 2 do
   m.(i).(i + 1) <- 1.
  done ;
  m ;;


(** {v matrix_float_Jordan order coefficient v} The real Jordan block of order n.

Le bloc de Jordan réel d'ordre n. *)
let matrix_float_Jordan = fun n x ->
 let m =Array.make_matrix n n 0. in
  for i = 0 to n - 2 do
   let row = m.(i) in
    row.(i) <- x ;
    row.(i + 1) <- 1.
  done ;
  m.(n - 1).(n - 1) <- x ;
  m ;;



(** {v zeros_int matrix v} *)
let zeros_int = function (m:int array array) ->
 let r = numrows m and c = numcolumns m in
 Array.make_matrix r c 0 ;;

(** {v null_int numrows numcolumns v} *)
let null_int = fun (r:int) (c:int) ->
 Array.make_matrix r c 0 ;;

(** {v ones_int matrix v} *)
let ones_int = function (m:int array array) ->
 let r = numrows m and c = numcolumns m in
 Array.make_matrix r c 1 ;;

(** {v units_int numrows numcolumns v} *)
let units_int = fun (r:int) (c:int) ->
 Array.make_matrix r c 1 ;;

(** {v fill_int matrix v} *)
let fill_int = fun (m:int array array) (x:int) ->
 Array.make_matrix (Array.length m) (Array.length m.(0)) x ;;

(** {v saturate_int numrows numcolumns integer v} *)
let saturate_int = fun (r:int) (c:int) x ->
 Array.make_matrix r c x ;;


(** {v identity_int numrows numcolumns v} *)
let identity_int = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0 in
  for i = 0 to ( min r c ) - 1 do
   m.(i).(i) <- 1
  done ;
  m ;;

(** {v identity_int_bis numrows numcolumns v} *)
let identity_int_bis = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0 
 and delta = Util.int_max 0 (c - r) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).( i + delta ) <- 1
  done ;
  m ;;

(** {v identity_int_ter numrows numcolumns v} *)
let identity_int_ter = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0 
 and delta = Util.int_max 0 (r - c) in
  for i = 0 to ( min r c ) - 1 do
   m.( i + delta ).(i) <- 1
  done ;
  m ;;


(** {v rank_int_matrix numrows numcolumns rank v} *)
let rank_int_matrix =  fun (r:int) (c:int) (rank:int) ->
 let m = Array.make_matrix r c 0 in
  for i = 0 to rank - 1 do
   m.(i).(i) <- 1
  done ;
  m ;;


(** {v eye_int matrix v} *)
let eye_int = function (m:int array array) ->
 identity_int (Array.length m) (Array.length m.(0)) ;;

(** {v eye_int_bis matrix v} *)
let eye_int_bis = function (m:int array array) ->
 identity_int_bis (Array.length m) (Array.length m.(0)) ;;

(** {v eye_int_ter matrix v} *)
let eye_int_ter = function (m:int array array) ->
 identity_int_ter (Array.length m) (Array.length m.(0)) ;;


(** {v scal_int numrows numcolumns integer v} *)
let scal_int = fun (r:int) (c:int) (x:int) ->
  let m = Array.make_matrix r c 0 in
   for i = 0 to ( min r c ) - 1 do
    m.(i).(i) <- x
   done ;
 m ;;

(** {v scal_int_bis numrows numcolumns integer v} *)
let scal_int_bis = fun (r:int) (c:int) (x:int) ->
 let m = Array.make_matrix r c 0
 and delta = Util.int_max 0 (c - r) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).( i + delta ) <- x
  done ;
  m ;;

(** {v scal_int_ter numrows numcolumns integer v} *)
let scal_int_ter = fun (r:int) (c:int) (x:int) ->
 let m = Array.make_matrix r c 0
 and delta = Util.int_max 0 (r - c) in
  for i = 0 to ( min r c ) - 1 do
   m.( i + delta ).(i) <- x
  done ;
  m ;;


(** {v diag_int vector v} *)
let diag_int = function (v:int array) ->
 let r = Array.length v in
  let m = Array.make_matrix r r 0 in
   for i = 0 to r - 1 do
    m.(i).(i) <- v.(i)
   done ;
 m ;;

(** {v matrix_int_permu size index1 index2 v} *)
let matrix_int_permu = fun n i j ->
 let m = identity_int n n in
   m.(i).(i) <- 0 ;
   m.(j).(j) <- 0 ;
   m.(i).(j) <- 1 ;
   m.(j).(i) <- 1 ;
 m ;;

(** {v oblique_int numrows numcolumns v} *)
let oblique_int = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0
 and s = ( min r c ) - 1 in
  for i = 0 to s do
   m.(i).(s-i) <- 1
  done ;
  m ;;

(** {v oblique_int_bis numrows numcolumns v} *)
let oblique_int_bis = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0
 and s = ( min r c ) - 1
 and delta = Util.int_max 0 (c - r) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).( s - i + delta ) <- 1
  done ;
  m ;;

(** {v oblique_int_ter numrows numcolumns v} *)
let oblique_int_ter = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c 0
 and s = ( min r c ) - 1
 and delta = Util.int_max 0 (r - c) in
  for i = 0 to ( min r c ) - 1 do
   m.( i + delta ).(s-i) <- 1
  done ;
  m ;;


(** {v antiscal_int numrows numcolumns integer v} *)
let antiscal_int = fun (r:int) (c:int) (x:int) ->
 let m = Array.make_matrix r c 0
 and s = ( min r c ) - 1 in
  for i = 0 to s do
   m.(i).(s-i) <- x
  done ;
  m ;;

(** {v antiscal_int_bis numrows numcolumns integer v} *)
let antiscal_int_bis = fun (r:int) (c:int) (x:int) ->
 let m = Array.make_matrix r c 0
 and s = ( min r c ) - 1
 and delta = Util.int_max 0 (c - r) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).( s - i + delta ) <- x
  done ;
  m ;;

(** {v antiscal_int_ter numrows numcolumns integer v} *)
let antiscal_int_ter = fun (r:int) (c:int) (x:int) ->
 let m = Array.make_matrix r c 0
 and s = ( min r c ) - 1
 and delta = Util.int_max 0 (r - c) in
  for i = 0 to ( min r c ) - 1 do
   m.( i + delta ).(s-i) <- x
  done ;
  m ;;


(** {v antidiag_int numrows numcolumns v} *)
let antidiag_int = function (v:int array) ->
 let r = Array.length v in
  let m = Array.make_matrix r r 0 in
   for i = 0 to r - 1 do
    m.(i).(r - 1 - i) <- v.(i)
   done ;
   m ;;

(** {v vector_int_random size range v} *)
let vector_int_random = fun n x ->
 let v = Array.make n 0 in
  for i = 0 to n - 1 do
   v.(i) <- Random.int x
 done ;
 v ;;

(** {v vector_int_sign_random size v} *)
let vector_int_sign_random = function n ->
 let v = Array.make n 0 in
  for i = 0 to n - 1 do
   v.(i) <- if Random.bool() then 1 else -1
 done ;
 v ;;

(** {v vector_int_random_progression size range v} *)
let vector_int_random_progression = fun n x ->
 let v = vector_int_random n x in
  for i = 1 to n - 1 do
   v.(i) <- v.(i - 1) + v.(i)
  done ;
  v ;;

(** {v vector_int_bal_random size range v} *)
let vector_int_bal_random = fun n x ->
 let v = Array.make n 0
 and y = x * 2 in
  for i = 0 to n - 1 do
   v.(i) <- (Random.int y) - x
 done ;
 v ;;


(** {v int_equal_range beginning step number_of_samples v} *)
let int_equal_range = fun (a:int) (s:int) (n:int) ->
 let v = Array.make n a in
  for i = 1 to pred n do
   v.(i) <- v.(i) + i * s ;
  done ;
  v ;;


(** {v matrix_int_random rows columns range v} *)
let matrix_int_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = 0 to c - 1 do
     row.(j) <- Random.int x
    done
  done ;
  m ;;

(** {v matrix_int_bal_random rows columns range v} *)
let matrix_int_bal_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 
 and y = x * 2 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = 0 to c - 1 do
     row.(j) <- (Random.int y) - x
    done
  done ;
  m ;;


(** {v upper_trig_int_random rows columns range v} *)
let upper_trig_int_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0
 and cc = c - 1 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = i to cc do
     row.(j) <- Random.int x
    done
  done ;
  m ;;

(** {v upper_trig_int_bal_random rows columns range v} *)
let upper_trig_int_bal_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 
 and y = x * 2
 and cc = c - 1 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = i to cc do
     row.(j) <- (Random.int y) - x
    done
  done ;
  m ;;

(** {v lower_trig_int_random rows columns range v} *)
let lower_trig_int_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = 0 to i do
     row.(j) <- Random.int x
    done
  done ;
  m ;;

(** {v lower_trig_int_bal_random rows columns range v} *)
let lower_trig_int_bal_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 
 and y = x * 2 in
  for i = 0 to r - 1 do
   let row = m.(i) in
    for j = 0 to i do
     row.(j) <- (Random.int y) - x
    done
  done ;
  m ;;

(** {v upper_nil_int_random rows columns range v} *)
let upper_nil_int_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0
 and cc = c - 1 in
  for i = 0 to r - 2 do
   let row = m.(i) in
    for j = i + 1 to cc do
     row.(j) <- Random.int x
    done
  done ;
  m ;;

(** {v upper_nil_int_bal_random rows columns range v} *)
let upper_nil_int_bal_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 
 and y = x * 2
 and cc = c - 1 in
  for i = 0 to r - 2 do
   let row = m.(i) in
    for j = i + 1 to cc do
     row.(j) <- (Random.int y) - x
    done
  done ;
  m ;;

(** {v lower_nil_int_random rows columns range v} *)
let lower_nil_int_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 in
  for i = 1 to r - 1 do
   let row = m.(i) in
    for j = 0 to i - 1 do
     row.(j) <- Random.int x
    done
  done ;
  m ;;

(** {v lower_nil_int_bal_random rows columns range v} *)
let lower_nil_int_bal_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 
 and y = x * 2 in
  for i = 1 to r - 1 do
   let row = m.(i) in
    for j = 0 to i - 1 do
     row.(j) <- (Random.int y) - x
    done
  done ;
  m ;;


(** {v upper_unip_int_random rows columns range v} *)
let upper_unip_int_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0
 and cc = c - 1 in
  for i = 0 to r - 2 do
   let row = m.(i) in
    row.(i) <- 1 ;
    for j = i + 1 to cc do
     row.(j) <- Random.int x
    done
  done ;
  let rr = pred r in
   m.(rr).(rr) <- 1 ;
   m ;;

(** {v upper_unip_int_bal_random rows columns range v} *)
let upper_unip_int_bal_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 
 and y = x * 2
 and cc = c - 1 in
  for i = 0 to r - 2 do
   let row = m.(i) in
    row.(i) <- 1 ;
    for j = i + 1 to cc do
     row.(j) <- (Random.int y) - x
    done
  done ;
  let rr = pred r in
   m.(rr).(rr) <- 1 ;
   m ;;

(** {v lower_unip_int_random rows columns range v} *)
let lower_unip_int_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 in
  m.(0).(0) <- 1 ;
  for i = 1 to r - 1 do
   let row = m.(i) in
    for j = 0 to i - 1 do
     row.(j) <- Random.int x
    done ;
    row.(i) <- 1
  done ;
  m ;;

(** {v lower_unip_int_bal_random rows columns range v} *)
let lower_unip_int_bal_random = fun (r:int) (c:int) x ->
 let m = Array.make_matrix r c 0 
 and y = x * 2 in
  m.(0).(0) <- 1 ;
  for i = 1 to r - 1 do
   let row = m.(i) in
    for j = 0 to i - 1 do
     row.(j) <- (Random.int y) - x
    done ;
    row.(i) <- 1
  done ;
  m ;;


(** {v matrix_int_binome rank v} The Pascal triangle in integer output.

Le triangle de Pascal en sortie entière. *)
let rec matrix_int_binome = function n ->
 match n with
 | 0 -> [|[|1|]|]
 | 1 -> [| [|1|] ; [| 1 ; 1 |] |]
 | _ -> let m = matrix_int_binome (n - 1)
  and v = Array.make ( n + 1 ) 1 in
   let row = m.( n - 1 ) in
    for i = 1 to n - 1 do
     v.(i) <- row.(i - 1) + row.(i)
    done ;
    Array.append m [| v |] ;;


(** {v matrix_int_Vandermonde vector v} The integer Vandermonde matrix.

La matrice de Vandermonde entière. *)
let matrix_int_Vandermonde = function v ->
 let n = Array.length v in
  let m = Array.make_matrix n n 1 in
   for i = 0 to n - 1 do
    let row = m.(i)
    and xi = v.(i) in
     for j = 1 to n - 1 do
      row.(j) <- row.(j - 1) * xi
     done
   done ;
   m ;;


(** {v matrix_int_nil order v} The model nilpotent integer matrix of order n and rank n - 1.

La matrice nilpotente modèle entière d'ordre n et de rang n - 1. *)
let matrix_int_nil = function n ->
 let m =Array.make_matrix n n 0 in
  for i = 0 to n - 2 do
   m.(i).(i + 1) <- 1
  done ;
  m ;;


(** {v matrix_int_Jordan order coefficient v} The integer Jordan block of order n.

Le bloc de Jordan entier d'ordre n. *)
let matrix_int_Jordan = fun n x ->
 let m =Array.make_matrix n n 0 in
  for i = 0 to n - 2 do
   let row = m.(i) in
    row.(i) <- x ;
    row.(i + 1) <- 1
  done ;
  m.(n - 1).(n - 1) <- x ;
  m ;;




(** {C § } *)
(** 
{1 Calcul élémentaire sur les matrices réelles}
{1 Elementary calculus for real matrices}
*)
(** {C  } *)




(** {v vector_float_copy vector v} *)
let vector_float_copy = function (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- v.(i)
   done ;
   vv ;;

(** {v vector_float_clip vector v} *)
let vector_float_clip = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- min x v.(i)
   done ;
   vv ;;

(** {v vector_float_crest real vector v} *)
let vector_float_crest = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    let y = v.(i) in
     if y > x then
      vv.(i) <- y -. x
   done ;
   vv ;;

(** {v vector_float_gully real vector v} *)
let vector_float_gully = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    let y = v.(i) in
     if y < x then
      vv.(i) <- x -. y
   done ;
   vv ;;

(** {v other_vector_float_copy vector v} *)
let other_vector_float_copy = function (v:float array) ->
  let vv = Array.make (Array.length v) 0. in
   for i = 0 to (Array.length v) - 1 do
    vv.(i) <- v.(i)
   done ;
   vv ;;

(** {v matrix_float_copy vector v} *)
let matrix_float_copy = function (m:float array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- row_input.(j)
    done
  done ;
  mm ;;

(** {v matrix_float_clip vector v} *)
let matrix_float_clip = fun (x:float) (m:float array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- min x row_input.(j)
    done
  done ;
  mm ;;

(** {v matrix_float_crest real vector v} *)
let matrix_float_crest = fun (x:float) (m:float array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     let y = row_input.(j) in
      if y > x then
       row_output.(j) <- y -. x
    done
  done ;
  mm ;;

(** {v matrix_float_gully real vector v} *)
let matrix_float_gully = fun (x:float) (m:float array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     let y = row_input.(j) in
      if y < x then
       row_output.(j) <- x -. y
    done
  done ;
  mm ;;


(** {v float_sub_matrix matrix beg-row end-row beg-col end-col v} *)
let float_sub_matrix = fun (m:float array array) i ii j jj ->
(** i et j : beginning: début ; ii et jj : end: fin *)
 let mm = Array.make_matrix (ii - i + 1) (jj - j + 1) 0. in
  for index = i to ii do
   let row_input = m.(index)
   and row_output = mm.(index - i) in
    for indice = j to jj do
     row_output.(indice - j) <- row_input.(indice)
    done
  done ;
 mm ;;


(** {v int_of_vector vector v} *)
let int_of_vector = function (v:float array) ->
 let vv = Array.make (Array.length v) 0 in
  for i = 0 to (Array.length v) - 1 do
   vv.(i) <- int_of_float v.(i)
  done ;
  vv ;;

(** {v int_of_matrix matrix v} *)
let int_of_matrix = function (m:float array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- int_of_float row_input.(j)
    done
  done ;
  mm ;;

(** {v vector_float_round vector v} *)
let vector_float_round = function (v:float array) ->
 let vv = Array.make (Array.length v) 0 in
  for i = 0 to (Array.length v) - 1 do
   vv.(i) <- Util.round v.(i)
  done ;
  vv ;;

(** {v matrix_float_round matrix v} *)
let matrix_float_round = function (m:float array array) ->
 let r = Array.length m
 and cc = Array.length m.(0) - 1 in
  let mm = Array.make_matrix r (Array.length m.(0)) 0 in
   for i = 0 to (Array.length m) - 1 do
    let row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do
      row_output.(j) <- Util.round row_input.(j)
     done
    done ;
    mm ;;

(** {v matrix_float_apply function matrix v} *)
let matrix_float_apply = fun (f:float -> float) (m:float array array) ->
 let r = Array.length m
 and cc = Array.length m.(0) - 1 in
  let mm = Array.make_matrix r (Array.length m.(0)) 0. in
   for i = 0 to (Array.length m) - 1 do
    let row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do
      row_output.(j) <- f row_input.(j)
     done
    done ;
    mm ;;


(** {v float_transpose matrix v} *)
let float_transpose = function (m:float array array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let mm = Array.make_matrix c r 0.
  and cc = c - 1 in
   for i = 0 to r - 1 do
    let row_input = m.(i) in
    for j = 0 to cc do
     mm.(j).(i) <- row_input.(j)
    done
   done ;  
  mm;;


(** {v vector_float_max vector v} *)
let vector_float_max = function (v:float array) ->
 let accu = ref v.(0) in
  for i = 0 to (Array.length v) - 1 do
   accu := max v.(i) !accu
  done ;
  !accu ;;

(** {v matrix_float_max_by_row matrix v} matrix_float_max_by_row even for lacunar matrices:

matrix_float_max_by_row fonctionne même pour les matrices lacunaires. *)
let matrix_float_max_by_row = function (m:float array array) ->
 let accu = Array.make (Array.length m) (-. max_float) in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i)
   and accumul = ref accu.(i) in
    for j = 0 to (Array.length row) - 1 do
     accumul := max row.(j) !accumul
    done ;
    accu.(i) <- !accumul
  done ;
  accu ;;

(** {v matrix_float_max_by_column matrix v} *)
let matrix_float_max_by_column = function (m:float array array) ->
 let cc = (numcolumns m) - 1 in
  let accu = Array.make (cc + 1) (-. max_float) in
   for j = 0 to cc do
    let accumul = ref accu.(j) in
     for i = 0 to ( (Array.length m) - 1 ) do
      accumul := max m.(i).(j) !accumul
     done ;
     accu.(j) <- !accumul
   done ;
  accu ;;

(** {v matrix_float_max matrix v} *)
let matrix_float_max = function (m:float array array) ->
 let accu = ref (-. max_float) in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i) in
    for j = 0 to (Array.length row) - 1 do
     accu := max row.(j) !accu
    done
  done ;
  !accu ;;


(** {v vector_float_min vector v} *)
let vector_float_min = function (v:float array) ->
 let accu = ref v.(0) in
  for i = 0 to (Array.length v) - 1 do
   accu := min v.(i) !accu
  done ;
  !accu ;;

(** {v matrix_float_min_by_row matrix v} matrix_float_min_by_row even for lacunar matrices:

matrix_float_min_by_row fonctionne même pour les matrices lacunaires. *)
let matrix_float_min_by_row = function (m:float array array) ->
 let accu = Array.make (Array.length m) (-. min_float) in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i)
   and accumul = ref accu.(i) in
    for j = 0 to (Array.length row) - 1 do
     accumul := min row.(j) !accumul
    done ;
    accu.(i) <- !accumul
  done ;
  accu ;;

(** {v matrix_float_min_by_column matrix v} *)
let matrix_float_min_by_column = function (m:float array array) ->
 let cc = (numcolumns m) - 1 in
  let accu = Array.make (cc + 1) (-. min_float) in
   for j = 0 to cc do
    let accumul = ref accu.(j) in
     for i = 0 to ( (Array.length m) - 1 ) do
      accumul := min m.(i).(j) !accumul
     done ;
     accu.(j) <- !accumul
   done ;
  accu ;;

(** {v matrix_float_min matrix v} *)
let matrix_float_min = function (m:float array array) ->
 let accu = ref (-. min_float) in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i) in
    for j = 0 to (Array.length row) - 1 do
     accu := min row.(j) !accu
    done
  done ;
  !accu ;;


(** {v vector_float_find_last element vector v} vector_float_find_last returns [-1] if it does not find:

vector_float_find_last retourne [-1] s'il ne trouve pas. *)
let vector_float_find_last = fun x (v:float array) ->
 let index = ref (-1) in
  let i = ref ( (Array.length v) - 1 ) in
   while  !i >= 0 do
    if x = v.(!i) then (index := !i ; i := -1) else i := !i - 1 ; 
   done ;
   !index ;;


(** {v vector_float_find_first element vector v} vector_float_find_first returns [-1] if it does not find:

vector_float_find_first retourne [-1] s'il ne trouve pas. *)
let vector_float_find_first = fun x (v:float array) ->
 let index = ref (-1)
 and i = ref 0 in
  while  !i < Array.length v do
   if x = v.(!i) then (index := !i ; i := Array.length v ) else i := !i + 1 ; 
  done ;
 !index ;;


(** {v vector_float_find_twin element vector v} vector_float_find_first returns [-1] if it does not find:

vector_float_find_twin retourne [-1] s'il ne trouve pas. *)
let vector_float_find_twin = fun x (v:float array) ->
 let index = ref (-1)
 and i = ref 0 in
  while  !i <= min ((Array.length v) - 1) ( int_of_float ( ceil ( (float (Array.length v)) /. 2. ) ) )  do
   if x = v.(!i) then (index := !i ; i := 1 + Array.length v )
   else
    begin
     let j = (Array.length v) - 1 - !i in
     if x = v.(j) then ( index := j ; i := 1 + Array.length v ) else i := !i + 1 ; 
    end
  done ;
 !index ;;


(** {v vector_float_find_all element vector v} vector_float_find_all returns [[||]]  if it does not find:

vector_float_find_all retourne [[||]] s'il ne trouve pas.*)
let vector_float_find_all = fun x (v:float array) ->
 let index = ref [||] in
  for i = 0 to (Array.length v) - 1 do
   if (x = v.(i)) then (index := Array.append !index [|i|] ; ())
  done ;
 !index ;;


(** {v matrix_float_find_last element vector v} matrix_float_find_last returns [[|-1;-1|]] if it does not find:

matrix_float_find_last retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_float_find_last = fun x (m:float array array) ->
 let index = ref (-1) and indice = ref (-1) in
  let i = ref ( (Array.length m) - 1 ) in
   while  !i >= 0 do
    let row = m.(!i) in
     let j = ref ( (Array.length row) - 1 ) in
      while  !j >= 0 do
       if x = row.(!j) then ( indice := !j ; j := -1 ) else j := !j - 1 ; 
      done ;
      if !indice <> -1 then ( index := !i ; i := -1 ) else i := !i - 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_float_find_last_first element vector v} matrix_find_last returns [[|-1;-1|]] if it does not find:

matrix_float_find_last_first retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_float_find_last_first = fun x (m:float array array) ->
 let index = ref (-1) and indice = ref (-1) in
  let i = ref ( ( Array.length m ) - 1 ) in
   while  !i >= 0 do
    let row = m.(!i) in
     let j = ref 0 in
      while  !j < ( Array.length row ) do
       if x = row.(!j) then ( indice := !j ; j := max_int ) else j := !j + 1 ; 
      done ;
      if !indice <> -1 then ( index := !i ; i := -1 ) else i := !i - 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_float_find_first element vector v} matrix_find_first returns [[|-1;-1|]] if it does not find:

matrix_float_find_first retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_float_find_first = fun x (m:float array array) ->
 let index = ref (-1) and indice = ref (-1) in
  let i = ref 0 in
   while  !i < Array.length m do
    let row = m.(!i) in
     let j = ref 0 in
      while  !j < Array.length row do
       if x = row.(!j) then ( indice := !j ; j := Array.length row ) else j := !j + 1 ; 
      done ;
      if !indice <> -1 then ( index := !i ; i := Array.length m ) else i := !i + 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_float_find_first_last element vector v} matrix_find_first returns [[|-1;-1|]] if it does not find:

matrix_float_find_first_last retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_float_find_first_last = fun x (m:float array array) ->
 let index = ref (-1) and indice = ref (-1) in
  let i = ref 0 in
   while  !i < Array.length m do
    let row = m.(!i) in
     let j = ref 0 in
      while  !j >= 0 do
       if x = row.(!j) then ( indice := !j ; j := -1 ) else j := !j - 1 ; 
      done ;
      if !indice <> -1 then ( index := !i ; i := Array.length m ) else i := !i + 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_float_find_twin element vector v} matrix_find_twin returns [[|-1;-1|]] if it does not find:

matrix_float_find_twin retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_float_find_twin = fun x (m:float array array) ->
 let index = ref (-1) and indice = ref (-1) in
  let i = ref 0 in
   while  !i <= min (Array.length m - 1) ( int_of_float ( ceil ( (float (Array.length m)) /. 2. ) ) ) do
    let row = m.(!i) in
     let j = ref 0 in
      while  !j <= min (Array.length row - 1) ( int_of_float ( ceil ( (float (Array.length row)) /. 2. ) ) ) do
       if x = row.(!j) then ( indice := !j ; j := 1 + Array.length row )
       else
        begin
         let k = (Array.length row) - 1 - !j in
         if x = row.(k) then ( indice := k ; j := 1 + Array.length row ) else j := !j + 1 ; 
        end
      done ;
      if !indice <> -1 then ( index := !i ; i := 1 + Array.length m ) else i := !i + 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_float_find_all element vector v} matrix_find_all returns [[|-1;-1|]] if it does not find:

matrix_float_find_all retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_float_find_all = fun x (m:float array array) ->
 let result = ref [||] and index = ref [||] and indice = ref [||] in
  for i = 0 to Array.length m -1 do
   let row = m.(i) in
    for j = 0 to Array.length row -1 do
     if x = row.(j) then ( indice := Array.append !indice [|j|] ; () )
    done ;
    if !indice <> [||] then 
     begin
      index := Array.map (function a -> [| i ; a|]) !indice ; 
      result := Array.append !result !index 
     end
  done ;
  !result ;;


(** {v vector_float_sum vector v} *)
let vector_float_sum = function (v:float array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length v) - 1 do
   accu := v.(i) +. !accu
  done ;
  !accu ;;

(** {v vector_float_contraction vector v} *)
let vector_float_contraction = function (v:float array) ->
 let accu = ref 1. in
  for i = 0 to (Array.length v) - 1 do
   accu := v.(i) *. !accu
  done ;
  !accu ;;

(** {v matrix_float_sum_by_row matrix v} *)
let matrix_float_sum_by_row = function (m:float array array) ->
 let accu = Array.make (Array.length m) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i)
   and accumul = ref accu.(i) in
    for j = 0 to (Array.length row) - 1 do
     accumul := row.(j) +. !accumul
    done ;
    accu.(i) <- !accumul
  done ;
  accu ;;

(** {v matrix_float_sum_by_column matrix v} *)
let matrix_float_sum_by_column = function (m:float array array) ->
 let cc = (numcolumns m) - 1 in
  let accu = Array.make (cc + 1) 0. in
   for j = 0 to cc do
    let accumul = ref accu.(j) in
     for i = 0 to ( (Array.length m) - 1 ) do
      accumul := m.(i).(j) +. !accumul
     done ;
     accu.(j) <- !accumul
   done ;
  accu ;;

(** {v matrix_float_sum matrix v} *)
let matrix_float_sum = function (m:float array array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i) in
    for j = 0 to (Array.length row) - 1 do
     accu := row.(j) +. !accu
    done
  done ;
  !accu ;;

(** {v matrix_float_mean_by_row matrix v} *)
let matrix_float_mean_by_row = function (m:float array array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let accu = Array.make r 0. in
   let cc = c - 1 in
    for i = 0 to r - 1 do
     let row = m.(i)
     and accumul = ref accu.(i) in
      for j = 0 to cc do
       accumul := row.(j) +. !accumul
      done ;
      accu.(i) <- !accumul /. (float c)
    done ;
    accu ;;

(** {v matrix_float_mean_by_column matrix v} *)
let matrix_float_mean_by_column = function (m:float array array) ->
 let cc = (numcolumns m) - 1
 and r = Array.length m in
  let accu = Array.make (cc + 1) 0.
  and rr = r - 1 in
   for j = 0 to cc do
    let accumul = ref accu.(j) in
     for i = 0 to rr do
      accumul := m.(i).(j) +. !accumul
     done ;
     accu.(j) <- !accumul /. (float r)
   done ;
  accu ;;


(** {v vector_float_abs vector v} *)
let vector_float_abs = function (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- abs_float v.(i)
   done ;
   vv ;;

(** {v matrix_float_abs matrix v} *)
let matrix_float_abs = function (m:float array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- abs_float row_input.(j)
    done
  done ;
  mm ;;

(** {v matrix_float_opp matrix v} *)
let matrix_float_opp = function (m:float array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- -. row_input.(j)
    done
  done ;
  mm ;;


(** {v string_of_vector_float vector v} *)
let string_of_vector_float = function (v:float array) ->
 let rr = pred ( Array.length v )
 and s = ref "[| " in
  for i = 0 to ( rr - 1 ) do
   s := !s ^ ( string_of_float v.(i) ) ^ " ; " 
  done ;
  s := !s ^ ( string_of_float v.(rr) ) ^ " |]" ;
  !s ;;

(** {v vector_float_print vector v} *)
let vector_float_print = function (v:float array) ->
 let s = string_of_vector_float v in
  print_string s ;
  print_newline () ;;

(** {v bare_vector_float_to_string vector v} *)
let bare_vector_float_to_string = function (v:float array) ->
 let rr = pred ( Array.length v )
 and s = ref "[| "  in
  for i = 0 to pred rr do
   s := !s ^ ( string_of_float v.(i) ) ^ " "
  done ;
  s := !s ^ ( string_of_float v.(rr) ) ^ "|]" ;
  !s ;;

(** {v bare_vector_float_print vector v} *)
let bare_vector_float_print = function (v:float array) ->
 let s = bare_vector_float_to_string v in
  print_string s ;;


(** {v vector_float_of_string string v} *)
let vector_float_of_string = function (s:string) ->
 let ls = String.length s in
  let st = String.sub s 3 ( ls - 6 ) in
   let listing = Str.split ( Str.regexp " ; " ) st in
    let a = Array.of_list listing in
     Array.map float_of_string a ;;

(** {v bare_vector_float_of_string string v} *)
let bare_vector_float_of_string = function (s:string) ->
 let ls = String.length s in
  let st = String.sub s 2 ( ls - 4 ) in
   let listing = Str.split ( Str.regexp " " ) st in
    let a = Array.of_list listing in
     Array.map float_of_string a ;;


(** {v matrix_float_to_string matrix v} *)
let matrix_float_to_string = function (m:float array array) ->
 let s = ref "[| with "
 and r = Array.length m
 and c = slow_numcolumns m in
  s := !s ^ string_of_int r ;
  if r > 1 then s := !s ^ " rows"
  else s := !s ^ " row" ;
  s := !s ^ "\n" ;
  s := !s ^ bare_vector_float_to_string m.(0) ^ " ;" ; 
  for i = 1 to ( Array.length m - 1 ) do
   begin
    s := !s ^ "\n" ^ ( bare_vector_float_to_string m.(i) ) ^ " ;"
   end
  done ;
  s := !s ^ "\n|] and " ^ ( string_of_int c ) ;
  if c > 1 then s := !s ^ " columns\n"
  else s := !s ^ " column\n" ;
  !s ;;


(** {v matrix_float_print matrix v} *)
let matrix_float_print = function (m:float array array) ->
 let s = matrix_float_to_string m in
  print_string s ;;


(** {v matrix_float_of_string string v} *)
let matrix_float_of_string = function (s:string) ->
 let ls = String.length s in
  let st = String.sub s 8 ( ls - 8 ) in
   let number_of_rows = 
    begin
     let position = Str.search_forward ( Str.regexp " row" ) st 0 in
      int_of_string ( String.sub st 0 position )
    end in
    let m = Array.make_matrix number_of_rows 0 0.
    and listing = Str.split ( Str.regexp " ;\n" ) s in
     let a = Array.sub ( Array.of_list listing ) 0 number_of_rows in
      for i = 1 to pred number_of_rows do
       m.(i) <- bare_vector_float_of_string a.(i)
      done ;
      let str = List.hd ( List.tl ( Str.split ( Str.regexp "\n" ) a.(0) ) ) in
       m.(0) <- bare_vector_float_of_string str ;
       m ;;


(** {v float_trace matrix v} *)
let float_trace = function (m:float array array) ->
 let r = min (Array.length m) (Array.length m.(0))
 and accumulateur = ref 0. in
  if r > 0 then 
   for i = 0 to r - 1 do
    accumulateur := !accumulateur +. m.(i).(i)
   done ;
  !accumulateur ;;


(** {v vector_float_scal_add coefficient vector v} *)
let vector_float_scal_add = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- x +. v.(i)
   done ;
   vv ;;

(** {v vector_float_scal_mult coefficient vector v} *)
let vector_float_scal_mult = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- x *. v.(i)
   done ;
   vv ;;

(** {v vector_float_scal_left_sub coefficient vector v} *)
let vector_float_scal_left_sub = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- v.(i) -. x
   done ;
   vv ;;

(** {v vector_float_scal_right_sub coefficient vector v} *)
let vector_float_scal_right_sub = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- x -. v.(i)
   done ;
   vv ;;

(** {v vector_float_scal_left_div coefficient vector v} *)
let vector_float_scal_left_div = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- v.(i) /. x
   done ;
   vv ;;

(** {v vector_float_scal_right_div coefficient vector v} *)
let vector_float_scal_right_div = fun (x:float) (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- x /. v.(i)
   done ;
   vv ;;

(** {v vector_float_opp vector v} *)
let vector_float_opp = fun (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    vv.(i) <- -. v.(i)
   done ;
   vv ;;

(** {v vector_float_inv vector v} *)
let vector_float_inv = fun (v:float array) ->
 let r = Array.length v in
  let vv = Array.make r 0. in
   for i = 0 to r - 1 do
    begin
     let x = v.(i) in
      if x == 0. then failwith "Division by zero in Matrix.vector_float_inv." ;
      vv.(i) <- 1. /. x
    end
   done ;
   vv ;;


(** {v vector_float_plus vector1 vector2 v} *)
let vector_float_plus = fun (v:float array) (vv:float array) ->
 let r = Array.length v in
  let vvv = Array.make r 0. in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) +. vv.(i)
   done ;
   vvv ;;

(** {v vector_float_minus vector1 vector2 v} *)
let vector_float_minus = fun (v:float array) (vv:float array) ->
 let r = Array.length v in
  let vvv = Array.make r 0. in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) -. vv.(i)
   done ;
   vvv ;;

(** {v vector_float_coeff_prod vector1 vector2 v} *)
let vector_float_coeff_prod = fun (v:float array) (vv:float array) ->
 let r = Array.length v in
  let vvv = Array.make r 0. in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) *. vv.(i)
   done ;
   vvv ;;

(** {v vector_float_coeff_div vector1 vector2 v} *)
let vector_float_coeff_div = fun (v:float array) (vv:float array) ->
 let r = Array.length v in
  let vvv = Array.make r 0. in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) /. vv.(i)
   done ;
   vvv ;;

(** {v vector_float_scal_prod vector1 vector2 v} *)
let vector_float_scal_prod = fun (v:float array) (vv:float array) ->
 let x = ref 0. in
  for i = 0 to ( (Array.length v) - 1 ) do
   x := !x +. v.(i) *. vv.(i)
  done ; 
  !x ;;


(** {v partial_float_scal_add beginning end vector1 vector2 v} *)
let partial_float_scal_add = fun i j x (v:float array) ->
 let w = Array.make (Array.length v) 0. in
  for k = i to j do
   w.(k) <- v.(k) +. x
  done ;
  w ;;

(** {v part_float_scal_add beginning end vector1 vector2 v} *)
let part_float_scal_add = fun i j x (v:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- v.(k) +. x
   done ;
   w ;;

(** {v partial_float_scal_mult beginning end vector1 vector2 v} *)
let partial_float_scal_mult = fun i j x (v:float array) ->
 let w = Array.make (Array.length v) 0. in
  for k = i to j do
   w.(k) <- v.(k) *. x
  done ;
  w ;;

(** {v part_float_scal_mult beginning end vector1 vector2 v} *)
let part_float_scal_mult = fun i j x (v:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- v.(k) *. x
   done ;
   w ;;

(** {v partial_float_scal_left_sub beginning end vector1 vector2 v} *)
let partial_float_scal_left_sub = fun i j x (v:float array) ->
 let w = Array.make (Array.length v) 0. in
  for k = i to j do
   w.(k) <- v.(k) -. x
  done ;
  w ;;

(** {v part_float_scal_left_sub beginning end vector1 vector2 v} *)
let part_float_scal_left_sub = fun i j x (v:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- v.(k) -. x
   done ;
   w ;;

(** {v partial_float_scal_right_sub beginning end vector1 vector2 v} *)
let partial_float_scal_right_sub = fun i j x (v:float array) ->
 let w = Array.make (Array.length v) 0. in
  for k = i to j do
   w.(k) <- x -. v.(k)
  done ;
  w ;;

(** {v part_float_scal_right_sub beginning end vector1 vector2 v} *)
let part_float_scal_right_sub = fun i j x (v:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- x -. v.(k)
   done ;
   w ;;

(** {v partial_float_scal_left_div beginning end vector1 vector2 v} *)
let partial_float_scal_left_div = fun i j x (v:float array) ->
 let w = Array.make (Array.length v) 0. in
  for k = i to j do
   w.(k) <- v.(k) /. x
  done ;
  w ;;

(** {v part_float_scal_left_div beginning end vector1 vector2 v} *)
let part_float_scal_left_div = fun i j x (v:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- v.(k) /. x
   done ;
   w ;;

(** {v partial_float_scal_right_div beginning end vector1 vector2 v} *)
let partial_float_scal_right_div = fun i j x (v:float array) ->
 let w = Array.make (Array.length v) 0. in
  for k = i to j do
   w.(k) <- x /. v.(k)
  done ;
  w ;;

(** {v part_float_scal_right_div beginning end vector1 vector2 v} *)
let part_float_scal_right_div = fun i j x (v:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- x /. v.(k)
   done ;
   w ;;


(** {v partial_float_plus beginning end vector1 vector2 v} *)
let partial_float_plus = fun i j (v:float array) (vv:float array) ->
 let c = min (Array.length v) (Array.length vv) in
  let w = Array.make c 0. in
   for k = i to j do
    w.(k) <- v.(k) +. vv.(k)
   done ;
   w ;;

(** {v part_float_plus beginning end vector1 vector2 v} *)
let part_float_plus = fun i j (v:float array) (vv:float array) ->
 let w =  Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- v.(k) +. vv.(k)
   done ;
   w ;;

(** {v partial_float_minus beginning end vector1 vector2 v} *)
let partial_float_minus = fun i j s t ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c 0. in
   for k = i to j do
    m.(k) <- s.(k) -. t.(k)
   done ;
   m ;;

(** {v part_float_minus beginning end vector1 vector2 v} *)
let part_float_minus = fun i j (v:float array) (vv:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- v.(k) -. vv.(k)
   done ;
   w ;;

(** {v partial_float_coeff_prod beginning end vector1 vector2 v} *)
let partial_float_coeff_prod = fun i j (v:float array) (vv:float array) ->
 let c = min (Array.length v) (Array.length vv) in
  let w = Array.make c 0. in
   for k = i to j do
    w.(k) <- v.(k) *. vv.(k)
   done ;
   w ;;

(** {v part_float_coeff_prod beginning end vector1 vector2 v} *)
let part_float_coeff_prod = fun i j (v:float array) (vv:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- v.(k) *. vv.(k)
   done ;
   w ;;

(** {v partial_float_coeff_div beginning end vector1 vector2 v} *)
let partial_float_coeff_div = fun i j (v:float array) (vv:float array) ->
 let c = min (Array.length v) (Array.length vv) in
  let w = Array.make c 0. in
   for k = i to j do
    w.(k) <- v.(k) /. vv.(k)
   done ;
   w ;;

(** {v part_float_coeff_div beginning end vector1 vector2 v} *)
let part_float_coeff_div = fun i j (v:float array) (vv:float array) ->
 let w = Array.make ( j - i + 1 ) 0. in
   for k = i to j do
    w.(k-i) <- v.(k) /. vv.(k)
   done ;
   w ;;


(** {v matrix_float_scal_add coefficient matrix v} *)
let matrix_float_scal_add = fun x (m:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- x +. row_input.(j)
    done ;
  done ;
  w ;;

(** {v matrix_float_scal_mult coefficient matrix v} *)
let matrix_float_scal_mult = fun x (m:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- x *. row_input.(j)
    done ;
  done ;
  w ;;

(** {v matrix_float_scal_left_sub coefficient matrix v} *)
let matrix_float_scal_left_sub = fun x (m:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- row_input.(j) -. x
    done ;
  done ;
  w ;;

(** {v matrix_float_scal_right_sub coefficient matrix v} *)
let matrix_float_scal_right_sub = fun x (m:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- x -. row_input.(j)
    done ;
  done ;
  w ;;

(** {v matrix_float_scal_right_div coefficient matrix v} *)
let matrix_float_scal_right_div = fun x (m:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- x /. row_input.(j)
    done ;
  done ;
  w ;;

(** {v matrix_float_scal_left_div coefficient matrix v} *)
let matrix_float_scal_left_div = fun x (m:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- row_input.(j) /. x
    done ;
  done ;
  w ;;


(** {v matrix_float_plus matrix1 matrix2 v} *)
let matrix_float_plus = fun (m:float array array) (mm:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) +. row_input_right.(j)
    done ;
  done ;
  w ;;

(** {v matrix_float_minus matrix1 matrix2 v} *)
let matrix_float_minus = fun (m:float array array) (mm:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) -. row_input_right.(j)
    done ;
  done ;
  w ;;

(** {v matrix_float_coeff_prod matrix1 matrix2 v} *)
let matrix_float_coeff_prod = fun (m:float array array) (mm:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) *. row_input_right.(j)
    done ;
  done ;
  w ;;

(** {v matrix_float_coeff_div matrix1 matrix2 v} *)
let matrix_float_coeff_div = fun (m:float array array) (mm:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) /. row_input_right.(j)
    done ;
  done ;
  w ;;


(** {v matrix_float_twisted_prod matrix1 matrix2 v} This calculates matrix1 times transpose ( matrix2 ).

Ceci calcule matrix1 fois transposée de matrix2. *)
let matrix_float_twisted_prod = fun (m:float array array) (mm:float array array) ->
 let r = Array.length m
 and c = Array.length mm
 and t = min ( Array.length m.(0) ) ( Array.length mm.(0) ) in
  let rr = r - 1
  and cc = c - 1
  and tt = t - 1
  and w = Array.make_matrix r c 0. in
   for i = 0 to rr do
   let row_input_left = m.(i)
   and row_output = w.(i) in
    for j = 0 to cc do
     let row_input_right = mm.(j)
     and coeff = ref row_output.(j) in
      for k = 0 to tt do
       coeff := !coeff +. row_input_left.(k) *. row_input_right.(k)
      done ;
      row_output.(j) <- !coeff
    done ;
   done ;
   w ;;

(** {v matrix_float_prod matrix1 matrix2 v} *)
let matrix_float_prod = fun (m:float array array) (mm:float array array) ->
 matrix_float_twisted_prod m ( float_transpose mm ) ;;

(** {v matrix_float_triple_prod matrix1 matrix2 v} *)
let matrix_float_triple_prod = fun (a:float array array) (b:float array array) (c:float array array)->
 matrix_float_twisted_prod a ( matrix_float_twisted_prod ( float_transpose c ) b ) ;;

(** {v matrix_float_naive_prod matrix1 matrix2 v} *)
let matrix_float_naive_prod = fun (m:float array array) (mm:float array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length mm.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_output) - 1 do
     for k = 0 to (Array.length mm) - 1 do
      row_output.(j) <- row_input_left.(k) *. mm.(k).(j) +. row_output.(j)
     done
    done ;
  done ;
  w ;;


(** {v matrix_float_twisted_commut matrix1 matrix2 v} *)
let matrix_float_twisted_commut = fun (m:float array array) (mm:float array array) ->
 matrix_float_minus ( matrix_float_twisted_prod m mm ) ( matrix_float_twisted_prod mm m ) ;;

(** {v matrix_float_twisted_commut_bis matrix1 matrix2 v} *)
let matrix_float_twisted_commut_bis = fun (m:float array array) (mm:float array array) ->
 let m_m = float_transpose m
 and m_mm = float_transpose mm in
  matrix_float_minus ( matrix_float_twisted_prod m mm ) ( matrix_float_twisted_prod m_mm m_m ) ;;

(** {v matrix_float_commut matrix1 matrix2 v} *)
let matrix_float_commut = fun (m:float array array) (mm:float array array) ->
 matrix_float_minus ( matrix_float_prod m mm ) ( matrix_float_prod mm m ) ;;


(** {v vector_matrix_float_prod vector matrix v} *)
let vector_matrix_float_prod = fun (v:float array) (m:float array array) ->
 let w = Array.make (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m.(0)) - 1 do
   let row_input = m.(i)
   and output = ref w.(i) in
    for j = 0 to (Array.length m) - 1 do
     output := row_input.(j) *. v.(j) +. !output
    done ;
    w.(i) <- !output ;
  done ;
  w ;;

(** {v matrix_vector_float_prod matrix vector v} *)
let matrix_vector_float_prod = fun (m:float array array) (v:float array) ->
 let w = Array.make (Array.length m) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and output = ref w.(i) in
    for j = 0 to (Array.length v) - 1 do
     output := row_input.(j) *. v.(j) +. !output
    done ;
    w.(i) <- !output ;
  done ;
  w ;;

(** {v vector_matrix_float_apply function vector matrix v} *)
let vector_matrix_float_apply = fun (f:float -> float -> float) (v:float array) (m:float array array) ->
 let l = Array.length m
 and c = Array.length m.(0) in
  let cc = c - 1
  and w = Array.make_matrix l c 0. in
   for i = 0 to l - 1 do
    let row_input = m.(i)
    and row_output = w.(i) in
    for j = 0 to cc do
     row_output.(j) <- f v.(j) row_input.(j)
    done ;
   done ;
   w ;;

(** {v vector_float_apply2 function vector vector v} *)
let vector_float_apply2 = fun (f:float -> float -> float) (u:float array) (v:float array) ->
 let l = Array.length u in
  let w = Array.make l 0. in
   for i = 0 to l - 1 do
    w.(i) <- f u.(i) v.(i)
   done ;
   w ;;

(** {v matrix_float_apply2 function matrix matrix v} *)
let matrix_float_apply2 = fun (f:float -> float -> float) (m:float array array) (w:float array array) ->
 let r = Array.length m
 and cc = Array.length m.(0) - 1 in
  let mm = Array.make_matrix r (Array.length m.(0)) 0. in
   for i = 0 to (Array.length m) - 1 do
    let row_input_left = m.(i)
    and row_input_right = w.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do
      row_output.(j) <- f row_input_left.(j) row_input_right.(j)
     done
    done ;
    mm ;;

(** {v matrix_vector_float_row_apply function vector matrix v} *)
let matrix_vector_float_row_apply = fun (f:float -> float -> float) (m:float array array) (v:float array) ->
 let l = Array.length m
 and c = Array.length m.(0) in
  let cc = c - 1
  and w = Array.make_matrix l c 0. in
   for i = 0 to l - 1 do
    let row_input = m.(i)
    and row_output = w.(i) in
     for j = 0 to cc do
      row_output.(j) <- f row_input.(j) v.(j)
     done ;
   done ;
   w ;;

(** {v matrix_float_row_apply_scal function matrix v} *)
let matrix_float_row_apply_scal = fun (f:float array -> float) (m:float array array) ->
 let l = Array.length m in
  let w = Array.make l 0. in
   for i = 0 to l - 1 do
    let row_input = m.(i) in
     w.(i) <- f row_input
   done ;
   w ;;

(** {v matrix_float_column_apply_scal function matrix v} *)
let matrix_float_column_apply_scal = fun (f:float array -> float) (m:float array array) ->
 matrix_float_row_apply_scal f ( float_transpose m ) ;;

(** {v matrix_float_row_apply_vect function matrix v} *)
let matrix_float_row_apply_vect = fun (f:float array -> float array) (m:float array array) ->
 let l = Array.length m in
  let w = Array.make l ( f m.(0) ) in
   for i = 0 to l - 1 do
    let row_input = m.(i) in
     w.(i) <- f row_input
   done ;
   w ;;

(** {v matrix_float_column_apply_vect function matrix v} *)
let matrix_float_column_apply_vect = fun (f:float array -> float array) (m:float array array) ->
 float_transpose ( matrix_float_row_apply_vect f ( float_transpose m ) ) ;;


(** {v float_sym matrix v} *)
let float_sym = function (m:float array array) ->
 matrix_float_scal_mult 0.5 ( matrix_float_plus (float_transpose m) m ) ;;

(** {v float_antisym matrix v} *)
let float_antisym = function (m:float array array) ->
 matrix_float_scal_mult 0.5 ( matrix_float_minus m (float_transpose m) ) ;;


(** {v vector_float_mean vector v} *)
let vector_float_mean = function (v:float array) ->
 ( vector_float_sum v ) /. ( float (Array.length v) ) ;;

(** {v vector_float_median vector v} *)
let vector_float_median = function (v:float array) ->
 let vv = List.fast_sort compare (Array.to_list v) in
  let l = List.length vv in
   let ll = l / 2 in
    if l mod 2 = 1 then List.nth vv ll
    else ( let a = List.nth vv ll and b = List.nth vv (ll - 1 ) in ( a +. b ) /. 2. ) ;;

(** {v matrix_float_median matrix v} *)
let matrix_float_median = function (m:float array array) ->
 vector_float_median ( inline m ) ;;

(** {v matrix_float_median_by_row matrix v} *)
let matrix_float_median_by_row = function (m:float array array) ->
 Array.map vector_float_median m ;;

(** {v matrix_float_median_by_column matrix v} *)
let matrix_float_median_by_column = function (m:float array array) ->
 matrix_float_median_by_row ( float_transpose m ) ;;

(** {v matrix_float_composed_median matrix v} *)
let matrix_float_composed_median = function (m:float array array) ->
 vector_float_median ( matrix_float_median_by_row m ) ;;

(** {v matrix_float_mean_median matrix v} *)
let matrix_float_mean_median = function (m:float array array) ->
 vector_float_mean ( matrix_float_median_by_row m ) ;;


(** {v float_eliminate vector_array v} *)
let float_eliminate = fun (v:float array array) ->
 let s = Array.length v.(0)
 and w = float_transpose v
 and condition = fun b z -> b || ( classify_float z ) = FP_infinite || ( classify_float z ) = FP_nan in
  let x = ref [| |] in
   for i = 0 to pred s do
    if not ( Array.fold_left condition false w.(i) ) then
     x := Array.append !x [| w.(i) |]
   done ;
   float_transpose !x ;;

(** {v vector_float_var vector v} *)
let vector_float_var = function (v:float array) ->
 let accu = ref 0.
 and accumul = ref 0. in
  for i = 0 to (Array.length v) - 1 do
   accu := v.(i) +. !accu ;
   accumul := v.(i) *. v.(i) +. !accumul
  done ;
  let q = ( float (Array.length v) ) in
   ( !accumul -. !accu *. !accu /. q ) /. q ;;

(** {v vector_float_stdev vector v} *)
let vector_float_stdev = function (v:float array) ->
 sqrt ( vector_float_var v ) ;;

(** {v vector_float_covar vector1 vector2 v} *)
let vector_float_covar = fun (v:float array) (vv:float array) ->
 let accu = ref 0.
 and accum = ref 0.
 and accumul = ref 0. in
  for i = 0 to (Array.length v) - 1 do
   accu := v.(i) +. !accu ;
   accum := vv.(i) +. !accum ;
   accumul := v.(i) *. vv.(i) +. !accumul
  done ;
  let q = ( float (Array.length v) ) in
   ( !accumul -. !accu *. !accum /. q ) /. q ;;

(** {v vector_float_linear_regression vector1 vector2 v} *)
let vector_float_linear_regression = fun (v:float array) (w:float array) ->
 let accuV = ref 0.
 and accuW = ref 0.
 and accuV2 = ref 0.
 and accuW2 = ref 0.
 and accuVW = ref 0. in
  for i = 0 to (Array.length v) - 1 do
   accuV := v.(i) +. !accuV ;
   accuV2 := v.(i) *. v.(i) +. !accuV2 ;
   accuVW := v.(i) *. w.(i) +. !accuVW ;
   accuW := w.(i) +. !accuW ;
   accuW2 := w.(i) *. w.(i) +. !accuW2 ;
  done ;
  let q = ( float (Array.length v) ) in
   let vbar = !accuV /. q
   and varv = ( !accuV2 -. !accuV *. !accuV /. q ) /. q
   and wbar = !accuW /. q
   and varw = ( !accuW2 -. !accuW *. !accuW /. q ) /. q
   and covar = ( !accuVW -. !accuV *. !accuW /. q ) /. q in
     let a = covar /. varv in
      let b = wbar -. a *. vbar
      and rho = covar /. sqrt ( varv *. varw ) in
       [| a ; b ; rho |] ;;

(** {v vector_float_norm_inf vector v} *)
let vector_float_norm_inf = function (v:float array) ->
 let accu = ref (-. max_float) in
  for i = 0 to (Array.length v) - 1 do
   accu := max ( abs_float v.(i) ) !accu
  done ;
  !accu ;;

(** {v vector_float_norm_1 vector v} *)
let vector_float_norm_1 = function (v:float array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length v) - 1 do
   accu := ( abs_float v.(i) ) +. !accu
  done ;
  !accu ;;

(** {v vector_float_norm_2 vector v} *)
let vector_float_norm_2 = function (v:float array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length v) - 1 do
   accu := v.(i) *. v.(i) +. !accu
  done ;
  sqrt ( !accu ) ;;

(** {v vector_float_square_norm_2 vector v} *)
let vector_float_square_norm_2 = function (v:float array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length v) - 1 do
   accu := v.(i) *. v.(i) +. !accu
  done ;
  !accu ;;

(** {v vector_float_norm exponent vector v} *)
let vector_float_norm = fun a (v:float array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length v) - 1 do
   accu :=  exp ( a *. log ( abs_float v.(i) ) ) +. !accu
  done ;
  exp ( log ( !accu ) /. a ) ;;


(** {v matrix_float_norm_inf matrix v} *)
let matrix_float_norm_inf = function (m:float array array) ->
 let accu = ref (-. max_float) in
  for i = 0 to (Array.length m) - 1 do
   let accumul = ref 0.
   and row_input = m.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     accumul := ( abs_float row_input.(j) ) +. !accumul
    done ;
    accu := max !accumul !accu
  done ;
  !accu ;;

(** {v matrix_float_norm_1 matrix v} *)
let matrix_float_norm_1 = function (m:float array array) ->
 matrix_float_norm_inf ( float_transpose m ) ;;

(** {v matrix_float_norm_frobenius matrix v} *)
let matrix_float_norm_frobenius = function (m:float array array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     accu := ( row_input.(j) *. row_input.(j) ) +. !accu
    done ;
  done ;
  sqrt !accu ;;

(** {v matrix_float_square_frobenius matrix v} *)
let matrix_float_square_frobenius = function (m:float array array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     accu := ( row_input.(j) *. row_input.(j) ) +. !accu
    done ;
  done ;
  !accu ;;

(** {v matrix_float_frobenius_prod matrix v} *)
let matrix_float_frobenius_prod = fun (m:float array array) (p:float array array) ->
 let accu = ref 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = p.(i)  in
    for j = 0 to (Array.length row_input_left) - 1 do
     accu := ( row_input_left.(j) *. row_input_right.(j) ) +. !accu
    done ;
  done ;
  !accu ;;

(** {v matrix_float_norm_1_approx matrix v} *)
let matrix_float_norm_1_approx = function (m:float array array) ->
 let accu = ref (-. max_float)
 and mm = matrix_float_bal_random (Array.length m.(0)) ( Util.int_max 100 ( Util.int_max (Array.length m.(0)) (Array.length m) ) ) 1. in
  let mmm = matrix_float_prod m mm in
  for i = 0 to (Array.length mmm.(0)) - 1 do
   let col_input = extract_column i mm
   and col_output = extract_column i mmm in
    let a = vector_float_norm_1 col_input
    and b = vector_float_norm_1 col_output in
     accu := max ( b /. a ) !accu
  done ;
  !accu ;;

(** {v matrix_float_norm_2_approx matrix v} *)
let matrix_float_norm_2_approx = function (m:float array array) ->
 let accu = ref (-. max_float)
 and mm = matrix_float_bal_random (Array.length m.(0)) ( Util.int_max 100 ( Util.int_max (Array.length m.(0)) (Array.length m) ) ) 1. in
  let mmm = matrix_float_prod m mm in
  for i = 0 to (Array.length mmm.(0)) - 1 do
   let col_input = extract_column i mm
   and col_output = extract_column i mmm in
    let a = vector_float_norm_2 col_input
    and b = vector_float_norm_2 col_output in
     accu := max ( b /. a ) !accu
  done ;
  !accu ;;

(** {v matrix_float_norm_approx exponent matrix v} *)
let matrix_float_norm_approx = fun alpha (m:float array array) ->
 let accu = ref (-. max_float)
 and mm = matrix_float_bal_random (Array.length m.(0)) ( Util.int_max 100 ( Util.int_max (Array.length m.(0)) (Array.length m) ) ) 1. in
  let mmm = matrix_float_prod m mm in
  for i = 0 to (Array.length mmm.(0)) - 1 do
   let col_input = extract_column i mm
   and col_output = extract_column i mmm in
    let a = vector_float_norm alpha col_input
    and b = vector_float_norm alpha col_output in
     accu := max ( b /. a ) !accu
  done ;
  !accu ;;


(** {v matrix_float_non_diag_part norm matrix v} *)
let matrix_float_non_diag_part = function (m:float array array) ->
let w = matrix_float_copy m in
 for i = 0 to ( Array.length m ) - 1 do
  w.(i).(i) <- 0. ;
 done ;
 w ;;

(** {v matrix_float_non_diagonality norm matrix v} *)
let matrix_float_non_diagonality = fun (distance:float array array -> float) (m:float array array) ->
 let mm = matrix_float_non_diag_part m in
  distance mm ;;

(** {v matrix_float_non_scal_part norm matrix v} *)
let matrix_float_non_scal_part = function (m:float array array) ->
 let rr = float (Array.length m) in
  matrix_float_scal_left_sub ( (float_trace m) /. rr ) m ;;

(** {v matrix_float_non_scalarity norm matrix v} *)
let matrix_float_non_scalarity = fun (distance:float array array -> float) (m:float array array) ->
 let mm = matrix_float_non_scal_part m in
  distance mm ;;




(** {C § } *)
(** 
{1 Calcul élémentaire sur les matrices entières}
{1 Elementary calculus for integer matrices}
*)
(** {C  } *)




(** {v vector_int_copy vector v} *)
let vector_int_copy = function (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- v.(i)
   done ;
   vv ;;

(** {v vector_int_clip vector v} *)
let vector_int_clip = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- min x v.(i)
   done ;
   vv ;;

(** {v vector_int_crest integer vector v} *)
let vector_int_crest = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    let y = v.(i) in
     if y > x then
      vv.(i) <- y - x
   done ;
   vv ;;

(** {v vector_int_gully integer vector v} *)
let vector_int_gully = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    let y = v.(i) in
     if y < x then
      vv.(i) <- x - y
   done ;
   vv ;;


(** {v other_vector_int_copy vector v} *)
let other_vector_int_copy = function (v:int array) ->
  let vv = Array.make (Array.length v) 0 in
   for i = 0 to (Array.length v) - 1 do
    vv.(i) <- v.(i)
   done ;
   vv ;;

(** {v matrix_int_copy vector v} *)
let matrix_int_copy = function (m:int array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- row_input.(j)
    done
  done ;
  mm ;;

(** {v matrix_int_clip vector v} *)
let matrix_int_clip = fun (x:int) (m:int array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- min x row_input.(j)
    done
  done ;
  mm ;;

(** {v matrix_int_crest integer vector v} *)
let matrix_int_crest = fun (x:int) (m:int array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     let y = row_input.(j) in
      if y > x then
       row_output.(j) <- y - x
    done
  done ;
  mm ;;

(** {v matrix_int_gully integer vector v} *)
let matrix_int_gully = fun (x:int) (m:int array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     let y = row_input.(j) in
      if y < x then
       row_output.(j) <- x - y
    done
  done ;
  mm ;;


(** {v int_sub_matrix matrix beg-row end-row beg-col end-col v} *)
let int_sub_matrix = fun (m:int array array) i ii j jj ->
(** i et j : beginning: début ; ii et jj : end: fin *)
 let mm = Array.make_matrix (ii - i + 1) (jj - j + 1) 0 in
  for index = i to ii do
   let row_input = m.(index)
   and row_output = mm.(index - i) in
    for indice = j to jj do
     row_output.(indice - j) <- row_input.(indice)
    done
  done ;
 mm ;;

(** {v float_of_vector vector v} *)
let float_of_vector = function (v:int array) ->
 let vv = Array.make (Array.length v) 0. in
  for i = 0 to (Array.length v) - 1 do
   vv.(i) <- float_of_int v.(i)
  done ;
  vv ;;

(** {v float_of_matrix matrix v} *)
let float_of_matrix = function (m:int array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0. in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- float_of_int row_input.(j)
    done
  done ;
  mm ;;


(** {v int_transpose matrix v} *)
let int_transpose = function (m:int array array) ->
 let mm = Array.make_matrix (Array.length m.(0)) (Array.length m) 0 in
   for i = 0 to (Array.length m - 1) do
    let row_input = m.(i) in
    for j = 0 to (Array.length row_input - 1) do
     mm.(j).(i) <- row_input.(j)
    done
   done ;  
  mm;;


(** {v vector_int_max vector v} *)
let vector_int_max = function (v:int array) ->
 let accu = ref v.(0) in
  for i = 0 to (Array.length v) - 1 do
   accu := Util.int_max v.(i) !accu
  done ;
  !accu ;;


(** {v matrix_int_max_by_row matrix v} matrix_int_max_by_row even for lacunar matrices:

matrix_int_max_by_row fonctionne même pour les matrices lacunaires. *)
let matrix_int_max_by_row = function (m:int array array) ->
 let accu = Array.make (Array.length m) (- max_int) in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i)
   and accumul = ref accu.(i) in
    for j = 0 to (Array.length row) - 1 do
     accumul := Util.int_max row.(j) !accumul
    done ;
    accu.(i) <- !accumul
  done ;
  accu ;;

(** {v matrix_int_max_by_column matrix v} *)
let matrix_int_max_by_column = function (m:int array array) ->
 let cc = (numcolumns m) - 1 in
  let accu = Array.make (cc + 1) (- max_int) in
   for j = 0 to cc do
    let accumul = ref accu.(j) in
     for i = 0 to ( (Array.length m) - 1 ) do
      accumul := Util.int_max m.(i).(j) !accumul
     done ;
     accu.(j) <- !accumul
   done ;
  accu ;;

(** {v matrix_int_max matrix v} *)
let matrix_int_max = function (m:int array array) ->
 let accu = ref (- max_int) in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i) in
    for j = 0 to (Array.length row) - 1 do
     accu := Util.int_max row.(j) !accu
    done
  done ;
  !accu ;;


(** {v vector_int_min vector v} *)
let vector_int_min = function (v:int array) ->
 let accu = ref v.(0) in
  for i = 0 to (Array.length v) - 1 do
   accu := min v.(i) !accu
  done ;
  !accu ;;


(** {v matrix_int_min_by_row matrix v} matrix_int_min_by_row even for lacunar matrices:

matrix_int_min_by_row fonctionne même pour les matrices lacunaires. *)
let matrix_int_min_by_row = function (m:int array array) ->
 let accu = Array.make (Array.length m) (- min_int) in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i)
   and accumul = ref accu.(i) in
    for j = 0 to (Array.length row) - 1 do
     accumul := min row.(j) !accumul
    done ;
    accu.(i) <- !accumul
  done ;
  accu ;;

(** {v matrix_int_min_by_column matrix v} *)
let matrix_int_min_by_column = function (m:int array array) ->
 let cc = (numcolumns m) - 1 in
  let accu = Array.make (cc + 1) (- min_int) in
   for j = 0 to cc do
    let accumul = ref accu.(j) in
     for i = 0 to ( (Array.length m) - 1 ) do
      accumul := min m.(i).(j) !accumul
     done ;
     accu.(j) <- !accumul
   done ;
  accu ;;

(** {v matrix_int_min matrix v} *)
let matrix_int_min = function (m:int array array) ->
 let accu = ref (- min_int) in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i) in
    for j = 0 to (Array.length row) - 1 do
     accu := min row.(j) !accu
    done
  done ;
  !accu ;;


(** {v vector_int_find_last element vector v} vector_int_find_last returns [-1] if it does not find:

vector_int_find_last retourne [-1] s'il ne trouve pas. *)
let vector_int_find_last = fun x (v:int array) ->
 let index = ref (-1) in
  let i = ref ( (Array.length v) - 1 ) in
   while  !i >= 0 do
    if x = v.(!i) then (index := !i ; i := -1) else i := !i - 1 ; 
   done ;
   !index ;;


(** {v vector_int_find_first element vector v} vector_int_find_first returns [-1] if it does not find:

vector_int_find_first retourne [-1] s'il ne trouve pas. *)
let vector_int_find_first = fun x (v:int array) ->
 let index = ref (-1)
 and i = ref 0 in
  while  !i < Array.length v do
   if x = v.(!i) then (index := !i ; i := Array.length v ) else i := !i + 1 ; 
  done ;
 !index ;;


(** {v vector_int_find_twin element vector v} vector_int_find_first returns [-1] if it does not find:

vector_int_find_twin retourne [-1] s'il ne trouve pas. *)
let vector_int_find_twin = fun x (v:int array) ->
 let index = ref (-1)
 and i = ref 0 in
  while  !i <= min ((Array.length v) - 1) ( int_of_float ( ceil ( (float (Array.length v)) /. 2. ) ) )  do
   if x = v.(!i) then (index := !i ; i := 1 + Array.length v )
   else
    begin
     let j = (Array.length v) - 1 - !i in
     if x = v.(j) then ( index := j ; i := 1 + Array.length v ) else i := !i + 1 ; 
    end
  done ;
 !index ;;


(** {v vector_int_find_all element vector v} vector_int_find_all returns [[||]]  if it does not find:

vector_int_find_all retourne [[||]] s'il ne trouve pas.*)
let vector_int_find_all = fun x (v:int array) ->
 let index = ref [||] in
  for i = 0 to (Array.length v) - 1 do
   if (x = v.(i)) then (index := Array.append !index [|i|] ; ())
  done ;
 !index ;;


(** {v matrix_int_find_last element vector v} matrix_int_find_last returns [[|-1;-1|]] if it does not find:

matrix_int_find_last retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_int_find_last = fun x (m:int array array) ->
 let index = ref (-1) and indice = ref (-1) in
  let i = ref ( (Array.length m) - 1 ) in
   while  !i >= 0 do
    let row = m.(!i) in
     let j = ref ( (Array.length row) - 1 ) in
      while  !j >= 0 do
       if x = row.(!j) then (indice := !j ; j := -1) else j := !j - 1 ; 
      done ;
      if !indice <> -1 then (index := !i ; i := -1) else i := !i - 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_int_find_first element vector v} matrix_find_first returns [[|-1;-1|]] if it does not find:

matrix_int_find_first retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_int_find_first = fun x (m:int array array) ->
 let index = ref (-1) and indice = ref (-1) in
  let i = ref 0 in
   while  !i < Array.length m do
    let row = m.(!i) in
     let j = ref 0 in
      while  !j < Array.length row do
       if x = row.(!j) then ( indice := !j ; j := Array.length row ) else j := !j + 1 ; 
      done ;
      if !indice <> -1 then ( index := !i ; i := Array.length m ) else i := !i + 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_int_find_twin element vector v} matrix_find_twin returns [[|-1;-1|]] if it does not find:

matrix_int_find_twin retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_int_find_twin = fun x (m:int array array) ->
 let index = ref (-1) and indice = ref (-1) in
  let i = ref 0 in
   while  !i <= min (Array.length m - 1) ( int_of_float ( ceil ( (float (Array.length m)) /. 2. ) ) ) do
    let row = m.(!i) in
     let j = ref 0 in
      while  !j <= min (Array.length row - 1) ( int_of_float ( ceil ( (float (Array.length row)) /. 2. ) ) ) do
       if x = row.(!j) then ( indice := !j ; j := 1 + Array.length row )
       else
        begin
         let k = (Array.length row) - 1 - !j in
         if x = row.(k) then ( indice := k ; j := 1 + Array.length row ) else j := !j + 1 ; 
        end
      done ;
      if !indice <> -1 then ( index := !i ; i := 1 + Array.length m ) else i := !i + 1 ;
   done ;
   [| !index ; !indice |] ;;


(** {v matrix_int_find_all element vector v} matrix_find_all returns [[|-1;-1|]] if it does not find:

matrix_int_find_all retourne [[|-1;-1|]] s'il ne trouve pas. *)
let matrix_int_find_all = fun x (m:int array array) ->
 let result = ref [||] and index = ref [||] and indice = ref [||] in
  for i = 0 to Array.length m - 1 do
   let row = m.(i) in
    for j = 0 to Array.length row -1 do
     if x = row.(j) then ( indice := Array.append !indice [|j|] ; () )
    done ;
    if !indice <> [||] then 
     begin
      index := Array.map (function a -> [| i ; a|]) !indice ; 
      result := Array.append !result !index 
     end
  done ;
  !result ;;


(** {v vector_int_sum vector v} *)
let vector_int_sum = function (v:int array) ->
 let accu = ref 0 in
  for i = 0 to (Array.length v) - 1 do
   accu := v.(i) + !accu
  done ;
  !accu ;;

(** {v vector_int_contraction vector v} *)
let vector_int_contraction = function (v:int array) ->
 let accu = ref 1 in
  for i = 0 to (Array.length v) - 1 do
   accu := v.(i) * !accu
  done ;
  !accu ;;

(** {v matrix_int_sum_by_row matrix v} *)
let matrix_int_sum_by_row = function (m:int array array) ->
 let accu = Array.make (Array.length m) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i)
   and accumul = ref accu.(i) in
    for j = 0 to (Array.length row) - 1 do
     accumul := row.(j) + !accumul
    done ;
    accu.(i) <- !accumul
  done ;
  accu ;;

(** {v matrix_int_sum_by_column matrix v} *)
let matrix_int_sum_by_column = function (m:int array array) ->
 let cc = (numcolumns m) - 1 in
  let accu = Array.make (cc + 1) 0 in
   for j = 0 to cc do
    let accumul = ref accu.(j) in
     for i = 0 to ( (Array.length m) - 1 ) do
      accumul := m.(i).(j) + !accumul
     done ;
     accu.(j) <- !accumul
   done ;
  accu ;;

(** {v matrix_int_sum matrix v} *)
let matrix_int_sum = function (m:int array array) ->
 let accu = ref 0 in
  for i = 0 to (Array.length m) - 1 do
   let row = m.(i) in
    for j = 0 to (Array.length row) - 1 do
     accu := row.(j) + !accu
    done
  done ;
  !accu ;;


(** {v vector_int_abs vector v} *)
let vector_int_abs = function (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- abs v.(i)
   done ;
   vv ;;

(** {v matrix_int_abs matrix v} *)
let matrix_int_abs = function (m:int array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- abs row_input.(j)
    done
  done ;
  mm ;;

(** {v matrix_int_opp matrix v} *)
let matrix_int_opp = function (m:int array array) ->
 let mm = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = mm.(i) in
    for j = 0 to (Array.length row_input) - 1 do    
     row_output.(j) <- - row_input.(j)
    done
  done ;
  mm ;;


(** {v string_of_vector_int vector v} *)
let string_of_vector_int = function (v:int array) ->
 let rr = Array.length v - 1
 and s = ref "[| " in
  for i = 0 to ( rr - 1 ) do
   s := !s ^ ( string_of_int v.(i) ) ^ " ; " 
  done ;
  s := !s ^ ( string_of_int v.(rr) ) ^ " |]" ;
  !s ;;

(** {v vector_int_print vector v} *)
let vector_int_print = function (v:int array) ->
 let s = string_of_vector_int v in
 print_string s ;
 print_newline () ;;

(** {v bare_vector_int_to_string vector v} *)
let bare_vector_int_to_string = function (v:int array) ->
 let r = Array.length v
  and s = ref "[|" in
  for i = 0 to ( r - 2 ) do
   s := !s ^ ( string_of_int v.(i) ) ^ " " 
  done ;
  s := !s ^ ( string_of_int v.(r - 1) ) ^ "|]" ;
  !s ;;

(** {v bare_vector_int_print vector v} *)
let bare_vector_int_print = function (v:int array) ->
 let s = bare_vector_int_to_string v in
  print_string s ;;


(** {v vector_int_of_string string v} *)
let vector_int_of_string = function (s:string) ->
 let ls = String.length s in
  let st = String.sub s 3 ( ls - 6 ) in
   let listing = Str.split ( Str.regexp " ; " ) st in
    let a = Array.of_list listing in
     Array.map int_of_string a ;;

(** {v bare_vector_int_of_string string v} *)
let bare_vector_int_of_string = function (s:string) ->
 let ls = String.length s in
  let st = String.sub s 2 ( ls - 4 ) in
   let listing = Str.split ( Str.regexp " " ) st in
    let a = Array.of_list listing in
     Array.map int_of_string a ;;


(** {v matrix_int_to_string matrix v} *)
let matrix_int_to_string = function (m:int array array) ->
 let s = ref "[| with "
 and r = Array.length m
 and c = slow_numcolumns m in
  s := !s ^ string_of_int r ;
  if r > 1 then s := !s ^ " rows"
  else s := !s ^ " row" ;
  s := !s ^ "\n" ;
  s := !s ^ bare_vector_int_to_string m.(0) ^ " ;" ; 
  for i = 1 to ( Array.length m - 1 ) do
   begin
    s := !s ^ "\n" ^ ( bare_vector_int_to_string m.(i) ) ^ " ;"
   end
  done ;
  s := !s ^ "\n|] and " ^ ( string_of_int c ) ;
  if c > 1 then s := !s ^ " columns\n"
  else s := !s ^ " column\n" ;
  !s ;;


(** {v matrix_int_print matrix v} *)
let matrix_int_print = function (m:int array array) ->
 let s = matrix_int_to_string m in
  print_string s ;;


(** {v matrix_int_of_string string v} *)
let matrix_int_of_string = function (s:string) ->
 let ls = String.length s in
  let st = String.sub s 8 ( ls - 8 ) in
   let number_of_rows = 
    begin
     let position = Str.search_forward ( Str.regexp " row" ) st 0 in
      int_of_string ( String.sub st 0 position )
    end in
    let m = Array.make_matrix number_of_rows 0 0
    and listing = Str.split ( Str.regexp " ;\n" ) s in
     let a = Array.sub ( Array.of_list listing ) 0 number_of_rows in
      for i = 1 to pred number_of_rows do
       m.(i) <- bare_vector_int_of_string a.(i)
      done ;
      let str = List.hd ( List.tl ( Str.split ( Str.regexp "\n" ) a.(0) ) ) in
       m.(0) <- bare_vector_int_of_string str ;
       m ;;


(** {v int_trace matrix v} *)
let int_trace = function (m:int array array) ->
 let r = min (Array.length m) (Array.length m.(0))
 and accumulateur = ref 0 in
  if r > 0 then 
   for i = 0 to r - 1 do
    accumulateur := !accumulateur + m.(i).(i)
   done ;
  !accumulateur ;;


(** {v vector_int_scal_add coefficient vector v} *)
let vector_int_scal_add = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- x + v.(i)
   done ;
   vv ;;

(** {v vector_int_scal_mult coefficient vector v} *)
let vector_int_scal_mult = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- x * v.(i)
   done ;
   vv ;;

(** {v vector_int_scal_left_sub coefficient vector v} *)
let vector_int_scal_left_sub = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- v.(i) - x
   done ;
   vv ;;

(** {v vector_int_scal_right_sub coefficient vector v} *)
let vector_int_scal_right_sub = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- x - v.(i)
   done ;
   vv ;;

(** {v vector_int_scal_left_div coefficient vector v} *)
let vector_int_scal_left_div = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- v.(i) / x
   done ;
   vv ;;

(** {v vector_int_scal_right_div coefficient vector v} *)
let vector_int_scal_right_div = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- x / v.(i)
   done ;
   vv ;;

(** {v vector_int_scal_left_mod coefficient vector v} *)
let vector_int_scal_left_mod = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- x mod v.(i)
   done ;
   vv ;;

(** {v vector_int_scal_right_mod coefficient vector v} *)
let vector_int_scal_right_mod = fun (x:int) (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- v.(i) mod x
   done ;
   vv ;;

(** {v vector_int_opp vector v} *)
let vector_int_opp = fun (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    vv.(i) <- - v.(i)
   done ;
   vv ;;

(** {v vector_int_inv vector v} *)
let vector_int_inv = fun (v:int array) ->
 let r = Array.length v in
  let vv = Array.make r 0 in
   for i = 0 to r - 1 do
    begin
     let x = v.(i) in
      if x == 0 then failwith "Division by zero in Matrix.vector_int_inv." ;
      vv.(i) <- 1 / x
    end
   done ;
   vv ;;


(** {v vector_int_plus vector1 vector2 v} *)
let vector_int_plus = fun (v:int array) (vv:int array) ->
 let r = Array.length v in
  let vvv = Array.make r 0 in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) + vv.(i)
   done ;
   vvv ;;

(** {v vector_int_minus vector1 vector2 v} *)
let vector_int_minus = fun (v:int array) (vv:int array) ->
 let r = Array.length v in
  let vvv = Array.make r 0 in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) - vv.(i)
   done ;
   vvv ;;

(** {v vector_int_coeff_prod vector1 vector2 v} *)
let vector_int_coeff_prod = fun (v:int array) (vv:int array) ->
 let r = Array.length v in
  let vvv = Array.make r 0 in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) * vv.(i)
   done ;
   vvv ;;

(** {v vector_int_coeff_div vector1 vector2 v} *)
let vector_int_coeff_div = fun (v:int array) (vv:int array) ->
 let r = Array.length v in
  let vvv = Array.make r 0 in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) / vv.(i)
   done ;
   vvv ;;

(** {v vector_int_coeff_mod vector1 vector2 v} *)
let vector_int_coeff_mod = fun (v:int array) (vv:int array) ->
 let r = Array.length v in
  let vvv = Array.make r 0 in
   for i = 0 to r - 1 do
    vvv.(i) <- v.(i) mod vv.(i)
   done ;
   vvv ;;

(** {v vector_int_scal_prod vector1 vector2 v} *)
let vector_int_scal_prod = fun (v:int array) (vv:int array) ->
 let x = ref 0 in
  for i = 0 to (Array.length v) - 1 do
   x := !x + v.(i) * vv.(i)
  done ; 
  !x ;;


(** {v partial_int_scal_add beginning end vector1 vector2 v} *)
let partial_int_scal_add = fun i j x (v:int array) ->
 let w = Array.make (Array.length v) 0 in
  for k = i to j do
   w.(k) <- v.(k) + x
  done ;
  w ;;

(** {v part_int_scal_add beginning end vector1 vector2 v} *)
let part_int_scal_add = fun i j x (v:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) + x
   done ;
   w ;;

(** {v partial_int_scal_mult beginning end vector1 vector2 v} *)
let partial_int_scal_mult = fun i j x (v:int array) ->
 let w = Array.make (Array.length v) 0 in
  for k = i to j do
   w.(k) <- v.(k) * x
  done ;
  w ;;

(** {v part_int_scal_mult beginning end vector1 vector2 v} *)
let part_int_scal_mult = fun i j x (v:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) * x
   done ;
   w ;;

(** {v partial_int_scal_left_sub beginning end vector1 vector2 v} *)
let partial_int_scal_left_sub = fun i j x (v:int array) ->
 let w = Array.make (Array.length v) 0 in
  for k = i to j do
   w.(k) <- v.(k) - x
  done ;
  w ;;

(** {v part_int_scal_left_sub beginning end vector1 vector2 v} *)
let part_int_scal_left_sub = fun i j x (v:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) - x
   done ;
   w ;;

(** {v partial_int_scal_right_sub beginning end vector1 vector2 v} *)
let partial_int_scal_right_sub = fun i j x (v:int array) ->
 let w = Array.make (Array.length v) 0 in
  for k = i to j do
   w.(k) <- x - v.(k)
  done ;
  w ;;

(** {v part_int_scal_right_sub beginning end vector1 vector2 v} *)
let part_int_scal_right_sub = fun i j x (v:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- x - v.(k)
   done ;
   w ;;

(** {v partial_int_scal_left_div beginning end vector1 vector2 v} *)
let partial_int_scal_left_div = fun i j x (v:int array) ->
 let w = Array.make (Array.length v) 0 in
  for k = i to j do
   w.(k) <- v.(k) / x
  done ;
  w ;;

(** {v part_int_scal_left_div beginning end vector1 vector2 v} *)
let part_int_scal_left_div = fun i j x (v:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) / x
   done ;
   w ;;

(** {v partial_int_scal_right_div beginning end vector1 vector2 v} *)
let partial_int_scal_right_div = fun i j x (v:int array) ->
 let w = Array.make (Array.length v) 0 in
  for k = i to j do
   w.(k) <- x / v.(k)
  done ;
  w ;;

(** {v part_int_scal_right_div beginning end vector1 vector2 v} *)
let part_int_scal_right_div = fun i j x (v:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- x / v.(k)
   done ;
   w ;;

(** {v partial_int_scal_left_mod beginning end vector1 vector2 v} *)
let partial_int_scal_left_mod = fun i j x (v:int array) ->
 let w = Array.make (Array.length v) 0 in
  for k = i to j do
   w.(k) <- x mod v.(k)
  done ;
  w ;;

(** {v part_int_scal_left_mod beginning end vector1 vector2 v} *)
let part_int_scal_left_mod = fun i j x (v:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- x mod v.(k)
   done ;
   w ;;

(** {v partial_int_scal_right_mod beginning end vector1 vector2 v} *)
let partial_int_scal_right_mod = fun i j x (v:int array) ->
 let w = Array.make (Array.length v) 0 in
  for k = i to j do
   w.(k) <- v.(k) mod x
  done ;
  w ;;

(** {v part_int_scal_right_mod beginning end vector1 vector2 v} *)
let part_int_scal_right_mod = fun i j x (v:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) mod x
   done ;
   w ;;



(** {v partial_int_plus beginning end vector1 vector2 v} *)
let partial_int_plus = fun i j (v:int array) (vv:int array) ->
 let c = min (Array.length v) (Array.length vv) in
  let w = Array.make c 0 in
   for k = i to j do
    w.(k) <- v.(k) + vv.(k)
   done ;
   w ;;

(** {v part_int_plus beginning end vector1 vector2 v} *)
let part_int_plus = fun i j (v:int array) (vv:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) + vv.(k)
   done ;
   w ;;

(** {v partial_int_minus beginning end vector1 vector2 v} *)
let partial_int_minus = fun i j s t ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c 0 in
   for k = i to j do
    m.(k) <- s.(k) - t.(k)
   done ;
   m ;;

(** {v part_int_minus beginning end vector1 vector2 v} *)
let part_int_minus = fun i j (v:int array) (vv:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) - vv.(k)
   done ;
   w ;;

(** {v partial_int_coeff_prod beginning end vector1 vector2 v} *)
let partial_int_coeff_prod = fun i j (v:int array) (vv:int array) ->
 let c = min (Array.length v) (Array.length vv) in
  let w = Array.make c 0 in
   for k = i to j do
    w.(k) <- v.(k) * vv.(k)
   done ;
   w ;;

(** {v part_int_coeff_prod beginning end vector1 vector2 v} *)
let part_int_coeff_prod = fun i j (v:int array) (vv:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) * vv.(k)
   done ;
   w ;;

(** {v partial_int_coeff_div beginning end vector1 vector2 v} *)
let partial_int_coeff_div = fun i j (v:int array) (vv:int array) ->
 let c = min (Array.length v) (Array.length vv) in
  let w = Array.make c 0 in
   for k = i to j do
    w.(k) <- v.(k) / vv.(k)
   done ;
   w ;;

(** {v part_int_coeff_div beginning end vector1 vector2 v} *)
let part_int_coeff_div = fun i j (v:int array) (vv:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) / vv.(k)
   done ;
   w ;;

(** {v partial_int_coeff_mod beginning end vector1 vector2 v} *)
let partial_int_coeff_mod = fun i j (v:int array) (vv:int array) ->
 let c = min (Array.length v) (Array.length vv) in
  let w = Array.make c 0 in
   for k = i to j do
    w.(k) <- v.(k) mod vv.(k)
   done ;
   w ;;

(** {v part_int_coeff_mod beginning end vector1 vector2 v} *)
let part_int_coeff_mod = fun i j (v:int array) (vv:int array) ->
 let w = Array.make ( j - i + 1 ) 0 in
   for k = i to j do
    w.(k-i) <- v.(k) mod vv.(k)
   done ;
   w ;;


(** {v matrix_int_scal_add coefficient matrix v} *)
let matrix_int_scal_add = fun x (m:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = ref w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     !row_output.(j) <- x + row_input.(j)
    done ;
    w.(i) <- !row_output
  done ;
  w ;;

(** {v matrix_int_scal_mult coefficient matrix v} *)
let matrix_int_scal_mult = fun x (m:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = ref w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     !row_output.(j) <- x * row_input.(j)
    done ;
    w.(i) <- !row_output
  done ;
  w ;;

(** {v matrix_int_scal_left_sub coefficient matrix v} *)
let matrix_int_scal_left_sub = fun x (m:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- row_input.(j) - x
    done ;
  done ;
  w ;;

(** {v matrix_int_scal_right_sub coefficient matrix v} *)
let matrix_int_scal_right_sub = fun x (m:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- x - row_input.(j)
    done ;
  done ;
  w ;;

(** {v matrix_int_scal_left_div coefficient matrix v} *)
let matrix_int_scal_left_div = fun x (m:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- row_input.(j) / x
    done ;
  done ;
  w ;;

(** {v matrix_int_scal_right_div coefficient matrix v} *)
let matrix_int_scal_right_div = fun x (m:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- x / row_input.(j)
    done ;
  done ;
  w ;;

(** {v matrix_int_scal_left_mod coefficient matrix v} *)
let matrix_int_scal_left_mod = fun x (m:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- x mod row_input.(j)
    done ;
  done ;
  w ;;

(** {v matrix_int_scal_right_mod coefficient matrix v} *)
let matrix_int_scal_right_mod = fun x (m:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     row_output.(j) <- row_input.(j) mod x
    done ;
  done ;
  w ;;


(** {v matrix_int_plus matrix1 matrix2 v} *)
let matrix_int_plus = fun (m:int array array) (mm:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) + row_input_right.(j)
    done ;
  done ;
  w ;;

(** {v matrix_int_minus matrix1 matrix2 v} *)
let matrix_int_minus = fun (m:int array array) (mm:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) - row_input_right.(j)
    done ;
  done ;
  w ;;

(** {v matrix_int_coeff_prod matrix1 matrix2 v} *)
let matrix_int_coeff_prod = fun (m:int array array) (mm:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) * row_input_right.(j)
    done ;
  done ;
  w ;;

(** {v matrix_int_coeff_div matrix1 matrix2 v} *)
let matrix_int_coeff_div = fun (m:int array array) (mm:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) / row_input_right.(j)
    done ;
  done ;
  w ;;

(** {v matrix_int_coeff_mod matrix1 matrix2 v} *)
let matrix_int_coeff_mod = fun (m:int array array) (mm:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_input_right = mm.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_input_left) - 1 do
     row_output.(j) <- row_input_left.(j) mod row_input_right.(j)
    done ;
  done ;
  w ;;


(** {v matrix_int_twisted_prod matrix1 matrix2 v} This calculates matrix1 times transpose ( matrix2 )

Ceci calcule matrix1 fois transposée de matrix2. *)
let matrix_int_twisted_prod = fun (m:int array array) (mm:int array array) ->
 let r = Array.length m
 and c = Array.length mm
 and t = min ( Array.length m.(0) ) ( Array.length mm.(0) ) in
  let rr = r - 1
  and cc = c - 1
  and tt = t - 1
  and w = Array.make_matrix r c 0 in
   for i = 0 to rr do
    let row_input_left = m.(i)
    and row_output = w.(i) in
     for j = 0 to cc do
      let row_input_right = mm.(j)
      and coeff = ref row_output.(j) in
       for k = 0 to tt do
        coeff := !coeff + row_input_left.(k) * row_input_right.(k)
       done ;
       row_output.(j) <- !coeff
     done ;
   done ;
   w ;;

(** {v matrix_int_prod matrix1 matrix2 v} *)
let matrix_int_prod = fun (m:int array array) (mm:int array array) ->
 matrix_int_twisted_prod m ( int_transpose mm ) ;;


(** {v matrix_int_twisted_commut matrix1 matrix2 v} *)
let matrix_int_twisted_commut = fun (m:int array array) (mm:int array array) ->
 matrix_int_minus ( matrix_int_twisted_prod m mm ) ( matrix_int_twisted_prod mm m ) ;;

(** {v matrix_int_twisted_commut_bis matrix1 matrix2 v} *)
let matrix_int_twisted_commut_bis = fun (m:int array array) (mm:int array array) ->
 let m_m = int_transpose m
 and m_mm = int_transpose mm in
  matrix_int_minus ( matrix_int_twisted_prod m mm ) ( matrix_int_twisted_prod m_mm m_m ) ;;

(** {v matrix_int_commut matrix1 matrix2 v} *)
let matrix_int_commut = fun (m:int array array) (mm:int array array) ->
 matrix_int_minus ( matrix_int_prod m mm ) ( matrix_int_prod mm m ) ;;

(** {v matrix_int_triple_prod matrix1 matrix2 v} *)
let matrix_int_triple_prod = fun (a:int array array) (b:int array array) (c:int array array) ->
 matrix_int_twisted_prod a ( matrix_int_twisted_prod ( int_transpose c ) b ) ;;

(** {v matrix_int_naive_prod matrix1 matrix2 v} *)
let matrix_int_naive_prod = fun (m:int array array) (mm:int array array) ->
 let w = Array.make_matrix (Array.length m) (Array.length mm.(0)) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input_left = m.(i)
   and row_output = w.(i) in
    for j = 0 to (Array.length row_output) - 1 do
     for k = 0 to (Array.length mm) - 1 do
      row_output.(j) <- row_input_left.(k) * mm.(k).(j) + row_output.(j)
     done
    done ;
  done ;
  w ;;

(** {v vector_matrix_int_prod vector matrix v} *)
let vector_matrix_int_prod = fun (v:int array) (m:int array array) ->
 let w = Array.make (Array.length m.(0)) 0 in
  for i = 0 to (Array.length m.(0)) - 1 do
   let row_input = m.(i)
   and output = ref w.(i) in
    for j = 0 to (Array.length m) - 1 do
     output := row_input.(j) * v.(j) + !output
    done ;
    w.(i) <- !output ;
  done ;
  w ;;

(** {v matrix_vector_int_prod vector matrix v} *)
let matrix_vector_int_prod = fun (m:int array array) (v:int array) ->
 let w = Array.make (Array.length m) 0 in
  for i = 0 to (Array.length m) - 1 do
   let row_input = m.(i)
   and output = ref w.(i) in
    for j = 0 to (Array.length v) - 1 do
     output := row_input.(j) * v.(j) + !output
    done ;
    w.(i) <- !output ;
  done ;
  w ;;


(** {v vector_matrix_int_apply function vector matrix v} *)
let vector_matrix_int_apply = fun (f:int -> int -> int) (v:int array) (m:int array array) ->
 let l = Array.length m
 and c = Array.length m.(0) in
  let cc = c - 1
  and w = Array.make_matrix l c 0 in
   for i = 0 to l - 1 do
    let row_input = m.(i)
    and row_output = w.(i) in
    for j = 0 to cc do
     row_output.(j) <- f v.(j) row_input.(j)
    done ;
   done ;
   w ;;

(** {v vector_int_apply2 function vector vector v} *)
let vector_int_apply2 = fun (f:int -> int -> int) (u:int array) (v:int array) ->
 let l = Array.length u in
  let w = Array.make l 0 in
   for i = 0 to l - 1 do
    w.(i) <- f u.(i) v.(i)
   done ;
   w ;;

(** {v matrix_int_apply2 function matrix matrix v} *)
let matrix_int_apply2 = fun (f:int -> int -> int) (m:int array array) (w:int array array) ->
 let r = Array.length m
 and cc = Array.length m.(0) - 1 in
  let mm = Array.make_matrix r (Array.length m.(0)) 0 in
   for i = 0 to (Array.length m) - 1 do
    let row_input_left = m.(i)
    and row_input_right = w.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do
      row_output.(j) <- f row_input_left.(j) row_input_right.(j)
     done
    done ;
    mm ;;

(** {v matrix_vector_int_row_apply function vector matrix v} *)
let matrix_vector_int_row_apply = fun (f:int -> int -> int) (m:int array array) (v:int array) ->
 let l = Array.length m
 and c = Array.length m.(0) in
  let cc = c - 1
  and w = Array.make_matrix l c 0 in
   for i = 0 to l - 1 do
    let row_input = m.(i)
    and row_output = w.(i) in
     for j = 0 to cc do
      row_output.(j) <- f row_input.(j) v.(j)
     done ;
   done ;
   w ;;

(** {v matrix_int_row_apply_scal function matrix v} *)
let matrix_int_row_apply_scal = fun (f:int array -> int) (m:int array array) ->
 let l = Array.length m in
  let w = Array.make l 0 in
   for i = 0 to l - 1 do
    let row_input = m.(i) in
     w.(i) <- f row_input
   done ;
   w ;;

(** {v matrix_int_column_apply_scal function matrix v} *)
let matrix_int_column_apply_scal = fun (f:int array -> int) (m:int array array) ->
 matrix_int_row_apply_scal f ( int_transpose m ) ;;

(** {v matrix_int_row_apply_vect function matrix v} *)
let matrix_int_row_apply_vect = fun (f:int array -> int array) (m:int array array) ->
 let l = Array.length m in
  let w = Array.make l ( f m.(0) ) in
   for i = 0 to l - 1 do
    let row_input = m.(i) in
     w.(i) <- f row_input
   done ;
   w ;;

(** {v matrix_int_column_apply_vect function matrix v} *)
let matrix_int_column_apply_vect = fun (f:int array -> int array) (m:int array array) ->
 int_transpose ( matrix_int_row_apply_vect f ( int_transpose m ) ) ;;


(** {v int_sym matrix v} *)
let int_sym = function (m:int array array) ->
 matrix_int_scal_left_div 2 ( matrix_int_plus (int_transpose m) m ) ;;

(** {v int_antisym matrix v} *)
let int_antisym = function (m:int array array) ->
 matrix_int_scal_left_div 2 ( matrix_int_minus m (int_transpose m) ) ;;


(** {v vector_int_norm_inf vector v} *)
let vector_int_norm_inf = function (v:int array) ->
 let accu = ref (- max_int) in
  for i = 0 to (Array.length v) - 1 do
   accu := Util.int_max ( abs v.(i) ) !accu
  done ;
  !accu ;;

(** {v vector_int_norm_1 vector v} *)
let vector_int_norm_1 = function (v:int array) ->
 let accu = ref 0 in
  for i = 0 to (Array.length v) - 1 do
   accu := ( abs v.(i) ) + !accu
  done ;
  !accu ;;


(** {v matrix_int_norm_inf matrix v} *)
let matrix_int_norm_inf = function (m:int array array) ->
 let accu = ref (- max_int) in
  for i = 0 to (Array.length m) - 1 do
   let accumul = ref 0
   and row_input = m.(i) in
    for j = 0 to (Array.length row_input) - 1 do
     accumul := ( abs row_input.(j) ) + !accumul
    done ;
    accu := Util.int_max !accumul !accu
  done ;
  !accu ;;

(** {v matrix_int_norm_1 matrix v} *)
let matrix_int_norm_1 = function (m:int array array) ->
 matrix_int_norm_inf ( int_transpose m ) ;;

(** {v matrix_int_non_diagonality norm matrix v} *)
let matrix_int_non_diagonality = fun (distance:int array array -> int) (m:int array array) ->
 let mm = matrix_int_minus m ( diag_int (extract_diag m) ) in
  distance mm ;;

(** {v matrix_int_non_scalarity norm matrix v} *)
let matrix_int_non_scalarity = fun (distance:int array array -> int) (m:int array array) ->
 let r = Array.length m in
  let mm = matrix_int_scal_left_sub ( (int_trace m) / r ) m in
   distance mm ;;




(** {C § } *)
(**
{1 Calcul substantiel sur les matrices}
{1 Substantial calculus on matrices}
*)
(** {C  } *)




(** {v float_diag_left_mult vector matrix v} *)
let float_diag_left_mult = fun (d:float array) (m:float array array) ->
 let r = min (Array.length m) (Array.length d)
 and cc = ( Array.length m.(0) ) - 1 in
  let mm = Array.make_matrix r ( cc + 1 ) 0. in
   for i = 0 to r - 1 do
    let x = d.(i)
    and row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do 
      row_output.(j) <- x *. row_input.(j)
     done
   done ;
   mm ;;

(** {v float_diag_left_div vector matrix v} *)
let float_diag_left_div = fun (d:float array) (m:float array array) ->
 let r = min (Array.length m) (Array.length d)
 and cc = ( Array.length m.(0) ) - 1 in
  let mm = Array.make_matrix r ( cc + 1 ) 0. in
   for i = 0 to r - 1 do
    let x = d.(i)
    and row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do 
      row_output.(j) <- row_input.(j) /. x
     done
   done ;
   mm ;;

(** {v float_diag_right_mult vector matrix v} *)
let float_diag_right_mult = fun (d:float array) (m:float array array) ->
 let r = min (Array.length m) (Array.length d)
 and cc = ( Array.length m.(0) ) - 1 in
  let mm = Array.make_matrix r ( cc + 1 ) 0. in
   for i = 0 to r - 1 do
    let row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do 
      row_output.(j) <- d.(j) *. row_input.(j)
     done
   done ;
   mm ;;

(** {v float_diag_right_div vector matrix v} *)
let float_diag_right_div = fun (d:float array) (m:float array array) ->
 let r = min (Array.length m) (Array.length d)
 and cc = ( Array.length m.(0) ) - 1 in
  let mm = Array.make_matrix r ( cc + 1 ) 0. in
   for i = 0 to r - 1 do
    let row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do 
      row_output.(j) <- row_input.(j) /. d.(j)
     done
   done ;
   mm ;;


(** {v int_diag_left_mult vector matrix v} *)
let int_diag_left_mult = fun (d:int array) (m:int array array) ->
 let r = min (Array.length m) (Array.length d)
 and cc = ( Array.length m.(0) ) - 1 in
  let mm = Array.make_matrix r ( cc + 1 ) 0 in
   for i = 0 to r - 1 do
    let x = d.(i)
    and row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do 
      row_output.(j) <- x * row_input.(j)
     done
   done ;
   mm ;;

(** {v int_diag_left_div vector matrix v} *)
let int_diag_left_div = fun (d:int array) (m:int array array) ->
 let r = min (Array.length m) (Array.length d)
 and cc = ( Array.length m.(0) ) - 1 in
  let mm = Array.make_matrix r ( cc + 1 ) 0 in
   for i = 0 to r - 1 do
    let x = d.(i)
    and row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do 
      row_output.(j) <- row_input.(j) / x
     done
   done ;
   mm ;;

(** {v int_diag_right_mult vector matrix v} *)
let int_diag_right_mult = fun (d:int array) (m:int array array) ->
 let r = min (Array.length m) (Array.length d)
 and cc = ( Array.length m.(0) ) - 1 in
  let mm = Array.make_matrix r ( cc + 1 ) 0 in
   for i = 0 to r - 1 do
    let row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do 
      row_output.(j) <- d.(j) * row_input.(j)
     done
   done ;
   mm ;;

(** {v int_diag_right_div vector matrix v} *)
let int_diag_right_div = fun (d:int array) (m:int array array) ->
 let r = min (Array.length m) (Array.length d)
 and cc = ( Array.length m.(0) ) - 1 in
  let mm = Array.make_matrix r ( cc + 1 ) 0 in
   for i = 0 to r - 1 do
    let row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to cc do 
      row_output.(j) <- row_input.(j) / d.(j)
     done
   done ;
   mm ;;



(** {v float_slow_pivot_downward i m p v} The matrix m is assumed to be upper triangular from line 0 to i - 1.
The matrix p registers the same changes as m.
Output: pivot acted on the matrix [m], pivot acted on the matrix [p], 
pair of permutation indexes, value of the inverse of the pivot.

La matrice m est supposée, de la ligne 0 à i - 1, triangulaire supérieure. 
La matrice p enregistre les mêmes changements que m.
Sortie : pivot effectué sur la matrice [m], pivot effecturé sur la matrice [p], 
paire d'indices de permutation, valeur de l'inverse du pivot. *)
let float_slow_pivot_downward = fun (i:int) (m:float array array) (p:float array array) ->
 let r = Array.length m
 and mmmm = ref (matrix_float_copy m)
 and pppp = ref (matrix_float_copy p)
 and c = numcolumns m in
  let s = min r c 
  and permutation = ref ( [| [| 0. ; 0. |] |] )
  and mm = float_sub_matrix m i (r - 1) i (c - 1) in 
   let mmm = matrix_float_abs mm in
    let plusgrand = matrix_max mmm in
     let index = matrix_float_find_first plusgrand mmm in
      if index.(0) <> 0 then 
       begin
        mmmm := exchange_row i (i+index.(0)) !mmmm ;
        pppp := exchange_row i (i+index.(0)) !pppp ;
       end ;
      if index.(1) <> 0 then 
       begin
        permutation := float_of_matrix [| [| i ; i+index.(1) |] |] ;
        mmmm := exchange_column i (i+index.(1)) !mmmm ;
       end ;
      let coefficient = !mmmm.(i).(i) in
       if coefficient == 0. then failwith "Division by zero in Matrix.float_slow_pivot_downward." ;
       let row = vector_float_scal_left_div coefficient !mmmm.(i) 
       and ligne = vector_float_scal_left_div coefficient !pppp.(i) in
       for j = i + 1 to s - 1 do (** Pay attention to order: attention à l'ordre *)
        let coeff = !mmmm.(j).(i) in 
         !pppp.(j) <- vector_float_minus !pppp.(j) ( vector_float_scal_mult coeff ligne ) ;
         !mmmm.(j) <- partial_float_minus (i + 1) (c - 1) !mmmm.(j) ( partial_float_scal_mult (i + 1) (c - 1) coeff row ) ;
       done ;
       [| !mmmm ; !pppp ; !permutation ; [|[|1. /. coefficient|]|] |]  ;;


(** {v float_restricted_slow_pivot_downward i m v} The matrix m is assumed to be upper triangular from line 0 to i - 1.
Output: pivot acted on the matrix [m], value of the pivot, 
flag indicating a transposition of indexes.

La matrice m est supposée, de la ligne 0 à i - 1, triangulaire supérieure.
Sortie : pivot effectué sur la matrice [m], valeur du pivot, 
drapeau indiquant une transposition des indices. *)
let float_restricted_slow_pivot_downward = fun (i:int) (m:float array array) ->
 let r = Array.length m
 and change_sign = ref false
 and flag = ref 0.
 and mmmm = ref (matrix_float_copy m)
 and c = numcolumns m in
  let s = min r c 
  and mm = float_sub_matrix m i (r - 1) i (c - 1) in 
   let mmm = matrix_float_abs mm in
    let plusgrand = matrix_max mmm in
     let index = matrix_float_find_first plusgrand mmm in
      if index.(0) <> 0 then 
       begin
        change_sign := not !change_sign ;
        mmmm := exchange_row i ( i + index.(0) ) !mmmm ;
       end ;
      if index.(1) <> 0 then 
       begin
        change_sign := not !change_sign ;
        mmmm := exchange_column i ( i + index.(1) ) !mmmm ;
       end ;
      let coefficient = !mmmm.(i).(i) in
       if ( coefficient <> 0. ) then
        begin
         let row = vector_float_scal_left_div coefficient !mmmm.(i) in
          for j = i + 1 to s - 1 do 
           let coeff = !mmmm.(j).(i) in 
            !mmmm.(j) <- partial_float_minus (i + 1) (c - 1) !mmmm.(j) ( partial_float_scal_mult (i + 1) (c - 1) coeff row ) ;
          done ;
        end ;
        if !change_sign then flag := 1. else flag := 0. ;
        [| !mmmm ; [|[| coefficient |]|] ; [|[| !flag |]|] |] ;;


(** {v float_slow_pivot_upward i m p v} The matrix m is assumed to be diagonal from line i + 1 to r - 1.
The matrix p registers the same changes as m.
Output: pivot acted on the matrix [m], pivot acted on the matrix [p].

La matrice m est supposée, de la ligne i + 1 à r - 1, diagonale. 
La matrice p enregistre les memes changements que m.
Sortie : pivot effectué sur la matrice [m], pivot effecturé sur la matrice [p]. *)
let float_slow_pivot_upward = fun (i:int) (m:float array array) (p:float array array) (coefficient:float) ->
 let mmmm = ref (matrix_float_copy m)
 and pppp = ref (matrix_float_copy p) in
  let row = vector_float_scal_mult coefficient !mmmm.(i) 
  and ligne = vector_float_scal_mult coefficient !pppp.(i) in
   for j = i - 1 downto 0 do (** Pay attention to order : attention à l'ordre *)
    let coeff = !mmmm.(j).(i) in 
     !pppp.(j) <- vector_float_minus !pppp.(j) ( vector_float_scal_mult coeff ligne ) ;
     !mmmm.(j) <- partial_float_minus j (i - 1) !mmmm.(j) ( partial_float_scal_mult j (i - 1) coeff row ) ;
   done ;
   [| !mmmm ; !pppp |];;


(** {v float_slow_inv matrix v} *)
let float_slow_inv = function (m:float array array) ->
 let r = ( min (numrows m) (numcolumns m) ) 
 and mm = ref (matrix_float_copy m)
 and permucol = ref []
 and pp = ref ( identity_float (Array.length m) (Array.length m.(0)) ) in
  let diagonale = Array.make r 0.
  and rr = pred r in
   for i = 0 to r - 2 do
    let resultat = float_slow_pivot_downward i !mm !pp in
     mm := resultat.(0) ;
     pp := resultat.(1) ;
     permucol := resultat.(2) :: !permucol ;
     diagonale.(i) <- resultat.(3).(0).(0)
   done ;
   let coeff = !mm.(rr).(rr) in
    if coeff == 0. then failwith "Division by zero in Matrix.float_slow_inv." ;
    diagonale.(rr) <- 1. /. coeff ;
   for i = rr downto 1 do
    let resultat = float_slow_pivot_upward i !mm !pp diagonale.(i) in
     mm := resultat.(0) ;
     pp := resultat.(1) ;
   done ;
    mm := float_diag_left_mult diagonale !pp ;
    let index = ref [| [| 0 ; 0 |] |] in
     while Util.list_non_empty !permucol do
      index := int_of_matrix ( List.hd !permucol ) ;
      mm := exchange_row (!index.(0).(0)) (!index.(0).(1)) !mm ;
      permucol := List.tl !permucol ;
     done ;
     !mm ;;


(** {v matrix_float_slow_left_quotient matrix1 matrix2 v} *)
let matrix_float_slow_left_quotient = fun (m:float array array) (p:float array array) ->
 let r = ( min (numrows m) (numcolumns m) ) 
 and mm = ref ( matrix_float_copy m )
 and permucol = ref []
 and pp = ref ( matrix_float_copy p ) in
  let diagonale = Array.make r 0.
  and rr = pred r in
   for i = 0 to r - 2 do
    let resultat = float_slow_pivot_downward i !mm !pp in
     mm := resultat.(0) ;
     pp := resultat.(1) ;
     permucol := resultat.(2) :: !permucol ;
     diagonale.(i) <- resultat.(3).(0).(0)
   done ;
   let coeff = !mm.(rr).(rr) in
    if coeff == 0. then failwith "Division by zero in Matrix.float_slow_left_quotient." ;
    diagonale.(rr) <- 1. /. coeff ;
   for i = rr downto 1 do
    let resultat = float_slow_pivot_upward i !mm !pp diagonale.(i) in
     mm := resultat.(0) ;
     pp := resultat.(1) ;
   done ;
    mm := float_diag_left_mult diagonale !pp ;
    let index = ref [| [| 0 ; 0 |] |] in
     while Util.list_non_empty !permucol do
      index := int_of_matrix ( List.hd !permucol ) ;
      mm := exchange_row (!index.(0).(0)) (!index.(0).(1)) !mm ;
      permucol := List.tl !permucol ;
     done ;
     !mm ;;


(** {v matrix_float_slow_right_quotient matrix1 matrix2 v} This gives matrix2 * (matrix1) ^ -1.

Ceci retourne matrix2 * (matrix1) ^ -1. *)
let matrix_float_slow_right_quotient = fun (m:float array array) (p:float array array) ->
 float_transpose ( matrix_float_slow_left_quotient (float_transpose m) (float_transpose p) ) ;;


(** {v line_float_slow_left_quotient matrix1 matrix_array v} *)
let line_float_slow_left_quotient = fun (m:float array array) (p:float array array array) ->
 Array.map ( matrix_float_slow_left_quotient m ) p ;;

(** {v line_float_slow_right_quotient matrix1 matrix_array v} *)
let line_float_slow_right_quotient = fun (m:float array array) (p:float array array array) ->
 Array.map ( matrix_float_slow_right_quotient m ) p ;;


(** {v float_slow_invertibility matrix v} *)
let float_slow_invertibility = function (m:float array array) ->
 let r = min (Array.length m) (numcolumns m)
 and mm = ref (matrix_float_copy m) in
  let i = ref 0
  and rr = pred r
  and output = ref true in
   while !i < rr do
    let resultat = float_restricted_slow_pivot_downward !i !mm in
     if ( resultat.(1).(0).(0) = 0. ) then ( i := r ; output := false )
     else
      begin 
       mm := resultat.(0) ;
       i := !i + 1 ;
      end
   done ;
   output := !output && ( !mm.(rr).(rr) <> 0. ) ;
   !output ;;


(** {v float_slow_invertibility_evaluation matrix v} *)
let float_slow_invertibility_evaluation = function (m:float array array) ->
 let r = min (Array.length m) (numcolumns m)
 and mm = ref (matrix_float_copy m) in
  let i = ref 0
  and rr = pred r
  and diagonale = Array.make r 0.
  and output = ref max_float in
   while !i < rr do
    begin
     let resultat = float_restricted_slow_pivot_downward !i !mm in
      let coeff = resultat.(1).(0).(0) in
       if coeff = 0. then ( i := r ; output := 0. )
       else
        begin
         mm := resultat.(0) ;
         diagonale.(!i) <- coeff ;
         i := !i + 1 ;
        end
    end
   done ;
   if !output <> 0. then 
    begin
     diagonale.(rr) <- !mm.(rr).(rr) ;
     let absdiag = vector_float_abs diagonale in
      let mini = vector_float_min absdiag in
       let index = vector_float_find_first mini absdiag in
        diagonale.(index)
    end
   else 0. ;;


(** {v float_slow_det matrix v} *)
let float_slow_det = function (m:float array array) ->
 let r = min (Array.length m) (numcolumns m)
 and change_sign = ref false
 and accu = ref 1.
 and mm = ref (matrix_float_copy m) in
  let i = ref 0
  and rr = pred r
  and diagonale = Array.make r 0. in
   while !i < rr do
    let resultat = float_restricted_slow_pivot_downward !i !mm in
     let coeff = resultat.(1).(0).(0) in
      if coeff = 0. then ( i := r ; accu := 0. )
      else
       begin
        if resultat.(2).(0).(0) <> 0. then change_sign := not !change_sign ;
        mm := resultat.(0) ;
        diagonale.(!i) <- coeff ;
        i := !i + 1
       end
   done ;
   if !accu <> 0. then 
    begin
     for i = 0 to r - 2 do
      accu := diagonale.(i) *. !accu
     done ;
     let candidat = !mm.(rr).(rr) *. !accu in
      if !change_sign then -. candidat else candidat
    end
   else 0. ;;



(** {v float_pivot_downward i m p v} The matrix m is assumed to be upper triangular from line 0 to i - 1.
The matrix p registers the same changes as [m].
The coefficients under the diagonal of the output matrix mm are not modified since they wil not be used.
Output: pivot acted on the matrix [m], pivot acted on the matrix [p], 
pair of permutation indexes, value of the inverse of the pivot.

La matrice m est supposée, de la ligne 0 à i - 1, triangulaire supérieure. 
La matrice p enregistre les memes changements que [m].
Les coefficients sous la diagonale de la matrice [mm] en sortie ne sont pas modifiés puisqu'ils ne seront pas utilisés.
Sortie : pivot effectué sur la matrice [m], pivot effecturé sur la matrice [p], 
paire d'indices de permutation, valeur de l'inverse du pivot. *)
let float_pivot_downward = fun (i:int) (m:float array array) (p:float array array) ->
 let r = Array.length m
 and mm = matrix_float_copy m
 and pp = identity_float (Array.length m) (Array.length m.(0)) in
  let permutation = ref ( [| [| 0. ; 0. |] |] )
  and rr = pred r
  and accu = ref mm.(i).(i)
  and index = ref i in
   for j = i + 1 to rr do
    if abs_float mm.(j).(i) > abs_float !accu then ( accu := mm.(j).(i) ; index := j )
   done ;
    if !index <> i then 
     begin
      permutation := float_of_matrix [| [| i ; !index |] |] ;
      let aux = mm.(i)
      and auxil = pp.(i) in
       mm.(i) <- mm.(!index) ;
       mm.(!index) <- aux ;
       pp.(i) <- pp.(!index) ;
       pp.(!index) <- auxil 
     end ;
    let row = mm.(i)
    and ligne = pp.(i) in
     let piv = row.(i) in
      if piv == 0. then failwith "Division by zero in Matrix.float_pivot_downward." ;
      let coefficient = 1. /. piv in
       for h = i + 1 to rr do
        let row_output = mm.(h)
        and ligne_sortie = pp.(h) in
         let coeff = row_output.(i) *. coefficient in
          for k = 0 to i do
           ligne_sortie.(k) <- ligne_sortie.(k) -. ligne.(k) *. coeff
          done ;
          for k = i + 1 to rr do
           ligne_sortie.(k) <- ligne_sortie.(k) -. ligne.(k) *. coeff ;
           row_output.(k) <- row_output.(k) -. row.(k) *. coeff
          done ;
       done ;
       [| mm ; pp ; !permutation ; [|[| coefficient |]|] |]  ;;


(** {v float_pivot_upward i m p v} The matrix m is assumed to be diagonal from line i + 1 to r - 1.
The matrix p registers the same changes as m.
Output: pivot acted on the matrix [m], pivot acted on the matrix [p].

La matrice m est supposée, de la ligne i + 1 à r - 1, diagonale. 
La matrice p enregistre les memes changements que m.
Sortie : pivot effectué sur la matrice [m], pivot effecturé sur la matrice [p]. *)
let float_pivot_upward = fun (i:int) (m:float array array) (p:float array array) (coefficient:float) ->
 let mm = matrix_float_copy m
 and pp = matrix_float_copy p in
  let r = Array.length m
  and ligne = pp.(i) in
     for jj = i - 1 downto 0 do
      let ligne_sortie = pp.(jj) in
       let coeff = mm.(jj).(i) *. coefficient in
        for  kk = 0 to jj do
         ligne_sortie.(kk) <- ligne_sortie.(kk) -. ligne.(kk) *. coeff
        done ;
        for kk = i to r - 1 do
         ligne_sortie.(kk) <- ligne_sortie.(kk) -. ligne.(kk) *. coeff
        done ;
        for kk = jj + 1 to i - 1 do
         ligne_sortie.(kk) <- ligne_sortie.(kk) -. ligne.(kk) *. coeff ;
        done ;
     done ;
     [| mm ; pp |] ;;


(** {v float_inv matrix v} *)
let float_inv = function (m:float array array) ->
 let r = Array.length m
 and mm = matrix_float_copy m
 and pp = identity_float (Array.length m) (Array.length m.(0)) in
  let diagonale = Array.make r 1.
  and rr = pred r in
(** pivot downward : descendant *)
   for i = 0 to r - 2 do
    let accu = ref mm.(i).(i)
    and index = ref i in
     for j = i + 1 to rr do
      if abs_float mm.(j).(i) > abs_float !accu then ( accu := mm.(j).(i) ; index := j )
     done ;
      if !index <> i then 
       begin
        let aux = mm.(i)
        and auxil = pp.(i) in
         mm.(i) <- mm.(!index) ;
         mm.(!index) <- aux ;
         pp.(i) <- pp.(!index) ;
         pp.(!index) <- auxil 
       end ;
      let row = mm.(i)
      and ligne = pp.(i) in
       let piv = row.(i) in
        if piv == 0. then failwith "Division by zero in Matrix.float_inv." ;
        let coefficient = 1. /. piv in
         diagonale.(i) <- coefficient ;
         for h = i + 1 to rr do
          let row_output = ref mm.(h)
          and ligne_sortie = ref pp.(h) in
           let coeff = !row_output.(i) *. coefficient in
            for k = 0 to i do
             !ligne_sortie.(k) <- !ligne_sortie.(k) -. ligne.(k) *. coeff
            done ;
            for k = i + 1 to rr do
             !ligne_sortie.(k) <- !ligne_sortie.(k) -. ligne.(k) *. coeff ;
             !row_output.(k) <- !row_output.(k) -. row.(k) *. coeff
            done ;
            pp.(h) <- !ligne_sortie ;
            mm.(h) <- !row_output
         done ;
   done ;
   let piv = mm.( rr ).( rr ) in
    if piv == 0. then failwith "Division by zero at the last row in Matrix.float_inv." ;
    diagonale.( rr ) <- 1. /. piv ;
(** pivot upward : montant *)
   for ii = rr downto 1 do
    let ligne = pp.(ii) in
     let coefficient = diagonale.(ii) in
      for jj = ii - 1 downto 0 do
       let ligne_sortie = ref pp.(jj) in
        let coeff = mm.(jj).(ii) *. coefficient in
         for  kk = 0 to rr do
          !ligne_sortie.(kk) <- !ligne_sortie.(kk) -. ligne.(kk) *. coeff
         done ;
         pp.(jj) <- !ligne_sortie ;
      done
   done ;
(** diagonale *)
   for hh = 0 to rr do
    let row_right = ref pp.(hh)
    and coeff_diag = diagonale.(hh) in
     for ll = 0 to rr do
      !row_right.(ll) <- !row_right.(ll) *. coeff_diag
     done ;
    pp.(hh) <- !row_right
   done ;
   pp ;;


(** {v matrix_float_left_quotient matrix1 matrix2 v} *)
let matrix_float_left_quotient = fun (m:float array array) (p:float array array) ->
 let r = Array.length m
 and mm = matrix_float_copy m
 and pp = matrix_float_copy p in
  let diagonale = Array.make r 1.
  and rr = pred r in
(** pivot downward : descendant *)
   for i = 0 to r - 2 do
    let accu = ref mm.(i).(i)
    and index = ref i in
     for j = i + 1 to rr do
      if abs_float mm.(j).(i) > abs_float !accu then ( accu := mm.(j).(i) ; index := j )
     done ;
      if !index <> i then 
       begin
        let aux = mm.(i)
        and auxil = pp.(i) in
         mm.(i) <- mm.(!index) ;
         mm.(!index) <- aux ;
         pp.(i) <- pp.(!index) ;
         pp.(!index) <- auxil 
       end ;
      let row = mm.(i)
      and ligne = pp.(i) in
       let piv = row.(i) in
        if piv == 0. then failwith "Division by zero in Matrix.float_left_quotient." ;
        let coefficient = 1. /. piv in
         diagonale.(i) <- coefficient ;
         for h = i + 1 to rr do
          let row_output = ref mm.(h)
          and ligne_sortie = ref pp.(h) in
           let coeff = !row_output.(i) *. coefficient in
            for k = 0 to i do
             !ligne_sortie.(k) <- !ligne_sortie.(k) -. ligne.(k) *. coeff
            done ;
            for k = i + 1 to rr do
             !ligne_sortie.(k) <- !ligne_sortie.(k) -. ligne.(k) *. coeff ;
             !row_output.(k) <- !row_output.(k) -. row.(k) *. coeff
            done ;
            pp.(h) <- !ligne_sortie ;
            mm.(h) <- !row_output
         done ;
   done ;
   let piv = mm.( rr ).( rr ) in
    if piv == 0. then failwith "Division by zero at the last row in Matrix.float_left_quotient." ;
    diagonale.( rr ) <- 1. /. piv ;
(** pivot upward : montant *)
  for ii = rr downto 1 do
   let ligne = pp.(ii) in
    let coefficient = diagonale.(ii) in
     for jj = ii - 1 downto 0 do
      let ligne_sortie = ref pp.(jj) in
       let coeff = mm.(jj).(ii) *. coefficient in
        for  kk = 0 to rr do
         !ligne_sortie.(kk) <- !ligne_sortie.(kk) -. ligne.(kk) *. coeff
        done ;
        pp.(jj) <- !ligne_sortie ;
     done
  done ;
(** diagonale *)
  for hh = 0 to rr do
   let row_right = ref pp.(hh)
   and coeff_diag = diagonale.(hh) in
    for ll = 0 to rr do
     !row_right.(ll) <- !row_right.(ll) *. coeff_diag
    done ;
   pp.(hh) <- !row_right
  done ;
  pp ;;


(** {v matrix_float_right_quotient matrix1 matrix2 v} This gives matrix2 * (matrix1) ^ -1.

Ceci retourne matrix2 * (matrix1) ^ -1. *)
let matrix_float_right_quotient = fun (m:float array array) (p:float array array) ->
 float_transpose ( matrix_float_left_quotient (float_transpose m) (float_transpose p) ) ;;

(** {v line_float_left_quotient matrix1 matrix_array v} *)
let line_float_left_quotient = fun (m:float array array) (p:float array array array) ->
 Array.map (matrix_float_left_quotient m) p ;;

(** {v line_float_right_quotient matrix1 matrix_array v} *)
let line_float_right_quotient = fun (m:float array array) (p:float array array array) ->
 Array.map (matrix_float_right_quotient m) p ;;


(** {v float_invertibility matrix v} *)
let float_invertibility = function (m:float array array) ->
 let r = Array.length m
 and mm = matrix_float_copy m
 and resultat = ref true
 and i = ref 0 in
  let rr = pred r in
   while !i <= r - 2 do
    let accu = ref mm.(!i).(!i)
    and index = ref !i in
     for j = !i + 1 to rr do
      if abs_float mm.(j).(!i) > abs_float !accu then ( accu := mm.(j).(!i) ; index := j )
     done ;
     if !accu = 0. then ( resultat := false ; i := r )
     else 
      begin
       if !index <> !i then 
        begin
         let aux = mm.(!i) in
          mm.(!i) <- mm.(!index) ;
          mm.(!index) <- aux
        end ;
       let row = mm.(!i) in
        let coefficient = 1. /. !accu in
         for h = !i + 1 to rr do
          let row_output = ref mm.(h) in
           let coeff = !row_output.(!i) in
            for k = !i + 1 to rr do
             !row_output.(k) <- !row_output.(k) -. row.(k) *. coeff *. coefficient
            done ;
           mm.(h) <- !row_output
         done ;
         i := !i + 1 
      end
   done ;
   let x = mm.(rr).(rr) in
    if ( x = 0. ) then ( resultat := false ) ;
    !resultat ;;


(** {v float_invertibility_evaluation matrix v} *)
let float_invertibility_evaluation = function (m:float array array) ->
 let r = Array.length m
 and mm = matrix_float_copy m
 and resultat = ref max_float
 and i = ref 0 in
  let rr = pred r in
   while !i <= r - 2 do
    let accu = ref mm.(!i).(!i)
    and index = ref !i in
     for j = !i + 1 to rr do
      if abs_float mm.(j).(!i) > abs_float !accu then ( accu := mm.(j).(!i) ; index := j )
     done ;
     if !accu = 0. then ( resultat := 0. ; i := r )
     else 
      begin
       if ( (abs_float !accu) < (abs_float !resultat) ) then resultat := !accu ;
       if !index <> !i then 
        begin
         let aux = mm.(!i) in
          mm.(!i) <- mm.(!index) ;
          mm.(!index) <- aux
        end ;
       let row = mm.(!i) in
        let coefficient = 1. /. !accu in
         for h = !i + 1 to rr do
          let row_output = ref mm.(h) in
           let coeff = !row_output.(!i) in
            for k = !i + 1 to rr do
             !row_output.(k) <- !row_output.(k) -. row.(k) *. coeff *. coefficient
            done ;
           mm.(h) <- !row_output
         done ;
         i := !i + 1 
      end
   done ;
   let x = mm.(rr).(rr) in
    if ( abs_float x < abs_float !resultat ) then ( resultat := x ) ;
    !resultat ;;


(** {v float_det matrix v} *)
let float_det = function (m:float array array) ->
 let r = Array.length m
 and change_sign = ref false
 and mm = matrix_float_copy m
 and resultat = ref 1.
 and i = ref 0 in
  let rr = pred r in
   while !i <= r - 2 do
    let accu = ref mm.(!i).(!i)
    and index = ref !i in
     for j = !i + 1 to rr do
      if abs_float mm.(j).(!i) > abs_float !accu then ( accu := mm.(j).(!i) ; index := j )
     done ;
     if !accu = 0. then ( resultat := 0. ; i := r )
     else 
      begin
       resultat := !accu *. !resultat ;
       if !index <> !i then 
        begin
         change_sign := not !change_sign ;
         let aux = mm.(!i) in
          mm.(!i) <- mm.(!index) ;
          mm.(!index) <- aux
        end ;
       let row = mm.(!i) in
        let coefficient = 1. /. !accu in
         for h = !i + 1 to rr do
          let row_output = ref mm.(h) in
           let coeff = !row_output.(!i) in
            for k = !i + 1 to rr do
             !row_output.(k) <- !row_output.(k) -. row.(k) *. coeff *. coefficient
            done ;
           mm.(h) <- !row_output
         done ;
         i := !i + 1 
      end
   done ;
   let x = mm.(rr).(rr) in
    resultat := x *. !resultat ;
    if !change_sign then -. !resultat else !resultat ;;


(** {v float_dirty_inv matrix v} This may apply to matrices near identity.

Ceci devrait s'appliquer aux matrices proches de l'identité. *)
let float_dirty_inv = function (m:float array array) ->
 let r = Array.length m in
  let mm = Array.make_matrix r r 0.
  and rr = pred r in
   for i = 0 to rr do
    let row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to i - 1 do
      row_output.(j) <- -. row_input.(j)
     done ;
     row_output.(i) <- 2. -. row_input.(i) ;
     for j = i + 1 to rr do
      row_output.(j) <- -. row_input.(j)
     done ;
     mm.(i) <- row_output
   done ;
   mm ;;

(** {v other_float_dirty_inv matrix v} This may apply to matrices near identity.

Ceci devrait s'appliquer aux matrices proches de l'identité. *)
let other_float_dirty_inv = function (m:float array array) ->
 let t = ref ( float_trace m )
 and r = Array.length m in
  let mm = Array.make_matrix r r 0.
  and rr = pred r in
   if !t = 0. then ( t := 2. *. (matrix_float_norm_inf m) /. (float r) ) else ( t := 2. *. !t /. (float r) ) ;
   for i = 0 to rr do
    let row_input = m.(i)
    and row_output = mm.(i) in
     for j = 0 to i - 1 do
      row_output.(j) <- -. row_input.(j)
     done ;
     row_output.(i) <- !t -. row_input.(i) ;
     for j = i + 1 to rr do
      row_output.(j) <- -. row_input.(j)
     done ;
     mm.(i) <- row_output
   done ;
   mm ;;


(** {v slow_float_tune_inv norm matrix matrix_inv_candidate v} This is taken from the HP15C calulator's high level mathematical functions manual.

Input : <matrix> = x ; <matrix_inv_candidate> = y with y = x ^ -1 + epsilon ; <norm> may be any norm on matrices.

Output = < candidate ; error > with

candidate = 2 * y - y * x * y  and

error = norm ( x * candidate - Id ) <= || x || ^ 2 * || epsilon || ^ 2.
Ceci provient du manuel des fonctions mathématiques de haut niveau de la calculette HP15C. *)
let slow_float_tune_inv = fun (distance:float array array -> float) (x:float array array) (y:float array array) ->
 let mm = matrix_float_prod x y
 and r = Array.length x in
  let lambda = scal_float r r 2.
  and delta = matrix_float_minus ( matrix_float_scal_mult 2. mm ) ( matrix_float_prod mm mm ) in
   let candidate = matrix_float_prod y ( matrix_float_minus lambda mm )
   and error = distance ( matrix_float_minus delta (identity_float r r) ) in
    [| candidate ; [|[|error|]|] |] ;;


(** {v float_tune_inv norm matrix matrix_inv_candidate v} This is taken from the HP15C calulator's high level mathematical functions manual.

Input : <matrix> = x ; <matrix_inv_candidate> = y with y = x ^ -1 + epsilon ; <norm> may be any norm on matrices.

Output = < candidate ; error > with

candidate = 2 * y - y * x * y  and

error = norm ( x * candidate - Id ) <= || x || ^ 2 * || epsilon || ^ 2.
Ceci provient du manuel des fonctions mathématiques de haut niveau de la calculette HP15C. *)
let float_tune_inv = fun (distance:float array array -> float) (x:float array array) (y:float array array) ->
 let r = Array.length x in
  let mm = Array.make_matrix r r 0.
  and rr = r - 1 in
   for i = 0 to rr do
    begin
     let row_left = x.(i)
     and row_output = mm.(i) in
      for j = 0 to rr do
       let coeff = ref row_output.(j) in
        for k = 0 to rr do
         coeff := !coeff +. row_left.(k) *. y.(k).(j)
        done ;
        row_output.(j) <- !coeff
      done ;
      mm.(i) <- row_output
    end
   done ;
   let candidate = Array.make_matrix r r 0. in
   for i = 0 to rr do
    begin
     let row_LEFT = y.(i)
     and row_OUTPUT = candidate.(i) in
      for j = 0 to rr do
       let coeff = ref row_OUTPUT.(j) in
        for k = 0 to rr do
         coeff := !coeff +. row_LEFT.(k) *. mm.(k).(j)
        done ;
        coeff := 2. *. row_LEFT.(j) -. !coeff ;
        row_OUTPUT.(j) <- !coeff
      done ;
      candidate.(i) <- row_OUTPUT
    end
   done ;
   let delta = Array.make_matrix r r 0. in
   for i = 0 to rr do
    begin
     let row_Left = x.(i)
     and row_Output = delta.(i) in
      begin
       for j = 0 to rr do
        let coeff = ref row_Output.(j) in
         for k = 0 to rr do
          coeff := !coeff +. row_Left.(k) *. candidate.(k).(j)
         done ;
         row_Output.(j) <- !coeff
       done ;
       row_Output.(i) <- row_Output.(i) -. 1.
      end ;
      delta.(i) <- row_Output
    end
   done ;
   let error = distance delta in
    [| candidate ; [|[|error|]|] |] ;;


(** {v slow_float_approx_inv norm invertor matrix v} If invertor = float_dirty_inv, then this may apply only to matrices near identity.
In other cases, it may enhance the precision of the result.

Dans le cas où l'inverseur vaut invertor = float_dirty_inv, ceci ne devrait s'appliquer qu'aux matrices proches de l'identité.
Dans les autres cas, la précision du résultat pourrait être améliorée. *)
let slow_float_approx_inv = fun (distance:float array array -> float) (invertor:float array array -> float array array) (x:float array array) ->
 let y = invertor x
 and r = Array.length x in
  let product = matrix_float_prod x y
  and result = slow_float_tune_inv distance x y in
   let error0 = distance ( matrix_float_minus product (identity_float r r) )
   and error1 = result.(1).(0).(0)
   and z = result.(0) in
    if error1 >= error0 then [| y ; [|[| error0 |]|] |] else [| z ; [|[| error1 |]|] |] ;; 


(** {v float_approx_inv norm invertor matrix v} If invertor = float_dirty_inv, then this may apply only to matrices near identity.
In other cases, it may enhance the precision of the result.

Dans le cas où l'inverseur vaut invertor = float_dirty_inv, ceci ne devrait s'appliquer qu'aux matrices proches de l'identité.
Dans les autres cas, la précision du résultat pourrait être améliorée. *)
let float_approx_inv = fun (distance:float array array -> float) (invertor:float array array -> float array array) (x:float array array) ->
 let y = invertor x
 and r = Array.length x in
  let product = matrix_float_prod x y
  and result = float_tune_inv distance x y in
   let error0 = distance ( matrix_float_minus product (identity_float r r) )
   and error1 = result.(1).(0).(0)
   and z = result.(0) in
    if error1 >= error0 then [| y ; [|[| error0 |]|] |] else [| z ; [|[| error1 |]|] |] ;; 


(** {v slow_float_loop_approx_inv norm invertor matrix v} If invertor = float_dirty_inv, then this may apply only to matrices near identity.
In other cases, it may enhance the precision of the result.

Dans le cas où invertor = float_dirty_inv, ceci ne devrait s'appliquer qu'aux matrices proches de l'identité.
Dans les autres cas, la précision du résultat pourrait être améliorée. *)
let slow_float_loop_approx_inv = fun (distance:float array array -> float) (invertor:float array array -> float array array) (x:float array array) ->
 let y = ref (invertor x)
 and r = Array.length x in
  let product = matrix_float_prod x !y in
   let error0 = ref max_float
   and error1 = ref ( distance ( matrix_float_minus product (identity_float r r) ) ) in
    while error1 < error0 do
     begin
      let result = slow_float_tune_inv distance x !y in
       let error2 = ref ( result.(1).(0).(0) ) in
        if !error2 < !error1 then
         begin
          error0 := !error1 ;
          error1 := !error2 ;
          y := result.(0)
         end
        else error0 := !error1
     end
    done ;
    [| !y ; [|[| !error1 |]|] |] ;;


(** {v slow_float_target_inv norm invertor threshold matrix v} If invertor = float_dirty_inv, then this may apply only to matrices near identity.
In other cases, it may enhance the precision of the result. The stop test is evaluated on the candidate for the inverse.

Dans le cas où invertor = float_dirty_inv, ceci ne devrait s'appliquer qu'aux matrices proches de l'identité.
Dans les autres cas, la précision du résultat pourrait être améliorée.
Le test d'arrêt est réalisé sur le candidat inverse. *)
let slow_float_target_inv = fun (distance:float array array -> float) (invertor:float array array -> float array array) (threshold:float) (x:float array array) ->
 let y = ref ( zeros_float x )
 and z = ref ( invertor x ) in
  let error0 = ref 0.
  and error1 = ref ( distance !z ) in
   while ( !error1 > threshold ) or ( !error1 > !error0 ) do
    begin
     y := !z ;
     error0 := !error1 ;
     let result = slow_float_tune_inv distance x !y in
      z := result.(0) ;
      error1 := distance ( matrix_float_minus !z !y ) ;
    end
   done ;
   [| !z ; [|[| !error1 |]|] |] ;;


(** {v float_loop_approx_inv norm invertor matrix v} If invertor = float_dirty_inv, then this may apply only to matrices near identity.
In other cases, it may enhance the precision of the result.

Dans le cas où invertor = float_dirty_inv, ceci ne devrait s'appliquer qu'aux matrices proches de l'identité.
Dans les autres cas, la précision du résultat pourrait être améliorée. *)
let float_loop_approx_inv = fun (distance:float array array -> float) (invertor:float array array -> float array array) (x:float array array) ->
 let y = ref (invertor x)
 and r = Array.length x in
  let product = matrix_float_prod x !y in
   let error0 = ref max_float
   and error1 = ref ( distance ( matrix_float_minus product (identity_float r r) ) ) in
    while error1 < error0 do
     begin
      let result = float_tune_inv distance x !y in
       let error2 = ref ( result.(1).(0).(0) ) in
        if !error2 < !error1 then
         begin
          error0 := !error1 ;
          error1 := !error2 ;
          y := result.(0)
         end
        else error0 := !error1
     end
    done ;
    [| !y ; [|[| !error1 |]|] |] ;;


(** {v float_target_inv norm invertor threshold matrix v} If invertor = float_dirty_inv, then this may apply only to matrices near identity.
In other cases, it may enhance the precision of the result. The stop test is evaluated on the candidate for the inverse.

Dans le cas où invertor = float_dirty_inv, ceci ne devrait s'appliquer qu'aux matrices proches de l'identité.
Dans les autres cas, la précision du résultat pourrait être améliorée.
Le test d'arrêt est réalisé sur le candidat inverse. *)
let float_target_inv = fun (distance:float array array -> float) (invertor:float array array -> float array array) (threshold:float) (x:float array array) ->
 let y = ref ( zeros_float x )
 and z = ref ( invertor x ) in
  let error0 = ref 0.
  and error1 = ref ( distance !z ) in
   while ( !error1 > threshold ) or ( !error1 > !error0 ) do
    begin
     y := !z ;
     error0 := !error1 ;
     let result = float_tune_inv distance x !y in
      z := result.(0) ;
      error1 := distance ( matrix_float_minus !z !y ) ;
    end
   done ;
   if error1 < error0 then [| !z ; [|[| !error1 |]|] |]
   else [| !y ; [|[| !error0 |]|] |] ;;


(** {v float_target_inv_seq norm invertor threshold matrix v} If invertor = float_dirty_inv, then this may apply only to matrices near identity.
In other cases, it may enhance the precision of the result. The stop test is evaluated on the candidate for the inverse.

Dans le cas où invertor = float_dirty_inv, ceci ne devrait s'appliquer qu'aux matrices proches de l'identité.
Dans les autres cas, la précision du résultat pourrait être améliorée.
Le test d'arrêt est réalisé sur le candidat inverse. *)
let float_target_inv_seq = fun (distance:float array array -> float) (invertor:float array array -> float array array) (threshold:float) (x:float array array) ->
 let y = ref ( zeros_float x )
 and z = ref ( invertor x ) in
  let error0 = ref 0.
  and seq = ref [| matrix_float_copy !z |]
  and error1 = ref ( distance !z ) in
   while ( !error1 > threshold ) or ( !error1 > !error0 ) do
    begin
     y := !z ;
     error0 := !error1 ;
     let result = float_tune_inv distance x !y in
      z := result.(0) ;
      seq := Array.append !seq [| matrix_float_copy !z |] ;
      error1 := distance ( matrix_float_minus !z !y ) ;
    end
   done ;
   if error1 >= error0 then seq := Array.sub !seq 1 ( pred ( Array.length !seq ) ) ;
   !seq ;;


(** {v float_target_inv_seq accelearator norm invertor threshold matrix v} If invertor = float_dirty_inv, then this may apply only to matrices near identity.
In other cases, it may enhance the precision of the result. The stop test is evaluated on the candidate for the inverse.

Dans le cas où invertor = float_dirty_inv, ceci ne devrait s'appliquer qu'aux matrices proches de l'identité.
Dans les autres cas, la précision du résultat pourrait être améliorée.
Le test d'arrêt est réalisé sur le candidat inverse. *)
let float_compensated_target_inv = fun accelerator (distance:float array array -> float) (invertor:float array array -> float array array) (threshold:float) (x:float array array) ->
 let s = float_target_inv_seq distance invertor threshold x in
  accelerator s ;;


(** {v float_cond norm invertor matrix v} *)
let float_cond = fun (distance:float array array -> float) (invertor:float array array -> float array array) (m:float array array) ->
(distance m) *. ( distance (invertor m) ) ;;


(** {v float_naive_solve invertor matrix vector v} *)
let float_naive_solve = fun (invertor:float array array -> float array array) (m:float array array) (v:float array) ->
 matrix_vector_float_prod (invertor m) v ;;

(** {v float_solve matrix vector v} *)
let float_solve = fun (m:float array array) (v:float array) ->
 let p = float_transpose [| v |] in
  let x = matrix_float_left_quotient m p in
   ( float_transpose x ).(0) ;;

(** {v float_slow_solve matrix vector v} *)
let float_slow_solve = fun (m:float array array) (v:float array) ->
 let p = float_transpose [| v |] in
  let x = matrix_float_slow_left_quotient m p in
   ( float_transpose x ).(0) ;;


(** {v int_det matrix v} *)
let int_det = function (m:int array array) ->
 let mm = float_of_matrix m in
  let d = float_det mm in
   Util.round d ;;

(** {v int_slow_det matrix v} *)
let int_slow_det = function (m:int array array) ->
 let mm = float_of_matrix m in
  let d = float_slow_det mm in
   Util.round d ;;

(** {v int_invertibility threshold matrix v} *)
let int_invertibility = fun (threshold:float) (m:int array array) ->
 let mm = float_of_matrix m in
  if float_invertibility mm then
   begin
    let d = float_det mm in
     if abs_float ( (abs_float d) -. 1. ) < threshold then true else false
   end
  else false ;;

(** {v int_slow_invertibility threshold matrix v} *)
let int_slow_invertibility = fun (threshold:float) (m:int array array) ->
 let mm = float_of_matrix m in
  if float_slow_invertibility mm then
   begin
    let d = float_slow_det mm in
     if abs_float ( (abs_float d) -. 1. ) < threshold then true else false
   end
  else false ;;

(** {v int_inv matrix v} *)
let int_inv = function (m:int array array) ->
 if int_invertibility 0.5 m then
  begin
   let mm = float_of_matrix m in
    let mmm = matrix_float_scal_left_div (matrix_float_norm_inf mm) mm in
     matrix_float_round ( matrix_float_scal_left_div (matrix_float_norm_inf mm) (float_inv mmm) )
  end
 else failwith "Inversion error in Matrix.int_inv." ;;

(** {v int_slow_inv matrix v} *)
let int_slow_inv = function (m:int array array) ->
 if int_slow_invertibility 0.5 m then
  begin
   let mm = float_of_matrix m in
    let mmm = matrix_float_scal_left_div (matrix_float_norm_inf mm) mm in
     matrix_float_round ( matrix_float_scal_left_div (matrix_float_norm_inf mm) (float_slow_inv mmm) )
  end
 else failwith "Inversion error in Matrix.int_slow_inv." ;;

(** {v vector_float_reciprocal vector v} *)
let vector_float_reciprocal = function (v:float array) ->
 vector_float_scal_left_div ( vector_float_square_norm_2 v ) v ;;

(** {v matrix_float_reciprocal vector v} *)
let matrix_float_reciprocal = function (m:float array array) ->
 matrix_float_scal_left_div ( matrix_float_square_frobenius m ) m ;;

(** {v matrix_float_trans_reciprocal vector v} *)
let matrix_float_trans_reciprocal = function (m:float array array) ->
 matrix_float_scal_left_div ( matrix_float_square_frobenius m ) ( float_transpose m ) ;;




(** {C § } *)
(** 
{1 Accélérateurs de convergence ou compensateurs d'erreurs}
{1 Convergence Accelerators or error compensators}
*)
(** {C  } *)
(**
{2 {C Suites dans [float]
---
Sequences of [float]} }
*)
(** {C  } *)




(** {v float_aitken_seki u(n) u(n+1) u(n+2) v} *)
let float_aitken_seki = fun (a:float) (b:float) (c:float) ->
 let d = b -. a
 and e = c -. b in
  let f = d *. e
  and g = d -. e in
   let h = f /. g in
    b +. h ;;


(** {v float_aitken_seki_rec k n value_array v} *)
let rec float_aitken_seki_rec = fun (k:int) (n:int) (s:float array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.float_aitken_seki_rec." ;
 if n < 0 then failwith "Negative index of sequence in Matrix.float_aitken_seki_rec." ;
 if Array.length s <= n + 2 * k then failwith "Too short sequence in Matrix.float_aitken_seki_rec." ;
 match k with
 | 0 -> s.(n)
 | 1 -> float_aitken_seki s.(n) s.( n + 1 ) s.( n + 2 )
 | _ ->
  let kk = pred k in
   let a = float_aitken_seki_rec kk n s
   and b = float_aitken_seki_rec kk ( n + 1 ) s
   and c = float_aitken_seki_rec kk ( n + 2 ) s in
    if b -. a == 0. || c -. b == 0. || a -. c == 0. then c
    else float_aitken_seki a b c ;;


(** {v float_shanks2 u(n) u(n+1) u(n+2) u(n+3) u(n+4) v} *)
let float_shanks2 = fun (a:float) (b:float) (c:float) (d:float) (e:float) ->
 let delta0 = b -. a
 and delta1 = c -. b
 and delta2 = d -. c
 and delta3 = e -. d in
  let dd0 = delta1 -. delta0
  and dd1 = delta2 -. delta1
  and dd2 = delta3 -. delta2 in
   let denom = float_slow_det [| [| dd0 ; dd1 |] ; [| dd1 ; dd2 |] |]
   and numer = float_slow_det [| [| a ; b ; c |] ; [| b ; c ; d |] ; [| c ; d ; e |] |] in
    numer /. denom ;;


(** {v float_wynn k n value_array v} *)
let rec float_wynn = fun (k:int) (n:int) (s:float array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.float_wynn." ;
 let km1 = pred k
 and km2 = k - 2
 and np = succ n in
  match k with
  | -1 -> 0.
  | 0 ->
   begin
    if n < 0 then failwith "Negative index of sequence in Matrix.float_wynn." ;
    if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.float_wynn." ;
    s.(n)
   end
  | _ ->
   begin
    let a = float_wynn km2 np s
    and b = float_wynn km1 np s
    and c = float_wynn km1 n s in
     let d = b -. c in
      if d == 0. then b
      else a +. 1. /. d 
   end ;;


(** {v float_wynn_rho k n value_array v} *)
let rec float_wynn_rho = fun (k:int) (n:int) (s:float array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.float_wynn_rho." ;
  let km1 = pred k
  and km2 = k - 2
  and np = succ n in
   match k with
   | -1 -> 0.
   | 0 ->
    begin
     if n < 0 then failwith "Negative index of sequence in Matrix.float_wynn_rho." ;
     if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.float_wynn_rho." ;
     s.(n)
    end
   | _ ->
    begin
     let a = float_wynn_rho km2 np s
     and b = float_wynn_rho km1 np s
     and c = float_wynn_rho km1 n s in
      let d = b -. c in
      if d == 0. then b
      else a +. ( float k ) /. d
    end ;;


(** {v float_brezinski k n value_array v} *)
let rec float_brezinski = fun (k:int) (n:int) (s:float array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.float_brezinski." ;
 match k with
 | -1 -> 0.
 | 0 ->
  begin
   if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.float_brezinski." ;
   if n < 0 then failwith "Negative index of sequence in Matrix.float_brezinski." ;
   s.(n)
  end
 | _ ->
  begin
   let km1 = pred k
   and km2 = k - 2
   and np = succ n in
    match k mod 2 with
    | 1 ->
     begin
      let a = float_brezinski km2 np s
      and b = float_brezinski km1 np s
      and c = float_brezinski km1 n s in
       let d = b -. c in
        if d == 0. then b
        else a +. 1. /. d
     end
    | _ ->
     begin
      let a = float_brezinski km2 np s
      and np2 = succ np in
       let b = float_brezinski km1 np2 s
       and bb = float_brezinski km1 np s
       and c = float_brezinski km2 np2 s
       and cc = float_brezinski km2 np s in
        let d = b -. bb
        and dd = c -. cc in
         let ee = d *. dd
         and eee = ( float_brezinski km1 n s ) +. ( float_brezinski km1 np2 s ) in
          let eeee = eee -. ( 2. *. ( float_brezinski km1 np s ) ) in
           if eeee == 0. then b
           else a +. ee /. eeee
     end
  end ;;


(** {v float_approx value_array v} *)
let float_approx = function (s:float array) ->
 let rr = pred ( Array.length s ) in
  let kk = rr / 2
  and parity = rr mod 2 in
   if parity == 0 then ( float_aitken_seki_rec kk 0 s )
   else ( float_aitken_seki_rec kk 1 s ) ;;




(** {C § } *)
(**
{2 {C Suites de vecteurs
---
Vector sequences} }
*)
(** {C  } *)




(** {v vector_float_aitken_seki u(n) u(n+1) u(n+2) v} *)
let vector_float_aitken_seki = fun (a:float array) (b:float array) (c:float array) ->
 let d = vector_float_minus b a
 and e = vector_float_minus c b in
  let f = vector_float_scal_prod d e
  and g = vector_float_minus d e in
   let h = vector_float_scal_mult f ( vector_float_reciprocal g ) in
    vector_float_plus b h ;;


(** {v vector_float_aitken_seki_rec k n value_array v} *)
let rec vector_float_aitken_seki_rec = fun (k:int) (n:int) (s:float array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.vector_float_aitken_seki_rec." ;
 if n < 0 then failwith "Negative index of sequence in Matrix.vector_float_aitken_seki_rec." ;
 if Array.length s <= n + 2 * k then failwith "Too short sequence in Matrix.vector_float_aitken_seki_rec." ;
 match k with
 | 0 -> s.(n)
 | 1 -> vector_float_aitken_seki s.(n) s.( n + 1 ) s.( n + 2 )
 | _ ->
  let kk = pred k in
   let a = vector_float_aitken_seki_rec kk n s
   and b = vector_float_aitken_seki_rec kk ( n + 1 ) s
   and c = vector_float_aitken_seki_rec kk ( n + 2 ) s in
    if vector_float_norm_inf ( vector_float_minus b a ) == 0. || vector_float_norm_inf ( vector_float_minus c b ) == 0. || vector_float_norm_inf ( vector_float_minus a c ) == 0. then c
    else vector_float_aitken_seki a b c ;;


(** {v vector_float_wynn k n value_array v} *)
let rec vector_float_wynn = fun (k:int) (n:int) (s:float array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.vector_float_wynn." ;
 let km1 = pred k
 and km2 = k - 2
 and np = succ n in
  match k with
  | -1 -> Array.make ( Array.length s.(0) ) 0.
  | 0 ->
   begin
    if n < 0 then failwith "Negative index of sequence in Matrix.vector_float_wynn." ;
    if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.vector_float_wynn." ;
    s.(n)
   end
  | _ ->
   begin
    let a = vector_float_wynn km2 np s
    and b = vector_float_wynn km1 np s
    and c = vector_float_wynn km1 n s in
     let d = vector_float_minus b c in
      if vector_float_norm_inf d == 0. then b
      else vector_float_plus a ( vector_float_reciprocal d )
   end ;;


(** {v vector_float_wynn_rho k n value_array v} *)
let rec vector_float_wynn_rho = fun (k:int) (n:int) (s:float array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.vector_float_wynn_rho." ;
  let km1 = pred k
  and km2 = k - 2
  and np = succ n in
   match k with
   | -1 -> Array.make ( Array.length s.(0) ) 0.
   | 0 ->
    begin
    if n < 0 then failwith "Negative index of sequence in Matrix.vector_float_wynn_rho." ;
    if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.vector_float_wynn_rho." ;
    s.(n)
    end
   | _ ->
    begin
     let a = vector_float_wynn_rho km2 np s
     and b = vector_float_wynn_rho km1 np s
     and c = vector_float_wynn_rho km1 n s in
      let d = vector_float_minus b c in
       if vector_float_norm_inf d == 0. then b
       else vector_float_plus a ( vector_float_scal_mult ( float k ) ( vector_float_reciprocal d ) )
    end ;;


(** {v vector_float_brezinski k n value_array v} *)
let rec vector_float_brezinski = fun (k:int) (n:int) (s:float array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.vector_float_brezinski." ;
 match k with
 | -1 -> Array.make ( Array.length s.(0) ) 0.
 | 0 ->
  begin
   if n < 0 then failwith "Negative index of sequence in Matrix.vector_float_brezinski." ;
   if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.vector_float_brezinski." ;
   s.(n)
  end
 | _ ->
  begin
   let km1 = pred k
   and km2 = k - 2
   and np = succ n in
    match k mod 2 with
    | 1 ->
     begin
      let a = vector_float_brezinski km2 np s
      and b = vector_float_brezinski km1 np s
      and c = vector_float_brezinski km1 n s in
       let d = vector_float_minus b c in
        if vector_float_norm_inf d == 0. then b
        else vector_float_plus a ( vector_float_reciprocal d )
     end
    | _ ->
     begin
      let a = vector_float_brezinski km2 np s
      and np2 = succ np in
       let b = vector_float_brezinski km1 np2 s
       and bb = vector_float_brezinski km1 np s
       and c = vector_float_brezinski km2 np2 s
       and cc = vector_float_brezinski km2 np s in
        let d = vector_float_minus b bb
        and dd = vector_float_minus c cc in
         let ee = vector_float_scal_prod d dd
         and eee = vector_float_plus ( vector_float_brezinski km1 n s ) ( vector_float_brezinski km1 np2 s ) in
          let eeee = vector_float_minus eee ( vector_float_scal_mult 2. ( vector_float_brezinski km1 np s ) ) in
           if vector_float_norm_inf eeee == 0. then b
           else vector_float_plus a ( vector_float_scal_mult ee ( vector_float_reciprocal eeee ) )
     end
  end ;;


(** {v vector_float_approx value_array v} *)
let vector_float_approx = function (s:float array array) ->
 let rr = pred ( Array.length s ) in
  let kk = rr / 2
  and parity = rr mod 2 in
   if parity == 0 then ( vector_float_aitken_seki_rec kk 0 s )
   else ( vector_float_aitken_seki_rec kk 1 s ) ;;


(** {v vector_float_approx_bis value_array v} *)
let vector_float_approx_bis = function (s:float array array) ->
 let rr = pred ( Array.length s ) in
  let kk = rr / 2
  and parity = rr mod 2 in
   if parity == 0 then ( vector_float_wynn ( 2 * kk ) 0 s )
   else ( vector_float_wynn ( 2 * kk ) 1 s ) ;;




(** {C § } *)
(**
{2 {C Suites de matrices
---
Matrix sequences} }
*)
(** {C  } *)




(** {v matrix_float_aitken_seki_bis u(n) u(n+1) u(n+2) v} *)
let matrix_float_aitken_seki_bis = fun (a:float array array) (b:float array array) (c:float array array) ->
 let d = matrix_float_minus b a
 and e = matrix_float_minus c b in
  let g = matrix_float_minus d e in
   let h = matrix_float_triple_prod d ( float_transpose e ) ( matrix_float_reciprocal g ) in
    matrix_float_plus b h ;;


(** {v matrix_float_aitken_seki u(n) u(n+1) u(n+2) v} *)
let matrix_float_aitken_seki = fun (a:float array array) (b:float array array) (c:float array array) ->
 let d = matrix_float_minus b a
 and e = matrix_float_minus c b in
  let f = matrix_float_frobenius_prod d e 
  and g = matrix_float_minus d e in
   let h = matrix_float_scal_mult f ( matrix_float_reciprocal g ) in
    matrix_float_plus b ( float_transpose h ) ;;


(** {v matrix_trans_float_aitken_seki_bis u(n) u(n+1) u(n+2) v} *)
let matrix_trans_float_aitken_seki_bis = fun (a:float array array) (b:float array array) (c:float array array) ->
 let d = matrix_float_minus b a
 and e = matrix_float_minus c b in
  let g = matrix_float_minus d e in
   let h = matrix_float_triple_prod d ( float_transpose e ) ( matrix_float_trans_reciprocal g ) in
    matrix_float_plus b h ;;


(** {v matrix_trans_float_aitken_seki u(n) u(n+1) u(n+2) v} *)
let matrix_trans_float_aitken_seki = fun (a:float array array) (b:float array array) (c:float array array) ->
 let d = matrix_float_minus b a
 and e = matrix_float_minus c b in
  let f = matrix_float_frobenius_prod d e 
  and g = matrix_float_minus d e in
   let h = matrix_float_scal_mult f ( matrix_float_trans_reciprocal g ) in
    matrix_float_plus b ( float_transpose h ) ;;


(** {v matrix_float_aitken_seki_rec_bis k n value_array v} *)
let rec matrix_float_aitken_seki_rec_bis = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_float_aitken_seki_rec_bis." ;
 if n < 0 then failwith "Negative index of sequence in Matrix.matrix_float_aitken_seki_rec_bis." ;
 if Array.length s <= n + 2 * k then failwith "Too short sequence in Matrix.matrix_float_aitken_seki_rec_bis." ;
 match k with
 | 0 -> s.(n)
 | 1 -> matrix_float_aitken_seki_bis s.(n) s.( n + 1 ) s.( n + 2 )
 | _ ->
  let kk = pred k in
   let a = matrix_float_aitken_seki_rec_bis kk n s
   and b = matrix_float_aitken_seki_rec_bis kk ( n + 1 ) s
   and c = matrix_float_aitken_seki_rec_bis kk ( n + 2 ) s in
    if matrix_float_norm_inf ( matrix_float_minus b a ) == 0. || matrix_float_norm_inf ( matrix_float_minus c b ) == 0. || matrix_float_norm_inf ( matrix_float_minus a c ) == 0. then c
    else matrix_float_aitken_seki_bis a b c ;;


(** {v matrix_float_aitken_seki_rec k n value_array v} *)
let rec matrix_float_aitken_seki_rec = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_float_aitken_seki_rec." ;
 if n < 0 then failwith "Negative index of sequence in Matrix.matrix_float_aitken_seki_rec." ;
 if Array.length s <= n + 2 * k then failwith "Too short sequence in Matrix.matrix_float_aitken_seki_rec." ;
 match k with
 | 0 -> s.(n)
 | 1 -> matrix_float_aitken_seki s.(n) s.( n + 1 ) s.( n + 2 )
 | _ ->
  let kk = pred k in
   let a = matrix_float_aitken_seki_rec kk n s
   and b = matrix_float_aitken_seki_rec kk ( n + 1 ) s
   and c = matrix_float_aitken_seki_rec kk ( n + 2 ) s in
    if matrix_float_norm_inf ( matrix_float_minus b a ) == 0. || matrix_float_norm_inf ( matrix_float_minus c b ) == 0. || matrix_float_norm_inf ( matrix_float_minus a c ) == 0. then c
    else matrix_float_aitken_seki a b c ;;


(** {v matrix_trans_float_aitken_seki_rec_bis k n value_array v} *)
let rec matrix_trans_float_aitken_seki_rec_bis = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_trans_float_aitken_seki_rec_bis." ;
 if n < 0 then failwith "Negative index of sequence in Matrix.matrix_trans_float_aitken_seki_rec_bis." ;
 if Array.length s <= n + 2 * k then failwith "Too short sequence in Matrix.matrix_trans_float_aitken_seki_rec_bis." ;
 match k with
 | 0 -> s.(n)
 | 1 -> matrix_trans_float_aitken_seki_bis s.(n) s.( n + 1 ) s.( n + 2 )
 | _ ->
  let kk = pred k in
   let a = matrix_trans_float_aitken_seki_rec_bis kk n s
   and b = matrix_trans_float_aitken_seki_rec_bis kk ( n + 1 ) s
   and c = matrix_trans_float_aitken_seki_rec_bis kk ( n + 2 ) s in
    if matrix_float_norm_inf ( matrix_float_minus b a ) == 0. || matrix_float_norm_inf ( matrix_float_minus c b ) == 0. || matrix_float_norm_inf ( matrix_float_minus a c ) == 0. then c
    else matrix_trans_float_aitken_seki_bis a b c ;;


(** {v matrix_trans_float_aitken_seki_rec k n value_array v} *)
let rec matrix_trans_float_aitken_seki_rec = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_trans_float_aitken_seki_rec." ;
 if n < 0 then failwith "Negative index of sequence in Matrix.matrix_trans_float_aitken_seki_rec." ;
 if Array.length s <= n + 2 * k then failwith "Too short sequence in Matrix.matrix_trans_float_aitken_seki_rec." ;
 match k with
 | 0 -> s.(n)
 | 1 -> matrix_trans_float_aitken_seki s.(n) s.( n + 1 ) s.( n + 2 )
 | _ ->
  let kk = pred k in
   let a = matrix_trans_float_aitken_seki_rec kk n s
   and b = matrix_trans_float_aitken_seki_rec kk ( n + 1 ) s
   and c = matrix_trans_float_aitken_seki_rec kk ( n + 2 ) s in
    if matrix_float_norm_inf ( matrix_float_minus b a ) == 0. || matrix_float_norm_inf ( matrix_float_minus c b ) == 0. || matrix_float_norm_inf ( matrix_float_minus a c ) == 0. then c
    else matrix_trans_float_aitken_seki a b c ;;


(** {v matrix_float_wynn k n value_array v} *)
let rec matrix_float_wynn = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_float_wynn." ;
 let km1 = pred k
 and km2 = k - 2
 and np = succ n in
  match k with
  | -1 -> Array.make_matrix ( Array.length s.(0) ) ( Array.length s.(0).(0) ) 0.
  | 0 ->
   begin
    if n < 0 then failwith "Negative index of sequence in Matrix.matrix_float_wynn." ;
    if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.matrix_float_wynn." ;
    s.(n)
   end
  | _ ->
   begin
    let a = matrix_float_wynn km2 np s
    and b = matrix_float_wynn km1 np s
    and c = matrix_float_wynn km1 n s in
     let d = matrix_float_minus b c in
      if matrix_float_norm_inf d == 0. then b
      else matrix_float_plus a ( matrix_float_reciprocal d )
   end ;;


(** {v matrix_trans_float_wynn k n value_array v} *)
let rec matrix_trans_float_wynn = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_trans_float_wynn." ;
 let km1 = pred k
 and km2 = k - 2
 and np = succ n in
  match k with
  | -1 -> Array.make_matrix ( Array.length s.(0) ) ( Array.length s.(0).(0) ) 0.
  | 0 ->
   begin
    if n < 0 then failwith "Negative index of sequence in Matrix.matrix_trans_float_wynn." ;
    if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.matrix_trans_float_wynn." ;
    s.(n)
   end
  | _ ->
   begin
    let a = matrix_trans_float_wynn km2 np s
    and b = matrix_trans_float_wynn km1 np s
    and c = matrix_trans_float_wynn km1 n s in
     let d = matrix_float_minus b c in
      if matrix_float_norm_inf d == 0. then b
      else matrix_float_plus a ( matrix_float_trans_reciprocal d )
   end ;;


(** {v matrix_float_wynn_rho k n value_array v} *)
let rec matrix_float_wynn_rho = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_float_wynn_rho." ;
  let km1 = pred k
  and km2 = k - 2
  and np = succ n in
   match k with
   | -1 -> Array.make_matrix ( Array.length s.(0) ) ( Array.length s.(0).(0) ) 0.
   | 0 ->
    begin
     if n < 0 then failwith "Negative index of sequence in Matrix.matrix_float_wynn_rho." ;
     if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.matrix_float_wynn_rho." ;
     s.(n)
    end
   | _ ->
    begin
     let a = matrix_float_wynn_rho km2 np s
     and b = matrix_float_wynn_rho km1 np s
     and c = matrix_float_wynn_rho km1 n s in
      let d = matrix_float_minus b c in
      if matrix_float_norm_inf d == 0. then b
      else matrix_float_plus a ( matrix_float_scal_mult ( float k ) ( matrix_float_reciprocal d ) )
    end ;;


(** {v matrix_trans_float_wynn_rho k n value_array v} *)
let rec matrix_trans_float_wynn_rho = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_trans_float_wynn_rho." ;
  let km1 = pred k
  and km2 = k - 2
  and np = succ n in
   match k with
   | -1 -> Array.make_matrix ( Array.length s.(0) ) ( Array.length s.(0).(0) ) 0.
   | 0 ->
    begin
     if n < 0 then failwith "Negative index of sequence in Matrix.matrix_trans_float_wynn_rho." ;
     if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.matrix_trans_float_wynn_rho." ;
     s.(n)
    end
   | _ ->
    begin
     let a = matrix_trans_float_wynn_rho km2 np s
     and b = matrix_trans_float_wynn_rho km1 np s
     and c = matrix_trans_float_wynn_rho km1 n s in
      let d = matrix_float_minus b c in
      if matrix_float_norm_inf d == 0. then b
      else matrix_float_plus a ( matrix_float_scal_mult ( float k ) ( matrix_float_trans_reciprocal d ) )
    end ;;


(** {v matrix_float_brezinski_bis k n value_array v} *)
let rec matrix_float_brezinski_bis = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_float_brezinski_bis." ;
 match k with
 | -1 -> Array.make_matrix ( Array.length s.(0) ) ( Array.length s.(0).(0) ) 0.
 | 0 ->
  begin
   if n < 0 then failwith "Negative index of sequence in Matrix.matrix_float_brezinski_bis." ;
   if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.matrix_float_brezinski_bis." ;
   s.(n)
  end
 | _ ->
  begin
   let km1 = pred k
   and km2 = k - 2
   and np = succ n in
    match k mod 2 with
    | 1 ->
     begin
      let a = matrix_float_brezinski_bis km2 np s
      and b = matrix_float_brezinski_bis km1 np s
      and c = matrix_float_brezinski_bis km1 n s in
       let d = matrix_float_minus b c in
        if matrix_float_norm_inf d == 0. then b
        else matrix_float_plus a ( matrix_float_reciprocal d )
     end
    | _ ->
     begin
      let a = matrix_float_brezinski_bis km2 np s
      and np2 = succ np in
       let b = matrix_float_brezinski_bis km1 np2 s
       and bb = matrix_float_brezinski_bis km1 np s
       and c = matrix_float_brezinski_bis km2 np2 s
       and cc = matrix_float_brezinski_bis km2 np s in
        let d = matrix_float_minus b bb
        and dd = matrix_float_minus c cc in
         let eee = matrix_float_plus ( matrix_float_brezinski_bis km1 n s ) ( matrix_float_brezinski_bis km1 np2 s ) in
          let eeee = matrix_float_minus eee ( matrix_float_scal_mult 2. ( matrix_float_brezinski_bis km1 np s ) ) in
           if matrix_float_norm_inf eeee == 0. then b
           else matrix_float_plus a ( matrix_float_triple_prod ( float_transpose d ) dd ( matrix_float_reciprocal eeee ) )
     end
  end ;;


(** {v matrix_trans_float_brezinski_bis k n value_array v} *)
let rec matrix_trans_float_brezinski_bis = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_trans_float_brezinski_bis." ;
 match k with
 | -1 -> Array.make_matrix ( Array.length s.(0) ) ( Array.length s.(0).(0) ) 0.
 | 0 ->
  begin
   if n < 0 then failwith "Negative index of sequence in Matrix.matrix_trans_float_brezinski_bis." ;
   if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.matrix_trans_float_brezinski_bis." ;
   s.(n)
  end
 | _ ->
  begin
   let km1 = pred k
   and km2 = k - 2
   and np = succ n in
    match k mod 2 with
    | 1 ->
     begin
      let a = matrix_trans_float_brezinski_bis km2 np s
      and b = matrix_trans_float_brezinski_bis km1 np s
      and c = matrix_trans_float_brezinski_bis km1 n s in
       let d = matrix_float_minus b c in
        if matrix_float_norm_inf d == 0. then b
        else matrix_float_plus a ( matrix_float_trans_reciprocal d )
     end
    | _ ->
     begin
      let a = matrix_trans_float_brezinski_bis km2 np s
      and np2 = succ np in
       let b = matrix_trans_float_brezinski_bis km1 np2 s
       and bb = matrix_trans_float_brezinski_bis km1 np s
       and c = matrix_trans_float_brezinski_bis km2 np2 s
       and cc = matrix_trans_float_brezinski_bis km2 np s in
        let d = matrix_float_minus b bb
        and dd = matrix_float_minus c cc in
         let eee = matrix_float_plus ( matrix_trans_float_brezinski_bis km1 n s ) ( matrix_trans_float_brezinski_bis km1 np2 s ) in
          let eeee = matrix_float_minus eee ( matrix_float_scal_mult 2. ( matrix_trans_float_brezinski_bis km1 np s ) ) in
           if matrix_float_norm_inf eeee == 0. then b
           else matrix_float_plus a ( matrix_float_triple_prod ( float_transpose d ) dd ( matrix_float_trans_reciprocal eeee ) )
     end
  end ;;


(** {v matrix_float_brezinski k n value_array v} *)
let rec matrix_float_brezinski = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_float_brezinski." ;
 match k with
 | -1 -> Array.make_matrix ( Array.length s.(0) ) ( Array.length s.(0).(0) ) 0.
 | 0 ->
  begin
   if n < 0 then failwith "Negative index of sequence in Matrix.matrix_float_brezinski." ;
   if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.matrix_float_brezinski." ;
   s.(n)
  end
 | _ ->
  begin
   let km1 = pred k
   and km2 = k - 2
   and np = succ n in
    match k mod 2 with
    | 1 ->
     begin
      let a = matrix_float_brezinski km2 np s
      and b = matrix_float_brezinski km1 np s
      and c = matrix_float_brezinski km1 n s in
       let d = matrix_float_minus b c in
        if matrix_float_norm_inf d == 0. then b
        else matrix_float_plus a ( matrix_float_reciprocal d )
     end
    | _ ->
     begin
      let a = matrix_float_brezinski km2 np s
      and np2 = succ np in
       let b = matrix_float_brezinski km1 np2 s
       and bb = matrix_float_brezinski km1 np s
       and c = matrix_float_brezinski km2 np2 s
       and cc = matrix_float_brezinski km2 np s in
        let d = matrix_float_minus b bb
        and dd = matrix_float_minus c cc in
         let ee = matrix_float_frobenius_prod d dd
         and eee = matrix_float_plus ( matrix_float_brezinski km1 n s ) ( matrix_float_brezinski km1 np2 s ) in
          let eeee = matrix_float_minus eee ( matrix_float_scal_mult 2. ( matrix_float_brezinski km1 np s ) ) in
           if matrix_float_norm_inf eeee == 0. then b
           else matrix_float_plus a ( matrix_float_scal_mult ee ( matrix_float_reciprocal eeee ) )
     end
  end ;;


(** {v matrix_trans_float_brezinski k n value_array v} *)
let rec matrix_trans_float_brezinski = fun (k:int) (n:int) (s:float array array array) ->
 if k < -1 then failwith "Needed k >= -1 in Matrix.matrix_trans_float_brezinski." ;
 match k with
 | -1 -> Array.make_matrix ( Array.length s.(0) ) ( Array.length s.(0).(0) ) 0.
 | 0 ->
  begin
   if n < 0 then failwith "Negative index of sequence in Matrix.matrix_trans_float_brezinski." ;
   if n > pred ( Array.length s ) then failwith "Too short sequence in Matrix.matrix_trans_float_brezinski." ;
   s.(n)
  end
 | _ ->
  begin
   let km1 = pred k
   and km2 = k - 2
   and np = succ n in
    match k mod 2 with
    | 1 ->
     begin
      let a = matrix_trans_float_brezinski km2 np s
      and b = matrix_trans_float_brezinski km1 np s
      and c = matrix_trans_float_brezinski km1 n s in
       let d = matrix_float_minus b c in
        if matrix_float_norm_inf d == 0. then b
        else matrix_float_plus a ( matrix_float_trans_reciprocal d )
     end
    | _ ->
     begin
      let a = matrix_trans_float_brezinski km2 np s
      and np2 = succ np in
       let b = matrix_trans_float_brezinski km1 np2 s
       and bb = matrix_trans_float_brezinski km1 np s
       and c = matrix_trans_float_brezinski km2 np2 s
       and cc = matrix_trans_float_brezinski km2 np s in
        let d = matrix_float_minus b bb
        and dd = matrix_float_minus c cc in
         let ee = matrix_float_frobenius_prod d dd
         and eee = matrix_float_plus ( matrix_trans_float_brezinski km1 n s ) ( matrix_trans_float_brezinski km1 np2 s ) in
          let eeee = matrix_float_minus eee ( matrix_float_scal_mult 2. ( matrix_trans_float_brezinski km1 np s ) ) in
           if matrix_float_norm_inf eeee == 0. then b
           else matrix_float_plus a ( matrix_float_scal_mult ee ( matrix_float_trans_reciprocal eeee ) )
     end
  end ;;


(** {v matrix_float_approx_bis value_array v} *)
let matrix_float_approx_bis = function (s:float array array array) ->
 let rr = pred ( Array.length s ) in
  let kk = rr / 2
  and parity = rr mod 2 in
   if parity == 0 then ( matrix_float_wynn kk 0 s )
   else ( matrix_float_wynn kk 1 s ) ;;


(** {v matrix_float_approx value_array v} *)
let matrix_float_approx = function (s:float array array array) ->
 let rr = pred ( Array.length s ) in
  let kk = rr / 2
  and parity = rr mod 2 in
   if parity == 0 then ( matrix_float_aitken_seki_rec kk 0 s )
   else ( matrix_float_aitken_seki_rec kk 1 s ) ;;


(** {v matrix_trans_float_approx_bis value_array v} *)
let matrix_trans_float_approx_bis = function (s:float array array array) ->
 let rr = pred ( Array.length s ) in
  let kk = rr / 2
  and parity = rr mod 2 in
   if parity == 0 then ( matrix_trans_float_wynn kk 0 s )
   else ( matrix_trans_float_wynn kk 1 s ) ;;


(** {v matrix_trans_float_approx value_array v} *)
let matrix_trans_float_approx = function (s:float array array array) ->
 let rr = pred ( Array.length s ) in
  let kk = rr / 2
  and parity = rr mod 2 in
   if parity == 0 then ( matrix_trans_float_aitken_seki_rec kk 0 s )
   else ( matrix_trans_float_aitken_seki_rec kk 1 s ) ;;




(** {C § } *)
(**
{1 Calcul sur les matrices par blocs}
{1 Calculus on block matrices}
*)
(** {C  } *)
(**
{2 {C Définitions et échanges
---
Definitions and exchanges} }
*)
(** {C  } *)




(** The [float_or_array] recursive type is used to collect real numbers, real vectors, real matrices, and the corresponding block matrices.

Le type récursif [float_or_array] sert à réunir nombres réels, vecteurs réels et matrices réelles avec les matrices par blocs correspondantes. *)
type float_or_array = 
   Empty
 | Float_cons of float 
 | Float_vector_cons of float array
 | Float_matrix_cons of float array array
 | Foa_vector_cons of float_or_array array
 | Foa_matrix_cons of float_or_array array array ;;


(** {v vector_foa_copy vector v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_copy = function (v:float_or_array) ->
 match v with
 | Float_cons x -> Float_cons ( 0. +. x )
 | Float_vector_cons x ->
  let vv = vector_float_copy x in
   Float_vector_cons vv
 | Foa_vector_cons x ->
  let r = Array.length x in
   let vvv = Array.make r (Float_cons 0.) in
    for i = 0 to r - 1 do
     vvv.(i) <- vector_foa_copy x.(i)
    done ;
    Foa_vector_cons vvv
 | _ -> Float_cons 0. ;;


(** {v vector_foa_eq vector1 vector2 v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_eq = fun (v:float_or_array) (w:float_or_array) ->
 match v with
 | Float_cons x ->
  begin
   match w with
   | Float_cons y -> x = y
   | _ -> false
  end
 | Float_vector_cons x ->
  begin
   match w with
   | Float_vector_cons y -> Util.array_eq ( = ) x y
   | _ -> false
  end
 | Foa_vector_cons x ->
  begin
   match w with
   | Foa_vector_cons y -> Util.array_eq vector_foa_eq x y
   | _ -> false
  end
 | _ -> false ;;


(** {v matrix_foa_copy matrix v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec matrix_foa_copy = function (m:float_or_array) ->
 match m with
 | Float_cons y -> Float_cons ( 0. +. y )
 | Float_matrix_cons y -> Float_matrix_cons ( matrix_float_copy y)
 | Foa_matrix_cons y ->
  let r = numrows y
  and cc = (numcolumns y) - 1 in
   let m_m = Array.make_matrix r (cc + 1) (Float_cons 0.) in
    for i = 0 to r - 1 do
     for j = 0 to cc do
      m_m.(i).(j) <- matrix_foa_copy y.(i).(j)
     done
    done ;
    Foa_matrix_cons m_m
 | _ -> Float_cons 0. ;;


(** {v float_demakeup coefficient v} *)
let float_demakeup = function (x:float_or_array) ->
 match x with
 | (Float_cons y) -> 0. +. y 
 | _ -> failwith "Bad type Float_cons in Matrix.float_demakeup." ;;

(** {v vector_float_demakeup vector v} *)
let vector_float_demakeup = function (x:float_or_array) ->
 match x with
 | (Float_vector_cons y) -> vector_float_copy y
 | _ -> failwith "Bad type Float_vector_cons in Matrix.vector_float_demakeup." ;;

(** {v matrix_float_demakeup matrix v} *)
let matrix_float_demakeup = function (x:float_or_array) ->
 match x with
 | (Float_matrix_cons y) -> matrix_float_copy y
 | _ -> failwith "Bad type Float_matrix_cons in Matrix.matrix_float_demakeup." ;;

(** {v vector_foa_demakeup vector v} *)
let vector_foa_demakeup = function (x:float_or_array) ->
 match x with
 | (Foa_vector_cons y) ->
  begin
   let r = Array.length y in
    let vvv = Array.make r (Float_cons 0.) in
     for i = 0 to r - 1 do
      vvv.(i) <- vector_foa_copy y.(i)
     done ;
     vvv
  end
 | _ -> failwith "Bad type Foa_vector_cons in Matrix.vector_foa_demakeup." ;;

(** {v matrix_foa_demakeup matrix v} *)
let matrix_foa_demakeup = function (x:float_or_array) ->
 match x with
 | (Foa_matrix_cons y) ->
  begin
   let r = numrows y
   and cc = (numcolumns y) - 1 in
    let m_m = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      for j = 0 to cc do
       m_m.(i).(j) <- matrix_foa_copy y.(i).(j)
      done
     done ;
     m_m
  end
 | _ -> failwith "Bad type Foa_matrix_cons in Matrix.matrix_foa_demakeup." ;;

(** {v foa_thickness coefficient-or-vector-or-matrix v} *)
let rec foa_thickness = function (x:float_or_array) ->
 match x with
 | (Float_cons y) -> 0
 | (Float_vector_cons y) -> 0
 | (Float_matrix_cons y) -> 0 
 | (Foa_vector_cons y) ->
  begin
   let accu = ref 0 in
    for i = 0 to (Array.length y) - 1 do
     let x = foa_thickness y.(i) in
      if x > !accu then accu := x
    done ;
    1 + !accu
  end
 | (Foa_matrix_cons y) ->
  begin
   let accu = ref 0 in
    for i = 0 to (Array.length y) - 1 do
     let row = y.(i) in
      for j = 0 to (Array.length row) - 1 do
       let x = foa_thickness row.(j) in
        if x > !accu then accu := x
      done
    done ;
    1 + !accu
  end
 | _ -> 0 ;;


(** The [int_or_array] recursive type is used to collect integer numbers, integer vectors, integer matrices, and the corresponding block matrices.

Le type récursif [int_or_array] sert à réunir nombres entiers, vecteurs entiers et matrices entières avec les matrices par blocs correspondantes. *)
type int_or_array = 
   Empty
 | Int_cons of int 
 | Int_vector_cons of int array
 | Int_matrix_cons of int array array
 | Ioa_vector_cons of int_or_array array
 | Ioa_matrix_cons of int_or_array array array  ;;


(** {v vector_ioa_copy vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_copy = function (v:int_or_array) ->
 match v with
 | Int_cons x -> Int_cons ( 0 + x )
 | Int_vector_cons x ->
  let vv = vector_int_copy x in
   Int_vector_cons vv
 | Ioa_vector_cons x ->
  let r = Array.length x in
   let vvv = Array.make r (Int_cons 0) in
    for i = 0 to r - 1 do
     vvv.(i) <- vector_ioa_copy x.(i)
    done ;
    Ioa_vector_cons vvv
 | _ -> Int_cons 0 ;;


(** {v matrix_ioa_copy matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_copy = function (m:int_or_array) ->
 match m with
 | Int_cons y -> Int_cons ( 0 + y )
 | Int_matrix_cons y -> Int_matrix_cons ( matrix_int_copy y)
 | Ioa_matrix_cons y ->
  let r = numrows y
  and cc = (numcolumns y) - 1 in
   let m_m = Array.make_matrix r (cc + 1) (Int_cons 0) in
    for i = 0 to r - 1 do
     for j = 0 to cc do
      m_m.(i).(j) <- matrix_ioa_copy y.(i).(j)
     done
    done ;
    Ioa_matrix_cons m_m
 | _ -> Int_cons 0 ;;

(** {v int_demakeup coefficient v} *)
let int_demakeup = function (x:int_or_array) ->
 match x with
 | (Int_cons y) -> 0 + y 
 | _ -> failwith "Bad type Int_cons in Matrix.int_demakeup." ;;

(** {v vector_int_demakeup vector v} *)
let vector_int_demakeup = function (x:int_or_array) ->
 match x with
 | (Int_vector_cons y) -> vector_int_copy y
 | _ -> failwith "Bad type Int_vector_cons in Matrix.vector_int_demakeup." ;;

(** {v matrix_int_demakeup matrix v} *)
let matrix_int_demakeup = function (x:int_or_array) ->
 match x with
 | (Int_matrix_cons y) -> matrix_int_copy y
 | _ -> failwith "Bad type Int_matrix_cons in Matrix.matrix_int_demakeup." ;;

(** {v vector_ioa_demakeup vector v} *)
let vector_ioa_demakeup = function (x:int_or_array) ->
 match x with
 | (Ioa_vector_cons y) ->
  begin
   let r = Array.length y in
    let vvv = Array.make r (Int_cons 0) in
     for i = 0 to r - 1 do
      vvv.(i) <- vector_ioa_copy y.(i)
     done ;
     vvv
  end
 | _ -> failwith "Bad type Ioa_vector_cons in Matrix.vector_ioa_demakeup." ;;

(** {v matrix_ioa_demakeup matrix v} *)
let matrix_ioa_demakeup = function (x:int_or_array) ->
 match x with
 | (Ioa_matrix_cons y) ->
  begin
   let r = numrows y
   and cc = (numcolumns y) - 1 in
    let m_m = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      for j = 0 to cc do
       m_m.(i).(j) <- matrix_ioa_copy y.(i).(j)
      done
     done ;
     m_m
  end
 | _ -> failwith "Bad type Ioa_matrix_cons in Matrix.matrix_ioa_demakeup." ;;


(** {v ioa_thickness coefficient-or-vector-or-matrix v} *)
let rec ioa_thickness = function (x:int_or_array) ->
 match x with
 | (Int_cons y) -> 0
 | (Int_vector_cons y) -> 0
 | (Int_matrix_cons y) -> 0 
 | (Ioa_vector_cons y) ->
  begin
   let accu = ref 0 in
    for i = 0 to (Array.length y) - 1 do
     let x = ioa_thickness y.(i) in
      if x > !accu then accu := x
    done ;
    1 + !accu
  end
 | (Ioa_matrix_cons y) ->
  begin
   let accu = ref 0 in
    for i = 0 to (Array.length y) - 1 do
     let row = y.(i) in
      for j = 0 to (Array.length row) - 1 do
       let x = ioa_thickness row.(j) in
        if x > !accu then accu := x
      done
    done ;
    1 + !accu
  end
 | _ -> 0 ;;


(** {v foa_of_ioa integer_or_array v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec foa_of_ioa = function (x:int_or_array) ->
 match x with
 | Int_cons y -> Float_cons (float_of_int y)
 | Int_vector_cons v -> Float_vector_cons ( float_of_vector v ) 
 | Ioa_vector_cons z ->
  begin
   let r = Array.length z in
    let ww = Array.make r (Float_cons 0.) in
     for i = 0 to r - 1 do
      ww.(i) <- foa_of_ioa z.(i)
     done ;
     Foa_vector_cons ww
  end
 | Int_matrix_cons m -> Float_matrix_cons ( float_of_matrix m ) 
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = Array.length w.(0) - 1 in
    let ww = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      let row_input = w.(i)
      and row_output = ww.(i) in
       for j = 0 to cc do
        row_output.(j) <- foa_of_ioa ( row_input.(j) )
       done
     done ;
     Foa_matrix_cons ww
  end
 | _ -> failwith "Not an integer_or_array in Matrix.foa_of_ioa." ;;


(** {v ioa_of_foa float_or_array v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec ioa_of_foa = function (x:float_or_array) ->
 match x with
 | Float_cons y -> Int_cons (int_of_float y)
 | Float_vector_cons v -> Int_vector_cons ( int_of_vector v ) 
 | Foa_vector_cons z ->
  begin
   let r = Array.length z in
    let ww = Array.make r (Int_cons 0) in
     for i = 0 to r - 1 do
      ww.(i) <- ioa_of_foa z.(i)
     done ;
     Ioa_vector_cons ww
  end
 | Float_matrix_cons m -> Int_matrix_cons ( int_of_matrix m )
 | Foa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = Array.length w.(0) - 1 in
    let ww = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_input = w.(i)
      and row_output = ww.(i) in
       for j = 0 to cc do
        row_output.(j) <- ioa_of_foa ( row_input.(j) )
       done
     done ;
     Ioa_matrix_cons ww
  end
 | _ -> failwith "Not a float_or_array in Matrix.ioa_of_foa." ;;


(** {v foa_apply float_or_array v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec foa_apply = fun (f:float -> float) (x:float_or_array) ->
match x with
 | Float_cons y -> Float_cons (f y)
 | Float_vector_cons v -> Float_vector_cons ( Array.map f v ) 
 | Foa_vector_cons z ->
  begin
   let r = Array.length z in
    let ww = Array.make r (Float_cons 0.) in
     for i = 0 to r - 1 do
      ww.(i) <- foa_apply f z.(i)
     done ;
     Foa_vector_cons ww
  end
 | Float_matrix_cons m -> Float_matrix_cons ( matrix_float_apply f m )
 | Foa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = Array.length w.(0) - 1 in
    let ww = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      let row_input = w.(i)
      and row_output = ww.(i) in
       for j = 0 to cc do
        row_output.(j) <- foa_apply f ( row_input.(j) )
       done
     done ;
     Foa_matrix_cons ww
  end
 | _ -> failwith "Not a float_or_array in Matrix.foa_apply." ;;


(** The type [foa_strip] is used for diagonal multiplication and Gauss pivot.

Le type [foa_strip] sert pour la multiplication diagonale et le pivot de Gauss. *)
type foa_strip = 
   Empty
 | Foa_cons of float_or_array
 | Foa_strip_cons of float_or_array array ;;

(** {v foa_strip_demakeup block-element v} *)
let foa_strip_demakeup = function (x:foa_strip) ->
 match x with
 | (Foa_cons y) -> let z = ref y in !z
 | _ -> failwith "Bad type Foa_cons in Matrix.foa_strip_demakeup." ;;

(** {v vector_foa_strip_demakeup block-vector v} *)
let vector_foa_strip_demakeup = function (x:foa_strip) ->
 match x with
 | (Foa_strip_cons y) -> let z = ref y in !z
 | _ -> failwith "Bad type Foa_strip_cons in Matrix.vector_foa_strip_demakeup." ;;


(** The type [ioa_strip] is used for diagonal multiplication.

Le type [ioa_strip] sert pour la multiplication diagonale. *)
type ioa_strip = 
   Empty
 | Ioa_cons of int_or_array
 | Ioa_strip_cons of int_or_array array ;;

(** {v ioa_strip_demakeup block-element v} *)
let ioa_strip_demakeup = function (x:ioa_strip) ->
 match x with
 | (Ioa_cons y) -> let z = ref y in !z
 | _ -> failwith "Bad type Ioa_cons in Matrix.ioa_strip_demakeup." ;;

(** {v vector_ioa_strip_demakeup block-vector v} *)
let vector_ioa_strip_demakeup = function (x:ioa_strip) ->
 match x with
 | (Ioa_strip_cons y) -> let z = ref y in !z
 | _ -> failwith "Bad type Ioa_strip_cons in Matrix.vector_ioa_strip_demakeup." ;;




(** {C § } *)
(**
{2 {C Constructions minimales pour les matrices par blocs
---
Minimal constructions for block matrices} }
*)
(** {C  } *)
(**
{9  ++++++ Pasting and cutting, numbers of rows and columns.

Collage et découpage, nombres de lignes et de colonnes. ++++++ }
*)
(** {C  } *)




(** {v vector_foa_cut parts vector v} *)
let vector_foa_cut = fun n (v:float_or_array) ->
 if n <= 1 then v else
  match v with
   | Float_vector_cons y  ->
     let r = Array.length y in
      if r < n then failwith "Too short in Matrix.vector_foa_cut."
      else
       let k = r / n in
        let vv = Array.make n (Float_cons 0.) in
         for i = 0 to n-2 do 
          vv.(i) <- Float_vector_cons ( Array.sub y (k*i) (k) )
         done ;
         vv.(n - 1) <- Float_vector_cons ( Array.sub y (k*(n - 1)) (r-k*(n - 1)) ) ;
         Foa_vector_cons vv 
   | Foa_vector_cons w ->
     let r = Array.length w in
      if r < n then failwith "Too short in Matrix.vector_foa_cut."
      else
       let k = r / n in
        let vv = Array.make n (Float_cons 0.) in
         for i = 0 to n-2 do 
          vv.(i) <- Foa_vector_cons ( Array.sub w (k*i) (k) )
         done ;
         vv.(n - 1) <- Foa_vector_cons ( Array.sub w (k*(n - 1)) (r-k*(n - 1)) ) ;
         Foa_vector_cons vv
   | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_cut." ;;

(** {v vector_foa_paste vector v} *)
let vector_foa_paste = function (v:float_or_array) ->
 match (foa_thickness v) with
  | 0 ->
   v
  | 1 ->
   let vv = vector_foa_demakeup v in
    let vvv = Array.map vector_float_demakeup vv in
     let vvvv = Array.to_list vvv in
      Float_vector_cons (Array.concat vvvv) 
  | _ ->
   let vv = vector_foa_demakeup v in
    let vvv = Array.map vector_foa_demakeup vv in
     let vvvv = Array.to_list vvv in
      Foa_vector_cons (Array.concat vvvv) ;;


(** {v vector_ioa_cut parts vector v} *)
let vector_ioa_cut = fun n (v:int_or_array) ->
 if n <= 1 then v else
  match (ioa_thickness v) with
   | 0 ->
    let w = vector_int_demakeup v in
     let r = Array.length w in
      if r < n then failwith "Too short in Matrix.vector_ioa_cut."
      else
       let k = r / n in
        let vv = Array.make n (Int_cons 0) in
         for i = 0 to n-2 do 
          vv.(i) <- Int_vector_cons ( Array.sub w (k*i) (k) )
         done ;
         vv.(n - 1) <- Int_vector_cons ( Array.sub w (k*(n - 1)) (r-k*(n - 1)) ) ;
         Ioa_vector_cons vv 
   | _ ->
    let w = vector_ioa_demakeup v in
     let r = Array.length w in
      if r < n then failwith "Too short in Matrix.vector_ioa_cut."
      else
       let k = r / n in
        let vv = Array.make n (Int_cons 0) in
         for i = 0 to n-2 do 
          vv.(i) <- Ioa_vector_cons ( Array.sub w (k*i) (k) )
         done ;
         vv.(n - 1) <- Ioa_vector_cons ( Array.sub w (k*(n - 1)) (r-k*(n - 1)) ) ;
         Ioa_vector_cons vv ;;

(** {v vector_ioa_paste vector v} *)
let vector_ioa_paste = function (v:int_or_array) ->
 match (ioa_thickness v) with
  | 0 ->
   v
  | 1 ->
   let vv = vector_ioa_demakeup v in
    let vvv = Array.map vector_int_demakeup vv in
     let vvvv = Array.to_list vvv in
      Int_vector_cons (Array.concat vvvv) 
  | _ ->
   let vv = vector_ioa_demakeup v in
    let vvv = Array.map vector_ioa_demakeup vv in
     let vvvv = Array.to_list vvv in
      Ioa_vector_cons (Array.concat vvvv) ;;


(** {v foa_numrows matrix v} *)
let foa_numrows = function (m:float_or_array) ->
 match (foa_thickness m) with
 | 0 -> numrows (matrix_float_demakeup m)
 | _ -> numrows (matrix_foa_demakeup m) ;;

(** {v foa_numcolumns matrix v} *)
let foa_numcolumns = function (m:float_or_array) ->
 match (foa_thickness m) with
 | 0 -> numcolumns (matrix_float_demakeup m)
 | _ -> numcolumns (matrix_foa_demakeup m) ;;


(** {v ioa_numrows matrix v} *)
let ioa_numrows = function (m:int_or_array) ->
 match (ioa_thickness m) with
 | 0 -> numrows (matrix_int_demakeup m)
 | _ -> numrows (matrix_ioa_demakeup m) ;;

(** {v ioa_numcolumns matrix v} *)
let ioa_numcolumns = function (m:int_or_array) ->
 match (ioa_thickness m) with
 | 0 -> numcolumns (matrix_int_demakeup m)
 | _ -> numcolumns (matrix_ioa_demakeup m) ;;


(** {v matrix_float_cut parts matrix v} *)
let matrix_float_cut = fun (n:int) (w:float array array) ->
 if n <= 1 then [| Float_matrix_cons w ; Foa_vector_cons ( [| Float_vector_cons [| 0. ; 0. |] |] ) |] else
  let r = Array.length w
  and c = numcolumns w in
   let pg = Util.int_max r c in
    let k = int_of_float ( ceil ( (float pg) /. (float n) ) ) in
     let kk = k * n in
      let rr = kk - r
      and cc = kk - c
      and nn = n - 1 in
       let m = Array.make_matrix kk kk 0. in
        for i = 0 to r - 1 do
         let row_output = m.(i)
         and row_input = w.(i) in
          Array.blit row_input 0 row_output 0 c
        done ;
        if (rr > 0) or (cc > 0) then 
         begin
          for i = ( min r c) to kk - 1 do
           m.(i).(i) <- 1.
          done
         end ;
        let k_k = k - 1
        and mmm = Array.make_matrix n n (Float_cons 0.) in
         for i = 0 to nn do
          let row_output = mmm.(i) in
           for j = 0 to nn do
            let mm = Array.make_matrix k k 0. in
             for ii = 0 to k_k do
              let ligne_sortie = mm.(ii)
              and ligne_entree = m.( k * i + ii ) in
               for jj = 0 to k_k do
                ligne_sortie.(jj) <- ligne_entree.( k * j + jj )
               done ;
             done ;
             row_output.(j) <- Float_matrix_cons mm
           done ;
         done ;
         [| Foa_matrix_cons mmm ; ( Foa_vector_cons [| Float_vector_cons [| float rr ; float cc |] |] ) |] ;;


(** {v matrix_foa_cut parts matrix v} *)
let matrix_foa_cut = fun n (m:float_or_array) ->
 if n <= 1 then m else
  let r = foa_numrows m
  and c = foa_numcolumns m in
   let pp = min r c in
    if pp < n then  failwith "Too short in Matrix.matrix_foa_cut."
    else
     let k = pp / n in
      let mm = Array.make_matrix n n (Float_cons 0.) in
       match m with
        | Float_matrix_cons w ->
         for i = 0 to n-2 do 
          let row_output = mm.(i)
          and debut = k*i
          and fin = k*(i + 1) - 1 in
           begin
            for j = 0 to n-2 do
             row_output.(j) <- Float_matrix_cons ( float_sub_matrix w debut fin (k*j) (k*(j + 1) - 1) )
            done ;
            row_output.(n - 1) <- Float_matrix_cons ( float_sub_matrix w debut fin (k*(n - 1)) (c - 1) ) ;
           end 
         done ;
         let row_output = mm.(n - 1)
         and debut = k * ( n - 1 )
         and fin = r - 1 in
          begin
           for j = 0 to n-2 do
            row_output.(j) <- Float_matrix_cons ( float_sub_matrix w debut fin (k*j) (k*(j + 1) - 1) )
           done ;
           row_output.(n - 1) <- Float_matrix_cons ( float_sub_matrix w debut fin debut (c - 1) ) ;
          end ;
          Foa_matrix_cons mm 
        | Foa_matrix_cons ww ->
         for i = 0 to n-2 do 
          let row_output = mm.(i)
          and debut = k*i
          and fin = k*(i + 1) - 1 in
           begin
            for j = 0 to n-2 do
             row_output.(j) <- Foa_matrix_cons ( sub_matrix ww debut fin (k*j) (k*(j + 1) - 1) )
            done ;
           row_output.(n - 1) <- Foa_matrix_cons ( sub_matrix ww debut fin (k*(n - 1)) (c - 1) ) ;
           end 
         done ;
         let row_output = mm.(n - 1)
         and debut = k * ( n - 1 )
         and fin = r - 1 in
          begin
           for j = 0 to n-2 do
            row_output.(j) <- Foa_matrix_cons ( sub_matrix ww debut fin (k*j) (k*(j + 1) - 1) )
           done ;
           row_output.(n - 1) <- Foa_matrix_cons ( sub_matrix ww debut fin debut (c - 1) ) ;
          end ;
          Foa_matrix_cons mm
        | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_cut." ;;


(** {v matrix_foa_paste matrix v} *)
let matrix_foa_paste = function (m:float_or_array) ->
 match (foa_thickness m) with
  | 0 -> m
  | 1 ->
   let mm = matrix_foa_demakeup m in
    let r = Array.length mm
    and cc = (numcolumns mm) - 1
    and m_m = ref [] in
     for i = 0 to r - 1 do
      let w = mm.(i) in
       let ww = matrix_float_demakeup w.(0) in
        let rr = Array.length ww in
         for k = 0 to rr - 1 do
          let row_output = ref ww.(k) in
           for j = 1 to cc do 
            row_output := Array.append !row_output ( ( matrix_float_demakeup w.(j) ).(k) )
           done ;
         ww.(k) <- !row_output
       done ;
      m_m := ww::!m_m
    done ;
    m_m := List.rev !m_m ;
    Float_matrix_cons (Array.concat !m_m)
  | _ ->
   let r = foa_numrows m
   and cc = (foa_numcolumns m) - 1
   and m_m = ref [] in
    for i = 0 to r - 1 do
     let mm = (matrix_foa_demakeup m).(i) in
      let mmm = matrix_foa_demakeup mm.(0)
      and rr = foa_numrows mm.(0) in
       for k = 0 to rr - 1 do
        let row_output = ref mmm.(k) in
         for j = 1 to cc do 
          row_output := Array.append !row_output ( ( matrix_foa_demakeup mm.(j) ).(k) )
         done ;
         mmm.(k) <- !row_output
       done ;
       m_m := mmm::!m_m
     done ;
     m_m := List.rev !m_m ;
     Foa_matrix_cons (Array.concat !m_m) ;;


(** {v matrix_float_crash data v} *)
let matrix_float_crash = fun (data:float_or_array array) ->
 let m = data.(0) and s = vector_foa_demakeup data.(1) in
  let margin = vector_float_demakeup s.(0)
  and mm = matrix_float_demakeup (matrix_foa_paste m) in
    let endrow = (Array.length mm) - (int_of_float margin.(0)) - 1
    and endcolumn = (numcolumns mm) - (int_of_float margin.(1)) - 1 in
     float_sub_matrix mm 0 endrow 0 endcolumn ;;


(** {v matrix_int_cut parts matrix v} *)
let matrix_int_cut = fun (n:int) (w:int array array) ->
 if n <= 1 then [| Int_matrix_cons w ; Ioa_vector_cons ( [| Int_vector_cons [| 0 ; 0 |] |] ) |] else
  let r = Array.length w
  and c = numcolumns w in
   let pg = Util.int_max r c in
    let k = int_of_float ( ceil ( (float pg) /. (float n) ) ) in
     let kk = k * n in
      let rr = kk - r
      and cc = kk - c
      and nn = n - 1 in
       let m = Array.make_matrix kk kk 0 in
        for i = 0 to r - 1 do
         let row_output = m.(i)
         and row_input = w.(i) in
          Array.blit row_input 0 row_output 0 c
        done ;
        if (rr > 0) or (cc > 0) then 
         begin
          for i = ( min r c) to kk - 1 do
           m.(i).(i) <- 1
          done
         end ;
        let k_k = k - 1
        and mmm = Array.make_matrix n n (Int_cons 0) in
         for i = 0 to nn do
          let row_output = mmm.(i) in
           for j = 0 to nn do
            let mm = Array.make_matrix k k 0 in
             for ii = 0 to k_k do
              let ligne_sortie = mm.(ii)
              and ligne_entree = m.( k * i + ii ) in
               for jj = 0 to k_k do
                ligne_sortie.(jj) <- ligne_entree.( k * j + jj )
               done ;
             done ;
             row_output.(j) <- Int_matrix_cons mm
           done ;
         done ;
         [| Ioa_matrix_cons mmm ; ( Ioa_vector_cons [| Int_vector_cons [| rr ; cc |] |] ) |] ;;


(** {v matrix_ioa_cut parts matrix v} *)
let matrix_ioa_cut = fun n (m:int_or_array) ->
 if n <= 1 then m else
  let r = ioa_numrows m
  and c = ioa_numcolumns m in
   let pp = min r c
   and rr = pred r in
    if pp < n then  failwith "Too short in Matrix.matrix_ioa_cut."
    else
     let k = pp / n in
      let mm = Array.make_matrix n n (Int_cons 0) in
       match m with
        | Int_matrix_cons w ->
         for i = 0 to n-2 do 
          let row_output = mm.(i)
          and debut = k*i
          and fin = k*(i + 1) - 1 in
           begin
            for j = 0 to n-2 do
             row_output.(j) <- Int_matrix_cons ( int_sub_matrix w debut fin (k*j) (k*(j + 1) - 1) )
            done ;
            row_output.(n - 1) <- Int_matrix_cons ( int_sub_matrix w debut fin (k*(n - 1)) (c - 1) ) ;
           end 
         done ;
         let row_output = mm.(n - 1)
         and debut = k * ( n - 1 )
         and fin = rr in
          begin
           for j = 0 to n-2 do
            row_output.(j) <- Int_matrix_cons ( int_sub_matrix w debut fin (k*j) (k*(j + 1) - 1) )
           done ;
           row_output.(n - 1) <- Int_matrix_cons ( int_sub_matrix w debut fin debut (c - 1) ) ;
          end ;
          Ioa_matrix_cons mm 
        | Ioa_matrix_cons ww ->
         for i = 0 to n-2 do 
          let row_output = mm.(i)
          and debut = k*i
          and fin = k*(i + 1) - 1 in
           begin
            for j = 0 to n-2 do
             row_output.(j) <- Ioa_matrix_cons ( sub_matrix ww debut fin (k*j) (k*(j + 1) - 1) )
            done ;
           row_output.(n - 1) <- Ioa_matrix_cons ( sub_matrix ww debut fin (k*(n - 1)) (c - 1) ) ;
           end 
         done ;
         let row_output = mm.(n - 1)
         and debut = k * ( n - 1 )
         and fin = rr in
          begin
           for j = 0 to n-2 do
            row_output.(j) <- Ioa_matrix_cons ( sub_matrix ww debut fin (k*j) (k*(j + 1) - 1) )
           done ;
           row_output.(n - 1) <- Ioa_matrix_cons ( sub_matrix ww debut fin debut (c - 1) ) ;
          end ;
          Ioa_matrix_cons mm
        | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_cut." ;;


(** {v matrix_ioa_paste matrix v} *)
let matrix_ioa_paste = function (m:int_or_array) ->
 match (ioa_thickness m) with
  | 0 -> m
  | 1 ->
   let r = ioa_numrows m
   and cc = (ioa_numcolumns m) - 1
   and m_m = ref [] in
    for i = 0 to r - 1 do
     let mm = (matrix_ioa_demakeup m).(i) in
      let mmm = matrix_int_demakeup mm.(0)
      and rr = ioa_numrows mm.(0) in
       for k = 0 to rr -1 do
        let row_output = ref mmm.(k) in
         for j = 1 to cc do 
          row_output := Array.append !row_output ( (matrix_int_demakeup mm.(j)).(k) )
         done ;
         mmm.(k) <- !row_output
        done ;
       m_m := mmm::!m_m
     done ;
     m_m := List.rev !m_m ;
     Int_matrix_cons (Array.concat !m_m)
  | _ ->
   let r = ioa_numrows m
   and cc = (ioa_numcolumns m) - 1 
   and m_m = ref [] in
    for i = 0 to r - 1 do
     let mm = (matrix_ioa_demakeup m).(i) in
      let mmm = matrix_ioa_demakeup mm.(0)
      and rr = ioa_numrows mm.(0) in
       for k = 0 to rr - 1 do
        let row_output = ref mmm.(k) in
         for j = 1 to cc do 
          row_output := Array.append !row_output ( (matrix_ioa_demakeup mm.(j)).(k) )
         done ;
         mmm.(k) <- !row_output
        done ;
       m_m := mmm::!m_m
     done ;
     m_m := List.rev !m_m ;
     Ioa_matrix_cons (Array.concat !m_m) ;;


(** {v matrix_foa_hash base matrix v} *)
let rec matrix_foa_hash = fun b (m:float_or_array) ->
 let r = foa_numrows m
 and c = foa_numcolumns m in
  let pp = min r c in
   if b >= pp then m
   else let mm = matrix_foa_cut (pp/b) m in
    matrix_foa_hash b mm ;;

(** {v matrix_ioa_hash base matrix v} *)
let rec matrix_ioa_hash = fun b (m:int_or_array) ->
 let r = ioa_numrows m
 and c = ioa_numcolumns m in
  let pp = min r c in
   if b >= pp then m
   else let mm = matrix_ioa_cut (pp/b) m in
    matrix_ioa_hash b mm ;;

(** {v matrix_foa_crash matrix v} *)
let rec matrix_foa_crash = function (m:float_or_array) ->
 match foa_thickness m with
 | 0 -> m
 | 1 -> matrix_foa_paste m 
 | _ -> matrix_foa_crash (matrix_foa_paste m) ;;

(** {v matrix_ioa_crash matrix v} *)
let rec matrix_ioa_crash = function (m:int_or_array) ->
 match ioa_thickness m with
 | 0 -> m
 | 1 -> matrix_ioa_paste m 
 | _ -> matrix_ioa_crash (matrix_ioa_paste m) ;;




(** {C § } *)
(**
{9 ++++++ Operations on float vectors.

Opérations sur les vecteurs réels. ++++++ }
*)
(** {C  } *)




(** {v vector_foa_scal_mult coefficient vector v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_scal_mult = fun (lambda:float) (v:float_or_array) ->
 match v with
 | Float_cons x -> Float_cons ( x *. lambda )
 | Float_vector_cons u -> Float_vector_cons ( vector_float_scal_mult lambda u )
 | Foa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_foa_scal_mult lambda w.(i) 
     done ;
     Foa_vector_cons !vvv 
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_scal_mult." ;;


(** {v vector_foa_scal_add coefficient vector v} Inner blocks reduced to a real are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec vector_foa_scal_add = fun (lambda:float) (v:float_or_array) ->
 match v with
 | Float_cons x -> Float_cons ( lambda +. x )
 | Float_vector_cons u -> Float_vector_cons ( vector_float_scal_add lambda u )
 | Foa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_foa_scal_add lambda w.(i) 
     done ;
     Foa_vector_cons !vvv 
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_scal_add." ;;


(** {v vector_foa_scal_right_div coefficient vector v} Inner blocks reduced to a real are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec vector_foa_scal_right_div = fun (lambda:float) (v:float_or_array) ->
 match v with
 | Float_cons x -> Float_cons ( x /. lambda )
 | Float_vector_cons u -> Float_vector_cons ( vector_float_scal_right_div lambda u )
 | Foa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_foa_scal_right_div lambda w.(i) 
     done ;
     Foa_vector_cons !vvv 
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_scal_right_div." ;;


(** {v vector_foa_scal_left_div coefficient vector v} Inner blocks reduced to areal are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec vector_foa_scal_left_div = fun (lambda:float) (v:float_or_array) ->
 match v with
 | Float_cons x ->  Float_cons ( lambda /. x ) 
 | Float_vector_cons u -> Float_vector_cons ( vector_float_scal_left_div lambda u )
 | Foa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_foa_scal_left_div lambda w.(i) 
     done ;
     Foa_vector_cons !vvv 
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_scal_left_div." ;;


(** {v vector_foa_scal_right_sub coefficient vector v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_scal_right_sub = fun (lambda:float) (v:float_or_array) ->
 match v with
 | Float_cons 0. -> Float_cons ( -. lambda )
 | Float_vector_cons u -> Float_vector_cons ( vector_float_scal_right_sub lambda u )
 | Foa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_foa_scal_right_sub lambda w.(i) 
     done ;
     Foa_vector_cons !vvv 
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_scal_right_sub." ;;


(** {v vector_foa_scal_left_sub coefficient vector v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_scal_left_sub = fun (lambda:float) (v:float_or_array) ->
 match v with
 | Float_cons 0. -> Float_cons lambda
 | Float_vector_cons u -> Float_vector_cons ( vector_float_scal_left_sub lambda u )
 | Foa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_foa_scal_left_sub lambda w.(i) 
     done ;
     Foa_vector_cons !vvv 
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_scal_left_sub." ;;


(** {v vector_foa_opp vector v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_opp = function (v:float_or_array) -> match v with
 | Float_cons 0. -> v
 | Float_vector_cons u -> Float_vector_cons ( vector_float_opp u )
 | Foa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_foa_opp w.(i) 
     done ;
     Foa_vector_cons !vvv 
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_opp." ;;


(** {v vector_foa_coeff_prod vector1 vector2 v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_coeff_prod = fun (v:float_or_array) (vv:float_or_array) ->
 match v with
 | Float_cons 0. -> v
 | Float_cons x -> vector_foa_scal_mult x vv
 | Float_vector_cons u ->
  begin
   match vv with 
    | Float_cons 0. -> vv
    | Float_cons y ->  vector_foa_scal_mult y v
    | Float_vector_cons uu -> Float_vector_cons ( vector_float_coeff_prod u uu )
    | _ -> failwith "Not a float_or_array vector of thickness 0 in Matrix.vector_foa_coeff_prod."
  end
 | Foa_vector_cons w ->
  begin
   match vv with
    | Float_cons 0. -> vv
    | Float_cons y -> vector_foa_scal_mult y v
    | Foa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref (Array.make r w.(0)) in
        for i = 0 to r - 1 do
         !vvv.(i) <- vector_foa_coeff_prod w.(i) ww.(i)
        done ;
        Foa_vector_cons !vvv 
     end
   | _ -> failwith "Not a float_or_array vector of same thickness in Matrix.vector_foa_coeff_prod."
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_coeff_prod." ;;


(** {v vector_foa_sum vector v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_sum = function (v:float_or_array) ->
 match v with
 | Float_cons 0. -> 0.
 | Float_vector_cons u -> vector_float_sum u
 | Foa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref 0. in
     for i = 0 to r - 1 do
      vvv := !vvv +. vector_foa_sum w.(i) 
     done ;
     !vvv
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_sum." ;;


(** {v vector_foa_scal_prod vector1 vector2 v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_scal_prod = fun (v:float_or_array) (vv:float_or_array) ->
 match v with
 | Float_cons 0. -> 0.
 | Float_cons x -> x *. vector_foa_sum vv
 | Float_vector_cons u ->
  begin
   match vv with 
    | Float_cons 0. -> 0.
    | Float_cons y -> y *. vector_float_sum u
    | Float_vector_cons uu -> vector_float_scal_prod u uu
    | _ -> failwith "Not a float_or_array vector of thickness 0 in Matrix.vector_foa_scal_prod."
  end
 | Foa_vector_cons w ->
  begin
   match vv with
    | Float_cons 0. -> 0.
    | Float_cons y -> y *. vector_foa_sum v
    | Foa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref 0. in
        for i = 0 to r - 1 do
         vvv := !vvv +. vector_foa_scal_prod w.(i) ww.(i)
        done ;
        !vvv 
     end
   | _ -> failwith "Not a float_or_array vector of same thickness in Matrix.vector_foa_scal_prod."
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_scal_prod." ;;


(** {v vector_foa_max vector v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_max = function (v:float_or_array) ->
 match v with
 | Float_cons x -> x
 | Float_vector_cons u -> vector_float_max u
 | Foa_vector_cons w ->
  begin
   let vvv = ref ( -. max_float )
   and rr = (Array.length w) - 1 in
    for i = 0 to rr do
     vvv := max !vvv ( vector_foa_max w.(i) )
    done ;
    !vvv
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_max." ;;


(** {v vector_foa_min vector v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_min = function (v:float_or_array) ->
 match v with
 | Float_cons x -> x
 | Float_vector_cons u -> vector_float_min u
 | Foa_vector_cons w ->
  begin
   let vvv = ref max_float
   and rr = (Array.length w) - 1 in
    for i = 0 to rr do
     vvv := min !vvv ( vector_foa_min w.(i) )
    done ;
    !vvv
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_min." ;;


(** {v vector_foa_plus vector1 vector2 v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_plus = fun (v:float_or_array) (vv:float_or_array) ->
 match v with
 | Float_cons 0. -> vv
 | Float_cons x -> vector_foa_scal_add x vv
 | Float_vector_cons u ->
  begin
   match vv with 
    | Float_cons 0. -> v
    | Float_cons y ->  vector_foa_scal_add y v
    | Float_vector_cons uu -> Float_vector_cons ( vector_float_plus u uu )
    | _ -> failwith "Not a float_or_array vector of thickness 0 in Matrix.vector_foa_plus."
  end
 | Foa_vector_cons w ->
  begin
   match vv with
    | Float_cons 0. -> v
    | Float_cons y -> vector_foa_scal_add y v
    | Foa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref (Array.make r w.(0)) in
        for i = 0 to r - 1 do
         !vvv.(i) <- vector_foa_plus w.(i) ww.(i)
        done ;
        Foa_vector_cons !vvv 
     end
   | _ -> failwith "Not a float_or_array vector of same thickness in Matrix.vector_foa_plus."
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_plus." ;;


(** {v vector_foa_minus vector1 vector2 v} Inner blocks reduced to the real 0. are accepted.

Des blocs réduits au réel 0. sont tolérés. *)
let rec vector_foa_minus = fun (v:float_or_array) (vv:float_or_array) ->
 match v with
 | Float_cons 0. -> vector_foa_opp vv
 | Float_cons x -> vector_foa_scal_right_sub x vv
 | Float_vector_cons u ->
  begin
   match vv with 
    | Float_cons 0. -> v
    | Float_cons y ->  vector_foa_scal_left_sub y v
    | Float_vector_cons uu -> Float_vector_cons ( vector_float_minus u uu )
    | _ -> failwith "Not a float_or_array vector of thickness 0 in Matrix.vector_foa_minus."
  end
 | Foa_vector_cons w ->
  begin
   match vv with
    | Float_cons 0. -> v
    | Float_cons y -> vector_foa_scal_left_sub y v
    | Foa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref (Array.make r w.(0)) in
        for i = 0 to r - 1 do
         !vvv.(i) <- vector_foa_minus w.(i) ww.(i)
        done ;
        Foa_vector_cons !vvv 
     end
   | _ -> failwith "Not a float_or_array vector of same thickness in Matrix.vector_foa_minus."
  end
 | _ -> failwith "Not a float_or_array vector in Matrix.vector_foa_minus." ;;




(** {C § } *)
(**
{9 ++++++ Operations on integer vectors.

Opérations sur les vecteurs entiers. ++++++ }
*)
(** {C  } *)




(** {v vector_ioa_scal_mult coefficient vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_mult = fun (lambda:int) (v:int_or_array) ->
 match v with
 | Int_cons 0 -> v
 | Int_vector_cons u -> Int_vector_cons ( vector_int_scal_mult lambda u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_scal_mult lambda w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_mult." ;;


(** {v vector_ioa_scal_add coefficient vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_add = fun (lambda:int) (v:int_or_array) ->
 match v with
 | Int_cons 0 -> Int_cons lambda
 | Int_vector_cons u -> Int_vector_cons ( vector_int_scal_add lambda u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_scal_add lambda w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_add." ;;


(** {v vector_ioa_scal_right_div coefficient vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_right_div = fun (lambda:int) (v:int_or_array) ->
 match v with
 | Int_cons 0 -> v
 | Int_vector_cons u -> Int_vector_cons ( vector_int_scal_right_div lambda u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_scal_right_div lambda w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_right_div." ;;


(** {v vector_ioa_scal_left_div coefficient vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_left_div = fun (lambda:int) (v:int_or_array) ->
 match v with
 | Int_cons 0 -> failwith "Division_by_zero in Matrix.vector_ioa_scal_left_div."
 | Int_vector_cons u -> Int_vector_cons ( vector_int_scal_left_div lambda u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_scal_left_div lambda w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_left_div." ;;


(** {v vector_ioa_scal_right_mod coefficient vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_right_mod = fun (lambda:int) (v:int_or_array) ->
 match v with
 | Int_cons 0 -> v
 | Int_vector_cons u -> Int_vector_cons ( vector_int_scal_right_mod lambda u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_scal_right_mod lambda w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_right_mod." ;;


(** {v vector_ioa_scal_left_mod coefficient vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_left_mod = fun (lambda:int) (v:int_or_array) ->
 match v with
 | Int_cons 0 -> Int_cons lambda
 | Int_vector_cons u -> Int_vector_cons ( vector_int_scal_left_mod lambda u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_scal_left_mod lambda w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_left_mod." ;;


(** {v vector_ioa_scal_right_sub coefficient vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_right_sub = fun (lambda:int) (v:int_or_array) ->
 match v with
 | Int_cons 0 -> Int_cons ( - lambda )
 | Int_vector_cons u -> Int_vector_cons ( vector_int_scal_right_sub lambda u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_scal_right_sub lambda w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_right_sub." ;;


(** {v vector_ioa_scal_left_sub coefficient vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_left_sub = fun (lambda:int) (v:int_or_array) ->
 match v with
 | Int_cons 0 -> Int_cons lambda
 | Int_vector_cons u -> Int_vector_cons ( vector_int_scal_left_sub lambda u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_scal_left_sub lambda w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_left_sub." ;;


(** {v vector_ioa_opp vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_opp = function (v:int_or_array) ->
 match v with
 | Int_cons 0 -> v
 | Int_vector_cons u -> Int_vector_cons ( vector_int_opp u )
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref (Array.make r w.(0)) in
     for i = 0 to r - 1 do
      !vvv.(i) <- vector_ioa_opp w.(i) 
     done ;
     Ioa_vector_cons !vvv 
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_opp." ;;


(** {v vector_ioa_coeff_prod vector1 vector2 v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_coeff_prod = fun (v:int_or_array) (vv:int_or_array) ->
 match v with
 | Int_cons 0 -> v
 | Int_cons x -> vector_ioa_scal_mult x vv
 | Int_vector_cons u ->
  begin
   match vv with 
    | Int_cons 0 -> vv
    | Int_cons y ->  vector_ioa_scal_mult y v
    | Int_vector_cons uu -> Int_vector_cons ( vector_int_coeff_prod u uu )
    | _ -> failwith "Not an int_or_array vector of thickness 0 in Matrix.vector_ioa_coeff_prod."
  end
 | Ioa_vector_cons w ->
  begin
   match vv with
    | Int_cons 0 -> vv
    | Int_cons y -> vector_ioa_scal_mult y v
    | Ioa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref (Array.make r w.(0)) in
        for i = 0 to r - 1 do
         !vvv.(i) <- vector_ioa_coeff_prod w.(i) ww.(i)
        done ;
        Ioa_vector_cons !vvv 
     end
   | _ -> failwith "Not an int_or_array vector of same thickness in Matrix.vector_ioa_coeff_prod."
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_coeff_prod." ;;


(** {v vector_ioa_coeff_div vector1 vector2 v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_coeff_div = fun (v:int_or_array) (vv:int_or_array) ->
 match v with
 | Int_cons 0 -> v
 | Int_cons x -> vector_ioa_scal_mult x vv
 | Int_vector_cons u ->
  begin
   match vv with 
    | Int_cons 0 -> vv
    | Int_cons y ->  vector_ioa_scal_mult y v
    | Int_vector_cons uu -> Int_vector_cons ( vector_int_coeff_div u uu )
    | _ -> failwith "Not an int_or_array vector of thickness 0 in Matrix.vector_ioa_coeff_div."
  end
 | Ioa_vector_cons w ->
  begin
   match vv with
    | Int_cons 0 -> vv
    | Int_cons y -> vector_ioa_scal_mult y v
    | Ioa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref (Array.make r w.(0)) in
        for i = 0 to r - 1 do
         !vvv.(i) <- vector_ioa_coeff_div w.(i) ww.(i)
        done ;
        Ioa_vector_cons !vvv 
     end
   | _ -> failwith "Not an int_or_array vector of same thickness in Matrix.vector_ioa_coeff_div."
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_coeff_div." ;;


(** {v vector_ioa_coeff_mod vector1 vector2 v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_coeff_mod = fun (v:int_or_array) (vv:int_or_array) ->
 match v with
 | Int_cons 0 -> v
 | Int_cons x -> vector_ioa_scal_mult x vv
 | Int_vector_cons u ->
  begin
   match vv with 
    | Int_cons 0 -> vv
    | Int_cons y ->  vector_ioa_scal_mult y v
    | Int_vector_cons uu -> Int_vector_cons ( vector_int_coeff_mod u uu )
    | _ -> failwith "Not an int_or_array vector of thickness 0 in Matrix.vector_ioa_coeff_mod."
  end
 | Ioa_vector_cons w ->
  begin
   match vv with
    | Int_cons 0 -> vv
    | Int_cons y -> vector_ioa_scal_mult y v
    | Ioa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref (Array.make r w.(0)) in
        for i = 0 to r - 1 do
         !vvv.(i) <- vector_ioa_coeff_mod w.(i) ww.(i)
        done ;
        Ioa_vector_cons !vvv 
     end
   | _ -> failwith "Not an int_or_array vector of same thickness in Matrix.vector_ioa_coeff_mod."
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_coeff_mod." ;;


(** {v vector_ioa_sum vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_sum = function (v:int_or_array) ->
 match v with
 | Int_cons 0 -> 0
 | Int_vector_cons u -> vector_int_sum u
 | Ioa_vector_cons w ->
  begin
   let r = Array.length w in
    let vvv = ref 0 in
     for i = 0 to r - 1 do
      vvv := !vvv + vector_ioa_sum w.(i) 
     done ;
     !vvv
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_sum." ;;


(** {v vector_ioa_scal_prod vector1 vector2 v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_scal_prod = fun (v:int_or_array) (vv:int_or_array) ->
 match v with
 | Int_cons 0 -> 0
 | Int_cons x -> x * vector_ioa_sum vv
 | Int_vector_cons u ->
  begin
   match vv with 
    | Int_cons 0 -> 0
    | Int_cons y -> y * vector_int_sum u
    | Int_vector_cons uu -> vector_int_scal_prod u uu
    | _ -> failwith "Not an int_or_array vector of thickness 0 in Matrix.vector_ioa_scal_prod."
  end
 | Ioa_vector_cons w ->
  begin
   match vv with
    | Int_cons 0 -> 0
    | Int_cons y -> y * vector_ioa_sum v
    | Ioa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref 0 in
        for i = 0 to r - 1 do
         vvv := !vvv + vector_ioa_scal_prod w.(i) ww.(i)
        done ;
        !vvv 
     end
   | _ -> failwith "Not an int_or_array vector of same thickness in Matrix.vector_ioa_scal_prod."
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_scal_prod." ;;


(** {v vector_ioa_max vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_max = function (v:int_or_array) ->
 match v with
 | Int_cons x -> x
 | Int_vector_cons u -> vector_int_max u
 | Ioa_vector_cons w ->
  begin
   let vvv = ref ( - max_int )
   and rr = (Array.length w) - 1 in
    for i = 0 to rr do
     vvv := Util.int_max !vvv ( vector_ioa_max w.(i) )
    done ;
    !vvv
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_max." ;;


(** {v vector_ioa_min vector v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_min = function (v:int_or_array) ->
 match v with
 | Int_cons x -> x
 | Int_vector_cons u -> vector_int_min u
 | Ioa_vector_cons w ->
  begin
   let vvv = ref max_int
   and rr = (Array.length w) - 1 in
    for i = 0 to rr do
     vvv := min !vvv ( vector_ioa_min w.(i) )
    done ;
    !vvv
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_min." ;;


(** {v vector_ioa_plus vector1 vector2 v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_plus = fun (v:int_or_array) (vv:int_or_array) ->
 match v with
 | Int_cons 0 -> vv
 | Int_cons x -> vector_ioa_scal_add x vv
 | Int_vector_cons u ->
  begin
   match vv with 
    | Int_cons 0 -> v
    | Int_cons y ->  vector_ioa_scal_add y v
    | Int_vector_cons uu -> Int_vector_cons ( vector_int_plus u uu )
    | _ -> failwith "Not an int_or_array vector of thickness 0 in Matrix.vector_ioa_plus."
  end
 | Ioa_vector_cons w ->
  begin
   match vv with
    | Int_cons 0 -> v
    | Int_cons y -> vector_ioa_scal_add y v
    | Ioa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref (Array.make r w.(0)) in
        for i = 0 to r - 1 do
         !vvv.(i) <- vector_ioa_plus w.(i) ww.(i)
        done ;
        Ioa_vector_cons !vvv 
     end
   | _ -> failwith "Not an int_or_array vector of same thickness in Matrix.vector_ioa_plus."
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_plus." ;;


(** {v vector_ioa_minus vector1 vector2 v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec vector_ioa_minus = fun (v:int_or_array) (vv:int_or_array) ->
 match v with
 | Int_cons 0 -> vector_ioa_opp vv
 | Int_cons x -> vector_ioa_scal_right_sub x vv
 | Int_vector_cons u ->
  begin
   match vv with 
    | Int_cons 0 -> v
    | Int_cons y ->  vector_ioa_scal_left_sub y v
    | Int_vector_cons uu -> Int_vector_cons ( vector_int_minus u uu )
    | _ -> failwith "Not an int_or_array vector of thickness 0 in Matrix.vector_ioa_minus."
  end
 | Ioa_vector_cons w ->
  begin
   match vv with
    | Int_cons 0 -> v
    | Int_cons y -> vector_ioa_scal_left_sub y v
    | Ioa_vector_cons ww ->
     begin
      let r = Array.length w in
       let vvv = ref (Array.make r w.(0)) in
        for i = 0 to r - 1 do
         !vvv.(i) <- vector_ioa_minus w.(i) ww.(i)
        done ;
        Ioa_vector_cons !vvv 
     end
   | _ -> failwith "Not an int_or_array vector of same thickness in Matrix.vector_ioa_minus."
  end
 | _ -> failwith "Not an int_or_array vector in Matrix.vector_ioa_minus." ;;




(** {C § } *)
(**
{9 ++++++ Search, extraction, affectation, measure and display.

Recherche, extraction, affectation, mesure et affichage. ++++++ }
*)
(** {C  } *)




(** {v matrix_foa_max matrix v} Inner blocks reduced to a real are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_max = function (m:float_or_array) ->
 match m with
 | Float_cons x -> x
 | Float_matrix_cons u -> matrix_float_max u
 | Foa_matrix_cons w ->
  begin
   let accu = ref (-. max_float) in
    for i = 0 to (Array.length w) - 1 do
     let row = w.(i) in
      for j = 0 to (Array.length row) - 1 do
       accu := max !accu ( matrix_foa_max row.(j) )
      done
    done ;
    !accu
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_max." ;;


(** {v matrix_foa_min matrix v} Inner blocks reduced to a real are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_min = function (m:float_or_array) ->
 match m with
 | Float_cons x -> x
 | Float_matrix_cons u -> matrix_float_min u
 | Foa_matrix_cons w ->
  begin
   let accu = ref (max_float) in
    for i = 0 to (Array.length w) - 1 do
     let row = w.(i) in
      for j = 0 to (Array.length row) - 1 do
       accu := min !accu ( matrix_foa_min row.(j) )
      done
    done ;
    !accu
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_min." ;;


(** {v vector_foa_find_last element vector v} vector_foa_find_last returns \[-1\] if it does not find:

vector_foa_find_last retourne \[-1\] s'il ne trouve pas. *)
let rec vector_foa_find_last = fun (x:float) (v:float_or_array) ->
 match (foa_thickness v) with
 | 0 -> [ vector_float_find_last x (vector_float_demakeup v) ]
 | _ ->
  let w = vector_foa_demakeup v in
   let r = Array.length w
   and index = ref (-1) 
   and suite = ref [] in
    let i = ref ( r - 1 ) in
     while !i >= 0 do
      let essaisuite = vector_foa_find_last x w.(!i) in
       if (List.hd essaisuite <> -1) then 
        begin
         index:= !i ;
         suite := essaisuite ;
         i := -1 ;
        end
       else i := !i - 1 
     done ;
     !index::!suite ;;


(** {v vector_foa_find_first element vector v} vector_foa_find_first returns \[-1\] if it does not find:

vector_foa_find_first retourne \[-1\] s'il ne trouve pas. *)
let rec vector_foa_find_first = fun (x:float) (v:float_or_array) ->
 match (foa_thickness v) with
 | 0 -> [ vector_float_find_first x (vector_float_demakeup v) ]
 | _ ->
 let w = vector_foa_demakeup v in
  let r = Array.length w
  and index = ref (-1) 
  and suite = ref [] 
  and i = ref 0 in
   while  !i < r do
    let essaisuite = vector_foa_find_first x w.(!i) in
     if (List.hd essaisuite <> -1) then 
      begin
       index := !i ;
       suite := essaisuite ;
       i := r ;
      end
     else i := !i + 1 ;
   done ;
  !index::!suite ;;


(** {v vector_foa_extract_element position vector v} vector_foa_extract_element walks 
along the layers following the list of indexes up to the desired depth.

vector_foa_extract_element parcourt les couches en suivant la liste des indices jusqu'à la profondeur souhaitée. *)
let rec vector_foa_extract_element = fun (pos:int list) (v:float_or_array) ->
 match ( List.length pos ) with
 | 0 -> failwith "List too short in Matrix.vector_foa_extract_element." 
 | 1 ->
  begin
   match (foa_thickness v) with
   | 0 -> Float_cons (vector_float_demakeup v).(List.hd pos)
   | _ -> (vector_foa_demakeup v).(List.hd pos) 
  end
 | _ -> vector_foa_extract_element (List.tl pos) (vector_foa_demakeup v).(List.hd pos) ;;


(** {v matrix_foa_extract_element position matrix v} matrix_foa_extract_element walks 
along the layers following the list of indexes up to the desired depth.

matrix_foa_extract_element parcourt les couches en suivant la liste des indices jusqu'à la profondeur souhaitée. *)
let rec matrix_foa_extract_element = fun (pos:int array list) (m:float_or_array) ->
 match ( List.length pos ) with
 | 0 -> failwith "List too short in Matrix.matrix_foa_extract_element." 
 | 1 ->
  let index = List.hd pos in
   begin
    match (foa_thickness m) with
    | 0 -> Float_cons (matrix_float_demakeup m).(index.(0)).(index.(1))
    | _ -> (matrix_foa_demakeup m).(index.(0)).(index.(1))
   end
 | _ ->
  let index = List.hd pos in
   matrix_foa_extract_element (List.tl pos) (matrix_foa_demakeup m).(index.(0)).(index.(1)) ;;


(** {v matrix_foa_extract_column column matrix v} matrix_foa_extract_column works on the superficial layer.

matrix_foa_extract_column travaille sur la couche superficielle. *)
let matrix_foa_extract_column = fun (i:int) (m:float_or_array) ->
 match (foa_thickness m) with
  | 0 ->
   let col = extract_column i (matrix_float_demakeup m) in
    Float_vector_cons col
  | _ ->
   let col = extract_column i (matrix_foa_demakeup m) in
    Foa_vector_cons col ;;


(** {v matrix_foa_extract_row row matrix v} matrix_foa_extract_row works on the superficial layer.

matrix_foa_extract_row travaille sur la couche superficielle. *)
let matrix_foa_extract_row = fun (i:int) (m:float_or_array) ->
 match (foa_thickness m) with
  | 0 ->
   Float_vector_cons ( (matrix_float_demakeup m).(i) )
  | _ ->
   Foa_vector_cons ( (matrix_foa_demakeup m).(i) ) ;;


(** {v matrix_foa_size matrix v} matrix_foa_size works on the superficial layer. 
Inner blocks reduced to a real are accepted.

matrix_foa_size travaille sur la couche superficielle.
Des blocs réduits à un réel sont tolérés. *)
let matrix_foa_size = function (m:float_or_array) ->
 match m with
 | Float_cons x -> Int_vector_cons [| 1 ; 1 |]
 | Float_matrix_cons mm -> Int_vector_cons ( [| Array.length mm ; numcolumns mm |] )
 | _ -> let mm = matrix_foa_demakeup m in
   Int_vector_cons ( [| Array.length mm ; numcolumns mm |] ) ;;


(** {v matrix_foa_rec_size depth matrix v} matrix_foa_rec_size walks 
along the layers up to the desired depth.
Inner blocks reduced to a real are accepted.

matrix_foa_rec_size parcourt les couches jusqu'à la profondeur souhaitée.
Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_rec_size = fun (d:int) (m:float_or_array) ->
 match d with
 | 0 -> matrix_foa_size m 
 | _ ->
  begin
   match m with
    | Float_cons x -> Int_vector_cons [| 1 ; 1 |]
    | Float_matrix_cons x -> matrix_foa_size m
    | _ -> let mm = matrix_foa_demakeup m in
     let r = Array.length mm 
     and c = numcolumns mm in
      let m_m = Array.make_matrix r c (Int_cons 0)
      and cc = c - 1 in 
       for i = 0 to r - 1 do
        let row_input = mm.(i)
        and row_output = m_m.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_foa_rec_size ( d - 1 ) row_input.(j)
         done
       done ;
       Ioa_matrix_cons m_m
  end ;;


(** {v vector_foa_affect index element vector v} vector_foa_affect works on the superficial layer.

vector_foa_affect travaille sur la couche superficielle. *)
let vector_foa_affect = fun (index:int) (x:float_or_array) (v:float_or_array) ->
 match (foa_thickness v) with
 | 0 ->
  let vv = vector_float_demakeup v in
   vv.(index) <- float_demakeup x ;
   Float_vector_cons vv
 | _ ->
  let vv = vector_foa_demakeup v in
   vv.(index) <- x ;
   Foa_vector_cons vv ;;


(** {v vector_foa_affect_element position element vector v} vector_foa_affect_element walks 
along the layers following the list of indexes up to the desired depth.

vector_foa_affect_element parcourt les couches en suivant la liste des indices jusqu'à la profondeur souhaitée. *)
let rec vector_foa_affect_element = fun (position:int list)  (x:float_or_array) (v:float_or_array) ->
 match ( List.length position ) with
 | 0 -> failwith "List too short in Matrix.vector_foa_affect_element."
 | 1 -> vector_foa_affect (List.hd position) x v
 | _ ->
  let vv = vector_foa_demakeup v
  and pos = List.hd position in
   vv.(pos) <- vector_foa_affect_element (List.tl position) x vv.(pos) ;
   Foa_vector_cons vv ;;


(** {v matrix_foa_affect index element matrix v} matrix_foa_affect works on the superficial layer.

matrix_foa_affect travaille sur la couche superficielle. *)
let matrix_foa_affect = fun (index:int array) (x:float_or_array) (m:float_or_array) ->
 match (foa_thickness m) with
 | 0 ->
  let mm = matrix_float_demakeup m in
   mm.(index.(0)).(index.(1)) <- float_demakeup x ;
   Float_matrix_cons mm
 | _ ->
  let mm = matrix_foa_demakeup m in
   mm.(index.(0)).(index.(1)) <- x ;
   Foa_matrix_cons mm ;;


(** {v matrix_foa_affect_element position element matrix v} matrix_foa_affect_element walks 
along the layers following the list of indexes up to the desired depth.

matrix_foa_affect_element parcourt les couches en suivant la liste des indices jusqu'à la profondeur souhaitée.*)
let rec matrix_foa_affect_element = fun (position:int array list) (x:float_or_array) (m:float_or_array) ->
 match ( List.length position ) with
 | 0 -> failwith "List too short in Matrix.matrix_foa_affect_element."
 | 1 -> matrix_foa_affect (List.hd position) x m
 | _ ->
  let mm = matrix_foa_demakeup m
  and pos = List.hd position in
   mm.( pos.(0) ).( pos.(1) ) <- matrix_foa_affect_element (List.tl position) x mm.( pos.(0) ).( pos.(1) ) ;
   Foa_matrix_cons mm ;;


(** {v matrix_foa_affect_row index vector matrix v} matrix_foa_affect_row works on the superficial layer.
The size of the vector may be bigger than the number of rows of the matrix.

matrix_foa_affect_row travaille sur la couche superficielle. 
La taille du vecteur peut dépassr le nombre de lignes de la matrice. *)
let matrix_foa_affect_row = fun (index:int) (v:float_or_array) (m:float_or_array) ->
 match (foa_thickness m) with
 | 0 ->
  let mm = matrix_float_demakeup m in
   mm.(index) <- vector_float_demakeup v ;
   Float_matrix_cons mm
 | _ ->
  let mm = matrix_foa_demakeup m in
   mm.(index) <- vector_foa_demakeup v ;
   Foa_matrix_cons mm ;;


(** {v matrix_foa_affect_column index vector matrix v} matrix_foa_affect_column works on the superficial layer.
The size of the vector may be bigger than the number of rows of the matrix.

matrix_foa_affect_column travaille sur la couche superficielle.
La taille du vecteur peut dépassr le nombre de lignes de la matrice. *)
let matrix_foa_affect_column = fun (index:int) (v:float_or_array) (m:float_or_array) ->
 match (foa_thickness m) with
 | 0 ->
  let mm = matrix_float_demakeup m in
   affect_column index (vector_float_demakeup v) mm ;
   Float_matrix_cons mm
 | _ ->
  let mm = matrix_foa_demakeup m in
   let r = Array.length mm in
    for i = 0 to r - 1 do
     mm.(i).(index) <- ( vector_foa_demakeup v ).(i)
    done ;
    Foa_matrix_cons mm ;;


(** {v matrix_foa_find_last element matrix v} matrix_foa_find_last returns \[\[|-1;-1|\]\] if it does not find:

matrix_foa_find_last retourne \[\[|-1;-1|\]\] s'il ne trouve pas. *)
let rec matrix_foa_find_last = fun (x:float) (m:float_or_array) ->
 match (foa_thickness m) with
 | 0 -> [ matrix_float_find_last x (matrix_float_demakeup m) ]
 | _ ->
  let mm = matrix_foa_demakeup m in
   let r = Array.length mm
   and c = numcolumns mm
   and index = ref ([|-1;-1|]) 
   and suite = ref [] in
    let i = ref ( r - 1 )
    and j = ref ( c - 1 ) in
     while !i >= 0 do
      let row_input = mm.(!i) in
       while !j >= 0 do
        let essaisuite = matrix_foa_find_last x ( row_input.(!j) ) in
         if ( List.hd essaisuite <> [|-1;-1|] ) then 
          begin
           index := [|!i;!j|] ;
           suite := essaisuite ;
           j := -1 ;
           i := -1 ;
          end
        else j := !j - 1 ;
       done ;
       if ( !index = [|-1;-1|] ) then i := !i - 1 ;
      done ;
      !index::!suite ;;


(** {v matrix_foa_find_first element matrix v} matrix_foa_find_first returns \[\[|-1;-1|\]\] if it does not find:

matrix_foa_find_first retourne \[\[|-1;-1|\]\] s'il ne trouve pas. *)
let rec matrix_foa_find_first = fun (x:float) (m:float_or_array) ->
 match (foa_thickness m) with
 | 0 -> [ matrix_float_find_first x (matrix_float_demakeup m) ]
 | _ ->
  let mm = matrix_foa_demakeup m in
   let r = Array.length mm
   and c = numcolumns mm
   and index = ref ([|-1;-1|]) 
   and suite = ref []
   and i = ref 0 
   and j = ref 0 in
    while !i < r do
     let row_input = mm.(!i) in
      while !j < c do 
       let essaisuite = matrix_foa_find_first x ( row_input.(!j) ) in
        if ( List.hd essaisuite <> [|-1;-1|] ) then 
         begin
          index := [|!i;!j|] ;
          suite := essaisuite ;
          j := c ;
          i := r ;
         end
        else j := !j + 1 ;
      done ;
      if ( !index = [|-1;-1|] ) then i := !i + 1 ;
     done ;
     !index::!suite ;;


(** {v vector_foa_print vector v} *)
let rec vector_foa_print = function (v:float_or_array) ->
 match (foa_thickness v) with
 | 0 -> bare_vector_float_print (vector_float_demakeup v)
 | _ -> let w = vector_foa_demakeup v in
  let rr = (Array.length w) - 1 in
   print_string "[| " ;
   vector_foa_print w.(0) ;
   print_string " ; " ;
   for i = 1 to ( rr - 1 ) do
    vector_foa_print w.(i) ; print_string " ; "
   done ;
   vector_foa_print w.(rr) ;
   print_string " |]" ;;


(** {v matrix_foa_print matrix v} *)
let rec matrix_foa_print = function (m:float_or_array) ->
 match (foa_thickness m) with
 | 0 -> matrix_float_print (matrix_float_demakeup m) 
 | _ -> let w = matrix_foa_demakeup m in
  let r = Array.length w
  and cc = (numcolumns w) - 1 in
   print_string "[| with " ;
   print_int r ;
   print_string " rows" ;
   print_newline () ;
   for i = 0 to r - 1 do
    let row = w.(i) in
     for j = 0 to cc do
      matrix_foa_print row.(j)  
     done ;
   done ;
   print_string "|] and " ;
   print_int (cc + 1) ;
   print_string " columns" ;
   print_newline () ;;



(** {v matrix_ioa_max matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_max = function (m:int_or_array) ->
 match m with
 | Int_cons x -> x
 | Int_matrix_cons u -> matrix_int_max u
 | Ioa_matrix_cons w ->
  begin
   let accu = ref (- max_int) in
    for i = 0 to (Array.length w) - 1 do
     let row = w.(i) in
      for j = 0 to (Array.length row) - 1 do
       accu := Util.int_max !accu ( matrix_ioa_max row.(j) )
      done
    done ;
    !accu
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_max." ;;


(** {v matrix_ioa_min matrix v} Inner blocks reduced to the integer 0 are accepted.

Des blocs réduits à l'entier 0 sont tolérés. *)
let rec matrix_ioa_min = function (m:int_or_array) ->
 match m with
 | Int_cons x -> x
 | Int_matrix_cons u -> matrix_int_min u
 | Ioa_matrix_cons w ->
  begin
   let accu = ref max_int in
    for i = 0 to (Array.length w) - 1 do
     let row = w.(i) in
      for j = 0 to (Array.length row) - 1 do
       accu := min !accu ( matrix_ioa_min row.(j) )
      done
    done ;
    !accu
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_min." ;;


(** {v vector_ioa_find_last element vector v} vector_ioa_find_last returns \[-1\] if it does not find:

vector_ioa_find_last retourne \[-1\] s'il ne trouve pas. *)
let rec vector_ioa_find_last = fun (x:int) (v:int_or_array) ->
 match (ioa_thickness v) with
 | 0 -> [ vector_int_find_last x (vector_int_demakeup v) ]
 | _ ->
  let w = vector_ioa_demakeup v in
   let r = Array.length w
   and index = ref (-1) 
   and suite = ref [] in
    let i = ref (r - 1) in
     while !i >= 0 do
      let essaisuite = vector_ioa_find_last x w.(!i) in
       if (List.hd essaisuite <> -1) then 
        begin
         index:= !i ;
         suite := essaisuite ;
         i := -1 ;
        end
       else i := !i - 1 
     done ;
     !index::!suite ;;


(** {v vector_ioa_find_first element vector v} vector_ioa_find_first returns \[-1\] if it does not find:

vector_ioa_find_first retourne \[-1\] s'il ne trouve pas. *)
let rec vector_ioa_find_first = fun (x:int) (v:int_or_array) ->
 match (ioa_thickness v) with
 | 0 -> [ vector_int_find_first x (vector_int_demakeup v) ]
 | _ ->
 let w = vector_ioa_demakeup v in
  let r = Array.length w
  and index = ref (-1) 
  and suite = ref [] 
  and i = ref 0 in
   while  !i < r do
    let essaisuite = vector_ioa_find_first x w.(!i) in
     if (List.hd essaisuite <> -1) then 
      begin
       index := !i ;
       suite := essaisuite ;
       i := r ;
      end
     else i := !i + 1 ;
   done ;
  !index::!suite ;;


(** {v vector_ioa_extract_element position vector v} vector_ioa_extract_element walks 
along the layers following the list of indexes up to the desired depth.

vector_ioa_extract_element parcourt les couches en suivant la liste des indices jusqu'à la profondeur souhaitée. *)
let rec vector_ioa_extract_element = fun (pos:int list) (v:int_or_array) ->
 match ( List.length pos ) with
 | 0 -> failwith "List too short in Matrix.vector_ioa_extract_element." 
 | 1 ->
  begin
   match (ioa_thickness v) with
   | 0 -> Int_cons (vector_int_demakeup v).(List.hd pos)
   | _ -> (vector_ioa_demakeup v).(List.hd pos) 
  end
 | _ -> vector_ioa_extract_element (List.tl pos) (vector_ioa_demakeup v).(List.hd pos) ;;


(** {v matrix_ioa_extract_element position matrix v} matrix_ioa_extract_element walks 
along the layers following the list of indexes up to the desired depth.

matrix_ioa_extract_element parcourt les couches en suivant la liste des indices jusqu'à la profondeur souhaitée. *)
let rec matrix_ioa_extract_element = fun (pos:int array list) (m:int_or_array) ->
 match ( List.length pos ) with
 | 0 -> failwith "List too short in Matrix.matrix_ioa_extract_element." 
 | 1 ->
  let index = List.hd pos in
   begin
    match (ioa_thickness m) with
    | 0 -> Int_cons (matrix_int_demakeup m).(index.(0)).(index.(1))
    | _ -> (matrix_ioa_demakeup m).(index.(0)).(index.(1))
   end
 | _ ->
  let index = List.hd pos in
   matrix_ioa_extract_element (List.tl pos) (matrix_ioa_demakeup m).(index.(0)).(index.(1)) ;;


(** {v matrix_ioa_extract_column column matrix v} matrix_ioa_extract_column works on the superficial layer.

matrix_ioa_extract_column travaille sur la couche superficielle. *)
let matrix_ioa_extract_column = fun (i:int) (m:int_or_array) ->
 match (ioa_thickness m) with
  | 0 ->
   let col = extract_column i (matrix_int_demakeup m) in
    Int_vector_cons col
  | _ ->
   let col = extract_column i (matrix_ioa_demakeup m) in
    Ioa_vector_cons col ;;


(** {v matrix_ioa_extract_row row matrix v} matrix_ioa_extract_row works on the superficial layer.

matrix_ioa_extract_row travaille sur la couche superficielle. *)
let matrix_ioa_extract_row = fun (i:int) (m:int_or_array) ->
 match (ioa_thickness m) with
  | 0 ->
   Int_vector_cons ( (matrix_int_demakeup m).(i) )
  | _ ->
   Ioa_vector_cons ( (matrix_ioa_demakeup m).(i) ) ;;


(** {v matrix_ioa_size matrix v} matrix_ioa_size works on the superficial layer. 
Inner blocks reduced to the integer 0 are accepted.

matrix_ioa_size travaille sur la couche superficielle.
Des blocs réduits à l'entier 0 sont tolérés. *)
let matrix_ioa_size = function (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_vector_cons [| 1 ; 1 |]
 | Int_matrix_cons mm -> Int_vector_cons ( [| Array.length mm ; numcolumns mm |] )
 | _ -> let mm = matrix_ioa_demakeup m in
   Int_vector_cons ( [| Array.length mm ; numcolumns mm |] ) ;;


(** {v matrix_ioa_rec_size depth matrix v} matrix_ioa_rec_size walks 
along the layers up to the desired depth.
Inner blocks reduced to the integer 0 are accepted.

matrix_ioa_rec_size parcourt les couches jusqu'à la profondeur souhaitée.
Des blocs réduits à l'entier 0 sont tolérés. *)
let rec matrix_ioa_rec_size = fun (d:int) (m:int_or_array) ->
 match d with
 | 0 -> matrix_ioa_size m
 | _ ->
  begin
   match m with
    | Int_cons x -> Int_vector_cons [| 1 ; 1 |]
    | Int_matrix_cons x -> matrix_ioa_size m
    | _ -> let mm = matrix_ioa_demakeup m in
     let r = Array.length mm 
     and c = numcolumns mm in
      let m_m = Array.make_matrix r c (Int_cons 0)
      and cc = c - 1 in 
       for i = 0 to r - 1 do
        let row_input = mm.(i)
        and row_output = m_m.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_ioa_rec_size ( d - 1 ) row_input.(j)
         done
       done ;
       Ioa_matrix_cons m_m
  end ;;


(** {v vector_ioa_affect index element vector v} vector_ioa_affect works on the superficial layer.

vector_ioa_affect travaille sur la couche superficielle. *)
let vector_ioa_affect = fun (index:int) (x:int_or_array) (v:int_or_array) ->
 match (ioa_thickness v) with
 | 0 ->
  let vv = vector_int_demakeup v in
   vv.(index) <- int_demakeup x ;
   Int_vector_cons vv
 | _ ->
  let vv = vector_ioa_demakeup v in
   vv.(index) <- x ;
   Ioa_vector_cons vv ;;


(** {v vector_ioa_affect_element position element vector v} vector_ioa_affect_element walks 
along the layers following the list of indexes up to the desired depth.

vector_ioa_affect_element parcourt les couches en suivant la liste des indices jusqu'à la profondeur souhaitée. *)
let rec vector_ioa_affect_element = fun (position:int list) (x:int_or_array) (v:int_or_array) ->
 match ( List.length position ) with
 | 0 -> failwith "List too short in Matrix.vector_ioa_affect_element."
 | 1 -> vector_ioa_affect (List.hd position) x v
 | _ ->
  let vv = vector_ioa_demakeup v
  and pos = List.hd position in
   vv.(pos) <- vector_ioa_affect_element (List.tl position) x vv.(pos) ;
   Ioa_vector_cons vv ;;


(** {v matrix_ioa_affect index element matrix v} matrix_ioa_affect works on the superficial layer.

matrix_ioa_affect travaille sur la couche superficielle. *)
let matrix_ioa_affect = fun (index:int array) (x:int_or_array) (m:int_or_array) ->
 match (ioa_thickness m) with
 | 0 ->
  let mm = matrix_int_demakeup m in
   mm.(index.(0)).(index.(1)) <- int_demakeup x ;
   Int_matrix_cons mm
 | _ ->
  let mm = matrix_ioa_demakeup m in
   mm.(index.(0)).(index.(1)) <- x ;
   Ioa_matrix_cons mm ;;


(** {v matrix_ioa_affect_element position element matrix v} matrix_ioa_affect_element walks 
along the layers following the list of indexes up to the desired depth.

matrix_ioa_affect_element parcourt les couches en suivant la liste des indices jusqu'à la profondeur souhaitée.*)
let rec matrix_ioa_affect_element = fun (position:int array list) (x:int_or_array) (m:int_or_array) ->
 match ( List.length position ) with
 | 0 -> failwith "List too short in Matrix.matrix_ioa_affect_element."
 | 1 -> matrix_ioa_affect (List.hd position) x m
 | _ ->
  let mm = matrix_ioa_demakeup m
  and pos = List.hd position in
   mm.( pos.(0) ).( pos.(1) ) <- matrix_ioa_affect_element (List.tl position) x mm.( pos.(0) ).( pos.(1) ) ;
   Ioa_matrix_cons mm ;;


(** {v matrix_ioa_affect_row index vector matrix v} matrix_ioa_affect_row works on the superficial layer.
The size of the vector may be bigger than the number of rows of the matrix.

matrix_ioa_affect_row travaille sur la couche superficielle. 
La taille du vecteur peut dépassr le nombre de lignes de la matrice. *)
let matrix_ioa_affect_row = fun (index:int) (v:int_or_array) (m:int_or_array) ->
 match (ioa_thickness m) with
 | 0 ->
  let mm = matrix_int_demakeup m in
   mm.(index) <- vector_int_demakeup v ;
   Int_matrix_cons mm
 | _ ->
  let mm = matrix_ioa_demakeup m in
   mm.(index) <- vector_ioa_demakeup v ;
   Ioa_matrix_cons mm ;;


(** {v matrix_ioa_affect_column index vector matrix v} matrix_ioa_affect_column works on the superficial layer.
The size of the vector may be bigger than the number of rows of the matrix.

matrix_ioa_affect_column travaille sur la couche superficielle.
La taille du vecteur peut dépassr le nombre de lignes de la matrice. *)
let matrix_ioa_affect_column = fun (index:int) (v:int_or_array) (m:int_or_array) ->
 match (ioa_thickness m) with
 | 0 ->
  let mm = matrix_int_demakeup m in
   affect_column index (vector_int_demakeup v) mm ;
   Int_matrix_cons mm
 | _ ->
  let mm = matrix_ioa_demakeup m in
   let r = Array.length mm in
    for i = 0 to r - 1 do
     mm.(i).(index) <- ( vector_ioa_demakeup v ).(i)
    done ;
    Ioa_matrix_cons mm ;;


(** {v matrix_ioa_find_last element matrix v} matrix_ioa_find_last returns \[\[|-1;-1|\]\] if it does not find:

matrix_ioa_find_last retourne \[\[|-1;-1|\]\] s'il ne trouve pas. *)
let rec matrix_ioa_find_last = fun (x:int) (m:int_or_array) ->
 match (ioa_thickness m) with
 | 0 -> [ matrix_int_find_last x (matrix_int_demakeup m) ]
 | _ ->
  let mm = matrix_ioa_demakeup m in
   let r = Array.length mm
   and c = numcolumns mm
   and index = ref ([|-1;-1|]) 
   and suite = ref [] in
    let i = ref ( r - 1 )
    and j = ref ( c - 1 ) in
     while !i >= 0 do
      let row_input = mm.(!i) in
       while !j >= 0 do
        let essaisuite = matrix_ioa_find_last x ( row_input.(!j) ) in
         if ( List.hd essaisuite <> [|-1;-1|] ) then 
          begin
           index := [|!i;!j|] ;
           suite := essaisuite ;
           j := -1 ;
           i := -1 ;
          end
        else j := !j - 1 ;
       done ;
       if ( !index = [|-1;-1|] ) then i := !i - 1 ;
      done ;
      !index::!suite ;;


(** {v matrix_ioa_find_first element matrix v} matrix_ioa_find_first returns \[\[|-1;-1|\]\] if it does not find:

matrix_ioa_find_first retourne \[\[|-1;-1|\]\] s'il ne trouve pas. *)
let rec matrix_ioa_find_first = fun (x:int) (m:int_or_array) ->
 match (ioa_thickness m) with
 | 0 -> [ matrix_int_find_first x (matrix_int_demakeup m) ]
 | _ ->
  let mm = matrix_ioa_demakeup m in
   let r = Array.length mm
   and c = numcolumns mm
   and index = ref ([|-1;-1|]) 
   and suite = ref []
   and i = ref 0 
   and j = ref 0 in
    while !i < r do
     let row_input = mm.(!i) in
      while !j < c do 
       let essaisuite = matrix_ioa_find_first x ( row_input.(!j) ) in
        if ( List.hd essaisuite <> [|-1;-1|] ) then 
         begin
          index := [|!i;!j|] ;
          suite := essaisuite ;
          j := c ;
          i := r ;
         end
        else j := !j + 1 ;
      done ;
      if ( !index = [|-1;-1|] ) then i := !i + 1 ;
     done ;
     !index::!suite ;;


(** {v vector_ioa_print vector v} *)
let rec vector_ioa_print = function (v:int_or_array) ->
 match (ioa_thickness v) with
 | 0 -> bare_vector_int_print (vector_int_demakeup v) 
 | _ -> let w = vector_ioa_demakeup v in
  let rr = (Array.length w) - 1 in
   print_string "[| " ;
   vector_ioa_print w.(0) ;
   print_string " ; " ;
   for i = 1 to ( rr - 1 ) do
    vector_ioa_print w.(i) ; print_string " ; "
   done ;
   vector_ioa_print w.(rr) ;
  print_string "|]" ;;


(** {v matrix_ioa_print matrix v} *)
let rec matrix_ioa_print = function (m:int_or_array) ->
 match (ioa_thickness m) with
 | 0 -> matrix_int_print (matrix_int_demakeup m) 
 | _ -> let w = matrix_ioa_demakeup m in
  let r = Array.length w
  and cc = (numcolumns w) - 1 in
   print_string "[| with " ;
   print_int r ;
   print_string " rows" ;
   print_newline () ;
   for i = 0 to r - 1 do
    let row = w.(i) in
     for j = 0 to cc do
      matrix_ioa_print row.(j)  
     done ;
   done ;
   print_string "|] and " ;
   print_int (cc + 1) ;
   print_string " columns" ;
   print_newline () ;;




(** {C § } *)
(**
{2 {C Calcul élémentaire sur les matrices par blocs
---
Elementary calculus on block matrices} }
*)
(** {C  } *)




(** {v matrix_foa_scal_add coefficient matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_scal_add = fun (lambda:float) (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons ( lambda +. x )
 | Float_matrix_cons u -> Float_matrix_cons ( matrix_float_scal_add lambda u )
 | Foa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_foa_scal_add lambda row_input.(j) 
       done
     done ;
     Foa_matrix_cons mmm
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_scal_add." ;;


(** {v matrix_foa_scal_mult coefficient matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_scal_mult = fun (lambda:float) (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons ( lambda *. x )
 | Float_matrix_cons u -> Float_matrix_cons ( matrix_float_scal_mult lambda u )
 | Foa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_foa_scal_mult lambda row_input.(j) 
       done
     done ;
     Foa_matrix_cons mmm
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_scal_mult." ;;


(** {v matrix_foa_scal_left_div coefficient matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_scal_left_div = fun (lambda:float) (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons ( x /. lambda )
 | Float_matrix_cons u -> Float_matrix_cons ( matrix_float_scal_left_div lambda u )
 | Foa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_foa_scal_left_div lambda row_input.(j) 
       done
     done ;
     Foa_matrix_cons mmm
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_scal_left_div." ;;


(** {v matrix_foa_scal_right_div coefficient matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_scal_right_div = fun (lambda:float) (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons ( lambda /. x )
 | Float_matrix_cons u -> Float_matrix_cons ( matrix_float_scal_right_div lambda u )
 | Foa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_foa_scal_right_div lambda row_input.(j) 
       done
     done ;
     Foa_matrix_cons mmm
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_scal_right_div." ;;


(** {v matrix_foa_scal_right_sub coefficient matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_scal_right_sub = fun (lambda:float) (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons ( x -. lambda )
 | Float_matrix_cons u -> Float_matrix_cons ( matrix_float_scal_right_sub lambda u )
 | Foa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_foa_scal_right_sub lambda row_input.(j) 
       done
     done ;
     Foa_matrix_cons mmm
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_scal_right_sub." ;;


(** {v matrix_foa_scal_left_sub coefficient matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_scal_left_sub = fun (lambda:float) (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons ( lambda -. x )
 | Float_matrix_cons u -> Float_matrix_cons ( matrix_float_scal_left_sub lambda u )
 | Foa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Float_cons 0.) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_foa_scal_left_sub lambda row_input.(j) 
       done
     done ;
     Foa_matrix_cons mmm
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_scal_left_sub." ;;


(** {v matrix_foa_opp matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let matrix_foa_opp = function m ->
 matrix_foa_scal_right_sub 0. m ;;


(** {v matrix_foa_plus matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_plus = fun (m:float_or_array) (mm:float_or_array) ->
 match m with
 | Float_cons x -> matrix_foa_scal_add x mm
 | Float_matrix_cons u ->
  begin
   match mm with 
   | Float_cons y -> Float_matrix_cons ( matrix_float_scal_add y u )
   | _ -> Float_matrix_cons ( matrix_float_plus u ( matrix_float_demakeup mm ) )
  end
 | Foa_matrix_cons w ->
  begin
   match mm with
   | Float_cons y -> matrix_foa_scal_add y m
   | Foa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Float_cons 0.) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_foa_plus row_left.(j) row_right.(j)
         done
       done ;
       Foa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_foa_plus."
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_plus." ;;


(** {v matrix_foa_minus matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_minus = fun (m:float_or_array) (mm:float_or_array) ->
 match m with
 | Float_cons x -> matrix_foa_scal_right_sub x mm
 | Float_matrix_cons u ->
  begin
   match mm with 
   | Float_cons y -> Float_matrix_cons ( matrix_float_scal_left_sub y u )
   | _ -> Float_matrix_cons ( matrix_float_minus u ( matrix_float_demakeup mm ) )
  end
 | Foa_matrix_cons w ->
  begin
   match mm with
   | Float_cons y -> matrix_foa_scal_left_sub y m
   | Foa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Float_cons 0.) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_foa_minus row_left.(j) row_right.(j)
         done
       done ;
       Foa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_foa_minus."
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_minus." ;;


(** {v matrix_foa_coeff_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_coeff_prod = fun (m:float_or_array) (mm:float_or_array) ->
 match m with
 | Float_cons x -> matrix_foa_scal_mult x mm
 | Float_matrix_cons u ->
  begin
   match mm with 
   | Float_cons y -> Float_matrix_cons ( matrix_float_scal_mult y u )
   | _ -> Float_matrix_cons ( matrix_float_coeff_prod u ( matrix_float_demakeup mm ) )
  end
 | Foa_matrix_cons w ->
  begin
   match mm with
   | Float_cons y -> matrix_foa_scal_mult y m
   | Foa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Float_cons 0.) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_foa_coeff_prod row_left.(j) row_right.(j)
         done
       done ;
       Foa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_foa_coeff_prod."
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_coeff_prod." ;;


(** {v matrix_foa_coeff_div matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_coeff_div = fun (m:float_or_array) (mm:float_or_array) ->
 match m with
 | Float_cons x -> matrix_foa_scal_right_div x mm
 | Float_matrix_cons u ->
  begin
   match mm with 
   | Float_cons y -> Float_matrix_cons ( matrix_float_scal_left_div y u )
   | _ -> Float_matrix_cons ( matrix_float_coeff_div u ( matrix_float_demakeup mm ) )
  end
 | Foa_matrix_cons w ->
  begin
   match mm with
   | Float_cons y -> matrix_foa_scal_left_div y m
   | Foa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Float_cons 0.) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_foa_coeff_div row_left.(j) row_right.(j)
         done
       done ;
       Foa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_foa_coeff_div."
  end
 | _ -> failwith "Not a float_or_array matrix in Matrix.matrix_foa_coeff_div." ;;


(** {v foa_transpose matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec foa_transpose = function (m:float_or_array) ->
 match m with
 | Float_cons x -> m
 | Float_matrix_cons w -> Float_matrix_cons ( float_transpose w )
 | _ -> let w = matrix_foa_demakeup m in
  let r = Array.length w
  and cc = (numcolumns w) - 1 in
   let mmm = Array.make_matrix (cc + 1) r (Float_cons 0.) in
    for i = 0 to r - 1 do
     let row_input = w.(i) in
      for j = 0 to cc do
       mmm.(j).(i) <- foa_transpose row_input.(j) 
      done
    done ;
    Foa_matrix_cons mmm ;;


(** {v foa_trace matrix v} foa_trace works on the superficial layer.
Inner blocks reduced to a real number are accepted.

foa_trace travaille sur la couche superficielle.
Des blocs réduits à un réel sont tolérés.  *)
let foa_trace = function (m:float_or_array) ->
 match m with
 | Float_cons x -> m
 | Float_matrix_cons w -> Float_cons ( float_trace w )
 | _ -> let w = matrix_foa_demakeup m in
  let r = min (Array.length w) (numcolumns w)
  and accumulateur = ref (Float_cons 0.) in
   for i = 0 to r - 1 do
    accumulateur := matrix_foa_plus !accumulateur w.(i).(i)    
   done ;
   !accumulateur ;;


(** {v foa_rec_trace matrix v} foa_rec_trace walks along all the layers. 
Inner blocks reduced to a real number are accepted.

foa_rec_trace parcourt toutes les couches.
Des blocs réduits à un réel sont tolérés. *)
let rec foa_rec_trace = function (m:float_or_array) ->
 match m with
 | Float_cons x -> x
 | Float_matrix_cons w -> float_trace w
 | _ -> let w = matrix_foa_demakeup m in
  let r = min (Array.length w) (numcolumns w)
  and accumulateur = ref 0. in
   for i = 0 to r - 1 do
    accumulateur := !accumulateur +. foa_rec_trace w.(i).(i)    
   done ;
   !accumulateur ;;


(** {v line_foa_plus line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let line_foa_plus = fun (s:float_or_array array) (t:float_or_array array) ->
 let r = Array.length s in
  let m = Array.make r (Float_cons 0.) in
   for i = 0 to r - 1 do
    m.(i) <- matrix_foa_plus s.(i) t.(i)
   done ;
   m ;;


(** {v line_foa_minus line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let line_foa_minus = fun (s:float_or_array array) (t:float_or_array array) ->
 let r = Array.length s in
  let m = Array.make r (Float_cons 0.) in
   for i = 0 to r - 1 do
    m.(i) <- matrix_foa_minus s.(i) t.(i)
   done ;
   m ;;


(** {v partial_foa_plus beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_foa_plus = fun (i:int) (j:int) (s:float_or_array array) (t:float_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_plus s.(k) t.(k)
   done ;
   m ;;


(** {v partial_foa_minus beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_foa_minus = fun (i:int) (j:int) (s:float_or_array array) (t:float_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_minus s.(k) t.(k)
   done ;
   m ;;


(** {v partial_foa_coeff_prod beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_foa_coeff_prod = fun (i:int) (j:int) (s:float_or_array array) (t:float_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_coeff_prod s.(k) t.(k)
   done ;
   m ;;


(** {v partial_foa_coeff_div beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_foa_coeff_div = fun (i:int) (j:int) (s:float_or_array array) (t:float_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_coeff_div s.(k) t.(k)
   done ;
   m ;;


(** {v foa_sym matrix v} *)
let foa_sym = function (m:float_or_array) ->
 matrix_foa_scal_mult 0.5 ( matrix_foa_plus (foa_transpose m) m ) ;;

(** {v foa_antisym matrix v} *)
let foa_antisym = function (m:float_or_array) ->
 matrix_foa_scal_mult 0.5 ( matrix_foa_minus m (foa_transpose m) ) ;;



(** {v matrix_ioa_scal_add coefficient matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_scal_add = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons ( lambda + x )
 | Int_matrix_cons u -> Int_matrix_cons ( matrix_int_scal_add lambda u )
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_ioa_scal_add lambda row_input.(j) 
       done
     done ;
     Ioa_matrix_cons mmm
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_scal_add." ;;


(** {v matrix_ioa_scal_mult coefficient matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_scal_mult = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons ( lambda * x )
 | Int_matrix_cons u -> Int_matrix_cons ( matrix_int_scal_mult lambda u )
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_ioa_scal_mult lambda row_input.(j) 
       done
     done ;
     Ioa_matrix_cons mmm
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_scal_mult." ;;


(** {v matrix_ioa_scal_right_div coefficient matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_scal_right_div = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons ( x / lambda )
 | Int_matrix_cons u -> Int_matrix_cons ( matrix_int_scal_right_div lambda u )
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_ioa_scal_right_div lambda row_input.(j) 
       done
     done ;
     Ioa_matrix_cons mmm
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_scal_right_div." ;;


(** {v matrix_ioa_scal_left_div coefficient matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_scal_left_div = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons ( lambda / x )
 | Int_matrix_cons u -> Int_matrix_cons ( matrix_int_scal_left_div lambda u )
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_ioa_scal_left_div lambda row_input.(j) 
       done
     done ;
     Ioa_matrix_cons mmm
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_scal_left_div." ;;


(** {v matrix_ioa_scal_right_mod coefficient matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_scal_right_mod = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons ( x mod lambda )
 | Int_matrix_cons u -> Int_matrix_cons ( matrix_int_scal_right_mod lambda u )
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_ioa_scal_right_mod lambda row_input.(j) 
       done
     done ;
     Ioa_matrix_cons mmm
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_scal_right_mod." ;;


(** {v matrix_ioa_scal_left_mod coefficient matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_scal_left_mod = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons ( lambda mod x )
 | Int_matrix_cons u -> Int_matrix_cons ( matrix_int_scal_left_mod lambda u )
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_ioa_scal_left_mod lambda row_input.(j) 
       done
     done ;
     Ioa_matrix_cons mmm
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_scal_left_mod." ;;


(** {v matrix_ioa_scal_right_sub coefficient matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_scal_right_sub = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons ( x - lambda )
 | Int_matrix_cons u -> Int_matrix_cons ( matrix_int_scal_right_sub lambda u )
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_ioa_scal_right_sub lambda row_input.(j) 
       done
     done ;
     Ioa_matrix_cons mmm
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_scal_right_sub." ;;


(** {v matrix_ioa_scal_left_sub coefficient matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_scal_left_sub = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons ( lambda - x )
 | Int_matrix_cons u -> Int_matrix_cons ( matrix_int_scal_left_sub lambda u )
 | Ioa_matrix_cons w ->
  begin
   let r = Array.length w
   and cc = (numcolumns w) - 1 in
    let mmm = Array.make_matrix r (cc + 1) (Int_cons 0) in
     for i = 0 to r - 1 do
      let row_output = mmm.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- matrix_ioa_scal_left_sub lambda row_input.(j) 
       done
     done ;
     Ioa_matrix_cons mmm
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_scal_left_sub." ;;


(** {v matrix_ioa_opp matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let matrix_ioa_opp = function m ->
 matrix_ioa_scal_right_sub 0 m ;;


(** {v matrix_ioa_plus matrix1 matrix2 v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_plus = fun (m:int_or_array) (mm:int_or_array) ->
 match m with
 | Int_cons x -> matrix_ioa_scal_add x mm
 | Int_matrix_cons u ->
  begin
   match mm with 
   | Int_cons y -> Int_matrix_cons ( matrix_int_scal_add y u )
   | _ -> Int_matrix_cons ( matrix_int_plus u ( matrix_int_demakeup mm ) )
  end
 | Ioa_matrix_cons w ->
  begin
   match mm with
   | Int_cons y -> matrix_ioa_scal_add y m
   | Ioa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Int_cons 0) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_ioa_plus row_left.(j) row_right.(j)
         done
       done ;
       Ioa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_ioa_plus."
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_plus." ;;


(** {v matrix_ioa_minus matrix1 matrix2 v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec matrix_ioa_minus = fun (m:int_or_array) (mm:int_or_array) ->
 match m with
 | Int_cons x -> matrix_ioa_scal_right_sub x mm
 | Int_matrix_cons u ->
  begin
   match mm with 
   | Int_cons y -> Int_matrix_cons ( matrix_int_scal_left_sub y u )
   | _ -> Int_matrix_cons ( matrix_int_minus u ( matrix_int_demakeup mm ) )
  end
 | Ioa_matrix_cons w ->
  begin
   match mm with
   | Int_cons y -> matrix_ioa_scal_left_sub y m
   | Ioa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Int_cons 0) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_ioa_minus row_left.(j) row_right.(j)
         done
       done ;
       Ioa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_ioa_minus."
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_minus." ;;


(** {v matrix_ioa_coeff_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_ioa_coeff_prod = fun (m:int_or_array) (mm:int_or_array) ->
 match m with
 | Int_cons x -> matrix_ioa_scal_mult x mm
 | Int_matrix_cons u ->
  begin
   match mm with 
   | Int_cons y -> Int_matrix_cons ( matrix_int_scal_mult y u )
   | _ -> Int_matrix_cons ( matrix_int_coeff_prod u ( matrix_int_demakeup mm ) )
  end
 | Ioa_matrix_cons w ->
  begin
   match mm with
   | Int_cons y -> matrix_ioa_scal_mult y m
   | Ioa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Int_cons 0) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_ioa_coeff_prod row_left.(j) row_right.(j)
         done
       done ;
       Ioa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_ioa_coeff_prod."
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_coeff_prod." ;;


(** {v matrix_ioa_coeff_div matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_ioa_coeff_div = fun (m:int_or_array) (mm:int_or_array) ->
 match m with
 | Int_cons x -> matrix_ioa_scal_right_div x mm
 | Int_matrix_cons u ->
  begin
   match mm with 
   | Int_cons y -> Int_matrix_cons ( matrix_int_scal_left_div y u )
   | _ -> Int_matrix_cons ( matrix_int_coeff_div u ( matrix_int_demakeup mm ) )
  end
 | Ioa_matrix_cons w ->
  begin
   match mm with
   | Int_cons y -> matrix_ioa_scal_left_div y m
   | Ioa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Int_cons 0) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_ioa_coeff_div row_left.(j) row_right.(j)
         done
       done ;
       Ioa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_ioa_coeff_div."
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_coeff_div." ;;


(** {v matrix_ioa_coeff_mod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_ioa_coeff_mod = fun (m:int_or_array) (mm:int_or_array) ->
 match m with
 | Int_cons x -> matrix_ioa_scal_left_mod x mm
 | Int_matrix_cons u ->
  begin
   match mm with 
   | Int_cons y -> Int_matrix_cons ( matrix_int_scal_right_mod y u )
   | _ -> Int_matrix_cons ( matrix_int_coeff_mod u ( matrix_int_demakeup mm ) )
  end
 | Ioa_matrix_cons w ->
  begin
   match mm with
   | Int_cons y -> matrix_ioa_scal_right_mod y m
   | Ioa_matrix_cons ww ->
    begin
     let r = Array.length w
     and cc = (numcolumns w) - 1 in
      let z = Array.make_matrix r (cc + 1) (Int_cons 0) in
       for i = 0 to r - 1 do
        let row_left = w.(i)
        and row_right = ww.(i)
        and row_output = z.(i) in
         for j = 0 to cc do
          row_output.(j) <- matrix_ioa_coeff_mod row_left.(j) row_right.(j)
         done
       done ;
       Ioa_matrix_cons z
    end
   | _ -> failwith "Bad thickness in Matrix.matrix_ioa_coeff_mod."
  end
 | _ -> failwith "Not an int_or_array matrix in Matrix.matrix_ioa_coeff_mod." ;;


(** {v ioa_transpose matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec ioa_transpose = function (m:int_or_array) ->
 match m with
 | Int_cons x -> m
 | Int_matrix_cons w -> Int_matrix_cons ( int_transpose w )
 | _ -> let w = matrix_ioa_demakeup m in
  let r = Array.length w
  and cc = (numcolumns w) - 1 in
   let mmm = Array.make_matrix (cc + 1) r (Int_cons 0) in
    for i = 0 to r - 1 do
     let row_input = w.(i) in
      for j = 0 to cc do
       mmm.(j).(i) <- ioa_transpose row_input.(j) 
      done
     done ;
     Ioa_matrix_cons mmm ;;


(** {v ioa_trace matrix v} ioa_trace works on the superficial layer.
Inner blocks reduced to an integer are accepted.

ioa_trace travaille sur la couche superficielle.
Des blocs réduits à un entier sont tolérés. *)
let ioa_trace = function (m:int_or_array) ->
 match m with
 | Int_cons x -> m
 | Int_matrix_cons w -> Int_cons ( int_trace w )
 | _ -> let w = matrix_ioa_demakeup m in
  let r = min (Array.length w) (numcolumns w)
  and accumulateur = ref (Int_cons 0) in
   for i = 0 to r - 1 do
    accumulateur := matrix_ioa_plus !accumulateur w.(i).(i)    
   done ;
   !accumulateur ;;


(** {v ioa_rec_trace matrix v} ioa_rec_trace walks 
along all the layers.
Inner blocks reduced to an integer are accepted.

ioa_rec_trace parcourt toutes les couches.
Des blocs réduits à un entier sont tolérés. *)
let rec ioa_rec_trace = function m ->
 match m with
 | Int_cons x -> x
 | Int_matrix_cons w  -> int_trace w
 | _ -> let w = matrix_ioa_demakeup m in
  let r = min (Array.length w) (numcolumns w)
  and accumulateur = ref 0 in
   for i = 0 to r - 1 do
    accumulateur := !accumulateur + ioa_rec_trace w.(i).(i)    
   done ;
   !accumulateur ;;


(** {v line_ioa_plus line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let line_ioa_plus = fun (s:int_or_array array) (t:int_or_array array) ->
 let r = Array.length s in
  let m = Array.make r (Int_cons 0) in
   for i = 0 to r - 1 do
    m.(i) <- matrix_ioa_plus s.(i) t.(i)
   done ;
   m ;;


(** {v line_ioa_minus line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let line_ioa_minus = fun (s:int_or_array array) (t:int_or_array array) ->
 let r = Array.length s in
  let m = Array.make r (Int_cons 0) in
   for i = 0 to r - 1 do
    m.(i) <- matrix_ioa_minus s.(i) t.(i)
   done ;
   m ;;


(** {v partial_ioa_plus beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_ioa_plus = fun (i:int) (j:int) (s:int_or_array array) (t:int_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Int_cons 0) in
   for k = i to j do
    m.(k) <- matrix_ioa_plus s.(k) t.(k)
   done ;
   m ;;


(** {v partial_ioa_minus beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_ioa_minus = fun (i:int) (j:int) (s:int_or_array array) (t:int_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Int_cons 0) in
   for k = i to j do
    m.(k) <- matrix_ioa_minus s.(k) t.(k)
   done ;
   m ;;


(** {v partial_ioa_coeff_prod beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_ioa_coeff_prod = fun (i:int) (j:int) (s:int_or_array array) (t:int_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Int_cons 0) in
   for k = i to j do
    m.(k) <- matrix_ioa_coeff_prod s.(k) t.(k)
   done ;
   m ;;


(** {v partial_ioa_coeff_div beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_ioa_coeff_div = fun (i:int) (j:int) (s:int_or_array array) (t:int_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Int_cons 0) in
   for k = i to j do
    m.(k) <- matrix_ioa_coeff_div s.(k) t.(k)
   done ;
   m ;;


(** {v partial_ioa_coeff_mod beginning end line1 line2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_ioa_coeff_mod = fun (i:int) (j:int) (s:int_or_array array) (t:int_or_array array) ->
 let c = min (Array.length s) (Array.length t) in
  let m = Array.make c (Int_cons 0) in
   for k = i to j do
    m.(k) <- matrix_ioa_coeff_mod s.(k) t.(k)
   done ;
   m ;;




(** {C § } *)
(**
{2 {C Construction de matrices par blocs
---
Construction of block matrices} }
*)
(** {C  } *)




(** {v zeros_foa matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let zeros_foa = function (m:float_or_array) ->
 matrix_foa_scal_mult 0. m ;;


(** {v null_foa numrows numcolumns v} *)
let null_foa = fun (r:int) (c:int) ->
 Foa_matrix_cons ( Array.make_matrix r c (Float_cons 0.) ) ;;


(** {v identity_foa numrows numcolumns v} *)
let identity_foa = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c (Float_cons 0.) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).(i) <- Float_cons 1.
  done ;
  Foa_matrix_cons m ;;


(** {v eye_foa matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec eye_foa = function (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons 1.
 | Float_matrix_cons w -> Float_matrix_cons ( identity_float (Array.length w) (Array.length w.(0)) )
 | _ ->
  begin
   let r = foa_numrows m
   and c = foa_numcolumns m
   and mmm = matrix_foa_demakeup m in
    let mm = Array.make_matrix r c (Float_cons 0.) in
     for i = 0 to r - 1 do
      mm.(i).(i) <- eye_foa mmm.(i).(i)
     done ;
     Foa_matrix_cons mm
  end ;;


(** {v scal_foa real matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec scal_foa = fun (lambda:float) (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons lambda
 | Float_matrix_cons w -> Float_matrix_cons ( scal_float (Array.length w) (Array.length w.(0)) lambda )
 | _ ->
  begin
   let r = foa_numrows m
   and c = foa_numcolumns m
   and w = matrix_foa_demakeup m in
    let mm = Array.make_matrix r c (Float_cons 0.) in
     for i = 0 to r - 1 do
      mm.(i).(i) <- scal_foa lambda w.(i).(i)
     done ;
     Foa_matrix_cons mm
  end ;;


(** {v diag_foa line v} *)
let diag_foa = function (v:float_or_array array) ->
 let r = Array.length v in
  let w = Array.make_matrix r r (Float_cons 0.) in
   for i = 0 to r - 1 do
    w.(i).(i) <- v.(i)
   done ;
   Foa_matrix_cons w ;;


(** {v matrix_foa_permu size index1 index2 v} *)
let matrix_foa_permu = fun (n:int) (i:int) (j:int) ->
 let w = Array.make_matrix n n (Float_cons 0.)
 and ii = min i j
 and jj = Util.int_max i j in
  for k = 0 to ii - 1 do
   w.(k).(k) <- Float_cons 1.
  done ;
  w.(ii).(jj) <- Float_cons 1. ;
  for k = ii + 1 to jj - 1 do
   w.(k).(k) <- Float_cons 1.
  done ;
  w.(jj).(ii) <- Float_cons 1. ;
  for k = jj + 1 to n do
   w.(k).(k) <- Float_cons 1.
  done ;
  Foa_matrix_cons w ;;


(** {v oblique_foa numrows numcolumns v} *)
let oblique_foa = fun (r:int) (c:int) ->
 let w = Array.make_matrix r c (Float_cons 0.)
 and s = ( min r c ) - 1 in
  for i = 0 to s do
   w.(i).(s - i) <- Float_cons 1.
  done ;
  Foa_matrix_cons w ;;


(** {v antiscal_foa numrows numcolumns v} *)
let antiscal_foa = fun (r:int) (c:int) (x:float) ->
 let w = Array.make_matrix r c (Float_cons 0.)
 and s = ( min r c ) - 1 in
  for i = 0 to s do
   w.(i).(s - i) <- Float_cons x
  done ;
  Foa_matrix_cons w ;;


(** {v antidiag_foa line v} *)
let antidiag_foa = function (v:float_or_array array) ->
 let r = Array.length v in
  let w = Array.make_matrix r r (Float_cons 0.) in
   for i = 0 to r - 1 do
    w.(i).(r - 1 - i) <- v.(i)
   done ;
   Foa_matrix_cons w ;;


(** {v gen_sympl_foa v} *)
let gen_sympl_foa =
 Foa_matrix_cons [| [| Float_cons 0. ; Float_cons (-1.) |] ; [| Float_cons 1. ; Float_cons 0. |] |] ;;



(** {v zeros_ioa matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let zeros_ioa = function m ->
 matrix_ioa_scal_mult 0 m ;;


(** {v null_ioa numrows numcolumns v} *)
let null_ioa = fun (r:int) (c:int) ->
 Ioa_matrix_cons ( Array.make_matrix r c (Int_cons 0) ) ;;


(** {v identity_ioa numrows numcolumns v} *)
let identity_ioa = fun (r:int) (c:int) ->
 let m = Array.make_matrix r c (Int_cons 0) in
  for i = 0 to ( min r c ) - 1 do
   m.(i).(i) <- Int_cons 1
  done ;
  Ioa_matrix_cons m ;;


(** {v eye_ioa matrix v} Inner blocks reduced to an integer are accepted.

Des blocs réduits à un entier sont tolérés. *)
let rec eye_ioa = function m ->
 match m with
 | Int_cons x -> Int_cons 1
 | Int_matrix_cons w -> Int_matrix_cons ( identity_int (Array.length w) (Array.length w.(0)) )
 | _ ->
  begin
   let r = ioa_numrows m 
   and c = ioa_numcolumns m
   and mmm = matrix_ioa_demakeup m in
    let mm = Array.make_matrix r c (Int_cons 0) in
     for i = 0 to r - 1 do
      mm.(i).(i) <- eye_ioa mmm.(i).(i)
     done ;
     Ioa_matrix_cons mm
  end ;;


(** {v scal_ioa real matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec scal_ioa = fun (lambda:int) (m:int_or_array) ->
 match m with
 | Int_cons x -> Int_cons lambda
 | Int_matrix_cons w -> Int_matrix_cons ( scal_int (Array.length w) (Array.length w.(0)) lambda )
 | _ ->
  begin
   let r = ioa_numrows m
   and c = ioa_numcolumns m
   and w = matrix_ioa_demakeup m in
    let mm = Array.make_matrix r c (Int_cons 0) in
     for i = 0 to r - 1 do
      mm.(i).(i) <- scal_ioa lambda w.(i).(i)
     done ;
     Ioa_matrix_cons mm
  end ;;


(** {v diag_ioa line matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let diag_ioa = function (v:int_or_array array) ->
 let r = Array.length v in
  let w = Array.make_matrix r r (Int_cons 0) in
   for i = 0 to r - 1 do
    w.(i).(i) <- v.(i)
   done ;
   Ioa_matrix_cons w ;;


(** {v matrix_ioa_permu size index1 index2 v} *)
let matrix_ioa_permu = fun (n:int) (i:int) (j:int) ->
 let w = Array.make_matrix n n (Int_cons 0)
 and ii = min i j
 and jj = Util.int_max i j in
  for k = 0 to ii - 1 do
   w.(k).(k) <- Int_cons 1
  done ;
  w.(ii).(jj) <- Int_cons 1 ;
  for k = ii + 1 to jj - 1 do
   w.(k).(k) <- Int_cons 1
  done ;
  w.(jj).(ii) <- Int_cons 1 ;
  for k = jj + 1 to n do
   w.(k).(k) <- Int_cons 1
  done ;
  Ioa_matrix_cons w ;;


(** {v oblique_ioa numrows numcolumns v} *)
let oblique_ioa = fun (r:int) (c:int) ->
 let w = Array.make_matrix r c (Int_cons 0)
 and s = ( min r c ) - 1 in
  for i = 0 to s do
   w.(i).(s - i) <- Int_cons 1
  done ;
  Ioa_matrix_cons w ;;


(** {v antiscal_ioa numrows numcolumns v} *)
let antiscal_ioa = fun (r:int) (c:int) (x:int) ->
 let w = Array.make_matrix r c (Int_cons 0)
 and s = ( min r c ) - 1 in
  for i = 0 to s do
   w.(i).(s - i) <- Int_cons x
  done ;
  Ioa_matrix_cons w ;;


(** {v antidiag_ioa line matrix v} *)
let antidiag_ioa = function (v:int_or_array array) ->
 let r = Array.length v in
  let w = Array.make_matrix r r (Int_cons 0) in
   for i = 0 to r - 1 do
    w.(i).(r - 1 - i) <- v.(i)
   done ;
   Ioa_matrix_cons w ;;


(** {v gen_sympl_ioa v} *)
let gen_sympl_ioa =
 Ioa_matrix_cons [| [| Int_cons 0 ; Int_cons (-1) |] ; [| Int_cons 1 ; Int_cons 0 |] |] ;;




(** {C § } *)
(**
{2 {C Découpage récursif des matrices par blocs
---
Recursive cutting of block matrices} }
*)
(** {C  } *)




(** {v hash_threshold number base v} *)
let hash_threshold = fun (n:int) (b:int) ->
 let m = n mod b in
  if m = 0 then 0 else b - m ;;


(** {v square_ioa_hash base matrix sequence v} *)
let rec square_ioa_hash = fun (b:int) (m:int_or_array) (s:int_or_array) ->
 let r = ioa_numrows m
 and c = ioa_numcolumns m in
  let pp = min r c
  and rr = pred r
  and pg = Util.int_max r c in
   match b >= pp with
   | true -> [| matrix_ioa_copy m ; vector_ioa_copy s |]
   | false -> match ioa_thickness m with
    | 0 ->
     begin
      let sequence = ref ( vector_ioa_demakeup s )
      and t = hash_threshold pg b in
       let nouveau = t + pg in
        let morerows = nouveau - r
        and morecolumns = nouveau - c in
         begin
          sequence := Array.append [| Int_vector_cons [| morerows ; morecolumns |] |] !sequence ;
          let mm = ref ( matrix_int_demakeup (matrix_ioa_copy m) ) in
           if morecolumns > 0 then 
            begin
             let terminaison = Array.make morecolumns 0 in
              for i = 0 to rr do
               !mm.(i) <- Array.append !mm.(i) terminaison
              done  
            end ;
           if morerows > 0 then
            begin
             let termin = Array.make_matrix morerows nouveau 0 in
              mm := Array.append !mm termin ; 
              for i = pp to nouveau - 1 do
               !mm.(i).(i) <- 1
              done ;
            end ;
           square_ioa_hash b ( matrix_ioa_cut (nouveau / b) (Int_matrix_cons !mm) ) ( Ioa_vector_cons !sequence )
         end
     end 
    | _ ->
     begin
      let sequence = ref ( vector_ioa_demakeup s )
      and t = hash_threshold pg b in
       let nouveau = t + pg in
        let morerows = nouveau - r
        and morecolumns = nouveau - c in
         begin
          sequence := Array.append [| Int_vector_cons [| morerows ; morecolumns |] |] !sequence ;
          let mm = ref ( matrix_ioa_demakeup m ) in
           if morecolumns > 0 then 
            begin
             let terminaison = Array.make morecolumns (Int_cons 0) in
              for i = 0 to rr do
               !mm.(i) <- Array.append !mm.(i) terminaison
              done  
            end ;
           if morerows > 0 then
            begin
             let termin = Array.make_matrix morerows nouveau (Int_cons 0) in
              mm := Array.append !mm termin ; 
              for i = pp to nouveau - 1 do
               !mm.(i).(i) <- Int_cons 1
              done
            end ;
           square_ioa_hash b ( matrix_ioa_cut (nouveau / b) (Ioa_matrix_cons !mm) ) ( Ioa_vector_cons !sequence )
         end
     end ;;


(** {v square_ioa_crash [| matrix ; sequence |] v} *)
let rec square_ioa_crash = function (data:int_or_array array) ->
 let m = data.(0) and s = vector_ioa_demakeup data.(1) in
  let margin = vector_int_demakeup s.(0) in
   match ioa_thickness m with
   | 0 -> let endrow = (ioa_numrows m) - margin.(0) - 1
    and endcolumn = (ioa_numcolumns m) - margin.(1) - 1
    and t = Array.sub s 1 ( (Array.length s) - 1 ) in
     let matrix = Int_matrix_cons ( int_sub_matrix (matrix_int_demakeup m) 0 endrow 0 endcolumn ) in
      [| matrix ; Ioa_vector_cons t |]
   | 1 -> let mm = matrix_ioa_paste m in
    let endrow = (ioa_numrows mm) - margin.(0) - 1
    and endcolumn = (ioa_numcolumns mm) - margin.(1) - 1
    and t = Array.sub s 1 ( (Array.length s) - 1 ) in
     let matrix = Int_matrix_cons ( int_sub_matrix (matrix_int_demakeup mm) 0 endrow 0 endcolumn ) in
      square_ioa_crash [| matrix ; Ioa_vector_cons t |]
   | _ -> let mm = matrix_ioa_paste m in
    let endrow = (ioa_numrows mm) - margin.(0) - 1
    and endcolumn = (ioa_numcolumns mm) - margin.(1) - 1
    and t = Array.sub s 1 ( (Array.length s) - 1 ) in
     let matrix = Ioa_matrix_cons ( sub_matrix (matrix_ioa_demakeup mm) 0 endrow 0 endcolumn ) in
      square_ioa_crash [| matrix ; Ioa_vector_cons t |] ;;


(** {v square_foa_hash base matrix sequence v} *)
let rec square_foa_hash = fun (b:int) (m:float_or_array) (s:float_or_array) ->
 let r = foa_numrows m
 and c = foa_numcolumns m in
  let pp = min r c
  and rr = pred r
  and pg = Util.int_max r c in
   match b >= pp with
   | true -> [| matrix_foa_copy m ; vector_foa_copy s |]
   | false -> match foa_thickness m with
    | 0 ->
     begin
      let sequence = ref ( vector_foa_demakeup s )
      and t = hash_threshold pg b in
       let nouveau = t + pg in
        let morerows = nouveau - r
        and morecolumns = nouveau - c in
         begin
          sequence := Array.append [| Float_vector_cons [| float morerows ; float morecolumns |] |] !sequence ;
          let mm = ref ( matrix_float_demakeup (matrix_foa_copy m) ) in
           if morecolumns > 0 then 
            begin
             let terminaison = Array.make morecolumns 0. in
              for i = 0 to rr do
               !mm.(i) <- Array.append !mm.(i) terminaison
              done  
            end ;
           if morerows > 0 then
            begin
             let termin = Array.make_matrix morerows nouveau 0. in
              mm := Array.append !mm termin ; 
              for i = pp to nouveau - 1 do
               !mm.(i).(i) <- 1.
              done ;
            end ;
           square_foa_hash b ( matrix_foa_cut (nouveau / b) (Float_matrix_cons !mm) ) ( Foa_vector_cons !sequence )
         end
     end 
    | _ ->
     begin
      let sequence = ref ( vector_foa_demakeup s )
      and t = hash_threshold pg b in
       let nouveau = t + pg in
        let morerows = nouveau - r
        and morecolumns = nouveau - c in
         begin
          sequence := Array.append [| Float_vector_cons [| float morerows ; float morecolumns |] |] !sequence ;
          let mm = ref ( matrix_foa_demakeup m ) in
           if morecolumns > 0 then 
            begin
             let terminaison = Array.make morecolumns (Float_cons 0.) in
              for i = 0 to rr do
               !mm.(i) <- Array.append !mm.(i) terminaison
              done  
            end ;
           if morerows > 0 then
            begin
             let termin = Array.make_matrix morerows nouveau (Float_cons 0.) in
              mm := Array.append !mm termin ; 
              for i = pp to nouveau - 1 do
               !mm.(i).(i) <- Float_cons 1.
              done
            end ;
           square_foa_hash b ( matrix_foa_cut (nouveau / b) (Foa_matrix_cons !mm) ) ( Foa_vector_cons !sequence )
         end
     end ;;


(** {v square_foa_crash [| matrix ; sequence |] v} *)
let rec square_foa_crash = function (data:float_or_array array) ->
 let m = data.(0) and s = vector_foa_demakeup data.(1) in
  let margin = vector_float_demakeup s.(0) in
   match foa_thickness m with
   | 0 -> let endrow = (foa_numrows m) - (int_of_float margin.(0)) - 1
    and endcolumn = (foa_numcolumns m) - (int_of_float margin.(1)) - 1
    and t = Array.sub s 1 ( (Array.length s) - 1 ) in
     let matrix = Float_matrix_cons ( float_sub_matrix (matrix_float_demakeup m) 0 endrow 0 endcolumn ) in
      [| matrix ; Foa_vector_cons t |]
   | 1 -> let mm = matrix_foa_paste m in
    let endrow = (foa_numrows mm) - (int_of_float margin.(0)) - 1
    and endcolumn = (foa_numcolumns mm) - (int_of_float margin.(1)) - 1
    and t = Array.sub s 1 ( (Array.length s) - 1 ) in
     let matrix = Float_matrix_cons ( float_sub_matrix (matrix_float_demakeup mm) 0 endrow 0 endcolumn ) in
      square_foa_crash [| matrix ; Foa_vector_cons t |]
   | _ -> let mm = matrix_foa_paste m in
    let endrow = (foa_numrows mm) - (int_of_float margin.(0)) - 1
    and endcolumn = (foa_numcolumns mm) - (int_of_float margin.(1)) - 1
    and t = Array.sub s 1 ( (Array.length s) - 1 ) in
     let matrix = Foa_matrix_cons ( sub_matrix (matrix_foa_demakeup mm) 0 endrow 0 endcolumn ) in
      square_foa_crash [| matrix ; Foa_vector_cons t |] ;;




(** {v exp_ioa_hash base matrix sequence v} *)
let rec exp_ioa_hash = fun (b:int) (m:int_or_array) (s:int_or_array) ->
 let r = ioa_numrows m
 and c = ioa_numcolumns m in
  let pp = min r c
  and rr = pred r
  and pg = Util.int_max r c in
   match b >= pp with
   | true -> [| matrix_ioa_copy m ; vector_ioa_copy s |]
   | false -> let epaisseur = ioa_thickness m in match epaisseur with
    | 0 ->
     begin
      let sequence = ref ( vector_ioa_demakeup s )
      and t = hash_threshold pg b in
       let nouveau = t + pg in
        let morerows = nouveau - r
        and morecolumns = nouveau - c in
         begin
          sequence := Array.append [| Int_vector_cons [| morerows ; morecolumns |] |] !sequence ;
          let mm = ref ( matrix_int_demakeup (matrix_ioa_copy m) ) in
           if morecolumns > 0 then 
            begin
             let terminaison = Array.make morecolumns (0) in
              for i = 0 to rr do
               !mm.(i) <- Array.append !mm.(i) terminaison
              done  
            end ;
           if morerows > 0 then
            begin
             let termin = Array.make_matrix morerows nouveau (0) in
              mm := Array.append !mm termin ; 
              for i = pp to nouveau - 1 do
               !mm.(i).(i) <- 1
              done ;
            end ;
           exp_ioa_hash b ( matrix_ioa_cut (nouveau / b) (Int_matrix_cons !mm) ) ( Ioa_vector_cons !sequence )
         end
     end 
    | _ ->
     begin
      let sequence = ref ( vector_ioa_demakeup s )
      and t = hash_threshold pg b in
       let nouveau = t + pg in
        let morerows = nouveau - r
        and morecolumns = nouveau - c in
         begin
          sequence := Array.append [| Int_vector_cons [| morerows ; morecolumns |] |] !sequence ;
          let mm = ref ( matrix_ioa_demakeup m ) in
           if morecolumns > 0 then 
            begin
             let terminaison = Array.make morecolumns (Int_cons 0) in
              for i = 0 to rr do
               !mm.(i) <- Array.append !mm.(i) terminaison
              done  
            end ;
           if morerows > 0 then
            begin
             let termin = Array.make_matrix morerows nouveau (Int_cons 0) in
              mm := Array.append !mm termin ; 
              for i = pp to nouveau - 1 do
               !mm.(i).(i) <- Int_cons 1
              done
            end ;
           let fragment = int_of_float ( (float nouveau) /. (float b) ** (float (1 + epaisseur)) ) in
            exp_ioa_hash b ( matrix_ioa_cut (fragment) (Ioa_matrix_cons !mm) ) ( Ioa_vector_cons !sequence )
         end
     end ;;


(** {v exp_foa_hash base matrix sequence v} *)
let rec exp_foa_hash = fun (b:int) (m:float_or_array) (s:float_or_array) ->
 let r = foa_numrows m
 and c = foa_numcolumns m in
  let pp = min r c
  and rr = pred r
  and pg = Util.int_max r c in
   match b >= pp with
   | true -> [| matrix_foa_copy m ; vector_foa_copy s |]
   | false -> let epaisseur = foa_thickness m in match epaisseur with
    | 0 ->
     begin
      let sequence = ref ( vector_foa_demakeup s )
      and t = hash_threshold pg b in
       let nouveau = t + pg in
        let morerows = nouveau - r
        and morecolumns = nouveau - c in
         begin
          sequence := Array.append [| Float_vector_cons [| float morerows ; float morecolumns |] |] !sequence ;
          let mm = ref ( matrix_float_demakeup (matrix_foa_copy m) ) in
           if morecolumns > 0 then 
            begin
             let terminaison = Array.make morecolumns (0.) in
              for i = 0 to rr do
               !mm.(i) <- Array.append !mm.(i) terminaison
              done  
            end ;
           if morerows > 0 then
            begin
             let termin = Array.make_matrix morerows nouveau (0.) in
              mm := Array.append !mm termin ; 
              for i = pp to nouveau - 1 do
               !mm.(i).(i) <- 1.
              done ;
            end ;
           exp_foa_hash b ( matrix_foa_cut (nouveau / b) (Float_matrix_cons !mm) ) ( Foa_vector_cons !sequence )
         end
     end 
    | _ ->
     begin
      let sequence = ref ( vector_foa_demakeup s )
      and t = hash_threshold pg b in
       let nouveau = t + pg in
        let morerows = nouveau - r
        and morecolumns = nouveau - c in
         begin
          sequence := Array.append [| Float_vector_cons [| float morerows ; float morecolumns |] |] !sequence ;
          let mm = ref ( matrix_foa_demakeup m ) in
           if morecolumns > 0 then 
            begin
             let terminaison = Array.make morecolumns (Float_cons 0.) in
              for i = 0 to rr do
               !mm.(i) <- Array.append !mm.(i) terminaison
              done  
            end ;
           if morerows > 0 then
            begin
             let termin = Array.make_matrix morerows nouveau (Float_cons 0.) in
              mm := Array.append !mm termin ; 
              for i = pp to nouveau - 1 do
               !mm.(i).(i) <- Float_cons 1.
              done
            end ;
           let fragment = int_of_float ( (float nouveau) /. (float b) ** (float (1 + epaisseur)) ) in
            exp_foa_hash b ( matrix_foa_cut (fragment) (Foa_matrix_cons !mm) ) ( Foa_vector_cons !sequence )
         end
     end ;;




(** {C § } *)
(**
{2 {C Calcul substantiel sur les matrices par blocs
---
Substantial calculus on block matrices} }
*)
(** {C  } *)




(** {v matrix_foa_twisted_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_twisted_prod = fun (m:float_or_array) (mm:float_or_array) ->
 match ( Util.int_max (foa_thickness m) (foa_thickness mm) ) with
 | 0 ->
  begin (** matrices plates --- flat matrices *)
   match m with
   | Float_cons 0. -> Float_cons 0.
   | Float_cons x ->
    begin
     match mm with
     | Float_cons 0. -> Float_cons 0.
     | Float_cons y -> Float_cons (x *. y)
     | _ -> Float_matrix_cons ( matrix_float_scal_mult x ( float_transpose (matrix_float_demakeup mm) ) )
    end
   | _ ->
    begin
     match mm with
     | Float_cons 0. -> Float_cons 0.
     | Float_cons y -> Float_matrix_cons ( matrix_float_scal_mult y (matrix_float_demakeup m) )
     | _ -> Float_matrix_cons ( matrix_float_twisted_prod (matrix_float_demakeup m) (matrix_float_demakeup mm) )
    end
  end (** matrices plates --- flat matrices *)
 | _ ->
  begin (** matrices épaisses --- thick matrices *)
   match m with
   | Float_cons 0. -> Float_cons 0.
   | Float_cons x -> matrix_foa_scal_mult x mm
   | Float_matrix_cons w ->
    begin (** m est plate --- m is flat *)
     let ww = matrix_foa_demakeup mm in
      let r = - 1 + Array.length w
      and c = - 1 + Array.length ww
      and s = - 1 + numcolumns ww in
       let mmm = Array.make_matrix (r + 1) (c + 1) (Float_cons 0.) in
        for i = 0 to r do
         let row_input = w.(i)
         and row_output = mmm.(i) in
          for j = 0 to c do
           let row_right = ww.(j) in
            row_output.(j) <- matrix_foa_scal_mult row_input.(0) ( foa_transpose row_right.(0) ) ;
            for k = 1 to s do
             row_output.(j) <- matrix_foa_plus row_output.(j) ( matrix_foa_scal_mult row_input.(k) ( foa_transpose row_right.(k) ) )
            done
          done
        done ;
        Foa_matrix_cons mmm
    end (** m est plate --- m is flat *)
   | _ ->
    begin (** m est épaisse --- m is thick *)
     match mm with
     | Float_cons 0. -> Float_cons 0.
     | Float_cons y -> matrix_foa_scal_mult y m
     | Float_matrix_cons ww ->
      begin (** mm est plate --- mm is flat *)
       let w = matrix_foa_demakeup m in
        let r = - 1 + Array.length w
        and c = - 1 + Array.length ww
        and s = - 1 + numcolumns ww in
         let mmm = Array.make_matrix (r + 1) (c + 1) (Float_cons 0.) in
          for i = 0 to r do
           let row_input = w.(i)
           and row_output = mmm.(i) in
            for j = 0 to c do
             let row_right = ww.(j) in
              row_output.(j) <- matrix_foa_scal_mult row_right.(0) row_input.(0) ;
              for k = 1 to s do
               row_output.(j) <- matrix_foa_plus row_output.(j) ( matrix_foa_scal_mult row_right.(k) row_input.(k) )
              done
            done
          done ;
          Foa_matrix_cons mmm
      end (** mm est plate --- mm is flat *)
     |_ ->
      begin (** mm est épaisse --- mm is thick *)
       let w = matrix_foa_demakeup m and ww = matrix_foa_demakeup mm in
        let r = - 1 + Array.length w
        and c = - 1 + Array.length ww
        and s = - 1 + numcolumns ww in
         let mmm = Array.make_matrix (r + 1) (c + 1) (Float_cons 0.) in
          for i = 0 to r do
           let row_input = w.(i)
           and row_output = mmm.(i) in
            for j = 0 to c do
             let row_right = ww.(j) in
              row_output.(j) <- matrix_foa_twisted_prod row_input.(0) row_right.(0) ;
              for k = 1 to s do
               row_output.(j) <- matrix_foa_plus row_output.(j) ( matrix_foa_twisted_prod row_input.(k) row_right.(k) )
              done
            done
          done ;
          Foa_matrix_cons mmm
      end (** mm est épaisse --- mm is thick *)
    end (** m est épaisse --- m is thick *)
  end  (** matrices épaisses --- thick matrices *) ;;


(** {v matrix_foa_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let matrix_foa_prod = fun (m:float_or_array) (mm:float_or_array) ->
 matrix_foa_twisted_prod m ( foa_transpose mm ) ;;

(** {v matrix_foa_triple_prod matrix1 matrix2 v} *)
let matrix_foa_triple_prod = fun (a:float_or_array) (b:float_or_array) (c:float_or_array)->
 matrix_foa_twisted_prod a ( matrix_foa_twisted_prod ( foa_transpose c ) b ) ;;


(** {v matrix_foa_twisted_commut matrix1 matrix2 v} *)
let matrix_foa_twisted_commut = fun (m:float_or_array) (mm:float_or_array) ->
 matrix_foa_minus ( matrix_foa_twisted_prod m mm ) ( matrix_foa_twisted_prod mm m ) ;;

(** {v matrix_foa_twisted_commut_bis matrix1 matrix2 v} *)
let matrix_foa_twisted_commut_bis = fun (m:float_or_array) (mm:float_or_array) ->
 let m_m = foa_transpose m
 and m_mm = foa_transpose mm in
  matrix_foa_minus ( matrix_foa_twisted_prod m mm ) ( matrix_foa_twisted_prod m_mm m_m ) ;;

(** {v matrix_foa_commut matrix1 matrix2 v} *)
let matrix_foa_commut = fun (m:float_or_array) (mm:float_or_array) ->
 matrix_foa_minus ( matrix_foa_prod m mm ) ( matrix_foa_prod mm m ) ;;


(** {v matrix_foa_naive_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_naive_prod = fun (m:float_or_array) (mm:float_or_array) ->
 match ( Util.int_max (foa_thickness m) (foa_thickness mm) ) with
 | 0 ->
  begin (** matrices plates --- flat matrices *)
   match m with
   | Float_cons 0. -> Float_cons 0.
   | Float_cons x ->
    begin
     match mm with
     | Float_cons 0. -> Float_cons 0.
     | Float_cons y -> Float_cons (x *. y)
     | _ -> Float_matrix_cons ( matrix_float_scal_mult x (matrix_float_demakeup mm) )
    end
   | _ ->
    begin
     match mm with
     | Float_cons 0. -> Float_cons 0.
     | Float_cons y -> Float_matrix_cons ( matrix_float_scal_mult y (matrix_float_demakeup m) )
     | _ -> Float_matrix_cons ( matrix_float_prod (matrix_float_demakeup m) (matrix_float_demakeup mm) )
    end
  end (** matrices plates --- flat matrices *)
 | _ ->
  begin (** matrices épaisses --- thick matrices *)
   match m with
   | Float_cons 0. -> Float_cons 0.
   | Float_cons x -> matrix_foa_scal_mult x mm
   | Float_matrix_cons w ->
    begin (** m est plate --- m is flat *)
     let ww = matrix_foa_demakeup mm in
      let r = - 1 + Array.length w
      and s = - 1 + Array.length ww
      and c = - 1 + numcolumns ww in
       let mmm = Array.make_matrix (r + 1) (c + 1) (Float_cons 0.) in
        for i = 0 to r do
         let row_input = w.(i)
         and row_output = mmm.(i) in
          for j = 0 to c do
           row_output.(j) <- matrix_foa_scal_mult row_input.(0) ww.(0).(j) ;
           for k = 1 to s do
            row_output.(j) <- matrix_foa_plus row_output.(j) ( matrix_foa_scal_mult row_input.(k) ww.(k).(j) )
           done
          done
        done ;
        Foa_matrix_cons mmm
    end (** m est plate --- m is flat *)
   | _ ->
    begin (** m est épaisse --- m is thick *)
     match mm with
     | Float_cons 0. -> Float_cons 0.
     | Float_cons y -> matrix_foa_scal_mult y m
     | Float_matrix_cons ww ->
      begin (** mm est plate --- mm is flat *)
       let w = matrix_foa_demakeup m in
        let r = - 1 + Array.length w
        and s = - 1 + Array.length ww
        and c = - 1 + numcolumns ww in
         let mmm = Array.make_matrix (r + 1) (c + 1) (Float_cons 0.) in
          for i = 0 to r do
           let row_input = w.(i)
           and row_output = mmm.(i) in
            for j = 0 to c do
             row_output.(j) <- matrix_foa_scal_mult ww.(0).(j) row_input.(0) ;
             for k = 1 to s do
              row_output.(j) <- matrix_foa_plus row_output.(j) ( matrix_foa_scal_mult ww.(k).(j) row_input.(k) )
             done
            done
          done ;
          Foa_matrix_cons mmm
      end (** mm est plate --- mm is flat *)
     |_ ->
      begin (** mm est épaisse --- mm is thick *)
       let w = matrix_foa_demakeup m and ww = matrix_foa_demakeup mm in
        let r = - 1 + Array.length w
        and s = - 1 + Array.length ww
        and c = - 1 + numcolumns ww in
         let mmm = Array.make_matrix (r + 1) (c + 1) (Float_cons 0.) in
          for i = 0 to r do
           let row_input = w.(i)
           and row_output = mmm.(i) in
            for j = 0 to c do
             row_output.(j) <- matrix_foa_naive_prod row_input.(0) ww.(0).(j) ;
             for k = 1 to s do
              row_output.(j) <- matrix_foa_plus row_output.(j) ( matrix_foa_naive_prod row_input.(k) ww.(k).(j) )
             done
            done
          done ;
          Foa_matrix_cons mmm
      end (** mm est épaisse --- mm is thick *)
    end (** m est épaisse --- m is thick *)
  end  (** matrices épaisses --- thick matrices *) ;;


(** {v matrix_foa_coeff_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_foa_coeff_prod = fun (m:float_or_array) (mm:float_or_array) ->
 match m with
 | Float_cons x -> matrix_foa_scal_mult x m
 | Float_matrix_cons w ->
  begin
   match mm with
   | Float_cons y -> Float_matrix_cons ( matrix_float_scal_mult y w )
   | Float_matrix_cons ww -> Float_matrix_cons ( matrix_float_coeff_prod w ww )
   | _ -> let ww = matrix_foa_demakeup mm in
    let r = Array.length ww
    and cc = ( numcolumns ww ) - 1 in
     for i = 0 to r - 1 do
      for j = 0 to cc do
       ww.(i).(j) <- matrix_foa_scal_mult w.(i).(j) ww.(i).(j)
      done
     done ;
     Foa_matrix_cons ww
  end
 | _ ->
  begin
   match mm with
   | Float_cons y -> matrix_foa_scal_mult y m
   | Float_matrix_cons ww -> matrix_foa_coeff_prod mm m
   | _ -> let ww = matrix_foa_demakeup mm and w = matrix_foa_demakeup m in
    let r = Array.length ww
    and cc = ( numcolumns ww ) - 1 in
     for i = 0 to r - 1 do
      for j = 0 to cc do
       ww.(i).(j) <- matrix_foa_coeff_prod w.(i).(j) ww.(i).(j)
      done
     done ;
     Foa_matrix_cons ww
  end ;;


(** {v foa_diag_left_mult blockVector matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let foa_diag_left_mult = fun (d:foa_strip) (m:float_or_array) ->
 match m with
 | Float_cons y ->
  begin 
   match d with
   | Foa_cons x -> matrix_foa_scal_mult y x
   | _ ->
    begin
     let v = vector_foa_strip_demakeup d in
      let r = Array.length v in
       let mm = Array.make_matrix r r ( Float_cons 0. ) in
        for i = 0 to r - 1 do
         mm.(i).(i) <- matrix_foa_scal_mult y v.(i)
        done ;
        Foa_matrix_cons mm
    end
  end
 | Float_matrix_cons w ->
  begin 
   match d with
   | Foa_cons x -> matrix_foa_prod x m
   | _ ->
    begin
     let v = vector_foa_strip_demakeup d in
      let r = Array.length v
      and cc = numcolumns w in
       let mm = Array.make_matrix r (cc + 1) ( Float_cons 0. ) in
        for i = 0 to r - 1 do
         let row_input = w.(i)
         and row_output = mm.(i)
         and coeff = v.(i) in
          for j = 0 to cc do
           row_output.(j) <- matrix_foa_scal_mult row_input.(j) coeff
          done
        done ;
        Foa_matrix_cons mm
    end
  end
 | _ -> let w = matrix_foa_demakeup m in
  begin
   match d with
   | Foa_cons x -> matrix_foa_prod x m
   | _ ->
    begin
     let v = vector_foa_strip_demakeup d in
      let r = Array.length w
      and cc = ( numcolumns w ) - 1 in
       let mm = Array.make_matrix r (cc + 1) ( Float_cons 0. ) in
        for i = 0 to r - 1 do
         let row_input = w.(i)
         and row_output = mm.(i)
         and coeff = v.(i) in
          for j = 0 to cc do
           row_output.(j) <- matrix_foa_prod coeff row_input.(j)
          done
        done ;
        Foa_matrix_cons mm
    end
  end ;;


(** {v foa_diag_right_mult blockVector matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let foa_diag_right_mult = fun (d:foa_strip) (m:float_or_array) ->
 match m with
 | Float_cons y -> foa_diag_left_mult d m
 | Float_matrix_cons w ->
  begin 
   match d with
   | Foa_cons x -> matrix_foa_prod m x
   | _ -> (** idem foa_diag_left_mult *)
    begin
     let v = vector_foa_strip_demakeup d in
      let r = Array.length v
      and cc = (numcolumns w) - 1 in
       let mm = Array.make_matrix r (cc + 1) ( Float_cons 0. ) in
        for i = 0 to r - 1 do
         let row_input = w.(i)
         and row_output = mm.(i) in
          for j = 0 to cc do
           row_output.(j) <- matrix_foa_scal_mult row_input.(j) v.(j)
          done
        done ;
        Foa_matrix_cons mm
    end
  end
 | _ -> let w = matrix_foa_demakeup m in
  begin
   match d with
   | Foa_cons x -> matrix_foa_prod m x
   | _ ->
    begin
     let v = vector_foa_strip_demakeup d in
      let r = Array.length w
      and cc = ( numcolumns w ) - 1 in
       let mm = Array.make_matrix r (cc + 1) ( Float_cons 0. ) in
        for i = 0 to r - 1 do
         let row_input = w.(i)
         and row_output = mm.(i) in
          for j = 0 to cc do
           row_output.(j) <- matrix_foa_prod row_input.(j) v.(j)
          done
        done ;
        Foa_matrix_cons mm
    end
  end ;;


(** {v partial_foa_left_prod beginning end element line v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_foa_left_prod = fun (i:int) (j:int) (x:float_or_array) (s:float_or_array array) ->
 let c = Array.length s in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_prod x s.(k)
   done ;
   m ;;


(** {v partial_foa_left_twisted_prod beginning end element line v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_foa_left_twisted_prod = fun (i:int) (j:int) (x:float_or_array) (s:float_or_array array) ->
 let c = Array.length s in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_twisted_prod x s.(k)
   done ;
   m ;;


(** {v partial_foa_right_prod beginning end element line v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_foa_right_prod = fun (i:int) (j:int) (x:float_or_array) (s:float_or_array array) ->
 let c = Array.length s in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_prod s.(k) x
   done ;
   m ;;

(** {v partial_foa_right_twisted_prod beginning end element line v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_foa_right_twisted_prod = fun (i:int) (j:int) (x:float_or_array) (s:float_or_array array) ->
 let c = Array.length s in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_twisted_prod s.(k) x
   done ;
   m ;;

(** {v other_partial_foa_right_twisted_prod beginning end element line v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let other_partial_foa_right_twisted_prod = fun (i:int) (j:int) (x:float_or_array) (s:float_or_array array) ->
 let c = Array.length s in
  let m = Array.make c (Float_cons 0.) in
   for k = i to j do
    m.(k) <- matrix_foa_twisted_prod ( foa_transpose s.(k) ) x
   done ;
   m ;;



(** {v matrix_ioa_twisted_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_ioa_twisted_prod = fun (m:int_or_array) (mm:int_or_array) ->
 match ( Util.int_max (ioa_thickness m) (ioa_thickness mm) ) with
 | 0 ->
  begin (** matrices plates --- flat matrices *)
   match m with
   | Int_cons 0 -> Int_cons 0
   | Int_cons x ->
    begin
     match mm with
     | Int_cons 0 -> Int_cons 0
     | Int_cons y -> Int_cons (x * y)
     | _ -> Int_matrix_cons ( matrix_int_scal_mult x ( int_transpose (matrix_int_demakeup mm) ) )
    end
   | _ ->
    begin
     match mm with
     | Int_cons 0 -> Int_cons 0
     | Int_cons y -> Int_matrix_cons ( matrix_int_scal_mult y (matrix_int_demakeup m) )
     | _ -> Int_matrix_cons ( matrix_int_twisted_prod (matrix_int_demakeup m) (matrix_int_demakeup mm) )
    end
  end (** matrices plates --- flat matrices *)
 | _ ->
  begin (** matrices épaisses --- thick matrices *)
   match m with
   | Int_cons 0 -> Int_cons 0
   | Int_cons x -> matrix_ioa_scal_mult x mm
   | Int_matrix_cons w ->
    begin (** m est plate --- m is flat *)
     let ww = matrix_ioa_demakeup mm in
      let r = - 1 + Array.length w
      and c = - 1 + Array.length ww
      and s = - 1 + numcolumns ww in
       let mmm = Array.make_matrix (r + 1) (c + 1) (Int_cons 0) in
        for i = 0 to r do
         let row_input = w.(i)
         and row_output = mmm.(i) in
          for j = 0 to c do
           let row_right = ww.(j) in
            row_output.(j) <- matrix_ioa_scal_mult row_input.(0) ( ioa_transpose row_right.(0) ) ;
            for k = 1 to s do
             row_output.(j) <- matrix_ioa_plus row_output.(j) ( matrix_ioa_scal_mult row_input.(k) ( ioa_transpose row_right.(k) ) )
            done
          done
        done ;
        Ioa_matrix_cons mmm
    end (** m est plate --- m is flat *)
   | _ ->
    begin (** m est épaisse --- m is thick *)
     match mm with
     | Int_cons 0 -> Int_cons 0
     | Int_cons y -> matrix_ioa_scal_mult y m
     | Int_matrix_cons ww ->
      begin (** mm est plate --- mm is flat *)
       let w = matrix_ioa_demakeup m in
        let r = - 1 + Array.length w
        and c = - 1 + Array.length ww
        and s = - 1 + numcolumns ww in
         let mmm = Array.make_matrix (r + 1) (c + 1) (Int_cons 0) in
          for i = 0 to r do
           let row_input = w.(i)
           and row_output = mmm.(i) in
            for j = 0 to c do
             let row_right = ww.(j) in
              row_output.(j) <- matrix_ioa_scal_mult row_right.(0) row_input.(0) ;
              for k = 1 to s do
               row_output.(j) <- matrix_ioa_plus row_output.(j) ( matrix_ioa_scal_mult row_right.(k) row_input.(k) )
              done
            done
          done ;
          Ioa_matrix_cons mmm
      end (** mm est plate --- mm is flat *)
     |_ ->
      begin (** mm est épaisse --- mm is thick *)
       let w = matrix_ioa_demakeup m and ww = matrix_ioa_demakeup mm in
        let r = - 1 + Array.length w
        and c = - 1 + Array.length ww
        and s = - 1 + numcolumns ww in
         let mmm = Array.make_matrix (r + 1) (c + 1) (Int_cons 0) in
          for i = 0 to r do
           let row_input = w.(i)
           and row_output = mmm.(i) in
            for j = 0 to c do
             let row_right = ww.(j) in
              row_output.(j) <- matrix_ioa_twisted_prod row_input.(0) row_right.(0) ;
              for k = 1 to s do
               row_output.(j) <- matrix_ioa_plus row_output.(j) ( matrix_ioa_twisted_prod row_input.(k) row_right.(k) )
              done
            done
          done ;
          Ioa_matrix_cons mmm
      end (** mm est épaisse --- mm is thick *)
    end (** m est épaisse --- m is thick *)
  end  (** matrices épaisses --- thick matrices *) ;;


(** {v matrix_ioa_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let matrix_ioa_prod = fun (m:int_or_array) (mm:int_or_array) ->
 matrix_ioa_twisted_prod m ( ioa_transpose mm ) ;;

(** {v matrix_ioa_triple_prod matrix1 matrix2 v} *)
let matrix_ioa_triple_prod = fun (a:int_or_array) (b:int_or_array) (c:int_or_array)->
 matrix_ioa_twisted_prod a ( matrix_ioa_twisted_prod ( ioa_transpose c ) b ) ;;


(** {v matrix_ioa_twisted_commut matrix1 matrix2 v} *)
let matrix_ioa_twisted_commut = fun (m:int_or_array) (mm:int_or_array) ->
 matrix_ioa_minus ( matrix_ioa_twisted_prod m mm ) ( matrix_ioa_twisted_prod mm m ) ;;

(** {v matrix_ioa_twisted_commut_bis matrix1 matrix2 v} *)
let matrix_ioa_twisted_commut_bis = fun (m:int_or_array) (mm:int_or_array) ->
 let m_m = ioa_transpose m
 and m_mm = ioa_transpose mm in
  matrix_ioa_minus ( matrix_ioa_twisted_prod m mm ) ( matrix_ioa_twisted_prod m_mm m_m ) ;;

(** {v matrix_ioa_commut matrix1 matrix2 v} *)
let matrix_ioa_commut = fun (m:int_or_array) (mm:int_or_array) ->
 matrix_ioa_minus ( matrix_ioa_prod m mm ) ( matrix_ioa_prod mm m ) ;;


(** {v matrix_ioa_naive_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_ioa_naive_prod = fun (m:int_or_array) (mm:int_or_array) ->
 match ( Util.int_max (ioa_thickness m) (ioa_thickness mm) ) with
 | 0 ->
  begin (** matrices plates --- flat matrices *)
   match m with
   | Int_cons 0 -> Int_cons 0
   | Int_cons x ->
    begin
     match mm with
     | Int_cons 0 -> Int_cons 0
     | Int_cons y -> Int_cons (x * y)
     | _ -> Int_matrix_cons ( matrix_int_scal_mult x (matrix_int_demakeup mm) )
    end
   | _ ->
    begin
     match mm with
     | Int_cons 0 -> Int_cons 0
     | Int_cons y -> Int_matrix_cons ( matrix_int_scal_mult y (matrix_int_demakeup m) )
     | _ -> Int_matrix_cons ( matrix_int_prod (matrix_int_demakeup m) (matrix_int_demakeup mm) )
    end
  end (** matrices plates --- flat matrices *)
 | _ ->
  begin (** matrices épaisses --- thick matrices *)
   match m with
   | Int_cons 0 -> Int_cons 0
   | Int_cons x -> matrix_ioa_scal_mult x mm
   | Int_matrix_cons w ->
    begin (** m est plate --- m is flat *)
     let ww = matrix_ioa_demakeup mm in
      let r = - 1 + Array.length w
      and s = - 1 + Array.length ww
      and c = - 1 + numcolumns ww in
       let mmm = Array.make_matrix (r + 1) (c + 1) (Int_cons 0) in
        for i = 0 to r do
         let row_input = w.(i)
         and row_output = mmm.(i) in
          for j = 0 to c do
           row_output.(j) <- matrix_ioa_scal_mult row_input.(0) ww.(0).(j) ;
           for k = 1 to s do
            row_output.(j) <- matrix_ioa_plus row_output.(j) ( matrix_ioa_scal_mult row_input.(k) ww.(k).(j) )
           done
          done
        done ;
        Ioa_matrix_cons mmm
    end (** m est plate --- m is flat *)
   | _ ->
    begin (** m est épaisse --- m is thick *)
     match mm with
     | Int_cons 0 -> Int_cons 0
     | Int_cons y -> matrix_ioa_scal_mult y m
     | Int_matrix_cons ww ->
      begin (** mm est plate --- mm is flat *)
       let w = matrix_ioa_demakeup m in
        let r = - 1 + Array.length w
        and s = - 1 + Array.length ww
        and c = - 1 + numcolumns ww in
         let mmm = Array.make_matrix (r + 1) (c + 1) (Int_cons 0) in
          for i = 0 to r do
           let row_input = w.(i)
           and row_output = mmm.(i) in
            for j = 0 to c do
             row_output.(j) <- matrix_ioa_scal_mult ww.(0).(j) row_input.(0) ;
             for k = 1 to s do
              row_output.(j) <- matrix_ioa_plus row_output.(j) ( matrix_ioa_scal_mult ww.(k).(j) row_input.(k) )
             done
            done
          done ;
          Ioa_matrix_cons mmm
      end (** mm est plate --- mm is flat *)
     |_ ->
      begin (** mm est épaisse --- mm is thick *)
       let w = matrix_ioa_demakeup m and ww = matrix_ioa_demakeup mm in
        let r = - 1 + Array.length w
        and s = - 1 + Array.length ww
        and c = - 1 + numcolumns ww in
         let mmm = Array.make_matrix (r + 1) (c + 1) (Int_cons 0) in
          for i = 0 to r do
           let row_input = w.(i)
           and row_output = mmm.(i) in
            for j = 0 to c do
             row_output.(j) <- matrix_ioa_naive_prod row_input.(0) ww.(0).(j) ;
             for k = 1 to s do
              row_output.(j) <- matrix_ioa_plus row_output.(j) ( matrix_ioa_naive_prod row_input.(k) ww.(k).(j) )
             done
            done
          done ;
          Ioa_matrix_cons mmm
      end (** mm est épaisse --- mm is thick *)
    end (** m est épaisse --- m is thick *)
  end  (** matrices épaisses --- thick matrices *) ;;


(** {v matrix_ioa_coeff_prod matrix1 matrix2 v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let rec matrix_ioa_coeff_prod = fun (m:int_or_array) (mm:int_or_array) ->
 match m with
 | Int_cons x -> matrix_ioa_scal_mult x m
 | Int_matrix_cons w ->
  begin
   match mm with
   | Int_cons y -> Int_matrix_cons ( matrix_int_scal_mult y w )
   | Int_matrix_cons ww -> Int_matrix_cons ( matrix_int_coeff_prod w ww )
   | _ -> let ww = matrix_ioa_demakeup mm in
    let r = Array.length ww
    and cc = ( numcolumns ww ) - 1 in
     for i = 0 to r - 1 do
      for j = 0 to cc do
       ww.(i).(j) <- matrix_ioa_scal_mult w.(i).(j) ww.(i).(j)
      done
     done ;
     Ioa_matrix_cons ww
  end
 | _ ->
  begin
   match mm with
   | Int_cons y -> matrix_ioa_scal_mult y m
   | Int_matrix_cons ww -> matrix_ioa_coeff_prod mm m
   | _ -> let ww = matrix_ioa_demakeup mm and w = matrix_ioa_demakeup m in
    let r = Array.length ww
    and cc = ( numcolumns ww ) - 1 in
     for i = 0 to r - 1 do
      for j = 0 to cc do
       ww.(i).(j) <- matrix_ioa_coeff_prod w.(i).(j) ww.(i).(j)
      done
     done ;
     Ioa_matrix_cons ww
  end ;;


(** {v ioa_diag_left_mult blockVector matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let ioa_diag_left_mult = fun (d:ioa_strip) (m:int_or_array) ->
 match m with
 | Int_cons y ->
  begin 
   match d with
   | Ioa_cons x -> matrix_ioa_scal_mult y x
   | _ ->
    begin
     let v = vector_ioa_strip_demakeup d in
      let r = Array.length v in
       let mm = Array.make_matrix r r ( Int_cons 0 ) in
        for i = 0 to r - 1 do
         mm.(i).(i) <- matrix_ioa_scal_mult y v.(i)
        done ;
        Ioa_matrix_cons mm
    end
  end
 | Int_matrix_cons w ->
  begin 
   match d with
   | Ioa_cons x -> matrix_ioa_prod x m
   | _ ->
    begin
     let v = vector_ioa_strip_demakeup d in
      let r = Array.length v
      and cc = numcolumns w in
       let mm = Array.make_matrix r (cc + 1) ( Int_cons 0 ) in
        for i = 0 to r - 1 do
         let row_input = w.(i)
         and row_output = mm.(i)
         and coeff = v.(i) in
          for j = 0 to cc do
           row_output.(j) <- matrix_ioa_scal_mult row_input.(j) coeff
          done
        done ;
        Ioa_matrix_cons mm
    end
  end
 | _ -> let w = matrix_ioa_demakeup m in
  begin
   match d with
   | Ioa_cons x -> matrix_ioa_prod x m
   | _ ->
    begin
     let v = vector_ioa_strip_demakeup d in
      let r = Array.length w
      and cc = ( numcolumns w ) - 1 in
       let mm = Array.make_matrix r (cc + 1) ( Int_cons 0 ) in
        for i = 0 to r - 1 do
         let row_input = w.(i)
         and row_output = mm.(i)
         and coeff = v.(i) in
          for j = 0 to cc do
           row_output.(j) <- matrix_ioa_prod coeff row_input.(j)
          done
        done ;
        Ioa_matrix_cons mm
    end
  end ;;


(** {v ioa_diag_right_mult blockVector matrix v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let ioa_diag_right_mult = fun (d:ioa_strip) (m:int_or_array) ->
 match m with
 | Int_cons y -> ioa_diag_left_mult d m
 | Int_matrix_cons w ->
  begin 
   match d with
   | Ioa_cons x -> matrix_ioa_prod m x
   | _ -> (** idem ioa_diag_left_mult *)
    begin
     let v = vector_ioa_strip_demakeup d in
      let r = Array.length v
      and cc = (numcolumns w) - 1 in
       let mm = Array.make_matrix r (cc + 1) ( Int_cons 0 ) in
        for i = 0 to r - 1 do
         let row_input = w.(i)
         and row_output = mm.(i) in
          for j = 0 to cc do
           row_output.(j) <- matrix_ioa_scal_mult row_input.(j) v.(j)
          done
        done ;
        Ioa_matrix_cons mm
    end
  end
 | _ -> let w = matrix_ioa_demakeup m in
  begin
   match d with
   | Ioa_cons x -> matrix_ioa_prod m x
   | _ ->
    begin
     let v = vector_ioa_strip_demakeup d in
      let r = Array.length w
      and cc = ( numcolumns w ) - 1 in
       let mm = Array.make_matrix r (cc + 1) ( Int_cons 0 ) in
        for i = 0 to r - 1 do
         let row_input = w.(i)
         and row_output = mm.(i) in
          for j = 0 to cc do
           row_output.(j) <- matrix_ioa_prod row_input.(j) v.(j)
          done
        done ;
        Ioa_matrix_cons mm
    end
  end ;;


(** {v partial_ioa_left_prod beginning end element line v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_ioa_left_prod = fun (i:int) (j:int) (x:int_or_array) (s:int_or_array array) ->
 let c = Array.length s in
  let m = Array.make c (Int_cons 0) in
   for k = i to j do
    m.(k) <- matrix_ioa_prod x s.(k)
   done ;
   m ;;


(** {v partial_ioa_right_prod beginning end element line v} Inner blocks reduced to a real number are accepted.

Des blocs réduits à un réel sont tolérés. *)
let partial_ioa_right_prod = fun (i:int) (j:int) (x:int_or_array) (s:int_or_array array) ->
 let c = Array.length s in
  let m = Array.make c (Int_cons 0) in
   for k = i to j do
    m.(k) <- matrix_ioa_prod s.(k) x
   done ;
   m ;;




(** {C § } *)
(** Les définitions des neuf fonctions qui suivent sont mutuellement récursives.

The definitions of the nine following functions are mutually recursive. *)
(** {C  } *)




(** {v foa_slow_pivot_downward i m p v} The block matrix m is assumed to be upper triangular from line 0 to i - 1.
The matrix p registers the same changes as m.

La matrice par blocs m est supposée, de la ligne 0 à i - 1, triangulaire supérieure. 
La matrice p enregistre les memes changements que m. *)
let rec foa_slow_pivot_downward = fun (i:int) (m:float_or_array) (p:float_or_array) ->
 match m with
 | Float_cons x -> [| m ; Float_cons 1. ; Float_matrix_cons [| [| 0. ; 0. |] |] ; Float_cons (1. /. x) |]
 | Float_matrix_cons w -> let resultat = float_slow_pivot_downward i w (matrix_float_demakeup p) in
  [| Float_matrix_cons resultat.(0) ; Float_matrix_cons resultat.(1) ;
   Float_matrix_cons resultat.(2) ; Float_cons resultat.(3).(0).(0) |]
 | _ ->
  let w = matrix_foa_demakeup m in
   let r = Array.length w
   and mmmm = ref ( matrix_foa_demakeup m )
   and pppp = ref ( matrix_foa_demakeup p )
   and c = numcolumns w in
    let s = min r c 
    and permutation = ref ( [| [| 0. ; 0. |] |] )
    and ww = sub_matrix w i (r - 1) i (c - 1) 
    and index = ref 0
    and k = ref 0 in 
     while !k < Array.length ww do
      if ( foa_slow_invertibility ww.(!k).(0) ) then ( index := !k ; k := r ) else ( k := !k + 1 ) ;
     done ;
      if !index <> 0 then 
       begin
        mmmm := exchange_row i (i + !index) !mmmm ;
        pppp := exchange_row i (i + !index) !pppp ;
       end ;
      let bloc = !mmmm.(i).(i) in
       if not ( foa_slow_invertibility bloc ) then failwith "Block non invertible in Matrix.foa_slow_pivot_downward." ;
       let coefficient = foa_slow_inv bloc in
        let ligne = Array.map (matrix_foa_prod coefficient) !pppp.(i)
        and row = partial_foa_left_prod (i + 1) (c - 1) coefficient !mmmm.(i) in
         for j = i + 1 to s - 1 do (** Pay attention to order: attention à l'ordre *)
          let coeff = !mmmm.(j).(i) in 
           !pppp.(j) <- line_foa_minus !pppp.(j) ( Array.map (matrix_foa_prod coeff) ligne ) ;
           !mmmm.(j) <- partial_foa_minus (i + 1) (c - 1) !mmmm.(j) ( partial_foa_left_prod (i + 1) (c - 1) coeff row ) ;
         done ;
         [| Foa_matrix_cons !mmmm ; Foa_matrix_cons !pppp ; Float_matrix_cons !permutation ; coefficient |]


(** {v foa_restricted_slow_pivot_downward i m v} The block matrix m is assumed to be upper triangular from line 0 to i - 1.

La matrice par blocs m est supposée, de la ligne 0 à i - 1, triangulaire supérieure. *)
and foa_restricted_slow_pivot_downward = fun (i:int) (m:float_or_array) ->
 match m with
 | Float_cons x -> [| m ; m |]
 | Float_matrix_cons w -> let resultat = float_restricted_slow_pivot_downward i w in
  [| Float_matrix_cons resultat.(0) ; Float_cons resultat.(1).(0).(0) |]
 | _ ->
  let w = matrix_foa_demakeup m in
   let r = Array.length w
   and mmmm = ref w
   and c = numcolumns w in
    let s = min r c 
    and ww = sub_matrix w i (r - 1) i (c - 1) 
    and index = ref 0
    and k = ref 0 in
     while !k < Array.length ww do
      if ( foa_slow_invertibility ww.(!k).(0) ) then ( index := !k ; k := r ) else ( k := !k + 1 ) ;
     done ;
      if !index <> 0 then mmmm := exchange_row i (i + !index) !mmmm ;
      let element = ref !mmmm.(i).(i) in
       if foa_slow_invertibility !element then
        let coefficient = foa_slow_inv !element in
         let row = partial_foa_left_prod (i + 1) (c - 1) coefficient !mmmm.(i) in
          for j = i + 1 to s - 1 do
           let coeff = !mmmm.(j).(i) in 
            !mmmm.(j) <- partial_foa_minus (i + 1) (c - 1) !mmmm.(j) ( partial_foa_left_prod (i + 1) (c - 1) coeff row ) ;
          done
       else element := Float_cons 0. ;
       [| Foa_matrix_cons !mmmm ; !element |]


(** {v foa_slow_pivot_upward i m p v} The block matrix m is assumed to be diagonal from line i + 1 to r - 1.
The matrix p registers the same changes as m.

La matrice par blocs m est supposée, de la ligne i + 1 à r - 1, diagonale. 
La matrice p enregistre les memes changements que m. *)
and foa_slow_pivot_upward = fun (i:int) (m:float_or_array) (p:float_or_array) (coefficient:float_or_array) ->
 match m with
 | Float_cons x -> [| m ; Float_cons 1. |]
 | Float_matrix_cons w -> let resultat = float_slow_pivot_upward i w (matrix_float_demakeup p) (float_demakeup coefficient) in
  [| Float_matrix_cons resultat.(0) ; Float_matrix_cons resultat.(1) |]
 | _ ->
  let mmmm = ref ( matrix_foa_demakeup m )
  and pppp = ref ( matrix_foa_demakeup p ) in
   let ligne = Array.map (matrix_foa_prod coefficient) !pppp.(i)
   and row = partial_foa_left_prod 0 (i - 1) coefficient !mmmm.(i) in
    for j = i - 1 downto 0 do (** Pay attention to order : attention à l'ordre *)
     let coeff = !mmmm.(j).(i) in 
      !pppp.(j) <- line_foa_minus !pppp.(j) ( Array.map (matrix_foa_prod coeff) ligne ) ;
      !mmmm.(j) <- partial_foa_minus j (i - 1) !mmmm.(j) ( partial_foa_left_prod j (i - 1) coeff row ) ;
    done ;
    [| Foa_matrix_cons !mmmm ; Foa_matrix_cons !pppp |]


(** {v foa_slow_invertibility matrix v} *)
and foa_slow_invertibility = function (m:float_or_array) -> match m with
 | Float_cons x -> x <> 0.
 | Float_matrix_cons w -> float_slow_invertibility w
 | _ ->
  let r = min (foa_numrows m) (foa_numcolumns m)
  and mm = ref (matrix_foa_copy m) in
   let i = ref 0
   and rr = pred r
   and output = ref true in
    while !i < rr do
    let resultat = foa_restricted_slow_pivot_downward !i !mm in
     if resultat.(1) = Float_cons 0. then ( i := r ; output := false )
     else
      begin
       mm := resultat.(0) ;
       i := !i + 1 ;
      end
    done ;
   output := !output && ( foa_slow_invertibility ( matrix_foa_demakeup !mm ).(rr).(rr) ) ;
   !output


(** {v foa_slow_invertibility_evaluation matrix v} *)
and foa_slow_invertibility_evaluation = function (m:float_or_array) ->
 match m with
 | Float_cons x -> x
 | Float_matrix_cons w -> float_slow_invertibility_evaluation w
 | _ ->
  let r = min (foa_numrows m) (foa_numcolumns m)
  and mm = ref (matrix_foa_copy m) in
   let i = ref 0
   and rr = pred r
   and diagonale = Array.make r (Float_cons 0.)
   and output = ref max_float in
    while !i < rr do
     let resultat = foa_restricted_slow_pivot_downward !i !mm in
      let coeff = resultat.(1) in
       if ( coeff = Float_cons 0. ) then ( i := r ; output := 0. )
       else
        begin
         mm := resultat.(0) ;
         diagonale.(!i) <- coeff ;
         i := !i + 1 ;
        end
    done ;
    if !output <> 0. then 
     begin
      diagonale.(rr) <- (matrix_foa_demakeup !mm).(rr).(rr) ;
      let suite = Array.map ( foa_slow_invertibility_evaluation ) diagonale in
       let absdiag = vector_float_abs suite in
        let mini = Util.vector_min absdiag in
         let index = vector_float_find_first mini absdiag in
          suite.(index)
     end
    else 0.


(** {v foa_slow_abs_det matrix v} *)
and foa_slow_abs_det = function (m:float_or_array) ->
 match m with
 | Float_cons x -> x
 | Float_matrix_cons w -> abs_float ( float_slow_det w )
 | _ ->
  let r = min (foa_numrows m) (foa_numcolumns m)
  and mm = ref (matrix_foa_copy m)
  and accu = ref 1. in
   let i = ref 0
   and rr = pred r in
    while !i < rr do
     let resultat = foa_restricted_slow_pivot_downward !i !mm in
      let coeff = resultat.(1) in
       if ( coeff = Float_cons 0. ) then ( i := r ; accu := 0. )
       else
        begin
         mm := resultat.(0) ;
         accu := !accu *. ( foa_slow_abs_det coeff ) ;
         i := !i + 1
        end
    done ;
    if !accu <> 0. then
     abs_float ( !accu *. ( foa_slow_abs_det ( matrix_foa_demakeup !mm ).(rr).(rr) ) )
    else 0.


(** {v matrix_foa_slow_left_quotient matrix1 matrix2 v} *)
and matrix_foa_slow_left_quotient = fun (m:float_or_array) (q:float_or_array) ->
 match m with
 | Float_cons x -> matrix_foa_scal_left_div x q
 | Float_matrix_cons w -> Float_matrix_cons (matrix_float_slow_left_quotient w ( matrix_float_demakeup q ) )
 | _ ->
  let r = min (foa_numrows m) (foa_numcolumns m)
  and mm = ref ( matrix_foa_copy m )
  and pp = ref ( matrix_foa_copy q ) in
   let diagonale = Array.make r (Float_cons 0.)
   and rr = pred r in
    for i = 0 to r - 2 do
     let resultat = foa_slow_pivot_downward i !mm !pp in
      mm := resultat.(0) ;
      pp := resultat.(1) ;
      diagonale.(i) <- resultat.(3)
    done ;
    let bloc = (matrix_foa_demakeup !mm).(rr).(rr) in
     if not ( foa_slow_invertibility bloc ) then failwith "Block non invertible in Matrix.matrix_foa_slow_left_quotient." ;
     diagonale.(rr) <- foa_slow_inv bloc ;
     for i = rr downto 1 do
      let resultat = foa_slow_pivot_upward i !mm !pp diagonale.(i) in
       mm := resultat.(0) ;
       pp := resultat.(1) ;
     done ;
     mm := foa_diag_left_mult ( Foa_strip_cons diagonale ) !pp ;
     !mm


(** {v line_foa_slow_left_quotient matrix1 matrix_array v} *)
and line_foa_slow_left_quotient = fun (m:float_or_array) (q:float_or_array array) ->
 Array.map ( matrix_foa_slow_left_quotient m ) q


(** {v foa_slow_inv matrix v} *)
and foa_slow_inv = function (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons ( 1. /. x )
 | Float_matrix_cons w -> Float_matrix_cons (float_slow_inv w)
 | _ ->
  let r = min (foa_numrows m) (foa_numcolumns m)
  and mm = ref (matrix_foa_copy m)
  and pp = ref (eye_foa m) in
   let diagonale = Array.make r (Float_cons 0.)
   and rr = pred r in
    for i = 0 to r - 2 do
     let resultat = foa_slow_pivot_downward i !mm !pp in
      mm := resultat.(0) ;
      pp := resultat.(1) ;
      diagonale.(i) <- resultat.(3)
    done ;
    let bloc = (matrix_foa_demakeup !mm).(rr).(rr) in
     if not ( foa_slow_invertibility bloc ) then failwith "Block non invertible in Matrix.foa_slow_inv." ;
     diagonale.(rr) <- foa_slow_inv bloc ;
     for i = rr downto 1 do
      let resultat = foa_slow_pivot_upward i !mm !pp diagonale.(i) in
       mm := resultat.(0) ;
       pp := resultat.(1) ;
     done ;
     mm := foa_diag_left_mult ( Foa_strip_cons diagonale ) !pp ;
     !mm ;;




(** Fin des neuf définitions mutuellement récursives.

End of the nine mutually recursive definitions. *)




(** {v matrix_foa_slow_right_quotient matrix1 matrix2 v} This gives matrix2 * (matrix1) ^ -1.

Ceci retourne matrix2 * (matrix1) ^ -1. *)
let matrix_foa_slow_right_quotient = fun (m:float_or_array) (q:float_or_array) ->
 foa_transpose ( matrix_foa_slow_left_quotient (foa_transpose m) (foa_transpose q) ) ;;




(** {C § } *)
(** Les définitions des six fonctions qui suivent sont mutuellement récursives.

The definitions of the six following functions are mutually recursive. *)
(** {C  } *)




(** {v foa_invertibility matrix v} *)
let rec foa_invertibility = function (m:float_or_array) ->
 match m with
 | Float_cons x -> x <> 0.
 | Float_matrix_cons w -> float_invertibility w
 | _ ->
  let w = matrix_foa_demakeup m in
   let r = Array.length w
   and c = numcolumns w
   and i = ref 0
   and output = ref true in
    let rr = r - 1
    and cc = c - 1
    and s = min r c in
     while !i < rr do
      let ii = !i + 1
      and index = ref !i
      and k = ref !i in
       while !k < rr do
        if ( foa_invertibility w.(!k).(!i) ) then ( index := !k ; k := r ) else ( k := !k + 1 ) ;
       done ;
       if !index <> !i then
        begin
         let auxil = w.(!i) in
          w.(!i) <- w.(!index) ;
          w.(!index) <- auxil ;
        end ;
        let element = ref w.(!i).(!i) in
         if ( foa_invertibility !element ) then
          let coefficient = foa_inv !element in
           let row = other_partial_foa_right_twisted_prod ii cc coefficient w.(!i) in
            for j = ii to s - 1 do
             let coeff = w.(j).(!i) in 
              w.(j) <- partial_foa_minus ii cc w.(j) ( partial_foa_left_twisted_prod ii cc coeff row ) ;
            done ;
            i := ii
         else ( i := r ; output := false ) ;
     done ;
     output := !output && ( foa_invertibility w.(rr).(rr) ) ;
     !output


(** {v foa_invertibility_evaluation matrix v} *)
and foa_invertibility_evaluation = function (m:float_or_array) ->
 match m with
 | Float_cons x -> x
 | Float_matrix_cons w -> float_invertibility_evaluation w
 | _ ->
  let w = matrix_foa_demakeup m in
   let r = Array.length w
   and c = numcolumns w
   and i = ref 0 in
    let rr = r - 1
    and cc = c - 1
    and s = min r c
    and diagonale = Array.make r 0.
    and output = ref max_float in
     while !i < rr do
      let ii = !i + 1
      and index = ref !i
      and k = ref !i in
       while !k < rr do
        if ( foa_invertibility w.(!k).(!i) ) then ( index := !k ; k := r ) else ( k := !k + 1 ) ;
       done ;
       if !index <> !i then
        begin
         let auxil = w.(!i) in
          w.(!i) <- w.(!index) ;
          w.(!index) <- auxil ;
        end ;
        let element = ref w.(!i).(!i) in
         if ( foa_invertibility !element ) then
          begin
           diagonale.(!i) <- foa_invertibility_evaluation !element ;
           let coefficient = foa_inv !element in
            let row = other_partial_foa_right_twisted_prod ii cc coefficient w.(!i) in
             for j = ii to s - 1 do
              let coeff = w.(j).(!i) in 
               w.(j) <- partial_foa_minus ii cc w.(j) ( partial_foa_left_twisted_prod ii cc coeff row ) ;
             done ;
             i := ii
          end
         else ( i := r ; output := 0. ) ;
     done ;
     if !output <> 0. then 
      begin
       diagonale.(rr) <- foa_invertibility_evaluation w.(rr).(rr) ;
        let absdiag = vector_float_abs diagonale in
        let mini = Util.vector_min absdiag in
         let index = vector_float_find_first mini absdiag in
          diagonale.(index)
      end
     else 0.


(** {v foa_abs_det matrix v} *)
and foa_abs_det = function (m:float_or_array) ->
 match m with
 | Float_cons x -> abs_float x
 | Float_matrix_cons w -> abs_float ( float_det w )
 | _ ->
  let w = matrix_foa_demakeup m in
   let r = Array.length w
   and c = numcolumns w
   and i = ref 0 in
    let rr = r - 1
    and cc = c - 1
    and s = min r c
    and accu = ref 1. in
     while !i < rr do
      let ii = !i + 1
      and index = ref !i
      and k = ref !i in
       while !k < rr do
        if ( foa_invertibility w.(!k).(!i) ) then ( index := !k ; k := r ) else ( k := !k + 1 ) ;
       done ;
       if !index <> !i then
        begin
         let auxil = w.(!i) in
          w.(!i) <- w.(!index) ;
          w.(!index) <- auxil ;
        end ;
        let element = ref w.(!i).(!i) in
         if ( foa_invertibility !element ) then
          begin
           accu := !accu *. ( foa_abs_det !element ) ;
           let coefficient = foa_inv !element in
            let row = other_partial_foa_right_twisted_prod ii cc coefficient w.(!i) in
             for j = ii to s - 1 do
              let coeff = w.(j).(!i) in 
               w.(j) <- partial_foa_minus ii cc w.(j) ( partial_foa_left_twisted_prod ii cc coeff row ) ;
             done ;
             i := ii
          end
         else ( i := r ; accu := 0. ) ;
     done ;
     if !accu <> 0. then 
      !accu *. ( foa_abs_det w.(rr).(rr) )
     else 0.


(** {v matrix_foa_left_quotient matrix1 matrix2 v} *)
and matrix_foa_left_quotient = fun (m:float_or_array) (q:float_or_array) ->
 match m with
 | Float_cons x -> matrix_foa_scal_mult ( 1. /. x ) q
 | Float_matrix_cons w -> Float_matrix_cons ( matrix_float_left_quotient w (matrix_float_demakeup q) )
 | _ ->
  let w = matrix_foa_demakeup m
  and p = matrix_foa_demakeup q in
   let r = Array.length w
   and c = numcolumns w in
    let rr = r - 1
    and cc = c - 1
    and index = ref 0
    and diagonale = Array.make r (Float_cons 0.) in
     for i = 0 to r - 2 do
      let ii = i + 1
      and k = ref i in
       while !k < r do
        if (foa_invertibility w.(!k).(i) = true) then ( index := !k ; k := r ) else ( k := !k + 1 ) ;
       done ;
       if !index <> i then 
        begin
         let auxil = w.(i)
         and aux = p.(i) in
          w.(i) <- w.( !index ) ;
          w.( !index ) <- auxil ;
          p.(i) <- p.( !index ) ;
          p.( !index ) <- aux
        end ;
        let bloc = w.(i).(i) in
         if not ( foa_invertibility bloc ) then failwith "Block non invertible in Matrix.matrix_foa_left_quotient." ;
         let coefficient = foa_inv bloc in
          let ligne = Array.map (function x -> matrix_foa_twisted_prod (foa_transpose x) coefficient) p.(i)
          and row = other_partial_foa_right_twisted_prod ii cc coefficient w.(i) in
           for j = ii to rr do (** Pay attention to order: attention à l'ordre *)
             let coeff = w.(j).(i) in 
             p.(j) <- line_foa_minus p.(j) ( Array.map (matrix_foa_twisted_prod coeff) ligne ) ;
             w.(j) <- partial_foa_minus ii cc w.(j) ( partial_foa_left_twisted_prod ii cc coeff row ) ;
           done ;
           diagonale.(i) <- coefficient ;
     done ;
     let bloc = w.(rr).(rr) in
      if not ( foa_invertibility bloc ) then failwith "Last block non invertible in Matrix.matrix_foa_left_quotient." ;
      diagonale.(rr) <- foa_inv bloc ;
      for i = rr downto 1 do
       let coefficient = diagonale.(i)
       and iii = i - 1 in
        let ligne = Array.map (function x -> matrix_foa_twisted_prod (foa_transpose x) coefficient) p.(i) in
         for j = iii downto 0 do (** Pay attention to order: attention à l'ordre *)
          let coeff = w.(j).(i) in 
           p.(j) <- line_foa_minus p.(j) ( Array.map (matrix_foa_twisted_prod coeff) ligne ) ;
         done ;
      done ;
      foa_diag_left_mult ( Foa_strip_cons diagonale ) ( Foa_matrix_cons p )


(** {v line_foa_left_quotient matrix1 matrix_array v} *)
and line_foa_left_quotient = fun (m:float_or_array) (q:float_or_array array) ->
 Array.map (matrix_foa_left_quotient m) q


(** {v foa_inv matrix v} *)
and foa_inv = function (m:float_or_array) ->
 match m with
 | Float_cons x -> Float_cons ( 1. /. x )
 | Float_matrix_cons w -> Float_matrix_cons (float_inv w)
 | _ ->
  let w = matrix_foa_demakeup m
  and p = matrix_foa_demakeup ( eye_foa m ) in
   let r = Array.length w
   and c = numcolumns w in
    let rr = r - 1
    and cc = c - 1
    and index = ref 0
    and diagonale = Array.make r (Float_cons 0.) in
     for i = 0 to r - 2 do
      let ii = i + 1
      and k = ref i in
       while !k < r do
        if (foa_invertibility w.(!k).(i) = true) then ( index := !k ; k := r ) else ( k := !k + 1 ) ;
       done ;
       if !index <> i then 
        begin
         let auxil = w.(i)
         and aux = p.(i) in
          w.(i) <- w.( !index ) ;
          w.( !index ) <- auxil ;
          p.(i) <- p.( !index ) ;
          p.( !index ) <- aux
        end ;
        let bloc = w.(i).(i) in
         if not ( foa_invertibility bloc ) then failwith "Block non invertible in Matrix.foa_inv." ;
         let coefficient = foa_inv bloc in
          let ligne = Array.map (function x -> matrix_foa_twisted_prod (foa_transpose x) coefficient) p.(i)
          and row = other_partial_foa_right_twisted_prod ii cc coefficient w.(i) in
           for j = ii to rr do (** Pay attention to order: attention à l'ordre *)
            let coeff = w.(j).(i) in 
             p.(j) <- line_foa_minus p.(j) ( Array.map (matrix_foa_twisted_prod coeff) ligne ) ;
             w.(j) <- partial_foa_minus ii cc w.(j) ( partial_foa_left_twisted_prod ii cc coeff row ) ;
           done ;
           diagonale.(i) <- coefficient ;
     done ;
     let bloc = w.(rr).(rr) in
      if not ( foa_invertibility bloc ) then failwith "Last block non invertible in Matrix.foa_inv." ;
      diagonale.(rr) <- foa_inv bloc ;
      for i = rr downto 1 do
       let coefficient = diagonale.(i)
       and iii = i - 1 in
        let ligne = Array.map (function x -> matrix_foa_twisted_prod (foa_transpose x) coefficient) p.(i) in
         for j = iii downto 0 do (** Pay attention to order: attention à l'ordre *)
          let coeff = w.(j).(i) in 
           p.(j) <- line_foa_minus p.(j) ( Array.map (matrix_foa_twisted_prod coeff) ligne ) ;
         done ;
      done ;
      foa_diag_left_mult ( Foa_strip_cons diagonale ) ( Foa_matrix_cons p ) ;;




(** Fin des six définitions mutuellement récursives.

End of the six mutually recursive definitions. *)




(** {v matrix_foa_right_quotient matrix1 matrix2 v} This gives matrix2 * (matrix1) ^ -1.

Ceci retourne matrix2 * (matrix1) ^ -1. *)
let matrix_foa_right_quotient = fun (m:float_or_array) (q:float_or_array) ->
 foa_transpose ( matrix_foa_left_quotient (foa_transpose m) (foa_transpose q) ) ;;


(** {v line_foa_right_quotient matrix1 matrix_array v} *)
let line_foa_right_quotient = fun (m:float_or_array) (q:float_or_array array) ->
 Array.map (matrix_foa_right_quotient m) q ;;




(** {C § } *)
(**
{2 {C Expérimentations par découpage des matrices plates
---
Experimentations with cutting of flat matrices} }
*)
(** {C  } *)




(** {v special_exponent size v} *)
let special_exponent = function r ->
  let s = 1. /. ( max 1. (log r) ) in
   0.45 *. s +. (1. -. s) *. 1.29 /. ( max 2. (log10 r) ) ;;



(** {v square_float_slow_inv base matrix v} *)
let square_float_slow_inv = fun b m ->
 let debut = ( Foa_vector_cons [| Float_vector_cons [|0.;0.|] |] ) in
  let mm = square_foa_hash b (Float_matrix_cons m) debut in
   let mmm = foa_slow_inv mm.(0) in
    let mmmm = square_foa_crash [| mmm ; mm.(1) |] in
     matrix_float_demakeup mmmm.(0) ;;

(** {v exp_float_slow_inv base matrix v} *)
let exp_float_slow_inv = fun b m ->
 let debut = ( Foa_vector_cons [| Float_vector_cons [|0.;0.|] |] ) in
  let mm = exp_foa_hash b (Float_matrix_cons m) debut in
   let mmm = foa_slow_inv mm.(0) in
    let mmmm = square_foa_crash [| mmm ; mm.(1) |] in
     matrix_float_demakeup mmmm.(0) ;;

(** {v sqrt_float_slow_inv matrix v} *)
let sqrt_float_slow_inv = function m ->
 let b = 10 + int_of_float ( ceil ( sqrt ( float (numrows m) ) ) )
 and debut = ( Foa_vector_cons [| Float_vector_cons [|0.;0.|] |] ) in
  let mm = square_foa_hash b (Float_matrix_cons m) debut in
   let mmm = foa_slow_inv mm.(0) in
    let mmmm = square_foa_crash [| mmm ; mm.(1) |] in
     matrix_float_demakeup mmmm.(0) ;;

(** {v alpha_float_slow_inv matrix v} *)
let alpha_float_slow_inv = function m ->
 let r = float (numrows m) in
  let alpha = special_exponent r
   and seuil = 10. in
    let b = int_of_float ( ceil ( seuil *. ( r /. seuil ) ** alpha ) )
    and debut = ( Foa_vector_cons [| Float_vector_cons [|0.;0.|] |] ) in
     let mm = exp_foa_hash b (Float_matrix_cons m) debut in
      let mmm = foa_slow_inv mm.(0) in
       let mmmm = square_foa_crash [| mmm ; mm.(1) |] in
        matrix_float_demakeup mmmm.(0) ;;



(** {v square_float_inv base matrix v} *)
let square_float_inv = fun b m ->
 let debut = Foa_vector_cons [| Float_vector_cons [|0.;0.|] |] in
  let mm = square_foa_hash b (Float_matrix_cons m) debut in
   let mmm = foa_inv mm.(0) in
    let mmmm = square_foa_crash [| mmm ; mm.(1) |] in
     matrix_float_demakeup mmmm.(0) ;;

(** {v exp_float_inv base matrix v} *)
let exp_float_inv = fun b m ->
 let debut = Foa_vector_cons [| Float_vector_cons [|0.;0.|] |] in
  let mm = exp_foa_hash b (Float_matrix_cons m) debut in
   let mmm = foa_inv mm.(0) in
    let mmmm = square_foa_crash [| mmm ; mm.(1) |] in
     matrix_float_demakeup mmmm.(0) ;;

(** {v sqrt_float_inv matrix v} *)
let sqrt_float_inv = function m ->
 let b = 10 + int_of_float ( ceil ( sqrt ( float (numrows m) ) ) )
 and debut = Foa_vector_cons [| Float_vector_cons [|0.;0.|] |] in
  let mm = square_foa_hash b (Float_matrix_cons m) debut in
   let mmm = foa_inv mm.(0) in
    let mmmm = square_foa_crash [| mmm ; mm.(1) |] in
     matrix_float_demakeup mmmm.(0) ;;



(** {v beta_float_inv matrix v} *)
let beta_float_inv = function m ->
 let b = int_of_float ( 0.4 *. sqrt ( float (Array.length m) ) ) in
  let mm = matrix_float_cut b m in
   let mmm = foa_inv mm.(0) in
    matrix_float_crash [| mmm ; mm.(1) |] ;;

(** {v gamma_float_inv matrix v} *)
let gamma_float_inv = function m ->
(** let b = int_of_float ( 0.4 *. sqrt ( float (Array.length m) ) ) in *)
 let r = Array.length m in
  if r < 2000 then float_inv m
  else
   begin
    let b = int_of_float ( ( float (Array.length m) ) /. 70. ) in
     let mm = matrix_float_cut b m
      and debut = [| Float_vector_cons [|0.;0.|] |] in
       let ww = square_foa_hash 8 mm.(0) ( Foa_vector_cons ( Array.append (vector_foa_demakeup mm.(1)) debut ) ) in
        let mmm = foa_inv ww.(0) in
         let mmmm = square_foa_crash [| mmm ; ww.(1) |] in
          matrix_float_demakeup mmmm.(0)
   end ;;

(** {v delta_float_inv matrix v} *)
let delta_float_inv = function m ->
 let r = Array.length m in
  if r < 1250 then float_inv m
  else (** ordre = 1250 : réduction de l' exposant qui compense l'augmentation de C dans : << temps ( n ) ~= C * ( n ** exposant ) >> *)
   begin
    let rr = float r in
     let alpha = special_exponent rr in
      let b = int_of_float ( floor ( rr ** alpha ) ) in
       let mm = matrix_float_cut b m in
        let mmm = foa_inv mm.(0) in
         matrix_float_crash [| mmm ; mm.(1) |]
   end ;;




(** {C § } *)
(**
{1 {C Fonctions et constructions supplémentaires
---
Further functions and constructions} }
*)
(** {C  } *)




(** {v critical_order v} Order = 1250: reduction of the exponent which compensates
the augmentation of C in:
<< duration ( n ) ~= C * ( n ** exponent ) >>

Ordre = 1250 : réduction de l' exposant qui compense l'augmentation de C dans : 
<< temps ( n ) ~= C * ( n ** exposant ) >> *)
let critical_order = 1250 ;;

(** {v generic_float_inv matrix v} For big ordres, the matrix must be invertible by blocks. 
This is achieved for some matrices obtained from the random float generator of Ocaml.

Pour de grands ordres, la matrice doit être bloc-inversible.
C'est vérifié pour certaines matrices construites à partir du générateur aléatoire de réels d'Ocaml. *)
let generic_float_inv = function m ->
 let r = Array.length m in
  if r < critical_order then float_inv m
  else
   begin
    let rr = float r in
     let alpha = special_exponent rr in
      let b = int_of_float ( floor ( rr ** alpha ) ) in
       let mm = matrix_float_cut b m in
        let mmm = foa_inv mm.(0) in
         matrix_float_crash [| mmm ; mm.(1) |]
   end ;;


(** {v generic_float_invertibility matrix v} For big ordres, the matrix must be invertible by blocks. 
This is achieved for some matrices obtained from the random float generator of Ocaml.

Pour de grands ordres, la matrice doit être bloc-inversible.
C'est vérifié pour certaines matrices construites à partir du générateur aléatoire de réels d'Ocaml. *)
let generic_float_invertibility = function m ->
 let r = Array.length m in
  if r < critical_order then float_invertibility m
  else
   begin
    let rr = float r in
     let alpha = special_exponent rr in
      let b = int_of_float ( floor ( rr ** alpha ) ) in
       let mm = matrix_float_cut b m in
        foa_invertibility mm.(0)
   end ;;


(** {v generic_float_invertibility_evaluation matrix v} For big ordres, the matrix must be invertible by blocks. 
This is achieved for some matrices obtained from the random float generator of Ocaml.

Pour de grands ordres, la matrice doit être bloc-inversible.
C'est vérifié pour certaines matrices construites à partir du générateur aléatoire de réels d'Ocaml. *)
let generic_float_invertibility_evaluation = function m ->
 let r = Array.length m in
  if r < critical_order then float_invertibility_evaluation m
  else
   begin
    let rr = float r in
     let alpha = special_exponent rr in
      let b = int_of_float ( floor ( rr ** alpha ) ) in
       let mm = matrix_float_cut b m in
        foa_invertibility_evaluation mm.(0)
   end ;;


(** {v generic_float_abs_det matrix v} For big ordres, the matrix must be invertible by blocks. 
This is achieved for some matrices obtained from the random float generator of Ocaml.

Pour de grands ordres, la matrice doit être bloc-inversible.
C'est vérifié pour certaines matrices construites à partir du générateur aléatoire de réels d'Ocaml. *)
let generic_float_abs_det = function m ->
 let r = Array.length m in
  if r < critical_order then abs_float ( float_det m )
  else
   begin
    let rr = float r in
     let alpha = special_exponent rr in
      let b = int_of_float ( floor ( rr ** alpha ) ) in
       let mm = matrix_float_cut b m in
        foa_abs_det mm.(0)
   end ;;


(** {v sym_float_bal_random order range v} *)
let sym_float_bal_random = fun (r:int) (x:float) ->
 let m = upper_trig_float_bal_random r r x in
  float_sym m ;;

(** {v sym_biased_float_random order range v} *)
let sym_biased_float_random = fun (r:int) (x:float) ->
 let m = upper_trig_float_random r r x in
  float_sym m ;;

(** {v sym_signed_float_random order range v} *)
let sym_signed_float_random = fun (r:int) (x:float) ->
 let m = sym_biased_float_random r x
 and w = matrix_float_random r r x in
  matrix_float_twisted_prod ( matrix_float_twisted_prod w m ) w ;;

(** {v sym_positive_float_random order rank range v} *)
let sym_positive_float_random = fun (r:int) (rank:int) (x:float) ->
 let j = rank_float_matrix r r rank
 and p = matrix_float_random r r x in
  matrix_float_twisted_prod p ( matrix_float_twisted_prod p j ) ;;

(** {v sym_positive_float_bal_random order rank range v} *)
let sym_positive_float_bal_random = fun (r:int) (rank:int) (x:float) ->
 let j = rank_float_matrix r r rank
 and p = matrix_float_bal_random r r x in
  matrix_float_twisted_prod p ( matrix_float_twisted_prod p j ) ;;

(** {v antisym_biased_float_random order range v} *)
let antisym_biased_float_random = fun (r:int) (x:float) ->
 let m = upper_nil_float_random r r x in
  float_antisym m ;;

(** {v antisym_float_bal_random order range v} *)
let antisym_float_bal_random = fun (r:int) (x:float) ->
 let m = upper_nil_float_bal_random r r x in
  float_antisym m ;;


(** {v special_float_pseudo_random order approx_quadratic_range v} The result is in SL(n,R),
the lower right coefficient is always [1.].

Le résultat est dans SL(n,R), le coefficient en bas à droite est toujours [1.]. *)
let special_float_pseudo_random = fun (r:int) (x:float) ->
 let a = upper_unip_float_bal_random r r x
 and b = upper_unip_float_bal_random r r x in
  matrix_float_twisted_prod a b ;;


(** {v special_float_random order approx_quartic_range v} The result is in SL(n,R).

Le résultat est dans SL(n,R). *)
let special_float_random = fun (r:int) (x:float) ->
 let rr = pred r in
  let a = special_float_pseudo_random r x
  and s = vector_float_sign_random rr
  and d = vector_float_random rr 2.
  and b = special_float_pseudo_random r x in
   let i = Random.int rr
   and ddd = vector_float_coeff_prod s d in
    let dd = Array.append ( Array.append ( Array.sub ddd 0 i ) [| 1. /. ( vector_float_contraction ddd ) |] ) ( Array.sub ddd i ( rr - i ) ) in
     matrix_float_twisted_prod a ( float_diag_left_mult dd b ) ;;



(** {v sym_int_bal_random order range v} *)
let sym_int_bal_random = fun (r:int) (x:int) ->
 let m = upper_trig_int_bal_random r r x in
  int_sym m ;;

(** {v sym_biased_int_random order range v} *)
let sym_biased_int_random = fun (r:int) (x:int) ->
 let m = upper_trig_int_random r r x in
  int_sym m ;;

(** {v sym_signed_int_random order range v} *)
let sym_signed_int_random = fun (r:int) (x:int) ->
 let m = sym_biased_int_random r x
 and w = matrix_int_random r r x in
  matrix_int_twisted_prod ( matrix_int_twisted_prod w m ) w ;;

(** {v sym_positive_int_random order rank range v} *)
let sym_positive_int_random = fun (r:int) (rank:int) (x:int) ->
 let j = rank_int_matrix r r rank
 and p = matrix_int_random r r x in
  matrix_int_twisted_prod p ( matrix_int_twisted_prod p j ) ;;

(** {v sym_positive_int_bal_random order rank range v} *)
let sym_positive_int_bal_random = fun (r:int) (rank:int) (x:int) ->
 let j = rank_int_matrix r r rank
 and p = matrix_int_bal_random r r x in
  matrix_int_twisted_prod p ( matrix_int_twisted_prod p j ) ;;

(** {v antisym_biased_int_random order range v} *)
let antisym_biased_int_random = fun (r:int) (x:int) ->
 let m = upper_nil_int_random r r x in
  int_antisym m ;;

(** {v antisym_int_bal_random order range v} *)
let antisym_int_bal_random = fun (r:int) (x:int) ->
 let m = upper_nil_int_bal_random r r x in
  int_antisym m ;;


(** {v special_int_pseudo_random order approx_quadratic_range v} The result is in SL(n,Z),
the lower right coefficient is always [1].

Le résultat est dans SL(n,Z), le coefficient en bas à droite est toujours [1]. *)
let special_int_pseudo_random = fun (r:int) (x:int) ->
 let a = upper_unip_int_bal_random r r 2
 and b = upper_unip_int_bal_random r r x in
  matrix_int_twisted_prod a b ;;


(** {v special_int_random order approx_quartic_range v} The result is in SL(n,Z).

Le résultat est dans SL(n,Z). *)
let special_int_random = fun (r:int) (x:int) ->
 let a = special_int_pseudo_random r x
 and d = vector_int_sign_random ( pred r )
 and b = special_int_pseudo_random r x in
  let dd = Array.append d [| vector_int_contraction d |] in
   matrix_int_twisted_prod a ( int_diag_left_mult dd b ) ;;


(** {v invertible_int_random order approx_quartic_range v} The result is in GL(n,Z).

Le résultat est dans GL(n,Z).*)
let invertible_int_random = fun (r:int) (x:int) ->
 let a = special_int_pseudo_random r x
 and d = vector_int_sign_random r
 and b = special_int_pseudo_random r x in
  matrix_int_twisted_prod a ( int_diag_left_mult d b ) ;;


(** {v gen_sympl_float half_dimension v} *)
let gen_sympl_float = function (n:int) ->
 let z = Float_matrix_cons ( null_float n n )
 and u = Float_matrix_cons ( identity_float n n )
 and uu = Float_matrix_cons ( scal_float n n (-1.) ) in
 matrix_float_demakeup ( matrix_foa_paste ( Foa_matrix_cons [| [| z ; uu |] ; [| u ; z |] |] ) ) ;;


(** {v gen_sympl_int half-dimension v} *)
let gen_sympl_int = function (n:int) ->
 let z = Int_matrix_cons ( null_int n n )
 and u = Int_matrix_cons ( identity_int n n )
 and uu = Int_matrix_cons ( scal_int n n (-1) ) in
 matrix_int_demakeup ( matrix_ioa_paste ( Ioa_matrix_cons [| [| z ; uu |] ; [| u ; z |] |] ) ) ;;


(** {v float_companion vector v} The vector contains the coefficients of a unitary polynomial 
(except the principal).

Le vecteur contient les coefficients d'un polynome unitaire (sauf le principal). *)
let float_companion = function (v:float array) ->
 let r = Array.length v in
  let rr = r - 1
  and m = matrix_float_nil r in
   let row = m.(rr) in
    for j = 0 to rr - 1 do
     row.(j) <- -. v.(j)
    done ;
    float_transpose m ;;


(** {v int_companion vector v} *)
let int_companion = function (v:int array) ->
 let r = Array.length v in
  let rr = r - 1
  and m = matrix_int_nil r in
   let row = m.(rr) in
    for j = 0 to rr do
     row.(j) <- - v.(j)
    done ;
    int_transpose m ;;


(** {v ortho_float_antisym matrix v} The input matrix is supposed to be antisymmetric.

La matrice entrante est supposée antisymétrique. *)
let ortho_float_antisym = function (a:float array array) ->
 let i = eye_float a in
  let b = matrix_float_plus i a in
   let c = float_approx_inv matrix_float_norm_inf float_inv b in
    matrix_float_minus ( matrix_float_scal_mult 2. c.(0) ) i ;;

(** {v generic_ortho_float_antisym matrix v} The input matrix is supposed to be antisymmetric.

La matrice entrante est supposée antisymétrique. *)
let generic_ortho_float_antisym = function (a:float array array) ->
 let i = eye_float a in
  let b = matrix_float_plus i a in
   let c = float_approx_inv matrix_float_norm_inf generic_float_inv b in
    matrix_float_minus ( matrix_float_scal_mult 2. c.(0) ) i ;;

(** {v ortho_biased_float_random order range v} *)
let ortho_biased_float_random = fun (r:int) (x:float) ->
 let a = antisym_biased_float_random r x in
  generic_ortho_float_antisym a ;;

(** {v ortho_float_bal_random order range v} *)
let ortho_float_bal_random = fun (r:int) (x:float) ->
 let a = antisym_float_bal_random r x in
  generic_ortho_float_antisym a ;;

(** {v sympl_float_sym matrix v} The input matrix is supposed to be symmetric.

La matrice entrante est supposée symétrique. *)
let sympl_float_sym = function (a:float array array) ->
 let i = eye_float a
 and l = Array.length a in
  let j = gen_sympl_float ( l / 2 ) in
   let b = matrix_float_prod a j in
    let c = matrix_float_plus i b in
     let d = float_approx_inv matrix_float_norm_inf float_inv c in
      matrix_float_minus ( matrix_float_scal_mult 2. d.(0) ) i ;;

(** {v sympl_biased_float_random half-order range v} *)
let sympl_biased_float_random = fun (r:int) (x:float) ->
 let a = sym_biased_float_random ( 2 * r ) x in
  let i = eye_float a
  and l = Array.length a in
   let j = gen_sympl_float ( l / 2 ) in
    let b = matrix_float_prod a j in
     let c = matrix_float_plus i b in
      let d = float_approx_inv matrix_float_norm_inf generic_float_inv c in
       matrix_float_minus ( matrix_float_scal_mult 2. d.(0) ) i ;;

(** {v sympl_float_bal_random half-order range v} *)
let sympl_float_bal_random = fun (r:int) (x:float) ->
 let a = sym_float_bal_random ( 2 * r ) x in
  let i = eye_float a
  and l = Array.length a in
   let j = gen_sympl_float ( l / 2 ) in
    let b = matrix_float_prod a j in
     let c = matrix_float_plus i b in
      let d = float_approx_inv matrix_float_norm_inf generic_float_inv c in
       matrix_float_minus ( matrix_float_scal_mult 2. d.(0) ) i ;;


(** {v givens_matrix order first_index second_index angle v} *)
let givens_matrix = fun (r:int) (i:int) (j:int) (t:float) ->
 let w = identity_float r r in
  if i < j && j < r then
   begin
    let row = w.(i)
    and ligne = w.(j)
    and c = cos t
    and s = sin t in 
     row.(i) <- c ;
     row.(j) <- -. s ;
     ligne.(i) <- s ;
     ligne.(j) <- c ;
   end ;
  if j < i && i < r then
   begin
    let row = w.(i)
    and ligne = w.(j)
    and c = cos t
    and s = sin t in 
     row.(i) <- c ;
     row.(j) <- s ;
     ligne.(i) <- -. s ;
     ligne.(j) <- c ;
   end ;
   w ;;


(** {v jacobi_step_angle threshold Mii Mjj Mij v} *)
let jacobi_step_angle = fun (threshold:float) (x:float) (y:float) (z:float) ->
 if abs_float ( x -. y ) < threshold then atan 1.
 else 0.5 *. ( atan2 ( y -. x ) ( 2. *. z ) ) ;;


(** {v sym_float_classical_jacobi_step threshold matrix v} Output : the transformed matrix of the matrix [m], 
the left factor of the previous triple product, the right factor of the triple product.

Input : threshold for [jacobi_step_angle], symmetric matrix.

Entrée : seuil pour [jacobi_step_angle], matrice symétrique.

Sortie : la matrice transformée de la matrice [m], le facteur de gauche du triple produit précédent, le facteur de droite du triple produit. *)
let sym_float_classical_jacobi_step = fun (threshold:float) (m:float array array) ->
 let mm = matrix_float_abs ( matrix_float_non_diag_part m )
 and w = ref ( matrix_float_copy m )
 and r = Array.length m in
  let mmm = matrix_max mm
  and g = ref ( identity_float r r )
  and gg = ref ( identity_float r r ) in
(** Une recherche polymorphe fonctionne mieux qu'une recherche dans la catégorie [float],
probablement à cause de troncatures de mantisses : 
A polymorphic query operates better than a query inside the [float] category, 
probably because of truncatures of mantissas. *)
   let index = matrix_float_find_first_last mmm mm in
    let i = index.(0)
    and j = index.(1) in
     if i <> j then
      begin
       let ii = min i j
       and jj = max i j in
        let row = m.(ii) in
         let t = jacobi_step_angle threshold row.(ii) m.(jj).(jj) row.(jj) in
          g := givens_matrix r ii jj t ;
          gg := givens_matrix r ii jj ( -. t ) ;
          w := matrix_float_triple_prod !gg m !g
      end ;
      [| !w ; !gg ; !g |] ;;


(** {v sym_float_classical_jacobi_reduc step_threshold elasticity diag_threshold max_steps matrix v} 
Input : threshold for [jacobi_step_angle], elasticity to rule overshoot, threshold for non-diagonality norm, maximal number of steps, symmetric matrix.

Output : approximate eigenvalues, left orthogonal transformation whose rows 
are the approximate eigenvectors, final iteration close to the diagonal matrix, 
right orthogonal transformation whose columns are the approximate eigenvectors,
measure of non_diagonality.

The elasticity is supposed to be between 0 and 1. The more it gets closer to zero,
the more one allows the error to diagonality to climb up between two steps.

Entrée : seuil pour [jacobi_step_angle], élasticité pour réguler l'outrepassement, seuil pour la norme de non-diagonalité, nombre maximal de pas, matrice symétrique.

Sortie : valeurs propres approchées, changement de coordonnées isométrique gauche dont les lignes 
sont les vecteurs propres approchés, itération finale approchant la matrice diagonalisée, 
changement de coordonnées isométrique droit dont les colonnes sont les vecteurs propres approchés, 
écart à la diagonalité.

L'élasticité est censée être comprise entre 0 et 1. Plus on s'approche de zéro, 
plus on tolère une remontée de l'écart à la diagonalité entre deux pas. *)
let sym_float_classical_jacobi_reduc = fun (step_threshold:float) (elasticity:float) (diag_threshold:float) (s:int) (m:float array array) ->
 let measure = ref ( matrix_float_non_diagonality matrix_float_square_frobenius m )
 and old_measure = ref max_float
 and steps = ref 0
 and mm = ref m
 and old_mm = ref m
 and r = Array.length m in
  let i = identity_float r r in
   let left = ref i
   and right = ref i in
    while ( !measure > diag_threshold ) && ( ( !old_measure > elasticity *. !measure ) && ( !steps < s ) ) do
     let resultat = sym_float_classical_jacobi_step step_threshold !mm in
      old_mm := !mm ;
      mm := resultat.(0) ;
      old_measure := !measure ;
      measure := matrix_float_non_diagonality matrix_float_square_frobenius !mm ;
      if elasticity *. !measure < !old_measure then
       begin
        left := matrix_float_prod resultat.(1) !left ;
        right := matrix_float_prod !right resultat.(2) ;
        steps := !steps + 1 ;
       end
      else
       begin
        mm := !old_mm ;
        measure := !old_measure ;
        steps := max_int ;
       end ;
    done ;
    [| [| extract_diag !mm |] ; !left ; !mm ; !right ; [|[| !measure |]|] |] ;;


(** {v sym_float_adapt matrix v} This is taken from the HP15C calulator's high level mathematical functions manual.

WARNING : THE SPECIFICATIONS FOR THIS ALGORITHM ARE UNKNOWN TO US.
CERTAIN SYMMETRIC MATRICES MAY PREVENT THIS ALGORITHM TO START: FOR EXAMPLE [\[| \[| 1. ; 1. |\] ; \[| 1. ; 1. |\] |\]].

The matrix obtained through orthogonal transformation should be closer to a diagonal matrix.

Output : the transformed matrix of the matrix [m], the left factor of the previous triple product, the right factor of the triple product.


La matrice obtenue par changement de coordonnées isométrique est censée se rapprocher d'une matrice diagonale.


AVERTISSEMENT : LES SPÉCIFICATIONS POUR CET ALGORITHME NOUS SONT INCONNUES.
CERTAINES MATRICES SYMÉTRIQUES PEUVENT EMPÊCHER CET ALGORITHME DE DÉMARRER : PAR EXEMPLE [\[| \[| 1. ; 1. |\] ; \[| 1. ; 1. |\] |\]].

Ceci provient du manuel des fonctions mathématiques de haut niveau de la calculette HP15C. 

Sortie : la matrice transformée de la matrice [m], le facteur de gauche du triple produit précédent, le facteur de droite du triple produit.*)
let sym_float_adapt = function (m:float array array) ->
 let r = Array.length m in
  let a = Array.make_matrix r r 0.
  and d = extract_diag m
  and rr = r - 1 in
   for i = 0 to rr do
    let row_input = m.(i)
    and row_output = a.(i) in
     let aa = d.(i) in
      for j = 0 to rr do
       if ( i <> j ) && ( row_input.(j) <> 0. ) then
        begin
         let angle = 2. *. row_input.(j) /. ( aa -. d.(j) ) in
          row_output.(j) <- tan ( (atan angle) /. 4. )
        end
       done ;
      done ;
      let b = float_antisym a in
       let q = ortho_float_antisym b in
        let qq = float_transpose q in
         [| matrix_float_triple_prod qq m q ; qq ; q |] ;;


(** {v sym_float_reduc threshold max_steps matrix v} This is taken from the HP15C calulator's high level mathematical functions manual.

WARNING : THE SPECIFICATIONS FOR THIS ALGORITHM ARE UNKNOWN TO US.
CERTAIN SYMMETRIC MATRICES MAY PREVENT THIS ALGORITHM TO START: FOR EXAMPLE [\[| \[| 1. ; 1. |\] ; \[| 1. ; 1. |\] |\]].

Input : threshold for non-diagonality norm, maximal number of steps, symmetric matrix.

Output : approximate eigenvalues, left orthogonal transformation whose rows 
are the approximate eigenvectors, final iteration close to the diagonal matrix, 
right orthogonal transformation whose columns are the approximate eigenvectors,
measure of non_diagonality.

A threshold equal to [ sqrt ( epsilon_float ) ] is good for small orders.
A bigger threshold is recommended for big orders.

AVERTISSEMENT : LES SPÉCIFICATIONS POUR CET ALGORITHME NOUS SONT INCONNUES.
CERTAINES MATRICES SYMÉTRIQUES PEUVENT EMPÊCHER CET ALGORITHME DE DÉMARRER : PAR EXEMPLE [\[| \[| 1. ; 1. |\] ; \[| 1. ; 1. |\] |\]].

Entrée : seuil pour la norme de non-diagonalité, nombre maximal de pas, matrice symétrique.

Sortie : valeurs propres approchées, changement de coordonnées isométrique gauche dont les lignes 
sont les vecteurs propres approchés, itération finale approchant la matrice diagonalisée, 
changement de coordonnées isométrique droit dont les colonnes sont les vecteurs propres approchés, 
écart à la diagonalité.

Un seuil égal à [ sqrt ( epsilon_float ) ] convient aux petits ordres.
Augmenter le seuil pour les grands ordres.

Ceci provient du manuel des fonctions mathématiques de haut niveau de la calculette HP15C. 
Certaines matrices symétriques peuvent mettre cet algorithme en échec :
utiliser sym_float_tune_reduc à la place.
*)
let sym_float_reduc = fun (threshold:float) (s:int) (m:float array array) ->
 let measure = ref max_float
 and steps = ref 0
 and mm = ref m
 and r = Array.length m in
  let i = identity_float r r in
   let left = ref i
   and right = ref i in
    while ( !measure > threshold ) && ( !steps < s ) do
     let resultat = sym_float_adapt !mm in
      mm := resultat.(0) ;
      left := matrix_float_prod resultat.(1) !left ;
      right := matrix_float_prod !right resultat.(2) ;
      measure := matrix_float_non_diagonality matrix_float_norm_inf !mm ;
      steps := !steps + 1 ;
    done ;
    [| [| extract_diag !mm |] ; !left ; !mm ; !right ; [|[| !measure |]|] |] ;;


(** {v sym_float_tune_reduc threshold max_steps matrix v} This may give better results than [sym_float_reduc].

WARNING : THE SPECIFICATIONS FOR THIS ALGORITHM ARE UNKNOWN TO US.
WE DO NOT KNOW FOR WHICH SYMMETRIC MATRIX THE ALGORITHM STARTS.

The input and output have the same form as for [sym_float_reduc].

Les résultats peuvent être meilleurs qu'avec [sym_float_reduc].

AVERTISSEMENT : LES SPÉCIFICATIONS POUR CET ALGORITHME NOUS SONT INCONNUES.
NOUS NE SAVONS PAS POUR QUELLES MATRICES SYMÉTRIQUES CET ALGORITHME DÉMARRE.

Les entrée et sortie ont la même forme que pour [sym_float_reduc]. *)
let sym_float_tune_reduc = fun (threshold:float) (s:int) (m:float array array) ->
 let g = ortho_float_bal_random (Array.length m) 0.1 in
  let gg = float_transpose g in
   let mm = matrix_float_prod gg ( matrix_float_prod m g ) in
    let result = sym_float_reduc threshold s mm in
     let q = matrix_float_prod g result.(3)
     and qq = matrix_float_prod result.(1) gg in
      [| result.(0) ; qq ; result.(2) ; q ; result.(4) |] ;;

(** {v sym_float_sort_reduc methode matrix v} The eigenvalues are sorted in increasing order.

The reduction method [methode] must be stated with all its parameters.

Output : approximate eigenvalues, left orthogonal transformation whose rows 
are the approximate eigenvectors, final iteration close to the diagonal matrix, 
right orthogonal transformation whose columns are the approximate eigenvectors,
measure of non_diagonality.

Les valeurs propres sont triées dans l'ordre croissant.

La méthode de réduction [methode] doit être précisée avec tous ses paramètres.

Sortie : valeurs propres approchées, changement de coordonnées isométrique gauche dont les lignes 
sont les vecteurs propres approchés, itération finale approchant la matrice diagonalisée, 
changement de coordonnées isométrique droit dont les colonnes sont les vecteurs propres approchés, 
écart à la diagonalité. *)
let sym_float_sort_reduc = fun methode (m:float array array) ->
 let r = methode m
 and l = Array.length m in
  let d = r.(0).(0)
  and left = ref r.(1)
  and right = ref r.(3)
  and index = Array.make l 0 in
   let dd = vector_float_copy d in
    Array.fast_sort compare dd ;
    for i = 0 to l - 1 do
     let j = vector_float_find_first dd.(i) d in
      d.(j) <- max_float ;
      index.(i) <- j ;
      right := exchange_column i j !right ;
      left := exchange_row i j !left ;
    done ;
    let result = matrix_float_triple_prod !left m !right in 
     let measure = matrix_float_non_diagonality matrix_float_norm_inf result in
      [| [| dd |] ; !left ; result ; !right ; [|[| measure |]|] |] ;;


(** {v float_trans_orthonormalize matrix v} The rows of the matrix form the basis of the subspace in question.

Les lignes de la matrices forment la base du sous-espace considéré. *)
let float_trans_orthonormalize = fun (m:float array array) ->
 let r = Array.length m
 and init = m.(0) in
  let c = Array.length init
  and accu = matrix_float_copy m
  and rr = r - 1 in
   let result = Array.make_matrix r c 0. in
    result.(0) <- vector_float_scal_left_div ( vector_float_norm_2 init ) init ;
    for i = 1 to rr do
     let row = ref accu.(i) in
      for ii = 0 to i - 1 do
       let x = ( vector_float_scal_prod !row result.(ii) ) in
        row := vector_float_minus !row ( vector_float_scal_mult x result.(ii) )
      done ;
      result.(i) <- vector_float_scal_left_div ( vector_float_norm_2 !row) !row
    done ;
    result ;;


(** {v float_orthonormalize matrix v} The columns of the matrix form the basis of the subspace in question.

Les colonnes de la matrices forment la base du sous-espace considéré. *)
let float_orthonormalize = fun (m:float array array) ->
 let mm = float_transpose m in
  float_transpose ( float_trans_orthonormalize mm ) ;;


(** {v sym_float_indirect_reduc factor methode_reduc matrix v} 
Output : approximate eigenvalues, left orthogonal transformation whose rows 
are the approximate eigenvectors, final iteration close to the diagonal matrix, 
right orthogonal transformation whose columns are the approximate eigenvectors,
measure of non_diagonality.

Sortie : valeurs propres approchées, changement de coordonnées isométrique gauche dont les lignes 
sont les vecteurs propres approchés, itération finale approchant la matrice diagonalisée, 
changement de coordonnées isométrique droit dont les colonnes sont les vecteurs propres approchés, 
écart à la diagonalité. *)
let sym_float_indirect_reduc = fun methode_reduc (m:float array array) ->
 let res = methode_reduc m in
  let p = res.(3) in
   let q = float_trans_orthonormalize p in
    let pp = float_transpose q in
     let d = matrix_float_triple_prod pp m q in
      let result = methode_reduc d in
       [| result.(0) ; matrix_float_prod pp result.(1) ; result.(2) ; matrix_float_prod result.(3) q ; result.(4) |] ;;


(** {v float_pca methode_reduc matrix v} The columns of the input matrix are the coordinates of the sample points.
The reduction methode [methode_reduc] applies to symmetric matrices ; it must contain all the parameters.
Output : a two_rows matrix containing the isobarycentre and the eigenvalues, the left factor in the diagonalization, 
the right factor in the diagonalization, the measure of non diagonality.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres.
Les colonnes de la matrice entrante forment les coordonnées des points-échantillons.
Sortie : une matrice à deux lignes contenant l'isobarycentre et les valeurs propres, le facteur de gauche de la diagonalisation,
le facteur de droite de la diagonalisation, la mesure de non-diagonalité. *)
let float_pca = fun methode_reduc (x:float array array) ->
 let cc = float ( Array.length x.(0) ) in
  let xbar = matrix_float_mean_by_row x in
   let xxx = Array.mapi ( function i -> vector_float_scal_left_sub xbar.(i) ) x in
    let xx = matrix_float_scal_left_div ( sqrt cc ) xxx in
     let z = matrix_float_twisted_prod xx xx in
      let zz = methode_reduc z in
       [| [| xbar ; zz.(0).(0) |] ; zz.(1) ; zz.(2) ; zz.(3) ; zz.(4) |] ;;


(** {v float_iterate steps matrix vector v} *)
let float_iterate = fun (s:int) (x:float array array) (v:float array) ->
 let y = ref v in
  if s >= 0 then
   begin
    for i = 1 to s do
     y := matrix_vector_float_prod x !y
    done
   end
  else
   begin
    let xx =  ( float_approx_inv matrix_float_norm_inf float_inv x ).(0) in
     for i = 1 to abs s do
      y := matrix_vector_float_prod xx !y
     done
   end ;
   !y ;;


(** {v float_normalized_iterate norm steps matrix vector v} *)
let float_normalized_iterate = fun norm (s:int) (x:float array array) (v:float array) ->
 if s < 0 then failwith "Negative power forbidden in Matrix.float_normalized_iterate." ;
 let y = ref ( vector_float_scal_left_div ( norm v ) v )
 and i = ref 1 in
  let z = ref !y in
   while !i <= s do
    z := matrix_vector_float_prod x !y ;
    z := vector_float_scal_left_div ( norm !z ) !z ;
    if norm ( vector_float_minus !z !y ) == 0. then i := max_int
    else ( i := !i + 1 ; y := !z ) ;
   done ;
   !z ;;


(** {v float_power steps matrix v} *)
let rec float_power = fun (s:int) (x:float array array) ->
 if s >= 0 then
  begin
   if s == 0 then eye_float x
   else
    let n = s / 2 in
     let factor = float_power n x in
      let prod = matrix_float_prod factor factor in
       if s mod 2 == 0 then prod
       else matrix_float_prod prod x
  end
 else
  begin
   let xx = ( float_approx_inv matrix_float_norm_inf float_inv x ).(0) in
    float_power ( abs s ) xx
  end ;;


(** {v float_nilpotence_order norm threshold matrix v} *)
let float_nilpotence_order = fun norm (threshold:float) (x:float array array) ->
 let s = ref 1. in
  if norm x >= threshold then
   begin
    let r = Array.length x
    and factor = ref x
    and exponent = ref 1 in
     while !exponent < r do
      begin
       let prod = matrix_float_prod !factor x in
        if norm prod < threshold then ( s := 1. +. ( float !exponent ) ; exponent := max_int )
        else ( factor := prod ; s := infinity ; exponent := succ !exponent ) ;
      end
     done ;
   end ;
  !s ;;


(** {v generic_float_iterate steps matrix vector v} *)
let generic_float_iterate = fun (s:int) (x:float array array) (v:float array) ->
 let y = ref v in
  if s >= 0 then
   begin
    for i = 1 to s do
     y := matrix_vector_float_prod x !y
    done
   end
  else
   begin
    let xx =  ( float_approx_inv matrix_float_norm_inf generic_float_inv x ).(0) in
     for i = 1 to abs s do
      y := matrix_vector_float_prod xx !y
     done
   end ;
   !y ;;


(** {v generic_float_power steps matrix v} *)
let rec generic_float_power = fun (s:int) (x:float array array) ->
 if s >= 0 then
  begin
   if s == 0 then eye_float x
   else
    let n = s / 2 in
     let factor = float_power n x in
      let prod = matrix_float_prod factor factor in
       if s mod 2 == 0 then prod
       else matrix_float_prod prod x
  end
 else
  begin
   let xx = ( float_approx_inv matrix_float_norm_inf generic_float_inv x ).(0) in
    float_power ( abs s ) xx
  end ;;


(** {v int_iterate steps matrix vector v} *)
let int_iterate = fun (s:int) (x:int array array) (v:int array) ->
 let y = ref v in
  if s >= 0 then
   begin
    for i = 1 to s do
     y := matrix_vector_int_prod x !y
    done
   end
  else
   begin
    let xx = int_inv x in
     for i = 1 to abs s do
      y := matrix_vector_int_prod xx !y
     done
   end ;
   !y ;;


(** {v int_normalized_iterate norm steps matrix vector v} *)
let int_normalized_iterate = fun norm (s:int) (x:int array array) (v:int array) ->
 if s < 0 then failwith "Negative power forbidden in Matrix.int_normalized_iterate." ;
 let y = ref ( vector_int_scal_left_div ( norm v ) v )
 and i = ref 1 in
  let z = ref !y in
   while !i <= s do
    z := matrix_vector_int_prod x !y ;
    z := vector_int_scal_left_div ( norm !z ) !z ;
    if norm ( vector_int_minus !z !y ) == 0 then i := max_int
    else ( i := !i + 1 ; y := !z ) ;
   done ;
   !z ;;


(** {v int_power steps matrix v} *)
let rec int_power = fun (s:int) (x:int array array) ->
 if s >= 0 then
  begin
   if s == 0 then eye_int x
   else
    let n = s / 2 in
     let factor = int_power n x in
      let prod = matrix_int_prod factor factor in
       if s mod 2 == 0 then prod
       else matrix_int_prod prod x
  end
 else
  begin
   let xx = int_inv x in
    int_power ( abs s ) xx
  end ;;


(** {v int_nilpotence_order threshold matrix v} *)
let int_nilpotence_order = function (x:int array array) ->
 let s = ref 1. in
  if matrix_int_norm_inf x <> 0 then
   begin
    let r = Array.length x
    and factor = ref x
    and exponent = ref 1 in
     while !exponent < r do
      begin
       let prod = matrix_int_prod !factor x in
        if matrix_int_norm_inf prod == 0 then ( s := 1. +. ( float !exponent ) ; exponent := max_int )
        else ( factor := prod ; s := infinity ; exponent := succ !exponent ) ;
      end
     done ;
   end ;
  !s ;;


(** {v float_principal steps matrix vector v} *)
let float_principal = fun (s:int) (x:float array array) (v:float array) ->
 let n = ref ( vector_float_norm_inf v )
 and accu = ref 0. in
  let y = ref ( vector_float_scal_left_div !n v ) in
   if s > 0 then
    begin
     for i = 1 to s do
      y := matrix_vector_float_prod x !y ;
      n := vector_float_norm_inf !y ;
      accu := !accu +. log !n ;
      y := vector_float_scal_left_div !n !y
     done ;
     accu := !accu /. ( float s )
    end
   else
    begin
     let xx =  ( float_approx_inv matrix_float_norm_inf float_inv x ).(0) in
      for i = 1 to abs s do
       y := matrix_vector_float_prod xx !y ;
       n := vector_float_norm_inf !y ;
       accu := !accu +. log !n ;
       y := vector_float_scal_left_div !n !y
      done ;
     accu := !accu /. ( abs_float ( float s ) )
    end ;
    [| [| !accu |] ; !y |] ;;


(** {v generic_float_principal steps matrix vector v} *)
let generic_float_principal = fun (s:int) (x:float array array) (v:float array) ->
 let n = ref ( vector_float_norm_inf v )
 and accu = ref 0. in
  let y = ref ( vector_float_scal_left_div !n v ) in
   if s > 0 then
    begin
     for i = 1 to s do
      y := matrix_vector_float_prod x !y ;
      n := vector_float_norm_inf !y ;
      accu := !accu +. log !n ;
      y := vector_float_scal_left_div !n !y
     done ;
     accu := !accu /. ( float s )
    end
   else
    begin
     let xx =  ( float_approx_inv matrix_float_norm_inf generic_float_inv x ).(0) in
      for i = 1 to abs s do
       y := matrix_vector_float_prod xx !y ;
       n := vector_float_norm_inf !y ;
       accu := !accu +. log !n ;
       y := vector_float_scal_left_div !n !y
      done ;
     accu := !accu /. ( abs_float ( float s ) )
    end ;
    [| [| !accu |] ; !y |] ;;


(** {v float_Gram matrix v} The vectors in question are the rows of the matrix.

Les vecteurs à étudier forment les lignes de la matrice. *)
let float_Gram = function (x:float array array) ->
 matrix_float_twisted_prod x x ;;


(** {v int_Gram matrix v} The vectors in question are the rows of the matrix.

Les vecteurs à étudier forment les lignes de la matrice. *)
let int_Gram = function (x:int array array) ->
 matrix_int_twisted_prod x x ;;


(** {v float_rank matrix v} *)
let rec float_rank = function (m:float array array) ->
 let r = ref ( Array.length m )
 and c = ref ( Array.length m.(0) )
 and m_m = ref m in
  if !c < !r then
   begin
    m_m := float_transpose m ; 
    c := !r ;
    r := Array.length !m_m ;
   end ;
   match !r with
   | 0 -> 0
   | 1 -> if vector_float_norm_inf !m_m.(0) = 0. then 0 else 1
   | 2 ->
    begin
     let v = !m_m.(0) in
      let vv = vector_float_abs v in
       let x = vector_float_max vv in
        if x = 0. then float_rank [| !m_m.(1) |]
        else
         begin
          let i = vector_float_find_twin x vv in
           let w = !m_m.(1) in
            let ww = vector_float_minus w ( vector_float_scal_mult ( w.(i) /. v.(i) ) v ) in
             1 + float_rank [| ww |]
         end
    end
   | _ ->
    begin
     let v = !m_m.(0) in
      let vv = vector_float_abs v in
       let x = vector_float_max vv in
        if x = 0. then float_rank ( float_sub_matrix !m_m 1 (!r - 1) 0 (!c - 1) )
        else
         begin
          let i = vector_float_find_twin x vv in
           let w = float_sub_matrix !m_m 1 (!r - 1) 0 (!c - 1) in
            for j = 0 to !r - 2 do
             w.(j) <- vector_float_minus w.(j) ( vector_float_scal_mult ( w.(j).(i) /. v.(i) ) v )
            done ;
            1 + float_rank w
         end
    end ;;


(** {v float_im matrix v} *)
let float_im = function (m:float array array) ->
 let rank = float_rank m
 and r = Array.length m
 and c = Array.length m.(0)
 and i = ref 0 in
  let rr = r - 1
  and w = Array.make_matrix rank r 0. in
   while !i < rank do
    w.(!i) <- matrix_vector_float_prod m ( vector_float_random c 1. ) ;
    let ww = float_sub_matrix w 0 !i 0 rr in
     if float_rank ww = !i + 1 then i := !i + 1
   done ;
   w ;;


(** {v float_ker methode_reduc threshold matrix v} The rows of the output form a basis of the kernel of the matrix.
The reduction methode [methode_reduc] applies to symmetric matrices ; it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres.
Les lignes de la sortie forment une base du noyau de la matrice. *)
let float_ker = fun methode_reduc (threshold:float) (m:float array array) ->
 let mm = float_transpose m
 and c = Array.length m.(0) in
  let s = matrix_float_twisted_prod mm mm
  and result = ref [] in
   let p = methode_reduc s in
    let diag = p.(0).(0)
    and base = p.(1) in
     for i = 0 to c - 1 do
      if abs_float diag.(i) <= threshold then
       begin
        result :=  base.(i) :: !result 
       end
     done ;
     Array.of_list !result ;;


(** {v int_rank matrix v} *)
let int_rank = function (m:int array array) ->
 let mm = float_of_matrix m in
  float_rank mm ;;


(** {v int_im matrix v} *)
let int_im = function (m:int array array) ->
 let rank = int_rank m
 and r = Array.length m
 and c = Array.length m.(0)
 and i = ref 0
 and accu = ref 1 in
  let rr = r - 1
  and w = Array.make_matrix rank r 0 in
   while !i < rank do
    w.(!i) <- matrix_vector_int_prod m ( vector_int_bal_random c !accu ) ;
    let ww = int_sub_matrix w 0 !i 0 rr in
     if int_rank ww = !i + 1 then ( i := !i + 1 ; accu := 1 ) else accu := 1 + !accu
   done ;
   w ;;


(** {v hyper_proj v} *)
let hyper_proj = fun v w ->
 vector_float_minus w ( vector_float_scal_mult ( ( vector_float_scal_prod v w ) /. ( vector_float_norm_2 v ) ) v )


(** {v float_ortho_proj matrix vector v} The columns of the matrix are required to be a basis of the subspace of the orthogonal projection.

Les colonnes de la matrice sont censées former une base du sous-espace de projection orthogonale. *)
let float_ortho_proj = fun (m:float array array) (v:float array) ->
 let r = Array.length m
 and c = Array.length m.(0)
 and mm = float_transpose m in
  let accu = Array.make r 0.
  and rr = r - 1
  and cc = c - 1 in
   for j = 0 to cc do
    let column = mm.(j) in
     let x = ( ( vector_float_scal_prod v column ) /. vector_float_square_norm_2 column ) in
      for i = 0 to rr do
       accu.(i) <- accu.(i) +. column.(i) *. x
      done
   done ;
   accu ;;


(** {v float_trans_ortho_proj matrix vector v} The rows of the matrix are required to be a basis of the subspace of the orthogonal projection.

Les lignes de la matrice sont censées former une base du sous-espace de projection orthogonale. *)
let float_trans_ortho_proj = fun (m:float array array) (v:float array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let accu = Array.make c 0.
  and rr = r - 1
  and cc = c - 1 in
   for i = 0 to rr do
    let row = m.(i) in
     let x = ( ( vector_float_scal_prod v row ) /. vector_float_square_norm_2 row ) in
      for j = 0 to cc do
       accu.(j) <- accu.(j) +. row.(j) *. x
      done
   done ;
   accu ;;


(** {v float_ortho_sym matrix vector v} The columns of the matrix are the generators of the subspace of the orthogonal symmetry.

Les colonnes de la matrices forment les générateurs du sous-espace de symétrie orthogonale. *)
let float_ortho_sym = fun (m:float array array) (v:float array) ->
 vector_float_minus ( vector_float_scal_mult 2. ( float_ortho_proj m v ) ) v ;;


(** {v float_trans_ortho_sym matrix vector v} The rows of the matrix are the generators of the subspace of the orthogonal symmetry.

Les lignes de la matrices forment les générateurs du sous-espace de symétrie orthogonale. *)
let float_trans_ortho_sym = fun (m:float array array) (v:float array) ->
 vector_float_minus ( vector_float_scal_mult 2. ( float_trans_ortho_proj m v ) ) v ;;


(** {v vector_float_chose_3 vector v} The aim is to complete the image of a vector by a homothety into an orthonormal basis
whose second vector is included in the horizontal plane. The vectors of the basis rae output by row.

Il s'agit de compléter l'homothétique unitaire d'un vecteur quelconque de R^3 en une base orthonormée dont le deuxième vecteur est dans le plan horizontal.
Les vecteurs de la base sortent par ligne. *)
let vector_float_chose_3 = fun (u:float array) ->
 let a = u.(0)
 and b = u.(1)
 and c = u.(2) in
  let d = a *. a +. b *. b in
   if d <> 0. then 
    let e = c *. c +. d in
     let eee = sqrt e in
      let ee = 1. /. eee in
       let uu = vector_float_scal_mult ee u
       and dd = 1. /. ( sqrt d ) in
        let v = [| -. b *. dd ; a *. dd ; 0. |] in
         if c > 0. then [| uu ; v ; Util.vector_float_prod_3 uu v |]
         else  [| uu ; v ; Util.vector_float_prod_3 v uu |]
   else 
    if c > 0. then 
     [| [| 0. ; 0. ; 1. |] ; [| 1. ; 0. ; 0. |] ; [| 0. ; 1. ; 0. |] |]
    else 
     [| [| 0. ; 0. ; -1. |] ; [| 1. ; 0. ; 0. |] ; [| 0. ; 1. ; 0. |] |] ;;



(** {v sym_float_pinv methode_reduc threshold matrix v} This comes from the [scilab] source code, 
restricted to the case when the matrix is symmetric.

Ceci provient du code source de [scilab], restreint au cas où la matrice est symétrique. *)
let sym_float_pinv = fun methode_reduc (threshold:float) (m:float array array) ->
 let c = Array.length m.(0)
 and p = methode_reduc m in
  let diag = p.(0).(0)
  and base = p.(1)
  and tbase = p.(3) in
   for i = 0 to c - 1 do
    if abs_float diag.(i) > threshold then
     diag.(i) <- 1. /. diag.(i)
    else diag.(i) <- 0.
   done ;
   matrix_float_prod tbase ( float_diag_left_mult diag base ) ;;


(** {v sym_float_apply methode_reduc function matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_apply = fun methode_reduc (f:float -> float) (m:float array array) ->
 let c = Array.length m.(0)
 and p = methode_reduc m in
  let diag = p.(0).(0)
  and base = p.(1)
  and tbase = p.(3) in
   for i = 0 to c - 1 do
    diag.(i) <- f diag.(i)
   done ;
   matrix_float_prod tbase ( float_diag_left_mult diag base ) ;;


(** {v sym_float_sqrt methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_sqrt = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc sqrt m ;;


(** {v sym_float_log methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_log = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc log m ;;


(** {v sym_float_log10 methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_log10 = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc log10 m ;;


(** {v sym_float_cos methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_cos = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc cos m ;;


(** {v sym_float_sin methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_sin = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc sin m ;;


(** {v sym_float_tan methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_tan = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc tan m ;;


(** {v sym_float_acos methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_acos = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc acos m ;;


(** {v sym_float_asin methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_asin = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc asin m ;;


(** {v sym_float_atan methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_atan = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc atan m ;;


(** {v sym_float_cosh methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_cosh = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc cosh m ;;


(** {v sym_float_sinh methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_sinh = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc sinh m ;;


(** {v sym_float_tanh methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_tanh = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc tanh m ;;


(** {v sym_float_abs_float methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_abs_float = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc abs_float m ;;


(** {v sym_float_frac methode_reduc matrix v} The reduction methode [methode_reduc] applies to symmetric matrices ; 
it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres. *)
let sym_float_frac = fun methode_reduc (m:float array array) ->
 sym_float_apply methode_reduc Util.frac m ;;


(** {v clean_inv matrix v} *)
let clean_inv = function m ->
( float_tune_inv matrix_float_norm_inf m ( float_slow_inv m ) ).(0) ;;


(** {v slow_clean_inv matrix v} *)
let slow_clean_inv = function m ->
( slow_float_tune_inv matrix_float_norm_inf m ( float_slow_inv m ) ).(0) ;;

(** {v matrix_float_singular_random rows columns rank range v} *)
let matrix_float_singular_random = fun (r:int) (c:int) (rank:int) (x:float) ->
 let j = rank_float_matrix r c rank
 and p = matrix_float_random r c x
 and q = matrix_float_random r c x in
  matrix_float_triple_prod p j q ;;

(** {v matrix_float_singular_bal_random rows columns rank range v} *)
let matrix_float_singular_bal_random = fun (r:int) (c:int) (rank:int) (x:float) ->
 let j = rank_float_matrix r c rank
 and p = matrix_float_bal_random r c x
 and q = matrix_float_bal_random r c x in
  matrix_float_triple_prod p j q ;;




(** {C § } *)
(**
{2 {C Expérimentations diverses
---
Miscellanous experimentations} }
*)
(** {C  } *)





(** {v right_float_pinv methode_reduc matrix v} This functions, but it is slow.
The reduction methode [methode_reduc] applies to symmetric matrices ; it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres.
Cela fonctionne, mais c'est lent. *)
let right_float_pinv = fun methode_reduc (threshold:float) m ->
 let mm = float_transpose m in
  let mmm = matrix_float_twisted_prod mm mm in
   let m_m = sym_float_pinv methode_reduc threshold mmm in
    matrix_float_twisted_prod m_m mm ;;


(** {v left_float_pinv methode_reduc matrix v} This functions, but it is slow.
The reduction methode [methode_reduc] applies to symmetric matrices ; it must contain all the parameters.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques ; elle doit comprendre tous ses paramètres.
Cela fonctionne, mais c'est lent. *)
let left_float_pinv = fun methode_reduc (threshold:float) m ->
 let mm = matrix_float_twisted_prod m m in
   let m_m = sym_float_pinv methode_reduc threshold mm in
    matrix_float_prod (float_transpose m) m_m ;;



(** {v sym_float_reduc_seq threshold max_steps matrix v} Some symmetric matrices may cause this algorithm to fail: use sym_float_tune_reduc instead.

Input : threshold for non-diagonality norm, maximal number of steps, symmetric matrix.

Output : final iteration close to the diagonal matrix, left orthogonal transformation whose rows are the approximate eigenvectors, 
right orthogonal transformation whose columns are the approximate eigenvectors.

Entrée : seuil pour la norme de non-diagonalité, nombre maximal de pas, matrice symétrique.

Sortie : itération finale approchant la matrice diagonalisée, changement de coordonnées isométrique gauche dont les lignes 
sont les vecteurs propres approchés, changement de coordonnées isométrique droit dont les colonnes sont les vecteurs propres approchés.

Certaines matrices symétriques peuvent mettre cet algorithme en échec :
utiliser sym_float_tune_reduc_seq à la place.
*)
let sym_float_reduc_seq = fun (threshold:float) (s:int) (m:float array array) ->
 let measure = ref max_float
 and seq_left = ref [| |]
 and seq_right = ref [| |]
 and seq_candidate = ref [| |]
 and steps = ref 0
 and mm = ref m
 and r = Array.length m in
  let i = identity_float r r in
   let left = ref i
   and right = ref i in
    while ( !measure > threshold ) && ( !steps < s ) do
     let resultat = sym_float_adapt !mm in
      mm := resultat.(0) ;
      seq_candidate := Array.append !seq_candidate [| !mm |] ;
      left := matrix_float_prod resultat.(1) !left ;
      seq_left := Array.append !seq_left [| !left |] ;
      right := matrix_float_prod !right resultat.(2) ;
      seq_right := Array.append !seq_right [| !right |] ;
      measure := matrix_float_non_diagonality matrix_float_norm_inf !mm ;
      steps := !steps + 1 ;
    done ;
    [| !seq_candidate ; !seq_left ; !seq_right |] ;;


(** {v float_normalized_iterate_seq norm steps matrix vector v} *)
let float_normalized_iterate_seq = fun norm (s:int) (x:float array array) (v:float array) ->
 if s < 0 then failwith "Negative power forbidden in Matrix.float_normalized_iterate." ;
 let y = ref ( vector_float_scal_left_div ( norm v ) v )
 and seq = ref [| |]
 and i = ref 1 in
  let z = ref !y in
   while !i <= s do
    z := matrix_vector_float_prod x !y ;
    z := vector_float_scal_left_div ( norm !z ) !z ;
    seq := Array.append !seq [| !z |] ;
    if norm ( vector_float_minus !z !y ) == 0. then i := max_int
    else ( i := !i + 1 ; y := !z ) ;
   done ;
   !seq ;;


(** {v extrap_inv parameter matrix v} *)
let extrap_inv = fun (parameter:float) (x:float array array) ->
 let y0 = float_slow_inv x in
  let y1 = ( float_tune_inv matrix_float_norm_inf x y0 ).(0) in
   matrix_float_plus y1 ( matrix_float_scal_mult parameter ( matrix_float_minus y1 y0 ) ) ;;

(** {v aggressive_inv parameter matrix v} *)
let aggressive_inv = function (x:float array array) ->
 extrap_inv epsilon_float x ;;


(** linear_regression methode_reduc departure arrival *)
let linear_regression = fun methode_reduc (x:float array array) (y:float array array) ->
 let bx = matrix_float_mean_by_row x
 and by = matrix_float_mean_by_row y
 and dim = Array.length x in
  let f = fun v bb -> vector_float_minus v bb
  and d = pred dim in
   let xx = matrix_float_column_apply_vect ( f bx ) x
   and yy = matrix_float_column_apply_vect ( f by ) y
   and dd = d + dim in
    let z = float_pca methode_reduc ( Array.append yy xx ) in
     let r = Array.map sqrt z.(0).(1) in
      let zz = matrix_float_prod z.(3) ( Array.mapi ( fun i v -> vector_float_scal_mult r.(i) v ) z.(1) ) in
       let alpha = float_sub_matrix zz 0 d 0 d
       and beta = float_sub_matrix zz 0 d dim dd
       and gamma = float_sub_matrix zz dim dd 0 d
       and delta = float_sub_matrix zz dim dd dim dd in
        let a0 = matrix_float_prod alpha ( aggressive_inv gamma ) 
        and a1 = matrix_float_prod beta ( aggressive_inv delta ) in
         let a = matrix_float_scal_mult 0.5 ( matrix_float_plus a0 a1 ) in
          let b = vector_float_minus by ( matrix_vector_float_prod a bx ) in
           [| a ; [| b |] |] ;;






(** {C § § § } *)

end ;;


