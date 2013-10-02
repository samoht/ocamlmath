
  


module Reduc = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)


(**
The mathematician will find in this module methods in order to:

- calculate with univariate polynomials with real or complex coefficients,

- calculate with complex matrices,

- calculate with polynomials of matrices,

- approximate eigenvalues and eigenvectors of matrices,

- separate nilpotent and diagonalizable parts of a matrix,

- approximate roots of polynomials.

Various tradeoffs between speed and presision are possible.
Some calculus in extended precision necessitate the module [sci.ml] which depends on the module [nums.cma] from the standard Ocaml distribution.


{2 Conventions}


Univariate polynomial are line-vectors (see the module {{!module:Matrix} matrix.ml}) containing the coefficients.
The coefficients are taken in the basis [(X^i)], for [i] natural integer.

Complex numbers are square matrices of order 2 with coefficients of type [float] of the form

[\[| \[| x ; -y |\] ;

 \[| y ; x|\] |\]. ***]

Complex matrices are real matrices of quadruple size orgnized as before where [x] and [y] represent respectively
the real and imaginary parts.

Complex vectors may be represented by double-size float vectors with imaginary part coming in the second half.
They may be represented too by real matrices with two columns as before [***] or by complex polynomials.

Complex polynomials are arrays of complex numbers.

Some translating functions with the type [Complex.t] of Ocaml are provided, 
and functions applying functions of the [Complex] module of Ocaml (like Complex.exp or Complex.arg) are provided too.

Gauss integers are square matrices of order 2 with coefficients of type [int] with the same form [***].

The degree and valuation of a polynomial are of type [float] in order to ease the arithmetic with infinity.


{2 Comments}


The Jordan decomposition algorithm is described in:

[www.lsv.ens-cachan.fr/~picaro/COURS/MG/DIVERS/dunford.pdf]


The approximating seek of the roots of a polynomial may produce a failure if:

- some distinct roots are close of one another,
- the multiplicities are high,
- the degree of the polynomial is high.

The combination of all these factors increases the difficulty. 


A shifted seek of the eigenvalues may enhance the results.


This module is distributed under the same licence as Ocaml.



{C § }



La mathématicienne ou le mathématicien trouvera dans ce module des méthodes pour :

- calculer avec des polynômes à une variable à coefficients réels ou complexes,

- calculer avec des matrices complexes,

- calculer avec des polynômes de matrices,

- approximer des valeurs et vecteurs propres de matrices,

- séparer parties nilpotente et diagonalisable d'une matrice,

- approximer des racines de polynômes.

Différents compromis entre vitesse et précision sont possibles.
Certains calculs en précision étendue font appel au module [sci.ml] qui dépend du module [nums.cma] de la distribution Ocaml normale.


{2 Conventions}


Les polynômes à une variable sont des vecteurs lignes (confer le module {{!module:Matrix} matrix.ml}) qui contiennent les coefficients.
Les coefficients sont dans la base [(X^i)], pour [i] entier naturel.

Les nombres complexes sont des matrices carrées d'ordre 2 à coefficients de type [float], sous la forme

[\[| \[| x ; -y |\] ;

 \[| y ; x|\] |\]. ***]

Les matrices complexes sont des matrices réelles de taille quadruple organisées comme ci-dessus où [x] et [y]
représentent respectivement les parties réelle et complexe.

Les vecteurs complexes peuvent être représentés par des vecteurs réels de taille double dont la partie imaginaire vient dans la deuxième moitié.
Ils peuvent aussi être représentés par des matrices réelles à deux colonnes comme ci-dessus [***] ou comme des polynômes complexes.

Les polynômes complexes sont des tableaux de nombres complexes.

Des fonctions de traduction avec le type [Complex.t] d'Ocaml sont fournies, 
et aussi des fonctions d'application des fonctions du module [Complex] (comme Complex.exp ou Complex.arg).

Les entiers de Gauss sont des matrices carrées d'ordre 2 à coefficients de type [int] sous la même forme [***].

Les degré et valuation d'un polynôme sont de type [float] pour faciliter l'arithmétique avec l'infini.


{2 Commentaires}


L'algorithme de décomposition de Jordan est décrit dans :

[www.lsv.ens-cachan.fr/~picaro/COURS/MG/DIVERS/dunford.pdf]


La recherche approchée des racines d'un polynôme peut être mise en échec si : 

- des racines distinctes sont proches,
- les multiplicités sont élevées, 
- le degré du polynôme est élevé. 

La combinaison de ces facteurs augmente encore la difficulté. 


Une recherche avec décalage (shift) des valeurs propres peut améliorer les résultats.


Ce module est distribué selon la même licence qu'Ocaml.



{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.4}
*)
(**
@version 0.4
*)
(**
@author Stéphane Grognet
*)
(**
@since 2011, 2012, 2013
*)




open Util ;;
open Matrix ;;
open Sci ;;




(** {C § } *)
(** 
{1 Constructions élémentaires}
{1 Elementary constructions}
*)
(** {C  } *)




(** 
{2 Complexes}
*)
(** {C  } *)




(** {v built_in_complex_to_matrix complex v} *)
let built_in_complex_to_matrix = function (z:Complex.t) ->
 let x = z.Complex.re
 and y = z.Complex.im in
  [| [| x ; -. y |] ; [| y ; x |] |] ;;

(** {v apply_built_in_complex_to_matrix function complex v} *)
let apply_built_in_complex_to_matrix = fun (f:Complex.t -> Complex.t) (z:float array array) ->
 let zz = { Complex.re = z.(0).(0) ; Complex.im = z.(1).(0) } in
  let zzz = f zz in
   let x = zzz.Complex.re
   and y = zzz.Complex.im in
    [| [| x ; -. y |] ; [| y ; x |] |] ;;

(** {v apply_built_in_complex_float_to_matrix function complex v} *)
let apply_built_in_complex_float_to_matrix = fun (f:Complex.t -> float) (z:float array array) ->
 let zz = { Complex.re = z.(0).(0) ; Complex.im = z.(1).(0) } in
  f zz ;;

(** {v polar_to_matrix modulus argument v} *)
let polar_to_matrix = fun (r:float) (t:float) ->
 let x = r *. ( cos t )
 and y = r *. ( sin t ) in
  [| [| x ; -. y |] ; [| y ; x |] |] ;;

(** {v apply2_built_in_complex_to_matrix function complex1 complex2 v} *)
let apply2_built_in_complex_to_matrix = fun (f:Complex.t -> Complex.t -> Complex.t) (z:float array array) (w:float array array) ->
 let zz = { Complex.re = z.(0).(0) ; Complex.im = z.(1).(0) }
 and ww = { Complex.re = w.(0).(0) ; Complex.im = w.(1).(0) } in
  let zzz = f zz ww in
   let x = zzz.Complex.re
   and y = zzz.Complex.im in
    [| [| x ; -. y |] ; [| y ; x |] |] ;;

(** {v float_to_complex float v} *)
let float_to_complex = function (x:float) ->
[| [| x ; 0. |] ; [| 0. ; x |] |] ;;

(** {v int_to_complex float v} *)
let int_to_complex = function (x:int) ->
[| [| float x ; 0. |] ; [| 0. ; float x |] |] ;;

(** {v clean_complex complex v} *)
let clean_complex = function (z:float array array) ->
 let a = z.(0).(0)
 and b = z.(1).(0)
 and c = z.(0).(1)
 and d = z.(1).(1) in
  let x = 0.5 *. ( a +. d )
  and y = 0.5 *. ( b -. c ) in
   [| [| x ; -. y |] ; [| y ; x |] |] ;;

(** {v complex_inv_bis complex v} *)
let complex_inv_bis = function (z:float array array) ->
 ( Matrix.float_tune_inv Matrix.matrix_float_norm_inf z ( Matrix.float_inv z ) ).(0) ;;

(** {v complex_inv_ter parameter complex v} *)
let complex_inv_ter = fun (parameter:float) (z:float array array) ->
 let y0 = built_in_complex_to_matrix ( Complex.inv {Complex.re=z.(0).(0) ; Complex.im=z.(1).(0)} )
 and y1 = complex_inv_bis z in
  Matrix.matrix_float_plus ( clean_complex y1 ) ( clean_complex ( Matrix.matrix_float_scal_mult parameter ( Matrix.matrix_float_minus y1 y0 ) ) ) ;;

(** {v complex_inv complex v} *)
let complex_inv = function (z:float array array) -> complex_inv_ter 2. z ;;

(** {v complex_slow_inv complex v} *)
let complex_slow_inv = function (z:float array array) ->
 ( Matrix.float_target_inv Matrix.matrix_float_norm_frobenius complex_inv 0. z ).(0) ;;

(** {v complex_div complex1 complex2 v} *)
let complex_div = fun (z:float array array) (zz:float array array) ->
 Matrix.matrix_float_prod ( complex_inv zz ) z ;;


(** {v complex_module complex v} This function permits to measurate the errors when getting away from the
complex structure because of the cumulated errors on the matrix computing.

Cette fonction permet de mesurer les erreurs quand on s'éloigne de la structure complexe
à cause des erreurs cumulées sur les calculs de matrices. *)
let complex_module = function (z:float array array) ->
 let a = z.(0).(0)
 and b = z.(1).(0)
 and c = z.(0).(1)
 and d = z.(1).(1) in
 sqrt ( 0.5 *. ( a *. a +. b *. b +. c *. c +. d *. d ) ) ;;


(** {v complex_square_module complex v} This function permits to measurate the errors when getting away from the
complex structure because of the cumulated errors on the matrix computing.

Cette fonction permet de mesurer les erreurs quand on s'éloigne de la structure complexe
à cause des erreurs cumulées sur les calculs de matrices. *)
let complex_square_module = function (z:float array array) ->
 let a = z.(0).(0)
 and b = z.(1).(0)
 and c = z.(0).(1)
 and d = z.(1).(1) in
  0.5 *. ( a *. a +. b *. b +. c *.c +. d *. d ) ;;

(** {v complex_abs_max complex v} *)
let complex_abs_max = function (z:float array array) ->
 let a = abs_float z.(0).(0)
 and b = abs_float z.(1).(0)
 and c = abs_float z.(0).(1)
 and d = abs_float z.(1).(1) in
  Matrix.vector_float_max [| a ; b ; c ; d |] ;;

(** {v complex_abs_sum complex v} *)
let complex_abs_sum = function (z:float array array) ->
 let a = abs_float z.(0).(0)
 and b = abs_float z.(1).(0)
 and c = abs_float z.(0).(1)
 and d = abs_float z.(1).(1) in
  0.5 *. ( a +. b +. c +. d ) ;;


(** {v complex_real_part complex v} *)
let complex_real_part = function (z:float array array) ->
 let a = z.(0).(0)
 and d = z.(1).(1) in
  0.5 *. ( a +. d ) ;;

(** {v complex_imaginary_part complex v} *)
let complex_imaginary_part = function (z:float array array) ->
 let b = z.(1).(0)
 and c = z.(0).(1) in
  0.5 *. ( b -. c ) ;;

(** {v complex_solve_degree_2 a b c v} *)
let complex_solve_degree_2 = fun (a:float array array) (b:float array array) (c:float array array) ->
 let bb = Matrix.matrix_float_prod b ( float_to_complex (-0.5) ) in
  let d = Matrix.matrix_float_minus ( Matrix.matrix_float_prod bb bb ) ( Matrix.matrix_float_prod a c ) in
   if complex_square_module d = 0. then
    begin
     let x = complex_div bb a in
      [| x ; x |]
    end
   else
    begin
     let dd = apply_built_in_complex_to_matrix Complex.sqrt d in
      let m = Matrix.matrix_float_minus bb dd
      and p = Matrix.matrix_float_plus bb dd in
       let mm = complex_square_module m
       and pp = complex_square_module p in
        if pp >= mm then 
         begin
          let x = complex_div p a in
           [| x ; complex_div c p |]
         end
        else
         begin
          let x = complex_div m a in
           [| complex_div c m ; x |]
         end
    end ;;

(** {v largo_complex_solve_degree_2 a b c v} *)
let largo_complex_solve_degree_2 = fun (a:float array array) (b:float array array) (c:float array array) ->
 Array.map Sci.complex_of_sci ( Sci.solve_degree_2_1024 ( Sci.sci_of_complex a ) ( Sci.sci_of_complex b ) ( Sci.sci_of_complex c ) ) ;;


(** {v complex_trace matrix v} *)
let complex_trace = function (m: float array array) ->
 let mm = Matrix.matrix_float_cut 2 m in
  let mmm = Matrix.matrix_foa_demakeup mm.(0) in
   let t = Matrix.float_demakeup ( Matrix.foa_trace mmm.(0).(0) )
   and tt = Matrix.float_demakeup ( Matrix.foa_trace mmm.(1).(0) ) in
    [| [| t ; -. tt |] ; [| tt ; t |] |] ;;

(** {v matrix_complex_real_part matrix v} *)
let matrix_complex_real_part = function (m:float array array) ->
 let rrrr = Array.length m
 and cccc = Array.length m.(0) in
  let rr = rrrr / 2
  and cc = cccc / 2
  and rrr = rrrr - 1
  and ccc = cccc - 1 in
   let r = rr - 1
   and c = cc - 1 in
   let x = Matrix.sub_matrix m 0 r 0 c
   and xx = Matrix.sub_matrix m r rrr cc ccc in
    Matrix.matrix_float_scal_mult 0.5 ( Matrix.matrix_float_plus x xx ) ;;

(** {v matrix_complex_imag_part matrix v} *)
let matrix_complex_imag_part = function (m:float array array) ->
 let rrrr = Array.length m
 and cccc = Array.length m.(0) in
  let rr = rrrr / 2
  and cc = cccc / 2
  and rrr = rrrr - 1
  and ccc = cccc - 1 in
   let r = rr - 1
   and c = cc - 1 in
   let y = Matrix.sub_matrix m rr rrr 0 c
   and yy = Matrix.sub_matrix m 0 r cc ccc in
    Matrix.matrix_float_scal_mult 0.5 ( Matrix.matrix_float_minus y yy ) ;;

(** {v matrix_complexify matrix1 matrix2 v} *)
let matrix_complexify = fun (x:float array array) (y:float array array) ->
 let m = [| [| Matrix.Float_matrix_cons x ; Matrix.Float_matrix_cons ( Matrix.matrix_float_opp y ) |] ;
            [| Matrix.Float_matrix_cons y ; Matrix.Float_matrix_cons x |] |] in
  Matrix.matrix_float_demakeup ( Matrix.matrix_foa_crash ( Matrix.Foa_matrix_cons m ) ) ;;

(** {v complex_saturate num_rows num_columns complex v} *)
let complex_saturate = fun (r:int) (c:int) z ->
 let x = Array.make_matrix r c z.(0).(0)
 and y = Array.make_matrix r c z.(1).(0) in
  matrix_complexify x y ;;

(** {v matrix_real_to_complex matrix v} *)
let matrix_real_to_complex = function (x:float array array) ->
 let z = Matrix.zeros_float x in
  matrix_complexify x z ;;

(** {v matrix_imag_to_complex matrix v} *)
let matrix_imag_to_complex = function (x:float array array) ->
 let z = Matrix.zeros_float x in
  matrix_complexify z x ;;

(** {v scal_complex num_rows num_columns complex v} *)
let scal_complex = fun (r:int) (c:int) (z:float array array) ->
 let x = Matrix.scal_float r c z.(0).(0)
 and y = Matrix.scal_float r c z.(1).(0) in
  matrix_complexify x y ;;

(** {v matrix_complex_jordan order complex v} *)
let matrix_complex_jordan = fun (n:int) (z:float array array) ->
 let x = scal_complex n n z in
  for i = 0 to n - 2 do
   x.(i).( i + 1 ) <- 1. ;
   let nn = n + i in
    x.(nn).( nn + 1 ) <- 1. ;
  done ;
  x ;;

(** {v vector_complex_scal_mult complex polynomial v} *)
let vector_complex_scal_mult = fun (x:float array array) (v:float array array array) ->
 let l = Array.length v
 and zz = Array.make_matrix 2 2 0. in
  let vv = Array.make l zz in
   for i = 0 to l - 1 do
    vv.(i) <- Matrix.matrix_float_prod x v.(i)
   done ;
   vv ;;

(** {v vector_complex_scal_left_div complex polynomial v} *)
let vector_complex_scal_left_div = fun (x:float array array) (v:float array array array) ->
 let l = Array.length v
 and zz = Array.make_matrix 2 2 0. in
  let vv = Array.make l zz in
   for i = 0 to l - 1 do
    vv.(i) <- Matrix.matrix_float_prod ( complex_inv x ) v.(i)
   done ;
   vv ;;

(** {v vector_complex_scal_right_div complex polynomial v} *)
let vector_complex_scal_right_div = fun (x:float array array) (v:float array array array) ->
 let l = Array.length v
 and zz = Array.make_matrix 2 2 0. in
  let vv = Array.make l zz in
   for i = 0 to l - 1 do
    vv.(i) <- Matrix.matrix_float_prod x ( complex_inv v.(i) )
   done ;
   vv ;;

(** {v matrix_complex_scal_mult complex matrix v} *)
let matrix_complex_scal_mult = fun (x:float array array) (m:float array array) ->
 let l = ( Array.length m ) / 2 in
  let xx = scal_complex l l x in
   Matrix.matrix_float_prod xx m ;;

(** {v matrix_complex_scal_left_div complex matrix v} *)
let matrix_complex_scal_left_div = fun (x:float array array) (m:float array array) ->
 let l = ( Array.length m ) / 2 in
  let xx = Matrix.float_inv ( scal_complex l l x ) in
   Matrix.matrix_float_prod xx m ;;


(** {v vector_complex_norm_inf polynomial v} *)
let vector_complex_norm_inf = function (p:float array array array) ->
 let v = Array.map complex_module p in
  Matrix.vector_float_norm_inf v ;;

(** {v vector_complex_norm_inf_bis polynomial v} *)
let vector_complex_norm_inf_bis = function (p:float array array array) ->
 let v = Array.map Matrix.matrix_float_norm_inf p in
  Matrix.vector_float_norm_inf v ;;

(** {v vector_complex_norm_1 polynomial v} *)
let vector_complex_norm_1 = function (p:float array array array) ->
 let v = Array.map complex_module p in
  Matrix.vector_float_norm_1 v ;;

(** {v vector_complex_norm_1_bis polynomial v} *)
let vector_complex_norm_1_bis = function (p:float array array array) ->
 let v = Array.map Matrix.matrix_float_norm_1 p in
  Matrix.vector_float_norm_1 v ;;

(** {v vector_complex_norm_2 polynomial v} *)
let vector_complex_norm_2 = function (p:float array array array) ->
 let v = Array.map complex_module p in
  Matrix.vector_float_norm_2 v ;;

(** {v vector_complex_square_norm_2 polynomial v} *)
let vector_complex_square_norm_2 = function (p:float array array array) ->
 let v = Array.map complex_square_module p in
  Matrix.vector_float_sum v ;;




(** 
{2 Entiers de Gauss}
{2 Gauss integers}
*)
(** {C  } *)




(** {v int_to_gauss integer v} *)
let int_to_gauss = function (x:int) ->
[| [| x ; 0 |] ; [| 0 ; x |] |] ;;

(** {v gauss_inv gauss_integer v} *)
let gauss_inv = function (z:int array array) ->
 Matrix.int_inv z ;;

(** {v gauss_div gauss_integer1 gauss_integer2 v} *)
let gauss_div = fun (z:int array array) (zz:int array array) ->
 Matrix.matrix_int_prod ( gauss_inv zz ) z ;;

(** {v gauss_square_module gauss_integer v} *)
let gauss_square_module = function (z:int array array) ->
 let a = z.(0).(0)
 and b = z.(1).(0)
 and c = z.(0).(1)
 and d = z.(1).(1) in
  ( a * a + b * b + c * c + d * d ) / 2 ;;

(** {v gauss_real_part gauss_integer v} *)
let gauss_real_part = function (z:int array array) ->
 let a = z.(0).(0)
 and d = z.(1).(1) in
  ( a + d ) / 2 ;;

(** {v gauss_imaginary_part gauss_integer v} *)
let gauss_imaginary_part = function (z:int array array) ->
 let b = z.(1).(0)
 and c = z.(0).(1) in
  ( b - c ) / 2 ;;

(** {v gauss_trace matrix v} *)
let gauss_trace = function (m: int array array) ->
 let mm = Matrix.matrix_int_cut 2 m in
  let mmm = Matrix.matrix_ioa_demakeup mm.(0) in
   let t = Matrix.int_demakeup ( Matrix.ioa_trace mmm.(0).(0) )
   and tt = Matrix.int_demakeup ( Matrix.ioa_trace mmm.(1).(0) ) in
    [| [| t ; - tt |] ; [| tt ; t |] |] ;;

(** {v matrix_gauss_real_part matrix v} *)
let matrix_gauss_real_part = function (m:int array array) ->
 let rrrr = Array.length m
 and cccc = Array.length m.(0) in
  let rr = rrrr / 2
  and cc = cccc / 2
  and rrr = rrrr - 1
  and ccc = cccc - 1 in
   let r = rr - 1
   and c = cc - 1 in
   let x = Matrix.sub_matrix m 0 r 0 c
   and xx = Matrix.sub_matrix m r rrr cc ccc in
    Matrix.matrix_int_scal_left_div 2 ( Matrix.matrix_int_plus x xx ) ;;

(** {v matrix_gauss_imag_part matrix v} *)
let matrix_gauss_imag_part = function (m:int array array) ->
 let rrrr = Array.length m
 and cccc = Array.length m.(0) in
  let rr = rrrr / 2
  and cc = cccc / 2
  and rrr = rrrr - 1
  and ccc = cccc - 1 in
   let r = rr - 1
   and c = cc - 1 in
   let y = Matrix.sub_matrix m rr rrr 0 c
   and yy = Matrix.sub_matrix m 0 r cc ccc in
    Matrix.matrix_int_scal_left_div 2 ( Matrix.matrix_int_minus y yy ) ;;

(** {v matrix_gauss_complexify matrix1 matrix2 v} *)
let matrix_gauss_complexify = fun (x:int array array) (y:int array array) ->
 let m = [| [| Matrix.Int_matrix_cons x ; Matrix.Int_matrix_cons ( Matrix.matrix_int_opp y ) |] ;
             [| Matrix.Int_matrix_cons y ; Matrix.Int_matrix_cons x |] |] in
   Matrix.matrix_int_demakeup ( Matrix.matrix_ioa_crash ( Matrix.Ioa_matrix_cons m ) ) ;;

(** {v matrix_real_to_gauss matrix v} *)
let matrix_real_to_gauss = function (x:int array array) ->
 let z = Matrix.zeros_int x in
  matrix_gauss_complexify x z ;;

(** {v matrix_imag_to_gauss matrix v} *)
let matrix_imag_to_gauss = function (x:int array array) ->
 let z = Matrix.zeros_int x in
  matrix_gauss_complexify z x ;;

(** {v scal_gauss num_rows num_columns gauss_integer v} *)
let scal_gauss = fun (r:int) (c:int) (z:int array array) ->
 let x = Matrix.scal_int r c z.(0).(0)
 and y = Matrix.scal_int r c z.(1).(0) in
  matrix_gauss_complexify x y ;;

(** {v matrix_gauss_jordan order gauss_integer v} *)
let matrix_gauss_jordan = fun (n:int) (z:int array array) ->
 let x = scal_gauss n n z in
  for i = 0 to n - 2 do
   x.(i).( i + 1 ) <- 1 ;
   let nn = n + i in
    x.(nn).( nn + 1 ) <- 1 ;
  done ;
  x ;;

(** {v vector_gauss_scal_mult gauss_integer polynomial v} *)
let vector_gauss_scal_mult = fun (x:int array array) (v:int array array array) ->
 let l = Array.length v
 and zz = Array.make_matrix 2 2 0 in
  let vv = Array.make l zz in
   for i = 0 to l - 1 do
    vv.(i) <- Matrix.matrix_int_prod x v.(i)
   done ;
   vv ;;

(** {v matrix_gauss_scal_mult gauss_integer matrix v} *)
let matrix_gauss_scal_mult = fun (x:int array array) (m:int array array) ->
 let l = ( Array.length m ) / 2 in
  let xx = scal_gauss l l x in
   Matrix.matrix_int_prod xx m ;;

(** {v matrix_gauss_scal_left_div gauss_integer matrix v} *)
let matrix_gauss_scal_left_div = fun (x:int array array) (m:int array array) ->
 let l = ( Array.length m ) / 2 in
  let xx = Matrix.int_inv ( scal_gauss l l x ) in
   Matrix.matrix_int_prod xx m ;;

(** {v vector_gauss_norm_inf polynomial v} *)
let vector_gauss_norm_inf = function (p:int array array array) ->
 let v = Array.map Matrix.matrix_int_norm_inf p in
  Matrix.vector_int_norm_inf v ;;

(** {v vector_gauss_norm_1 polynomial v} *)
let vector_gauss_norm_1 = function (p:int array array array) ->
 let v = Array.map Matrix.matrix_int_norm_1 p in
  Matrix.vector_int_norm_1 v ;;

(** {v vector_gauss_square_norm_2 polynomial v} *)
let vector_gauss_square_norm_2 = function (p:int array array array) ->
 let v = Array.map gauss_square_module p in
  Matrix.vector_int_sum v ;;




(** 
{2 Constantes}
{2 Constants}
*)
(** {C  } *)




(** {v complex_1 v} *)
let complex_1 = Matrix.identity_float 2 2 ;;

(** {v complex_minus_1 v} *)
let complex_minus_1 = Matrix.scal_float 2 2 (-1.) ;;

(** {v complex_0 v} *)
let complex_0 = Matrix.null_float 2 2 ;;

(** {v complex_i v} *)
let complex_i = [| [| 0. ; -1. |] ; [| 1. ; 0. |] |] ;;


(** {v gauss_1 v} *)
let gauss_1 = Matrix.identity_int 2 2 ;;

(** {v gauss_minus_1 v} *)
let gauss_minus_1 = Matrix.scal_int 2 2 (-1) ;;

(** {v gauss_0 v} *)
let gauss_0 = Matrix.null_int 2 2 ;;

(** {v gauss_i v} *)
let gauss_i = [| [| 0 ; -1 |] ; [| 1 ; 0 |] |] ;;


(** {v poly_real_x v} *)
let poly_real_x = [| 0. ; 1. |] ;;

(** {v poly_real_x_power integer v} *)
let poly_real_x_power = function (n:int) ->
 match n with
 | 0 -> [| 1. |]
 | 1 -> poly_real_x
 | _ -> let w = Array.make ( n + 1 ) 0. in
  w.(n) <- 1. ;
  w ;;

(** {v poly_real_x_a a v} *)
let poly_real_x_a = function (a:float) ->
 [| -. a ; 1. |] ;;


(** {v poly_int_x v} *)
let poly_int_x = [| 0 ; 1 |] ;;

(** {v poly_int_x_power integer v} *)
let poly_int_x_power = function (n:int) ->
 match n with
 | 0 -> [| 1 |]
 | 1 -> poly_int_x
 | _ -> let w = Array.make ( n + 1 ) 0 in
  w.(n) <- 1 ;
  w ;;

(** {v poly_int_x_a a v} *)
let poly_int_x_a = function (a:int) -> [| - a ; 1 |] ;;


(** {v poly_complex_x v} *)
let poly_complex_x = [| complex_0 ; complex_1 |] ;;

(** {v poly_complex_x_power integer v} *)
let poly_complex_x_power = function (n:int) ->
 match n with
 | 0 -> [| complex_1 |]
 | 1 -> poly_complex_x
 | _ -> let w = Array.make ( n + 1 ) complex_0 in
  w.(n) <- complex_1 ;
  w ;;

(** {v poly_complex_x_a a v} *)
let poly_complex_x_a = function (a:float array array) -> [| Matrix.matrix_float_opp a ; complex_1 |] ;;


(** {v poly_gauss_x v} *)
let poly_gauss_x = [| gauss_0 ; gauss_1 |] ;;

(** {v poly_gauss_x_power integer v} *)
let poly_gauss_x_power = function (n:int) -> match n with
 | 0 -> [| gauss_1 |]
 | 1 -> poly_gauss_x
 | _ -> let w = Array.make ( n + 1 ) gauss_0 in
  w.(n) <- gauss_1 ;
  w ;;

(** {v poly_gauss_x_a a v} *)
let poly_gauss_x_a = function (a:int array array) -> [| Matrix.matrix_int_opp a ; gauss_1 |] ;;


(** {v poly_sci_x v} *)
let poly_sci_x = [| Sci.sci_0 ; Sci.sci_1 |] ;;

(** {v poly_sci_x_power integer v} *)
let poly_sci_x_power = function (n:int) -> match n with
 | 0 -> [| Sci.sci_1 |]
 | 1 -> poly_sci_x
 | _ -> let w = Array.make ( n + 1 ) Sci.sci_0 in
  w.(n) <- Sci.sci_1 ;
  w ;;

(** {v poly_sci_x_a a v} *)
let poly_sci_x_a = function (a:Num.num array) -> [| Sci.opp a ; Sci.sci_1 |] ;;




(** {C § } *)
(** 
{1 Opérations élémentaires}
{1 Elementary operations}
*)
(** {C  } *)




(** 
{2 Opérations polymorphes sur les polynômes}
{2 Polymorphic operations on polynomials}
*)
(** {C  } *)




(** {v extract_even_part polynomial v} *)
let extract_even_part = function p ->
 let r = Array.length p in
  if r = 0 then [| |]
  else
   begin
    let rr = ( r + 1 ) / 2 in
     let q = Array.make rr p.(0) in
      for i = 0 to pred rr do
       q.(i) <- p.( 2 * i )
      done ;
      q ;
   end ;;


(** {v extract_odd_part polynomial v} *)
let extract_odd_part = function p ->
 let r = Array.length p in
  if r = 0 then [| |]
  else
   begin
    let rr = ( r / 2 ) in
     let q = Array.make rr p.(0) in
      for i = 0 to pred rr do
       q.(i) <- p.( 2 * i + 1 )
      done ;
      q ;
   end ;;




(** 
{2 Opérations polymorphes sur les matrices}
{2 Polymorphic operations on matrices}
*)
(** {C  } *)




(** {v matrix_complex_extract_coefficient row column matrix v} *)
let matrix_complex_extract_coefficient = fun (i:int) (j:int) m ->
 let n = ( Array.length m ) / 2 in
  let ii = n + i
  and jj = n + j
  and first_row = m.(i) in
   let second_row = m.(ii) in
    [| [| first_row.(j) ; first_row.(jj) |] ; [| second_row.(j) ; second_row.(jj) |] |] ;;


(** {v matrix_complex_extract_row_to_poly row_number matrix v} This function may apply to complex coefficients
(float array array) or Gauss integer coefficients (int array array).

Cette fonction peut être appliquée aux complexes (float array array) ou aux entiers de Gauss (int array array). *)
let matrix_complex_extract_row_to_poly = fun (i:int) m ->
 let r = ( Array.length m ) / 2
 and c = ( Array.length m.(0) ) / 2 in
  let ii = r + i
  and first_row = m.(i)
  and x = Array.make c ( Array.make_matrix 2 2 m.(0).(0) ) in
   let second_row = m.(ii) in
    for j = 0 to c - 1 do
     let jj = c + j in
      x.(j) <- [| [| first_row.(j) ; first_row.(jj) |] ; [| second_row.(j) ; second_row.(jj) |] |] ;
    done ;
    x ;;

(** {v matrix_complex_extract_row_to_vector row_number matrix v} *)
let matrix_complex_extract_row_to_vector = fun (i:int) m ->
 let r = ( Array.length m ) / 2
 and cc = Array.length m.(0) in
  let ii = r + i
  and c = cc / 2
  and x = Array.make cc m.(0).(0) in
   for j = 0 to c - 1 do
    let jj = c + j in
     x.(j) <- m.(i).(j) ;
     x.(jj) <- m.(ii).(j) ;
   done ;
   x ;;

(** {v matrix_complex_extract_row_to_matrix row_number matrix v} *)
let matrix_complex_extract_row_to_matrix = fun (i:int) m ->
 let cc = Array.length m.(0) in
  let r = ( Array.length m ) / 2
  and c = cc / 2 in
   let ii = r + i
   and first_row = m.(i)
   and x = Array.make_matrix cc 2 m.(0).(0) in
    let second_row = m.(ii) in
     for j = 0 to c - 1 do
      let jj = c + j in
       x.(j) <- [| first_row.(j) ; first_row.(jj) |] ;
       x.(jj) <- [| second_row.(j) ; second_row.(jj) |] ;
     done ;
     x ;;

(** {v matrix_complex_extract_row_to_matrix_trans row_number matrix v} *)
let matrix_complex_extract_row_to_matrix_trans = fun (i:int) m ->
 let first = m.(i)
 and second = m.( ( ( Array.length m ) / 2 ) + i ) in
  let cc = Array.length first in
   let x = Array.make cc m.(0).(0)
   and y = Array.make cc m.(0).(0)
   and c = cc / 2 in
    for j = 0 to c - 1 do
     let jj = c + j in
      x.(j) <- first.(j) ;
      x.(jj) <- second.(j) ;
      y.(j) <- first.(jj) ;
      y.(jj) <- second.(jj) ;
    done ;
    [| x ; y |] ;;


(** {v matrix_complex_extract_column_to_poly row_number matrix v} *)
let matrix_complex_extract_column_to_poly = fun (j:int) m ->
 let r = ( Array.length m ) / 2 in
  let jj = ( ( Array.length m.(0) ) / 2 ) + j
  and x = Array.make r ( Array.make_matrix 2 2 m.(0).(0) ) in
   for i = 0 to r - 1 do
    let first_row = m.(i)
    and ii = r + i in
     let second_row = m.(ii) in
      x.(i) <- [| [| first_row.(j) ; first_row.(jj) |] ; [| second_row.(j) ; second_row.(jj) |] |] ;
   done ;
   x ;;

(** {v matrix_complex_extract_column_to_vector column_number matrix v} *)
let matrix_complex_extract_column_to_vector = fun (j:int) m ->
 let rr = Array.length m in
  let r = rr / 2
  and x = Array.make rr m.(0).(0) in
   for i = 0 to r - 1 do
    let ii = r + i in
     x.(i) <- m.(i).(j) ;
     x.(ii) <- m.(ii).(j) ;
   done ;
   x ;;

(** {v matrix_complex_extract_column_to_matrix column_number matrix v} *)
let matrix_complex_extract_column_to_matrix = fun (j:int) m ->
 let rr = Array.length m in
  let r = rr / 2
  and x = Array.make_matrix rr 2 m.(0).(0) in
   let jj = ( ( Array.length m.(0) ) / 2 ) + j in
    for i = 0 to r - 1 do
     let first_row = m.(i)
     and ii = r + i in
      let second_row = m.(ii) in
       x.(i) <- [| first_row.(j) ; first_row.(jj) |] ;
       x.(ii) <- [| second_row.(j) ; second_row.(jj) |] ;
    done ;
    x ;;

(** {v matrix_complex_extract_column_to_matrix_trans column_number matrix v} *)
let matrix_complex_extract_column_to_matrix_trans = fun (j:int) m ->
 let rr = Array.length m in
  let r = rr / 2
  and x = Array.make rr m.(0).(0)
  and y = Array.make rr m.(0).(0) in
   let jj = ( Array.length m.(0) / 2 ) + j in
    for i = 0 to r - 1 do
     let first_row = m.(i)
     and ii = r + i in
      let second_row = m.(ii) in
       x.(i) <- first_row.(j) ;
       x.(ii) <- second_row.(j) ;
       y.(i) <- first_row.(jj) ;
       y.(ii) <- second_row.(jj) ;
    done ;
    [| x ; y |] ;;




(** 
{2 Opérations élémentaires sur les matrices complexes}
{2 Elementary operations on complex matrices}
*)
(** {C  } *)




(** {v matrix_complex_extract_diag_to_poly matrix v} *)
let matrix_complex_extract_diag_to_poly = function (m:float array array) ->
 let n = ( min ( Array.length m ) ( Array.length m.(0) ) ) / 2 in
  let d = Array.make n complex_0 in
   for i = 0 to n - 1 do
    d.(i) <- matrix_complex_extract_coefficient i i m ;
   done ;
   d ;;

(** {v vector_complex_contraction polynomial v} *)
let vector_complex_contraction = fun (v:float array array array) ->
 let accu = ref complex_1 in
  for i = 0 to ( ( Array.length v ) / 2 ) - 1 do
   accu := Matrix.matrix_float_prod !accu v.(i) ;
  done ;
  !accu ;;


(** {v vector_complex_hermitian_product vector1 vector2 v} The vectors must have the form of real matrices with two rows.

Les vecteurs doivent être sous forme de matrices réelles à deux lignes. *)
let vector_complex_hermitian_prod = fun (u:float array array) (v:float array array) ->
 Matrix.matrix_float_twisted_prod v u ;;

(** {v diag_complex polynomial v} *)
let diag_complex = function (x:float array array array) ->
 let n = Array.length x in
  let nnn = 2 * n
  and nn = n - 1 in
   let m = Array.make_matrix nnn nnn 0. in
    for i = 0 to nn do
     let ii = n + i
     and coeff = x.(i)
     and first_row = m.(i) in
      let second_row = m.(ii) in
       first_row.(i) <- coeff.(0).(0) ;
       first_row.(ii) <- coeff.(0).(1) ;
       second_row.(ii) <- coeff.(1).(1) ;
       second_row.(i) <- coeff.(1).(0) ;
    done ;
    m ;;

(** {v matrix_complex_non_diagonality norm matrix v} *)
let matrix_complex_non_diagonality = fun distance (m:float array array) ->
 let mm = Matrix.matrix_float_minus m ( diag_complex ( matrix_complex_extract_diag_to_poly m ) ) in
  distance mm ;;

(** {v matrix_complex_non_diagonal_part matrix v} *)
let matrix_complex_non_diagonal_part = function (m:float array array) ->
 Matrix.matrix_float_minus m ( diag_complex ( matrix_complex_extract_diag_to_poly m ) ) ;;

(** {v vector_complex_i_times vector v} *)
let vector_complex_i_times = function (v:float array) ->
 let rr = Array.length v in
  let r = rr / 2
  and w = Array.make rr v.(0) in
   for i = 0 to r - 1 do
    let ii = r + i in
    w.(i) <- -. v.(ii) ;
    w.(ii) <- v.(i) ;
   done ;
   w ;;

(** {v vector_complex_to_matrix vector v} *)
let vector_complex_to_matrix = function (v:float array) ->
 [| v ; vector_complex_i_times v |] ;;




(** 
{2 Opérations élémentaires sur les matrices à coefficients entiers de Gauss}
{2 Elementary operations on matrices with Gauss integer coefficients}
*)
(** {C  } *)




(** {v matrix_gauss_extract_diag_to_poly matrix v} *)
let matrix_gauss_extract_diag_to_poly = function (m:int array array) ->
 let n = ( min ( Array.length m ) ( Array.length m.(0) ) ) / 2 in
  let d = Array.make n gauss_0 in
   for i = 0 to n - 1 do
    d.(i) <- matrix_complex_extract_coefficient i i m ;
   done ;
   d ;;

(** {v vector_gauss_contraction vector v} *)
let vector_gauss_contraction = fun (v:int array array array) ->
 let accu = ref gauss_1 in
  for i = 0 to ( ( Array.length v ) / 2 ) - 1 do
   accu := Matrix.matrix_int_prod !accu v.(i) ;
  done ;
  !accu ;;


(** {v vector_gauss_hermitian_product vector1 vector2 v} The vectors must have the form of real matrices with two rows.

Les vecteurs doivent être sous forme de matrices réelles à deux lignes. *)
let vector_gauss_hermitian_prod = fun (u:int array array) (v:int array array) ->
 Matrix.matrix_int_twisted_prod v u ;;

(** {v diag_gauss polynomial v} *)
let diag_gauss = function (x:int array array array) ->
 let n = Array.length x in
  let nnn = 2 * n
  and nn = n - 1 in
   let m = Array.make_matrix nnn nnn 0 in
    for i = 0 to nn do
     let ii = n + i
     and coeff = x.(i)
     and first_row = m.(i) in
      let second_row = m.(ii) in
       first_row.(i) <- coeff.(0).(0) ;
       first_row.(ii) <- coeff.(0).(1) ;
       second_row.(ii) <- coeff.(1).(1) ;
       second_row.(i) <- coeff.(1).(0) ;
    done ;
    m ;;

(** {v matrix_gauss_non_diagonality norm matrix v} *)
let matrix_gauss_non_diagonality = fun distance (m:int array array) ->
 let mm = Matrix.matrix_int_minus m ( diag_gauss ( matrix_gauss_extract_diag_to_poly m ) ) in
  distance mm ;;

(** {v matrix_gauss_non_diagonal_part matrix v} *)
let matrix_gauss_non_diagonal_part = function (m:int array array) ->
 Matrix.matrix_int_minus m ( diag_gauss ( matrix_gauss_extract_diag_to_poly m ) ) ;;

(** {v vector_gauss_i_times vector v} *)
let vector_gauss_i_times = function (v:int array) ->
 let rr = Array.length v in
  let r = rr / 2
  and w = Array.make rr v.(0) in
   for i = 0 to r - 1 do
    let ii = r + i in
    w.(i) <- - v.(ii) ;
    w.(ii) <- v.(i) ;
   done ;
   w ;;

(** {v vector_gauss_to_matrix vector v} *)
let vector_gauss_to_matrix = function (v:int array) ->
 [| v ; vector_gauss_i_times v |] ;;




(** 
{2 Opérations élémentaires sur les polynômes}
{2 Elementary operations on polynomials}
*)
(** {C  } *)




(** 
{3 Polynômes à coefficients réels}
{3 Polynomials with real coefficients}
*)
(** {C  } *)




(** {v poly_real_deg polynomial v} *) 
let poly_real_deg = function (p:float array) ->
 let r = ref ( ( Array.length p ) - 1 )
 and deg = ref neg_infinity in
  if !r >= 0 then 
   begin
    while !r >= 0 do
     begin 
      let coeff = p.(!r) in
       if coeff <> 0. then
        begin
         if abs_float coeff > max_float then
          ( deg := infinity ; r := -1 )
         else ( deg := float !r ; r := -1 )
        end
       else r := !r - 1 ;
     end
    done ;
   end ;
   !deg ;;

(** {v poly_real_val polynomial v} *) 
let poly_real_val = function (p:float array) ->
 let rr = ( ( Array.length p ) - 1 )
 and r = ref 0
 and valuation = ref infinity in
  if !r <= rr then 
   begin
    while !r <= rr do
     begin 
      let coeff = p.(!r) in
       if coeff <> 0. then
        begin
         if abs_float coeff > max_float then
          ( valuation := neg_infinity ; r := max_int )
         else ( valuation := float !r ; r := max_int )
        end
       else r := !r + 1 ;
     end
    done ;
   end ;
   !valuation ;;

(** {v poly_real_cleanup polynomial v} *)
let poly_real_cleanup = function (p:float array) ->
 let d = poly_real_deg p in
  if d < 0. then [| 0. |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) 0. in
      for i = 0 to dd do
       q.(i) <- p.(i) ;
      done ;
      q ;
   end ;;

(** {v poly_real_normalize polynomial v} *)
let poly_real_normalize = function (p:float array) ->
 let d = poly_real_deg p in
  if d < 0. then [| 0. |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) 1.
     and coeff = 1. /. p.(dd) in
      for i = 0 to dd - 1 do
       q.(i) <- p.(i) *. coeff ;
      done ;
      q ;
   end ;;

(** {v poly_real_deriv polynomial v} *) 
let poly_real_deriv = function (p:float array) ->
 let pp = ( Array.length p ) - 1 in
  let r = Array.make pp 0. in
   for i = 1 to pp do
    r.( i - 1 ) <- ( float i ) *. p.(i)
   done ;
   r ;;

(** {v poly_real_plus polynomial1 polynomial2 v} *) 
let poly_real_plus = fun (p:float array) (q:float array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let x = Array.concat [ p ; ( Array.make ( rr - pp ) 0. ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) 0. ) ] in
    Matrix.vector_float_plus x y ;;

(** {v poly_real_minus polynomial1 polynomial2 v} *) 
let poly_real_minus = fun (p:float array) (q:float array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let x = Array.concat [ p ; ( Array.make ( rr - pp ) 0. ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) 0. ) ] in
    Matrix.vector_float_minus x y ;;

(** {v poly_real_mult polynomial1 polynomial2 v} *) 
let poly_real_mult = fun (p:float array) (q:float array) ->
 let pp = Array.length p
 and qq = Array.length q
 and accu = ref 0. in
  let qqq = qq - 1 in
   let rr = pp + qqq in
    let x = Array.concat [ p ; ( Array.make qq 0. ) ]
    and y = Array.concat [ q ; ( Array.make pp 0. ) ]
    and r = Array.make rr 0. in
     for i = 0 to rr - 1 do
      for j = 0 to i do
       accu := !accu +. x.(j) *. y.( i - j ) ;
      done ;
      r.(i) <- !accu ;
      accu := 0. ;
     done ;
     r ;;

(** {v poly_real_mult_karatsuba threshold polynomial1 polynomial2 v} *)
let rec poly_real_mult_karatsuba = fun (threshold:int) (p:float array) (q:float array) ->
 let l = Array.length p
 and ll = Array.length q in
  let l_l = max l ll in
   if l_l <= threshold then poly_real_mult p q
   else
    begin
     let half = ( l_l ) / 2
     and p_p = Array.append p ( Array.make ( l_l - l ) 0. )
     and q_q = Array.append q ( Array.make ( l_l - ll ) 0. ) in
      let pp = Array.sub p_p 0 half
      and ppp = Array.sub p_p half ( l_l - half )
      and qq = Array.sub q_q 0 half
      and qqq = Array.sub q_q half ( l_l - half ) in
       let debut = poly_real_mult_karatsuba threshold pp qq
       and fin = poly_real_mult_karatsuba threshold ppp qqq
       and psum = poly_real_plus pp ppp
       and qsum = poly_real_plus qq qqq in
        let mix = poly_real_mult_karatsuba threshold psum qsum in
         let inter = poly_real_minus mix debut in
          let milieu = poly_real_minus inter fin in
           let first = poly_real_plus debut ( Array.append ( Array.make ( half ) 0. ) milieu ) in
            let raw_prod = poly_real_plus first ( Array.append ( Array.make ( 2 * half ) 0. ) fin ) in
             poly_real_cleanup raw_prod
    end ;;

(** {v poly_real_pow mult_rule power polynomial v} *) 
let rec poly_real_pow = fun mult_rule (n:int) (p:float array) ->
 match n with
  | 0 -> [| 1. |]
  | 1 -> p
  | 2 -> mult_rule p p
  | _ ->
   begin
    let nn = n / 2 in
     let pp = poly_real_pow mult_rule nn p in
      let prod = mult_rule pp pp in
       if n mod 2 = 0 then
        prod
       else
        mult_rule prod p
   end ;;

(** {v poly_real_finite_prod mult_rule polynomial_array v} *) 
let rec poly_real_finite_prod = fun mult_rule (p:float array array) ->
 let n = ( Array.length p ) - 1
 and q = ref p.(0) in
  for i = 1 to n do
   q := mult_rule !q p.(i) ;
  done ;
  !q ;;

(** {v poly_real_from_roots mult_rule roots_array v} *) 
let poly_real_from_roots = fun mult_rule (r:float array) ->
 let a = Array.map poly_real_x_a r in
  poly_real_finite_prod mult_rule a ;;

(** {v poly_real_horner_comp polynomial1 polynomial2 v} *) 
let poly_real_horner_comp = fun (p:float array) (q:float array) ->
 let pp = Array.length p in
  let res = ref [| p.( pp - 1 ) |] in 
   for i = pp - 2 downto 0 do
    res := poly_real_plus ( poly_real_mult !res q ) [| p.(i) |] ;
   done ;
   !res ;;

(** {v poly_real_ranged_horner_comp mult_rule index order polynomial1 polynomial2 v} *) 
let poly_real_ranged_horner_comp = fun mult_rule (i:int) (l:int) (p:float array) (q:float array) ->
 let res = ref [| p.( i + l - 1 ) |] in 
  for j = l - 2 downto 0 do
   res := poly_real_plus ( mult_rule !res q ) [| p.( i + j ) |] ;
  done ;
  !res ;;


(** {v poly_real_brent_kung_hart_novocin_comp mult_rule parameter polynomial1 polynomial2 v} The length [l] must be greater than or equal to [3].
The algorithm comes from the document located at the following address.

[http://hal-ens-lyon.archives-ouvertes.fr/docs/00/54/61/02/PDF/dmtcs_NOVOCIN2010.pdf]

L'algorithme provient du document situé à l'adresse précédente.
La longueur [l] doit être supérieure ou égale à [3]. *) 
let poly_real_brent_kung_hart_novocin_comp = fun mult_rule (l:int) (p:float array) (q:float array) ->
 let n = ( Array.length p ) - 1
 and g = ref ( poly_real_pow mult_rule l q ) in
  let k = ref ( ( n + 1 ) / l + 1 ) in
   let pp = Array.append p ( Array.make ( !k * l - n ) 0. ) in
    k := ( 1 + ( Array.length pp ) ) / l ;
    let h = Array.make_matrix ( ( !k + l ) * 2 ) 1 0.
    and hh = Array.make_matrix ( ( !k + l ) * 2 ) 1 0. in 
     for j = 0 to !k - 1 do
      h.(j) <- poly_real_ranged_horner_comp mult_rule ( j * l ) l pp q ;
     done ;
     while !k > 1 do
      k := ( !k + 1 )/ 2 ;
      for j = 0 to !k do
       hh.(j) <- poly_real_plus h.( 2 * j ) ( mult_rule !g h.( 2 * j + 1 ) )
      done ;
      for j = 0 to !k - 1 do
       h.(j) <- Matrix.vector_float_copy hh.(j) ;
      done ;
      if !k > 1 then g := mult_rule !g !g ;
     done ;
     h.(0) ;;

(** {v poly_real_evaluate comp_rule polynomial real v} *)
let poly_real_evaluate = fun comp_rule (p:float array) (x0:float) ->
 ( comp_rule p [| x0 |] ).(0) ;;


(** {v real_sylvester_matrix polynomial1 polynomial2 v} *)
let real_sylvester_matrix = fun (p:float array) (q:float array) ->
 let dp = int_of_float ( poly_real_deg p )
 and dq = int_of_float ( poly_real_deg q ) in
  let dd = dp + dq in
   let m = Array.make_matrix dd dd 0. in
    for i = 0 to pred dq do
     let row = m.(i)
     and ii = i + dp in
      for j = i to ii do
       row.(j) <- p.( ii - j ) ;
      done ;
    done ;
    for i = 0 to pred dp do
     let ii = i + dq in
      let row = m.(ii) in
       for j = i to ii do
        row.(j) <- q.( ii - j ) ;
       done ;
    done ;
    m ;;

(** {v real_resultant det_methode polynomial1 polynomial2 v} *)
let real_resultant = fun det_methode (p:float array) (q:float array) ->
 let m = real_sylvester_matrix p q in
  det_methode m ;;

(** {v real_discriminant det_methode polynomial v} *)
let real_discriminant = fun det_methode (p:float array) ->
 real_resultant det_methode p ( poly_real_deriv p ) ;;




(** 
{3 Polynômes à coefficients entiers}
{3 Polynomials with integer coefficients}
*)
(** {C  } *)




(** {v poly_int_deg polynomial v} *) 
let poly_int_deg = function (p:int array) ->
 let r = ref ( ( Array.length p ) - 1 )
 and deg = ref neg_infinity in
  if !r >= 0 then 
   begin
    while !r >= 0 do
     begin 
      if p.(!r) <> 0 then
       ( deg := float !r ; r := -1 )
      else r := !r - 1 ;
     end
    done ;
   end ;
   !deg ;;

(** {v poly_int_val polynomial v} *) 
let poly_int_val = function (p:int array) ->
 let rr = ( ( Array.length p ) - 1 )
 and r = ref 0
 and valuation = ref infinity in
  if !r <= rr then 
   begin
    while !r <= rr do
     begin 
      if p.(!r) <> 0 then
       ( valuation := float !r ; r := max_int )
      else r := !r + 1 ;
     end
    done ;
   end ;
   !valuation ;;

(** {v poly_int_cleanup polynomial v} *)
let poly_int_cleanup = function (p:int array) ->
 let d = poly_int_deg p in
  if d < 0. then [| 0 |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) 0 in
      for i = 0 to dd do
       q.(i) <- p.(i) ;
      done ;
      q ;
   end ;;

(** {v poly_int_deriv polynomial v} *) 
let poly_int_deriv = function (p:int array) ->
 let pp = ( Array.length p ) - 1 in
  let r = Array.make pp 0 in
   for i = 1 to pp do
    r.( i - 1 ) <- i * p.(i)
   done ;
   r ;;

(** {v poly_int_plus polynomial1 polynomial2 v} *) 
let poly_int_plus = fun (p:int array) (q:int array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let x = Array.concat [ p ; ( Array.make ( rr - pp ) 0 ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) 0 ) ] in
    Matrix.vector_int_plus x y ;;

(** {v poly_int_minus polynomial1 polynomial2 v} *) 
let poly_int_minus = fun (p:int array) (q:int array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let x = Array.concat [ p ; ( Array.make ( rr - pp ) 0 ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) 0 ) ] in
    Matrix.vector_int_minus x y ;;

(** {v poly_int_mult polynomial1 polynomial2 v} *) 
let poly_int_mult = fun (p:int array) (q:int array) ->
 let pp = Array.length p
 and qq = Array.length q
 and accu = ref 0 in
  let qqq = qq - 1 in
   let rr = pp + qqq in
    let x = Array.concat [ p ; ( Array.make qq 0 ) ]
    and y = Array.concat [ q ; ( Array.make pp 0 ) ]
    and r = Array.make rr 0 in
     for i = 0 to rr - 1 do
      for j = 0 to i do
       accu := !accu + x.(j) * y.( i - j ) ;
      done ;
      r.(i) <- !accu ;
      accu := 0 ;
     done ;
     r ;;

(** {v poly_int_mult_karatsuba threshold polynomial1 polynomial2 v} *)
let rec poly_int_mult_karatsuba = fun (threshold:int) (p:int array) (q:int array) ->
 let l = Array.length p
 and ll = Array.length q in
  let l_l = max l ll in
   if l_l <= threshold then poly_int_mult p q
   else
    begin
     let half = ( l_l ) / 2
     and p_p = Array.append p ( Array.make ( l_l - l ) 0 )
     and q_q = Array.append q ( Array.make ( l_l - ll ) 0 ) in
      let pp = Array.sub p_p 0 half
      and ppp = Array.sub p_p half ( l_l - half )
      and qq = Array.sub q_q 0 half
      and qqq = Array.sub q_q half ( l_l - half ) in
       let debut = poly_int_mult_karatsuba threshold pp qq
       and fin = poly_int_mult_karatsuba threshold ppp qqq
       and psum = poly_int_plus pp ppp
       and qsum = poly_int_plus qq qqq in
        let mix = poly_int_mult_karatsuba threshold psum qsum in
         let inter = poly_int_minus mix debut in
          let milieu = poly_int_minus inter fin in
           let first = poly_int_plus debut ( Array.append ( Array.make ( half ) 0 ) milieu ) in
            let raw_prod = poly_int_plus first ( Array.append ( Array.make ( 2 * half ) 0 ) fin ) in
             poly_int_cleanup raw_prod
    end ;;

(** {v poly_int_pow mult_rule power polynomial v} *) 
let rec poly_int_pow = fun mult_rule (n:int) (p:int array) ->
 match n with
  | 0 -> [| 1 |]
  | 1 -> p
  | 2 -> mult_rule p p
  | _ ->
   begin
    let nn = n / 2 in
     let pp = poly_int_pow mult_rule nn p in
      let prod = mult_rule pp pp in
       if n mod 2 = 0 then
        prod
       else
        mult_rule prod p
   end ;;

(** {v poly_int_finite_prod mult_rule polynomial_array v} *) 
let rec poly_int_finite_prod = fun mult_rule (p:int array array) ->
 let n = ( Array.length p ) - 1
 and q = ref p.(0) in
  for i = 1 to n do
   q := mult_rule !q p.(i) ;
  done ;
  !q ;;

(** {v poly_int_from_roots mult_rule roots_array v} *) 
let poly_int_from_roots = fun mult_rule (r:int array) ->
 let a = Array.map poly_int_x_a r in
  poly_int_finite_prod mult_rule a ;;

(** {v poly_int_horner_comp polynomial1 polynomial2 v} *) 
let poly_int_horner_comp = fun (p:int array) (q:int array) ->
 let pp = Array.length p in
  let res = ref [| p.( pp - 1 ) |] in 
   for i = pp - 2 downto 0 do
    res := poly_int_plus ( poly_int_mult !res q ) [| p.(i) |] ;
   done ;
   !res ;;

(** {v poly_int_ranged_horner_comp mult_rule index order polynomial1 polynomial2 v} *) 
let poly_int_ranged_horner_comp = fun mult_rule (i:int) (l:int) (p:int array) (q:int array) ->
 let res = ref [| p.( i + l - 1 ) |] in 
  for j = l - 2 downto 0 do
   res := poly_int_plus ( mult_rule !res q ) [| p.( i + j ) |] ;
  done ;
  !res ;;


(** {v poly_int_brent_kung_hart_novocin_comp mult_rule parameter polynomial1 polynomial2 v} The length [l] must be greater than or equal to [3].
The algorithm comes from the document located at the following address.

[http://hal-ens-lyon.archives-ouvertes.fr/docs/00/54/61/02/PDF/dmtcs_NOVOCIN2010.pdf]

L'algorithme provient du document situé à l'adresse précédente.
La longueur [l] doit être supérieure ou égale à [3]. *) 
let poly_int_brent_kung_hart_novocin_comp = fun mult_rule (l:int) (p:int array) (q:int array) ->
 let n = ( Array.length p ) - 1
 and g = ref ( poly_int_pow mult_rule l q ) in
  let k = ref ( ( n + 1 ) / l + 1 ) in
   let pp = Array.append p ( Array.make ( !k * l - n ) 0 ) in
    k := ( 1 + ( Array.length pp ) ) / l ;
    let h = Array.make_matrix ( ( !k + l ) * 2 ) 1 0
    and hh = Array.make_matrix ( ( !k + l ) * 2 ) 1 0 in 
     for j = 0 to !k - 1 do
      h.(j) <- poly_int_ranged_horner_comp mult_rule ( j * l ) l pp q ;
     done ;
     while !k > 1 do
      k := ( !k + 1 )/ 2 ;
      for j = 0 to !k do
       hh.(j) <- poly_int_plus h.( 2 * j ) ( mult_rule !g h.( 2 * j + 1 ) )
      done ;
      for j = 0 to !k - 1 do
       h.(j) <- Matrix.vector_int_copy hh.(j) ;
      done ;
      if !k > 1 then g := mult_rule !g !g ;
     done ;
     h.(0) ;;

(** {v poly_int_evaluate comp_rule polynomial integer v} *)
let poly_int_evaluate = fun comp_rule (p:int array) (x0:int) ->
 ( comp_rule p [| x0 |] ).(0) ;;


(** {v int_sylvester_matrix polynomial1 polynomial2 v} *)
let int_sylvester_matrix = fun (p:int array) (q:int array) ->
 let dp = int_of_float ( poly_int_deg p )
 and dq = int_of_float ( poly_int_deg q ) in
  let dd = dp + dq in
   let m = Array.make_matrix dd dd 0 in
    for i = 0 to pred dq do
     let row = m.(i)
     and ii = i + dp in
      for j = i to ii do
       row.(j) <- p.( ii - j ) ;
      done ;
    done ;
    for i = 0 to pred dp do
     let ii = i + dq in
      let row = m.(ii) in
       for j = i to ii do
        row.(j) <- q.( ii - j ) ;
       done ;
    done ;
    m ;;

(** {v int_resultant det_methode polynomial1 polynomial2 v} *)
let int_resultant = fun det_methode (p:int array) (q:int array) ->
 let m = int_sylvester_matrix p q in
  det_methode m ;;

(** {v int_discriminant det_methode polynomial v} *)
let int_discriminant = fun det_methode (p:int array) ->
 int_resultant det_methode p ( poly_int_deriv p ) ;;




(** 
{3 Polynômes à coefficients complexes}
{3 Polynomials with complex coefficients}
*)
(** {C  } *)




(** {v poly_complex_deg polynomial v} *) 
let poly_complex_deg = function (p:float array array array) ->
 let r = ref ( ( Array.length p ) - 1 )
 and deg = ref neg_infinity in
  if !r >= 0 then 
   begin
    while !r >= 0 do
     begin 
      let coeff = Matrix.matrix_float_norm_inf p.(!r) in
       if coeff <> 0. then
        if coeff > max_float then
         ( deg := infinity ; r := -1 )
         else ( deg := float !r ; r := -1 )
       else r := !r - 1 ;
     end
    done ;
   end ;
   !deg ;;

(** {v poly_complex_val polynomial v} *) 
let poly_complex_val = function (p:float array array array) ->
 let rr = ( ( Array.length p ) - 1 )
 and r = ref 0
 and valuation = ref infinity in
  if !r <= rr then 
   begin
    while !r <= rr do
     begin
      let coeff = Matrix.matrix_float_norm_inf p.(!r) in
       if coeff <> 0. then
        begin
         if coeff > max_float then
          ( valuation := neg_infinity ; r := max_int )
         else ( valuation := float !r ; r := max_int )
        end
       else r := !r + 1 ;
     end
    done ;
   end ;
   !valuation ;;

(** {v poly_complex_cleanup polynomial v} *)
let poly_complex_cleanup = function (p:float array array array) ->
 let d = poly_complex_deg p in
  if d < 0. then [| complex_0 |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) complex_0 in
      for i = 0 to dd do
       q.(i) <- p.(i) ;
      done ;
      q ;
   end ;;

(** {v poly_complex_normalize polynomial v} *)
let poly_complex_normalize = function (p:float array array array) ->
 let d = poly_complex_deg p in
  if d < 0. then [| complex_0 |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) complex_1
     and coeff = complex_inv p.(dd) in
      for i = 0 to dd - 1 do
       q.(i) <- Matrix.matrix_float_prod p.(i) coeff ;
      done ;
      q ;
   end ;;

(** {v poly_complex_copy polynomial v} *)
let poly_complex_copy = function (p:float array array array) ->
 let r = Array.length p in
  let q = Array.make r complex_0 in
   for i = 0 to r - 1 do
    q.(i) <- Matrix.matrix_float_copy p.(i)
   done ;
   q ;;

(** {v complex_poly_to_complex_vector polynomial v} *)
let complex_poly_to_complex_vector = function (p:float array array array) ->
 let l = Array.length p in
  let v = Array.make ( 2 * l ) 0. in
   for i = 0 to l - 1 do
    v.(i) <- 0.5 *. ( p.(i).(0).(0) +. p.(i).(1).(1) ) ;
    v.( l + i ) <- 0.5 *. ( p.(i).(1).(0) -. p.(i).(0).(1) ) ;
   done ;
   v ;;

(** {v complex_poly_to_complex_double_vector polynomial v} *)
let complex_poly_to_complex_double_vector = function (p:float array array array) ->
 let l = Array.length p in
  let v = Array.make ( 2 * l ) 0.
  and vv = Array.make ( 2 * l ) 0. in
   for i = 0 to l - 1 do
    v.(i) <- p.(i).(0).(0) ;
    v.( l + i ) <- p.(i).(1).(0) ;
    vv.(i) <- p.(i).(0).(1) ;
    vv.( l + i ) <- p.(i).(1).(1) ;
   done ;
   [| v ; vv |] ;;

(** {v complex_vector_to_complex_poly vector v} *)
let complex_vector_to_complex_poly = function (v:float array) ->
 let l = ( Array.length v ) / 2 in
  let p = Array.make l complex_0 in
   for i = 0 to l - 1 do
    p.(i) <- [| [| v.(i) ; -. v.( l + i ) |] ; [| v.( l + i ) ; v.(i) |] |] ;
   done ;
   p ;;

(** {v poly_complex_deriv polynomial v} *) 
let poly_complex_deriv = function (p:float array array array) ->
 let pp = ( Array.length p ) - 1 in
  let r = Array.make pp ( Array.make_matrix 2 2 0. ) in
   for i = 1 to pp do
    r.( i - 1 ) <- Matrix.matrix_float_scal_mult ( float i ) p.(i)
   done ;
   r ;;

(** {v poly_complex_plus polynomial1 polynomial2 v} *) 
let poly_complex_plus = fun (p:float array array array) (q:float array array array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let r = Array.make rr ( Array.make_matrix 2 2 0. )
   and x = Array.concat [ p ; ( Array.make ( rr - pp ) ( Array.make_matrix 2 2 0. ) ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) ( Array.make_matrix 2 2 0. ) ) ] in
    for i = 0 to rr - 1 do
     r.(i) <- Matrix.matrix_float_plus x.(i) y.(i)
    done ; 
    r ;;

(** {v poly_complex_minus polynomial1 polynomial2 v} *) 
let poly_complex_minus = fun (p:float array array array) (q:float array array array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let r = Array.make rr ( Array.make_matrix 2 2 0. )
   and x = Array.concat [ p ; ( Array.make ( rr - pp ) ( Array.make_matrix 2 2 0. ) ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) ( Array.make_matrix 2 2 0. ) ) ] in
    for i = 0 to rr - 1 do
     r.(i) <- Matrix.matrix_float_minus x.(i) y.(i)
    done ; 
    r ;;

(** {v poly_complex_mult polynomial1 polynomial2 v} *) 
let poly_complex_mult = fun (p:float array array array) (q:float array array array) ->
 let pp = Array.length p
 and qq = Array.length q
 and accu = ref ( Array.make_matrix 2 2 0. ) in
  let qqq = qq - 1 in
   let rr = pp + qqq in
    let x = Array.concat [ p ; ( Array.make qq ( Array.make_matrix 2 2 0. ) ) ]
    and y = Array.concat [ q ; ( Array.make pp ( Array.make_matrix 2 2 0. ) ) ]
    and r = Array.make rr ( Array.make_matrix 2 2 0. ) in
     for i = 0 to rr - 1 do
      for j = 0 to i do
       accu := Matrix.matrix_float_plus !accu ( Matrix.matrix_float_prod x.(j) y.( i - j ) ) ;
      done ;
      r.(i) <- !accu ;
      accu := Array.make_matrix 2 2 0. ;
     done ;
     r ;;

(** {v poly_complex_mult_karatsuba threshold polynomial1 polynomial2 v} *)
let rec poly_complex_mult_karatsuba = fun (threshold:int) (p:float array array array) (q:float array array array) ->
 let l = Array.length p
 and ll = Array.length q in
  let l_l = max l ll in
   if l_l <= threshold then poly_complex_mult p q
   else
    begin
     let half = ( l_l ) / 2
     and p_p = Array.append p ( Array.make ( l_l - l ) complex_0 )
     and q_q = Array.append q ( Array.make ( l_l - ll ) complex_0 ) in
      let pp = Array.sub p_p 0 half
      and ppp = Array.sub p_p half ( l_l - half )
      and qq = Array.sub q_q 0 half
      and qqq = Array.sub q_q half ( l_l - half ) in
       let debut = poly_complex_mult_karatsuba threshold pp qq
       and fin = poly_complex_mult_karatsuba threshold ppp qqq
       and psum = poly_complex_plus pp ppp
       and qsum = poly_complex_plus qq qqq in
        let mix = poly_complex_mult_karatsuba threshold psum qsum in
         let inter = poly_complex_minus mix debut in
          let milieu = poly_complex_minus inter fin in
           let first = poly_complex_plus debut ( Array.append ( Array.make ( half ) complex_0 ) milieu ) in
            let raw_prod = poly_complex_plus first ( Array.append ( Array.make ( 2 * half ) complex_0 ) fin ) in
             poly_complex_cleanup raw_prod
    end ;;

(** {v poly_complex_pow mult_rule power polynomial v} *) 
let rec poly_complex_pow = fun mult_rule (n:int) (p:float array array array) ->
 match n with
  | 0 -> [| complex_1 |]
  | 1 -> p
  | 2 -> mult_rule p p
  | _ ->
   begin
    let nn = n / 2 in
     let pp = poly_complex_pow mult_rule nn p in
      let prod = mult_rule pp pp in
       if n mod 2 = 0 then
        prod
       else
        mult_rule prod p
   end ;;

(** {v poly_complex_finite_prod mult_rule polynomial_array v} *) 
let rec poly_complex_finite_prod = fun mult_rule (p:float array array array array) ->
 let n = ( Array.length p ) - 1
 and q = ref p.(0) in
  for i = 1 to n do
   q := mult_rule !q p.(i) ;
  done ;
  !q ;;

(** {v poly_complex_from_roots mult_rule roots_array v} *) 
let poly_complex_from_roots = fun mult_rule (r:float array array array) ->
 let a = Array.map poly_complex_x_a r in
  poly_complex_finite_prod mult_rule a ;;

(** {v poly_complex_horner_comp polynomial1 polynomial2 v} *) 
let poly_complex_horner_comp = fun (p:float array array array ) (q:float array array array) ->
 let pp = Array.length p in
  let res = ref [| p.( pp - 1 ) |] in 
   for i = pp - 2 downto 0 do
    res := poly_complex_plus ( poly_complex_mult !res q ) [| p.(i) |] ;
   done ;
   !res ;;

(** {v poly_complex_ranged_horner_comp mult_rule index order polynomial1 polynomial2 v} *) 
let poly_complex_ranged_horner_comp = fun mult_rule (i:int) (l:int) (p:float array array array) (q:float array array array) ->
 let res = ref [| p.( i + l - 1 ) |] in 
  for j = l - 2 downto 0 do
   res := poly_complex_plus ( mult_rule !res q ) [| p.( i + j ) |] ;
  done ;
  !res ;;


(** {v poly_complex_brent_kung_hart_novocin_comp mult_rule parameter polynomial1 polynomial2 v} The length [l] must be greater than or equal to [3].
The algorithm comes from the document located at the following address.

[http://hal-ens-lyon.archives-ouvertes.fr/docs/00/54/61/02/PDF/dmtcs_NOVOCIN2010.pdf]

L'algorithme provient du document situé à l'adresse précédente.
La longueur [l] doit être supérieure ou égale à [3]. *) 
let poly_complex_brent_kung_hart_novocin_comp = fun mult_rule (l:int) (p:float array array array) (q:float array array array) ->
 let n = ( Array.length p ) - 1
 and g = ref ( poly_complex_pow mult_rule l q ) in
  let k = ref ( ( n + 1 ) / l + 1 ) in
   let pp = Array.append p ( Array.make ( !k * l - n ) complex_0 ) in
    k := ( 1 + ( Array.length pp ) ) / l ;
    let h = Array.make_matrix ( ( !k + l ) * 2 ) 1 complex_0
    and hh = Array.make_matrix ( ( !k + l ) * 2 ) 1 complex_0 in 
     for j = 0 to !k - 1 do
      h.(j) <- poly_complex_ranged_horner_comp mult_rule ( j * l ) l pp q ;
     done ;
     while !k > 1 do
      k := ( !k + 1 )/ 2 ;
      for j = 0 to !k do
       hh.(j) <- poly_complex_plus h.( 2 * j ) ( mult_rule !g h.( 2 * j + 1 ) )
      done ;
      for j = 0 to !k - 1 do
       h.(j) <- poly_complex_copy hh.(j) ;
      done ;
      if !k > 1 then g := mult_rule !g !g ;
     done ;
     h.(0) ;;

(** {v poly_complex_evaluate comp_rule polynomial complex v} *)
let poly_complex_evaluate = fun comp_rule (p:float array array array) (x0:float array array) ->
 ( comp_rule p [| x0 |] ).(0) ;;


(** {v complex_sylvester_matrix polynomial1 polynomial2 v} *)
let complex_sylvester_matrix = fun (p:float array array array) (q:float array array array) ->
 let dp = int_of_float ( poly_complex_deg p )
 and dq = int_of_float ( poly_complex_deg q ) in
  let dd = dp + dq in
   let ddd = 2 * dd in
    let m = Array.make_matrix ddd ddd 0. in
     for i = 0 to pred dq do
      let first_row = m.(i)
      and second_row = m.( dd + i )
      and ii = i + dp in
       for j = i to ii do
        let jj = dd + j in
         first_row.(j) <- complex_real_part p.( ii - j ) ;
         first_row.(jj) <- -. complex_imaginary_part p.( ii - j ) ;
         second_row.(j) <- complex_imaginary_part p.( ii - j ) ;
         second_row.(jj) <- complex_real_part p.( ii - j ) ;
       done ;
     done ;
     for i = 0 to pred dp do
      let ii = i + dq in
       let first_row = m.(ii)
       and second_row = m.( dd + ii ) in
        for j = i to ii do
         let jj = dd + j in
          first_row.(j) <- complex_real_part q.( ii - j ) ;
          first_row.(jj) <- -. complex_imaginary_part q.( ii - j ) ;
          second_row.(j) <- complex_imaginary_part q.( ii - j ) ;
          second_row.(jj) <- complex_real_part q.( ii - j ) ;
        done ;
     done ;
     m ;;

(** {v complex_resultant det_methode polynomial1 polynomial2 v} *)
let complex_resultant = fun det_methode (p:float array array array) (q:float array array array) ->
 let m = complex_sylvester_matrix p q in
  det_methode m ;;

(** {v complex_discriminant det_methode polynomial v} *)
let complex_discriminant = fun det_methode (p:float array array array) ->
 complex_resultant det_methode p ( poly_complex_deriv p ) ;;




(** 
{3 Polynômes à coefficients entiers de Gauss}
{3 Polynomials with Gauss integer coefficients}
*)
(** {C  } *)




(** {v poly_gauss_deg polynomial v} *) 
let poly_gauss_deg = function (p:int array array array) ->
 let r = ref ( ( Array.length p ) - 1 )
 and deg = ref neg_infinity in
  if !r >= 0 then 
   begin
    while !r >= 0 do
     begin 
      let coeff = Matrix.matrix_int_norm_inf p.(!r) in
       if coeff <> 0 then
        ( deg := float !r ; r := -1 )
       else r := !r - 1 ;
     end
    done ;
   end ;
   !deg ;;

(** {v poly_gauss_val polynomial v} *) 
let poly_gauss_val = function (p:int array array array) ->
 let rr = ( ( Array.length p ) - 1 )
 and r = ref 0
 and valuation = ref infinity in
  if !r <= rr then 
   begin
    while !r <= rr do
     begin
      let coeff = Matrix.matrix_int_norm_inf p.(!r) in
       if coeff <> 0 then
        ( valuation := float !r ; r := max_int )
       else r := !r + 1 ;
     end
    done ;
   end ;
   !valuation ;;

(** {v poly_gauss_cleanup polynomial v} *)
let poly_gauss_cleanup = function (p:int array array array) ->
 let d = poly_gauss_deg p in
  if d < 0. then [| gauss_0 |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) gauss_0 in
      for i = 0 to dd do
       q.(i) <- p.(i) ;
      done ;
      q ;
   end ;;

(** {v poly_gauss_copy polynomial v} *)
let poly_gauss_copy = function (p:int array array array) ->
 let r = Array.length p in
  let q = Array.make r gauss_0 in
   for i = 0 to r - 1 do
    q.(i) <- Matrix.matrix_int_copy p.(i)
   done ;
   q ;;

(** {v gauss_poly_to_gauss_vector polynomial v} *)
let gauss_poly_to_gauss_vector = function (p:int array array array) ->
 let l = Array.length p in
  let v = Array.make ( 2 * l ) 0 in
   for i = 0 to l - 1 do
    v.(i) <- ( p.(i).(0).(0) + p.(i).(1).(1) ) / 2 ;
    v.( l + i ) <- ( p.(i).(1).(0) - p.(i).(0).(1) ) / 2 ;
   done ;
   v ;;

(** {v gauss_poly_to_gauss_double_vector polynomial v} *)
let gauss_poly_to_gauss_double_vector = function (p:int array array array) ->
 let l = Array.length p in
  let v = Array.make ( 2 * l ) 0
  and vv = Array.make ( 2 * l ) 0 in
   for i = 0 to l - 1 do
    v.(i) <- p.(i).(0).(0) ;
    v.( l + i ) <- p.(i).(1).(0) ;
    vv.(i) <- p.(i).(0).(1) ;
    vv.( l + i ) <- p.(i).(1).(1) ;
   done ;
   [| v ; vv |] ;;

(** {v gauss_vector_to_gauss_poly vector v} *)
let gauss_vector_to_gauss_poly = function (v:int array) ->
 let l = ( Array.length v ) / 2 in
  let p = Array.make l gauss_0 in
   for i = 0 to l - 1 do
    p.(i) <- [| [| v.(i) ; - v.( l + i ) |] ; [| v.( l + i ) ; v.(i) |] |] ;
   done ;
   p ;;

(** {v poly_gauss_deriv polynomial v} *) 
let poly_gauss_deriv = function (p:int array array array) ->
 let pp = ( Array.length p ) - 1 in
  let r = Array.make pp ( Array.make_matrix 2 2 0 ) in
   for i = 1 to pp do
    r.( i - 1 ) <- Matrix.matrix_int_scal_mult i p.(i)
   done ;
   r ;;

(** {v poly_gauss_plus polynomial1 polynomial2 v} *) 
let poly_gauss_plus = fun (p:int array array array) (q:int array array array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let r = Array.make rr ( Array.make_matrix 2 2 0 )
   and x = Array.concat [ p ; ( Array.make ( rr - pp ) ( Array.make_matrix 2 2 0 ) ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) ( Array.make_matrix 2 2 0 ) ) ] in
    for i = 0 to rr - 1 do
     r.(i) <- Matrix.matrix_int_plus x.(i) y.(i)
    done ; 
    r ;;

(** {v poly_gauss_minus polynomial1 polynomial2 v} *) 
let poly_gauss_minus = fun (p:int array array array) (q:int array array array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let r = Array.make rr ( Array.make_matrix 2 2 0 )
   and x = Array.concat [ p ; ( Array.make ( rr - pp ) ( Array.make_matrix 2 2 0 ) ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) ( Array.make_matrix 2 2 0 ) ) ] in
    for i = 0 to rr - 1 do
     r.(i) <- Matrix.matrix_int_minus x.(i) y.(i)
    done ; 
    r ;;

(** {v poly_gauss_mult polynomial1 polynomial2 v} *) 
let poly_gauss_mult = fun (p:int array array array) (q:int array array array) ->
 let pp = Array.length p
 and qq = Array.length q
 and accu = ref ( Array.make_matrix 2 2 0 ) in
  let qqq = qq - 1 in
   let rr = pp + qqq in
    let x = Array.concat [ p ; ( Array.make qq ( Array.make_matrix 2 2 0 ) ) ]
    and y = Array.concat [ q ; ( Array.make pp ( Array.make_matrix 2 2 0 ) ) ]
    and r = Array.make rr ( Array.make_matrix 2 2 0 ) in
     for i = 0 to rr - 1 do
      for j = 0 to i do
       accu := Matrix.matrix_int_plus !accu ( Matrix.matrix_int_prod x.(j) y.( i - j ) ) ;
      done ;
      r.(i) <- !accu ;
      accu := Array.make_matrix 2 2 0 ;
     done ;
     r ;;

(** {v poly_gauss_mult_karatsuba threshold polynomial1 polynomial2 v} *)
let rec poly_gauss_mult_karatsuba = fun (threshold:int) (p:int array array array) (q:int array array array) ->
 let l = Array.length p
 and ll = Array.length q in
  let l_l = max l ll in
   if l_l <= threshold then poly_gauss_mult p q
   else
    begin
     let half = ( l_l ) / 2
     and p_p = Array.append p ( Array.make ( l_l - l ) gauss_0 )
     and q_q = Array.append q ( Array.make ( l_l - ll ) gauss_0 ) in
      let pp = Array.sub p_p 0 half
      and ppp = Array.sub p_p half ( l_l - half )
      and qq = Array.sub q_q 0 half
      and qqq = Array.sub q_q half ( l_l - half ) in
       let debut = poly_gauss_mult_karatsuba threshold pp qq
       and fin = poly_gauss_mult_karatsuba threshold ppp qqq
       and psum = poly_gauss_plus pp ppp
       and qsum = poly_gauss_plus qq qqq in
        let mix = poly_gauss_mult_karatsuba threshold psum qsum in
         let inter = poly_gauss_minus mix debut in
          let milieu = poly_gauss_minus inter fin in
           let first = poly_gauss_plus debut ( Array.append ( Array.make ( half ) gauss_0 ) milieu ) in
            let raw_prod = poly_gauss_plus first ( Array.append ( Array.make ( 2 * half ) gauss_0 ) fin ) in
             poly_gauss_cleanup raw_prod
    end ;;

(** {v poly_gauss_pow mult_rule power polynomial v} *) 
let rec poly_gauss_pow = fun mult_rule (n:int) (p:int array array array) ->
 match n with
  | 0 -> [| gauss_1 |]
  | 1 -> p
  | 2 -> mult_rule p p
  | _ ->
   begin
    let nn = n / 2 in
     let pp = poly_gauss_pow mult_rule nn p in
      let prod = mult_rule pp pp in
       if n mod 2 = 0 then
        prod
       else
        mult_rule prod p
   end ;;

(** {v poly_gauss_finite_prod mult_rule polynomial_array v} *) 
let rec poly_gauss_finite_prod = fun mult_rule (p:int array array array array) ->
 let n = ( Array.length p ) - 1
 and q = ref p.(0) in
  for i = 1 to n do
   q := mult_rule !q p.(i) ;
  done ;
  !q ;;

(** {v poly_gauss_from_roots mult_rule roots_array v} *) 
let poly_gauss_from_roots = fun mult_rule (r:int array array array) ->
 let a = Array.map poly_gauss_x_a r in
  poly_gauss_finite_prod mult_rule a ;;

(** {v poly_gauss_horner_comp polynomial1 polynomial2 v} *) 
let poly_gauss_horner_comp = fun (p:int array array array ) (q:int array array array) ->
 let pp = Array.length p in
  let res = ref [| p.( pp - 1 ) |] in 
   for i = pp - 2 downto 0 do
    res := poly_gauss_plus ( poly_gauss_mult !res q ) [| p.(i) |] ;
   done ;
   !res ;;

(** {v poly_gauss_ranged_horner_comp mult_rule index order polynomial1 polynomial2 v} *) 
let poly_gauss_ranged_horner_comp = fun mult_rule (i:int) (l:int) (p:int array array array) (q:int array array array) ->
 let res = ref [| p.( i + l - 1 ) |] in 
  for j = l - 2 downto 0 do
   res := poly_gauss_plus ( mult_rule !res q ) [| p.( i + j ) |] ;
  done ;
  !res ;;


(** {v poly_gauss_brent_kung_hart_novocin_comp mult_rule parameter polynomial1 polynomial2 v} The length [l] must be greater than or equal to [3].
The algorithm comes from the document located at the following address.

[http://hal-ens-lyon.archives-ouvertes.fr/docs/00/54/61/02/PDF/dmtcs_NOVOCIN2010.pdf]

L'algorithme provient du document situé à l'adresse précédente.
La longueur [l] doit être supérieure ou égale à [3]. *) 
let poly_gauss_brent_kung_hart_novocin_comp = fun mult_rule (l:int) (p:int array array array) (q:int array array array) ->
 let n = ( Array.length p ) - 1
 and g = ref ( poly_gauss_pow mult_rule l q ) in
  let k = ref ( ( n + 1 ) / l + 1 ) in
   let pp = Array.append p ( Array.make ( !k * l - n ) gauss_0 ) in
    k := ( 1 + ( Array.length pp ) ) / l ;
    let h = Array.make_matrix ( ( !k + l ) * 2 ) 1 gauss_0
    and hh = Array.make_matrix ( ( !k + l ) * 2 ) 1 gauss_0 in 
     for j = 0 to !k - 1 do
      h.(j) <- poly_gauss_ranged_horner_comp mult_rule ( j * l ) l pp q ;
     done ;
     while !k > 1 do
      k := ( !k + 1 )/ 2 ;
      for j = 0 to !k do
       hh.(j) <- poly_gauss_plus h.( 2 * j ) ( mult_rule !g h.( 2 * j + 1 ) )
      done ;
      for j = 0 to !k - 1 do
       h.(j) <- poly_gauss_copy hh.(j) ;
      done ;
      if !k > 1 then g := mult_rule !g !g ;
     done ;
     h.(0) ;;

(** {v poly_gauss_evaluate comp_rule polynomial gauss_integer v} *)
let poly_gauss_evaluate = fun comp_rule (p:int array array array) (x0:int array array) ->
 ( comp_rule p [| x0 |] ).(0) ;;


(** {v gauss_sylvester_matrix polynomial1 polynomial2 v} *)
let gauss_sylvester_matrix = fun (p:int array array array) (q:int array array array) ->
 let dp = int_of_float ( poly_gauss_deg p )
 and dq = int_of_float ( poly_gauss_deg q ) in
  let dd = dp + dq in
   let ddd = 2 * dd in
    let m = Array.make_matrix ddd ddd 0 in
     for i = 0 to pred dq do
      let first_row = m.(i)
      and second_row = m.( dd + i )
      and ii = i + dp in
       for j = i to ii do
        let jj = dd + j in
         first_row.(j) <- gauss_real_part p.( ii - j ) ;
         first_row.(jj) <- - gauss_imaginary_part p.( ii - j ) ;
         second_row.(j) <- gauss_imaginary_part p.( ii - j ) ;
         second_row.(jj) <- gauss_real_part p.( ii - j ) ;
       done ;
     done ;
     for i = 0 to pred dp do
      let ii = i + dq in
       let first_row = m.(ii)
       and second_row = m.( dd + ii ) in
        for j = i to ii do
         let jj = dd + j in
          first_row.(j) <- gauss_real_part q.( ii - j ) ;
          first_row.(jj) <- - gauss_imaginary_part q.( ii - j ) ;
          second_row.(j) <- gauss_imaginary_part q.( ii - j ) ;
          second_row.(jj) <- gauss_real_part q.( ii - j ) ;
        done ;
     done ;
     m ;;

(** {v gauss_resultant det_methode polynomial1 polynomial2 v} *)
let gauss_resultant = fun det_methode (p:int array array array) (q:int array array array) ->
 let m = gauss_sylvester_matrix p q in
  det_methode m ;;

(** {v gauss_discriminant det_methode polynomial v} *)
let gauss_discriminant = fun det_methode (p:int array array array) ->
 gauss_resultant det_methode p ( poly_gauss_deriv p ) ;;




(** 
{3 Polynômes à coefficients en précision étendue}
{3 Polynomials with coefficients in extended precision}
*)
(** {C  } *)




(** {v poly_sci_deg polynomial v} *) 
let poly_sci_deg = function (p:Num.num array array) ->
 let r = ref ( ( Array.length p ) - 1 )
 and deg = ref neg_infinity in
  if !r >= 0 then 
   begin
    while !r >= 0 do
     begin 
      let coeff = Sci.square_module p.(!r) in
       if Sci.not_eq_0 coeff then ( deg := float !r ; r := -1 )
       else r := !r - 1 ;
     end
    done ;
   end ;
   !deg ;;

(** {v poly_sci_val polynomial v} *) 
let poly_sci_val = function (p:Num.num array array) ->
 let rr = ( ( Array.length p ) - 1 )
 and r = ref 0
 and valuation = ref infinity in
  if !r <= rr then 
   begin
    while !r <= rr do
     begin
      let coeff = Sci.square_module p.(!r) in
       if Sci.not_eq_0 coeff then ( valuation := float !r ; r := max_int )
       else r := !r + 1 ;
     end
    done ;
   end ;
   !valuation ;;

(** {v poly_sci_cleanup polynomial v} *)
let poly_sci_cleanup = function (p:Num.num array array) ->
 let d = poly_sci_deg p in
  if d < 0. then [| Sci.sci_0 |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) Sci.sci_0 in
      for i = 0 to dd do
       q.(i) <- p.(i) ;
      done ;
      q ;
   end ;;

(** {v poly_sci_normalize polynomial v} *)
let poly_sci_normalize = function (p:Num.num array array) ->
 let d = poly_sci_deg p in
  if d < 0. then [| Sci.sci_0 |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) Sci.sci_1
     and coeff = Sci.inv p.(dd) in
      for i = 0 to dd - 1 do
       q.(i) <- Sci.mult p.(i) coeff ;
      done ;
      q ;
   end ;;

(** {v poly_sci_copy polynomial v} *)
let poly_sci_copy = function (p:Num.num array array) ->
 let r = Array.length p in
  let q = Array.make r Sci.sci_0 in
   for i = 0 to r - 1 do
    q.(i) <- Sci.sci_copy p.(i)
   done ;
   q ;;

(** {v poly_sci_deriv polynomial v} *) 
let poly_sci_deriv = function (p:Num.num array array) ->
 let pp = ( Array.length p ) - 1 in
  let r = Array.make pp Sci.sci_0 in
   for i = 1 to pp do
    r.( i - 1 ) <- Sci.mult ( Sci.sci_of_int i ) p.(i)
   done ;
   r ;;

(** {v poly_sci_plus polynomial1 polynomial2 v} *) 
let poly_sci_plus = fun (p:Num.num array array) (q:Num.num array array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let r = Array.make rr Sci.sci_0
   and x = Array.concat [ p ; ( Array.make ( rr - pp ) Sci.sci_0 ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) Sci.sci_0 ) ] in
    for i = 0 to rr - 1 do
     r.(i) <- Sci.plus x.(i) y.(i)
    done ; 
    r ;;

(** {v poly_sci_minus polynomial1 polynomial2 v} *) 
let poly_sci_minus = fun (p:Num.num array array) (q:Num.num array array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let r = Array.make rr Sci.sci_0
   and x = Array.concat [ p ; ( Array.make ( rr - pp ) Sci.sci_0 ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) Sci.sci_0 ) ] in
    for i = 0 to rr - 1 do
     r.(i) <- Sci.minus x.(i) y.(i)
    done ; 
    r ;;

(** {v poly_sci_mult polynomial1 polynomial2 v} *) 
let poly_sci_mult = fun (p:Num.num array array) (q:Num.num array array) ->
 let pp = Array.length p
 and qq = Array.length q
 and accu = ref Sci.sci_0 in
  let qqq = qq - 1 in
   let rr = pp + qqq in
    let x = Array.concat [ p ; ( Array.make qq Sci.sci_0 ) ]
    and y = Array.concat [ q ; ( Array.make pp Sci.sci_0 ) ]
    and r = Array.make rr Sci.sci_0 in
     for i = 0 to rr - 1 do
      for j = 0 to i do
       accu := Sci.plus !accu ( Sci.mult x.(j) y.( i - j ) ) ;
      done ;
      r.(i) <- !accu ;
      accu := Sci.sci_0 ;
     done ;
     r ;;

(** {v poly_sci_mult_karatsuba threshold polynomial1 polynomial2 v} *)
let rec poly_sci_mult_karatsuba = fun (threshold:int) (p:Num.num array array) (q:Num.num array array) ->
 let l = Array.length p
 and ll = Array.length q in
  let l_l = max l ll in
   if l_l <= threshold then poly_sci_mult p q
   else
    begin
     let half = ( l_l ) / 2
     and p_p = Array.append p ( Array.make ( l_l - l ) Sci.sci_0 )
     and q_q = Array.append q ( Array.make ( l_l - ll ) Sci.sci_0 ) in
      let pp = Array.sub p_p 0 half
      and ppp = Array.sub p_p half ( l_l - half )
      and qq = Array.sub q_q 0 half
      and qqq = Array.sub q_q half ( l_l - half ) in
       let debut = poly_sci_mult_karatsuba threshold pp qq
       and fin = poly_sci_mult_karatsuba threshold ppp qqq
       and psum = poly_sci_plus pp ppp
       and qsum = poly_sci_plus qq qqq in
        let mix = poly_sci_mult_karatsuba threshold psum qsum in
         let inter = poly_sci_minus mix debut in
          let milieu = poly_sci_minus inter fin in
           let first = poly_sci_plus debut ( Array.append ( Array.make ( half ) Sci.sci_0 ) milieu ) in
            let raw_prod = poly_sci_plus first ( Array.append ( Array.make ( 2 * half ) Sci.sci_0 ) fin ) in
             poly_sci_cleanup raw_prod
    end ;;

(** {v poly_sci_pow mult_rule power polynomial v} *) 
let rec poly_sci_pow = fun mult_rule (n:int) (p:Num.num array array) ->
 match n with
  | 0 -> [| Sci.sci_1 |]
  | 1 -> p
  | 2 -> mult_rule p p
  | _ ->
   begin
    let nn = n / 2 in
     let pp = poly_sci_pow mult_rule nn p in
     let prod = mult_rule pp pp in
      if n mod 2 = 0 then
       prod
      else
       mult_rule prod p
   end ;;

(** {v poly_sci_finite_prod mult_rule polynomial_array v} *) 
let rec poly_sci_finite_prod = fun mult_rule (p:Num.num array array array) ->
 let n = ( Array.length p ) - 1
 and q = ref p.(0) in
  for i = 1 to n do
   q := mult_rule !q p.(i) ;
  done ;
  !q ;;

(** {v poly_sci_from_roots mult_rule roots_array v} *) 
let poly_sci_from_roots = fun mult_rule (r:Num.num array array) ->
 let a = Array.map poly_sci_x_a r in
  poly_sci_finite_prod mult_rule a ;;

(** {v poly_sci_horner_comp polynomial1 polynomial2 v} *) 
let poly_sci_horner_comp = fun (p:Num.num array array ) (q:Num.num array array) ->
 let pp = Array.length p in
  let res = ref ( Array.make 1 ( Sci.sci_copy p.( pp - 1 ) ) ) in 
   for i = pp - 2 downto 0 do
    res := poly_sci_plus ( poly_sci_mult !res q ) ( Array.make 1 ( Sci.sci_copy p.(i) ) ) ;
   done ;
   !res ;;

(** {v poly_sci_ranged_horner_comp mult_rule index order polynomial1 polynomial2 v} *) 
let poly_sci_ranged_horner_comp = fun mult_rule (i:int) (l:int) (p:Num.num array array) (q:Num.num array array) ->
 let res = ref ( Array.make 1 ( Sci.sci_copy p.( i + l - 1 ) ) ) in 
  for j = l - 2 downto 0 do
   res := poly_sci_plus ( mult_rule !res q ) ( Array.make 1 ( Sci.sci_copy p.( i + j ) ) ) ;
  done ;
  !res ;;


(** {v poly_sci_brent_kung_hart_novocin_comp mult_rule parameter polynomial1 polynomial2 v} The length [l] must be greater than or equal to [3].
The algorithm comes from the document located at the following address.

[http://hal-ens-lyon.archives-ouvertes.fr/docs/00/54/61/02/PDF/dmtcs_NOVOCIN2010.pdf]

L'algorithme provient du document situé à l'adresse précédente.
La longueur [l] doit être supérieure ou égale à [3]. *) 
let poly_sci_brent_kung_hart_novocin_comp = fun mult_rule (l:int) (p:Num.num array array) (q:Num.num array array) ->
 let n = ( Array.length p ) - 1
 and g = ref ( poly_sci_pow mult_rule l q ) in
  let k = ref ( ( n + 1 ) / l + 1 ) in
   let pp = Array.append p ( Array.make ( !k * l - n ) Sci.sci_0 ) in
    k := ( 1 + ( Array.length pp ) ) / l ;
    let h = Array.make_matrix ( ( !k + l ) * 2 ) 1 Sci.sci_0
    and hh = Array.make_matrix ( ( !k + l ) * 2 ) 1 Sci.sci_0 in 
     for j = 0 to !k - 1 do
      h.(j) <- poly_sci_ranged_horner_comp mult_rule ( j * l ) l pp q ;
     done ;
     while !k > 1 do
      k := ( !k + 1 )/ 2 ;
      for j = 0 to !k do
       hh.(j) <- poly_sci_plus h.( 2 * j ) ( mult_rule !g h.( 2 * j + 1 ) )
      done ;
      for j = 0 to !k - 1 do
       h.(j) <- poly_sci_copy hh.(j) ;
      done ;
      if !k > 1 then g := mult_rule !g !g ;
     done ;
     h.(0) ;;

(** {v poly_sci_evaluate comp_rule polynomial complex v} *)
let poly_sci_evaluate = fun comp_rule (p:Num.num array array) (x0:Num.num array) ->
 ( comp_rule p [| x0 |] ).(0) ;;

(** {v poly_sci_eq_0 polynomial v} *)
let poly_sci_eq_0 = function (p:Num.num array array) ->
 Array.fold_left (&&) true ( Array.map ( function x -> ( Sci.square_module x ).(0) = Sci.num_0 ) p ) ;;

(** {v poly_sci_eq polynomial1 polynomial2 v} *)
let poly_sci_eq = fun (p:Num.num array array) (q:Num.num array array) ->
 poly_sci_eq_0 ( poly_sci_minus p q ) ;;




(** 
{3 Polynômes à coefficients en précision intermédiaire}
{3 Polynomials with coefficients in intermediate precision}
*)
(** {C  } *)




(** {v poly_sci_1024_normalize polynomial v} *)
let poly_sci_1024_normalize = function (p:Num.num array array) ->
 let d = poly_sci_deg p in
  if d < 0. then [| Sci.sci_0 |]
  else
   begin
    let dd = int_of_float d in
     let q = Array.make ( dd + 1 ) Sci.sci_1
     and coeff = Sci.inv_1024 p.(dd) in
      for i = 0 to dd - 1 do
       q.(i) <- Sci.mult_1024 p.(i) coeff ;
      done ;
      q ;
   end ;;

(** {v poly_sci_1024_deriv polynomial v} *) 
let poly_sci_1024_deriv = function (p:Num.num array array) ->
 let pp = ( Array.length p ) - 1 in
  let r = Array.make pp Sci.sci_0 in
   for i = 1 to pp do
    r.( i - 1 ) <- Sci.mult_1024 ( Sci.sci_of_int i ) p.(i)
   done ;
   r ;;

(** {v poly_sci_1024_plus polynomial1 polynomial2 v} *) 
let poly_sci_1024_plus = fun (p:Num.num array array) (q:Num.num array array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let r = Array.make rr Sci.sci_0
   and x = Array.concat [ p ; ( Array.make ( rr - pp ) Sci.sci_0 ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) Sci.sci_0 ) ] in
    for i = 0 to rr - 1 do
     r.(i) <- Sci.plus_1024 x.(i) y.(i)
    done ; 
    r ;;

(** {v poly_sci_1024_minus polynomial1 polynomial2 v} *) 
let poly_sci_1024_minus = fun (p:Num.num array array) (q:Num.num array array) ->
 let pp = Array.length p
 and qq = Array.length q in
  let rr = max pp qq in
   let r = Array.make rr Sci.sci_0
   and x = Array.concat [ p ; ( Array.make ( rr - pp ) Sci.sci_0 ) ]
   and y = Array.concat [ q ; ( Array.make ( rr - qq ) Sci.sci_0 ) ] in
    for i = 0 to rr - 1 do
     r.(i) <- Sci.minus_1024 x.(i) y.(i)
    done ; 
    r ;;

(** {v poly_sci_1024_mult polynomial1 polynomial2 v} *) 
let poly_sci_1024_mult = fun (p:Num.num array array) (q:Num.num array array) ->
 let pp = Array.length p
 and qq = Array.length q
 and accu = ref Sci.sci_0 in
  let qqq = qq - 1 in
   let rr = pp + qqq in
    let x = Array.concat [ p ; ( Array.make qq Sci.sci_0 ) ]
    and y = Array.concat [ q ; ( Array.make pp Sci.sci_0 ) ]
    and r = Array.make rr Sci.sci_0 in
     for i = 0 to rr - 1 do
      for j = 0 to i do
       accu := Sci.plus_1024 !accu ( Sci.mult_1024 x.(j) y.( i - j ) ) ;
      done ;
      r.(i) <- !accu ;
      accu := Sci.sci_0 ;
     done ;
     r ;;

(** {v poly_sci_1024_mult_karatsuba threshold polynomial1 polynomial2 v} *)
let rec poly_sci_1024_mult_karatsuba = fun (threshold:int) (p:Num.num array array) (q:Num.num array array) ->
 let l = Array.length p
 and ll = Array.length q in
  let l_l = max l ll in
   if l_l <= threshold then poly_sci_1024_mult p q
   else
    begin
     let half = ( l_l ) / 2
     and p_p = Array.append p ( Array.make ( l_l - l ) Sci.sci_0 )
     and q_q = Array.append q ( Array.make ( l_l - ll ) Sci.sci_0 ) in
      let pp = Array.sub p_p 0 half
      and ppp = Array.sub p_p half ( l_l - half )
      and qq = Array.sub q_q 0 half
      and qqq = Array.sub q_q half ( l_l - half ) in
       let debut = poly_sci_1024_mult_karatsuba threshold pp qq
       and fin = poly_sci_1024_mult_karatsuba threshold ppp qqq
       and psum = poly_sci_1024_plus pp ppp
       and qsum = poly_sci_1024_plus qq qqq in
        let mix = poly_sci_1024_mult_karatsuba threshold psum qsum in
         let inter = poly_sci_1024_minus mix debut in
          let milieu = poly_sci_1024_minus inter fin in
           let first = poly_sci_1024_plus debut ( Array.append ( Array.make ( half ) Sci.sci_0 ) milieu ) in
            let raw_prod = poly_sci_1024_plus first ( Array.append ( Array.make ( 2 * half ) Sci.sci_0 ) fin ) in
             poly_sci_cleanup raw_prod
    end ;;

(** {v poly_sci_1024_pow mult_rule power polynomial v} *) 
let rec poly_sci_1024_pow = fun mult_rule (n:int) (p:Num.num array array) ->
 match n with
  | 0 -> [| Sci.sci_1 |]
  | 1 -> p
  | 2 -> mult_rule p p
  | _ ->
   begin
    let nn = n / 2 in
     let pp = poly_sci_1024_pow mult_rule nn p in
      let prod = mult_rule pp pp in
       if n mod 2 = 0 then
        prod
       else
        mult_rule prod p
   end ;;

(** {v poly_sci_1024_finite_prod mult_rule polynomial_array v} *) 
let rec poly_sci_1024_finite_prod = fun mult_rule (p:Num.num array array array) ->
 let n = ( Array.length p ) - 1
 and q = ref p.(0) in
  for i = 1 to n do
   q := mult_rule !q p.(i) ;
  done ;
  !q ;;

(** {v poly_sci_1024_from_roots mult_rule roots_array v} *) 
let poly_sci_1024_from_roots = fun mult_rule (r:Num.num array array) ->
 let a = Array.map poly_sci_x_a r in
  poly_sci_1024_finite_prod mult_rule a ;;

(** {v poly_sci_1024_horner_comp polynomial1 polynomial2 v} *) 
let poly_sci_1024_horner_comp = fun (p:Num.num array array ) (q:Num.num array array) ->
 let pp = Array.length p in
  let res = ref ( Array.make 1 ( Sci.sci_copy p.( pp - 1 ) ) ) in 
   for i = pp - 2 downto 0 do
    res := poly_sci_1024_plus ( poly_sci_1024_mult !res q ) ( Array.make 1 ( Sci.sci_copy p.(i) ) ) ;
   done ;
   !res ;;

(** {v poly_sci_1024_ranged_horner_comp mult_rule index order polynomial1 polynomial2 v} *) 
let poly_sci_1024_ranged_horner_comp = fun mult_rule (i:int) (l:int) (p:Num.num array array) (q:Num.num array array) ->
 let res = ref ( Array.make 1 ( Sci.sci_copy p.( i + l - 1 ) ) ) in 
  for j = l - 2 downto 0 do
   res := poly_sci_1024_plus ( mult_rule !res q ) ( Array.make 1 ( Sci.sci_copy p.( i + j ) ) ) ;
  done ;
  !res ;;


(** {v poly_sci_1024_brent_kung_hart_novocin_comp mult_rule parameter polynomial1 polynomial2 v} The length [l] must be greater than or equal to [3].
The algorithm comes from the document located at the following address.

[http://hal-ens-lyon.archives-ouvertes.fr/docs/00/54/61/02/PDF/dmtcs_NOVOCIN2010.pdf]

L'algorithme provient du document situé à l'adresse précédente.
La longueur [l] doit être supérieure ou égale à [3]. *) 
let poly_sci_1024_brent_kung_hart_novocin_comp = fun mult_rule (l:int) (p:Num.num array array) (q:Num.num array array) ->
 let n = ( Array.length p ) - 1
 and g = ref ( poly_sci_1024_pow mult_rule l q ) in
  let k = ref ( ( n + 1 ) / l + 1 ) in
   let pp = Array.append p ( Array.make ( !k * l - n ) Sci.sci_0 ) in
    k := ( 1 + ( Array.length pp ) ) / l ;
    let h = Array.make_matrix ( ( !k + l ) * 2 ) 1 Sci.sci_0
    and hh = Array.make_matrix ( ( !k + l ) * 2 ) 1 Sci.sci_0 in 
     for j = 0 to !k - 1 do
      h.(j) <- poly_sci_1024_ranged_horner_comp mult_rule ( j * l ) l pp q ;
     done ;
     while !k > 1 do
      k := ( !k + 1 )/ 2 ;
      for j = 0 to !k do
       hh.(j) <- poly_sci_1024_plus h.( 2 * j ) ( mult_rule !g h.( 2 * j + 1 ) )
      done ;
      for j = 0 to !k - 1 do
       h.(j) <- poly_sci_copy hh.(j) ;
      done ;
      if !k > 1 then g := mult_rule !g !g ;
     done ;
     h.(0) ;;

(** {v poly_sci_1024_evaluate comp_rule polynomial complex v} *)
let poly_sci_1024_evaluate = fun comp_rule (p:Num.num array array) (x0:Num.num array) ->
 ( comp_rule p [| x0 |] ).(0) ;;

(** {v poly_sci_1024_eq_0 polynomial v} *)
let poly_sci_1024_eq_0 = function (p:Num.num array array) ->
 Array.fold_left (&&) true ( Array.map ( function x -> ( Sci.square_module_1024 x ).(0) = Sci.num_0 ) p ) ;;

(** {v poly_sci_1024_eq polynomial1 polynomial2 v} *)
let poly_sci_1024_eq = fun (p:Num.num array array) (q:Num.num array array) ->
 poly_sci_1024_eq_0 ( poly_sci_1024_minus p q ) ;;




(** 
{3 Conversions de types}
{3 Type conversions}
*)
(** {C  } *)




(** {v poly_real_to_complex polynomial v} *)
let poly_real_to_complex = function (p:float array) ->
 let f = function x -> Matrix.scal_float 2 2 x in
  Array.map f p ;;

(** {v poly_int_to_gauss polynomial v} *)
let poly_int_to_gauss = function (p:int array) ->
 let f = function x -> Matrix.scal_int 2 2 x in
  Array.map f p ;;

(** {v poly_complex_to_sci polynomial v} *)
let poly_complex_to_sci = function (p:float array array array) ->
 let f = function x -> Sci.sci_of_complex x in
  Array.map f p ;;

(** {v poly_sci_to_complex polynomial v} *)
let poly_sci_to_complex = function (p:Num.num array array) ->
 let f = function x -> Sci.complex_of_sci x in
  Array.map f p ;;

(** {v poly_real_to_sci polynomial v} *)
let poly_real_to_sci = function (p:float array) ->
poly_complex_to_sci ( poly_real_to_complex p ) ;;

(** {v poly_complex_real_part polynomial v} *)
let poly_complex_real_part = function (p:float array array array) ->
 let f = function x -> x.(0).(0) in
  Array.map f p ;;

(** {v poly_complex_imag_part polynomial v} *)
let poly_complex_imag_part = function (p:float array array array) ->
 let f = function x -> x.(1).(0) in
  Array.map f p ;;

(** {v poly_gauss_real_part polynomial v} *)
let poly_gauss_real_part = function (p:int array array array) ->
 let f = function x -> x.(0).(0) in
  Array.map f p ;;

(** {v poly_gauss_imag_part polynomial v} *)
let poly_gauss_imag_part = function (p:int array array array) ->
 let f = function x -> x.(0).(0) in
  Array.map f p ;;

(** {v poly_sci_real_part polynomial v} *)
let poly_sci_real_part = function (p:Num.num array array) ->
 let f = function x -> ( Sci.complex_of_sci x ).(0).(0) in
  Array.map f p ;;

(** {v poly_sci_imag_part polynomial v} *)
let poly_sci_imag_part = function (p:Num.num array array) ->
 let f = function x -> ( Sci.complex_of_sci x ).(1).(0) in
  Array.map f p ;;




(** {C § } *)
(** 
{1 Opérations arithmétiques sur les polynômes}
{1 Arithmetic operations on polynomials}
*)
(** {C  } *)




(** 
{2 Polynômes à coefficients réels}
{2 Polynomials with real coefficients}
*)
(** {C  } *)




(** {v poly_real_div polynomial1 polynomial2 v} Output: quotient, remainder.

Sortie : quotient, reste. *) 
let poly_real_div = fun (p:float array) (d:float array) ->
 let ddd = poly_real_deg d
 and r = Matrix.vector_float_copy p in
  if ddd < 0. then failwith "Division by zero in Reduc.poly_real_div."
  else
   let dd = int_of_float ddd
   and dr = ref ( poly_real_deg r ) in
    let dominant = d.(dd)
    and q = Array.make ( max 1 ( ( int_of_float !dr ) - dd + 1 ) ) 0. in
     while !dr >= ddd do
      begin
       let place = int_of_float ( !dr ) - dd
       and drdr = int_of_float ( !dr ) in
        let coeff = r.( drdr ) /. dominant in
         q.(place) <- coeff ;
         for i = drdr - 1 downto place do
          r.(i) <- r.(i) -. coeff *. d.( i - place )
         done ;
         r.(drdr) <- 0. ;
         dr := poly_real_deg r ;
      end ;
     done ;
     [| q ; r |] ;;


(** {v poly_real_div_inc order polynomial1 polynomial2 v} Output: quotient, remainder.

Sortie : quotient, reste. *) 
let poly_real_div_inc = fun (order:int) (p:float array) (d:float array) ->
 let ddd = poly_real_deg d
 and oo = float order
 and dp = int_of_float ( poly_real_deg p ) in
  if ( d.(0) = 0. ) then failwith "Bad divisor in Reduc.poly_real_div_inc."
  else
   let dd = int_of_float ddd in
    let r = Array.make ( order + dp + dd + 1 ) 0. in
     for i = 0 to dp do
      r.(i) <- p.(i) ;
     done ;
     let vr = ref ( poly_real_val r )
     and dominant = d.(0) in
      let q = Array.make ( order + 1 ) 0. in
       while !vr <= oo do
        begin
         let vrvr = int_of_float ( !vr ) in
          let place = vrvr + dd
          and coeff = r.(vrvr) /. dominant in
           q.(vrvr) <- coeff ;
          for i = vrvr + 1 to place do
           r.(i) <- r.(i) -. coeff *. d.( i - vrvr )
          done ;
          r.(vrvr) <- 0. ;
          vr := poly_real_val r ;
        end ;
       done ;
       [| q ; r |] ;;


(** {v poly_real_mod polynomial1 polynomial2 v} *) 
let poly_real_mod = fun (p:float array) (q:float array) ->
 poly_real_cleanup ( poly_real_div p q ).(1) ;;


(** {v poly_real_gcd polynomial1 polynomial2 v} *) 
let poly_real_gcd = fun (p:float array) (q:float array) ->
 let r = poly_real_mod p q
 and s = poly_real_mod q p
 and pp = ref p
 and ppp = ref p
 and qq = ref q in
  let rr = poly_real_deg r
  and ss = poly_real_deg s in
   if rr = neg_infinity then poly_real_normalize q
   else
    begin
     if ss = neg_infinity then poly_real_normalize p
     else
      begin
       if rr < ss then ( pp := q ; qq := r )
       else ( qq := s ) ; 
        while poly_real_deg !qq >= 0. do
         ppp := !qq ;
         qq := poly_real_mod !pp !qq ;
         pp := !ppp ;
        done ;
        poly_real_normalize !pp
      end
    end ;;


(** {v poly_real_bezout mult_rule polynomial1 polynomial2 v} The output yields in that order
the gcd [d] and the Bézout coefficients [u] and [v] such that [up+vq=d].

La sortie fournit dans l'ordre le pgcd [d] et les coefficients de Bézout [u] et [v] tels que [up+vq=d]. *) 
let poly_real_bezout = fun mult_rule (p:float array) (q:float array) ->
 let r = poly_real_div p q
 and s = poly_real_div q p
 and pp = ref p
 and qqq = ref q
 and qq = ref q in
  let u = ref [| 1. |]
  and v = ref [| 0. |]
  and uu = ref [| 0. |]
  and vv = ref [| 1. |]
  and uuu = ref [| 1. |]
  and vvv = ref [| 0. |]
  and rr = poly_real_deg r.(1)
  and ss = poly_real_deg s.(1) in
   if rr = neg_infinity then
    begin
     let degre = poly_real_deg q in
      let x = 1. /. q.( int_of_float degre ) in
       qq := Matrix.vector_float_scal_mult x q ;
       uu := [| 1. |] ;
       vv := poly_real_minus [| x |] r.(0) ;
       [| !qq ; !uu ; !vv |]
    end
   else
    begin
     if ss = neg_infinity then
      begin
       let degre = poly_real_deg p in
        let x = 1. /. p.( int_of_float degre ) in
         pp := Matrix.vector_float_scal_mult x p ;
         vv := [| 1. |] ;
         uu := poly_real_minus [| x |] s.(0) ;
         [| !pp ; !uu ; !vv |]
      end
     else
      begin
       while poly_real_deg !qq >= 0. do
        let d = poly_real_div !pp !qq in
         qqq := d.(1) ;
         pp := !qq ;
         qq := !qqq ;
         let dd = d.(0) in
          uuu := poly_real_minus !u ( mult_rule dd !uu ) ;
          u := !uu ;
          uu := !uuu ;
          vvv := poly_real_minus !v ( mult_rule dd !vv ) ;
          v := !vv ;
          vv := !vvv ;
       done ;
       let degre = poly_real_deg !pp in
        let x = 1. /. !pp.( int_of_float degre ) in
         pp := Matrix.vector_float_scal_mult x !pp ;
         u := Matrix.vector_float_scal_mult x !u ;
         v := Matrix.vector_float_scal_mult x !v ;
         [| !pp ; !u ; !v |]
      end
    end ;;


(** {v poly_real_lcm mult_rule polynomial1 polynomial2 v} *) 
let poly_real_lcm = fun mult_rule (p:float array) (q:float array) ->
 let d = poly_real_gcd p q
 and pq = mult_rule p q in
  ( poly_real_div pq d ).(0) ;;


(** {v poly_real_simplify polynomial v} *)
let poly_real_simplify = function (p:float array) ->
 let dp = poly_real_deriv p in
  let g = poly_real_gcd p dp in
   ( poly_real_div p g ).(0) ;;




(** 
{2 Polynômes à coefficients complexes}
{2 Polynomials with complex coefficients}
*)
(** {C  } *)




(** {v poly_complex_div polynomial1 polynomial2 v} Output: quotient, remainder.

Sortie : quotient, reste. *) 
let poly_complex_div = fun (p:float array array array) (d:float array array array) ->
 let ddd = poly_complex_deg d
 and r = poly_complex_copy p in
  if ddd < 0. then failwith "Division by zero in Reduc.poly_complex_div."
  else
   let dd = int_of_float ddd
   and dr = ref ( poly_complex_deg r ) in
    let dominant = complex_inv d.(dd)
    and q = Array.make ( max 1 ( ( int_of_float !dr ) - dd + 1 ) ) complex_0 in
     while !dr >= ddd do
      begin
       let place = int_of_float ( !dr ) - dd
       and drdr = int_of_float ( !dr ) in
        let coeff = Matrix.matrix_float_prod r.( drdr ) dominant in
         q.(place) <- coeff ;
         for i = drdr - 1 downto place do
          r.(i) <- Matrix.matrix_float_minus r.(i) ( Matrix.matrix_float_prod coeff d.( i - place ) )
         done ;
         r.(drdr) <- complex_0 ;
         dr := poly_complex_deg r ;
      end ;
     done ;
     [| q ; r |] ;;


(** {v poly_complex_div_inc order polynomial1 polynomial2 v} Output: quotient, remainder.

Sortie : quotient, reste. *) 
let poly_complex_div_inc = fun (order:int) (p:float array array array) (d:float array array array) ->
 let ddd = poly_complex_deg d
 and oo = float order
 and dp = int_of_float ( poly_complex_deg p ) in
  if ( Matrix.matrix_float_norm_inf d.(0) = 0. ) then failwith "Bad divisor in Reduc.poly_complex_div_inc."
  else
   let dd = int_of_float ddd in
    let r = Array.make ( order + dp + dd + 1 ) complex_0 in
     for i = 0 to dp do
      r.(i) <- p.(i) ;
     done ;
     let vr = ref ( poly_complex_val r )
     and dominant = complex_inv d.(0) in
      let q = Array.make ( order + 1 ) complex_0 in
       while !vr <= oo do
        begin
         let vrvr = int_of_float ( !vr ) in
          let place = vrvr + dd
          and coeff = Matrix.matrix_float_prod r.(vrvr) dominant in
           q.(vrvr) <- coeff ;
          for i = vrvr + 1 to place do
           r.(i) <- Matrix.matrix_float_minus r.(i) ( Matrix.matrix_float_prod coeff d.( i - vrvr ) )
          done ;
          r.(vrvr) <- complex_0 ;
          vr := poly_complex_val r ;
        end ;
       done ;
       [| q ; r |] ;;


(** {v poly_complex_mod polynomial1 polynomial2 v} *) 
let poly_complex_mod = fun (p:float array array array) (q:float array array array) ->
 poly_complex_cleanup ( poly_complex_div p q ).(1) ;;


(** {v poly_complex_gcd polynomial1 polynomial2 v} *) 
let poly_complex_gcd = fun (p:float array array array) (q:float array array array) ->
 let r = poly_complex_mod p q
 and s = poly_complex_mod q p
 and pp = ref p
 and ppp = ref p
 and qq = ref q in
  let rr = poly_complex_deg r
  and ss = poly_complex_deg s in
   if rr = neg_infinity then poly_complex_normalize q
   else
    begin
     if ss = neg_infinity then poly_complex_normalize p
     else
      begin
       if rr < ss then ( pp := q ; qq := r )
       else ( qq := s ) ; 
        while poly_complex_deg !qq >= 0. do
         ppp := !qq ;
         qq := poly_complex_mod !pp !qq ;
         pp := !ppp ;
        done ;
        poly_complex_normalize !pp
      end
    end ;;


(** {v poly_complex_bezout mult_rule polynomial1 polynomial2 v} The output yields in that order
the gcd [d] and the Bézout coefficients [u] and [v] such that [up+vq=d].

La sortie fournit dans l'ordre le pgcd [d] et les coefficients de Bézout [u] et [v] tels que [up+vq=d]. *) 
let poly_complex_bezout = fun mult_rule (p:float array array array) (q:float array array array) ->
 let r = poly_complex_div p q
 and s = poly_complex_div q p
 and pp = ref p
 and qqq = ref q
 and qq = ref q in
  let u = ref [| complex_1 |]
  and v = ref [| complex_0 |]
  and uu = ref [| complex_0 |]
  and vv = ref [| complex_1 |]
  and uuu = ref [| complex_1 |]
  and vvv = ref [| complex_0 |]
  and rr = poly_complex_deg r.(1)
  and ss = poly_complex_deg s.(1) in
   if rr = neg_infinity then
    begin
     let degre = poly_complex_deg q in
      let x = complex_inv q.( int_of_float degre ) in
       qq := Array.map ( Matrix.matrix_float_prod x ) q ;
       uu := [| complex_1 |] ;
       vv := poly_complex_minus [| x |] r.(0) ;
       [| !qq ; !uu ; !vv |]
    end
   else
    begin
     if ss = neg_infinity then
      begin
       let degre = poly_complex_deg p in
        let x = complex_inv p.( int_of_float degre ) in
         pp := Array.map ( Matrix.matrix_float_prod x ) p ;
         vv := [| complex_1 |] ;
         uu := poly_complex_minus [| x |] s.(0) ;
         [| !pp ; !uu ; !vv |]
      end
     else
      begin
       while poly_complex_deg !qq >= 0. do
        let d = poly_complex_div !pp !qq in
         qqq := d.(1) ;
         pp := !qq ;
         qq := !qqq ;
         let dd = d.(0) in
          uuu := poly_complex_minus !u ( mult_rule dd !uu ) ;
          u := !uu ;
          uu := !uuu ;
          vvv := poly_complex_minus !v ( mult_rule dd !vv ) ;
          v := !vv ;
          vv := !vvv ;
       done ;
       let degre = poly_complex_deg !pp in
        let x = complex_inv !pp.( int_of_float degre ) in
         pp := vector_complex_scal_mult x !pp ;
         u := vector_complex_scal_mult x !u ;
         v := vector_complex_scal_mult x !v ;
         [| !pp ; !u ; !v |]
      end
    end ;;


(** {v poly_complex_lcm mult_rule polynomial1 polynomial2 v} *) 
let poly_complex_lcm = fun mult_rule (p:float array array array) (q:float array array array) ->
 let d = poly_complex_gcd p q
 and pq = mult_rule p q in
  ( poly_complex_div pq d ).(0) ;;


(** {v poly_complex_simplify polynomial v} *)
let poly_complex_simplify = function (p:float array array array) ->
 let dp = poly_complex_deriv p in
  let g = poly_complex_gcd p dp in
   ( poly_complex_div p g ).(0) ;;




(** 
{2 Polynômes à coefficients en précision étendue}
{2 Polynomials with coefficients in extended precision}
*)
(** {C  } *)




(** {v poly_sci_div polynomial1 polynomial2 v} Output: quotient, remainder.

Sortie : quotient, reste. *) 
let poly_sci_div = fun (p:Num.num array array) (d:Num.num array array) ->
 let ddd = poly_sci_deg d
 and r = poly_sci_copy p in
  if ddd < 0. then failwith "Division by zero in Reduc.poly_sci_div."
  else
   let dd = int_of_float ddd
   and dr = ref ( poly_sci_deg r ) in
    let dominant = Sci.inv d.(dd)
    and q = Array.make ( max 1 ( ( int_of_float !dr ) - dd + 1 ) ) Sci.sci_0 in
     while !dr >= ddd do
      begin
       let place = int_of_float ( !dr ) - dd
       and drdr = int_of_float ( !dr ) in
        let coeff = Sci.mult r.( drdr ) dominant in
         q.(place) <- coeff ;
         for i = drdr - 1 downto place do
          r.(i) <- Sci.minus r.(i) ( Sci.mult coeff d.( i - place ) )
         done ;
         r.(drdr) <- Sci.sci_0 ;
         dr := poly_sci_deg r ;
      end ;
     done ;
     [| q ; r |] ;;


(** {v poly_sci_div_inc order polynomial1 polynomial2 v} Output: quotient, remainder.

Sortie : quotient, reste. *) 
let poly_sci_div_inc = fun (order:int) (p:Num.num array array) (d:Num.num array array) ->
 let ddd = poly_sci_deg d
 and oo = float order
 and dp = int_of_float ( poly_sci_deg p ) in
  if Sci.eq_0 ( Sci.square_module d.(0) ) then failwith "Bad divisor in Reduc.poly_sci_div_inc."
  else
   let dd = int_of_float ddd in
    let r = Array.make ( order + dp + dd + 1 ) Sci.sci_0 in
     for i = 0 to dp do
      r.(i) <- p.(i) ;
     done ;
     let vr = ref ( poly_sci_val r )
     and dominant = Sci.inv d.(0) in
      let q = Array.make ( order + 1 ) Sci.sci_0 in
       while !vr <= oo do
        begin
         let vrvr = int_of_float ( !vr ) in
          let place = vrvr + dd
          and coeff = Sci.mult r.(vrvr) dominant in
           q.(vrvr) <- coeff ;
          for i = vrvr + 1 to place do
           r.(i) <- Sci.minus r.(i) ( Sci.mult coeff d.( i - vrvr ) )
          done ;
          r.(vrvr) <- Sci.sci_0 ;
          vr := poly_sci_val r ;
        end ;
       done ;
       [| q ; r |] ;;


(** {v poly_sci_mod polynomial1 polynomial2 v} *) 
let poly_sci_mod = fun (p:Num.num array array) (q:Num.num array array) ->
 poly_sci_cleanup ( poly_sci_div p q ).(1) ;;


(** {v poly_sci_gcd polynomial1 polynomial2 v} *) 
let poly_sci_gcd = fun (p:Num.num array array) (q:Num.num array array) ->
 let r = poly_sci_mod p q
 and s = poly_sci_mod q p
 and pp = ref p
 and ppp = ref p
 and qq = ref q in
  let rr = poly_sci_deg r
  and ss = poly_sci_deg s in
   if rr = neg_infinity then poly_sci_normalize q
   else
    begin
     if ss = neg_infinity then poly_sci_normalize p
     else
      begin
       if rr < ss then ( pp := q ; qq := r )
       else ( qq := s ) ; 
        while poly_sci_deg !qq >= 0. do
         ppp := !qq ;
         qq := poly_sci_mod !pp !qq ;
         pp := !ppp ;
        done ;
        poly_sci_normalize !pp
      end
    end ;;


(** {v poly_sci_bezout mult_rule polynomial1 polynomial2 v} The output yields in that order
the gcd [d] and the Bézout coefficients [u] and [v] such that [up+vq=d].

La sortie fournit dans l'ordre le pgcd [d] et les coefficients de Bézout [u] et [v] tels que [up+vq=d]. *) 
let poly_sci_bezout = fun mult_rule (p:Num.num array array) (q:Num.num array array) ->
 let r = poly_sci_div p q
 and s = poly_sci_div q p
 and pp = ref ( poly_sci_copy p )
 and qqq = ref ( poly_sci_copy q )
 and qq = ref ( poly_sci_copy q ) in
  let u = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_1 ) )
  and v = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_0 ) )
  and uu = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_0 ) )
  and vv = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_1 ) )
  and uuu = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_1 ) )
  and vvv = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_0 ) )
  and rr = poly_sci_deg r.(1)
  and ss = poly_sci_deg s.(1) in
   if rr = neg_infinity then
    begin
     let degre = poly_sci_deg q in
      let x = Sci.inv q.( int_of_float degre ) in
       qq := Array.map ( Sci.mult x ) q ;
       uu := Array.make 1 ( Sci.sci_copy Sci.sci_1 ) ;
       vv := poly_sci_minus ( Array.make 1 ( Sci.sci_copy x ) ) r.(0) ;
       [| !qq ; !uu ; !vv |]
    end
   else
    begin
     if ss = neg_infinity then
      begin
       let degre = poly_sci_deg p in
        let x = Sci.inv p.( int_of_float degre ) in
         pp := Array.map ( Sci.mult x ) p ;
         vv := Array.make 1 ( Sci.sci_copy Sci.sci_1 ) ;
         uu := poly_sci_minus ( Array.make 1 ( Sci.sci_copy x ) ) s.(0) ;
         [| !pp ; !uu ; !vv |]
      end
     else
      begin
       while poly_sci_deg !qq >= 0. do
        let d = poly_sci_div !pp !qq in
         qqq := d.(1) ;
         pp := !qq ;
         qq := !qqq ;
         let dd = d.(0) in
          uuu := poly_sci_minus !u ( mult_rule dd !uu ) ;
          u := !uu ;
          uu := !uuu ;
          vvv := poly_sci_minus !v ( mult_rule dd !vv ) ;
          v := !vv ;
          vv := !vvv ;
       done ;
       let degre = poly_sci_deg !pp in
        let x = Sci.inv !pp.( int_of_float degre ) in
         pp := Array.map ( Sci.mult x ) !pp ;
         u := Array.map ( Sci.mult x ) !u ;
         v := Array.map ( Sci.mult x ) !v ;
         [| !pp ; !u ; !v |]
      end
    end ;;


(** {v poly_sci_lcm mult_rule polynomial1 polynomial2 v} *) 
let poly_sci_lcm = fun mult_rule (p:Num.num array array) (q:Num.num array array) ->
 let d = poly_sci_gcd p q
 and pq = mult_rule p q in
  ( poly_sci_div pq d ).(0) ;;


(** {v poly_sci_simplify polynomial v} *)
let poly_sci_simplify = function (p:Num.num array array) ->
 let dp = poly_sci_deriv p in
  let g = poly_sci_gcd p dp in
   ( poly_sci_div p g ).(0) ;;




(** 
{3 Polynômes à coefficients en précision intermédiaire}
{3 Polynomials with coefficients in intermediate precision}
*)
(** {C  } *)




(** {v poly_sci_1024_div polynomial1 polynomial2 v} Output: quotient, remainder.

Sortie : quotient, reste. *) 
let poly_sci_1024_div = fun (p:Num.num array array) (d:Num.num array array) ->
 let ddd = poly_sci_deg d
 and r = poly_sci_copy p in
  if ddd < 0. then failwith "Division by zero in Reduc.poly_sci_1024_div."
  else
   let dd = int_of_float ddd
   and dr = ref ( poly_sci_deg r ) in
    let dominant = Sci.inv_1024 d.(dd)
    and q = Array.make ( max 1 ( ( int_of_float !dr ) - dd + 1 ) ) Sci.sci_0 in
     while !dr >= ddd do
      begin
       let place = int_of_float ( !dr ) - dd
       and drdr = int_of_float ( !dr ) in
        let coeff = Sci.mult_1024 r.( drdr ) dominant in
         q.(place) <- coeff ;
         for i = drdr - 1 downto place do
          r.(i) <- Sci.minus_1024 r.(i) ( Sci.mult_1024 coeff d.( i - place ) )
         done ;
         r.(drdr) <- Sci.sci_0 ;
         dr := poly_sci_deg r ;
      end ;
     done ;
     [| q ; r |] ;;


(** {v poly_sci_1024_div_inc order polynomial1 polynomial2 v} Output: quotient, remainder.

Sortie : quotient, reste. *) 
let poly_sci_1024_div_inc = fun (order:int) (p:Num.num array array) (d:Num.num array array) ->
 let ddd = poly_sci_deg d
 and oo = float order
 and dp = int_of_float ( poly_sci_deg p ) in
  if Sci.eq_0 ( Sci.square_module_1024 d.(0) ) then failwith "Bad divisor in Reduc.poly_sci_1024_div_inc."
  else
   let dd = int_of_float ddd in
    let r = Array.make ( order + dp + dd + 1 ) Sci.sci_0 in
     for i = 0 to dp do
      r.(i) <- p.(i) ;
     done ;
     let vr = ref ( poly_sci_val r )
     and dominant = Sci.inv_1024 d.(0) in
      let q = Array.make ( order + 1 ) Sci.sci_0 in
       while !vr <= oo do
        begin
         let vrvr = int_of_float ( !vr ) in
          let place = vrvr + dd
          and coeff = Sci.mult_1024 r.(vrvr) dominant in
           q.(vrvr) <- coeff ;
          for i = vrvr + 1 to place do
           r.(i) <- Sci.minus_1024 r.(i) ( Sci.mult_1024 coeff d.( i - vrvr ) )
          done ;
          r.(vrvr) <- Sci.sci_0 ;
          vr := poly_sci_val r ;
        end ;
       done ;
       [| q ; r |] ;;


(** {v poly_sci_1024_mod polynomial1 polynomial2 v} *) 
let poly_sci_1024_mod = fun (p:Num.num array array) (q:Num.num array array) ->
 poly_sci_cleanup ( poly_sci_1024_div p q ).(1) ;;


(** {v poly_sci_1024_gcd polynomial1 polynomial2 v} *) 
let poly_sci_1024_gcd = fun (p:Num.num array array) (q:Num.num array array) ->
 let r = poly_sci_1024_mod p q
 and s = poly_sci_1024_mod q p
 and pp = ref p
 and ppp = ref p
 and qq = ref q in
  let rr = poly_sci_deg r
  and ss = poly_sci_deg s in
   if rr = neg_infinity then poly_sci_1024_normalize q
   else
    begin
     if ss = neg_infinity then poly_sci_1024_normalize p
     else
      begin
       if rr < ss then ( pp := q ; qq := r )
       else ( qq := s ) ; 
        while poly_sci_deg !qq >= 0. do
         ppp := !qq ;
         qq := poly_sci_1024_mod !pp !qq ;
         pp := !ppp ;
        done ;
        poly_sci_1024_normalize !pp
      end
    end ;;


(** {v poly_sci_1024_bezout mult_rule polynomial1 polynomial2 v} The output yields in that order
the gcd [d] and the Bézout coefficients [u] and [v] such that [up+vq=d].

La sortie fournit dans l'ordre le pgcd [d] et les coefficients de Bézout [u] et [v] tels que [up+vq=d]. *) 
let poly_sci_1024_bezout = fun mult_rule (p:Num.num array array) (q:Num.num array array) ->
 let r = poly_sci_1024_div p q
 and s = poly_sci_1024_div q p
 and pp = ref ( poly_sci_copy p )
 and qqq = ref ( poly_sci_copy q )
 and qq = ref ( poly_sci_copy q ) in
  let u = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_1 ) )
  and v = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_0 ) )
  and uu = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_0 ) )
  and vv = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_1 ) )
  and uuu = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_1 ) )
  and vvv = ref ( Array.make 1 ( Sci.sci_copy Sci.sci_0 ) )
  and rr = poly_sci_deg r.(1)
  and ss = poly_sci_deg s.(1) in
   if rr = neg_infinity then
    begin
     let degre = poly_sci_deg q in
      let x = Sci.inv_1024 q.( int_of_float degre ) in
       qq := Array.map ( Sci.mult_1024 x ) q ;
       uu := Array.make 1 ( Sci.sci_copy Sci.sci_1 ) ;
       vv := poly_sci_1024_minus ( Array.make 1 ( Sci.sci_copy x ) ) r.(0) ;
       [| !qq ; !uu ; !vv |]
    end
   else
    begin
     if ss = neg_infinity then
      begin
       let degre = poly_sci_deg p in
        let x = Sci.inv_1024 p.( int_of_float degre ) in
         pp := Array.map ( Sci.mult_1024 x ) p ;
         vv := Array.make 1 ( Sci.sci_copy Sci.sci_1 ) ;
         uu := poly_sci_1024_minus ( Array.make 1 ( Sci.sci_copy x ) ) s.(0) ;
         [| !pp ; !uu ; !vv |]
      end
     else
      begin
       while poly_sci_deg !qq >= 0. do
        let d = poly_sci_1024_div !pp !qq in
         qqq := d.(1) ;
         pp := !qq ;
         qq := !qqq ;
         let dd = d.(0) in
          uuu := poly_sci_1024_minus !u ( mult_rule dd !uu ) ;
          u := !uu ;
          uu := !uuu ;
          vvv := poly_sci_1024_minus !v ( mult_rule dd !vv ) ;
          v := !vv ;
          vv := !vvv ;
       done ;
       let degre = poly_sci_deg !pp in
        let x = Sci.inv_1024 !pp.( int_of_float degre ) in
         pp := Array.map ( Sci.mult_1024 x ) !pp ;
         u := Array.map ( Sci.mult_1024 x ) !u ;
         v := Array.map ( Sci.mult_1024 x ) !v ;
         [| !pp ; !u ; !v |]
      end
    end ;;


(** {v poly_sci_1024_lcm mult_rule polynomial1 polynomial2 v} *) 
let poly_sci_1024_lcm = fun mult_rule (p:Num.num array array) (q:Num.num array array) ->
 let d = poly_sci_1024_gcd p q
 and pq = mult_rule p q in
  ( poly_sci_1024_div pq d ).(0) ;;


(** {v poly_sci_1024_simplify polynomial v} *)
let poly_sci_1024_simplify = function (p:Num.num array array) ->
 let dp = poly_sci_1024_deriv p in
  let g = poly_sci_1024_gcd p dp in
   ( poly_sci_1024_div p g ).(0) ;;




(** {C § } *)
(** 
{1 Opérations entre les polynômes et les matrices}
{1 Operations between polynomials and matrices}
*)
(** {C  } *)




(** 
{2 Coefficients réels}
{2 Real coefficients}
*)
(** {C  } *)




(** {v poly_real_apply_matrix polynomial matrix v} *)
let poly_real_apply_matrix = fun (p:float array) (m:float array array) ->
 let r = Array.length m
 and d = int_of_float ( poly_real_deg p ) in
  let mm = ref ( Matrix.scal_float r r p.(d) ) in
   for i = d - 1 downto 0 do
    mm := Matrix.matrix_float_prod m !mm ;
    mm := Matrix.matrix_float_plus !mm ( Matrix.scal_float r r p.(i) ) ;
   done ;
   !mm ;;

(** {v poly_real_apply_matrix_rec polynomial matrix v} *)
let rec poly_real_apply_matrix_rec = fun (p:float array) (m:float array array) ->
 let d = int_of_float ( poly_real_deg p ) in
  if d <= 3 then poly_real_apply_matrix p m
  else
   begin
    let dd = d / 2 in
     let tail = Array.sub p 0 dd
     and head = Array.sub p dd ( d - dd + 1 )
     and mm = Matrix.float_power dd m in
      let mmm = poly_real_apply_matrix_rec head m in
       Matrix.matrix_float_plus ( poly_real_apply_matrix_rec tail m ) ( Matrix.matrix_float_prod mm mmm )
   end ;;

(** {v real_companion polynomial v} *)
let real_companion = function (p:float array) ->
 let d = int_of_float ( poly_real_deg p ) in
  let q = Array.sub p 0 d
  and coeff = (-1.) /. p.(d) in
   for i = 0 to d - 1 do
    q.(i) <- q.(i) *. coeff
   done ;
   let m = Matrix.identity_float_bis ( d - 1 ) d in
    Array.append m [| q |] ;;

(** {v leverrier_real_char_poly matrix v} *)
let leverrier_real_char_poly = function (m:float array array) ->
 let l = Array.length m in
  let p = Array.make ( l + 1 ) 1.
  and t = ref ( -. Matrix.float_trace m ) in
   let u = ref ( Matrix.matrix_float_plus m ( Matrix.scal_float l l !t ) ) in
    p.( l - 1 ) <- !t ;
    for i = 2 to l do
     let uu = Matrix.matrix_float_prod m !u in
      t := (Matrix.float_trace uu ) /. ( float ( - i ) ) ;
      u := Matrix.matrix_float_plus uu ( Matrix.scal_float l l !t ) ;
      p.( l - i ) <- !t ;
    done ;
    p ;;


(** {v real_poly_det char_poly matrix v} The characteristic polynomial must be provided, 
as for example [leverrier_real_char_poly m].

Le polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m]. *)
let real_poly_det = fun (p:float array) (m:float array array) ->
 let x = p.(0) in
  if ( Array.length m ) mod 2 = 0 then x else -. x ;;


(** {v real_resultant_bis characteristic_polynomial_methode polynomial1 polynomial2 v} *)
let real_resultant_bis = fun char_poly_methode (p:float array) (q:float array) ->
 let m = real_sylvester_matrix p q in
  let pp = char_poly_methode m in
   real_poly_det pp m ;;


(** {v real_discriminant_bis characteristic_polynomial_methode polynomial v} *)
let real_discriminant_bis = fun char_poly_methode (p:float array) ->
 real_resultant_bis char_poly_methode p ( poly_real_deriv p ) ;;


(** {v matrix_real_inv char_poly matrix v} The characteristic polynomial must be provided, 
as for example [leverrier_real_char_poly m].

Le polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m]. *)
let matrix_real_inv = fun (p:float array) (m:float array array) ->
 let x = (-1.) /. p.(0)
 and q = Array.sub p 1 ( Array.length m ) in
  let mm = poly_real_apply_matrix q m in
   Matrix.matrix_float_scal_mult x mm ;;




(** 
{2 Coefficients entiers}
{2 Integer coefficients}
*)
(** {C  } *)




(** {v poly_int_apply_matrix polynomial matrix v} *)
let poly_int_apply_matrix = fun (p:int array) (m:int array array) ->
 let r = Array.length m
 and d = int_of_float ( poly_int_deg p ) in
  let mm = ref ( Matrix.scal_int r r p.(d) ) in
   for i = d - 1 downto 0 do
    mm := Matrix.matrix_int_prod m !mm ;
    mm := Matrix.matrix_int_plus !mm ( Matrix.scal_int r r p.(i) ) ;
   done ;
   !mm ;;

(** {v poly_int_apply_matrix_rec polynomial matrix v} *)
let rec poly_int_apply_matrix_rec = fun (p:int array) (m:int array array) ->
 let d = int_of_float ( poly_int_deg p ) in
  if d <= 3 then poly_int_apply_matrix p m
  else
   begin
    let dd = d / 2 in
     let tail = Array.sub p 0 dd
     and head = Array.sub p dd ( d - dd + 1 )
     and mm = Matrix.int_power dd m in
      let mmm = poly_int_apply_matrix_rec head m in
       Matrix.matrix_int_plus ( poly_int_apply_matrix_rec tail m ) ( Matrix.matrix_int_prod mm mmm )
   end ;;

(** {v int_companion polynomial v} *)
let int_companion = function (p:int array) ->
 let d = int_of_float ( poly_int_deg p ) in
  let q = Array.sub p 0 d
  and coeff = (-1) / p.(d) in
   for i = 0 to d - 1 do
    q.(i) <- q.(i) * coeff
   done ;
   let m = Matrix.identity_int_bis ( d - 1 ) d in
    Array.append m [| q |] ;;


(** {v leverrier_int_char_poly matrix v} This algorithm may work
despite the use of an euclidean division.

Cet algorithme peut fonctionner malgré l'utilisation d'une division euclidienne.*)
let leverrier_int_char_poly = function (m:int array array) ->
 let l = Array.length m in
  let p = Array.make ( l + 1 ) 1
  and t = ref ( - Matrix.int_trace m ) in
   let u = ref ( Matrix.matrix_int_plus m ( Matrix.scal_int l l !t ) ) in
    p.( l - 1 ) <- !t ;
    for i = 2 to l do
     let uu = Matrix.matrix_int_prod m !u in
      t := (Matrix.int_trace uu ) / ( - i ) ;
      u := Matrix.matrix_int_plus uu ( Matrix.scal_int l l !t ) ;
      p.( l - i ) <- !t ;
    done ;
    p ;;


(** {v int_poly_det char_poly matrix v} The characteristic polynomial must be provided, 
as for example [leverrier_real_char_poly m].

Le polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m]. *)
let int_poly_det = fun (p:int array) (m:int array array) ->
 let x = p.(0) in
  if ( Array.length m ) mod 2 = 0 then x else - x ;;


(** {v int_resultant_bis characteristic_polynomial_methode polynomial1 polynomial2 v} *)
let int_resultant_bis = fun char_poly_methode (p:int array) (q:int array) ->
 let m = int_sylvester_matrix p q in
  let pp = char_poly_methode m in
   int_poly_det pp m ;;


(** {v int_discriminant_bis characteristic_polynomial_methode polynomial v} *)
let int_discriminant_bis = fun char_poly_methode (p:int array) ->
 int_resultant_bis char_poly_methode p ( poly_int_deriv p ) ;;


(** {v matrix_int_inv char_poly matrix v} The characteristic polynomial must be provided, 
as for example [leverrier_real_char_poly m].

Le polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m]. *)
let matrix_int_inv = fun (p:int array) (m:int array array) ->
 let x = (-1) / p.(0)
 and q = Array.sub p 1 ( Array.length m ) in
  let mm = poly_int_apply_matrix q m in
   Matrix.matrix_int_scal_mult x mm ;;




(** 
{2 Coefficients complexes}
{2 Complex coefficients}
*)
(** {C  } *)




(** {v poly_complex_apply_matrix polynomial matrix v} *)
let poly_complex_apply_matrix = fun (p:float array array array) (m:float array array) ->
 let r = ( Array.length m ) / 2
 and d = int_of_float ( poly_complex_deg p ) in
  let mm = ref ( scal_complex r r p.(d) ) in
   for i = d - 1 downto 0 do
    mm := Matrix.matrix_float_prod m !mm ;
    mm := Matrix.matrix_float_plus !mm ( scal_complex r r p.(i) ) ;
   done ;
   !mm ;;

(** {v poly_complex_apply_matrix_rec polynomial matrix v} *)
let rec poly_complex_apply_matrix_rec = fun (p:float array array array) (m:float array array) ->
 let d = int_of_float ( poly_complex_deg p ) in
  if d <= 3 then poly_complex_apply_matrix p m
  else
   begin
    let dd = d / 2 in
     let tail = Array.sub p 0 dd
     and head = Array.sub p dd ( d - dd + 1 )
     and mm = Matrix.float_power dd m in
      let mmm = poly_complex_apply_matrix_rec head m in
       Matrix.matrix_float_plus ( poly_complex_apply_matrix_rec tail m ) ( Matrix.matrix_float_prod mm mmm )
   end ;;

(** {v complex_companion polynomial v} *)
let complex_companion = function (p:float array array array) ->
 let d = int_of_float ( poly_complex_deg p ) in
  let q = Array.sub p 0 d
  and coeff = complex_inv p.(d) in
   for i = 0 to d - 1 do
    q.(i) <- Matrix.matrix_float_prod q.(i) coeff
   done ;
   let qq = Array.append q [| complex_1 |] in
    let x = real_companion ( poly_complex_real_part qq )
    and qqq = poly_complex_imag_part q in
     let y = Array.append ( Matrix.null_float ( d - 1 ) d ) [| qqq |] in
(** Attention : les coefficients du polynôme changent de signe.
Be careful: the signs of the coefficients of the polynomial change. *)
      let m = [| [| Matrix.Float_matrix_cons x ; Matrix.Float_matrix_cons y |] ;
               [| Matrix.Float_matrix_cons ( Matrix.matrix_float_opp y ) ; Matrix.Float_matrix_cons x |] |] in
      Matrix.matrix_float_demakeup ( Matrix.matrix_foa_crash ( Matrix.Foa_matrix_cons m ) ) ;;

(** {v leverrier_complex_char_poly matrix v} *)
let leverrier_complex_char_poly = function (m:float array array) ->
 let l = ( Array.length m ) / 2 in
  let p = Array.make ( l + 1 ) complex_1
  and t = ref ( Matrix.matrix_float_opp ( complex_trace m ) ) in
   let u = ref ( Matrix.matrix_float_plus m ( scal_complex l l !t ) ) in
    p.( l - 1 ) <- !t ;
    for i = 2 to l do
     let uu = Matrix.matrix_float_prod m !u in
      t := complex_div ( complex_trace uu ) ( float_to_complex ( float ( - i ) ) ) ;
      u := Matrix.matrix_float_plus uu ( scal_complex l l !t ) ;
      p.( l - i ) <- !t ;
    done ;
    p ;;


(** {v complex_poly_det char_poly matrix v} The characteristic polynomial must be provided, 
as for example [leverrier_real_char_poly m].

Le polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m]. *)
let complex_poly_det = fun (p:float array array array) (m:float array array) ->
 let x = p.(0) in
  if ( ( Array.length m ) / 2 ) mod 2 = 0 then x else Matrix.matrix_float_opp x ;;


(** {v complex_resultant_bis characteristic_polynomial_methode polynomial1 polynomial2 v} *)
let complex_resultant_bis = fun char_poly_methode (p:float array array array) (q:float array array array) ->
 let m = complex_sylvester_matrix p q in
  let pp = char_poly_methode m in
   complex_poly_det pp m ;;


(** {v complex_discriminant_bis characteristic_polynomial_methode polynomial v} *)
let complex_discriminant_bis = fun char_poly_methode (p:float array array array) ->
 complex_resultant_bis char_poly_methode p ( poly_complex_deriv p ) ;;


(** {v matrix_complex_inv char_poly matrix v} The the characteristic polynomial must be provided, 
as for example [leverrier_real_char_poly m].

Le polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m]. *)
let matrix_complex_inv = fun (p:float array array array) (m:float array array) ->
 let x = Matrix.matrix_float_opp ( complex_inv p.(0) )
 and q = Array.sub p 1 ( ( Array.length m ) / 2 ) in
  let mm = poly_complex_apply_matrix q m in
   matrix_complex_scal_mult x mm ;;




(** 
{2 Coefficients entiers de Gauss}
{2 Gauss integer coefficients}
*)
(** {C  } *)




(** {v poly_gauss_apply_matrix polynomial matrix v} *)
let poly_gauss_apply_matrix = fun (p:int array array array) (m:int array array) ->
 let r = Array.length m
 and d = int_of_float ( poly_gauss_deg p ) in
  let mm = ref ( scal_gauss r r p.(d) ) in
   for i = d - 1 downto 0 do
    mm := Matrix.matrix_int_prod m !mm ;
    mm := Matrix.matrix_int_plus !mm ( scal_gauss r r p.(i) ) ;
   done ;
   !mm ;;

(** {v poly_gauss_apply_matrix_rec polynomial matrix v} *)
let rec poly_gauss_apply_matrix_rec = fun (p:int array array array) (m:int array array) ->
 let d = int_of_float ( poly_gauss_deg p ) in
  if d <= 3 then poly_gauss_apply_matrix p m
  else
   begin
    let dd = d / 2 in
     let tail = Array.sub p 0 dd
     and head = Array.sub p dd ( d - dd + 1 )
     and mm = Matrix.int_power dd m in
      let mmm = poly_gauss_apply_matrix_rec head m in
       Matrix.matrix_int_plus ( poly_gauss_apply_matrix_rec tail m ) ( Matrix.matrix_int_prod mm mmm )
   end ;;

(** {v gauss_companion polynomial v} *)
let gauss_companion = function (p:int array array array) ->
 let d = int_of_float ( poly_gauss_deg p ) in
  let q = Array.sub p 0 d
  and coeff = gauss_inv p.(d) in
   for i = 0 to d - 1 do
    q.(i) <- Matrix.matrix_int_prod q.(i) coeff
   done ;
   let qq = Array.append q [| gauss_1 |] in
    let x = int_companion ( poly_gauss_real_part qq )
    and qqq = poly_gauss_imag_part q in
     let y = Array.append ( Matrix.null_int ( d - 1 ) d ) [| qqq |] in
(** Attention : les coefficients du polynôme changent de signe.
Be careful: the signs of the coefficients of the polynomial change. *)
    let m = [| [| Matrix.Int_matrix_cons x ; Matrix.Int_matrix_cons y |] ;
               [| Matrix.Int_matrix_cons ( Matrix.matrix_int_opp y ) ; Matrix.Int_matrix_cons x |] |] in
     Matrix.matrix_int_demakeup ( Matrix.matrix_ioa_crash ( Matrix.Ioa_matrix_cons m ) ) ;;

(** {v leverrier_gauss_char_poly matrix v} *)
let leverrier_gauss_char_poly = function (m:int array array) ->
 let l = ( Array.length m ) / 2 in
  let p = Array.make ( l + 1 ) gauss_1
  and t = ref ( Matrix.matrix_int_opp ( gauss_trace m ) ) in
   let u = ref ( Matrix.matrix_int_plus m ( scal_gauss l l !t ) ) in
    p.( l - 1 ) <- !t ;
    for i = 2 to l do
     let uu = Matrix.matrix_int_prod m !u in
      t := Matrix.matrix_int_scal_left_div ( - i ) ( gauss_trace uu ) ;
      u := Matrix.matrix_int_plus uu ( scal_gauss l l !t ) ;
      p.( l - i ) <- !t ;
    done ;
    p ;;


(** {v gauss_poly_det char_poly matrix v} The characteristic polynomial must be provided, 
as for example [leverrier_real_char_poly m].

Le polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m]. *)
let gauss_poly_det = fun (p:int array array array) (m:int array array) ->
 let x = p.(0) in
  if ( ( Array.length m ) / 2 ) mod 2 = 0 then x else Matrix.matrix_int_opp x ;;


(** {v gauss_resultant_bis characteristic_polynomial_methode polynomial1 polynomial2 v} *)
let gauss_resultant_bis = fun char_poly_methode (p:int array array array) (q:int array array array) ->
 let m = gauss_sylvester_matrix p q in
  let pp = char_poly_methode m in
   gauss_poly_det pp m ;;


(** {v gauss_discriminant_bis characteristic_polynomial_methode polynomial v} *)
let gauss_discriminant_bis = fun char_poly_methode (p:int array array array) ->
 gauss_resultant_bis char_poly_methode p ( poly_gauss_deriv p ) ;;


(** {v matrix_gauss_inv char_poly matrix v} The characteristic polynomial must be provided, 
as for example [leverrier_real_char_poly m].

Le polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m]. *)
let matrix_gauss_inv = fun (p:int array array array) (m:int array array) ->
 let x = Matrix.matrix_int_opp p.(0)
 and q = Array.sub p 1 ( ( Array.length m ) / 2 ) in
  let mm = poly_gauss_apply_matrix q m in
   matrix_gauss_scal_left_div x mm ;;




(** 
{2 Décomposition de Jordan}
{2 Jordan decomposition}
*)
(** {C  } *)




(** {v sci_jordan_decomposition_polynomial mult_rule comp_rule polynomial v} *)
let sci_jordan_decomposition_polynomial = fun mult_rule comp_rule (p:Num.num array array) ->
 let l = ( Array.length p ) - 1
 and s0 = poly_sci_x
 and i = ref 0 in
  let s = ref s0
  and s_old = ref s0 in
   let pp = poly_sci_simplify p in
    let deg_pp = poly_sci_deg pp
    and dpp = poly_sci_deriv pp in
     let max_iter = ( int_of_float ( ( log ( ( float l ) /. deg_pp ) ) /. ( log 2. ) ) ) + 1 in
      while !i <= max_iter do
       let p_p = comp_rule dpp !s in
        let z = poly_sci_bezout mult_rule p_p p in
         let u = z.(1) in
          let y = mult_rule u ( comp_rule pp !s ) in
           let x = poly_sci_cleanup ( poly_sci_minus !s ( poly_sci_mod y p ) ) in
            let w = poly_sci_mod x p in
             s_old := !s ;
             s := w ;
             if poly_sci_eq !s !s_old then i := max_int
             else ( i := !i + 1 ; s_old := !s )
      done ;
      !s ;;


(** {v sci_1024_jordan_decomposition_polynomial mult_rule comp_rule polynomial v} *)
let sci_1024_jordan_decomposition_polynomial = fun mult_rule comp_rule (p:Num.num array array) ->
 let l = ( Array.length p ) - 1
 and s0 = poly_sci_x
 and i = ref 0 in
  let s = ref s0
  and s_old = ref s0 in
   let pp = poly_sci_1024_simplify p in
    let deg_pp = poly_sci_deg pp
    and dpp = poly_sci_1024_deriv pp in
     let max_iter = ( int_of_float ( ( log ( ( float l ) /. deg_pp ) ) /. ( log 2. ) ) ) + 1 in
      while !i <= max_iter do
       let p_p = comp_rule dpp !s in
        let z = poly_sci_1024_bezout mult_rule p_p p in
         let u = z.(1) in
          let y = mult_rule u ( comp_rule pp !s ) in
           let x = poly_sci_cleanup ( poly_sci_1024_minus !s ( poly_sci_1024_mod y p ) ) in
            let w = poly_sci_1024_mod x p in
             s_old := !s ;
             s := w ;
             if poly_sci_1024_eq !s !s_old then i := max_int
             else ( i := !i + 1 ; s_old := !s )
      done ;
      !s ;;


(** {v lento_complex_jordan_decomposition_polynomial sci_mult_rule sci_comp_rule polynomial v} *)
let lento_complex_jordan_decomposition_polynomial = fun sci_mult_rule sci_comp_rule (p:float array array array) ->
 poly_sci_to_complex ( sci_jordan_decomposition_polynomial sci_mult_rule sci_comp_rule ( poly_complex_to_sci p ) ) ;;


(** {v lento_real_jordan_decomposition_polynomial sci_mult_rule sci_comp_rule polynomial v} *)
let lento_real_jordan_decomposition_polynomial = fun sci_mult_rule sci_comp_rule (p:float array) ->
 poly_sci_real_part ( sci_jordan_decomposition_polynomial sci_mult_rule sci_comp_rule ( poly_real_to_sci p ) ) ;;


(** {v largo_complex_jordan_decomposition_polynomial sci_mult_rule sci_comp_rule polynomial v} *)
let largo_complex_jordan_decomposition_polynomial = fun sci_1024_mult_rule sci_1024_comp_rule (p:float array array array) ->
 poly_sci_to_complex ( sci_1024_jordan_decomposition_polynomial sci_1024_mult_rule sci_1024_comp_rule ( poly_complex_to_sci p ) ) ;;


(** {v largo_real_jordan_decomposition_polynomial sci_mult_rule sci_comp_rule polynomial v} *)
let largo_real_jordan_decomposition_polynomial = fun sci_1024_mult_rule sci_1024_comp_rule (p:float array) ->
 poly_sci_real_part ( sci_1024_jordan_decomposition_polynomial sci_1024_mult_rule sci_1024_comp_rule ( poly_real_to_sci p ) ) ;;


(** {v real_jordan_decomposition_polynomial mult_rule comp_rule polynomial v} *)
let real_jordan_decomposition_polynomial = fun mult_rule comp_rule (p:float array) ->
 let l = ( Array.length p ) - 1
 and seuil = 128. *. epsilon_float
 and s0 = poly_real_x
 and i = ref 0 in
  let s = ref s0
  and s_old = ref s0 in
   let pp = poly_real_simplify p in
    let deg_pp = poly_real_deg pp
    and dpp = poly_real_deriv pp in
     let max_iter = ( int_of_float ( ( log ( ( float l ) /. deg_pp ) ) /. ( log 2. ) ) ) + 1 in
      while !i <= max_iter do
       let p_p = comp_rule dpp !s in
        let z = poly_real_bezout mult_rule p_p p in
         let u = z.(1) in
          let y = mult_rule u ( comp_rule pp !s ) in
           let x = poly_real_cleanup ( poly_real_minus !s ( poly_real_mod y p ) ) in
            let w = poly_real_mod x p in
             s_old := !s ;
             s := w ;
             if Matrix.vector_float_norm_inf ( poly_real_minus !s !s_old ) <= seuil *. ( Matrix.vector_float_norm_1 !s_old ) then i := max_int
             else ( i := !i + 1 ; s_old := !s )
      done ;
      !s ;;


(** {v complex_jordan_decomposition_polynomial mult_rule comp_rule polynomial v} The computing time seems to be
in [O(n^4)] where [n] is the order of the matrix when there is no iteration.
When the order is too big, the calculus is erroneous.
The characteristic polynomial must be provided, as for example [leverrier_real_char_poly m].

La méthode de calcul du polynôme caractéristique doit être précisé, comme par exemple [leverrier_real_char_poly m].
Il semble que le temps de calcul soit en [O(n^4)] où [n] est l'ordre de la matrice
quand il n' y a pas d'itération. Quand l'ordre est trop grand, le calcul est erroné. *)
let complex_jordan_decomposition_polynomial = fun mult_rule comp_rule (p:float array array array) ->
 let l = ( Array.length p ) - 1
 and seuil = 128. *. epsilon_float
 and s0 = poly_complex_x
 and i = ref 0 in
  let s = ref s0
  and s_old = ref s0 in
   let pp = poly_complex_simplify p in
    let deg_pp = poly_complex_deg pp
    and dpp = poly_complex_deriv pp in
     let max_iter = ( int_of_float ( ( log ( ( float l ) /. deg_pp ) ) /. ( log 2. ) ) ) + 1 in
      while !i <= max_iter do
       let p_p = comp_rule dpp !s in
        let z = poly_complex_bezout mult_rule p_p p in
         let u = z.(1) in
          let y = mult_rule u ( comp_rule pp !s ) in
           let x = poly_complex_cleanup ( poly_complex_minus !s ( poly_complex_mod y p ) ) in
            let w = poly_complex_mod x p in
             s_old := !s ;
             s := w ;
             if vector_complex_norm_inf_bis ( poly_complex_minus !s !s_old ) <= seuil *. ( vector_complex_norm_1_bis !s_old ) then i := max_int
             else ( i := !i + 1 ; s_old := !s )
      done ;
      !s ;;


(** {v jordan_decomposition apply_rule polynomial matrix v} The (real or complex) decomposition polynomial of the matrix must be provided, 
using for example the function [lento_complex_jordan_decomposition_polynomial].
The rule of application of a polynomial to a matrix must be provided too.

Le polynôme de décomposition (réel ou complexe) de la matrice doit être précisé, 
en utilisant par exemple la fonction [lento_complex_jordan_decomposition_polynomial].
La loi d'application d'un polynôme à une matrice doit aussi être précisée. *)
let jordan_decomposition = fun apply_rule p (m:float array array) ->
 let d = apply_rule p m in
  let n = Matrix.matrix_float_minus m d in
   [| d ; n |] ;;




(** {C § } *)
(** 
{1 Autres constructions}
{1 Further constructions}
*)
(** {C  } *)




(** 
{2 Coefficients du binôme et accélérateurs de convergence}
{2 Binoùmila coefficients and convergence accelerators}
*)
(** {C  } *)




(** {v newton_float_binom_coeff mult_rule integer v} *)
let newton_float_binom_coeff = fun mult_rule (n:int) ->
 poly_real_pow mult_rule n [| 1. ; 1. |] ;;

(** {v alternate_newton_float_binom_coeff mult_rule integer v} *)
let alternate_newton_float_binom_coeff = fun mult_rule (n:int) ->
 poly_real_pow mult_rule n [| 1. ; -1. |] ;;

(** {v newton_int_binom_coeff mult_rule integer v} *)
let newton_int_binom_coeff = fun mult_rule (n:int) ->
 poly_int_pow mult_rule n [| 1 ; 1 |] ;;

(** {v alternate_newton_int_binom_coeff mult_rule integer v} *)
let alternate_newton_int_binom_coeff = fun mult_rule (n:int) ->
 poly_int_pow mult_rule n [| 1 ; -1 |] ;;

(** {v newton_complex_binom_coeff mult_rule integer v} *)
let newton_complex_binom_coeff = fun mult_rule (n:int) ->
 poly_complex_pow mult_rule n [| complex_1 ; complex_1 |] ;;

(** {v alternate_newton_complex_binom_coeff mult_rule integer v} *)
let alternate_newton_complex_binom_coeff = fun mult_rule (n:int) ->
 poly_complex_pow mult_rule n [| complex_1 ; complex_minus_1 |] ;;

(** {v newton_gauss_binom_coeff mult_rule integer v} *)
let newton_gauss_binom_coeff = fun mult_rule (n:int) ->
 poly_gauss_pow mult_rule n [| gauss_1 ; gauss_1 |] ;;

(** {v alternate_newton_gauss_binom_coeff mult_rule integer v} *)
let alternate_newton_gauss_binom_coeff = fun mult_rule (n:int) ->
 poly_gauss_pow mult_rule n [| gauss_1 ; gauss_minus_1 |] ;;

(** {v newton_sci_binom_coeff mult_rule integer v} *)
let newton_sci_binom_coeff = fun mult_rule (n:int) ->
 poly_sci_pow mult_rule n [| Sci.sci_1 ; Sci.sci_1 |] ;;

(** {v alternate_newton_sci_binom_coeff mult_rule integer v} *)
let alternate_newton_sci_binom_coeff = fun mult_rule (n:int) ->
 poly_sci_pow mult_rule n [| Sci.sci_1 ; Sci.sci_minus_1 |] ;;


(** {v float_euler_transform mult_rule index sequence v} *)
let float_euler_transform = fun mult_rule (n:int) (seq:float array) ->
 if n = 0 then seq.(0) *. 0.5
 else
  begin
   let v = newton_float_binom_coeff mult_rule n
   and s = Array.sub seq 0 ( succ n ) in
    ( (2.) ** ( float ( - ( succ n ) ) ) ) *. ( Matrix.vector_float_scal_prod v s )
  end ;;

(** {v float_differences sequence v} *)
let float_differences = function (seq:float array) ->
 let length = pred ( Array.length seq ) in
  let s = Array.mapi ( fun i x -> x -. seq.(i) ) ( Array.sub seq 1 length ) in
   Array.append [| seq.(0) |] s ;;

(** {v float_euler_sum_series mult_rule sequence v} *)
let float_euler_sum_series = fun mult_rule (seq:float array) ->
 let accu = ref 0. in
  for i = 0 to ( Array.length seq ) - 1 do
   accu := !accu +. ( float_euler_transform mult_rule i seq ) ;
  done ;
  !accu ;;

(** {v float_euler_sum_sequence mult_rule sequence v} *)
let float_euler_sum_sequence = fun mult_rule (seq:float array) ->
 let s = float_differences seq in
  float_euler_sum_series mult_rule s ;;

(** {v complex_euler_transform mult_rule index sequence v} *)
let complex_euler_transform = fun mult_rule (n:int) (seq:float array array array) ->
 if n = 0 then Matrix.matrix_float_scal_mult 0.5 seq.(0)
 else
  begin
   let v = complex_poly_to_complex_double_vector ( newton_complex_binom_coeff mult_rule n )
   and s = complex_poly_to_complex_double_vector ( Array.sub seq 0 ( succ n ) ) in
    Matrix.matrix_float_scal_mult ( (2.) ** ( float ( - ( succ n ) ) ) ) ( vector_complex_hermitian_prod v s )
  end ;;

(** {v complex_differences sequence v} *)
let complex_differences = function (seq:float array array array) ->
 let length = pred ( Array.length seq ) in
  let s = Array.mapi ( fun i x -> Matrix.matrix_float_minus x seq.(i) ) ( Array.sub seq 1 length ) in
   Array.append [| seq.(0) |] s ;;

(** {v complex_euler_sum_series mult_rule sequence v} *)
let complex_euler_sum_series = fun mult_rule (seq:float array array array) ->
 let accu = ref complex_0 in
  for i = 0 to ( Array.length seq ) - 1 do
   accu := Matrix.matrix_float_plus !accu ( complex_euler_transform mult_rule i seq ) ;
  done ;
  !accu ;;

(** {v complex_euler_sum_sequence mult_rule sequence v} *)
let complex_euler_sum_sequence = fun mult_rule (seq:float array array array) ->
 let s = complex_differences seq in
  complex_euler_sum_series mult_rule s ;;




(** 
{2 Générateurs aléatoires}
{2 Random generators}
*)
(** {C  } *)




(** {v matrix_complex_random rows columns range v} *)
let matrix_complex_random = fun (r:int) (c:int) (w:float) ->
 let x = Matrix.matrix_float_bal_random r c w
 and y = Matrix.matrix_float_bal_random r c w in
  matrix_complexify x y ;;

(** {v complex_random range v} *)
let complex_random = function(x:float) ->
 matrix_complex_random 1 1 x ;;

(** {v poly_complex_random degree range v} *)
let poly_complex_random = fun (d:int) (x:float) ->
 let v = Matrix.vector_float_bal_random ( 2 * ( d + 1 ) ) x in
  complex_vector_to_complex_poly v ;;

(** {v poly_complex_unitary_random degree range v} *)
let poly_complex_unitary_random = fun (d:int) (x:float) ->
 let p = poly_complex_random ( pred d ) x in
  Array.append p [| complex_1 |] ;;

(** {v matrix_complex_herm_random order range v} *)
let matrix_complex_herm_random = fun (r:int) (x:float) ->
 let s = Matrix.sym_float_bal_random r x
 and a = Matrix.antisym_float_bal_random r x in
  matrix_complexify s a ;;

(** {v matrix_complex_anti_herm_random order range v} *)
let matrix_complex_anti_herm_random = fun (r:int) (x:float) ->
 let s = Matrix.sym_float_bal_random r x
 and a = Matrix.antisym_float_bal_random r x in
  matrix_complexify a s ;;

(** {v matrix_unitary_random order range v} *)
let matrix_unitary_random = fun (r:int) (x:float) ->
 let a = matrix_complex_anti_herm_random r x in
  Matrix.generic_ortho_float_antisym a ;;


(** {v matrix_gauss_random rows columns range v} *)
let matrix_gauss_random = fun (r:int) (c:int) (w:int) ->
 let x = Matrix.matrix_int_bal_random r c w
 and y = Matrix.matrix_int_bal_random r c w in
  matrix_gauss_complexify x y ;;

(** {v gauss_random range v} *)
let gauss_random = function(x:int) ->
 matrix_gauss_random 1 1 x ;;

(** {v poly_gauss_random degree range v} *)
let poly_gauss_random = fun (d:int) (x:int) ->
 let v = Matrix.vector_int_bal_random ( 2 * ( d + 1 ) ) x in
  gauss_vector_to_gauss_poly v ;;

(** {v poly_gauss_unitary_random degree range v} *)
let poly_gauss_unitary_random = fun (d:int) (x:int) ->
 let p = poly_gauss_random ( pred d ) x in
  Array.append p [| gauss_1 |] ;;

(** {v matrix_gauss_herm_random order range v} *)
let matrix_gauss_herm_random = fun (r:int) (x:int) ->
 let s = Matrix.sym_int_bal_random r x
 and a = Matrix.antisym_int_bal_random r x in
  matrix_gauss_complexify s a ;;

(** {v matrix_gauss_anti_herm_random order range v} *)
let matrix_gauss_anti_herm_random = fun (r:int) (x:int) ->
 let s = Matrix.sym_int_bal_random r x
 and a = Matrix.antisym_int_bal_random r x in
  matrix_gauss_complexify a s ;;




(** {C § } *)
(** 
{1 Réduction des matrices et racines des polynômes}
{1 Reduction of matrices and roots of polynomials}
*)
(** {C  } *)




(** {v complex_diago_spectrum diagonalization_methode matrix v} *)
let complex_diago_spectrum = fun (diagonalization_methode:float array array -> float array array array array) (m:float array array) ->
 let w = diagonalization_methode m in
  w.(0) ;;


(** {v complex_householder_step threshold index matrix v} Output: transformation matrix, coefficient.

Sortie : matrice de transformation, coefficient. *)
let complex_householder_step = fun (threshold:float) (i:int) (m:float array array) ->
 let x = matrix_complex_extract_column_to_matrix_trans i m
 and coeff = matrix_complex_extract_coefficient i i m
 and r = Array.length m
 and c = Array.length m.(0) in
  let rr = r / 2
  and xx = Matrix.matrix_float_copy x
  and argument = apply_built_in_complex_float_to_matrix Complex.arg coeff in
   let first_row = xx.(0)
   and second_row = xx.(1) in
    for j = 0 to i - 1 do
     let jj = rr + j in
      first_row.(j) <- 0. ;
      first_row.(jj) <- 0. ;
      second_row.(j) <- 0. ;
      second_row.(jj) <- 0. ;
    done ;
    let norm = Matrix.vector_float_norm_2 xx.(0) in
     if norm <= threshold then [| Matrix.identity_float r c ; complex_0 |]
     else
      begin
       let u = Matrix.matrix_float_copy xx
       and ii = i + rr in
        let alpha = Matrix.matrix_float_scal_mult ( -. norm ) ( polar_to_matrix 1. argument )
        and fr = u.(0)
        and sr = u.(1) in
         let a00 = 0.5 *. ( alpha.(0).(0) +. alpha.(1).(1) )
         and b01 = 0.5 *. ( alpha.(0).(1) -. alpha.(1).(0) )
         and c10 = 0.5 *. ( alpha.(1).(0) -. alpha.(0).(1) )
         and d11 = 0.5 *. ( alpha.(1).(1) +. alpha.(0).(0) ) in
          fr.(i) <- fr.(i) +. a00 ;
          fr.(ii) <- fr.(ii) +. b01 ;
          sr.(i) <- sr.(i) +. c10 ;
          sr.(ii) <- sr.(ii) +. d11 ;
          let length = Matrix.vector_float_norm_2 fr in
           let v = Matrix.matrix_float_scal_left_div length u in
            let w1 = vector_complex_hermitian_prod v x
            and w2 = vector_complex_hermitian_prod x v
            and vv = Matrix.float_transpose v in
             let w = ( if complex_module w2 <> 0. then Matrix.matrix_float_prod w1 ( complex_inv w2 ) else complex_0 ) in
              w.(0).(0) <- w.(0).(0) +. 1. ;
              w.(1).(1) <- w.(1).(1) +. 1. ;
              let ww = matrix_complex_scal_mult w ( Matrix.matrix_float_twisted_prod vv vv ) in
               let q = Matrix.matrix_float_minus ( Matrix.identity_float r c ) ww in
                [| q ; alpha |] ;
      end ;;


(** {v complex_qr_decomposition threshold matrix v} Output: unitary transformation matrix [q] and complex upper triangular matrix [r] such that [m = qr], transposed transformation matrix.

Sortie : matrice de transformation unitaire [q], matrice triangulaire supérieure complexe [r] telles que [m = qr], transposée de la matrice de transformation. *)
let complex_qr_decomposition = fun (threshold:float) (m:float array array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let n = ( min r c ) / 2
  and qq = ref ( Matrix.identity_float r c )
  and rr = ref ( Matrix.matrix_float_copy m ) in
   for i = 0 to n - 2 do
    let result = complex_householder_step threshold i !rr in
     rr := Matrix.matrix_float_prod result.(0) !rr ;
     qq := Matrix.matrix_float_prod result.(0) !qq ;
   done ;
   [| Matrix.float_transpose !qq ; !rr ; !qq |] ;;


(** {v complex_francis_iteration threshold_qr threshold max_steps threshold matrix v} Output: candidate for the upper trigonality, 
unitary transformation matrix [q], transposed transformation matrix, measure of the under diagonal part.

Sortie : candidat pour la trigonalité supérieure, matrice de transformation unitaire [q], transposée de la matrice de transformation, mesure de la partie sous-diagonale. *)
let complex_francis_iteration = fun (threshold_qr:float) (threshold:float) (steps:int) (m:float array array) ->
 let r = Array.length m
 and c = Array.length m.(0)
 and res = complex_qr_decomposition threshold_qr m in
  let n = ( min r c ) / 2 - 1
  and q = ref res.(0)
  and qq = ref res.(2)
  and i = ref 0
  and error = ref threshold in
   let candidate = ref ( Matrix.matrix_float_prod res.(1) !q ) in
    while ( !i <= steps ) && ( !error >= threshold ) do
     let result = complex_qr_decomposition threshold_qr !candidate in
      candidate := Matrix.matrix_float_prod result.(1) result.(0) ;
      qq := Matrix.matrix_float_prod result.(2) !qq ;
      q := Matrix.matrix_float_prod !q result.(0) ;
      i := !i + 1 ;
      error := 0. ;
      for j = 1 to n do
       let row = matrix_complex_extract_row_to_poly j !candidate in
        error := !error +. ( vector_complex_norm_2 ( Array.sub row 0 j ) ) ;
      done ;
    done ;
    [| !candidate ; !q ; !qq ; [|[| !error |]|] |] ;;


(** {v complex_francis_shifted_step threshold shift matrix v} Output: candidate for the tridiagonality, 
left unitary transformation matrix, transpose of the transformation matrix. *)
let complex_francis_shifted_step = fun (threshold:float) (shift:float array array) (m:float array array) ->
 let r = Array.length m in
  let nn = r / 2 in
   let sc = scal_complex nn nn shift in
    let mm = Matrix.matrix_float_minus m sc in
     let result = complex_qr_decomposition threshold mm in
      let product = Matrix.matrix_float_prod result.(1) result.(0) in
       [| Matrix.matrix_float_plus sc product ; result.(0) ; result.(2) |] ;;


(** {v complex_francis_shifted_iteration threshold_qr threshold max_steps shift matrix v} Output: candidate for the upper trigonality, 
unitary transformation matrix [q], transposed transformation matrix, measure of the under diagonal part.

Sortie : candidat pour la trigonalité supérieure, matrice de transformation unitaire [q], transposée de la matrice de transformation, mesure de la partie sous-diagonale. *)
let complex_francis_shifted_iteration = fun (threshold_qr:float) (threshold:float) (steps:int) (shift:float array array) (m:float array array) ->
 let res = ref ( complex_francis_shifted_step threshold_qr shift m )
 and r = Array.length m
 and error = ref threshold in
  let n = r / 2 - 1
  and candidate = ref !res.(0)
  and q = ref !res.(1)
  and qq = ref !res.(2)
  and i = ref 0 in
   while ( !i <= steps ) && ( !error >= threshold ) do
    let result = complex_francis_shifted_step threshold_qr shift !candidate in
     candidate := result.(0) ;
     qq := Matrix.matrix_float_prod result.(2) !qq ;
     q := Matrix.matrix_float_prod !q result.(1) ;
     i := !i + 1 ;
     error := 0. ;
     for j = 1 to n do
      let row = matrix_complex_extract_row_to_poly j !candidate in
      error := !error +. ( vector_complex_norm_2 ( Array.sub row 0 j ) ) ;
     done ;
    done ;
    [| !candidate ; !q ; !qq ; [|[| !error |]|] |] ;;


(** {v complex_francis_schur_decomposition threshold_qr threshold max_steps matrix v} Output: candidate for the upper trigonality, 
unitary transformation matrix [q], transposed transformation matrix, measure of the under diagonal part.

Sortie : candidat pour la trigonalité supérieure, matrice de transformation unitaire [q], transposée de la matrice de transformation, mesure de la partie sous-diagonale. *)
let complex_francis_schur_decomposition = fun (threshold_qr:float) (threshold:float) (steps:int) (m:float array array) ->
 let old_res = ref ( complex_francis_iteration threshold_qr threshold 0 m )
 and i = ref 2 in
  let res = ref ( complex_francis_iteration threshold_qr threshold 0 !old_res.(0) )
  and old_q = ref !old_res.(1)
  and old_qq = ref !old_res.(2) in
   let q = ref ( Matrix.matrix_float_prod !old_q !res.(1) )
   and qq = ref ( Matrix.matrix_float_prod !res.(2) !old_qq ) in
    while ( !res.(3).(0).(0) < !old_res.(3).(0).(0) ) && ( !i <= steps ) do
     old_res := !res ;
     res := complex_francis_iteration threshold_qr threshold 0 !old_res.(0) ;
     old_q := !q ;
     old_qq := !qq ;
     q := Matrix.matrix_float_prod !old_q !res.(1) ;
     qq := Matrix.matrix_float_prod !res.(2) !old_qq ;
     i := !i + 1 ;
    done ;
    if !res.(3).(0).(0) < !old_res.(3).(0).(0) then
     [| !res.(0) ; !q ; !qq ; !res.(3) |]
    else [| !old_res.(0) ; !old_q ; !old_qq ; !old_res.(3) |] ;;


(** {v complex_shifted_francis_schur_decomposition threshold_qr threshold max_steps shift matrix v} Output: candidate for the upper trigonality, 
unitary transformation matrix [q], transposed transformation matrix, measure of the under diagonal part.

Sortie : candidat pour la trigonalité supérieure, matrice de transformation unitaire [q], transposée de la matrice de transformation, mesure de la partie sous-diagonale. *)
let complex_shifted_francis_schur_decomposition = fun (threshold_qr:float) (threshold:float) (steps:int) (shift:float array array) (m:float array array) ->
 let old_res = ref ( complex_francis_shifted_iteration threshold_qr threshold 0 shift m )
 and i = ref 2 in
  let res = ref ( complex_francis_shifted_iteration threshold_qr threshold 0 shift !old_res.(0) )
  and old_q = ref !old_res.(1)
  and old_qq = ref !old_res.(2) in
   let q = ref ( Matrix.matrix_float_prod !old_q !res.(1) )
   and qq = ref ( Matrix.matrix_float_prod !res.(2) !old_qq ) in
    while ( !res.(3).(0).(0) < !old_res.(3).(0).(0) ) && ( !i <= steps ) do
     old_res := !res ;
     res := complex_francis_shifted_iteration threshold_qr threshold 0 shift !old_res.(0) ;
     old_q := !q ;
     old_qq := !qq ;
     q := Matrix.matrix_float_prod !old_q !res.(1) ;
     qq := Matrix.matrix_float_prod !res.(2) !old_qq ;
     i := !i + 1 ;
    done ;
    if !res.(3).(0).(0) < !old_res.(3).(0).(0) then
     [| !res.(0) ; !q ; !qq ; !res.(3) |]
    else [| !old_res.(0) ; !old_q ; !old_qq ; !old_res.(3) |] ;;


(** {v complex_francis_spectrum threshold_qr threshold max_steps matrix v} *)
let complex_francis_spectrum = fun (threshold_qr:float) (threshold:float) (steps:int) (m:float array array) ->
 let r = ( Array.length m ) / 2 in
  let u = matrix_unitary_random r 1. in
   let v = Matrix.matrix_float_twisted_prod u m in
    let w = Matrix.matrix_float_twisted_prod u v in
     let old_res = ref ( complex_francis_iteration threshold_qr threshold 1 w )
     and i = ref 2 in
      let res = ref ( complex_francis_iteration threshold_qr threshold 1 !old_res.(0) ) in
       while ( !res.(3).(0).(0) < !old_res.(3).(0).(0) ) && ( !i < steps ) do
        old_res := !res ;
        res := complex_francis_iteration threshold_qr threshold 1 !old_res.(0) ;
        i := !i + 1 ;
       done ;
       if !res.(3).(0).(0) < !old_res.(3).(0).(0) then
        matrix_complex_extract_diag_to_poly !res.(0)
       else matrix_complex_extract_diag_to_poly !old_res.(0) ;;


(** {v complex_shifted_francis_spectrum threshold_qr threshold max_steps shift matrix v} *)
let complex_shifted_francis_spectrum = fun (threshold_qr:float) (threshold:float) (steps:int) (shift:float array array) (m:float array array) ->
 let r = ( Array.length m ) / 2 in
  let u = matrix_unitary_random r 1. in
   let v = Matrix.matrix_float_twisted_prod u m in
    let w = Matrix.matrix_float_twisted_prod u v in
     let old_res = ref ( complex_francis_shifted_iteration threshold_qr threshold 1 shift w )
     and i = ref 2 in
      let res = ref ( complex_francis_shifted_iteration threshold_qr threshold 1 shift !old_res.(0) ) in
       while ( !res.(3).(0).(0) < !old_res.(3).(0).(0) ) && ( !i < steps ) do
        old_res := !res ;
        res := complex_francis_shifted_iteration threshold_qr threshold 1 shift !old_res.(0) ;
        i := !i + 1 ;
       done ;
       if !res.(3).(0).(0) < !old_res.(3).(0).(0) then
        matrix_complex_extract_diag_to_poly !res.(0)
       else matrix_complex_extract_diag_to_poly !old_res.(0) ;;


(** {v complex_francis_spectrum_seq threshold_qr threshold max_steps matrix v} *)
let complex_francis_spectrum_seq = fun (threshold_qr:float) (threshold:float) (steps:int) (m:float array array) ->
 let r = ( Array.length m ) / 2 in
  let u = matrix_unitary_random r 1. in
   let v = Matrix.matrix_float_twisted_prod u m in
    let w = Matrix.matrix_float_twisted_prod u v in
     let old_res = ref ( complex_francis_iteration threshold_qr threshold 1 w )
     and i = ref 1 in
      let res = ref ( complex_francis_iteration threshold_qr threshold 1 !old_res.(0) ) in
       let seq = ref [| complex_poly_to_complex_vector ( matrix_complex_extract_diag_to_poly !old_res.(0) ) ;
        complex_poly_to_complex_vector ( matrix_complex_extract_diag_to_poly !res.(0) ) |] in
        while ( !res.(3).(0).(0) < !old_res.(3).(0).(0) ) && ( !i < pred steps ) do
         old_res := !res ;
         res := complex_francis_iteration threshold_qr threshold 1 !old_res.(0) ;
         seq := Array.append !seq [| complex_poly_to_complex_vector ( matrix_complex_extract_diag_to_poly !res.(0) ) |] ;
         i := !i + 1 ;
        done ;
        if !res.(3).(0).(0) >= !old_res.(3).(0).(0) then
         seq := Array.sub !seq 0 ( pred ( Array.length !seq ) ) ;
        !seq ;;


(** {v complex_shifted_francis_spectrum_seq threshold_qr threshold max_steps shift matrix v} *)
let complex_shifted_francis_spectrum_seq = fun (threshold_qr:float) (threshold:float) (steps:int) (shift:float array array) (m:float array array) ->
 let r = ( Array.length m ) / 2 in
  let u = matrix_unitary_random r 1. in
   let v = Matrix.matrix_float_twisted_prod u m in
    let w = Matrix.matrix_float_twisted_prod u v in
     let old_res = ref ( complex_francis_shifted_iteration threshold_qr threshold 1 shift w )
     and i = ref 1 in
      let res = ref ( complex_francis_shifted_iteration threshold_qr threshold 1 shift !old_res.(0) ) in
       let seq = ref [| complex_poly_to_complex_vector ( matrix_complex_extract_diag_to_poly !old_res.(0) ) ;
        complex_poly_to_complex_vector ( matrix_complex_extract_diag_to_poly !res.(0) ) |] in
        while ( !res.(3).(0).(0) < !old_res.(3).(0).(0) ) && ( !i < pred steps ) do
         old_res := !res ;
         res := complex_francis_shifted_iteration threshold_qr threshold 1 shift !old_res.(0) ;
         seq := Array.append !seq [| complex_poly_to_complex_vector ( matrix_complex_extract_diag_to_poly !res.(0) ) |] ;
         i := !i + 1 ;
        done ;
        if !res.(3).(0).(0) >= !old_res.(3).(0).(0) then
         seq := Array.sub !seq 0 ( pred ( Array.length !seq ) ) ;
        !seq ;;


(** {v complex_compensated_francis_spectrum accelerator threshold_qr threshold max_steps matrix v} A convergence accelerator for real vector sequences
must be provided, like for instance [Matrix.vector_float_approx_bis].

Il faut fournir un accélérateur de convergence de suites de vecteurs réels, comme par exemple [Matrix.vector_float_approx_bis]. *)
let complex_compensated_francis_spectrum = fun accel (threshold_qr:float) (threshold:float) (steps:int) (m:float array array) ->
 let s = complex_francis_spectrum_seq threshold_qr threshold steps m in
  let limit = accel s in
   complex_vector_to_complex_poly limit ;;


(** {v complex_compensated_shifted_francis_spectrum accelerator threshold_qr threshold max_steps shift matrix v} A convergence accelerator for real vector sequences
must be provided, like for instance [Matrix.vector_float_approx_bis].

Il faut fournir un accélérateur de convergence de suites de vecteurs réels, comme par exemple [Matrix.vector_float_approx_bis]. *)
let complex_compensated_shifted_francis_spectrum = fun accel (threshold_qr:float) (threshold:float) (steps:int) (shift:float array array) (m:float array array) ->
 let s = complex_shifted_francis_spectrum_seq threshold_qr threshold steps shift m in
  let limit = accel s in
   complex_vector_to_complex_poly limit ;;


(** {v direct_complex_diagonalization methode_ker threshold_qr threshold steps_qr steps_power matrix v} The matrix is supposed to be diagonalizable.
The first number of steps [steps_qr] is the one used in
the QR algorithm of Francis ; the second [steps] is the one used in the inverse iteration.
The method [methode_ker] may be the one used to search for a kernel with the singular value decomposition.
In the case when the matrix is not simple, the passage matrix has few precision.
Output: spectrum, matrix whose columns are the respective eigenvectors, matrix whose rows are the respective eigenvectors.

Sortie : spectre, matrice dont les colonnes sont les vecteurs propres correspondants, matrice dont les lignes sont les vecteurs propores correspondants.
La matrice est supposée diagonalisable.
Le premier nombre maximal de pas [steps_qr] est celui utilisé pour l'algorithme QR de Francis ;
le deuxième [steps] est celui utilisé dans l'itération inverse.
La méthode [methode_ker] peut être celle utilisée pour rechercher un noyau avec la décomposition en valeurs singulières.
Dans le cas où la matrice n'est pas simple, la matrice de passage est peu précise. *)
let direct_complex_diagonalization = fun methode_ker (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (m:float array array) ->
 let res = complex_francis_schur_decomposition threshold_qr threshold steps_qr m
 and seuil = sqrt epsilon_float
 and nn = Array.length m
 and accu_integer = ref 0
 and accu_complex = ref complex_0 in
  let p = Array.map clean_complex ( matrix_complex_extract_diag_to_poly res.(0) )
  and q = Matrix.float_transpose res.(1)
  and n = nn / 2 in
   let spectrum = Array.make n complex_0
   and n_n = n - 1
   and trans_passage = Array.make_matrix nn nn 0. in
    for i = 0 to n_n do
     let candidate_value = p.(i) in
      let difference = Matrix.matrix_float_minus m ( scal_complex n n candidate_value ) in
       let test = Matrix.float_invertibility difference in
        if test then
         begin
          let mm = Matrix.clean_inv difference in
           let w = Matrix.float_normalized_iterate Matrix.vector_float_norm_2 steps mm q.(i) in
            trans_passage.(i) <- Matrix.vector_float_copy w ;
            trans_passage.( n + i ) <- vector_complex_i_times w ;
            let ww = Matrix.matrix_vector_float_prod mm w
            and z = complex_vector_to_complex_poly w in
             let zz = complex_vector_to_complex_poly ww in
              for j = 0 to n_n do
               let divisor = z.(j) in
                if complex_module divisor > seuil then
                 begin
                  accu_complex := Matrix.matrix_float_plus !accu_complex ( Matrix.matrix_float_prod zz.(j) ( complex_inv divisor ) ) ;
                  accu_integer := succ !accu_integer ;
                 end ;
              done ;
              let coeff = clean_complex ( Matrix.matrix_float_scal_left_div ( float !accu_integer ) !accu_complex ) in
               accu_complex := complex_0 ;
               accu_integer := 0 ;
               spectrum.(i) <- Matrix.matrix_float_plus candidate_value ( complex_inv coeff ) ;
         end
        else
         begin
          spectrum.(i) <- candidate_value ;
          let k = methode_ker difference in
           let kk = Array.length k in
            let v = Matrix.vector_float_bal_random kk 10. in
             let w_w = ( Matrix.matrix_vector_float_prod ( Matrix.float_transpose k ) ( Matrix.vector_float_scal_mult ( 1. /. ( Matrix.vector_float_norm_2 v ) ) v ) ) in
              let w_coeff = 1. /. ( Matrix.vector_float_norm_2 w_w ) in
               let w = Matrix.vector_float_scal_mult w_coeff w_w in
                trans_passage.(i) <- Matrix.vector_float_copy w ;
                trans_passage.( n + i ) <- vector_complex_i_times w ;
         end ;
    done ;
    [| spectrum ; [| Matrix.float_transpose trans_passage |] ; [| trans_passage  |] |] ;; 


(** {v direct_complex_shifted_diagonalization methode_ker threshold_qr threshold steps_qr steps_power shift matrix v} The matrix is supposed to be diagonalizable.
The first number of steps [steps_qr] is the one used in
the QR algorithm of Francis ; the second [steps] is the one used in the inverse iteration.
The method [methode_ker] may be the one used to search for a kernel with the singular value decomposition.
In the case when the matrix is not simple, the passage matrix has few precision.
Output: spectrum, matrix whose columns are the respective eigenvectors, matrix whose rows are the respective eigenvectors.

Sortie : spectre, matrice dont les colonnes sont les vecteurs propres correspondants, matrice dont les lignes sont les vecteurs propores correspondants.
La matrice est supposée diagonalisable.
Le premier nombre maximal de pas [steps_qr] est celui utilisé pour l'algorithme QR de Francis ;
le deuxième [steps] est celui utilisé dans l'itération inverse.
La méthode [methode_ker] peut être celle utilisée pour rechercher un noyau avec la décomposition en valeurs singulières.
Dans le cas où la matrice n'est pas simple, la matrice de passage est peu précise. *)
let direct_complex_shifted_diagonalization = fun methode_ker (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (shift:float array array) (m:float array array) ->
 let res = complex_shifted_francis_schur_decomposition threshold_qr threshold steps_qr shift m
 and seuil = sqrt epsilon_float
 and nn = Array.length m
 and accu_integer = ref 0
 and accu_complex = ref complex_0 in
  let p = Array.map clean_complex ( matrix_complex_extract_diag_to_poly res.(0) )
  and q = Matrix.float_transpose res.(1)
  and n = nn / 2 in
   let spectrum = Array.make n complex_0
   and n_n = n - 1
   and trans_passage = Array.make_matrix nn nn 0. in
    for i = 0 to n_n do
     let candidate_value = p.(i) in
      let difference = Matrix.matrix_float_minus m ( scal_complex n n candidate_value ) in
       let test = Matrix.float_invertibility difference in
        if test then
         begin
          let mm = Matrix.clean_inv difference in
           let w = Matrix.float_normalized_iterate Matrix.vector_float_norm_2 steps mm q.(i) in
            trans_passage.(i) <- Matrix.vector_float_copy w ;
            trans_passage.( n + i ) <- vector_complex_i_times w ;
            let ww = Matrix.matrix_vector_float_prod mm w
            and z = complex_vector_to_complex_poly w in
             let zz = complex_vector_to_complex_poly ww in
              for j = 0 to n_n do
               let divisor = z.(j) in
                if complex_module divisor > seuil then
                 begin
                  accu_complex := Matrix.matrix_float_plus !accu_complex ( Matrix.matrix_float_prod zz.(j) ( complex_inv divisor ) ) ;
                  accu_integer := succ !accu_integer ;
                 end ;
              done ;
              let coeff = clean_complex ( Matrix.matrix_float_scal_left_div ( float !accu_integer ) !accu_complex ) in
               accu_complex := complex_0 ;
               accu_integer := 0 ;
               spectrum.(i) <- Matrix.matrix_float_plus candidate_value ( complex_inv coeff ) ;
         end
        else
         begin
          spectrum.(i) <- candidate_value ;
          let k = methode_ker difference in
           let kk = Array.length k in
            let v = Matrix.vector_float_bal_random kk 10. in
             let w_w = ( Matrix.matrix_vector_float_prod ( Matrix.float_transpose k ) ( Matrix.vector_float_scal_mult ( 1. /. ( Matrix.vector_float_norm_2 v ) ) v ) ) in
              let w_coeff = 1. /. ( Matrix.vector_float_norm_2 w_w ) in
               let w = Matrix.vector_float_scal_mult w_coeff w_w in
                trans_passage.(i) <- Matrix.vector_float_copy w ;
                trans_passage.( n + i ) <- vector_complex_i_times w ;
         end ;
    done ;
    [| spectrum ; [| Matrix.float_transpose trans_passage |] ; [| trans_passage  |] |] ;; 


(** {v direct_complex_spectrum threshold_qr threshold steps_qr steps matrix v} The first number of steps [steps_qr] 
is the one used in the QR algorithm of Francis ; the second number [steps] is the one used in the inverse iteration
The matrix is supposed to be diagonalizable.

La matrice est supposée diagonalisable.
Le premier nombre maximal de pas [steps_qr] est celui utilisé pour l'algorithme QR de Francis ;
le deuxième [steps] est celui utilisé dans l'itération inverse.
 *)
let direct_complex_spectrum = fun (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (m:float array array) ->
 let res = complex_francis_schur_decomposition threshold_qr threshold steps_qr m
 and seuil = sqrt epsilon_float
 and nn = Array.length m
 and accu_integer = ref 0
 and accu_complex = ref complex_0 in
  let p = Array.map clean_complex ( matrix_complex_extract_diag_to_poly res.(0) )
  and q = Matrix.float_transpose res.(1)
  and n = nn / 2 in
   let spectrum = Array.make n complex_0
   and n_n = n - 1 in
    for i = 0 to n_n do
     let candidate_value = p.(i) in
      let difference = Matrix.matrix_float_minus m ( scal_complex n n candidate_value ) in
       let test = Matrix.float_invertibility difference in
        if test then
         begin
          let mm = Matrix.clean_inv difference in
           let w = Matrix.float_normalized_iterate Matrix.vector_float_norm_2 steps mm q.(i) in
            let ww = Matrix.matrix_vector_float_prod mm w
            and z = complex_vector_to_complex_poly w in
             let zz = complex_vector_to_complex_poly ww in
              for j = 0 to n_n do
               let divisor = z.(j) in
                if complex_module divisor > seuil then
                 begin
                  accu_complex := Matrix.matrix_float_plus !accu_complex ( Matrix.matrix_float_prod zz.(j) ( complex_inv divisor ) ) ;
                  accu_integer := succ !accu_integer ;
                 end ;
              done ;
              let coeff = clean_complex ( Matrix.matrix_float_scal_left_div ( float !accu_integer ) !accu_complex ) in
               accu_complex := complex_0 ;
               accu_integer := 0 ;
               spectrum.(i) <- Matrix.matrix_float_plus candidate_value ( complex_inv coeff ) ;
         end
        else
          spectrum.(i) <- candidate_value ;
    done ;
    spectrum ;;


(** {v direct_complex_shifted_spectrum threshold_qr threshold steps_qr steps shift matrix v} The first number of steps [steps_qr] 
is the one used in the QR algorithm of Francis ; the second number [steps] is the one used in the inverse iteration
The matrix is supposed to be diagonalizable.

La matrice est supposée diagonalisable.
Le premier nombre maximal de pas [steps_qr] est celui utilisé pour l'algorithme QR de Francis ;
le deuxième [steps] est celui utilisé dans l'itération inverse.
 *)
let direct_complex_shifted_spectrum = fun (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (shift:float array array) (m:float array array) ->
 let res = complex_shifted_francis_schur_decomposition threshold_qr threshold steps_qr shift m
 and seuil = sqrt epsilon_float
 and nn = Array.length m
 and accu_integer = ref 0
 and accu_complex = ref complex_0 in
  let p = Array.map clean_complex ( matrix_complex_extract_diag_to_poly res.(0) )
  and q = Matrix.float_transpose res.(1)
  and n = nn / 2 in
   let spectrum = Array.make n complex_0
   and n_n = n - 1 in
    for i = 0 to n_n do
     let candidate_value = p.(i) in
      let difference = Matrix.matrix_float_minus m ( scal_complex n n candidate_value ) in
       let test = Matrix.float_invertibility difference in
        if test then
         begin
          let mm = Matrix.clean_inv difference in
           let w = Matrix.float_normalized_iterate Matrix.vector_float_norm_2 steps mm q.(i) in
            let ww = Matrix.matrix_vector_float_prod mm w
            and z = complex_vector_to_complex_poly w in
             let zz = complex_vector_to_complex_poly ww in
              for j = 0 to n_n do
               let divisor = z.(j) in
                if complex_module divisor > seuil then
                 begin
                  accu_complex := Matrix.matrix_float_plus !accu_complex ( Matrix.matrix_float_prod zz.(j) ( complex_inv divisor ) ) ;
                  accu_integer := succ !accu_integer ;
                 end ;
              done ;
              let coeff = clean_complex ( Matrix.matrix_float_scal_left_div ( float !accu_integer ) !accu_complex ) in
               accu_complex := complex_0 ;
               accu_integer := 0 ;
               spectrum.(i) <- Matrix.matrix_float_plus candidate_value ( complex_inv coeff ) ;
         end
        else
          spectrum.(i) <- candidate_value ;
    done ;
    spectrum ;;


(** {v complex_tune_diagonalization factor methode_diag matrix passage_candidate v}
This function may be unstable.
The matrix is supposed to be diagonalizable.
The output is the same as for [direct_complex_diagonalization].

La sortie est la même que pour [direct_complex_diagonalization].
La matrice est supposée diagonalisable.
Cette fonction peut être instable. *)
let complex_tune_diagonalization = fun (factor:float) methode_diag (m:float array array) (p:float array array) ->
 let n = ( Array.length p ) / 2
 and p_r = matrix_complex_real_part p
 and p_i = matrix_complex_imag_part p in
  let f = function y -> Matrix.matrix_float_bal_random n n y
  and x = factor *. ( Matrix.matrix_float_norm_inf p ) /. ( float n ) in
   let a = f x
   and b = f x in
    let pa = Matrix.matrix_float_plus p_r a
    and pb = Matrix.matrix_float_plus p_i b in
     let aa = matrix_real_to_complex pa
     and bb = matrix_imag_to_complex pb in
      let pp = Matrix.matrix_float_plus aa bb in
       let q = Matrix.clean_inv pp in
        let r = Matrix.matrix_float_triple_prod q m pp in
         methode_diag r ;;


(** {v indirect_complex_diagonalization factor methode_ker threshold_qr threshold steps_qr steps_power matrix v}
The results are often worse than with the function [direct_complex_diagonalization]
but may be enhanced sometimes by playing with the parameters.
This function may be unstable.
The matrix is supposed to be diagonalizable.
The output is the same as for [direct_complex_diagonalization].

La sortie est la même que pour [direct_complex_diagonalization].
La matrice est supposée diagonalisable.
Les résultats sont parfois moins bons qu'avec la fonction [direct_complex_diagonalization]
mais peuvent être améliorés parfois en jouant sur les paramètres.
Cette fonction peut être instable. *)
let indirect_complex_diagonalization = fun (factor:float) methode_ker (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (m:float array array) ->
 let res = direct_complex_diagonalization methode_ker threshold_qr threshold steps_qr steps m in
  let p = res.(1).(0) and tp = res.(2).(0) in
   let result = complex_tune_diagonalization factor ( direct_complex_diagonalization methode_ker threshold_qr threshold steps_qr steps ) m p in
    let new_p = Matrix.matrix_float_prod p result.(1).(0)
    and new_tp = Matrix.matrix_float_prod result.(2).(0) tp in
     [| result.(0) ; [| new_p |] ; [| new_tp |] |] ;;


(** {v indirect_complex_shifted_diagonalization factor methode_ker threshold_qr threshold steps_qr steps_power shift matrix v}
The results are often worse than with the function [direct_complex_diagonalization]
but may be enhanced sometimes by playing with the parameters.
This function may be unstable.
The matrix is supposed to be diagonalizable.
The output is the same as for [direct_complex_diagonalization].

La sortie est la même que pour [direct_complex_diagonalization].
La matrice est supposée diagonalisable.
Les résultats sont parfois moins bons qu'avec la fonction [direct_complex_diagonalization]
mais peuvent être améliorés parfois en jouant sur les paramètres.
Cette fonction peut être instable. *)
let indirect_complex_shifted_diagonalization = fun (factor:float) methode_ker (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (shift:float array array) (m:float array array) ->
 let res = direct_complex_shifted_diagonalization methode_ker threshold_qr threshold steps_qr steps shift m in
  let p = res.(1).(0) and tp = res.(2).(0) in
   let result = complex_tune_diagonalization factor ( direct_complex_shifted_diagonalization methode_ker threshold_qr threshold steps_qr steps shift ) m p in
    let new_p = Matrix.matrix_float_prod p result.(1).(0)
    and new_tp = Matrix.matrix_float_prod result.(2).(0) tp in
     [| result.(0) ; [| new_p |] ; [| new_tp |] |] ;;


(** {v direct_complex_compensated_spectrum accelerator stages factor methode_ker threshold_qr threshold steps_qr steps matrix v} 
The [accelerator] is appied to real vectors. The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux vecteurs réels. Le facteur [factor] doit être choisi entre 0 et 1. *)
let direct_complex_compensated_spectrum = fun accelerator (stages:int) (factor:float) methode_ker (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (m:float array array) ->
 let f = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) )
 and s = Array.make_matrix stages 1 complex_0
 and etapes = pred stages in
  for i = 0 to pred etapes do
   s.(i) <- direct_complex_spectrum threshold_qr threshold steps_qr ( f steps ( etapes - i ) ) m
  done ;
  s.(etapes) <- direct_complex_spectrum threshold_qr threshold steps_qr steps m ;
  let sequence = Array.map complex_poly_to_complex_vector s in
   let limit = accelerator sequence in
    complex_vector_to_complex_poly limit ;;


(** {v direct_complex_compensated_shifted_spectrum accelerator stages factor methode_ker threshold_qr threshold steps_qr steps shift matrix v} 
The [accelerator] is appied to real vectors. The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux vecteurs réels. Le facteur [factor] doit être choisi entre 0 et 1. *)
let direct_complex_compensated_shifted_spectrum = fun accelerator (stages:int) (factor:float) methode_ker (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (shift:float array array) (m:float array array) ->
 let f = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) )
 and s = Array.make_matrix stages 1 complex_0
 and etapes = pred stages in
  for i = 0 to pred etapes do
   s.(i) <- direct_complex_shifted_spectrum threshold_qr threshold steps_qr ( f steps ( etapes - i ) ) shift m
  done ;
  s.(etapes) <- direct_complex_shifted_spectrum threshold_qr threshold steps_qr steps shift m ;
  let sequence = Array.map complex_poly_to_complex_vector s in
   let limit = accelerator sequence in
    complex_vector_to_complex_poly limit ;;


(** {v complex_compensated_spectrum accelerator stages factor threshold_qr threshold steps_qr steps matrix v} 
The [accelerator] is appied to real vectors. The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux vecteurs réels. Le facteur [factor] doit être choisi entre 0 et 1. *)
let complex_compensated_spectrum = fun accelerator (stages:int) (factor:float) (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (m:float array array) ->
 let f = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) )
 and s = Array.make_matrix stages 1 complex_0
 and etapes = pred stages in
  for i = 0 to pred etapes do
   s.(i) <- direct_complex_spectrum threshold_qr threshold steps_qr ( f steps ( etapes - i ) ) m
  done ;
  s.(etapes) <- direct_complex_spectrum threshold_qr threshold steps_qr steps m ;
  let sequence = Array.map complex_poly_to_complex_vector s in
   let limit = accelerator sequence in
    complex_vector_to_complex_poly limit ;;


(** {v complex_compensated_shifted_spectrum accelerator stages factor threshold_qr threshold steps_qr steps shift matrix v} 
The [accelerator] is appied to real vectors. The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux vecteurs réels. Le facteur [factor] doit être choisi entre 0 et 1. *)
let complex_compensated_shifted_spectrum = fun accelerator (stages:int) (factor:float) (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (shift:float array array) (m:float array array) ->
 let f = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) )
 and s = Array.make_matrix stages 1 complex_0
 and etapes = pred stages in
  for i = 0 to pred etapes do
   s.(i) <- direct_complex_shifted_spectrum threshold_qr threshold steps_qr ( f steps ( etapes - i ) ) shift m
  done ;
  s.(etapes) <- direct_complex_shifted_spectrum threshold_qr threshold steps_qr steps shift m ;
  let sequence = Array.map complex_poly_to_complex_vector s in
   let limit = accelerator sequence in
    complex_vector_to_complex_poly limit ;;


(** {v largo_complex_compensated_spectrum accelerator_sci_1024 stages factor threshold_qr threshold steps_qr steps matrix v} 
The [accelerator] is appied to complex numbers with extended precision, like [Sci.approx_1024]. 
The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux nombres complexes en précision étendue, comme [Sci.approx_1024]. 
Le facteur [factor] doit être choisi entre 0 et 1. *)
let largo_complex_compensated_spectrum = fun accelerator (stages:int) (factor:float) (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (m:float array array) ->
 let f = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) )
 and s = Array.make_matrix stages 1 complex_0
 and etapes = pred stages in
  for i = 0 to pred etapes do
   s.(i) <- direct_complex_spectrum threshold_qr threshold steps_qr ( f steps ( etapes - i ) ) m
  done ;
  s.(etapes) <- direct_complex_spectrum threshold_qr threshold steps_qr steps m ;
  let sequence = Util.transpose s in
   let seq = Array.map poly_complex_to_sci sequence in
    let limits = Array.map accelerator seq in
     poly_sci_to_complex limits ;;


(** {v largo_complex_compensated_shifted_spectrum accelerator_sci_1024 stages factor threshold_qr threshold steps_qr steps shift matrix v} 
The [accelerator] is appied to complex numbers with extended precision, like [Sci.approx_1024]. 
The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux nombres complexes en précision étendue, comme [Sci.approx_1024]. 
Le facteur [factor] doit être choisi entre 0 et 1. *)
let largo_complex_compensated_shifted_spectrum = fun accelerator (stages:int) (factor:float) (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) (shift:float array array) (m:float array array) ->
 let f = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) )
 and s = Array.make_matrix stages 1 complex_0
 and etapes = pred stages in
  for i = 0 to pred etapes do
   s.(i) <- direct_complex_shifted_spectrum threshold_qr threshold steps_qr ( f steps ( etapes - i ) ) shift m
  done ;
  s.(etapes) <- direct_complex_shifted_spectrum threshold_qr threshold steps_qr steps shift m ;
  let sequence = Util.transpose s in
   let seq = Array.map poly_complex_to_sci sequence in
    let limits = Array.map accelerator seq in
     poly_sci_to_complex limits ;;


(** {v clean_complex_spectrum stages steps matrix v} *)
let clean_complex_spectrum = fun (stages:int) (steps:int) (m:float array array) ->
 largo_complex_compensated_spectrum Sci.approx_1024 stages 0.58 0. 0. steps steps m ;;


(** {v clean_complex_shifted_spectrum stages steps shift matrix v} *)
let clean_complex_shifted_spectrum = fun (stages:int) (steps:int) (shift:float array array) (m:float array array) ->
 largo_complex_compensated_shifted_spectrum Sci.approx_1024 stages 0.58 0. 0. steps steps shift m ;;



(** {v poly_complex_raw_roots spectrum_methode jordan_decomposition_methode polynomial v} *)
let poly_complex_raw_roots = fun spectrum_methode jordan_decomposition_methode (p:float array array array) ->
 spectrum_methode ( jordan_decomposition_methode p ( complex_companion p ) ).(0) ;;


(** {v poly_real_complex_raw_roots spectrum_method jordan_decomposition_methode real_polynomial v} *)
let poly_real_complex_raw_roots = fun spectrum_methode jordan_decomposition_methode (p:float array) ->
 let q = poly_real_to_complex p in
  spectrum_methode ( jordan_decomposition_methode q ( complex_companion q ) ).(0) ;;


(** {v poly_complex_tune_root_step eval_rule polynomial derivative_array threshold candidate v} 
The array of successive derivatives [derivative_array] must contain at least [p] and [p'].
A good value for [threshold] may lie between [min_float] and [epsilon_float].
Output : estimated root, estimated multiplicity, module of the evaluation of the polynomial at the estimated root, 
successive derivative array.

Sortie : racine estimée, multiplicité estimée, module de l'évaluation du polynôme en la racine, tableau des dérivées successives.
Le tableau de dérivées successives doit contenir au moins p et p'.
Une bonne valeur pour le seuil [threshold] peut être entre [min_float] et [epsilon_float]. *)
let poly_complex_tune_root_step = fun eval_rule (p:float array array array) (derivative_array:float array array array array) (threshold:float) (z0:float array array) ->
 let z = ref z0
 and mult = ref 1
 and old_z = ref z0
 and error = ref max_float
 and old_error = ref ( complex_module ( eval_rule p z0 ) )
 and d = int_of_float ( poly_complex_deg p )
 and i = ref 1
 and deriv = ref derivative_array in
  if !old_error <= threshold then
   begin
    while !i < d do
     let last_deriv = !deriv.(!i) in
      error := complex_module ( eval_rule !deriv.(!i) z0 ) ;
      if !error > threshold then ( mult := !i ; i := d )
      else
       begin
        i := !i + 1  ;
        if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_complex_deriv last_deriv |] ) ;
       end
    done ;
    [| [|[| !z ; [|[| float !mult |]|] ; [|[| !old_error |]|] |]|] ; !deriv |]
   end
  else
   begin
    while !i < d do
     let last_deriv = !deriv.(!i) in
      let denom = ( eval_rule last_deriv z0 ) in
       if complex_module denom <= threshold then
        begin
         old_error := !error ;
         i := !i + 1 ;
         if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_complex_deriv last_deriv |] ) ;
        end
       else
        begin
         let quotient = Matrix.matrix_float_prod ( eval_rule ( !deriv.( !i - 1 ) ) z0 ) ( complex_inv denom ) in
          old_z := !z ;
          z := clean_complex ( Matrix.matrix_float_minus z0 ( Matrix.matrix_float_scal_mult ( float !i ) quotient ) ) ;
          old_error := !error ;
          error := complex_module ( eval_rule p !z ) ;
           if !error > !old_error then ( z := !old_z ; error := !old_error ; i := d )
           else
            begin
             mult := !mult + 1 ;
             i := !i + 1 ;
             if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_complex_deriv last_deriv |] ) ;
            end
        end
    done ;
    [| [|[| !z ; [|[| float ( !mult ) |]|] ; [|[| !error |]|] |]|] ; !deriv |]
   end ;;


(** {v poly_complex_tune_roots eval_rule threshold mult_threshold max_steps candidates complex_polynomial v}
Output : estimated roots, estimated multiplicities with [mult_threshold], means of estimated multiplicities during
the iteration, modules of the evaluations of the polynomial at the estimated roots, successive derivative array.

Sortie : racines estimées, multiplicités estimées avec [mult_threshold], moyenne des multiplicités estimées pendant l'itération,
modules des évaluations du polynôme en les racines, tableau des dérivées successives. *)
let poly_complex_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (steps:int) (candidates:float array array array) (p:float array array array) ->
 let f = function x -> complex_module ( eval_rule p x )
 and derivative_array = ref [| p ; poly_complex_deriv p |]
 and d = int_of_float ( poly_complex_deg p ) in
  let dd = d - 1
  and j = ref 1
  and old_error = Array.make d max_float
  and error = Array.map f candidates
  and mult = Array.make_matrix d 1 1.
  and multiplicity = Array.make d 1.
  and multip = Array.make d 1.
  and accu = ref d
  and roots = poly_complex_copy candidates
  and old_roots = Array.make d complex_0 in
   for i = 0 to dd do
    while !j < steps do
     let result = poly_complex_tune_root_step eval_rule p !derivative_array threshold roots.(i) in
      mult.(i) <- Array.append mult.(i) [| result.(0).(0).(1).(0).(0) |] ;
      if Array.length !derivative_array < Array.length result.(1) then derivative_array := result.(1) ;
      let test = result.(0).(0).(2).(0).(0) in
       if test > old_error.(i) then j := max_int
       else 
        begin
         old_roots.(i) <- roots.(i) ;
         roots.(i) <- result.(0).(0).(0) ;
         old_error.(i) <- error.(i) ;
         error.(i) <- test ;
         j := !j + 1 ;
        end
    done ;
    multiplicity.(i) <- Matrix.vector_float_mean mult.(i) ;
    j := 1 ;
     while !j < d do
      if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_complex_deriv !derivative_array.( !j - 1 ) |] ;
      let essai = complex_module ( eval_rule !derivative_array.(!j) roots.(i) ) in
       if essai > mult_threshold then ( accu := !j ; j := d ) ;
       j := !j + 1 ;
     done ;
    multip.(i) <- float !accu ;
    j := 1 ;
   done ;
   [| [| roots ; [|[| multip |]|] ; [|[| multiplicity |]|] ; [|[| error |]|] |] ; !derivative_array |] ;;


(** {v poly_sci_tune_root_step eval_rule polynomial derivative_array threshold candidate v} 
The array of successive derivatives [derivative_array] must contain at least [p] and [p'].
A good value for [threshold] may lie between [min_float] and [epsilon_float].
Output : estimated root, estimated multiplicity, module of the evaluation of the polynomial at the estimated root, 
successive derivative array.

Sortie : racine estimée, multiplicité estimée, module de l'évaluation du polynôme en la racine, tableau des dérivées successives.
Le tableau de dérivées successives doit contenir au moins p et p'.
Une bonne valeur pour le seuil [threshold] peut être entre [min_float] et [epsilon_float]. *)
let poly_sci_tune_root_step = fun eval_rule (p:Num.num array array) (derivative_array:Num.num array array array) (threshold:float) (z0:Num.num array) ->
 let z = ref z0
 and mult = ref 1
 and old_z = ref z0
 and error = ref max_float
 and old_error = ref ( complex_module ( Sci.complex_of_sci ( eval_rule p z0 ) ) )
 and d = int_of_float ( poly_sci_deg p )
 and i = ref 1
 and deriv = ref derivative_array in
  if !old_error <= threshold then
   begin
    while !i < d do
     let last_deriv = !deriv.(!i) in
      error := complex_module ( Sci.complex_of_sci ( eval_rule !deriv.(!i) z0 ) ) ;
      if !error > threshold then ( mult := !i ; i := d )
      else
       begin
        i := !i + 1  ;
        if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_sci_deriv last_deriv |] ) ;
       end
    done ;
    [| [|[| !z ; Sci.sci_of_int !mult ; Sci.sci_of_float !old_error |]|] ; !deriv |]
   end
  else
   begin
    while !i < d do
     let last_deriv = !deriv.(!i) in
      let denom = ( eval_rule last_deriv z0 ) in
       if complex_module ( Sci.complex_of_sci denom ) <= threshold then
        begin
         old_error := !error ;
         i := !i + 1 ;
         if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_sci_deriv last_deriv |] ) ;
        end
       else
        begin
         let quotient = Sci.mult ( eval_rule ( !deriv.( !i - 1 ) ) z0 ) ( Sci.inv denom ) in
          old_z := !z ;
          z := Sci.minus z0 ( Sci.mult ( Sci.format [| Num.num_of_int !i ; Sci.num_0 ; Sci.num_0 |] ) quotient ) ;
          old_error := !error ;
          error := complex_module ( Sci.complex_of_sci ( eval_rule p !z ) ) ;
           if !error > !old_error then ( z := !old_z ; error := !old_error ; i := d )
           else
            begin
             mult := !mult + 1 ;
             i := !i + 1 ;
             if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_sci_deriv last_deriv |] ) ;
            end
        end
    done ;
    [| [|[| !z ; Sci.sci_of_int ( !mult ) ; Sci.sci_of_float !error |]|] ; !deriv |]
   end ;;


(** {v poly_sci_tune_roots eval_rule threshold mult_threshold max_steps candidates sci_polynomial v}
Output : estimated roots, estimated multiplicities with [mult_threshold], means of estimated multiplicities during
the iteration, modules of the evaluations of the polynomial at the estimated roots, successive derivative array.

Sortie : racines estimées, multiplicités estimées avec [mult_threshold], moyenne des multiplicités estimées pendant l'itération,
carrés des modules des évaluations du polynôme en les racines, tableau des dérivées successives. *)
let poly_sci_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (steps:int) (candidates:Num.num array array) (p:Num.num array array) ->
 let f = function x -> complex_module ( Sci.complex_of_sci ( eval_rule p x ) )
 and derivative_array = ref [| p ; poly_sci_deriv p |]
 and d = int_of_float ( poly_sci_deg p ) in
  let dd = d - 1
  and j = ref 1
  and old_error = Array.make d max_float
  and error = Array.map f candidates
  and mult = Array.make_matrix d 1 1.
  and multiplicity = Array.make d 1.
  and multip = Array.make d 1
  and accu = ref d
  and roots = poly_sci_copy candidates
  and old_roots = Array.make d Sci.sci_0 in
   for i = 0 to dd do
    while !j < steps do
     let result = poly_sci_tune_root_step eval_rule p !derivative_array threshold roots.(i) in
      mult.(i) <- Array.append mult.(i) [| ( Sci.complex_of_sci result.(0).(0).(1) ).(0).(0) |] ;
      if Array.length !derivative_array < Array.length result.(1) then derivative_array := result.(1) ;
      let test = ( Sci.complex_of_sci result.(0).(0).(2) ).(0).(0) in
       if test > old_error.(i) then j := max_int
       else 
        begin
         old_roots.(i) <- roots.(i) ;
         roots.(i) <- result.(0).(0).(0) ;
         old_error.(i) <- error.(i) ;
         error.(i) <- test ;
         j := !j + 1 ;
        end
    done ;
    multiplicity.(i) <- Matrix.vector_float_mean mult.(i) ;
    j := 1 ;
     while !j < d do
      if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_sci_deriv !derivative_array.( !j - 1 ) |] ;
      let essai = ( complex_module ( Sci.complex_of_sci ( eval_rule !derivative_array.(!j) roots.(i) ) ) ) in
       if essai > mult_threshold then ( accu := !j ; j := d ) ;
       j := !j + 1 ;
     done ;
    multip.(i) <- !accu ;
    j := 1 ;
   done ;
   [| [| roots ; Array.map Sci.sci_of_int multip ; Array.map Sci.sci_of_float multiplicity ; Array.map Sci.sci_of_float error |] ; !derivative_array |] ;;


(** {v poly_sci_1024_tune_root_step eval_rule polynomial derivative_array threshold candidate v} 
The array of successive derivatives [derivative_array] must contain at least [p] and [p'].
A good value for [threshold] may lie between [min_float] and [epsilon_float].
Output : estimated root, estimated multiplicity, module of the evaluation of the polynomial at the estimated root, 
successive derivative array.

Sortie : racine estimée, multiplicité estimée, module de l'évaluation du polynôme en la racine, tableau des dérivées successives.
Le tableau de dérivées successives doit contenir au moins p et p'.
Une bonne valeur pour le seuil [threshold] peut être entre [min_float] et [epsilon_float]. *)
let poly_sci_1024_tune_root_step = fun eval_rule (p:Num.num array array) (derivative_array:Num.num array array array) (threshold:float) (z0:Num.num array) ->
 let z = ref z0
 and mult = ref 1
 and old_z = ref z0
 and error = ref max_float
 and old_error = ref ( complex_module ( Sci.complex_of_sci ( eval_rule p z0 ) ) )
 and d = int_of_float ( poly_sci_deg p )
 and i = ref 1
 and deriv = ref derivative_array in
  if !old_error <= threshold then
   begin
    while !i < d do
     let last_deriv = !deriv.(!i) in
      error := complex_module ( Sci.complex_of_sci ( eval_rule !deriv.(!i) z0 ) ) ;
      if !error > threshold then ( mult := !i ; i := d )
      else
       begin
        i := !i + 1  ;
        if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_sci_1024_deriv last_deriv |] ) ;
       end
    done ;
    [| [|[| !z ; Sci.sci_of_int !mult ; Sci.sci_of_float !old_error |]|] ; !deriv |]
   end
  else
   begin
    while !i < d do
     let last_deriv = !deriv.(!i) in
      let denom = ( eval_rule last_deriv z0 ) in
       if complex_module ( Sci.complex_of_sci denom ) <= threshold then
        begin
         old_error := !error ;
         i := !i + 1 ;
         if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_sci_1024_deriv last_deriv |] ) ;
        end
       else
        begin
         let quotient = Sci.mult_1024 ( eval_rule ( !deriv.( !i - 1 ) ) z0 ) ( Sci.inv_1024 denom ) in
          old_z := !z ;
          z := Sci.minus_1024 z0 ( Sci.mult_1024 ( Sci.format_1024 [| Num.num_of_int !i ; Sci.num_0 ; Sci.num_0 |] ) quotient ) ;
          old_error := !error ;
          error := complex_module ( Sci.complex_of_sci ( eval_rule p !z ) ) ;
           if !error > !old_error then ( z := !old_z ; error := !old_error ; i := d )
           else
            begin
             mult := !mult + 1 ;
             i := !i + 1 ;
             if ( Array.length !deriv ) <= !i then ( deriv := Array.append !deriv [| poly_sci_1024_deriv last_deriv |] ) ;
            end
        end
    done ;
    [| [|[| !z ; Sci.sci_of_int ( !mult ) ; Sci.sci_of_float !error |]|] ; !deriv |]
   end ;;


(** {v poly_sci_1024_tune_roots eval_rule threshold mult_threshold max_steps candidates sci_polynomial v}
Output : estimated roots, estimated multiplicities with [mult_threshold], means of estimated multiplicities during
the iteration, modules of the evaluations of the polynomial at the estimated roots, successive derivative array.

Sortie : racines estimées, multiplicités estimées avec [mult_threshold], moyenne des multiplicités estimées pendant l'itération,
carrés des modules des évaluations du polynôme en les racines, tableau des dérivées successives. *)
let poly_sci_1024_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (steps:int) (candidates:Num.num array array) (p:Num.num array array) ->
 let f = function x -> complex_module ( Sci.complex_of_sci ( eval_rule p x ) )
 and derivative_array = ref [| p ; poly_sci_1024_deriv p |]
 and d = int_of_float ( poly_sci_deg p ) in
  let dd = d - 1
  and j = ref 1
  and old_error = Array.make d max_float
  and error = Array.map f candidates
  and mult = Array.make_matrix d 1 1.
  and multiplicity = Array.make d 1.
  and multip = Array.make d 1
  and accu = ref d
  and roots = poly_sci_copy candidates
  and old_roots = Array.make d Sci.sci_0 in
   for i = 0 to dd do
    while !j < steps do
     let result = poly_sci_1024_tune_root_step eval_rule p !derivative_array threshold roots.(i) in
      mult.(i) <- Array.append mult.(i) [| ( Sci.complex_of_sci result.(0).(0).(1) ).(0).(0) |] ;
      if Array.length !derivative_array < Array.length result.(1) then derivative_array := result.(1) ;
      let test = ( Sci.complex_of_sci result.(0).(0).(2) ).(0).(0) in
       if test > old_error.(i) then j := max_int
       else 
        begin
         old_roots.(i) <- roots.(i) ;
         roots.(i) <- result.(0).(0).(0) ;
         old_error.(i) <- error.(i) ;
         error.(i) <- test ;
         j := !j + 1 ;
        end
    done ;
    multiplicity.(i) <- Matrix.vector_float_mean mult.(i) ;
    j := 1 ;
     while !j < d do
      if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_sci_1024_deriv !derivative_array.( !j - 1 ) |] ;
      let essai = complex_module ( Sci.complex_of_sci ( eval_rule !derivative_array.(!j) roots.(i) ) ) in
       if essai > mult_threshold then ( accu := !j ; j := d ) ;
       j := !j + 1 ;
     done ;
    multip.(i) <- !accu ;
    j := 1 ;
   done ;
   [| [| roots ; Array.map Sci.sci_of_int multip ; Array.map Sci.sci_of_float multiplicity ; Array.map Sci.sci_of_float error |] ; !derivative_array |] ;;


(** {v poly_real_complex_tune_roots eval_rule threshold mult_threshold max_steps candidates real_polynomial v} *)
let poly_real_complex_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (steps:int) (candidates:float array array array) (p:float array) ->
 poly_complex_tune_roots eval_rule threshold mult_threshold steps candidates ( poly_real_to_complex p ) ;;


(** {v lento_poly_complex_tune_roots eval_rule threshold mult_threshold steps candidates polynomial v} *)
let lento_poly_complex_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (steps:int) (candidates:float array array array) (p:float array array array) ->
 let f = Array.map ( Array.map ( Sci.complex_of_sci ) ) in
  Array.map f ( poly_sci_tune_roots eval_rule threshold mult_threshold steps ( Array.map Sci.sci_of_complex candidates ) ( Array.map Sci.sci_of_complex p ) ) ;;


(** {v lento_poly_real_complex_tune_roots eval_rule threshold mult_threshold steps candidates polynomial v} *)
let lento_poly_real_complex_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (steps:int) (candidates:float array array array) (p:float array) ->
 let f = Array.map ( Array.map ( Sci.complex_of_sci ) ) in
  Array.map f (  poly_sci_tune_roots eval_rule threshold mult_threshold steps ( Array.map Sci.sci_of_complex candidates ) ( Array.map Sci.sci_of_float p ) ) ;;


(** {v largo_poly_complex_tune_roots eval_rule threshold mult_threshold steps candidates polynomial v} *)
let largo_poly_complex_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (steps:int) (candidates:float array array array) (p:float array array array) ->
 let f = Array.map ( Array.map ( Sci.complex_of_sci ) ) in
  Array.map f ( poly_sci_1024_tune_roots eval_rule threshold mult_threshold steps ( Array.map Sci.sci_of_complex candidates ) ( Array.map Sci.sci_of_complex p ) ) ;;


(** {v largo_poly_real_complex_tune_roots eval_rule threshold mult_threshold steps candidates polynomial v} *)
let largo_poly_real_complex_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (steps:int) (candidates:float array array array) (p:float array) ->
 let f = Array.map ( Array.map ( Sci.complex_of_sci ) ) in
  Array.map f (  poly_sci_1024_tune_roots eval_rule threshold mult_threshold steps ( Array.map Sci.sci_of_complex candidates ) ( Array.map Sci.sci_of_float p ) ) ;;


(** {v poly_aitken_seki_1024_tune_roots eval_rule threshold mult_threshold candidates polynomial v} *)
let poly_aitken_seki_1024_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (candidates:Num.num array array) (p:Num.num array array) ->
 let l = Array.length candidates
 and derivative_array = ref [| p ; poly_sci_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_sci_deg p ) in
  let z = Array.make l Sci.sci_0
  and accu = ref d
  and multip = Array.make d 1 in
   for i = 0 to l - 1 do
    let ra = candidates.(i) in
     let rb = poly_sci_1024_tune_root_step eval_rule p !derivative_array threshold ra in
      let rc = poly_sci_1024_tune_root_step eval_rule p rb.(1) threshold rb.(0).(0).(0) in
       let rd = Sci.aitken_seki_1024 ra rb.(0).(0).(0) rc.(0).(0).(0) in
        z.(i) <- rd ;
        derivative_array := rc.(1) ;
        j := 1 ;
        while !j < d do
         if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_sci_1024_deriv !derivative_array.( !j - 1 ) |] ;
         let essai = complex_module ( Sci.complex_of_sci ( eval_rule !derivative_array.(!j) rd ) ) in
          if essai > mult_threshold then ( accu := !j ; j := d ) ;
          j := !j + 1 ;
        done ;
        multip.(i) <- !accu ;
        j := 1 ;
   done ;
   [| [| z ; Array.map Sci.sci_of_int multip |] ; !derivative_array |] ;;


(** {v poly_shanks2_1024_tune_roots eval_rule threshold mult_threshold candidates polynomial v} *)
let poly_shanks2_1024_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (candidates:Num.num array array) (p:Num.num array array) ->
 let l = Array.length candidates
 and derivative_array = ref [| p ; poly_sci_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_sci_deg p ) in
  let z = Array.make l Sci.sci_0
  and accu = ref d
  and multip = Array.make d 1 in
   for i = 0 to l - 1 do
    let ra = candidates.(i) in
     let rb = poly_sci_1024_tune_root_step eval_rule p !derivative_array threshold ra in
      let rc = poly_sci_1024_tune_root_step eval_rule p rb.(1) threshold rb.(0).(0).(0) in
       let rd = poly_sci_1024_tune_root_step eval_rule p rc.(1) threshold rc.(0).(0).(0) in
        let re = poly_sci_1024_tune_root_step eval_rule p rd.(1) threshold rd.(0).(0).(0) in
         let rf = Sci.shanks2_1024 ra rb.(0).(0).(0) rc.(0).(0).(0) rd.(0).(0).(0) re.(0).(0).(0) in
          z.(i) <- rf ;
          derivative_array := re.(1) ;
          j := 1 ;
          while !j < d do
           if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_sci_1024_deriv !derivative_array.( !j - 1 ) |] ;
           let essai = complex_module ( Sci.complex_of_sci ( eval_rule !derivative_array.(!j) rf ) ) in
            if essai > mult_threshold then ( accu := !j ; j := d ) ;
            j := !j + 1 ;
          done ;
          multip.(i) <- !accu ;
          j := 1 ;
   done ;
   [| [| z ; Array.map Sci.sci_of_int multip |] ; !derivative_array |] ;;


(** {v poly_wynn_1024_tune_roots eval_rule half_order threshold mult_threshold candidates polynomial v} *)
let poly_wynn_1024_tune_roots = fun eval_rule (k:int) (threshold:float) (mult_threshold:float) (candidates:Num.num array array) (p:Num.num array array) ->
 let l = Array.length candidates
 and dk = 2 * k
 and derivative_array = ref [| p ; poly_sci_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_sci_deg p ) in
  let z = Array.make l Sci.sci_0
  and kk = succ dk
  and accu = ref d
  and multip = Array.make d 1 in
   let roots = Array.make_matrix kk 1 ( Array.make_matrix 1 1 Sci.sci_0 ) in
    for i = 0 to l - 1 do
     roots.(0) <- [| [|[| candidates.(i) ; Sci.sci_1 ; Sci.sci_of_float max_float |]|] ; !derivative_array |] ;
     for index = 1 to dk do
      let racine = roots.( pred index ) in
       roots.(index) <- poly_sci_1024_tune_root_step eval_rule p racine.(1) threshold racine.(0).(0).(0) ;
     done ;
     let estimate = Sci.wynn_1024 dk 0 ( Array.map ( function x -> x.(0).(0).(0) ) roots ) in
      z.(i) <- estimate ;
      derivative_array := roots.(dk).(1) ;
      j := 1 ;
      while !j < d do
       if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_sci_1024_deriv !derivative_array.( !j - 1 ) |] ;
       let essai = complex_module ( Sci.complex_of_sci ( eval_rule !derivative_array.(!j) estimate ) ) in
        if essai > mult_threshold then ( accu := !j ; j := d ) ;
        j := !j + 1 ;
      done ;
      multip.(i) <- !accu ;
      j := 1 ;
   done ;
   [| [| z ; Array.map Sci.sci_of_int multip |] ; !derivative_array |] ;;


(** {v poly_brezinski_1024_tune_roots eval_rule half_order threshold mult_threshold candidates polynomial v} *)
let poly_brezinski_1024_tune_roots = fun eval_rule (k:int) (threshold:float) (mult_threshold:float) (candidates:Num.num array array) (p:Num.num array array) ->
 let l = Array.length candidates
 and dk = 2 * k
 and derivative_array = ref [| p ; poly_sci_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_sci_deg p ) in
  let z = Array.make l Sci.sci_0
  and kk = succ dk
  and accu = ref d
  and multip = Array.make d 1 in
   let roots = Array.make_matrix kk 1 ( Array.make_matrix 1 1 Sci.sci_0 ) in
    for i = 0 to l - 1 do
     roots.(0) <- [| [|[| candidates.(i) ; Sci.sci_1 ; Sci.sci_of_float max_float |]|] ; !derivative_array |] ;
     for index = 1 to dk do
      let racine = roots.( pred index ) in
       roots.(index) <- poly_sci_1024_tune_root_step eval_rule p racine.(1) threshold racine.(0).(0).(0) ;
     done ;
     let estimate = Sci.brezinski_1024 ( dk - 2 ) 0 ( Array.map ( function x -> x.(0).(0).(0) ) roots ) in
      z.(i) <- estimate ;
      derivative_array := roots.(dk).(1) ;
      j := 1 ;
      while !j < d do
       if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_sci_1024_deriv !derivative_array.( !j - 1 ) |] ;
       let essai = complex_module ( Sci.complex_of_sci ( eval_rule !derivative_array.(!j) estimate ) ) in
        if essai > mult_threshold then ( accu := !j ; j := d ) ;
        j := !j + 1 ;
      done ;
      multip.(i) <- !accu ;
      j := 1 ;
   done ;
   [| [| z ; Array.map Sci.sci_of_int multip |] ; !derivative_array |] ;;


(** {v poly_aitken_seki_rec_1024_tune_roots eval_rule half_order threshold mult_threshold candidates polynomial v} *)
let poly_aitken_seki_rec_1024_tune_roots = fun eval_rule (k:int) (threshold:float) (mult_threshold:float) (candidates:Num.num array array) (p:Num.num array array) ->
 let l = Array.length candidates
 and dk = 2 * k
 and derivative_array = ref [| p ; poly_sci_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_sci_deg p ) in
  let z = Array.make l Sci.sci_0
  and kk = succ dk
  and accu = ref d
  and multip = Array.make l 1 in
   let roots = Array.make_matrix kk 1 ( Array.make_matrix 1 1 Sci.sci_0 ) in
    for i = 0 to l - 1 do
     roots.(0) <- [| [|[| candidates.(i) ; Sci.sci_1 ; Sci.sci_of_float max_float |]|] ; !derivative_array |] ;
     for index = 1 to dk do
      let racine = roots.( pred index ) in
       roots.(index) <- poly_sci_1024_tune_root_step eval_rule p racine.(1) threshold racine.(0).(0).(0) ;
     done ;
     let estimate = Sci.aitken_seki_rec_1024 k 0 ( Array.map ( function x -> x.(0).(0).(0) ) roots ) in
      z.(i) <- estimate ;
      derivative_array := roots.(dk).(1) ;
      j := 1 ;
      while !j < d do
       if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_sci_1024_deriv !derivative_array.( !j - 1 ) |] ;
       let essai = complex_module ( Sci.complex_of_sci ( eval_rule !derivative_array.(!j) estimate ) ) in
        if essai > mult_threshold then ( accu := !j ; j := d ) ;
        j := !j + 1 ;
      done ;
      multip.(i) <- !accu ;
      j := 1 ;
   done ;
   [| [| z ; Array.map Sci.sci_of_int multip |] ; !derivative_array |] ;;


(** {v poly_aitken_seki_complex_tune_roots eval_rule threshold mult_threshold candidates polynomial v} *)
let poly_aitken_seki_complex_tune_roots = fun eval_rule (threshold:float) (mult_threshold:float) (candidates:float array array array) (p:float array array array) ->
 let l = Array.length candidates
 and derivative_array = ref [| p ; poly_complex_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_complex_deg p ) in
  let z = Array.make l complex_0
  and accu = ref d
  and multip = Array.make d 1 in
   for i = 0 to l - 1 do
    let ra = candidates.(i) in
     let rb = poly_complex_tune_root_step eval_rule p !derivative_array threshold ra in
      let rc = poly_complex_tune_root_step eval_rule p rb.(1) threshold rb.(0).(0).(0) in
       let rd = Matrix.matrix_float_aitken_seki ra rb.(0).(0).(0) rc.(0).(0).(0) in
        z.(i) <- rd ;
        derivative_array := rc.(1) ;
        j := 1 ;
        while !j < d do
         if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_complex_deriv !derivative_array.( !j - 1 ) |] ;
         let essai = complex_module ( eval_rule !derivative_array.(!j) rd ) in
          if essai > mult_threshold then ( accu := !j ; j := d ) ;
          j := !j + 1 ;
        done ;
        multip.(i) <- !accu ;
        j := 1 ;
   done ;
   [| [| z ; Array.map int_to_complex multip |] ; !derivative_array |] ;;


(** {v poly_wynn_complex_tune_roots eval_rule half_order threshold mult_threshold candidates polynomial v} *)
let poly_wynn_complex_tune_roots = fun eval_rule (k:int) (threshold:float) (mult_threshold:float) (candidates:float array array array) (p:float array array array) ->
 let l = Array.length candidates
 and dk = 2 * k
 and derivative_array = ref [| p ; poly_complex_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_complex_deg p ) in
  let z = Array.make l complex_0
  and kk = succ dk
  and accu = ref d
  and multip = Array.make d 1 in
   let roots = Array.make_matrix kk 1 ( Array.make_matrix 1 1 complex_0 ) in
    for i = 0 to l - 1 do
     roots.(0) <- [| [|[| candidates.(i) ; complex_1 ; float_to_complex max_float |]|] ; !derivative_array |] ;
     for index = 1 to dk do
      let racine = roots.( pred index ) in
       roots.(index) <- poly_complex_tune_root_step eval_rule p racine.(1) threshold racine.(0).(0).(0) ;
     done ;
     let estimate = Matrix.matrix_float_wynn dk 0 ( Array.map ( function x -> x.(0).(0).(0) ) roots ) in
      z.(i) <- estimate ;
      derivative_array := roots.(dk).(1) ;
      j := 1 ;
      while !j < d do
       if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_complex_deriv !derivative_array.( !j - 1 ) |] ;
       let essai = complex_module ( eval_rule !derivative_array.(!j) estimate ) in
        if essai > mult_threshold then ( accu := !j ; j := d ) ;
        j := !j + 1 ;
      done ;
      multip.(i) <- !accu ;
      j := 1 ;
   done ;
   [| [| z ; Array.map int_to_complex multip |] ; !derivative_array |] ;;


(** {v poly_brezinski_complex_tune_roots eval_rule half_order threshold mult_threshold candidates polynomial v} *)
let poly_brezinski_complex_tune_roots = fun eval_rule (k:int) (threshold:float) (mult_threshold:float) (candidates:float array array array) (p:float array array array) ->
 let l = Array.length candidates
 and dk = 2 * k
 and derivative_array = ref [| p ; poly_complex_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_complex_deg p ) in
  let z = Array.make l complex_0
  and kk = succ dk
  and accu = ref d
  and multip = Array.make d 1 in
   let roots = Array.make_matrix kk 1 ( Array.make_matrix 1 1 complex_0 ) in
    for i = 0 to l - 1 do
     roots.(0) <- [| [|[| candidates.(i) ; complex_1 ; float_to_complex max_float |]|] ; !derivative_array |] ;
     for index = 1 to dk do
      let racine = roots.( pred index ) in
       roots.(index) <- poly_complex_tune_root_step eval_rule p racine.(1) threshold racine.(0).(0).(0) ;
     done ;
     let estimate = Matrix.matrix_float_brezinski ( dk - 2 ) 0 ( Array.map ( function x -> x.(0).(0).(0) ) roots ) in
      z.(i) <- estimate ;
      derivative_array := roots.(dk).(1) ;
      j := 1 ;
      while !j < d do
       if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_complex_deriv !derivative_array.( !j - 1 ) |] ;
       let essai = complex_module ( eval_rule !derivative_array.(!j) estimate ) in
        if essai > mult_threshold then ( accu := !j ; j := d ) ;
        j := !j + 1 ;
      done ;
      multip.(i) <- !accu ;
      j := 1 ;
   done ;
   [| [| z ; Array.map int_to_complex multip |] ; !derivative_array |] ;;


(** {v poly_aitken_seki_rec_complex_tune_roots eval_rule half_order threshold mult_threshold candidates polynomial v} *)
let poly_aitken_seki_rec_complex_tune_roots = fun eval_rule (k:int) (threshold:float) (mult_threshold:float) (candidates:float array array array) (p:float array array array) ->
 let l = Array.length candidates
 and dk = 2 * k
 and derivative_array = ref [| p ; poly_complex_deriv p |]
 and j = ref 1
 and d = int_of_float ( poly_complex_deg p ) in
  let z = Array.make l complex_0
  and kk = succ dk
  and accu = ref d
  and multip = Array.make l 1 in
   let roots = Array.make_matrix kk 1 ( Array.make_matrix 1 1 complex_0 ) in
    for i = 0 to l - 1 do
     roots.(0) <- [| [|[| candidates.(i) ; complex_1 ; float_to_complex max_float |]|] ; !derivative_array |] ;
     for index = 1 to dk do
      let racine = roots.( pred index ) in
       roots.(index) <- poly_complex_tune_root_step eval_rule p racine.(1) threshold racine.(0).(0).(0) ;
     done ;
     let estimate = Matrix.matrix_float_aitken_seki_rec k 0 ( Array.map ( function x -> x.(0).(0).(0) ) roots ) in
      z.(i) <- estimate ;
      derivative_array := roots.(dk).(1) ;
      j := 1 ;
      while !j < d do
       if Array.length !derivative_array = !j then derivative_array := Array.append !derivative_array [| poly_complex_deriv !derivative_array.( !j - 1 ) |] ;
       let essai = complex_module ( eval_rule !derivative_array.(!j) estimate ) in
        if essai > mult_threshold then ( accu := !j ; j := d ) ;
        j := !j + 1 ;
      done ;
      multip.(i) <- !accu ;
      j := 1 ;
   done ;
   [| [| z ; Array.map int_to_complex multip |] ; !derivative_array |] ;;


(** {v simple_complex_roots eval_rule stages_spectrum stages_roots steps threshold polynomial v} *)
let rec simple_complex_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (p:float array array array) ->
 let d = poly_complex_deg p in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_complex_roots." ;
  match d with
  | 0. -> failwith "Nonzero constant polynomial in Reduc.simple_complex_roots."
  | 1. ->
   begin
    let s = complex_div p.(0) p.(1) in
     [| Matrix.matrix_float_opp s |]
   end
  | 2. -> complex_solve_degree_2 p.(2) p.(1) p.(0)
  | _ ->
   begin
    let m = complex_companion p in
     let s = clean_complex_spectrum stages_spectrum steps m in
      let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold 1. [| s.(0) |] p in
       let a = res.(0).(0).(0) in
        let q = ( poly_complex_div p ( poly_complex_x_a a ) ).(0) in
         Array.append [| a |] ( simple_complex_roots eval_rule stages_spectrum stages_roots steps threshold q )
   end ;;


(** {v maehly_complex_step eval_rule roots polynomial derivative value v} The algorithm is described at the following address.

www.cs.iastate.edu/~cs577/handouts/polyroots.pdf

L'algorithme est décrit à l'adresse précédente. *)
let maehly_complex_step = fun eval_rule (r:float array array array) (p:float array array array) (der_p:float array array array) (x:float array array) ->
 let vector = Array.map ( Matrix.matrix_float_minus x ) r
 and numerator = eval_rule p x
 and init = Matrix.matrix_float_opp ( eval_rule der_p x ) in
  let other_vector = Array.map ( complex_div numerator ) vector in
   let denominator = Array.fold_left Matrix.matrix_float_plus init other_vector in
    let fraction = complex_div numerator denominator in
     Matrix.matrix_float_plus x fraction ;;

(** {v simple_maehly_complex_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold roots polynomial derivative v} *)
let rec simple_maehly_complex_roots = fun mult_rule eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (r:float array array array) (p:float array array array) (der_p:float array array array) ->
 let d = poly_complex_deg p
 and n = float ( Array.length r ) in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_maehly_complex_roots." ;
  let dd = d -. n in
   if dd < 0. then failwith "Too much roots in Reduc.simple_maehly_complex_roots." ;
   match dd with
   | 0. -> r
   | 1. ->
    begin
     let q = ( poly_complex_div p ( poly_complex_from_roots mult_rule r ) ).(0) in
      let s = complex_div q.(0) q.(1) in
       Array.append r [| Matrix.matrix_float_opp s |]
    end
   | 2. ->
    begin
     let q = ( poly_complex_div p ( poly_complex_from_roots mult_rule r ) ).(0) in
      Array.append r ( complex_solve_degree_2 q.(2) q.(1) q.(0) )
    end
   | _ ->
    begin
     let q = if Array.length r > 0 then ( poly_complex_div p ( poly_complex_from_roots mult_rule r ) ).(0) else p in
      let m = complex_companion q in
       let s =
        begin
         try
          begin
           let u = matrix_unitary_random ( pred ( Array.length p ) ) 1. in
            let v = Matrix.matrix_float_twisted_prod u m in
             let w = Matrix.matrix_float_twisted_prod u v in
              clean_complex_spectrum stages_spectrum steps w
          end
         with _ ->
          clean_complex_spectrum stages_spectrum steps m
        end in
        let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold 1. [| s.(0) |] q in
         let a = ref res.(0).(0).(0) in
          let error = ref ( complex_abs_sum ( eval_rule p !a ) ) in
           while !error > threshold do
            a := maehly_complex_step eval_rule r p der_p !a ;
            error := complex_abs_sum ( eval_rule p !a ) ;
           done ;
           simple_maehly_complex_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold ( Array.append r [| !a |] ) p der_p
    end ;;

(** {v simple_sci_1024_roots eval_rule stages_spectrum stages_roots steps threshold polynomial v} *)
let rec simple_sci_1024_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (p:Num.num array array) ->
 let d = poly_sci_deg p in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_sci_1024_roots." ;
  match d with
  | 0. -> failwith "Nonzero constant polynomial in Reduc.simple_sci_1024_roots."
  | 1. ->
   begin
    let s = Sci.div_1024 p.(0) p.(1) in
     [| Sci.opp s |]
   end
  | 2. -> Sci.solve_degree_2_1024 p.(2) p.(1) p.(0)
  | 3. -> Sci.solve_degree_3_1024 p.(3) p.(2) p.(1) p.(0)
  | 4. -> Sci.solve_degree_4_1024 p.(4) p.(3) p.(2) p.(1) p.(0)
  | _ ->
   begin
    let pp = poly_sci_to_complex p in
     let m = complex_companion pp in
      let s = clean_complex_spectrum stages_spectrum steps m in
       let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold 1. [| Sci.sci_of_complex s.(0) |] p in
        let a = res.(0).(0).(0) in
         let q = ( poly_sci_1024_div p ( poly_sci_x_a a ) ).(0) in
          Array.append [| a |] ( simple_sci_1024_roots eval_rule stages_spectrum stages_roots steps threshold q )
   end ;;


(** {v maehly_sci_1024_step eval_rule roots polynomial derivative value v} The algorithm is described at the following address.

www.cs.iastate.edu/~cs577/handouts/polyroots.pdf

L'algorithme est décrit à l'adresse précédente. *)
let maehly_sci_1024_step = fun eval_rule (r:Num.num array array) (p:Num.num array array) (der_p:Num.num array array) (x:Num.num array) ->
 let vector = Array.map ( Sci.minus_1024 x ) r
 and numerator = eval_rule p x
 and init = Sci.opp ( eval_rule der_p x ) in
  let other_vector = Array.map ( Sci.div_1024 numerator ) vector in
   let denominator = Array.fold_left Sci.plus_1024 init other_vector in
    let fraction = Sci.div_1024 numerator denominator in
     Sci.plus_1024 x fraction ;;

(** {v simple_maehly_sci_1024_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold roots polynomial derivative v} *)
let rec simple_maehly_sci_1024_roots = fun mult_rule eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (r:Num.num array array) (p:Num.num array array) (der_p:Num.num array array) ->
 let d = poly_sci_deg p
 and n = float ( Array.length r ) in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_maehly_sci_1024_roots." ;
  let dd = d -. n in
   if dd < 0. then failwith "Too much roots in Reduc.simple_maehly_sci_1024_roots." ;
   match dd with
   | 0. -> r
   | 1. ->
    begin
     let q = ( poly_sci_1024_div p ( poly_sci_1024_from_roots mult_rule r ) ).(0) in
      let s = Sci.div_1024 q.(0) q.(1) in
       Array.append r [| Sci.opp s |]
    end
   | 2. ->
    begin
     let q = ( poly_sci_1024_div p ( poly_sci_1024_from_roots mult_rule r ) ).(0) in
      Array.append r ( Sci.solve_degree_2_1024 q.(2) q.(1) q.(0) )
    end
   | 3. ->
    begin
     let q = ( poly_sci_1024_div p ( poly_sci_1024_from_roots mult_rule r ) ).(0) in
      Array.append r ( Sci.solve_degree_3_1024 q.(3) q.(2) q.(1) q.(0) )
    end
   | 4. ->
    begin
     let q = ( poly_sci_1024_div p ( poly_sci_1024_from_roots mult_rule r ) ).(0) in
      Array.append r ( Sci.solve_degree_4_1024 q.(4) q.(3) q.(2) q.(1) q.(0) )
    end
   | _ ->
    begin
     let q = if Array.length r > 0 then ( poly_sci_1024_div p ( poly_sci_1024_from_roots mult_rule r ) ).(0) else p in
      let qq = Array.map Sci.complex_of_sci q in
       let m = complex_companion qq in
        let s =
         begin
          try
           begin
            let u = matrix_unitary_random ( pred ( Array.length p ) ) 1. in
             let v = Matrix.matrix_float_twisted_prod u m in
              let w = Matrix.matrix_float_twisted_prod u v in
               clean_complex_spectrum stages_spectrum steps w
           end
          with _ ->
           clean_complex_spectrum stages_spectrum steps m
         end in
         let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold 1. [| Sci.sci_of_complex s.(0) |] q in
          let a = ref res.(0).(0).(0) in
           let error = ref ( complex_abs_sum ( Sci.complex_of_sci ( eval_rule p !a ) ) ) in
            while !error > threshold do
             a := maehly_sci_1024_step eval_rule r p der_p !a ;
             error := complex_abs_sum ( Sci.complex_of_sci ( eval_rule p !a ) ) ;
            done ;
            simple_maehly_sci_1024_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold ( Array.append r [| !a |] ) p der_p
    end ;;


(** {v largo_simple_complex_roots eval_rule stages_spectrum stages_roots steps threshold polynomial v} *)
let largo_simple_complex_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (p:float array array array) ->
 let r = simple_sci_1024_roots eval_rule stages_spectrum stages_roots steps threshold ( poly_complex_to_sci p ) in
  poly_sci_to_complex r ;;


(** {v largo_simple_maehly_complex_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold polynomial v} *)
let largo_simple_maehly_complex_roots = fun mult_rule eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (p:float array array array) ->
 let pp = poly_complex_to_sci p in
  let r = simple_maehly_sci_1024_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold [| |] pp ( poly_sci_1024_deriv pp ) in
   poly_sci_to_complex r ;;


(** {v simple_direct_complex_roots eval_rule stages_roots steps threshold_qr threshold polynomial v} *)
let rec simple_direct_complex_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (p:float array array array) ->
 let d = poly_complex_deg p in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_direct_complex_roots." ;
  match d with
  | 0. -> failwith "Nonzero constant polynomial in Reduc.simple_direct_complex_roots."
  | 1. ->
   begin
    let s = complex_div p.(0) p.(1) in
     [| Matrix.matrix_float_opp s |]
   end
  | 2. -> complex_solve_degree_2 p.(2) p.(1) p.(0)
  | _ ->
   begin
    let m = complex_companion p in
     let s = direct_complex_spectrum threshold_qr threshold steps steps m in
      let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold 1. [| s.(0) |] p in
       let a = res.(0).(0).(0) in
        let q = ( poly_complex_div p ( poly_complex_x_a a ) ).(0) in
         Array.append [| a |] ( simple_direct_complex_roots eval_rule stages_roots steps threshold_qr threshold q )
   end ;;


(** {v simple_direct_sci_1024_roots eval_rule stages_roots steps threshold_qr threshold polynomial v} *)
let rec simple_direct_sci_1024_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (p:Num.num array array) ->
 let d = poly_sci_deg p in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_direct_sci_1024_roots." ;
  match d with
  | 0. -> failwith "Nonzero constant polynomial in Reduc.simple_direct_sci_1024_roots."
  | 1. ->
   begin
    let s = Sci.div_1024 p.(0) p.(1) in
     [| Sci.opp s |]
   end
  | 2. -> Sci.solve_degree_2_1024 p.(2) p.(1) p.(0)
  | 3. -> Sci.solve_degree_3_1024 p.(3) p.(2) p.(1) p.(0)
  | 4. -> Sci.solve_degree_4_1024 p.(4) p.(3) p.(2) p.(1) p.(0)
  | _ ->
   begin
    let pp = poly_sci_to_complex p in
     let m = complex_companion pp in
      let s = direct_complex_spectrum threshold_qr threshold steps steps m in
       let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold 1. [| Sci.sci_of_complex s.(0) |] p in
        let a = res.(0).(0).(0) in
         let q = ( poly_sci_1024_div p ( poly_sci_x_a a ) ).(0) in
          Array.append [| a |] ( simple_direct_sci_1024_roots eval_rule stages_roots steps threshold_qr threshold q )
   end ;;


(** {v largo_simple_direct_complex_roots eval_rule stages_roots steps threshold_qr threshold polynomial v} *)
let largo_simple_direct_complex_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (p:float array array array) ->
 let r = simple_direct_sci_1024_roots eval_rule stages_roots steps threshold_qr threshold ( poly_complex_to_sci p ) in
  poly_sci_to_complex r ;;


(** {v naive_complex_roots eval_rule stages_spectrum stages_roots steps threshold mult_threshold polynomial v} *)
let naive_complex_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (mult_threshold:float) (p:float array array array) ->
 let q = poly_complex_simplify p in
  let s = simple_complex_roots eval_rule stages_spectrum stages_roots steps threshold q in
   let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold mult_threshold s p in
    res.(0) ;;


(** {v complex_maehly_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold mult_threshold polynomial v} *)
let complex_maehly_roots = fun mult_rule eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (mult_threshold:float) (p:float array array array) ->
 let q = poly_complex_simplify p in
  let s = simple_maehly_complex_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold [| |] q ( poly_complex_deriv q ) in
   let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold mult_threshold s p in
    res.(0) ;;


(** {v direct_complex_roots eval_rule stages_roots steps threshold_qr threshold mult_threshold polynomial v} *)
let direct_complex_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (mult_threshold:float) (p:float array array array) ->
 let s = simple_direct_complex_roots eval_rule stages_roots steps threshold_qr threshold p in
  let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold mult_threshold s p in
   res.(0) ;;


(** {v naive_sci_1024_roots eval_rule stages_spectrum stages_roots steps threshold mult_threshold polynomial v} *)
let naive_sci_1024_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (mult_threshold:float) (p:Num.num array array) ->
 let q = poly_sci_1024_simplify p in
  let s = simple_sci_1024_roots eval_rule stages_spectrum stages_roots steps threshold q in
   let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold mult_threshold s p in
    res.(0) ;;


(** {v sci_1024_maehly_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold mult_threshold polynomial v} *)
let sci_1024_maehly_roots = fun mult_rule eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (mult_threshold:float) (p:Num.num array array) ->
 let q = poly_sci_1024_simplify p in
  let s = simple_maehly_sci_1024_roots mult_rule eval_rule stages_spectrum stages_roots steps threshold [| |] q ( poly_sci_1024_deriv q ) in
   let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold mult_threshold s p in
    res.(0) ;;


(** {v direct_sci_1024_roots eval_rule stages_roots steps threshold_qr threshold mult_threshold polynomial v} *)
let direct_sci_1024_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (mult_threshold:float) (p:Num.num array array) ->
 let s = simple_direct_sci_1024_roots eval_rule stages_roots steps threshold_qr threshold p in
  let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold mult_threshold s p in
   res.(0) ;;


(** {v largo_naive_complex_roots eval_rule stages_spectrum stages_roots steps threshold mult_threshold polynomial v} *)
let largo_naive_complex_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (mult_threshold:float) (p:float array array array) ->
 let r = naive_sci_1024_roots eval_rule stages_spectrum stages_roots steps threshold mult_threshold ( poly_complex_to_sci p ) in
  Array.map poly_sci_to_complex r ;;


(** {v largo_direct_complex_roots eval_rule stages_roots steps threshold_qr threshold mult_threshold polynomial v} *)
let largo_direct_complex_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (mult_threshold:float) (p:float array array array) ->
 let r = direct_sci_1024_roots eval_rule stages_roots steps threshold_qr threshold mult_threshold ( poly_complex_to_sci p ) in
  Array.map poly_sci_to_complex r ;;


(** {v simple_complex_shifted_roots eval_rule stages_spectrum stages_roots steps threshold shift polynomial v} *)
let rec simple_complex_shifted_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (shift:float array array) (p:float array array array) ->
 let d = poly_complex_deg p in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_complex_shifted_roots." ;
  match d with
  | 0. -> failwith "Nonzero constant polynomial in Reduc.simple_complex_shifted_roots."
  | 1. ->
   begin
    let s = complex_div p.(0) p.(1) in
     [| Matrix.matrix_float_opp s |]
   end
  | 2. -> complex_solve_degree_2 p.(2) p.(1) p.(0)
  | _ ->
   begin
    let m = complex_companion p in
     let s = clean_complex_shifted_spectrum stages_spectrum steps shift m in
      let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold 1. [| s.(0) |] p in
        let a = res.(0).(0).(0) in
         let q = ( poly_complex_div p ( poly_complex_x_a a ) ).(0) in
          Array.append [| a |] ( simple_complex_shifted_roots eval_rule stages_spectrum stages_roots steps threshold shift q )
   end ;;


(** {v simple_sci_1024_shifted_roots eval_rule stages_spectrum stages_roots steps threshold shift polynomial v} *)
let rec simple_sci_1024_shifted_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (shift:float array array) (p:Num.num array array) ->
 let d = poly_sci_deg p in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_sci_1024_shifted_roots." ;
  match d with
  | 0. -> failwith "Nonzero constant polynomial in Reduc.simple_sci_1024_shifted_roots."
  | 1. ->
   begin
    let s = Sci.div_1024 p.(0) p.(1) in
     [| Sci.opp s |]
   end
  | 2. -> Sci.solve_degree_2_1024 p.(2) p.(1) p.(0)
  | 3. -> Sci.solve_degree_3_1024 p.(3) p.(2) p.(1) p.(0)
  | 4. -> Sci.solve_degree_4_1024 p.(4) p.(3) p.(2) p.(1) p.(0)
  | _ ->
   begin
    let pp = poly_sci_to_complex p in
     let m = complex_companion pp in
      let s = clean_complex_shifted_spectrum stages_spectrum steps shift m in
       let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold 1. [| Sci.sci_of_complex s.(0) |] p in
        let a = res.(0).(0).(0) in
         let q = ( poly_sci_1024_div p ( poly_sci_x_a a ) ).(0) in
          Array.append [| a |] ( simple_sci_1024_shifted_roots eval_rule stages_spectrum stages_roots steps threshold shift q )
   end ;;


(** {v largo_simple_complex_shifted_roots eval_rule stages_spectrum stages_roots steps threshold shift polynomial v} *)
let rec largo_simple_complex_shifted_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (shift:float array array) (p:float array array array) ->
 let r = simple_sci_1024_shifted_roots eval_rule stages_spectrum stages_roots steps threshold shift ( poly_complex_to_sci p ) in
  poly_sci_to_complex r ;;


(** {v simple_direct_complex_shifted_roots eval_rule stages_roots steps threshold_qr threshold shift polynomial v} *)
let rec simple_direct_complex_shifted_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (shift:float array array) (p:float array array array) ->
 let d = poly_complex_deg p in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_direct_complex_shifted_roots." ;
  match d with
  | 0. -> failwith "Nonzero constant polynomial in Reduc.simple_direct_complex_shifted_roots."
  | 1. ->
   begin
    let s = complex_div p.(0) p.(1) in
     [| Matrix.matrix_float_opp s |]
   end
  | 2. -> complex_solve_degree_2 p.(2) p.(1) p.(0)
  | _ ->
   begin
    let m = complex_companion p in
     let s = direct_complex_shifted_spectrum threshold_qr threshold steps steps shift m in
      let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold 1. [| s.(0) |] p in
       let a = res.(0).(0).(0) in
        let q = ( poly_complex_div p ( poly_complex_x_a a ) ).(0) in
         Array.append [| a |] ( simple_direct_complex_shifted_roots eval_rule stages_roots steps threshold_qr threshold shift q )
   end ;;


(** {v simple_direct_sci_1024_shifted_roots eval_rule stages_roots steps threshold_qr threshold shift polynomial v} *)
let rec simple_direct_sci_1024_shifted_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (shift:float array array) (p:Num.num array array) ->
 let d = poly_sci_deg p in
  if d = neg_infinity then failwith "Null polynomial in Reduc.simple_direct_sci_1024_shifted_roots." ;
  match d with
  | 0. -> failwith "Nonzero constant polynomial in Reduc.simple_direct_sci_1024_shifted_roots."
  | 1. ->
   begin
    let s = Sci.div_1024 p.(0) p.(1) in
     [| Sci.opp s |]
   end
  | 2. -> Sci.solve_degree_2_1024 p.(2) p.(1) p.(0)
  | 3. -> Sci.solve_degree_3_1024 p.(3) p.(2) p.(1) p.(0)
  | 4. -> Sci.solve_degree_4_1024 p.(4) p.(3) p.(2) p.(1) p.(0)
  | _ ->
   begin
    let pp = poly_sci_to_complex p in
     let m = complex_companion pp in
      let s = direct_complex_shifted_spectrum threshold_qr threshold steps steps shift m in
       let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold 1. [| Sci.sci_of_complex s.(0) |] p in
        let a = res.(0).(0).(0) in
         let q = ( poly_sci_1024_div p ( poly_sci_x_a a ) ).(0) in
          Array.append [| a |] ( simple_direct_sci_1024_shifted_roots eval_rule stages_roots steps threshold_qr threshold shift q )
   end ;;


(** {v largo_simple_direct_complex_shifted_roots eval_rule stages_roots steps threshold_qr threshold shift polynomial v} *)
let largo_simple_direct_complex_shifted_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (shift:float array array) (p:float array array array) ->
 let r = simple_direct_sci_1024_shifted_roots eval_rule stages_roots steps threshold_qr threshold shift ( poly_complex_to_sci p ) in
  poly_sci_to_complex r ;;


(** {v complex_shifted_roots eval_rule stages_spectrum stages_roots steps threshold mult_threshold shift polynomial v} *)
let complex_shifted_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (mult_threshold:float) (shift:float array array) (p:float array array array) ->
 let q = poly_complex_simplify p in
  let s = simple_complex_shifted_roots eval_rule stages_spectrum stages_roots steps threshold shift q in
   let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold mult_threshold s p in
    res.(0) ;;


(** {v sci_1024_shifted_roots eval_rule stages_spectrum stages_roots steps threshold mult_threshold shift polynomial v} *)
let sci_1024_shifted_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (mult_threshold:float) (shift:float array array) (p:Num.num array array) ->
 let q = poly_sci_1024_simplify p in
  let s = simple_sci_1024_shifted_roots eval_rule stages_spectrum stages_roots steps threshold shift q in
   let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold mult_threshold s p in
    res.(0) ;;


(** {v direct_complex_shifted_roots eval_rule stages_roots steps threshold_qr threshold mult_threshold shift polynomial v} *)
let direct_complex_shifted_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (mult_threshold:float) (shift:float array array) (p:float array array array) ->
 let s = simple_direct_complex_shifted_roots eval_rule stages_roots steps threshold_qr threshold shift p in
  let res = poly_aitken_seki_rec_complex_tune_roots eval_rule stages_roots threshold mult_threshold s p in
   res.(0) ;;


(** {v direct_sci_1024_shifted_roots eval_rule stages_roots steps threshold_qr threshold mult_threshold shift polynomial v} *)
let direct_sci_1024_shifted_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (mult_threshold:float) (shift:float array array) (p:Num.num array array) ->
 let s = simple_direct_sci_1024_shifted_roots eval_rule stages_roots steps threshold_qr threshold shift p in
  let res = poly_aitken_seki_rec_1024_tune_roots eval_rule stages_roots threshold mult_threshold s p in
   res.(0) ;;


(** {v largo_complex_shifted_roots eval_rule stages_spectrum stages_roots steps threshold mult_threshold shift polynomial v} *)
let largo_complex_shifted_roots = fun eval_rule (stages_spectrum:int) (stages_roots:int) (steps:int) (threshold:float) (mult_threshold:float) (shift:float array array) (p:float array array array) ->
 let r = sci_1024_shifted_roots eval_rule stages_spectrum stages_roots steps threshold mult_threshold shift ( poly_complex_to_sci p ) in
  Array.map poly_sci_to_complex r ;;


(** {v largo_direct_complex_shifted_roots eval_rule stages_roots steps threshold_qr threshold mult_threshold shift polynomial v} *)
let largo_direct_complex_shifted_roots = fun eval_rule (stages_roots:int) (steps:int) (threshold_qr:float) (threshold:float) (mult_threshold:float) (shift:float array array) (p:float array array array) ->
 let r = direct_sci_1024_shifted_roots eval_rule stages_roots steps threshold_qr threshold mult_threshold shift ( poly_complex_to_sci p ) in
  Array.map poly_sci_to_complex r ;;


(** {v complex_roots threshold polynomial v} *)
let complex_roots = fun (threshold:float) (p:float array array array) ->
 let mr = poly_complex_mult
 and er = poly_complex_evaluate poly_complex_horner_comp in
  ( complex_maehly_roots mr er 2 0 100 threshold 1. p ).(0) ;;

(** {v sci_1024_roots threshold polynomial v} *)
let sci_1024_roots = fun (threshold:float) (p:Num.num array array) ->
 let mr = poly_sci_1024_mult
 and er = poly_sci_1024_evaluate poly_sci_1024_horner_comp in
  ( sci_1024_maehly_roots mr er 2 0 100 threshold 1. p ).(0) ;;

(** {v largo_complex_roots threshold polynomial v} *)
let largo_complex_roots = fun (threshold:float) (p:float array array array) ->
 let mr = poly_sci_1024_mult
 and er = poly_sci_1024_evaluate poly_sci_1024_horner_comp in
  let r = ( sci_1024_maehly_roots mr er 2 0 100 threshold 1. ( poly_complex_to_sci p ) ).(0) in
   poly_sci_to_complex r ;;


(** {v complex_diagonalization methode_ker threshold_qr threshold steps_qr steps_power methode_tune_roots char_pol matrix v} 
The matrix is supposed to be diagonalizable.
The first number of steps [steps_qr] is the one used in
the QR algorithm of Francis ; the second [steps] is the one used in the inverse iteration.
The method [methode_ker] may be the one used to search for a kernel with the singular value decomposition.
In the case when the matrix is not simple, the passage matrix has few precision.
The output is the same as for [direct_complex_diagonalization].

La sortie est la même que pour [direct_complex_diagonalization].
La matrice est supposée diagonalisable.
Le premier nombre maximal de pas [steps_qr] est celui utilisé pour l'algorithme QR de Francis ;
le deuxième [steps] est celui utilisé dans l'itération inverse.
La méthode [methode_ker] peut être celle utilisée pour rechercher un noyau avec la décomposition en valeurs singulières.
Dans le cas où la matrice n'est pas simple, la matrice de passage est peu précise. *)
let complex_diagonalization = fun methode_ker (threshold_qr:float) (threshold:float) (steps_qr:int) (steps:int) methode_tune_roots (char_pol:float array array array) (m:float array array) ->
 let res = complex_francis_schur_decomposition threshold_qr threshold steps_qr m
 and seuil = sqrt epsilon_float
 and nn = Array.length m
 and accu_integer = ref 0
 and accu_complex = ref complex_0 in
  let pp = Array.map clean_complex ( matrix_complex_extract_diag_to_poly res.(0) )
  and q = Matrix.float_transpose res.(1)
  and n = nn / 2 in
   let spectrum = Array.make n complex_0
   and ppp = methode_tune_roots pp char_pol
   and n_n = n - 1
   and trans_passage = Array.make_matrix nn nn 0. in
    let p = ppp.(0).(0) in
     for i = 0 to n_n do
      let candidate_value = p.(i) in
       let difference = Matrix.matrix_float_minus m ( scal_complex n n candidate_value ) in
        let test = Matrix.float_invertibility difference in
         if test then
          begin
           let mm = Matrix.clean_inv difference in
            let w = Matrix.float_normalized_iterate Matrix.vector_float_norm_2 steps mm q.(i) in
             trans_passage.(i) <- Matrix.vector_float_copy w ;
             trans_passage.( n + i ) <- vector_complex_i_times w ;
             let ww = Matrix.matrix_vector_float_prod mm w
             and z = complex_vector_to_complex_poly w in
              let zz = complex_vector_to_complex_poly ww in
               for j = 0 to n_n do
                let divisor = z.(j) in
                 if complex_module divisor > seuil then
                  begin
                   accu_complex := Matrix.matrix_float_plus !accu_complex ( Matrix.matrix_float_prod zz.(j) ( complex_inv divisor ) ) ;
                   accu_integer := succ !accu_integer ;
                  end ;
               done ;
               let coeff = clean_complex ( Matrix.matrix_float_scal_left_div ( float !accu_integer ) !accu_complex ) in
                accu_complex := complex_0 ;
                accu_integer := 0 ;
                spectrum.(i) <- Matrix.matrix_float_plus candidate_value ( complex_inv coeff ) ;
          end
         else
          begin
           spectrum.(i) <- candidate_value ;
           let k = methode_ker difference in
            let kk = Array.length k in
             let v = Matrix.vector_float_bal_random kk 10. in
              let w_w = ( Matrix.matrix_vector_float_prod ( Matrix.float_transpose k ) ( Matrix.vector_float_scal_mult ( 1. /. ( Matrix.vector_float_norm_2 v ) ) v ) ) in
               let w_coeff = 1. /. ( Matrix.vector_float_norm_2 w_w ) in
                let w = Matrix.vector_float_scal_mult w_coeff w_w in
                 trans_passage.(i) <- Matrix.vector_float_copy w ;
                 trans_passage.( n + i ) <- vector_complex_i_times w ;
          end ;
     done ;
     [| spectrum ; [| Matrix.float_transpose trans_passage |] ; [| trans_passage  |] |] ;; 


(** {v demultip roots_and_multiplicity_array v} Input : roots; multiplicities.
Output : roots counted with respect to their multiplicities.

Entrée : racines ; multiplicités.
Sortie : racines comptées avec leur multiplicité. *)
let demultip = function (r:float array array array array) ->
 let roots = r.(0)
 and multiplicities = r.(1)
 and s = ref [||] in
  let l = Array.length roots in
   for i = 0 to pred l do
    for j = 1 to int_of_float ( complex_real_part multiplicities.(i) ) do
     s := Array.append !s [| roots.(i) |]
    done ;
   done ;
   !s ;;


(** {v complex_det spectrum_method matrix v} *)
let complex_det = fun spectrum_methode (m:float array array) ->
 let p = spectrum_methode m in
  vector_complex_contraction p ;;

(** {v gauss_det spectrum_method matrix v} *)
let gauss_det = fun spectrum_methode (m:int array array) ->
 let mm = Array.map ( Array.map float ) m in
  let p = spectrum_methode mm in
   Matrix.matrix_float_round ( vector_complex_contraction p ) ;;


(** {v complex_krylov_reduction apply_rule decomposition_polynomial methode_diag matrix v} Output: conjugate matrix (in the eigenbasis), 
matrix whose columns are the respective eigenvectors of the diagonalizable part, inverse of the preceding matrix, 
candidate for the diagonal matrix, conjugate matrix of the nilpotent part (in the eigenbasis), matrix whose rows are the eigenvectors of the diagonalizable part.


Sortie : matrice conjuguée (dans la base propre), matrice dont les colonnes sont les vecteurs propres de la partie diagonalisable, 
matrice inverse de la précédente, candidat pour la matrice diagonale, matrice conjuguée de la partie nilpotente (dans la base propre), 
matrice dont les lignes sont les vecteurs propres de la partie diagonalisable. *)
let complex_krylov_reduction = fun apply_rule (decomposition_polynomial:float array array array) methode_diag (m:float array array) ->
 let dec = jordan_decomposition apply_rule decomposition_polynomial m in
  let d = dec.(0)
  and n = dec.(1) in
   let dd = methode_diag d in
    let p = dd.(1).(0)
    and ddd = diag_complex dd.(0) in
     let q = Matrix.clean_inv p in
      let nn = Matrix.matrix_float_triple_prod q n p in
       [| Matrix.matrix_float_plus nn ddd ; p ; q ; ddd ; nn ; dd.(2).(0) |] ;;













(** {C § § § } *)




end ;;

