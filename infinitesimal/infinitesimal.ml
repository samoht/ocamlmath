



module Infinitesimal = struct



(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module methods in order to:

- calculate derivatives of functions and samples,

- calculate Serret-Frenet frames and multi-curvatures of whatever parameterized or sampled curves,

- calculate curvature of surfaces embedded in R^3,

- interpolate samples,

- calculate zeros of functions,

- calculate critical points of functions,

- calculate integrals of functions and samples,

- calculate transforms of functions and samples,

- solve ordinary differential equations,

- calculate the exponential of a real square matrix.

Thanks to curryfication, the mathematician will be able to multiply and complexify the variants of calculus methods ad libitum and therefore to chose various tradeoffs between speed and accuracy.


{2 Conventions}


Vectors are rows of scalars ([float] or [int]), of type [float array] or [int array].

A (bidimensional) matrix is a row vector,
each element of which being a row of the matrix.
Matrices are of type [float array array] or [int array array].


{2 Warning}


When using local inversion, an exception probably means that some process 
of local inversion passed through a critical point.
The problem may arise during search of zeros or of critical points.
In this case, the start point has to be changed, or some method of zero finding 
should differentiate up to a lower order, or some parameters have to be tuned.
Sometimes diminishing the maximum number of steps might avoid the difficulty.


{2 Sources}


Most of the data and algorithms have been harvested on the internet.
Among the sites, one may quote [wikipedia.org] about the derivatives, the ordinary differential equations, the miscellaneous transforms
and [http://people.sc.fsu.edu/~jburkardt] and ACM algorithms [http://www.netlib.org/toms/index.html] about integration.
For the differential calculus, much comes from E. Ramis, C. Deschamps, J. Odoux : Cours de mathématiques spéciales tome 5, Masson, Paris 1981 ;
J Lelong_Ferrand, J.-M. Arnaudiès : Cours de mathématiques tome 3, Dunod, Paris.
For elliptic integrals, the definitions follow V. Prasolov, Y. Solovyev : Elliptic functions and elliptic integrals, AMS mathematical monographs, Providence 1997.
For the distributions, the curious reader will find a study of the links with the lambda-calculus in O. P. Misra : Distribution htoery in computer science, SCI TECH publishing, Houston 2002 ;
nevertheless we limit ourselves to the definitions of L. Schwartz : Théorie des distributions, Hermann, Paris 1966.


This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module des méthodes pour :

- calculer des dérivées de fonctions et d'échantillons,

- calculer des repères de Serret-Frenet et multicourbures sur des courbes qu'elles soient paramétrées ou échantillonnées,

- calculer des courbures de surfaces plongées dans R^3,

- interpoler des échantillons,

- calculer des points d'annulations de fonctions,

- calculer des points critiques de fonctions,

- calculer des intégrales de fonctions et d'échantillons,

- calculer des transformées de fonctions et d'échantillons,

- résoudre des équations différentielles ordinaires,

- calculer l'exponentielle d'une matrice carrée réelle.

Grâce à la curryfication, le mathématicien ou la mathématicienne pourra multiplier et complexifier les variantes de méthodes de calcul à volonté, et donc choisir différents compromis entre vitesse et précision.


{2 Conventions}


Les vecteurs sont des lignes de scalaires ([float] ou [int]), de type [float array] ou [int array].

Une matrice (bidimensionnelle) est un vecteur ligne 
dont chaque élément est une ligne de la matrice. 
Les matrices sont de type [float array array] ou [int array array].


{2 Avertissement}


En utilisant des inversions locales, une exception signifie probablement 
qu'un procédé d'inversion locale est passé par un point critique.
Le problème peut se produire aussi pendant la recherhe d'un zéro ou d'un point critique.
Dans ce cas, il faut changer le point de départ, ou prendre une méthode de recherche de zéro
qui dérive moins, ou ajuster divers paramètres.
Parfois, diminuer le nombre maximal de pas suffit à contourner la difficulté.


{2 Sources}


La plupart des données et algorithmes ont été glanés sur internet.
Parmi les sites, on peut citer [wikipedia.org] concernant la dérivation, les équations différentielles ordinaires,
les transformées diverses et [http://people.sc.fsu.edu/~jburkardt] concernant l'intégration.
Pour le calcul différentiel, beaucoup provient de E. Ramis, C. Deschamps, J. Odoux : Cours de mathématiques spéciales tome 5, Masson, Paris 1981 ; 
J Lelong_Ferrand, J.-M. Arnaudiès : Cours de mathématiques tome 3, Dunod, Paris .
Pour les intégrales elliptiques, les définitions suivent V. Prasolov, Y. Solovyev : Elliptic functions and elliptic integrals, AMS mathematical monographs, Providence 1997.
Pour les distributions, le lecteur curieux trouvera une étude des liens avec le lambda-calcul dans O. P. Misra : Distribution htoery in computer science, SCI TECH publishing, Houston 2002 ;
néanmoins nous nous limitons aux définitions de L. Schwartz : Théorie des distributions, Hermann, Paris 1966.


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




(** {C § } *)
(** 
{1 Constructions}
*)
(** {C  } *)



open Util ;;
open Matrix ;;



(** 
{2 Constantes}
{2 Constants}
*)
(** {C  } *)




let sqrt_of_2 = sqrt 2. ;;
let sqrt_of_3 = sqrt 3. ;;
let sqrt_of_5 = sqrt 5. ;;
let sqrt_of_6 = sqrt 6. ;;
let sqrt_of_15 = sqrt 15. ;;
let inv_sqrt_of_2 = 1. /. sqrt 2. ;;
let inv_sqrt_of_3 = 1. /. sqrt 3. ;;

let pi = atan2 0. (-1.) ;;
let halfpi = atan2 1. 0. ;;
let quarterpi = atan 1. ;;
let doublepi = 2. *. atan2 0. (-1.) ;;
let inv_sqrt_pi = 1. /. ( sqrt pi ) ;;
let inv_doublepi = 1. /. doublepi ;;
let inv_sqrt_doublepi = sqrt inv_doublepi ;;

let log_of_2 = log 2. ;;


(** 
{2 Fonctions}
{2 Functions}
*)
(** {C  } *)




(** {v heaviside_step float v} *)
let heaviside_step = function (x:float) ->
 let y = x -. abs_float x in
  match y with
  | 0. -> 1.
  | _ -> 0. ;;

(** {v unit_bowl_indic vector v} *)
let unit_bowl_indic = function (x:float array) ->
 heaviside_step ( 1. -. ( Matrix.vector_float_norm_2 x ) ) ;;

(** {v bowl_indic center radius vector v} *)
let bowl_indic = fun (x0:float array) (r:float) (x:float array) ->
 let z = Matrix.vector_float_minus x x0 in
  let y = Matrix.vector_float_scal_left_div r z in
   unit_bowl_indic ( y ) ;;

(** {v float_sign float v} *)
let float_sign = function (x:float) ->
 float ( compare x 0. ) ;;

(** {v int_sign float v} *)
let int_sign = function (x:int) ->
 compare x 0 ;;


(** {v low_stirling real v} *)
let low_stirling = function (x:float) ->
 let a = sqrt ( doublepi *. x )
 and b = x *. ( ( log x ) -. 1. ) in
  a *. exp b ;;

(** {v up_stirling real v} *)
let up_stirling = function (x:float) ->
 let a = sqrt ( doublepi *. x )
 and b = x *. ( ( log x ) -. 1. ) +. 1. /. ( 12. *. x ) in
  a *. exp b ;;

(** {v stirling_bis v} *)
let stirling_bis = function (x:float) ->
 let a = sqrt ( doublepi *. x )
 and b = x *. ( ( log x ) -. 1. )
 and y = 1. /. ( 12. *. x ) in
  let c = 1. +. y *. ( 1. +. y *. ( 0.5 -. y *. ( 139. +. y *. 571. /. 4. ) /. 30. ) ) in
   a *. c *. exp b ;;


(** {v half_unit_circle float v} *)
let half_unit_circle = function (x:float) -> sqrt ( 1. -. x *. x ) ;;

(** {v gauss_bell float v} *)
let gauss_bell = function (x:float) -> exp ( -. x *. x /. 2. ) ;;

(** {v sin_inv float v} *)
let sin_inv = function (x:float) -> match x with
 | 0. -> 0.
 | _ -> sin (1. /. x ) ;;

(** {v sinc float v} *)
let sinc = function (x:float) -> match x with
 | 0. -> 1.
 | _ -> ( sin x ) /. x ;;

(** {v nsinc float v} *)
let nsinc = function (x:float) -> match x with
 | 0. -> 1.
 | _ -> let xx = pi *. x in ( sin xx ) /. xx ;;


(** {v real_float_solve_degree_2 a b c v} Gives the two solutions of [a x ^ 2 + b x + c = 0].

Donne les deux solutions x de [a x ^ 2 + b x + c = 0]. *)
let real_float_solve_degree_2 = fun (a:float) (b:float) (c:float) ->
 let bb = b /. (-2.) in
  let delta = bb *. bb -. a *. c in
   if delta < 0. then failwith "Strictly negative discriminant in Infinitesimal.real_float_solve_degree_2."
   else let dd = sqrt delta in
    [| ( bb +. dd ) /. a ; ( bb -. dd ) /. a |] ;;


(** {v unit_circle angle v} *)
let unit_circle = function (x:float) -> [| cos x ; sin x |] ;;

(** {v ellipse a b t v} *)
let ellipse = fun (a:float) (b:float) (t:float) ->
 [| a *. ( cos t ) ; b *. ( sin t ) |] ;;

(** {v hyperbola a b t v} *)
let hyperbola = fun (a:float) (b:float) (t:float) ->
 [| a *. ( cosh t ) ; b *. ( sinh t ) |] ;;


(** {v polar_curve function angle v} *)
let polar_curve = fun (r:float -> float) (angle:float) ->
 [| ( r angle ) *. ( cos angle ) ; ( r angle ) *. ( sin angle ) |] ;;

(** {v polar_curve_bis function angle v} *)
let polar_curve_bis = fun (angle:float -> float) (r:float) ->
 [| r *. cos ( angle r ) ; r *. sin ( angle r ) |] ;;

(** {v rotating_frame function angle v} *)
let rotating_frame = fun (v:float -> float array) (angle:float) ->
 let c = cos angle
 and s = sin angle in
 [| ( v angle ).(0) *. c -. ( v angle ).(1) *. s ;
 ( v angle ).(0) *. s +. ( v angle ).(1) *. c |] ;;

(** {v cycloid rotating_radius drawing_radius time v} *)
let cycloid = fun (rotating_radius:float) (drawing_radius:float) (t:float) ->
 [| rotating_radius *. t -. drawing_radius *. sin t ; rotating_radius -. drawing_radius *. cos t |] ;;

(** {v trochoid big_radius signed_rotating_radius time v} *)
let trochoid = fun (a:float) (alpha:float) (t0:float) (b:float) (beta:float) (t1:float) (t:float) ->
 let v = Matrix.vector_float_scal_mult a ( unit_circle ( alpha *. ( t -. t0 ) ) )
 and w = Matrix.vector_float_scal_mult b ( unit_circle ( beta *. ( t -. t1 ) ) ) in
  Matrix.vector_float_plus v w ;;

(** {v epicycloid big_radius signed_rotating_radius time v} *)
let epicycloid = fun (r:float) (a:float) (t:float) ->
 let parameter = 1. +. r /. a in
  let v = Matrix.vector_float_scal_mult parameter ( unit_circle t )
  and w = unit_circle ( parameter *. t ) in
   Matrix.vector_float_scal_mult a ( Matrix.vector_float_minus v w ) ;;
  
(** {v cardioid time v} *)
 let cardioid = function (t:float) -> epicycloid 1. 1. t ;;

(** {v nephroid time v} *)
 let nephroid = function (t:float) -> epicycloid 1. 0.5 t ;;

(** {v hypocycloid_3 time v} *)
 let hypocycloid_3 = function (t:float) -> epicycloid 3. (-1.) t ;;

(** {v astroid time v} *)
 let astroid = function (t:float) -> epicycloid 1. (-0.25) t ;;

(** {v conic excentricity parameter time v} *)
let conic = fun (e:float) (p:float) (t:float) ->
 let r = p /. ( 1. -. e *. cos t ) in
  [| r *. cos t ; r *. sin t |] ;;

(** {v tractrix parameter time v} *)
let tractrix = fun (a:float) (t:float) ->
 let u = t /. a in
  [| t -. a *. tanh u ; a /. cosh u |] ;;

(** {v helicoid radius step float v} *)
let helicoid = fun (a:float) (b:float) (x:float) ->
 [| a *. cos x ; a *. sin x ; b *. x |] ;;

(** {v spherical_loxodromy radius parameter angle v} *)
let spherical_loxodromy = fun (radius:float) (parameter:float) (angle:float) ->
 let u = parameter *. angle in
  let r = radius /. ( cosh u ) in
   [| r *. cos angle ; r *. sin angle ; r *. tanh u |] ;;


(** {v revolution_surface function parameters v} *)
let revolution_surface = fun (f:float -> float) (v:float array) ->
 let angle = v.(0)
 and z = v.(1) in
  let r = f z in
   [| r *. ( cos angle ) ; r *. ( sin angle ) ; z |] ;;

(** {v pseudo_sphere parameters v} *)
let pseudo_sphere = fun (v:float array) ->
 let angle = v.(0)
 and t = v.(1) in
  let w = tractrix 1. t in
   let z = w.(0)
   and r = w.(1) in
    [| r *. cos angle ; r *. sin angle ; z |] ;;


(** {v cyl_coord_unit_sphere parameters v} *)
let cyl_coord_unit_sphere = fun (v:float array) ->
 let r = v.(0) and angle = v.(1) in
  [| r *. ( cos angle ) ; r *. ( sin angle ) ; sqrt ( 1. -. r *. r ) |] ;;

(** {v sph_coord_unit_sphere parameters v} *)
let sph_coord_unit_sphere = fun (v:float array) ->
 let site = v.(0)
 and azimut = v.(1) in
  let r = cos site in
   [| r *. cos azimut ; r *. sin azimut ; sin site |] ;;


(** {v cyl_coord_ellipsoid a b c parameters v} *)
let cyl_coord_ellipsoid = fun (a:float) (b:float) (c:float) (v:float array) ->
 Matrix.vector_float_coeff_prod [| a ; b ; c |] ( cyl_coord_unit_sphere v ) ;;

(** {v sph_coord_ellipsoid a b c parameters v} *)
let sph_coord_ellipsoid = fun (a:float) (b:float) (c:float) (v:float array) -> 
 Matrix.vector_float_coeff_prod [| a ; b ; c |] ( sph_coord_unit_sphere v ) ;;

(** {v graph_ellipsoid a b c parameters v} *)
let graph_ellipsoid = fun (a:float) (b:float) (c:float) (v:float array) ->
 let x = v.(0) /. a
 and y = v.(1) /. b in
  c *. sqrt ( 1. -. ( x *. x +. y *. y ) ) ;;



(** {v ln float v} *)
let ln = function (x:float) -> match x with
 | 0. -> [| 0. ; 0. |] ;
 | _ -> let y = x -. abs_float x in
  match y with
  | 0. -> [| log x ; 0. |] ;
  | _ -> [| log ( abs_float x ) ; pi |] ;;


(** {v log_bin float v} *)
let log_bin = function (x:float) ->
 ( log x ) /. log_of_2 ;;



(** The four following smoothing functions are symmetric. 
Their derivatives are null in zero up to the order equal to the number in the name minus 1.
They are flat at x=1.
They satisfy to the three following conditions.

f(0) = 0 ; f(0.5) = 0.5 ; |x| >= 1 ===> f( x ) = 0.

Les quatre fonctions régularisantes qui suivent sont paires.
Leurs dérivées s'annulent en zéro jusqu'à l'ordre égal au nombre du nom moins un.
Elles sont plates en x=1.
Elles satisfont aux trois conditions précédentes. *)

(** {C  } *)

(** {v float_decay_16 real v} *)
let float_decay_16 = fun (x:float) ->
 let y = ref ( 1.95470663435444503 *. ( abs_float x ) ) in
  y := !y *. !y ;
  y := !y *. !y ;
  y := !y *. !y ;
  y := !y *. !y ;
  exp ( -. !y ) ;;

(** {v float_decay_8 real v} *)
let float_decay_8 = fun (x:float) ->
 let xx = abs_float x in
  let y = ref ( 1.91043901319464093 *. xx ) in
   y := !y *. !y ;
   y := !y *. !y ;
   y := !y *. !y ;
   exp ( -. !y *. ( exp ( abs_float ( 4. *. xx -. 2. ) ) ) ) ;;

(** {v float_decay_4 real v} *)
let float_decay_4 = fun (x:float) ->
 let xx = abs_float x in
  let y = ref ( 1.82488861156805693 *. xx ) in
   y := !y *. !y ;
   y := !y *. !y ;
  exp ( -. !y *. ( exp ( abs_float ( xx *. ( 9. *. xx -. 4.5 ) ) ) ) ) ;;

(** {v float_decay_2 real v} *)
let float_decay_2 = fun (x:float) ->
 let xx = abs_float x in
  let y = ref ( 1.66510922231539538 *. xx ) in
   y := !y *. !y ;
   exp ( -. !y *. ( exp ( abs_float ( xx *. xx *. ( 12. *. xx -. 6. ) ) ) ) ) ;;

(** {v regular_truncature_right function cliff beach real v} *)
let regular_truncature_right = fun (f:float -> float) (a:float) (b:float) (x:float) ->
 if ( a >= b ) then failwith "Bad order in Infinitesimal.regular_truncature_right." ;
 if x <= a then 1.
 else if x >= b then 0.
  else f ( ( x -. a ) /. ( b -. a ) ) ;;

(** {v regular_truncature_left function cliff beach real v} *)
let regular_truncature_left = fun (f:float -> float) (a:float) (b:float) (x:float) ->
 if ( a >= b ) then failwith "Bad order in Infinitesimal.regular_truncature_left." ;
 if x <= a then 0.
 else if x >= b then 1.
  else f ( ( b -. x ) /. ( b -. a ) ) ;;

(** {v partition_of_1_table function left_foot left_cliff right_cliff right_foot v} *)
let partition_of_1_table = fun (f:float -> float) (a:float) (b:float) (c:float) (d:float) (x:float) ->
 if ( ( a >= b ) or ( b >= c ) or ( c >= d ) ) then failwith "Bad order in Infinitesimal.partition_of_1_table." ;
 let y = regular_truncature_left f a b x
 and z = regular_truncature_right f c d x in
  y *. z ;;

(** {v partition_of_1_gap function left_cliff left_bottom right_bottom right_cliff v} *)
let partition_of_1_gap = fun (f:float -> float) (a:float) (b:float) (c:float) (d:float) (x:float) ->
 if ( ( a >= b ) or ( b >= c ) or ( c >= d ) ) then failwith "Bad order in Infinitesimal.partition_of_1_gap." ;
 let y = regular_truncature_right f a b x
 and z = regular_truncature_left f c d x in
  y +. z ;;



(** {v float_polynomial_1 coefficients real v} *)
let float_polynomial_1 = fun (c:float array) (x:float) ->
 let deg = ( Array.length c ) - 1 in
  let accu = ref c.(deg) in
   for i = deg - 1 downto 0 do
    accu := !accu *. x +. c.(i) ;
   done ;
   !accu ;;

(** {v float_polynomial_2 coefficients x y v} The coefficients are given in a matrix.
The columns match the increasing powers of the second varaible y, the rows match the increasing powers of the first variable x. 

Les coefficients sont stockés dans une matrice. Les colonnes correspondent aux puissances croissantes de la deuxième variable y,
les lignes aux puissances croissantes de la première variable x. *)
let float_polynomial_2 = fun (c:float array array) (x:float) (y:float) ->
 let deg_x = ( Array.length c ) - 1 in
  let accu_row = ref 0.
  and last = ref 0
  and accu = ref 0. in
   for i = deg_x downto 0 do
    let row = c.(i) in
     last := ( Array.length row ) - 1 ;
     accu_row := row.(!last) ;
     for j = !last - 1 downto 0 do
      accu_row := !accu_row *. y +. row.(j)
     done ;
     accu := !accu *. x +. !accu_row ;
   done ;
   !accu ;;


(** {v float_rational_1 coefficients_up coefficients_down real v} *)
let float_rational_1 = fun (num:float array) (denom:float array) (x:float) ->
 let numer = float_polynomial_1 num x
 and denomin = float_polynomial_1 denom x in
  numer /. denomin ;;


(** {v float_rational_2 coefficients_up coefficients_down real v} The coefficients of the dividend and of the divisor are given
by matrices as in the case of polynomials in two variables.

Les coefficients du numérateur et du dénominateur sont donnés dans des matrices comme pour les polynômes à deux variables. *)
let float_rational_2 = fun (num:float array array) (denom:float array array) (x:float) (y:float) ->
 let numer = float_polynomial_2 num x y
 and denomin = float_polynomial_2 denom x y in
  numer /. denomin ;;




(** {C § } *)
(** 
{1 Dérivation}
*)
(** {C  } *)




(** {C  } *)
(** 
{2 Fonctions dérivées}
{2 Derivated functions}
*)
(** {C  } *)




(** {v float_approx_deriv step function float v} *)
let float_approx_deriv = fun (step:float) (f:float -> float) (x:float) ->
 ( ( f ( x +. step ) ) -. ( f x ) ) /. step ;;

(** {v float_richardson_binary_deriv degree step function float v} *)
let rec float_richardson_binary_deriv = fun (degree:int) (step:float) (f:float -> float) (x:float) ->
 match degree with
 | 0 -> float_approx_deriv step f x
 | 1 -> ( 4. *. f ( x +. step *. 0.5 ) -. 3. *. ( f x ) -. f ( x +. step ) ) /. step
 | 2 -> ( 32. *. f ( x +. step /. 4. ) -. 21. *. ( f x ) -. 12. *. f ( x +. step *. 0.5 ) +. f ( x +. step ) ) /. ( 3. *. step )
 | _ -> let coeff = 2. ** ( float degree )
  and oo = degree - 1 in
   ( coeff *. ( float_richardson_binary_deriv oo ( step *. 0.5 ) f x ) -. ( float_richardson_binary_deriv oo step f x ) ) /. ( coeff -. 1. ) ;;

(** {v compensated_float_richardson_binary_deriv degree step function float v} *)
let compensated_float_richardson_binary_deriv = fun (degree:int) (step:float) (f:float -> float) (x:float) ->
 let seq = Array.make ( succ degree ) 0. in
  for i = 0 to degree do
   seq.(i) <- float_richardson_binary_deriv i step f x ;
  done ;
  Matrix.float_approx seq ;;

(** {v float_richardson_deriv radix degree step function real v} *)
let rec float_richardson_deriv = fun (radix:float) (degree:int) (step:float) (f:float -> float) (x:float) ->
 match degree with
 | 0 -> float_approx_deriv step f x
 | 1 -> let coeff = radix *. radix in
  ( coeff *. f ( x +. step /. radix ) +. ( 1. -. coeff ) *. ( f x ) -. f ( x +. step ) ) /. ( step *. ( radix -. 1. ) )
 | _ -> let coeff = radix ** ( float degree )
  and oo = degree - 1 in
   ( coeff *. ( float_richardson_deriv radix oo ( step /. radix ) f x ) -. ( float_richardson_deriv radix oo step f x ) ) /. ( coeff -. 1. ) ;;

(** {v compensated_float_richardson_deriv accelerator radix degree step function float v} A convergence accelerator for real sequences
must be provided, like for instance [Matrix.float_approx].

Il faut fournir un accélérateur de convergence de suites réelles, comme par exemple [Matrix.float_approx]. *)
let compensated_float_richardson_deriv = fun accelerator (radix:float) (degree:int) (step:float) (f:float -> float) (x:float) ->
 let seq = Array.make ( succ degree ) 0. in
  for i = 0 to degree do
   seq.(i) <- float_richardson_deriv radix i step f x ;
  done ;
  accelerator seq ;;


(** {v vector_speed methode function real v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_speed = fun methode (f:float -> float array) (x:float) -> 
 let g = fun i y -> methode ( function z -> (f z).(i) ) y in 
  Array.mapi g ( Array.make ( Array.length (f x) ) x ) ;;


(** {v matrix_speed methode function real v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let matrix_speed = fun methode (f:float -> float array array) (x:float) ->
 let g = fun i y -> vector_speed methode ( function z -> (f z).(i) ) y in
  Array.mapi g ( Array.make ( Array.length (f x) ) x ) ;;


(** {v acceleration methode function float v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let acceleration = fun methode (f:float -> float array) (x:float) -> 
 let g = vector_speed methode f in
  vector_speed methode g x ;;


(** {v matrix_acceleration methode function real v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let matrix_acceleration = fun methode (f:float -> float array array) (x:float) ->
 let g = matrix_speed methode f in
  matrix_speed methode g x ;;


(** {v partial index vector float v} This function replaces the coordinate number [i] by the variable [x] inside of the vector [v].

Cette fonction remplace la coordonnée numéro [i] par la variable [x] dans le vecteur [v].*)
let partial = fun (i:int) (v:float array) (x:float) ->
 let l = Array.length v in
  let w = Array.make l 0. in
   for j = 0 to i - 1 do
    w.(j) <- v.(j) ;
   done ;
   w.(i) <- x ;
   for j = i + 1 to l - 1 do
    w.(j) <- v.(j) ;
   done ;
   w ;;


(** {v matrix_partial index vector float v} *)
let matrix_partial = fun (i:int) (j:int) (m:float array array) (x:float) ->
 let w = Matrix.matrix_float_copy m in
  w.(i).(j) <- x ;;


(** {v gradient methode function vector v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let gradient = fun methode (f:float array -> float) (v:float array) -> 
 let g = fun i y -> methode ( function z -> f ( partial i v z ) ) y in 
  Array.mapi g v ;;


(** {v tlm methode function vector v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let tlm = fun methode (f:float array -> float array) (v:float array) -> 
 let g = fun i y -> gradient methode ( function z -> (f z).(i) ) y in 
  Array.mapi g ( Array.make ( Array.length (f v) ) v ) ;;


(** {v div methode function vector v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let div = fun methode (f:float array -> float array) (v:float array) -> 
 let m = tlm methode f v
 and accu = ref 0. in
  let l = Array.length m
  and c = Array.length m.(0) in
   for i = 0 to ( min l c ) - 1 do
    accu := m.(i).(i) +. !accu
   done ;
   !accu ;;


(** {v det_jac methode function vector v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let det_jac = fun methode (f:float array -> float array) (v:float array) -> 
 let m = tlm methode f v in
  Matrix.float_slow_det m ;;


(** {v rot_curl methode function vector v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let rot_curl = fun methode (f:float array -> float array) (v:float array) -> 
 let l = Array.length v
 and c = Array.length (f v) in
  match [| l ; c |] with
  | [| 3 ; 3 |] -> let m = tlm methode f v in
   [| m.(2).(1) -. m.(1).(2) ; m.(0).(2) -. m.(2).(0) ; m.(1).(0) -. m.(0).(1) |]
  | _ -> failwith "Bad dimensions in Infinitesimal.rot_curl." ;;


(** {v hess methode function vector v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let hess = fun methode (f:float array -> float) (v:float array) -> 
 let g = gradient methode f in
  tlm methode g v ;;


(** {v float_jet methode order function v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let float_jet = fun methode (order:int) (f:float -> float) (x:float) ->
 let v = Array.make ( order + 1 ) f in
  for i = 1 to order do
   let g = function t -> methode v.(i - 1) t in
    v.(i) <- g
  done ;
  Array.map ( function h -> h x ) v ;;


(** {v float_poly_coeff methode order function v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let float_poly_coeff = fun methode (order:int) (f:float -> float) (x:float) ->
 let v = float_jet methode order f x
 and accu = ref 1. in
  for i = 2 to order do
   accu := !accu *. ( float i ) ;
   v.(i) <- v.(i) /. !accu ;
  done ;
  v ;;


(** {v vector_jet methode order function real v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_jet = fun methode (order:int) (f:float -> float array) (x:float) ->
 let v = Array.make ( order + 1 ) f in
  for i = 1 to order do
   let g = function y -> vector_speed methode v.(i - 1) y in
    v.(i) <- g
  done ;
  Array.map ( function h -> h x ) v ;;


(** {v matrix_jet methode order function real v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let matrix_jet = fun methode (order:int) (f:float -> float array array) (x:float) ->
 let v = Array.make ( order + 1 ) f in
  for i = 1 to order do
   let g = function y -> matrix_speed methode v.(i - 1) y in
    v.(i) <- g
  done ;
  Array.map ( function h -> h x ) v ;;


(** {v graph_curvature methode function real v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let graph_curvature = fun methode (f:float -> float) (x:float) ->
 let g = methode f in
  let h = methode g
  and y = g x in
   let z = h x
   and u = 1. +. y *. y in
    let v = sqrt u in
     z /. (u *. v ) ;;


(** {v curvature_2 methode function real v} The fonction must take its values in R^2. 
The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La fonction doit être à valeurs dans R^2.*)
let curvature_2 = fun methode (f:float -> float array) (x:float) ->
 let g = vector_speed methode f in
  let h = vector_speed methode g
  and v = g x in
   let a = h x
   and v0 = v.(0)
   and v1 = v.(1) in
    let u = v0 *. v0 +. v1 *. v1 in
     ( v0 *. a.(1) -. v1 *. a.(0) ) /. ( u *. ( sqrt u ) ) ;;


(** {v developpee_2 methode function real v} The fonction must take its values in R^2. 
The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La fonction doit être à valeurs dans R^2.*)
let developpee_2 = fun methode (f:float -> float array) (x:float) ->
 let g = vector_speed methode f in
  let h = vector_speed methode g
  and v = g x in
   let a = h x
   and v0 = v.(0)
   and v1 = v.(1) in
    let u = v0 *. v0 +. v1 *. v1
    and a0 = a.(0)
    and a1 = a.(1) in
     let p = a0 *. v0 +. a1 *. v1 
     and uu = 1. /. u in
      let kappa = ( v0 *. a1 -. v1 *. a0 ) *. ( uu *. ( sqrt uu ) ) in
       let r = 1. /. kappa in
        let n0 = ( a0 -. p *. v0 /. u ) *. r *. uu
        and n1 = ( a1 -. p *. v1 /. u ) *. r *. uu
        and position = f x in
         [| position.(0) +. r *. n0 ; position.(1) +. r *. n1 |] ;;


(** {v parallel_arc_2 methode function real v} The fonction must take its values in R^2. 
The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La fonction doit être à valeurs dans R^2.*)
let parallel_arc_2 = fun methode (f:float -> float array) (parameter:float) (x:float) ->
 let g = vector_speed methode f in
  let h = vector_speed methode g
  and v = g x in
   let a = h x
   and v0 = v.(0)
   and v1 = v.(1) in
    let u = v0 *. v0 +. v1 *. v1
    and a0 = a.(0)
    and a1 = a.(1) in
     let p = a0 *. v0 +. a1 *. v1 
     and uu = 1. /. u in
      let kappa = ( v0 *. a1 -. v1 *. a0 ) *. ( uu *. ( sqrt uu ) ) in
       let r = 1. /. kappa in
        let n0 = ( a0 -. p *. v0 /. u ) *. r *. uu
        and n1 = ( a1 -. p *. v1 /. u ) *. r *. uu
        and position = f x in
         [| position.(0) +. parameter *. n0 ; position.(1) +. parameter *. n1 |] ;;


(** {v curvature methode function real v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let curvature = fun methode (f:float -> float array) (x:float) ->
 let g = vector_speed methode f in
  let a = vector_speed methode g x
  and v = g x in
   let p = Matrix.vector_float_scal_prod a v
   and nn = Matrix.vector_float_square_norm_2 v in
    let z = Matrix.vector_float_scal_mult nn a
    and zz = Matrix.vector_float_scal_mult p v in
     ( Matrix.vector_float_norm_2 ( Matrix.vector_float_minus z zz ) ) /. ( nn *. nn ) ;;


(** {v serret_frenet_3 methode function real v} The fonction must take its values in R^3. 
The (unidimensional) derivating method must contain the parameters, including the step.
The output gives the (curvature;torsion) vector, then the tangent vector,
then the normal vector, then the binormal vector.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La fonction doit être à valeurs dans R^3. La sortie comporte le vecteur (courbure;torsion),
puis le vecteur tangent, puis le vecteur normal, puis le vecteur binormal. *)
let serret_frenet_3 = fun methode (f:float -> float array) (x:float) ->
 let g = vector_speed methode f in
  let h = vector_speed methode g
  and v = g x in
   let k = vector_speed methode h
   and a = h x
   and n = Matrix.vector_float_norm_2 v
   and nn = Matrix.vector_float_square_norm_2 v in
    let dertier = k x
    and tangent = Matrix.vector_float_scal_left_div n v
    and product = Util.vector_float_prod_3 v a in
     let p = Matrix.vector_float_norm_2 product in
      let binormal = Matrix.vector_float_scal_left_div p product
      and curvature = p /. ( n *. nn ) in
       let normal = Util.vector_float_prod_3 binormal tangent
       and torsion = ( Matrix.vector_float_scal_prod binormal dertier ) /. p in
        [| [| curvature ; torsion |] ; tangent ; normal ; binormal |] ;;


(** {v curvature_center_3 methode function real v} The fonction must take its values in R^3. 
The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La fonction prend ses valeurs dans R^3. *)
let curvature_center_3 = fun methode (f:float -> float array) (x:float) ->
 let resultat = serret_frenet_3 methode f x in
  Matrix.vector_float_plus ( f x ) ( Matrix.vector_float_scal_mult ( 1. /. resultat.(0).(0) ) resultat.(2) ) ;;


(** {v parallel_arc_3 methode function parameter real v} The fonction must take its values in R^3. 
The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La fonction prend ses valeurs dans R^3. *)
let parallel_arc_3 = fun methode (f:float -> float array) (parameter:float) (x:float) ->
 let resultat = serret_frenet_3 methode f x in
  Matrix.vector_float_plus ( f x ) ( Matrix.vector_float_scal_mult parameter resultat.(2) ) ;;


(** {v serret_frenet methode function real v} The (unidimensional) derivating method must contain the parameters, including the step.
The output gives the multi-curvature vector in a one-row matrix, then the Serret-Frenet frame presented row-by-row, 
then the product of all the curvatures in a matrix reduced to a scalar.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La sortie comporte le vecteur multicourbure dans une matrice uniligne, puis le repère de Serret-Frenet ligne par ligne,
puis le produit de toutes les courbures dans une matrice réduite à un scalaire. *)
let serret_frenet = fun methode (f:float -> float array) (x:float) ->
 let dim = Array.length ( f x )
 and accu = ref 1. in
  let m = vector_jet methode dim f x
  and n = dim - 1 in
   let mm = Matrix.float_sub_matrix m 1 dim 0 n in
    let frame = Matrix.float_trans_orthonormalize mm
    and curvature = Array.make n 0. in
     let celer = Matrix.vector_float_scal_prod mm.(0) frame.(0) in
      curvature.(0) <- ( Matrix.vector_float_scal_prod mm.(1) frame.(1) ) /. ( celer *. celer ) ;
      accu := curvature.(0) ;
      for i = 2 to n do
       curvature.( i - 1 ) <- ( Matrix.vector_float_scal_prod mm.(i) frame.(i) ) /. ( !accu *. ( celer ** ( float ( i + 1 ) ) ) ) ;
       accu := !accu *. curvature.( i - 1 ) ;
      done ;
      [| [| curvature |] ; frame ; [| [| !accu |] |] |] ;;


(** {v curvature_center methode function real v} The fonction must take its values in R^2. 
The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La fonction doit être à valeurs dans R^2.*)
let curvature_center = fun methode (f:float -> float array) (x:float) ->
 let g = vector_speed methode f in
  let a = vector_speed methode g x
  and v = g x in
   let p = Matrix.vector_float_scal_prod a v
   and nn = Matrix.vector_float_square_norm_2 v in
    let z = Matrix.vector_float_scal_mult nn a
    and zz = Matrix.vector_float_scal_mult p v
    and u = 1. /. nn in
     let w = Matrix.vector_float_scal_mult ( u *. u ) ( Matrix.vector_float_minus z zz ) in
      let kappa = Matrix.vector_float_norm_2 w in
       let ww = Matrix.vector_float_scal_mult ( 1. /. ( kappa *. kappa ) ) w in
        Matrix.vector_float_plus ( f x ) ww ;;


(** {v parallel_arc methode function parameter real v} The fonction must take its values in R^2. 
The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La fonction doit être à valeurs dans R^2.*)
let parallel_arc = fun methode (f:float -> float array) (parameter:float) (x:float) ->
 let g = vector_speed methode f in
  let a = vector_speed methode g x
  and v = g x in
   let p = Matrix.vector_float_scal_prod a v
   and nn = Matrix.vector_float_square_norm_2 v in
    let z = Matrix.vector_float_scal_mult nn a
    and zz = Matrix.vector_float_scal_mult p v
    and u = 1. /. nn in
     let w = Matrix.vector_float_scal_mult ( u *. u ) ( Matrix.vector_float_minus z zz ) in
      let kappa = Matrix.vector_float_norm_2 w in
       let ww = Matrix.vector_float_scal_mult ( parameter /. kappa ) w in
        Matrix.vector_float_plus ( f x ) ww ;;



(** {v fond_form_I methode function position vector1 vector2 v} *)
let fond_form_I = fun methode (f:float array -> float array) (position:float array) (vector1:float array) (vector2:float array) ->
 let a = tlm methode f position in
  let w1 = Matrix.matrix_vector_float_prod a vector1
  and w2 = Matrix.matrix_vector_float_prod a vector2 in
   Matrix.vector_float_scal_prod w1 w2 ;;


(** {v surface_normal_3 methode f position v} The function must take its varaiables in R^2 and its values in R^3.

La fonction doit aller de R^2 dans R^3. *)
let surface_normal_3 = fun methode (f:float array -> float array) (position:float array) ->
 let a = tlm methode f position in
  let b = Matrix.float_transpose a in
   let v = Util.vector_float_prod_3 b.(0) b.(1) in
    let l = Matrix.vector_float_norm_2 v in
     Matrix.vector_float_scal_left_div l v ;;


(** {v surface_area_element_3 methode f position v} The function must take its varaiables in R^2 and its values in R^3.

La fonction doit aller de R^2 dans R^3. *)
let surface_area_element_3 = fun methode (f:float array -> float array) (position:float array) ->
 let a = tlm methode f position in
  let b = Matrix.float_transpose a in
   let v = Util.vector_float_prod_3 b.(0) b.(1) in
    Matrix.vector_float_norm_2 v ;;


(** {v surface_area_vector_3 methode f position v} The function must take its varaiables in R^2 and its values in R^3.

La fonction doit aller de R^2 dans R^3. *)
let surface_area_vector_3 = fun methode (f:float array -> float array) (position:float array) ->
 let a = tlm methode f position in
  let b = Matrix.float_transpose a in
   Util.vector_float_prod_3 b.(0) b.(1) ;;


(** {v surface_weingarten_3 methode f position v} The function must take its varaiables in R^2 and its values in R^3.

La fonction doit aller de R^2 dans R^3. *)
let surface_weingarten_3 = fun methode (f:float array -> float array) (position:float array) ->
 let n = surface_normal_3 methode f in
  let w = tlm methode n position
  and ww = tlm methode f position in
   let www = Matrix.float_transpose ww in
    let w_ww = Matrix.float_trans_orthonormalize www in
     let w_w = Matrix.matrix_float_prod w_ww ww in
      let w_inv = Matrix.clean_inv w_w in
       Matrix.matrix_float_triple_prod w_ww w w_inv ;;


(** {v surface_fond_form_II_3 methode function position vector1 vector2 v} *)
let surface_fond_form_II_3 = fun methode (f:float array -> float array) (position:float array) (vector1:float array) (vector2:float array) ->
 fond_form_I methode f position vector1 ( Matrix.matrix_vector_float_prod (surface_weingarten_3 methode f position ) vector2 ) ;;


(** {v surface_principal_curvatures_3 methode function position v} *)
let surface_principal_curvatures_3 = fun methode (f:float array -> float array) (position:float array) ->
 let s = surface_weingarten_3 methode f position in
  let a = s.(0).(0)
  and b = 0.5 *. ( s.(0).(1) +. s.(1).(0) )
  and d = s.(1).(1) in
   real_float_solve_degree_2 1. ( -. ( a +. d ) ) ( a *. d -. b *. b ) ;;

(** {v surface_principal_curvatures_3_bis threshold max_steps methode function position v} *)
let surface_principal_curvatures_3_bis = fun (threshold:float) (max_steps:int) methode (f:float array -> float array) (position:float array) ->
 let a = surface_weingarten_3 methode f position in
  let s = Matrix.float_sym a in
   ( Matrix.sym_float_tune_reduc threshold max_steps s ).(0).(0) ;;


(** {v surface_ombilic_deviation_3 methode function position v} *)
let surface_ombilic_deviation_3 = fun methode (f:float array -> float array) (position:float array) ->
 let k = surface_principal_curvatures_3 methode f position in
  let a = k.(0)
  and b = k.(1) in
   let z = ( abs_float a ) +. ( abs_float b ) in
    if z = 0. then 0. else ( a -. b ) /. z ;;

(** {v surface_ombilic_deviation_3_bis threshold max_steps methode function position v} *)
let surface_ombilic_deviation_3_bis = fun (threshold:float) (max_steps:int) methode (f:float array -> float array) (position:float array) ->
 let k = surface_principal_curvatures_3_bis threshold max_steps methode f position in
  let a = k.(0)
  and b = k.(1) in
   let z = ( abs_float a ) +. ( abs_float b ) in
    if z = 0. then 0. else ( a -. b ) /. z ;;


(** {v surface_mean_curvature_3 methode function position v} *)
let surface_mean_curvature_3 = fun methode (f:float array -> float array) (position:float array) ->
 let s = surface_weingarten_3 methode f position in
  0.5 *. ( s.(0).(0) +. s.(1).(1) ) ;;

(** {v surface_mean_curvature_3_bis threshold max_steps methode function position v} *)
let surface_mean_curvature_3_bis = fun (threshold:float) (max_steps:int) methode (f:float array -> float array) (position:float array) ->
 let k = surface_principal_curvatures_3_bis threshold max_steps methode f position in
  0.5 *. ( k.(0) +. k.(1) ) ;;


(** {v surface_gauss_curvature_3 methode function position v} *)
let surface_gauss_curvature_3 = fun methode (f:float array -> float array) (position:float array) ->
 let s = surface_weingarten_3 methode f position in
  s.(0).(0) *. s.(1).(1) -. s.(0).(1) *. s.(1).(0) ;;

(** {v surface_gauss_curvature_3_bis threshold max_steps methode function position v} *)
let surface_gauss_curvature_3_bis = fun (threshold:float) (max_steps:int) methode (f:float array -> float array) (position:float array) ->
 let k = surface_principal_curvatures_3_bis threshold max_steps methode f position in
  k.(0) *. k.(1) ;;


(** {v graph_surface_3 function position v} The function must take its variables in R^2 and its values in R.

La fonction doit aller de R^2 dans R. *)
let graph_surface_3 = fun (f:float array -> float) (v:float array) ->
 Array.append v [| f v |] ;;


(** {v graph_principal_curvatures_3 function position v} The function must take its variables in R^2 and its values in R.

La fonction doit aller de R^2 dans R. *)
let graph_principal_curvatures_3 = fun methode (f:float array -> float) (position:float array) ->
 surface_principal_curvatures_3 methode ( graph_surface_3 f ) position ;;


(** {v graph_principal_curvatures_3_bis threshold max_steps function position v} The function must take its variables in R^2 and its values in R.

La fonction doit aller de R^2 dans R. *)
let graph_principal_curvatures_3_bis = fun (threshold:float) (max_steps:int) methode (f:float array -> float) (position:float array) ->
 surface_principal_curvatures_3_bis threshold max_steps methode ( graph_surface_3 f ) position ;;


(** {v graph_mean_curvature_3 function position v} The function must take its variables in R^2 and its values in R.

La fonction doit aller de R^2 dans R. *)
let graph_mean_curvature_3 = fun methode (f:float array -> float) (position:float array) ->
 surface_mean_curvature_3 methode ( graph_surface_3 f ) position ;;


(** {v graph_mean_curvature_3_bis threshold max_steps function position v} The function must take its variables in R^2 and its values in R.

La fonction doit aller de R^2 dans R. *)
let graph_mean_curvature_3_bis = fun methode (threshold:float) (max_steps:int) (f:float array -> float) (position:float array) ->
 surface_mean_curvature_3_bis threshold max_steps methode ( graph_surface_3 f ) position ;;


(** {v graph_gauss_curvature_3 function position v} The function must take its variables in R^2 and its values in R.

La fonction doit aller de R^2 dans R. *)
let graph_gauss_curvature_3 = fun methode (f:float array -> float) (position:float array) ->
 surface_gauss_curvature_3 methode ( graph_surface_3 f ) position ;;


(** {v graph_gauss_curvature_3_bis threshold max_steps function position v} The function must take its variables in R^2 and its values in R.

La fonction doit aller de R^2 dans R. *)
let graph_gauss_curvature_3_bis = fun (threshold:float) (max_steps:int) methode (f:float array -> float) (position:float array) ->
 surface_gauss_curvature_3_bis threshold max_steps methode ( graph_surface_3 f ) position ;;




(** {C § } *)
(** 
{2 Dérivées discrètes}
{2 Discrete derivatives}
*)
(** {C  } *)




(** {v float_discrete_diff scale vector v} *)
let float_discrete_diff = fun scale (v:float array) ->
 let n = Array.length v
 and inv_scale = 1. /. scale in
  let w = Array.make ( n - 1 ) 0. in
    for i = 0 to ( n - 2 ) do
     w.(i) <- ( v.( i + 1 ) -. v.(i) ) *. inv_scale ;
    done ;
    w ;;


(** {v float_discrete_richardson_binary_diff degree scale vector v} The maximum degree taken into account is two.

Le degré plafonne à 2. *)
let float_discrete_richardson_binary_diff = fun (degree:int) (scale:float) (v:float array) ->
 let n = Array.length v in
  match degree with
  | 0 -> float_discrete_diff scale v
  | 1 -> let w = Array.make ( n - 2 ) 0.
   and inv_doublescale = 1. /. ( 2. *. scale ) in
    for i = 0 to ( n - 3 ) do
     w.(i) <- ( 4. *. v.( i + 1 ) -. 3. *. v.(i) -. v.( i + 2 ) ) *. inv_doublescale ;
    done ; 
    w
  | _ -> let w = Array.make ( n - 4 ) 0.
   and inv_otherscale = 1. /. ( 12. *. scale ) in
    for i = 0 to ( n - 5 ) do
     w.(i) <- ( 32. *. v.( i + 1 ) -. 21. *. v.(i) -. 12. *. v.( i + 2 ) +. v.( i + 4 ) ) *. inv_otherscale ;
    done ; 
    w ;;


(** {v float_discrete_richardson_diff radix scale vector v} The degree is equal to one.

Le degré vaut 1. *)
let float_discrete_richardson_diff = fun (radix:int) (scale:float) (v:float array) ->
 let n = Array.length v
 and r = float radix in
  let w = Array.make ( n - radix ) 0.
  and rr = r *. r
  and inv_otherscale = 1. /. ( r *. ( r -. 1. ) *. scale ) in
    for i = 0 to ( n - radix - 1 ) do
     w.(i) <- ( rr *. v.( i + 1 ) -. ( rr -. 1. ) *. v.(i) -. v.( i + radix ) ) *. inv_otherscale ;
    done ; 
    w ;;


(** {v mean_float_discrete_diff scale vector v} *)
let mean_float_discrete_diff = fun scale (v:float array) ->
 let n = Array.length v
 and inv_scale = 1. /. scale in
  let w = Array.make n 0.
  and nn = n - 1 in
   let inv_otherscale = 1. /. ( ( float nn ) *. scale ) in
    w.(0) <- ( v.(1) -. v.(0) ) *. inv_scale ;
    for i = 1 to ( n - 2 ) do
     w.(i) <- ( ( float ( nn - i ) ) *. v.( i + 1 ) +. ( float ( 2 * i - nn ) ) *. v.(i) -. ( float i ) *. v.( i - 1 ) ) *. inv_otherscale ;
    done ;
    w.( nn ) <- ( v.( nn ) -. v.( nn - 1 ) ) *. inv_scale ;
    w ;;


(** {v mean_float_discrete_richardson_binary_diff scale vector v} The degree is equal to 1.

Le degré vaut 1. *)
let mean_float_discrete_richardson_binary_diff = fun (scale:float) (v:float array) ->
 let n = Array.length v
 and inv_doublescale = 1. /. ( 2. *. scale )
 and inv_quadscale = 1. /. ( 4. *. scale )
 and inv_otherscale = 1. /. ( 8. *. scale ) in
  let nn = n - 1
  and w = Array.make n 0. in
   w.(0) <- ( 4. *. v.(1) -. 3. *. v.(0) -. v.(2) ) *. inv_doublescale ;
   w.(1) <- ( (-3.) *. v.(0) +. v.(1) +. 3. *. v.(2) -. v.(3) ) *. inv_quadscale ;
   for i = 2 to ( n - 3 ) do
    w.(i) <- ( (-3.) *. v.( i - 2 ) -. 2. *. v.( i - 1 ) +. 4. *. v.(i) +. 2. *. v.( i + 1 ) -. v.( i + 2 ) ) *. inv_otherscale ;
   done ; 
   w.( n - 2 ) <- ( (-3.) *. v.( nn - 3 ) +. v.( nn - 2 ) +. 3. *. v.( nn - 1 ) -. v.(nn) ) *. inv_quadscale ;
   w.( nn ) <- ( 4. *. v.( nn - 1 ) -. 3. *. v.( nn - 2 ) -. v.( nn ) ) *. inv_doublescale ;
   w ;;


(** {v discrete_trans_vector_speed methode scale position_matrix v} The data must be presented by coordinates.

Les données doivent être présentées coordonnées par coordonnées. *)
let discrete_trans_vector_speed = fun methode (scale:float) (position:float array array) ->
 let d = Array.length position in
  let vitesse = Array.make_matrix d 1 0. in
   for i = 0 to d - 1 do
    vitesse.(i) <- methode scale position.(i)
   done ;
   vitesse ;;


(** {v discrete_vector_speed methode scale position_matrix v} *)
let discrete_vector_speed = fun methode (scale:float) (position:float array array) ->
 Matrix.float_transpose ( discrete_trans_vector_speed methode scale ( Matrix.float_transpose position ) ) ;;


(** {v discrete_trans_acceleration methode scale position_matrix v} The data must be presented by coordinates.

Les données doivent être présentées coordonnées par coordonnées. *)
let discrete_trans_acceleration = fun methode (scale:float) (position:float array array) ->
 let d = Array.length position in
  let vitesse = Array.make_matrix d 1 0.
  and acceleration = Array.make_matrix d 1 0. in
   for i = 0 to d - 1 do
    begin 
     let w = methode scale position.(i) in
      acceleration.(i) <- methode scale w ;
      vitesse.(i) <- w ;
    end ;
   done ;
   acceleration ;;


(** {v discrete_acceleration methode scale position_matrix v} *)
let discrete_acceleration = fun methode (scale:float) (position:float array array) ->
 Matrix.float_transpose ( discrete_trans_acceleration methode scale ( Matrix.float_transpose position ) ) ;;


(** {v discrete_trans_jet methode order scale position_matrix v} The data must be presented by coordinates.

Les données doivent être présentées coordonnées par coordonnées. *)
let discrete_trans_jet = fun methode (order:int) (scale:float) (position:float array array) ->
 let d = Array.length position in
  let jet = Array.make_matrix ( order + 1 ) d [| 0. |] in
   for i = 0 to d - 1 do
    begin
     let w = ref position.(i) in
      jet.(0).(i) <- !w ;
      for o = 1 to order do 
       begin
        w := methode scale !w ;
        jet.(o).(i) <- !w ;
       end
      done ;
    end
   done ;
   jet ;;


(** {v discrete_jet methode order scale position_matrix v} *)
let discrete_jet = fun methode (order:int) (scale:float) (position:float array array) ->
 Util.transpose ( Array.map Matrix.float_transpose ( discrete_trans_jet methode order scale ( Matrix.float_transpose position ) ) ) ;;


(** {v discrete_graph_curvature methode scale vector v} *)
let discrete_graph_curvature = fun methode (scale:float) (position:float array) ->
 let first = methode scale position in
  let second = methode scale first in
   let n = Array.length second in
    let curvature = Array.make n 0. in
     for i = 0 to n - 1 do
      let coeff = first.(i) in
       let coefficient = 1. +. coeff *. coeff in
        curvature.(i) <- second.(i) /. ( coefficient *. ( sqrt coefficient ) )
     done ;
     curvature ;;


(** {v discrete_trans_curvature_2 methode scale position_matrix v} The data must be presented by coordinates.

Les données doivent être présentées coordonnées par coordonnées. *)
let discrete_trans_curvature_2 = fun methode (scale:float) (position:float array array) ->
 let second_jet = discrete_trans_jet methode 2 scale position in
  let first = second_jet.(1)
  and second = second_jet.(2) in
   let xdot = first.(0)
   and ydot = first.(1)
   and xsec = second.(0)
   and ysec = second.(1) in
    let n = Array.length xsec in
     let curvature = Array.make n 0. in
      for i = 0 to n - 1 do
       let v0 = xdot.(i)
       and v1 = ydot.(i)
       and a0 = xsec.(i)
       and a1 = ysec.(i) in
        let coeff = v0 *. v0 +. v1 *. v1 in
         curvature.(i) <- ( v0 *. a1 -. v1 *. a0 ) /. ( coeff *. ( sqrt coeff ) )
      done ;
      curvature ;;


(** {v discrete_curvature_2 methode scale position_matrix v} *)
let discrete_curvature_2 = fun methode (scale:float) (position:float array array) ->
 discrete_trans_curvature_2 methode scale ( Matrix.float_transpose position ) ;;


(** {v discrete_curvature methode scale position_matrix v} *)
let discrete_curvature = fun methode (scale:float) (position:float array array) ->
 let second_jet = discrete_jet methode 2 scale position in
  let first = second_jet.(1)
  and second = second_jet.(2) in
   let nn = Array.map Matrix.vector_float_square_norm_2 first
   and size = Array.length second in
    let p = Array.make size 0.
    and curvature = Array.make size 0. in
     for i = 0 to size - 1 do
      p.(i) <- Matrix.vector_float_scal_prod second.(i) first.(i) ;
      let z = Matrix.vector_float_scal_mult nn.(i) second.(i) ;
      and zz = Matrix.vector_float_scal_mult p.(i) first.(i) in
       curvature.(i) <- ( Matrix.vector_float_norm_2 ( Matrix.vector_float_minus z zz ) ) /. ( nn.(i) *. nn.(i) ) ;
     done ;
     curvature ;;



(** {v discrete_trans_curvature methode scale position_matrix v} The data must be presented by coordinates.

Les données doivent être présentées coordonnées par coordonnées. *)
let discrete_trans_curvature = fun methode (scale:float) (position:float array array) ->
 discrete_curvature methode scale ( Matrix.float_transpose position ) ;;


(** {v discrete_trans_serret_frenet methode scale position_matrix v} The samples at the input must be presented by coordinates.
The output gives the samples of the multi-curvature vector, then the samples of the Serret-Frenet frame, 
then the samples of the product of all the curvatures.

Les échantillons doivent être entrés coordonnée par coordonnée.
La sortie comporte les échantillons du vecteur multicourbure, puis les échantillons du repère de Serret-Frenet,
puis les échantillons du produit de toutes les courbures. *)
let discrete_trans_serret_frenet = fun methode (scale:float) (position:float array array) ->
 let dim = Array.length position in
  let dim_jet = discrete_trans_jet methode dim scale position
  and n = dim - 1 in
   let size = Array.length dim_jet.(dim).(n) in
    let accu = Array.make size 1.
    and celer = Array.make size 0.
    and curvature = Array.make_matrix size n 0.
    and frame = Array.make size ( Array.make_matrix dim dim 0. ) in
     let information = Array.sub dim_jet 1 dim in
      let m = Array.map Matrix.float_transpose information in
       let mm = Util.transpose m in
        for i = 0 to size - 1 do
         let repere = mm.(i) in
         frame.(i) <- Matrix.float_trans_orthonormalize repere ;
         celer.(i) <- Matrix.vector_float_scal_prod repere.(0) frame.(i).(0) ;
         curvature.(i).(0) <- ( Matrix.vector_float_scal_prod repere.(1) frame.(i).(1) ) /. ( celer.(i) *. celer.(i) ) ;
         accu.(i) <- curvature.(i).(0) ;
         for j = 2 to n do
          curvature.(i).( j - 1 ) <- ( Matrix.vector_float_scal_prod repere.(j) frame.(i).(j) ) /. ( accu.(i) *. ( celer.(i) ** ( float ( j + 1 ) ) ) ) ;
          accu.(i) <- accu.(i) *. curvature.(i).( j - 1 ) ;
         done ;
        done ;
        [| [| curvature |] ; frame ; [| [| accu |] |] |] ;;


(** {v discrete_serret_frenet methode scale position_matrix v} The output gives the samples of the multi-curvature vector, 
then the samples of the Serret-Frenet frame, then the samples of the product of all the curvatures.

La sortie comporte les échantillons du vecteur multicourbure, puis les échantillons du repère de Serret-Frenet,
puis les échantillons du produit de toutes les courbures. *)
let discrete_serret_frenet = fun methode (scale:float) (position:float array array) ->
 let dim = Array.length position.(0) in
  let dim_jet = discrete_jet methode dim scale position
  and n = dim - 1 in
   let size = Array.length dim_jet in
    let accu = Array.make size 1.
    and celer = Array.make size 0.
    and curvature = Array.make_matrix size n 0.
    and frame = Array.make size ( Array.make_matrix dim dim 0. ) in
     for i = 0 to size - 1 do
      let repere = Array.sub dim_jet.(i) 1 dim in
       frame.(i) <- Matrix.float_trans_orthonormalize repere ;
       celer.(i) <- Matrix.vector_float_scal_prod repere.(0) frame.(i).(0) ;
       curvature.(i).(0) <- ( Matrix.vector_float_scal_prod repere.(1) frame.(i).(1) ) /. ( celer.(i) *. celer.(i) ) ;
       accu.(i) <- curvature.(i).(0) ;
       for j = 2 to n do
        curvature.(i).( j - 1 ) <- ( Matrix.vector_float_scal_prod repere.(j) frame.(i).(j) ) /. ( accu.(i) *. ( celer.(i) ** ( float ( j + 1 ) ) ) ) ;
        accu.(i) <- accu.(i) *. curvature.(i).( j - 1 ) ;
       done ;
     done ;
     [| [| curvature |] ; frame ; [| [| accu |] |] |] ;;



(** {v discrete_partial_diff_x methode matrix v} The column numbers of the matrix correspond to the abscissae.
The method [methode] is that of one_dimensional discrete derivation.

Les numéros de colonnes de la matrice représentent les abscisses. La méthode [methode] est celle de dérivation discrète unidimensionnelle.*)
let discrete_partial_diff_x = fun methode (m:float array array) ->
 Array.map methode m ;;


(** {v discrete_partial_diff_y methode matrix v} The column numbers of the matrix correspond to the abscissae.
The method [methode] is that of one_dimensional discrete derivation.

Les numéros de colonnes de la matrice représentent les abscisses. La méthode [methode] est celle de dérivation discrète unidimensionnelle.*)
let discrete_partial_diff_y = fun methode (m:float array array) ->
 let w = Matrix.float_transpose m in
  Matrix.float_transpose ( discrete_partial_diff_x methode w ) ;;


(** {v discrete_area_element methode matrix v} The column numbers of the matrix correspond to the abscissae.
The method [methode] is that of one_dimensional discrete derivation.

Les numéros de colonnes de la matrice représentent les abscisses. La méthode [methode] est celle de dérivation discrète unidimensionnelle.*)
let discrete_area_element = fun methode (m:float array array) ->
 let w = discrete_partial_diff_x methode m
 and ww = discrete_partial_diff_y methode m
 and f = fun x y -> sqrt ( 1. +. x *. x +. y *. y ) in
  Matrix.matrix_float_apply2 f w ww ;;


(** {v discrete_gauss_curvature methode matrix v} The column numbers of the matrix correspond to the abscissae.
The method [methode] is that of one_dimensional discrete derivation.

Les numéros de colonnes de la matrice représentent les abscisses. La méthode [methode] est celle de dérivation discrète unidimensionnelle.*)
let discrete_gauss_curvature = fun methode (m:float array array) ->
 let w = discrete_partial_diff_x methode m
 and ww = discrete_partial_diff_y methode m
 and f = fun x y -> 1. /. ( 1. +. x *. x +. y *. y ) in
  let w_xx = discrete_partial_diff_x methode w
  and w_xy = discrete_partial_diff_x methode ww
  and w_yy = discrete_partial_diff_y methode ww
  and w1 = Matrix.matrix_float_apply2 f w ww in
   let w2 = Matrix.matrix_float_coeff_prod w_xx w_yy
   and w3 = Matrix.matrix_float_coeff_prod w_xy w_xy in
    let w4 = Matrix.matrix_float_minus w2 w3 in
     Matrix.matrix_float_coeff_prod w4 w1 ;;


(** {v discrete_mean_curvature methode matrix v} The column numbers of the matrix correspond to the abscissae.
The method [methode] is that of one_dimensional discrete derivation.

Les numéros de colonnes de la matrice représentent les abscisses. La méthode [methode] est celle de dérivation discrète unidimensionnelle.*)
let discrete_mean_curvature = fun methode (m:float array array) ->
 let w = discrete_partial_diff_x methode m
 and ww = discrete_partial_diff_y methode m
 and f = fun x y -> let z = 1. +. x *. x +. y *. y in 0.5 /. ( z *. ( sqrt z ) )
 and g = fun x y -> ( 1. +. x *. x ) *. y
 and h = fun x y -> (-2.) *. x *. y in
  let w_xx = discrete_partial_diff_x methode w
  and w_xy = discrete_partial_diff_x methode ww
  and w_yy = discrete_partial_diff_y methode ww
  and w0 = Matrix.matrix_float_coeff_prod w ww
  and w1 = Matrix.matrix_float_apply2 f w ww in
   let w2 = Matrix.matrix_float_apply2 g ww w_xx
   and w3 = Matrix.matrix_float_apply2 g w w_yy
   and w4 = Matrix.matrix_float_apply2 h w0 w_xy in
    let w5 = Matrix.matrix_float_plus w2 w3 in
     let w6 = Matrix.matrix_float_plus w5 w4 in
      Matrix.matrix_float_coeff_prod w6 w1 ;;


(** {v discrete_principal_curvatures methode matrix v} The column numbers of the matrix correspond to the abscissae.
The method [methode] is that of one_dimensional discrete derivation.

Les numéros de colonnes de la matrice représentent les abscisses. La méthode [methode] est celle de dérivation discrète unidimensionnelle.*)
let discrete_principal_curvatures = fun methode (m:float array array) ->
 let w = discrete_mean_curvature methode m
 and ww = discrete_gauss_curvature methode m
 and f = fun x y -> ( real_float_solve_degree_2 1. ( -. x )  y ).(0)
 and g = fun x y -> ( real_float_solve_degree_2 1. ( -. x ) y ).(1) in
  let w1 = Matrix.matrix_float_apply2 f w ww
  and w2 = Matrix.matrix_float_apply2 g w ww in
   [| w1 ; w2 |] ;;





(** {C § } *)
(** 
{2 Interpolation}
*)
(** {C  } *)




(** {v float_linear_interpol vector real v} *)
let float_linear_interpol = fun (v:float array) (x:float) ->
 let ll = ( Array.length v ) - 1 in
  if ( x <= 0. ) then v.(0) 
  else if x >= float ll then v.(ll)
   else let e = floor x in
    let index = int_of_float e
    and y = x -. e in
     y *. v.( index + 1 ) +. ( 1. -. y ) *. v.(index) ;;

(** {v float_regular_stair_interpol function vector real v} *)
let float_regular_stair_interpol = fun (f:float -> float) (v:float array) (x:float) ->
 let ll = ( Array.length v ) - 1 in
  if ( x <= 0. ) then v.(0) 
  else if x >= float ll then v.(ll)
   else let e = floor x in
    let index = int_of_float e
    and y = x -. e in
     let z = f y in
     z *. v.(index) +. ( 1. -. z ) *. v.( index+ 1 ) ;;

(** {v float_medium_interpol function vector real v} *)
let float_medium_interpol = fun (f:float -> float) (v:float array) (x:float) ->
 let ll = ( Array.length v ) - 1 in
  if ( x <= 0. ) then v.(0) 
  else if x >= float ll then v.(ll)
   else let e = floor x in
    let index = int_of_float e
    and y = x -. e in
     let z = f y in
      let w = 0.5 *. ( z +. 1. -. y ) in
       w *. v.(index) +. ( 1. -. w ) *. v.( index+ 1 ) ;;


(** {v float_fit_interpol methode function parameter vector real v} The absolute value of the [parameter]
conditions the importance of the differentiable stair interpolation against the importance of the linear
interpolation. The method [methode] is that of differentiation, and must preserve the size of the sample.

La valeur absolue du paramètre [parameter] dose l'importance de l'interpolation en escalier dérivable
contre l'importance de l'interpolation linéaire.
La méthode [methode] est celle de dérivation, et doit préserver la taille de l'échantillon. *)
let float_fit_interpol = fun methode (f:float -> float) (parameter:float) (v:float array) (x:float) ->
 let ll = ( Array.length v ) - 1
 and c = discrete_graph_curvature methode 1. v in
  if ( x <= 0. ) then v.(0) 
  else if x >= float ll then v.(ll)
   else let e = floor x in
    let index = int_of_float e
    and y = x -. e in
     let z = f y
     and coeff = abs_float ( tanh ( parameter *. c.(index) ) ) in
      let w = coeff *. z +. ( 1. -. coeff ) *. ( 1. -. y ) in
       w *. v.(index) +. ( 1. -. w ) *. v.( index + 1 ) ;;


(** {v float_tune_interpol methode function vector real v} The parameter [scale] is
used in the discrete differential method [methode]. 
It may be used here in order to tune the smoothing effect. In case of doubt, use 1.0.
The [parameter] is used for the interpolation [float_fit_interpol] applied to the
derivative. A value of 0.1 seems convenient.

Le paramètre [scale] est utilisé dans la méthode [methode] de différentiation discrète.
Il peut servir à doser l'effet de régularisation ici. Dans le doute, prendre 1.0.
Le paramètre [parameter] sert pour l'interpolation [float_fit_interpol]
appliquée à la dérivée. Une valeur de 0.1 paraît raisonnable. *)
let float_tune_interpol = fun methode (f:float -> float) (v:float array) (x:float) ->
 let ll = ( Array.length v ) - 1
 and c = discrete_graph_curvature methode 1e0 v in
  if ( x <= 0. ) then v.(0) 
  else if x >= float ll then v.(ll)
   else let parameter = 1e1 *. sinh ( float_medium_interpol f c x ) in
     float_fit_interpol methode f parameter v x ;;


(** {v vector_trans_interpol methode vector_sample v} The data must be presented by coordinates.

Les données doivent être présentées coordonnées par coordonnées. *)
let vector_trans_interpol = fun methode (v:float array array) (x:float) ->
 let r = Array.length v in
  let w = Array.make r 0. in
   for i = 0 to r - 1 do
    w.(i) <- methode v.(i) x ;
   done ;
   w ;;


(** {v vector_interpol methode vector_sample v}  *)
let vector_interpol = fun methode (v:float array array) (x:float) ->
 vector_trans_interpol methode ( Matrix.float_transpose v ) x ;;


(** {v matrix_trans_interpol methode matrix_sample v} The data must be presented by coefficients.

Les données doivent être présentées coefficients par coefficients. *)
let matrix_trans_interpol = fun methode (m:float array array array) (x:float) ->
 let r = Array.length m
 and l = Array.length m.(0) in
  let w = Array.make_matrix r l 0. in
   for i = 0 to r - 1 do
    w.(i) <- vector_trans_interpol methode m.(i) x ;
   done ;
   w ;;


(** {v matrix_interpol methode matrix_sample v}  *)
let matrix_interpol = fun methode (m:float array array array) (x:float) ->
 matrix_trans_interpol methode ( Array.map Matrix.float_transpose ( Util.transpose m ) ) x ;;


(** {C  } *)

(** The following multi-variables interpolations are polymorphic.

Les interpolations à plusieurs variables qui suivent sont polymorphes. *)

(** {C  } *)

(** {v interpol_2 methode matrix abscissa ordinate v} The row numbers correspond to the ordinates
and the column numbers correspond to the abscissae. The method [methode] is that of the chosen interpolation.

Les numéros de lignes correspondent aux ordonnées et les numéros de colonnes aux abscisses.
La méthode [methode] est la méthode d'interpolation choisie. *)
let interpol_2 = fun methode (m:'a array array) (x:float) (y:float) ->
 let intermed = fun i z -> methode m.(i) x
 and v = Array.make ( Array.length m ) 0. in
  let vv = Array.mapi intermed v in
   methode vv y ;;


(** {v interpol_3 methode matrix abscissa ordinate v} The row numbers correspond to the altitudes
and the following numbers correspond to the ordinates then the abscissae. The method [methode] is that of the chosen interpolation.

Les numéros de lignes correspondent aux cotes et les numéros suivants aux ordonnées puis abscisses.
La méthode [methode] est la méthode d'interpolation choisie. *)
let interpol_3 = fun methode (m:'a array array array) (x:float) (y:float) (z:float) ->
 let intermed = fun i z -> interpol_2 methode m.(i) x y
 and v = Array.make ( Array.length m ) 0. in
  let vv = Array.mapi intermed v in
   methode vv z ;;


(** {v interpol methode multi_vector abscissa ordinate v} The row numbers correspond to the last coordinate
and the following numbers correspond to the coordinates in reverse order. The method [methode] is that of the chosen interpolation.

Les numéros de lignes correspondent à la dernière coordonnée et les numéros suivants aux coordonnées en ordre inverse.
La méthode [methode] est la méthode d'interpolation choisie. *)
let rec interpol = fun methode (m:Matrix.float_or_array) (x:float array) ->
 let d = Matrix.foa_thickness m in
  match d with
  | 0 -> let mm = Matrix.vector_float_demakeup m in methode mm x.(0)
  | _ -> 
   let mm = Matrix.vector_foa_demakeup m in 
    let r = Array.length mm in
     let rr = r - 1 in
      let intermed = fun i z -> interpol methode mm.(i) ( Array.sub x 0 rr )
      and v = Array.make r 0. in
       let vv = Array.mapi intermed v in
        methode vv x.(rr) ;;




(** {C § } *)
(** 
{1 Zéros}
*)
(** {C  } *)




(** {v float_zero_secant maxstep function start v} This method may work for a root of order one.

Cette méthode peut fonctionner pour une racine d'ordre un.*)
let float_zero_secant = fun (maxstep:int) (f:float -> float) (a:float) ->
 let step = ref 0
 and x = ref a
 and z = ref a
 and xx = ref ( f a )
 and y = ref ( a +. Random.float ( abs_float ( a /. 10. ) +. sqrt epsilon_float ) ) in
  let yy = ref ( f !y )
  and xxx = ref ( abs_float !xx )
  and zz = ref !xx in
   let yyy = ref ( abs_float !yy )
   and zzz = ref ( abs_float !zz ) in
    let v = [| !xxx ; !yyy ; !zzz |] in
     while ( ( !step <= maxstep ) && ( !xx <> 0. ) ) do
      let pente = ( !x -. !y ) /. ( !yy -. !xx ) in
       z := !y +. !yy *. pente ;
       zz := f !z ;
       zzz := abs_float !zz ;
       v.(2) <- !zzz ;
       Array.fast_sort compare v ;
       if ( v.(0) = !zzz ) then ( y := !z ; yy := !zz ; yyy := !zzz ) ;
       if ( v.(0) = !xxx ) then ( y := !x ; yy := !xx ; yyy := !xxx ) ;
       if ( v.(1) = !zzz ) then ( x := !z ; xx := !zz ; xxx := !zzz ) ;
       if ( v.(1) = !yyy ) then ( x := !y ; xx := !yy ; xxx := !yyy ) ;
       step := !step + 1 ;
     done ;
     !x ;;


(** {v float_zero_newton methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let float_zero_newton = fun methode (maxstep:int) (f:float -> float) (a:float) ->
 let step = ref 0
 and x = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   let pente = methode f !x
   and ordonnee = f !x in
    y := !x -. ordonnee /. pente ;
    step := !step + 1 ;
    if ( abs_float ( f !x ) > abs_float ( f !y ) ) then  x := !y
  done ;
  !x ;;


(** {v float_zero_halley methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let float_zero_halley = fun methode (maxstep:int) (f:float -> float) (a:float) ->
 let step = ref 0
 and g = function x -> 1. /. ( f x )
 and x = ref a in
  let y = ref !x
  and j = float_jet methode 2 g in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   let num = ( j !x ).(1)
   and denom = ( j !x ).(2) in
    y := !x +. 2. *. num /. denom ;
    step := !step + 1 ;
    if ( abs_float ( f !x ) > abs_float ( f !y ) ) then  x := !y
  done ;
  !x ;;


(** {v float_zero_householder methode order maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let float_zero_householder = fun methode (order:int) (maxstep:int) (f:float -> float) (a:float) ->
 let step = ref 0
 and g = function x -> 1. /. ( f x )
 and x = ref a in
  let y = ref !x
  and j = float_jet methode order g in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   let num = ( j !x ).( order - 1 )
   and denom = ( j !x ).(order) in
    y := !x +. ( float order ) *. num /. denom ;
    step := !step + 1 ;
    if ( abs_float ( f !x ) > abs_float ( f !y ) ) then  x := !y
  done ;
  !x ;;


(** {v float_zero_pot_pourri methode maxorder maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let float_zero_pot_pourri = fun methode (maxorder:int) (maxstep:int) (f:float -> float) (a:float) ->
 let step = ref 0
 and x = ref a
 and xx = ref ( f a )
 and y = ref a in
  let yy = ref !xx in
   while ( ( !step <= maxstep ) && ( !xx <> 0. ) ) do
    y := float_zero_secant 1 f !x ;
    yy := f !y ;
    if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
    y := float_zero_newton methode 1 f !x ;
    yy := f !y ;
    if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
    y := float_zero_halley methode 1 f !x ;
    yy := f !y ;
    if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
    if ( maxorder >= 3 ) then
     begin
      for order = 3 to maxorder do
       begin
        y := float_zero_householder methode order 1 f !x ;
        yy := f !y ;
        if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
       end
      done ;
     end ;
    step := !step + 1 ;
   done ;
   !x ;;


(** {v float_zero_pot_pourri_alea methode maxorder maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let float_zero_pot_pourri_alea = fun methode (maxorder:int) (maxstep:int) (f:float -> float) (a:float) ->
 let step = ref 0
 and excursion = ref ( ( float maxstep ) *. epsilon_float )
 and x = ref a
 and xx = ref ( f a )
 and y = ref a in
  let yy = ref !xx in
   while ( ( !step <= maxstep ) && ( !xx <> 0. ) ) do
    y := float_zero_secant 1 f !x ;
    yy := f !y ;
    if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
    y := float_zero_newton methode 1 f !x ;
    yy := f !y ;
    if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
    y := float_zero_halley methode 1 f !x ;
    excursion := 2. *. ( !y -. !x ) ; 
    if !excursion = 0. then excursion := ( float maxstep ) *. epsilon_float ;
    yy := f !y ;
    if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
    if ( maxorder >= 3 ) then
     begin
      for order = 3 to maxorder do
       begin
        y := float_zero_householder methode order 1 f !x ;
        yy := f !y ;
        if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
       end
      done ;
     end ;
    y := !x *. ( 1. -. Random.float !excursion) ;
    yy := f !y ;
    if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
    y := !x *. ( 1. +. Random.float !excursion) ;
    yy := f !y ;
    if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ) ;
    step := !step + 1 ;
   done ;
   !x ;;


(** {v float_zero_general methode maxorder maxstep function start v} The idea of the auxiliary function
comes from the HP journal of december 1979 about the HP34C calculator.
 The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
L'idée de la fonction auxiliaire provient du journal HP de décembre 1979 concernant la calculette HP34C.*)
let float_zero_general = fun methode (maxorder:int) (maxstep:int) (f:float -> float) (a:float) ->
 let g = function x -> ( exp ( -. abs_float ( f x ) ) -. 1. )
 and step = ref 0 in
  let x = ref a
  and xx = ref ( f a )
  and xxx = ref ( g a )
  and y = ref a in
   let yy = ref !xx
   and yyy = ref !xxx in
    while ( ( !step <= maxstep ) && ( !xx <> 0. ) ) do
     y := float_zero_pot_pourri methode maxorder 1 f !x ;
     yy := f !y ;
     if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ; xxx := g !y ) ;
     y := float_zero_pot_pourri methode maxorder 1 g !x ;
     yyy := g !y ;
     if ( abs_float !xxx > abs_float ( !yyy ) ) then ( x := !y ; xx := f !y ; xxx := !yyy ) ;
     step := !step + 1 ;
    done ;
    !x ;;


(** {v float_zero_general_alea methode maxorder maxstep function start v} The idea of the auxiliary function
comes from the HP journal of december 1979 about the HP34C calculator.
 The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
L'idée de la fonction auxiliaire provient du journal HP de décembre 1979 concernant la calculette HP34C.*)
let float_zero_general_alea = fun methode (maxorder:int) (maxstep:int) (f:float -> float) (a:float) ->
 let g = function x -> ( exp ( -. abs_float ( f x ) ) -. 1. )
 and step = ref 0 in
  let x = ref a
  and xx = ref ( f a )
  and xxx = ref ( g a )
  and y = ref a in
   let yy = ref !xx
   and yyy = ref !xxx in
    while ( ( !step <= maxstep ) && ( !xx <> 0. ) ) do
     y := float_zero_pot_pourri_alea methode maxorder 1 f !x ;
     yy := f !y ;
     if ( abs_float !xx > abs_float ( !yy ) ) then ( x := !y ; xx := !yy ; xxx := g !y ) ;
     y := float_zero_pot_pourri_alea methode maxorder 1 g !x ;
     yyy := g !y ;
     if ( abs_float !xxx > abs_float ( !yyy ) ) then ( x := !y ; xx := f !y ; xxx := !yyy ) ;
     step := !step + 1 ;
    done ;
    !x ;;


(** {v vector_float_zero_secant maxstep function start v} This method may work for a root of order one.

Cette méthode peut fonctionner pour une racine d'ordre un.*)
let vector_float_zero_secant = fun (maxstep:int) (f:float array -> float) (a:float array) ->
 let step = ref 0
 and x = ref a
 and z = ref a
 and xx = ref ( f a )
 and y = ref ( Matrix.vector_float_plus a ( Matrix.vector_float_bal_random ( Array.length a ) ( abs_float ( ( Matrix.vector_float_norm_inf a ) /. 10. ) +. sqrt epsilon_float ) ) ) in
  let yy = ref ( f !y )
  and xxx = ref ( abs_float !xx )
  and zz = ref !xx in
   let yyy = ref ( abs_float !yy )
   and zzz = ref ( abs_float !zz ) in
    let v = [| !xxx ; !yyy ; !zzz |] in
     while ( ( !step <= maxstep ) && ( !xx <> 0. ) ) do
      let pente = Matrix.vector_float_scal_left_div ( !yy -. !xx ) ( Matrix.vector_float_minus !x !y ) in
       z := Matrix.vector_float_plus !y ( Matrix.vector_float_scal_mult !yy pente ) ;
       zz := f !z ;
       zzz := abs_float !zz ;
       v.(2) <- !zzz ;
       Array.fast_sort compare v ;
       if ( v.(0) = !zzz ) then ( y := !z ; yy := !zz ; yyy := !zzz ) ;
       if ( v.(0) = !xxx ) then ( y := !x ; yy := !xx ; yyy := !xxx ) ;
       if ( v.(1) = !zzz ) then ( x := !z ; xx := !zz ; xxx := !zzz ) ;
       if ( v.(1) = !yyy ) then ( x := !y ; xx := !yy ; xxx := !yyy ) ;
       step := !step + 1 ;
     done ;
     !x ;;


(** {v desc_grad_zero methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let desc_grad_zero = fun methode (maxstep:int) (f:float array -> float) (a:float array) ->
 let step = ref 0
 and x = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   let pente = gradient methode f !x
   and ordonnee = f !x in
     y := Matrix.vector_float_minus !x ( Matrix.vector_float_scal_right_div ordonnee pente ) ;
     step := !step + 1 ;
     if ( abs_float ( f !x ) > abs_float ( f !y ) ) then x := !y
  done ;
  !x ;;


(** {v vector_float_halley_zero methode_reduc methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.
The parameter [n] is the maximum number of steps for the pseudo-inverse of symmetric matrices.
The reduction method [methode_reduc] applies to real symmetric matrices.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques réelles.
La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas.
La paramètre [n] est le nombre maximal de pas pour le pseudo-inverse des matrices symétriques. *)
let vector_float_halley_zero = fun methode_reduc methode (maxstep:int) (f:float array -> float) (a:float array) ->
 let step = ref 0
 and x = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   let pente = gradient methode f !x
   and quadra = Matrix.sym_float_pinv methode_reduc 1e-3 ( hess methode f !x ) in
     y := Matrix.vector_float_minus !x ( Matrix.matrix_vector_float_prod quadra pente ) ;
     step := !step + 1 ;
     if ( abs_float ( f !x ) > abs_float ( f !y ) ) then x := !y
  done ;
  !x ;;


(** {v vector_float_zero_general methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_float_zero_general = fun methode (maxstep:int) (f:float array -> float) (a:float array) ->
 let g = function x -> ( exp ( -. abs_float ( f x ) ) -. 1. )
 and step = ref 0
 and x = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   y := vector_float_zero_secant 1 f !x ;
   x := !y ;
   y := desc_grad_zero methode 1 f !x ;
   x := !y ;
   y := vector_float_zero_secant 1 g !x ;
   x := !y ;
   y := desc_grad_zero methode 1 g !x ;
   x := !y ;
   step := !step + 1 ;
  done ;
  !x ;;


(** {v vector_float_zero_general_alea methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_float_zero_general_alea = fun methode (maxstep:int) (f:float array -> float) (a:float array) ->
 let g = function x -> ( exp ( -. abs_float ( f x ) ) -. 1. )
 and step = ref 0
 and l = Array.length a
 and excursion = ref ( ( float maxstep ) *. epsilon_float )
 and x = ref a
 and z = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   y := vector_float_zero_secant 1 f !x ;
   x := !y ;
   y := desc_grad_zero methode 1 f !x ;
   x := !y ;
   y := vector_float_zero_secant 1 g !x ;
   x := !y ;
   y := desc_grad_zero methode 1 g !x ;
   x := !y ;
   excursion := 2. *. ( Matrix.vector_float_norm_inf ( Matrix.vector_float_minus !y !x ) ) ;
   if !excursion = 0. then excursion := ( float maxstep ) *. epsilon_float ;
   if ( abs_float ( f !x ) > abs_float ( f !y ) ) then x := !y ;
   z := Matrix.vector_float_bal_random l !excursion ;
   z := Matrix.vector_float_plus !z !y ;
   if ( abs_float ( f !z ) < abs_float ( f !y ) ) then ( x := !z ) ;
   step := !step + 1 ;
  done ;
  !x ;;


(** {v vector_float_zero_general_2 methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.
The reduction method [methode_reduc] applies to real symmetric matrices.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques réelles.
La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_float_zero_general_2 = fun methode_reduc methode (maxstep:int) (f:float array -> float) (a:float array) ->
 let g = function x -> ( exp ( -. abs_float ( f x ) ) -. 1. )
 and step = ref 0
 and x = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   y := vector_float_zero_secant 1 f !x ;
   x := !y ;
   y := desc_grad_zero methode 1 f !x ;
   x := !y ;
   y := vector_float_halley_zero methode_reduc methode maxstep f !x ;
   x := !y ;
   y := vector_float_zero_secant 1 g !x ;
   x := !y ;
   y := desc_grad_zero methode 1 g !x ;
   x := !y ;
   y := vector_float_halley_zero methode_reduc methode maxstep g !x ;
   x := !y ;
   step := !step + 1 ;
  done ;
  !x ;;


(** {v vector_float_zero_general_2_alea methode_reduc methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.
The reduction method [methode_reduc] applies to real symmetric matrices.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques réelles.
La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_float_zero_general_2_alea = fun methode_reduc methode (maxstep:int) (f:float array -> float) (a:float array) ->
 let g = function x -> ( exp ( -. abs_float ( f x ) ) -. 1. )
 and step = ref 0
 and l = Array.length a
 and excursion = ref ( ( float maxstep ) *. epsilon_float )
 and x = ref a
 and z = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( f !x <> 0. ) ) do
   y := vector_float_zero_secant 1 f !x ;
   x := !y ;
   y := desc_grad_zero methode 1 f !x ;
   x := !y ;
   y := vector_float_halley_zero methode_reduc methode maxstep f !x ;
   x := !y ;
   y := vector_float_zero_secant 1 g !x ;
   x := !y ;
   y := desc_grad_zero methode 1 g !x ;
   x := !y ;
   y := vector_float_halley_zero methode_reduc methode maxstep g !x ;
   excursion := 2. *. ( Matrix.vector_float_norm_inf ( Matrix.vector_float_minus !y !x ) ) ;
   if !excursion = 0. then excursion := ( float maxstep ) *. epsilon_float ;
   if ( abs_float ( f !x ) > abs_float ( f !y ) ) then x := !y ;
   z := Matrix.vector_float_bal_random l !excursion ;
   z := Matrix.vector_float_plus !z !y ;
   if ( abs_float ( f !z ) < abs_float ( f !x ) ) then ( x := !z ) ;
   step := !step + 1 ;
  done ;
  !x ;;


(** {v vector_zero_newton methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_zero_newton = fun methode (maxstep:int) (f:float array -> float array) (a:float array) ->
 let step = ref 0
 and x = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( Matrix.vector_float_norm_inf ( f !x ) <> 0. ) ) do
   let pente = tlm methode f !x
   and ordonnee = f !x in
    let inverse = Matrix.clean_inv pente in
     y := Matrix.vector_float_minus !x ( Matrix.matrix_vector_float_prod inverse ordonnee ) ;
     step := !step + 1 ;
     if ( Matrix.vector_float_norm_inf ( f !x ) > Matrix.vector_float_norm_inf ( f !y ) ) then x := !y
  done ;
  !x ;;
  

(** {v vector_zero_general methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_zero_general = fun methode (maxstep:int) (f:float array -> float array) (a:float array) ->
 let step = ref 0
 and g = function vector -> Matrix.vector_float_norm_inf ( f vector )
 and x = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( g !x <> 0. ) ) do
   y := vector_zero_newton methode 1 f !x ;
   x := !y ;
   y := vector_float_zero_general methode 1 g !x ;
   x := !y ;
   step := !step + 1 ;
  done ;
  !x ;;


(** {v vector_zero_general_alea methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_zero_general_alea = fun methode (maxstep:int) (f:float array -> float array) (a:float array) ->
 let step = ref 0
 and g = function vector -> Matrix.vector_float_norm_inf ( f vector )
 and l = Array.length a
 and excursion = ref ( ( float maxstep ) *. epsilon_float )
 and x = ref a
 and z = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( g !x <> 0. ) ) do
   y := vector_zero_newton methode 1 f !x ;
   x := !y ;
   y := vector_float_zero_general_alea methode 1 g !x ;
   excursion := 2. *. ( Matrix.vector_float_norm_inf ( Matrix.vector_float_minus !y !x ) ) ;
   if !excursion = 0. then excursion := ( float maxstep ) *. epsilon_float ;
   if ( ( g !x ) > ( g !y ) ) then x := !y ;
   z := Matrix.vector_float_bal_random l !excursion ;
   z := Matrix.vector_float_plus !z !x ;
   if ( ( g !z ) < ( g !x ) ) then ( x := !z ) ;
   step := !step + 1 ;
  done ;
  !x ;;


(** {v vector_zero_general_2 methode_reduc methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.
The reduction method [methode_reduc] applies to real symmetric matrices.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques réelles.
La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_zero_general_2 = fun methode_reduc methode (maxstep:int) (f:float array -> float array) (a:float array) ->
 let step = ref 0
 and g = function vector -> Matrix.vector_float_norm_inf ( f vector )
 and x = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( g !x <> 0. ) ) do
   y := vector_zero_newton methode 1 f !x ;
   x := !y ;
   y := vector_float_zero_general_2 methode_reduc methode 1 g !x ;
   x := !y ;
   step := !step + 1 ;
  done ;
  !x ;;


(** {v vector_zero_general_2_alea methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.
The reduction method [methode_reduc] applies to real symmetric matrices.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques réelles.
La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_zero_general_2_alea = fun methode_reduc methode (maxstep:int) (f:float array -> float array) (a:float array) ->
 let step = ref 0
 and g = function vector -> Matrix.vector_float_norm_inf ( f vector )
 and l = Array.length a
 and excursion = ref ( ( float maxstep ) *. epsilon_float )
 and x = ref a
 and z = ref a
 and y = ref a in
  while ( ( !step <= maxstep ) && ( g !x <> 0. ) ) do
   y := vector_zero_newton methode 1 f !x ;
   x := !y ;
   y := vector_float_zero_general_2_alea methode_reduc methode 1 g !x ;
   excursion := 2. *. ( Matrix.vector_float_norm_inf ( Matrix.vector_float_minus !y !x ) ) ;
   if !excursion = 0. then excursion := ( float maxstep ) *. epsilon_float ;
   if ( g !y < g !x ) then ( x := !y ) ;
   z := Matrix.vector_float_bal_random l !excursion ;
   z := Matrix.vector_float_plus !z !x ;
   if ( g !z < g !x ) then ( x := !z ) ;
   step := !step + 1 ;
  done ;
  !x ;;


(** {v vector_matrix_zero_general methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_matrix_zero_general = fun methode (maxstep:int) (f:float array -> float array array) (a:float array) ->
 let g = function vector -> Array.map Matrix.vector_float_norm_inf ( f vector ) in
  vector_zero_general methode maxstep g a ;;


(** {v vector_matrix_zero_general_alea methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.

La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_matrix_zero_general_alea = fun methode (maxstep:int) (f:float array -> float array array) (a:float array) ->
 let g = function vector -> Array.map Matrix.vector_float_norm_inf ( f vector ) in
  vector_zero_general_alea methode maxstep g a ;;


(** {v vector_matrix_zero_general_2 methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.
The reduction method [methode_reduc] applies to real symmetric matrices.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques réelles.
La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_matrix_zero_general_2 = fun methode_reduc methode (maxstep:int) (f:float array -> float array array) (a:float array) ->
 let g = function vector -> Array.map Matrix.vector_float_norm_inf ( f vector ) in
  vector_zero_general_2 methode_reduc methode maxstep g a ;;


(** {v vector_matrix_zero_general_2_alea methode maxstep function start v} The (unidimensional) derivating method must contain the parameters, including the step.
The reduction method [methode_reduc] applies to real symmetric matrices.

La méthode de réduction [methode_reduc] s'applique aux matrices symétriques réelles.
La méthode de dérivation (unidimensionnelle) doit contenir les paramètres, y compris le pas. *)
let vector_matrix_zero_general_2_alea = fun methode_reduc methode (maxstep:int) (f:float array -> float array array) (a:float array) ->
 let g = function vector -> Array.map Matrix.vector_float_norm_inf ( f vector ) in
  vector_zero_general_2_alea methode_reduc methode maxstep g a ;;


(** {v matrix_zero methode function start v} The zeroing method 
(of a function which associates a matrix to a vector) must contain all the parameters.

La méthode d'annulation (d'une fonction qui à un vecteur associe une matrice) doit contenir tous les paramètres. *)
let matrix_zero = fun methode (f:float array array -> float array array) (a:float array array) ->
 let l = Array.length a in
  let g = function vector -> f ( Array.map Matrix.vector_float_demakeup ( Matrix.vector_foa_demakeup ( Matrix.vector_foa_cut l ( Matrix.Float_vector_cons vector ) ) ) ) in
   Array.map Matrix.vector_float_demakeup ( Matrix.vector_foa_demakeup ( Matrix.vector_foa_cut l ( Matrix.Float_vector_cons ( methode g ( Array.fold_left Array.append [| |] a ) ) ) ) ) ;;


(** {v matrix_vector_zero methode function start v} The zeroing method 
(of a function which associates a vector to a vector) must contain all the parameters.

La méthode d'annulation (d'une fonction qui à un vecteur associe un vecteur) doit contenir tous les paramètres. *)
let matrix_vector_zero = fun methode (f:float array array -> float array) (a:float array array) ->
 let l = Array.length a in
  let g = function vector -> f ( Array.map Matrix.vector_float_demakeup ( Matrix.vector_foa_demakeup ( Matrix.vector_foa_cut l ( Matrix.Float_vector_cons vector ) ) ) ) in
   Array.map Matrix.vector_float_demakeup ( Matrix.vector_foa_demakeup ( Matrix.vector_foa_cut l ( Matrix.Float_vector_cons ( methode g ( Array.fold_left Array.append [| |] a ) ) ) ) ) ;;


(** {v matrix_float_zero methode function start v} The zeroing method 
(of a function which associates a real to a vector) must contain all the parameters.

La méthode d'annulation (d'une fonction qui à un vecteur associe un réel) doit contenir tous les paramètres. *)
let matrix_float_zero = fun methode (f:float array array -> float) (a:float array array) ->
 let l = Array.length a in
  let g = function vector -> f ( Array.map Matrix.vector_float_demakeup ( Matrix.vector_foa_demakeup ( Matrix.vector_foa_cut l ( Matrix.Float_vector_cons vector ) ) ) ) in
   Array.map Matrix.vector_float_demakeup ( Matrix.vector_foa_demakeup ( Matrix.vector_foa_cut l ( Matrix.Float_vector_cons ( methode g ( Array.fold_left Array.append [| |] a ) ) ) ) ) ;;




(** {C § } *)
(** 
{3 Remarque}
{3 Remark}
*)
(** {C  } *)




(** When searching for zeros on discrete data via the interpolation, be aware of the
[float_regular_stair_interpol] and of the other methods of interpolation which use it.

En cas de recherche de zéros sur des données discrètes via l'interpolation,
se méfier de l'interpolation par escalier dérivable [float_regular_stair_interpol] et
des autres méthodes d'interpolation qui l'utilisent. *)




(** {C § } *)
(** 
{1 Intégration}
*)
(** {C  } *)




(** {C  } *)
(** 
{2 Fonctions intégrées}
{2 Integrated functions}
*)
(** {C  } *)




(** {v float_int_rect nintervals function a b v} *)
let float_int_rect = fun (n:int) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and length = ( b -. a ) /. ( float n )
 and nn = n - 1 in
  for i = 0 to nn do
   accu := !accu +. ( f ( a +. ( float i ) *. length ) ) *. length
  done ;
  !accu ;;


(** {v float_int_incr_rect ratio nintervals function a b v} The step increases exponentially 
from [a] to [b]. The ratio [ratio] is the decreasing ratio, situated strictly between 0 and 1.

Le pas croît exponentiellement de [a] vers [b]. Le taux [ratio] est le taux de décroissance, 
compris strictement entre 0 et 1. *)
let float_int_incr_rect = fun (ratio:float) (n:int) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and x = ref b
 and length = ref ( ( b -. a ) *. ( 1. -. ratio ) /. ( 1. -. ratio ** ( float n ) ) ) in
  for i = 0 to n - 1 do
   x := !x -. !length ;
   accu := !accu +. ( f !x ) *. !length ;
   length := ratio *. !length ;
  done ;
  !accu ;;


(** {v float_int_decr_rect ratio nintervals function a b v} The step decreases exponentially 
from [a] to [b]. The ratio [ratio] is the decreasing ratio, situated strictly between 0 and 1.

Le pas décroît exponentiellement de [a] vers [b]. Le taux [ratio] est le taux de décroissance, 
compris strictement entre 0 et 1. *)
let float_int_decr_rect = fun (ratio:float) (n:int) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and x = ref a
 and length = ref ( ( b -. a ) *. ( 1. -. ratio ) /. ( 1. -. ratio ** ( float n ) ) ) in
  for i = 0 to n - 1 do
   x := !x +. !length ;
   accu := !accu +. ( f !x ) *. !length ;
   length := ratio *. !length ;
  done ;
  !accu ;;


(** {v float_int_trapez nintervals function a b v} *)
let float_int_trapez = fun (n:int) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and length = ( b -. a ) /. ( float n )
 and nn = n - 1 in
  for i = 1 to nn do
   accu := !accu +. ( f ( a +. ( float i ) *. length ) ) *. length
  done ;
  accu := !accu +. ( f a +. f b ) *. length *. 0.5 ;
  !accu ;;


(** {v float_int_incr_trapez ratio nintervals function a b v} The step increases exponentially 
from [a] to [b]. The ratio [ratio] is the decreasing ratio, situated strictly between 0 and 1.

Le pas croît exponentiellement de [a] vers [b]. Le taux [ratio] est le taux de décroissance, 
compris strictement entre 0 et 1. *)
let float_int_incr_trapez = fun (ratio:float) (n:int) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and x = ref b
 and xx = ref b
 and length = ref ( ( b -. a ) *. ( 1. -. ratio ) /. ( 1. -. ratio ** ( float n ) ) ) in
  for i = 0 to n - 1 do
   x := !x -. !length ;
   accu := !accu +. ( ( f !x ) +. ( f !xx ) ) *. !length *. 0.5 ;
   length := ratio *. !length ;
   xx := !x ;
  done ;
  !accu ;;


(** {v float_int_decr_trapez ratio nintervals function a b v} The step decreases exponentially 
from [a] to [b]. The ratio [ratio] is the decreasing ratio, situated strictly between 0 and 1.

Le pas décroît exponentiellement de [a] vers [b]. Le taux [ratio] est le taux de décroissance, 
compris strictement entre 0 et 1. *)
let float_int_decr_trapez = fun (ratio:float) (n:int) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and x = ref a
 and xx = ref a
 and length = ref ( ( b -. a ) *. ( 1. -. ratio ) /. ( 1. -. ratio ** ( float n ) ) ) in
  for i = 0 to n - 1 do
   xx := !xx +. !length ;
   accu := !accu +. ( ( f !x ) +. ( f !xx ) ) *. !length *. 0.5 ;
   length := ratio *. !length ;
   x := !xx ;
  done ;
  !accu ;;


(** {v float_int_simpson nintervals function a b v} *)
let float_int_simpson = fun (n:int) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and length = ( b -. a ) /. ( 2. *. float n )
 and nn = n - 1 in
  for i = 1 to nn do
   accu := !accu +. ( f ( a +. ( float ( 2 * i ) ) *. length ) +. 2. *. f ( a +. ( float ( 2 * i + 1 ) ) *. length ) ) *. length *. 2. /. 3.
  done ;
  accu := !accu +. ( f a +. f b +. 4. *. f ( a +. length ) ) *. length /. 3. ;
  !accu ;;


(** {v float_int_romberg order degree function a b v} *)
let rec float_int_romberg = fun (n:int) (k:int) (f:float -> float) (a:float) (b:float) ->
 let kk = abs k
 and nn = abs n in
  match kk with
  | 0 -> float_int_trapez ( int_of_float ( 2. ** ( float nn ) ) ) f a b
  | 1 -> float_int_simpson ( int_of_float ( 2. ** ( float nn ) ) ) f a b
  | _ -> 
   let kkk = kk - 1
   and coeff = 4. ** ( float kk ) in
    let aa = float_int_romberg ( nn + 1 ) kkk f a b
    and bb = float_int_romberg nn kkk f a b in
     ( coeff *. aa -. bb ) /. ( coeff -. 1. ) ;;

(** {v float_int order degree function a b v} *)
let float_int = fun (n:int) (k:int) (f:float -> float) (a:float) (b:float) ->
 let nn = ( 2 * n ) / 3 in
  let seq = Array.make ( succ nn ) 0.
  and nnn = n - nn in
   for i = 0 to nn do
    seq.(i) <- float_int_romberg ( nnn + i ) k f a b ;
   done ;
   Matrix.float_approx seq ;;


(** {v float_int_monte_carlo nsamples function a b v} *)
let float_int_monte_carlo = fun (n:int) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and range = b -. a in
  let length = range /. ( float n ) in
   for i = 1 to n do
    accu := !accu +. ( f ( a +. Random.float range ) ) *. length
   done ;
   !accu ;;


(** {v float_romberg_step_trapez function a b v} *)
let float_romberg_step_trapez = fun (f:float -> float) (a:float) (b:float) ->
 ( f a +. f b ) *. ( b -. a ) *. 0.5 ;;

(** {v float_int_romberg_trapez_adapt tolerance function a b v} *)
let rec float_int_romberg_trapez_adapt = fun (tol:float) (f:float -> float) (a:float) (b:float) ->
 let first = float_romberg_step_trapez f a b
 and c = ( a +. b ) *. 0.5 in
  let second = ref ( float_romberg_step_trapez f a c +. float_romberg_step_trapez f c b ) in
   if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
    begin
     second := float_int_romberg_trapez_adapt tol f a c +. float_int_romberg_trapez_adapt tol f c b
    end ;
    !second ;;

(** {v float_int_romberg_trapez_bounded maxstages tolerance function a b v} *)
let rec float_int_romberg_trapez_bounded = fun (maxstages:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 match maxstages with 
 | 0 -> float_romberg_step_trapez f a b
 | _ -> 
  let first = float_romberg_step_trapez f a b
  and c = ( a +. b ) *. 0.5 in
   let second = ref ( float_romberg_step_trapez f a c +. float_romberg_step_trapez f c b ) in
    if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
     begin
      second := float_int_romberg_trapez_bounded ( ( abs maxstages ) - 1 ) tol f a c
       +. float_int_romberg_trapez_bounded ( ( abs maxstages ) - 1 ) tol f c b
     end ;
     !second ;;


(** {v float_romberg_step_simpson function a b v} *)
let float_romberg_step_simpson = fun (f:float -> float) (a:float) (b:float) ->
 ( f a +. f b +. 4. *. f ( ( a +. b ) *. 0.5 ) ) *. ( b -. a ) /. 6. ;;

(** {v float_int_romberg_simpson_adapt tolerance function a b v} *)
let rec float_int_romberg_simpson_adapt = fun (tol:float) (f:float -> float) (a:float) (b:float) ->
 let first = float_romberg_step_simpson f a b
 and c = ( a +. b ) *. 0.5 in
  let second = ref ( float_romberg_step_simpson f a c +. float_romberg_step_simpson f c b ) in
   if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
    begin
     second := float_int_romberg_simpson_adapt tol f a c +. float_int_romberg_simpson_adapt tol f c b
    end ;
    !second ;;

(** {v float_int_romberg_simpson_bounded maxstages tolerance function a b v} *)
let rec float_int_romberg_simpson_bounded = fun (maxstages:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 match maxstages with 
 | 0 -> float_romberg_step_simpson f a b
 | _ -> 
  let first = float_romberg_step_simpson f a b
  and c = ( a +. b ) *. 0.5 in
   let second = ref ( float_romberg_step_simpson f a c +. float_romberg_step_simpson f c b ) in
    if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
     begin
      second := float_int_romberg_simpson_bounded ( ( abs maxstages ) - 1 ) tol f a c
       +. float_int_romberg_simpson_bounded ( ( abs maxstages ) - 1 ) tol f c b
     end ;
     !second ;;


(** {v float_romberg_step_3_8 function a b v} *)
let float_romberg_step_3_8 = fun (f:float -> float) (a:float) (b:float) ->
 let step = ( b -. a ) /. 3. in
  ( f a +. f b +. 3. *. ( f ( a +. step ) +. f ( b -. step ) ) ) *. ( b -. a ) *. 0.125 ;;

(** {v float_int_romberg_3_8_adapt tolerance function a b v} *)
let rec float_int_romberg_3_8_adapt = fun (tol:float) (f:float -> float) (a:float) (b:float) ->
 let first = float_romberg_step_3_8 f a b
 and c = ( a +. b ) *. 0.5 in
  let second = ref ( float_romberg_step_3_8 f a c +. float_romberg_step_3_8 f c b ) in
   if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
    begin
     second := float_int_romberg_3_8_adapt tol f a c +. float_int_romberg_3_8_adapt tol f c b
    end ;
    !second ;;

(** {v float_int_romberg_3_8_bounded maxstages tolerance function a b v} *)
let rec float_int_romberg_3_8_bounded = fun (maxstages:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 match maxstages with 
 | 0 -> float_romberg_step_3_8 f a b
 | _ -> 
  let first = float_romberg_step_3_8 f a b
  and c = ( a +. b ) *. 0.5 in
   let second = ref ( float_romberg_step_3_8 f a c +. float_romberg_step_3_8 f c b ) in
    if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
     begin
      second := float_int_romberg_3_8_bounded ( ( abs maxstages ) - 1 ) tol f a c
       +. float_int_romberg_3_8_bounded ( ( abs maxstages ) - 1 ) tol f c b
     end ;
     !second ;;


(** {v float_romberg_step_milne function a b v} *)
let float_romberg_step_milne = fun (f:float -> float) (a:float) (b:float) ->
 let step = ( b -. a ) *. 0.25 in
 ( 7. *. ( ( f a ) +. ( f b ) ) +. 32. *. ( ( f ( a +. step ) ) +. ( f ( b -. step ) ) ) +. 12. *. ( f ( 0.5 *. ( a +. b ) ) ) ) *. ( b -. a ) /. 90. ;;

(** {v float_int_romberg_milne_adapt tolerance function a b v} *)
let rec float_int_romberg_milne_adapt = fun (tol:float) (f:float -> float) (a:float) (b:float) ->
 let first = float_romberg_step_milne f a b
 and c = ( a +. b ) *. 0.5 in
  let second = ref ( float_romberg_step_milne f a c +. float_romberg_step_milne f c b ) in
   if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
    begin
     second := float_int_romberg_milne_adapt tol f a c +. float_int_romberg_milne_adapt tol f c b
    end ;
    !second ;;

(** {v float_int_romberg_milne_bounded maxstages tolerance function a b v} *)
let rec float_int_romberg_milne_bounded = fun (maxstages:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 match maxstages with 
 | 0 -> float_romberg_step_milne f a b
 | _ -> 
  let first = float_romberg_step_milne f a b
  and c = ( a +. b ) *. 0.5 in
   let second = ref ( float_romberg_step_milne f a c +. float_romberg_step_milne f c b ) in
    if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
     begin
      second := float_int_romberg_milne_bounded ( ( abs maxstages ) - 1 ) tol f a c
       +. float_int_romberg_milne_bounded ( ( abs maxstages ) - 1 ) tol f c b
     end ;
     !second ;;


(** {C § } *)

(** The following vectors are coefficients to use with [float_simple_step_gauss_kronrod_generic] 
and to define [float_romberg_step_gauss_kronrod].

Les vecteurs ci-dessous sont des coefficients pour [float_simple_step_gauss_kronrod_generic] 
et définissant [float_romberg_step_gauss_kronrod]. *)

(** {C  } *)



let gauss_abscissae_7_bis = [| 0.949107912342759 ; 0.741531185599394 ; 0.405845151377397 ;  0. |] ;;

let gauss_weights_7_bis = [| 0.129484966168870 ; 0.279705391489277 ; 0.381830050505119 ; 0.417959183673469 |] ;;

let kronrod_abscissae_15_bis = [| 0.991455371120813 ; 0.949107912342759 ; 0.864864423359769 ; 
 0.741531185599394 ; 0.586087235467691 ; 0.405845151377397 ; 0.207784955007898 ; 0. |] ;;

let kronrod_weights_15_bis = [| 0.022935322010529 ; 0.063092092629979 ; 0.104790010322250 ; 0.140653259715525 ; 
 0.169004726639267 ; 0.190350578064785 ; 0.204432940075298 ; 0.209482141084728 |] ;;

let gauss_weights_7 = [| 0.129484966168869693270611432679082 ; 0.279705391489276667901467771423780 ;
 0.381830050505118944950369775488975 ; 0.417959183673469387755102040816327 |] ;;

let gauss_abscissae_7 = [| 0.949107912342758524526189684047851 ;
 0.741531185599394439863864773280788 ; 0.405845151377397166906606412076961 ; 0. |] ;;

let kronrod_abscissae_15 = [| 0.991455371120812639206854697526329 ; 0.949107912342758524526189684047851 ; 0.864864423359769072789712788640926 ;
 0.741531185599394439863864773280788 ; 0.586087235467691130294144838258730 ; 0.405845151377397166906606412076961 ; 0.207784955007898467600689403773245 ; 0.0 |] ;;

let kronrod_weights_15 = [| 0.022935322010529224963732008058970; 0.063092092629978553290700663189204 ;
 0.104790010322250183839876322541518 ; 0.140653259715525918745189590510238 ; 0.169004726639267902826583426598550 ;
 0.190350578064785409913256402421014 ; 0.204432940075298892414161999234649 ; 0.209482141084727828012999174891714 |] ;;

let gauss_kronrod_abscissae_7 = [| 0.960491 ; 0.774597 ; 0.434244 ; 0.000000 |] ;;

let gauss_kronrod_weights_7 = [| 0.104656 ; 0.268488 ; 0.401397 ; 0.450917 |] ;;

let gauss_kronrod_weights_5to7 = [| 0.0 ; 0.555556 ; 0.0 ; 0.888889 |] ;;

let gauss_kronrod_abscissae_9 = [| 0.976560 ; 0.861136 ; 0.640286 ; 0.339981 ; 0.0 |]

let gauss_kronrod_weights_9 = [| 0.062977 ; 0.170054 ; 0.266798 ; 0.326949 ; 0.346443 |] ;;

let gauss_kronrod_weights_7to9 = [| 0.0 ; 0.347855 ; 0.0 ; 0.652145 ; 0.0 |] ;;

let gauss_kronrod_abscissae_15 = [| 0.2077849550789850 ; 0.4058451513773972 ; 0.5860872354676911 ;
 0.7415311855993944 ; 0.8648644233597691 ; 0.9491079123427585 ; 0.9914553711208126 ; 0.0 |] ;;

let gauss_kronrod_weights_15 = [| 0.2044329400752989 ; 0.1903505780647854 ; 0.1690047266392679 ;
 0.1406532597155259 ; 0.1047900103222502 ; 0.06309209262997855 ; 0.02293532201052922 ; 0.2094821410847278 |] ;;

let gauss_kronrod_abscissae_21 = [| 0.99565716302580808073552728070 ; 0.97390652851717172007796401210 ;  0.93015749135570822600120718010 ; 
 0.86506336668898451073209668840 ; 0.78081772658641689706371757830 ; 0.67940956829902440623432736510 ; 0.56275713466860468333900009930 ;
 0.43339539412924719079926594320 ; 0.29439286270146019813112660310 ; 0.14887433898163121088482600110 ; 0.0 |] ;;

let gauss_kronrod_weights_21 = [| 0.11694638867371874278064396060e-1 ; 0.32558162307964727478818972460e-1 ; 0.54755896574351996031381300240e-1 ; 
 0.75039674810919952767043140920e-1 ; 0.93125454583697605535065465080e-1 ; 0.10938715880229764189921059030 ; 0.12349197626206585107795810980 ; 
 0.13470921731147332592805400180 ; 0.14277593857706008079709427310 ; 0.14773910490133849137484151600 ; 0.14944555400291690566493646840 |] ;;

let gauss_kronrod_weights_11to21 = [| 0. ; 0.66671344308688137593568809890e-1 ; 0. ; 0.14945134915058059314577633970 ; 0. ; 
 0.21908636251598204399553493420 ; 0. ; 0.26926671930999635509122692160 ; 0. ; 0.29552422471475287017389299470 |] ;;

let gauss_kronrod_abscissae_31 = [| 0.1011420669187175 ; 0.2011940939974345 ; 0.2991800071531688 ; 0.3941513470775634 ; 
0.4850818636402397 ; 0.5709721726085388 ; 0.6509967412974170 ; 0.7244177313601700 ; 0.7904185014424659 ; 0.8482065834104272 ;
 0.8972645323440819 ; 0.9372733924007059 ; 0.9677390756791391 ; 0.9879925180204854 ; 0.9980022986933971 ; 0.0 |] ;;

let gauss_kronrod_weights_31 = [| 0.1007698455238756 ; 0.09917359872179196 ; 0.09664272698362368 ; 0.09312659817082532 ;
 0.08856444305621177 ; 0.08308050282313302 ; 0.07684968075772038 ; 0.06985412131872826 ; 0.06200956780067064 ; 0.05348152469092809 ;
 0.04458975132476488 ; 0.03534636079137585 ; 0.02546084732671532 ; 0.01500794732931612 ; 0.05377479872923349 ; 0.001013300070147915 |] ;;

let gauss_kronrod_abscissae_41 = [| 0.07652652113349733 ; 0.1526054652409227 ; 0.2277858511416451 ; 0.3016278681149130 ;
 0.3737060887154196 ; 0.4435931752387251 ; 0.5108670019508271 ; 0.5751404468197103 ; 0.6360536807265150 ; 0.6932376563347514 ;
 0.7463319064601508 ; 0.7950414288375512 ; 0.8391169718222188 ; 0.8782768112522820 ; 0.9122344282513259 ; 0.9408226338317548 ;
 0.9639719272779138 ; 0.9815078774502503 ; 0.9931285991850949 ; 0.9988590315882777 ; 0.0 |] ;;

let gauss_kronrod_weights_41 = [| 0.07637786767208074 ; 0.07570449768455667 ; 0.07458287540049919 ; 0.07303069033278667 ;
 0.07105442355344407 ; 0.06864867292852162 ; 0.06583459713361842 ; 0.06265323755478117 ; 0.05911140088063957 ; 0.05519510534828599 ;
 0.05094457392372869 ; 0.04643482186749767 ; 0.04166887332797369 ; 0.03660016975820080 ; 0.03128730677703280 ; 0.02588213360495116 ;
 0.02038837346126652 ; 0.01462616925697125 ; 0.008600269855642942 ; 0.003073583718520532 ; 0.07660071191799966 |] ;;


(** {C § } *)


(** {v float_romberg_step_gauss_kronrod function a b v} The data come from the source code of [scilab], 
which quotes [quadpack].

Les données proviennent du cose source de [scilab], qui cite [quadpack]. *)
let float_romberg_step_gauss_kronrod = fun (f:float -> float) (a:float) (b:float) ->
 let x = gauss_kronrod_abscissae_21
 and w = gauss_kronrod_weights_21
 and ww = gauss_kronrod_weights_11to21
 and delta = ref 0.
 and valueplus = ref 0.
 and valueminus = ref 0.
 and halflength = ( b -. a ) *. 0.5
 and c = ( a +. b ) *. 0.5
 and accumul = ref 0. in
  let accu = ref ( w.(10) *. f c ) in
   for i = 0 to 9 do
    delta := halflength *. x.(i) ;
    valueplus := f ( c +. !delta ) ;
    valueminus := f ( c -. !delta ) ;
    accu := !accu +. w.(i) *. !valueplus ;
    accu := !accu +. w.(i) *. !valueminus ;
    accumul := !accumul +. ww.(i) *. !valueplus ;
    accumul := !accumul +. ww.(i) *. !valueminus ;
   done ;
   [| halflength *. !accu ; halflength *. !accumul |] ;;


(** {v float_simple_step_gauss_kronrod_generic abscissae weights function a b v}  *)
let float_simple_step_gauss_kronrod_generic = fun (x:float array) (w:float array) (f:float -> float) (a:float) (b:float) ->
 let ll = ( Array.length w ) - 1
 and delta = ref 0.
 and valueplus = ref 0.
 and valueminus = ref 0.
 and halflength = ( b -. a ) *. 0.5
 and c = ( a +. b ) *. 0.5 in
  let accu = ref ( w.(ll) *. f c ) in
   for i = 0 to ll - 1 do
    delta := halflength *. x.(i) ;
    valueplus := f ( c +. !delta ) ;
    valueminus := f ( c -. !delta ) ;
    accu := !accu +. w.(i) *. !valueplus ;
    accu := !accu +. w.(i) *. !valueminus ;
   done ;
   halflength *. !accu ;;


(** {v float_simple_step_gauss_kronrod function a b v} *)
let float_simple_step_gauss_kronrod = fun (f:float -> float) (a:float) (b:float) ->
 ( float_romberg_step_gauss_kronrod f a b ).(0) ;;

(** {v float_int_romberg_gauss_kronrod_adapt tolerance function a b v} *)
let rec float_int_romberg_gauss_kronrod_adapt = fun (tol:float) (f:float -> float) (a:float) (b:float) ->
 let first = float_romberg_step_gauss_kronrod f a b in
  if abs_float ( first.(0) -. first.(1) ) < tol *. abs_float ( first.(0) )
   then first.(0)
  else let c = ( a +. b ) *. 0.5 in
   ( float_int_romberg_gauss_kronrod_adapt tol f a c ) +. ( float_int_romberg_gauss_kronrod_adapt tol f c b ) ;;

(** {v float_int_romberg_gauss_kronrod_bounded maxstages tolerance function a b v} *)
let rec float_int_romberg_gauss_kronrod_bounded = fun (maxstages:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 match maxstages with 
 | 0 -> float_simple_step_gauss_kronrod f a b
 | _ -> 
 let first = float_romberg_step_gauss_kronrod f a b in
  if abs_float ( first.(0) -. first.(1) ) < tol *. abs_float ( first.(0) )
   then first.(0)
  else let c = ( a +. b ) *. 0.5 in
   ( float_int_romberg_gauss_kronrod_bounded ( ( abs maxstages ) - 1 ) tol f a c )
    +. ( float_int_romberg_gauss_kronrod_bounded ( ( abs maxstages ) - 1 ) tol f c b ) ;;


(** {v float_int_tanh nintervals function a b v} *)
let float_int_tanh = fun (n:int) (f:float -> float) (a:float) (b:float) ->
 let nnn = float n
 and halflength = 0.5 *. ( b -. a )
 and center = 0.5 *. ( a +. b ) in
  let h = halfpi *. sqrt ( 2. /. nnn ) -. ( 1. /. nnn )
  and accu = ref 0.
  and nn = 2 * n + 1 in
   let x = Array.mapi ( fun i z -> tanh ( ( float ( i - n ) ) *. h ) ) ( Array.make nn 0. ) in
    let w = Matrix.vector_float_scal_mult h ( Matrix.vector_float_scal_right_sub 1. ( Matrix.vector_float_coeff_prod x x ) ) in
     for i = 0 to nn - 1 do
      accu := !accu +. w.(i) *. ( f ( center +. halflength *. x.(i) ) ) ;
     done ;
     !accu *. halflength ;;


(** {v float_int_tanh_sinh parameter function a b v} *)
let float_int_tanh_sinh = fun (m:int) (f:float -> float) (a:float) (b:float) ->
 let mm = float m
 and halflength = 0.5 *. ( b -. a )
 and center = 0.5 *. ( a +. b ) in
  let coeff = 2. ** mm in
   let n = 8 * ( int_of_float coeff )
   and h = 1. /. coeff
   and accu = ref 0. in
    let nn = 2 * n + 1 in
     let x = Array.mapi ( fun i z -> tanh ( halfpi *. sinh ( ( float ( i - n ) ) *. h ) ) ) ( Array.make nn 0. )
     and xx = Array.mapi ( fun i z -> cosh ( ( float ( i - n ) ) *. h ) ) ( Array.make nn 0. ) in
      let w = Matrix.vector_float_coeff_prod xx ( Matrix.vector_float_scal_mult ( h *. halfpi ) ( Matrix.vector_float_scal_right_sub 1. ( Matrix.vector_float_coeff_prod x x ) ) ) in
       for i = 0 to nn - 1 do
        accu := !accu +. w.(i) *. ( f ( center +. halflength *. x.(i) ) ) ;
       done ;
       !accu *. halflength ;;


(** {C § } *)

(** The following vectors are used to calculate the [float_weighted_int_0_1] method of integration.

Les vecteurs suivants servent à calculer la méthode d'intégration [float_weighted_int_0_1]. *)

(** {C  } *)


let adams_bashforth_1_w = [| 1. |] ;;

let adams_bashforth_2_w = Matrix.vector_float_scal_left_div 2. [| 3. ; -1. |] ;;

let adams_bashforth_3_w = Matrix.vector_float_scal_left_div 12. [| 23. ; -16. ; 5. |] ;;

let adams_bashforth_4_w = Matrix.vector_float_scal_left_div 24. [| 55. ; -59. ; 37. ; -9. |] ;;

let adams_bashforth_5_w = Matrix.vector_float_scal_left_div 720. [| 1901. ; -2774. ; 2616. ; -1274. ; 251. |] ;;

let adams_bashforth_6_w = Matrix.vector_float_scal_left_div 1440. [| 4277. ; -7923. ; 9982. ; -7298. ; 2877. ; -475. |] ;;

let adams_bashforth_7_w = Matrix.vector_float_scal_left_div 60480. [| 198721. ; -447288. ; 705549. ; -688256. ; 407139. ; -134472. ; 19087. |] ;;

let adams_bashforth_8_w = Matrix.vector_float_scal_left_div 120960. [| 434241. ; -1152169. ; 2183877. ; -2664477. ; 2102243. ; -1041723. ; 295767. ; -36799. |] ;;

let adams_bashforth_9_w = Matrix.vector_float_scal_left_div 3628800. [| 14097247. ; -43125206. ;
 95476786. ; -139855262. ; 137968480. ; -91172642. ; 38833486. ; -9664106. ; 1070017. |] ;;

let adams_bashforth_10_w = Matrix.vector_float_scal_left_div 7257600. [| 30277247. ; -104995189. ; 265932680. ; -454661776. ; 538363838. ;
 -444772162. ; 252618224. ; -94307320. ; 20884811. ; -2082753. |] ;;

let adams_bashforth_12_w = Matrix.vector_float_scal_left_div 958003200. [| 4527766399. ; -19433810163. ; 61633227185. ; -135579356757. ;
 214139355366. ; -247741639374. ; 211103573298. ; -131365867290. ; 58189107627. ; -17410248271. ; 3158642445. ; -262747265. |] ;;

let adams_bashforth_14_w = Matrix.vector_float_scal_left_div 5230697472e3 [| 27511554976875. ; -140970750679621. ; 537247052515662. ; -1445313351681906. ; 2854429571790805. ; -4246767353305755. ; 4825671323488452. ;
 -4204551925534524. ; 2793869602879077. ; -1393306307155755. ; 505586141196430. ; -126174972681906. ; 19382853593787. ; -1382741929621. |] ;;

let adams_bashforth_16_w = Matrix.vector_float_scal_left_div 62768369664e3 [| 362555126427073. ; -2161567671248849. ; 9622096909515337. ;
 -30607373860520569. ; 72558117072259733. ; -131963191940828581. ; 187463140112902893. ; -210020588912321949. ; 186087544263596643. ;
 -129930094104237331. ; 70724351582843483. ; -29417910911251819. ; 9038571752734087. ; -1934443196892599. ; 257650275915823. ; -16088129229375.  |] ;;

let adams_bashforth_18_w = Matrix.vector_float_scal_left_div 6402373705728e4 [| 401972381695456831. ; -2735437642844079789. ; 13930159965811142228. ; 
 -51150187791975812900. ; 141500575026572531760. ; -304188128232928718008. ; 518600355541383671092. ; -710171024091234303204. ; 
 786600875277595877750. ; -706174326992944287370. ; 512538584122114046748. ; -298477260353977522892. ; 137563142659866897224. ;
 -49070094880794267600. ; 13071639236569712860. ; -2448689255584545196. ; 287848942064256339. ; -15980174332775873. |] ;;

let adams_bashforth_20_w = Matrix.vector_float_scal_left_div 10218188434341888e4 [| 691668239157222107697. ; -5292843584961252933125. ;
 30349492858024727686755. ; -126346544855927856134295. ; 399537307669842150996468. ; -991168450545135070835076. ; 1971629028083798845750380. ;
 -3191065388846318679544380. ; 4241614331208149947151790. ; -4654326468801478894406214. ; 4222756879776354065593786. ; -3161821089800186539248210. ;
 1943018818982002395655620. ; -970350191086531368649620. ; 387739787034699092364924. ; -121059601023985433003532. ; 28462032496476316665705. ;
 -4740335757093710713245. ; 498669220956647866875. ; -24919383499187492303. |] ;;


let adams_bashforth_m = Array.map Util.reverse_array [| adams_bashforth_1_w ; adams_bashforth_2_w ; adams_bashforth_3_w ; adams_bashforth_4_w ; adams_bashforth_5_w ;
 adams_bashforth_6_w ; adams_bashforth_7_w ; adams_bashforth_8_w ; adams_bashforth_9_w ; adams_bashforth_10_w ; Array.append adams_bashforth_10_w [| 0. |] ; 
 adams_bashforth_12_w ; Array.append adams_bashforth_12_w [| 0. |] ; adams_bashforth_14_w ; Array.append adams_bashforth_14_w [| 0. |] ;
 adams_bashforth_16_w ; Array.append adams_bashforth_16_w [| 0. |] ; adams_bashforth_18_w ; Array.append adams_bashforth_18_w [| 0. |] ; adams_bashforth_20_w |] ;;


let adams_bashforth_x = function (n:int) -> Matrix.float_closed_equal_subdivision 1. n 0. ;;


let adams_moulton_x = adams_bashforth_x ;;


let adams_moulton_1_w = [| 1.0 |] ;;

let adams_moulton_2_w = Matrix.vector_float_scal_left_div 2.0 [| 1.0 ; 1.0 |] ;;

let adams_moulton_3_w = Matrix.vector_float_scal_left_div 12.0 
 [| 5.0 ; 8.0 ; - 1.0 |] ;;

let adams_moulton_4_w = Matrix.vector_float_scal_left_div 24.0 
 [| 9.0 ; 19.0 ; - 5.0 ; 1.0 |] ;;

let adams_moulton_5_w = Matrix.vector_float_scal_left_div 720.0 
 [| 251.0 ; 646.0 ; - 264.0 ; 106.0 ; - 19.0 |] ;;

let adams_moulton_6_w = Matrix.vector_float_scal_left_div 1440.0 
 [| 475.0 ; 1427.0 ; - 798.0 ; 482.0 ; - 173.0 ; 27.0 |] ;;

let adams_moulton_7_w = Matrix.vector_float_scal_left_div 60480.0 
 [| 19087.0 ; 65112.0 ; - 46461.0 ; 37504.0 ; - 20211.0 ; 6312.0 ; - 863.0 |] ;;

let adams_moulton_8_w = Matrix.vector_float_scal_left_div 120960.0 
 [| 36799.0 ; 139849.0 ; - 121797.0 ; 123133.0 ; - 88547.0 ; 41499.0 ; - 11351.0 ; 1375.0 |] ;;

let adams_moulton_9_w = Matrix.vector_float_scal_left_div 3628800.0 
[| 1070017.0 ; 4467094.0 ; - 4604594.0 ; 5595358.0 ; - 5033120.0 ; 3146338.0 ; - 1291214.0 ; 312874.0 ; - 33953.0 |] ;;

let adams_moulton_10_w = Matrix.vector_float_scal_left_div 7257600.0 
 [| 2082753.0 ; 9449717.0 ; - 11271304.0 ; 16002320.0 ; - 17283646.0 ; 13510082.0 ; - 7394032.0 ; 2687864.0 ; - 583435.0 ; 57281.0 |] ;;

let adams_moulton_12_w = Matrix.vector_float_scal_left_div 958003200.0 
 [| 262747265.0 ; 1374799219.0 ; -2092490673.0 ; 3828828885.0 ; -5519460582.0 ;
 6043521486.0 ; -4963166514.0 ; 3007739418.0 ; -1305971115.0 ; 384709327.0 ; -68928781.0 ; 5675265.0 |] ;;

let adams_moulton_14_w = Matrix.vector_float_scal_left_div 5230697472000.0 
 [| 1382741929621.0 ; 8153167962181.0 ; -15141235084110.0 ; 33928990133618.0 ; -61188680131285.0 ; 86180228689563.0 ;
 -94393338653892.0 ; 80101021029180.0 ; -52177910882661.0 ; 25620259777835.0 ; -9181635605134.0 ; 2268078814386.0 ; -345457086395.0 ; 24466579093.0 |] ;;

let adams_moulton_16_w = Matrix.vector_float_scal_left_div 62768369664000.0 
[| 16088129229375.0 ; 105145058757073.0 ; -230992163723849.0 ; 612744541065337.0 ; -1326978663058069.0 ; 2285168598349733.0 ;
 -3129453071993581.0 ; 3414941728852893.0 ; -2966365730265699.0 ; 2039345879546643.0 ; -1096355235402331.0 ; 451403108933483.0 ; -137515713789319.0 ;
 29219384284087.0 ; -3867689367599.0 ; 240208245823.0 |] ;;
 
let adams_moulton_18_w = Matrix.vector_float_scal_left_div 64023737057280000.0 
[| 15980174332775873.0 ; 114329243705491117.0 ; -290470969929371220.0 ; 890337710266029860.0 ; -2250854333681641520.0 ;
 4582441343348851896.0 ; -7532171919277411636.0 ; 10047287575124288740.0 ; -10910555637627652470.0 ; 9644799218032932490.0 ; -6913858539337636636.0 ;
 3985516155854664396.0 ; -1821304040326216520.0 ; 645008976643217360.0 ; -170761422500096220.0 ; 31816981024600492.0 ; -3722582669836627.0 ;
 205804074290625.0 |] ;;

let adams_moulton_20_w = Matrix.vector_float_scal_left_div 102181884343418880000.0
[| 24919383499187492303.0 ; 193280569173472261637.0 ; -558160720115629395555.0 ; 1941395668950986461335.0 ; -5612131802364455926260.0 ;
 13187185898439270330756.0 ; -25293146116627869170796.0 ; 39878419226784442421820.0 ; -51970649453670274135470.0 ; 56154678684618739939910.0 ;
 -50320851025594566473146.0 ; 37297227252822858381906.0 ; -22726350407538133839300.0 ; 11268210124987992327060.0 ; -4474886658024166985340.0 ;
 1389665263296211699212.0 ; -325187970422032795497.0 ; 53935307402575440285.0 ; -5652892248087175675.0 ; 281550972898020815.0 |] ;;


let gauss_legendre_0_1_1_x = [| 0.5 |] ;;

let gauss_legendre_0_1_1_w = [| 1. |] ;;

let gauss_legendre_0_1_2_x = [| 0.5 -. sqrt_of_3 /. 6. ; 0.5 +. sqrt_of_3 /. 6. |]
(** [| 0.2113248654 ; 0.7886751346 |] *) ;;

let gauss_legendre_0_1_2_w = [| 0.5 ; 0.5 |] ;;

let gauss_legendre_0_1_3_x = [| 0.5 -. 0.1 *. ( sqrt_of_15 ) ; 0.5 ; 0.5 +. 0.1 *. ( sqrt_of_15 ) |]
(** [| 0.1127016654 ; 0.5 ; 0.887298334 |] *) ;;

let gauss_legendre_0_1_3_w = [| 5. /. 18. ; 8. /. 18. ; 5. /. 18. |] ;;

let gauss_legendre_0_1_4_x = [| 0.0694318442 ; 0.3300094782 ; 0.6699905218 ; 0.9305681558 |] ;;

let gauss_legendre_0_1_4_w = [| 0.1739274226 ; 0.3260725774 ; 0.3260725774 ; 0.1739274226 |] ;;

let gauss_legendre_0_1_5_x = [| 0.0469100770 ; 0.2307653449 ; 0.5 ; 0.7692346551 ; 0.9530899230 |] ;;

let gauss_legendre_0_1_5_w = [| 0.1184634425 ; 0.2393143352 ; 0.2844444444 ; 0.2393143352 ; 0.1184634425 |] ;;

let gauss_legendre_0_1_6_x = [| 0.0337652429 ; 0.1693953068 ; 0.3806904070 ; 0.6193095930 ; 0.8306046932 ; 0.9662347571 |] ;;

let gauss_legendre_0_1_6_w = [| 0.0856622462 ; 0.1803807865 ; 0.2339569673 ; 0.2339569673 ; 0.1803807865 ; 0.0856622462 |] ;;

let gauss_legendre_0_1_7_x = [| 0.0254460438 ; 0.1292344072 ; 0.2970774243 ; 0.5000000000 ; 0.7029225757 ; 0.8707655928 ; 0.9745539562 |] ;;

let gauss_legendre_0_1_7_w = [| 0.0647424831 ; 0.1398526957 ; 0.1909150253 ; 0.2089795918 ; 0.1909150253 ; 0.1398526957 ; 0.0647424831 |] ;;

let gauss_legendre_0_1_8_x = [| 0.0198550718 ; 0.1016667613 ; 0.2372337950 ; 0.4082826788 ; 0.5917173212 ; 0.7627662050 ; 0.8983332387 ; 0.9801449282 |] ;;

let gauss_legendre_0_1_8_w = [| 0.0506142681 ; 0.1111905172 ; 0.1568533229 ; 0.1813418917 ; 0.1813418917 ; 0.1568533229 ; 0.1111905172 ; 0.0506142681 |] ;;



(** {C  } *)



(** {v float_weighted_int_0_1 abscissae weights function v} The standard abscissae are spread over the interval \[0 ; 1\].

Les abscisses normalisées sont réparties sur l'interavlle \[0 ; 1\]. *)
let float_weighted_int_0_1 = fun (x:float array) (w:float array) (f:float -> float) ->
 let l = Array.length x
 and accu = ref 0. in
  for i = 0 to l - 1 do
   accu := !accu +. w.(i) *. ( f x.(i) ) ;
  done ;
  !accu ;;



(** {C § } *)

(** The following vectors are used to calculate the [float_weighted_int_minus1_1] method of integration.

Les vecteurs suivants servent à calculer la méthode d'intégration [float_weighted_int_minus1_1]. *)

(** {C  } *)




let clenshaw_curtis_1_x = [| 0. |] ;;

let clenshaw_curtis_1_w = [| 2. |] ;;


let clenshaw_curtis_2_x = [| -1. ; 1. |] ;;

let clenshaw_curtis_2_w = [| 1. ; 1. |] ;;


let clenshaw_curtis_3_x = [| -1. ; 0. ; 1. |] ;;

let clenshaw_curtis_3_w = [| 1. /. 3. ; 4. /. 3. ; 1. /. 3. |] ;;


let clenshaw_curtis_4_x = [| -1. ; -0.5 ; 0.5 ; 1. |] ;;

let clenshaw_curtis_4_w = [| 1. /. 9. ; 8. /. 9. ; 8. /. 9. ; 1. /. 9. ; |] ;;


let clenshaw_curtis_x_5_x = [| -1. ; -0.70710678118654752440 ; 0. ; 0.70710678118654752440 ; 1. |] ;;

let clenshaw_curtis_5_w = [| 0.06666666666666666667 ; 0.53333333333333333333 ; 0.80000000000000000000 ; 0.53333333333333333333 ; 0.06666666666666666667 |] ;;


let clenshaw_curtis_6_x = [| -1. ; -0.80901699437494742410 ; -0.30901699437494742410 ; 0.30901699437494742410 ; 0.80901699437493732410 ; 1. |] ;;

let clenshaw_curtis_6_w = [| 0.04 ; 0.36074304120001121619 ; 0.59925695879998878381 ; 0.59925695879998878381 ; 0.36074304120001121619 ; 0.04 |] ;;


let clenshaw_curtis_7_x = [| -1. ; -0.86602540378443864676 ; -0.5 ; 0. ; 0.5 ; 0.86602540378443864676 ; 1. |] ;;

let clenshaw_curtis_7_w = [| 0.02857142857142857143 ; 0.25396825396825396825 ; 0.45714285714285714286 ;
 0.52063492063492063492 ; 0.45714285714285714286 ; 0.25396825396825396825 ; 0.02857142857142857143 |] ;;


let clenshaw_curtis_8_x = [| -1. ; -0.90096886790241912624 ; -0.62348980185873353053 ; -0.22252093395631440429 ;
 0.22252093395631440429 ; 0.62348980185873353053 ; 0.90096886790241910624 ; 1. |] ;;

let clenshaw_curtis_8_w = [| 0.02040816326530612245 ; 0.19014100721820835178 ; 0.35224242371815911533 ; 0.43720840579832641044 ;
 0.43720840579832641044 ; 0.35224242371815911533 ; 0.19014100721820835178 ; 0.02040816326530612245 |] ;;


let clenshaw_curtis_9_x = [| -1. ; -0.92387953251128675613 ; -0.70710678118654752440 ; -0.38268343236508977173 ; 0.00000000000000000000 ;
 0.38268343236508977173 ; 0.70710678118654752440 ; 0.92387953251128675613 ; 1. |] ;;

let clenshaw_curtis_9_w = [| 0.01587301587301587302 ; 0.14621864921601815501 ; 0.27936507936507936508 ; 0.36171785872048978150 ;
 0.39365079365079365079 ; 0.36171785872048978150 ; 0.27936507936507936508 ; 0.14621864921601815501 ; 0.01587301587301587302 |] ;;


let clenshaw_curtis_10_x = [| -1. ; -0.93969262078590838405 ; -0.76604444311897903520 ; -0.50000000000000000000 ; -0.17364817766693034885 ;
 0.17364817766693034885 ; 0.50000000000000000000 ; 0.76604444311897903520 ; 0.93969262078590838405 ; 1. |] ;;

let clenshaw_curtis_10_w = [| 0.01234567901234567901 ; 0.11656745657203712296 ; 0.22528432333810440813 ; 0.30194003527336860670 ; 0.34386250580414418320 ;
 0.34386250580414418320 ; 0.30194003527336860670 ; 0.22528432333810440813 ; 0.11656745657203712296 ; 0.01234567901234567901 |] ;;


let clenshaw_curtis_11_x = [| -1. ; -0.95105651629515357212 ; -0.80901699437494742410 ; -0.58778525229247312917 ; -0.30901699437494742410 ; 0. ;
 0.30901699437494742410 ; 0.58778525229247312917 ; 0.80901699437494742410 ; 0.95105651629515357212 ; 1. |] ;;

let clenshaw_curtis_11_w = [| 0.01010101010101010101 ; 0.09457905488370156116 ; 0.18563521442424776529 ; 0.25358833328368660623 ; 0.29921327042423708320 ;
 0.31376623376623376623 ; 0.29921327042423708320 ; 0.25358833328368660623 ; 0.18563521442424776529 ; 0.09457905488370156116 ; 0.01010101010101010101 |] ;;


let clenshaw_curtis_12_x = [| -1. ; -0.95949297361449738989 ; -0.84125353283118116886 ; -0.65486073394528506406 ; -0.41541501300188642553 ;
 -0.14231483827328514044 ; 0.14231483827328514044 ; 0.41541501300188642553 ; 0.65486073394528506406 ; 0.84125353283118116886 ; 0.95949297361449738989 ; 1. |] ;;

let clenshaw_curtis_12_w = [| 0.00826446280991735537 ; 0.07856015374620000543 ; 0.15504045508256136552 ; 0.21556254600086858099 ;
 0.25991734106691617602 ; 0.28265504129353651666 ; 0.28265504129353651666 ; 0.25991734106691617602 ; 0.21556254600086858099 ;
 0.15504045508256136552 ; 0.07856015374620000543 ; 0.00826446280991735537 |] ;;


let clenshaw_curtis_13_x = [| -1. ; -0.96592582628906828675 ; -0.86602540378443864676 ; -0.70710678118654752440 ; -0.5 ; -0.25881904510252076235 ;
 0.0 ; 0.25881904510252076235 ; 0.5 ; 0.70710678118654752440 ; 0.86602540378443864676 ; 0.96592582628906828675 ; 1. |] ;;

let clenshaw_curtis_13_w = [| 0.00699300699300699301 ; 0.06605742495207439452 ; 0.13154253154253154253 ; 0.18476338476338476338 ;
 0.22697302697302697303 ; 0.25267569378104433860 ; 0.26198986198986198986 ; 0.25267569378104433860 ; 0.22697302697302697303 ;
 0.18476338476338476338 ; 0.13154253154253154253 ; 0.06605742495207439452 ; 0.00699300699300699301 |] ;;


let clenshaw_curtis_14_x = [| -1. ; -0.97094181742605202716 ; -0.88545602565320989590 ; -0.74851074817110109863 ; -0.56806474673115580251 ;
 -0.35460488704253562597 ; -0.12053668025532305335 ;  0.12053668025532305335 ;  0.35460488704253562597 ;  0.56806474673115580251 ;
  0.74851074817110109863 ;  0.88545602565320989590 ; 0.97094181742605202716 ; 1. |] ;;

let clenshaw_curtis_14_w = [| 0.00591715976331360947 ; 0.05646531376341444627 ; 0.11276867248985655881 ; 0.16003802611671868523 ;
 0.19899241036578321848 ; 0.22590304977856444935 ; 0.23991536772234903239 ; 0.23991536772234903239 ; 0.22590304977856444935 ;
 0.19899241036578321848 ; 0.16003802611671868523 ; 0.11276867248985655881 ; 0.05646531376341444627 ; 0.00591715976331360947 |] ;;


let clenshaw_curtis_15_x = [| -1. ; -0.97492791218182360702 ; -0.90096886790241912624 ; -0.78183148246802980871 ; -0.62348980185873353053 ;
 -0.43388373911755812048 ; -0.22252093395631440429 ;  0. ;  0.22252093395631440429 ;  0.43388373911755812048 ;  0.62348980185873353053 ;
  0.78183148246802980871 ;  0.90096886790241912624 ; 0.97492791218182360702 ; 1. |] ;;

let clenshaw_curtis_15_w = [| 0.00512820512820512821 ; 0.04869938729508823855 ; 0.09782039167605215913 ; 0.13966507849560431803 ;
 0.17560578900106674677 ; 0.20205146748238357364 ; 0.21888151163057340180 ; 0.22429633858205286777 ; 0.21888151163057340180 ;
 0.20205146748238357364 ; 0.17560578900106674677 ; 0.13966507849560431803 ; 0.09782039167605215913 ; 0.04869938729508823855 ; 0.00512820512820512821 |] ;;


let clenshaw_curtis_16_x = [| -1. ; -0.97814760073380563793 ; -0.91354545764260089550 ; -0.80901699437494742410 ; -0.66913060635885821383 ;
 -0.5 ; -0.30901699437494742410 ; -0.10452846326765347140 ;  0.10452846326765347140 ;  0.30901699437494742410 ;  0.5 ;  0.66913060635885821383 ;
  0.80901699437494742410 ;  0.91354545764260089550 ;  0.97814760073380563793 ; 1. |] ;;

let clenshaw_curtis_16_w = [| 0.00444444444444444444 ; 0.04251476624752508988 ; 0.08553884025933288291 ; 0.12294010082849361533 ;
 0.15573317603967369176 ; 0.18132978132978132978 ; 0.19921478132638853955 ; 0.20828410952436040635 ; 0.20828410952436040635 ;
 0.19921478132638853955 ; 0.18132978132978132978 ; 0.15573317603967369176 ; 0.12294010082849361533 ; 0.08553884025933288291 ;
 0.04251476624752508988 ; 0.00444444444444444444 |] ;;


let clenshaw_curtis_17_x = [| -1. ; -0.98078528040323044913 ; -0.92387953251128675613 ; -0.83146961230254523708 ; -0.70710678118654752440 ;
 -0.55557023301960222474 ; -0.38268343236508977173 ; -0.19509032201612826785 ;  0. ;  0.19509032201612826785 ;  0.38268343236508977173 ;
  0.55557023301960222474 ;  0.70710678118654752440 ;  0.83146961230254523708 ;  0.92387953251128675613 ;  0.98078528040323044913 ; 1. |] ;;

let clenshaw_curtis_17_w = [| 0.00392156862745098039 ; 0.03736870283720561032 ; 0.07548233154315183441 ; 0.10890555258189093044 ;
 0.13895646836823307412 ; 0.16317266428170330256 ; 0.18147378423649335700 ; 0.19251386461292564687 ; 0.19641012582189052777 ;
 0.19251386461292564687 ; 0.18147378423649335700 ; 0.16317266428170330256 ; 0.13895646836823307412 ; 0.10890555258189093044 ;
 0.07548233154315183441 ; 0.03736870283720561032 ; 0.00392156862745098039 |] ;;


let clenshaw_curtis_33_x = [| -1. ; -0.99518472667219688624 ; -0.98078528040323044913 ; -0.95694033573220886494 ; -0.92387953251128675613 ;
 -0.88192126434835502971 ; -0.83146961230254523708 ; -0.77301045336273696081 ; -0.70710678118654752440 ; -0.63439328416364549822 ;
 -0.55557023301960222474 ; -0.47139673682599764856 ; -0.38268343236508977173 ; -0.29028467725446236764 ; -0.19509032201612826785 ;
 -0.098017140329560601994 ; 0. ; 0.098017140329560601994 ; 0.19509032201612826785 ; 0.29028467725446236764 ; 0.38268343236508977173 ;
 0.47139673682599764856 ; 0.55557023301960222474 ; 0.63439328416364549822 ; 0.70710678118654752440 ; 0.77301045336273696081 ;
 0.83146961230254523708 ; 0.88192126434835502971 ; 0.92387953251128675613 ; 0.95694033573220886494 ; 0.98078528040323044913 ; 0.99518472667219688624 ; 1. |] ;;

let clenshaw_curtis_33_w = [| 0.00097751710654936461 ; 0.00939319796295501470 ; 0.01923424513268114918 ; 0.02845791667723369009 ;
 0.03759434191404720602 ; 0.04626276283775174949 ; 0.05455501630398031044 ; 0.06227210954529400455 ; 0.06942757563043545090 ;
 0.07588380044138847048 ; 0.08163481765493851023 ; 0.08657753844182743544 ; 0.09070611286772099874 ; 0.09394324443876873573 ;
 0.09629232594548817919 ; 0.09769818820805558182 ; 0.09817857778176829677 ; 0.09769818820805558182 ; 0.09629232594548817919 ;
 0.09394324443876873573 ; 0.09070611286772099874 ; 0.08657753844182743544 ; 0.08163481765493851023 ; 0.07588380044138847048 ;
 0.06942757563043545090 ; 0.06227210954529400455 ; 0.05455501630398031044 ; 0.04626276283775174949 ; 0.03759434191404720602 ;
 0.02845791667723369009 ; 0.01923424513268114918 ; 0.00939319796295501470 ; 0.00097751710654936461 |] ;;


let clenshaw_curtis_65_x = [| -1. ; -0.99879545620517239271 ; -0.99518472667219688624 ; -0.98917650996478097345 ; -0.98078528040323044913 ;
 -0.97003125319454399260 ; -0.95694033573220886494 ; -0.94154406518302077841 ; -0.92387953251128675613 ; -0.90398929312344333159 ;
 -0.88192126434835502971 ; -0.85772861000027206990 ; -0.83146961230254523708 ; -0.80320753148064490981 ; -0.77301045336273696081 ;
 -0.74095112535495909118 ; -0.70710678118654752440 ; -0.67155895484701840063 ; -0.63439328416364549822 ; -0.59569930449243334347 ;
 -0.55557023301960222474 ; -0.51410274419322172659 ; -0.47139673682599764856 ; -0.42755509343028209432 ; -0.38268343236508977173 ;
 -0.33688985339222005069 ; -0.29028467725446236764 ; -0.24298017990326388995 ; -0.19509032201612826785 ; -0.14673047445536175166 ;
 -0.098017140329560601994 ; -0.049067674327418014255 ; 0. ; 0.049067674327418014255 ; 0.098017140329560601994 ;
 0.14673047445536175166 ; 0.19509032201612826785 ; 0.24298017990326388995 ; 0.29028467725446236764 ; 0.33688985339222005069 ;
 0.38268343236508977173 ; 0.42755509343028209432 ; 0.47139673682599764856 ; 0.51410274419322172659 ; 0.55557023301960222474 ;
 0.59569930449243334347 ; 0.63439328416364549822 ; 0.67155895484701840063 ; 0.70710678118654752440 ; 0.74095112535495909118 ;
 0.77301045336273696081 ; 0.80320753148064490981 ; 0.83146961230254523708 ; 0.85772861000027206990 ; 0.88192126434835502971 ;
 0.90398929312344333159 ; 0.92387953251128675613 ; 0.94154406518302077841 ; 0.95694033573220886494 ; 0.97003125319454399260 ;
 0.98078528040323044913 ; 0.98917650996478097345 ; 0.99518472667219688624 ; 0.99879545620517239271 ; 1. |] ;;

let clenshaw_curtis_65_w = [| 0.00024420024420024420 ; 0.00235149067531170332 ; 0.00483146544879091264 ; 0.00719269316173611402 ;
 0.00958233879528379039 ; 0.01192339471421277160 ; 0.01425206043235199679 ; 0.01653498765728958965 ; 0.01878652974179578354 ;
 0.02098627442973743378 ; 0.02314069493435819848 ; 0.02523506498175476590 ; 0.02727225714146838686 ; 0.02924065319746833770 ;
 0.03114129710406762447 ; 0.03296454656997632997 ; 0.03471049818092511427 ; 0.03637092028663918309 ; 0.03794545992128481711 ;
 0.03942698871295609976 ; 0.04081501340035783384 ; 0.04210333111141810203 ; 0.04329151496169082935 ; 0.04437417923925731580 ;
 0.04535110955166067221 ; 0.04621766751092557684 ; 0.04697395904661414870 ; 0.04761604458525019296 ; 0.04814443257251220341 ;
 0.04855584485714105274 ; 0.04885125664306609371 ; 0.04902801843102555294 ; 0.04908762351494245585 ; 0.04902801843102555294 ;
 0.04885125664306609371 ; 0.04855584485714105274 ; 0.04814443257251220341 ; 0.04761604458525019296 ; 0.04697395904661414870 ;
 0.04621766751092557684 ; 0.04535110955166067221 ; 0.04437417923925731580 ; 0.04329151496169082935 ; 0.04210333111141810203 ;
 0.04081501340035783384 ; 0.03942698871295609976 ; 0.03794545992128481711 ; 0.03637092028663918309 ; 0.03471049818092511427 ;
 0.03296454656997632997 ; 0.03114129710406762447 ; 0.02924065319746833770 ; 0.02727225714146838686 ; 0.02523506498175476590 ;
 0.02314069493435819848 ; 0.02098627442973743378 ; 0.01878652974179578354 ; 0.01653498765728958965 ; 0.01425206043235199679 ;
 0.01192339471421277160 ; 0.00958233879528379039 ; 0.00719269316173611402 ; 0.00483146544879091264 ; 0.00235149067531170332 ;
 0.00024420024420024420 |] ;;


let clenshaw_curtis_129_x = [| -1. ; -0.99969881869620422012 ; -0.99879545620517239271 ; -0.99729045667869021614 ; -0.99518472667219688624 ;
 -0.99247953459870999816 ; -0.98917650996478097345 ; -0.98527764238894124477 ; -0.98078528040323044913 ; -0.97570213003852854446 ;
 -0.97003125319454399260 ; -0.96377606579543986669 ; -0.95694033573220886494 ; -0.94952818059303666720 ; -0.94154406518302077841 ;
 -0.93299279883473888771 ; -0.92387953251128675613 ; -0.91420975570353065464 ; -0.90398929312344333159 ; -0.89322430119551532034 ;
 -0.88192126434835502971 ; -0.87008699110871141865 ; -0.85772861000027206990 ; -0.84485356524970707326 ; -0.83146961230254523708 ;
 -0.81758481315158369650 ; -0.80320753148064490981 ; -0.78834642762660626201 ; -0.77301045336273696081 ; -0.75720884650648454758 ;
 -0.74095112535495909118 ; -0.72424708295146692094 ; -0.70710678118654752440 ; -0.68954054473706692462 ; -0.67155895484701840063 ;
 -0.65317284295377676408 ; -0.63439328416364549822 ; -0.61523159058062684548 ; -0.59569930449243334347 ; -0.57580819141784530075 ;
 -0.55557023301960222474 ; -0.53499761988709721066 ; -0.51410274419322172659 ; -0.49289819222978403687 ; -0.47139673682599764856 ;
 -0.44961132965460660005 ; -0.42755509343028209432 ; -0.40524131400498987091 ; -0.38268343236508977173 ; -0.35989503653498814878 ;
 -0.33688985339222005069 ; -0.31368174039889147666 ; -0.29028467725446236764 ; -0.26671275747489838633 ; -0.24298017990326388995 ;
 -0.21910124015686979723 ; -0.19509032201612826785 ; -0.17096188876030122636 ; -0.14673047445536175166 ; -0.12241067519921619850 ;
 -0.098017140329560601994 ; -0.073564563599667423529 ; -0.049067674327418014255 ; -0.024541228522912288032 ; 0. ; 0.024541228522912288032 ;
 0.049067674327418014255 ; 0.073564563599667423529 ; 0.098017140329560601994 ; 0.12241067519921619850 ; 0.14673047445536175166 ;
 0.17096188876030122636 ; 0.19509032201612826785 ; 0.21910124015686979723 ; 0.24298017990326388995 ; 0.26671275747489838633 ;
 0.29028467725446236764 ; 0.31368174039889147666 ; 0.33688985339222005069 ; 0.35989503653498814878 ; 0.38268343236508977173 ;
 0.40524131400498987091 ; 0.42755509343028209432 ; 0.44961132965460660005 ; 0.47139673682599764856 ; 0.49289819222978403687 ;
 0.51410274419322172659 ; 0.53499761988709721066 ; 0.55557023301960222474 ; 0.57580819141784530075 ; 0.59569930449243334347 ;
 0.61523159058062684548 ; 0.63439328416364549822 ; 0.65317284295377676408 ; 0.67155895484701840063 ; 0.68954054473706692462 ;
 0.70710678118654752440 ; 0.72424708295146692094 ; 0.74095112535495909118 ; 0.75720884650648454758 ; 0.77301045336273696081 ;
 0.78834642762660626201 ; 0.80320753148064490981 ; 0.81758481315158369650 ; 0.83146961230254523708 ; 0.84485356524970707326 ;
 0.85772861000027206990 ; 0.87008699110871141865 ; 0.88192126434835502971 ; 0.89322430119551532034 ; 0.90398929312344333159 ;
 0.91420975570353065464 ; 0.92387953251128675613 ; 0.93299279883473888771 ; 0.94154406518302077841 ; 0.94952818059303666720 ;
 0.95694033573220886494 ; 0.96377606579543986669 ; 0.97003125319454399260 ; 0.97570213003852854446 ; 0.98078528040323044913 ;
 0.98527764238894124477 ; 0.98917650996478097345 ; 0.99247953459870999816 ; 0.99518472667219688624 ; 0.99729045667869021614 ;
 0.99879545620517239271 ; 0.99969881869620422012 ; 1. |] ;;

let clenshaw_curtis_129_w = [| 0.00006103888176768602 ; 0.00058807215382869754 ; 0.00120930061875273991 ; 0.00180308126695362360 ;
 0.00240715327877140915 ; 0.00300345869904497128 ; 0.00360197835812614147 ; 0.00419553798718534675 ; 0.00478862143341336763 ;
 0.00537724746840184621 ; 0.00596388034730799521 ; 0.00654590843862298928 ; 0.00712483332325489785 ; 0.00769875778896082811 ;
 0.00826865154203087108 ; 0.00883303867470133581 ; 0.00939256583934814871 ; 0.00994602784923457905 ; 0.01049386202576892125 ;
 0.01103504877427254184 ; 0.01156988348290849967 ; 0.01209748052807164113 ; 0.01261803597977743271 ; 0.01313076516693974630 ;
 0.01363579321293772047 ; 0.01413241437853094133 ; 0.01462070254634350205 ; 0.01510001572479266783 ; 0.01557039073899425960 ;
 0.01603123858745057916 ; 0.01648256956220377909 ; 0.01692383985846499368 ; 0.01735504125411394958 ; 0.01777566938875279997 ;
 0.01818570377926339481 ; 0.01858467519566908661 ; 0.01897255587067948426 ; 0.01934890842392451844 ; 0.01971370183700155725 ;
 0.02006652805198357604 ; 0.02040735612003867863 ; 0.02073580533490147816 ; 0.02105184759002011131 ; 0.02135512797425970725 ;
 0.02164562356712882440 ; 0.02192300400598756892 ; 0.02218725355897195088 ; 0.02243806539722630184 ; 0.02267543270456671718 ;
 0.02289907134390605882 ; 0.02310898491627407168 ; 0.02330491126131143273 ; 0.02348686571193163505 ; 0.02365460746057766523 ;
 0.02380816473024258975 ; 0.02394731750476901502 ; 0.02407210792327850000 ; 0.02418233623893147567 ; 0.02427805942075745923 ;
 0.02435909748927643184 ; 0.02442552306156708690 ; 0.02447717542743444284 ; 0.02451414358881568292 ; 0.02453628559651495473 ;
 0.02454370750551418263 ; 0.02453628559651495473 ; 0.02451414358881568292 ; 0.02447717542743444284 ; 0.02442552306156708690 ;
 0.02435909748927643184 ; 0.02427805942075745923 ; 0.02418233623893147567 ; 0.02407210792327850000 ; 0.02394731750476901502 ;
 0.02380816473024258975 ; 0.02365460746057766523 ; 0.02348686571193163505 ; 0.02330491126131143273 ; 0.02310898491627407168 ;
 0.02289907134390605882 ; 0.02267543270456671718 ; 0.02243806539722630184 ; 0.02218725355897195088 ; 0.02192300400598756892 ;
 0.02164562356712882440 ; 0.02135512797425970725 ; 0.02105184759002011131 ; 0.02073580533490147816 ; 0.02040735612003867863 ;
 0.02006652805198357604 ; 0.01971370183700155725 ; 0.01934890842392451844 ; 0.01897255587067948426 ; 0.01858467519566908661 ;
 0.01818570377926339481 ; 0.01777566938875279997 ; 0.01735504125411394958 ; 0.01692383985846499368 ; 0.01648256956220377909 ;
 0.01603123858745057916 ; 0.01557039073899425960 ; 0.01510001572479266783 ; 0.01462070254634350205 ; 0.01413241437853094133 ;
 0.01363579321293772047 ; 0.01313076516693974630 ; 0.01261803597977743271 ; 0.01209748052807164113 ; 0.01156988348290849967 ;
 0.01103504877427254184 ; 0.01049386202576892125 ; 0.00994602784923457905 ; 0.00939256583934814871 ; 0.00883303867470133581 ;
 0.00826865154203087108 ; 0.00769875778896082811 ; 0.00712483332325489785 ; 0.00654590843862298928 ; 0.00596388034730799521 ;
 0.00537724746840184621 ; 0.00478862143341336763 ; 0.00419553798718534675 ; 0.00360197835812614147 ; 0.00300345869904497128 ;
 0.00240715327877140915 ; 0.00180308126695362360 ; 0.00120930061875273991 ; 0.00058807215382869754 ; 0.00006103888176768602 |] ;;


let gauss_legendre_1_x = [| 0.0 |] ;;

let gauss_legendre_1_w = [| 2.0 |] ;;


let gauss_legendre_2_x = [| -0.577350269189625764509148780502 ; 0.577350269189625764509148780502 |] ;;

let gauss_legendre_2_w = [| 1.0 ; 1.0 |] ;;


let gauss_legendre_3_x = [| -0.774596669241483377035853079956 ; 0.0 ; 0.774596669241483377035853079956 |] ;;

let gauss_legendre_3_w = [| 0.555555555555555555555555555556 ; 0.888888888888888888888888888889 ; 0.555555555555555555555555555556 |] ;;


let gauss_legendre_4_x = [| -0.861136311594052575223946488893 ; -0.339981043584856264802665759103 ; 0.339981043584856264802665759103 ; 0.861136311594052575223946488893 |] ;;

let gauss_legendre_4_w = [| 0.347854845137453857373063949222 ; 0.652145154862546142626936050778 ; 0.652145154862546142626936050778 ; 0.347854845137453857373063949222 |] ;;


let gauss_legendre_5_x = [| -0.906179845938663992797626878299 ; -0.538469310105683091036314420700 ;
 0.0 ; 0.538469310105683091036314420700 ; 0.906179845938663992797626878299 |] ;;

let gauss_legendre_5_w = [| 0.236926885056189087514264040720 ; 0.478628670499366468041291514836 ; 0.568888888888888888888888888889 ;
 0.478628670499366468041291514836 ; 0.236926885056189087514264040720 |] ;;


let gauss_legendre_6_x = [| -0.932469514203152027812301554494 ; -0.661209386466264513661399595020 ; -0.238619186083196908630501721681 ;
 0.238619186083196908630501721681 ; 0.661209386466264513661399595020 ; 0.932469514203152027812301554494 |] ;;

let gauss_legendre_6_w = [| 0.171324492379170345040296142173 ; 0.360761573048138607569833513838 ; 0.467913934572691047389870343990 ;
 0.467913934572691047389870343990 ; 0.360761573048138607569833513838 ; 0.171324492379170345040296142173 |] ;;


let gauss_legendre_7_x = [| -0.949107912342758524526189684048 ; -0.741531185599394439863864773281 ; -0.405845151377397166906606412077 ;
 0.0 ; 0.405845151377397166906606412077 ; 0.741531185599394439863864773281 ; 0.949107912342758524526189684048 |] ;;

let gauss_legendre_7_w = [| 0.129484966168869693270611432679 ; 0.279705391489276667901467771424 ; 0.381830050505118944950369775489 ; 0.417959183673469387755102040816 ;
 0.381830050505118944950369775489 ; 0.279705391489276667901467771424 ; 0.129484966168869693270611432679 |] ;;


let gauss_legendre_8_x = [| -0.960289856497536231683560868569 ; -0.796666477413626739591553936476 ; -0.525532409916328985817739049189 ; -0.183434642495649804939476142360 ;
 0.183434642495649804939476142360 ; 0.525532409916328985817739049189 ; 0.796666477413626739591553936476 ; 0.960289856497536231683560868569 |] ;;

let gauss_legendre_8_w = [| 0.101228536290376259152531354310 ; 0.222381034453374470544355994426 ; 0.313706645877887287337962201987 ; 0.362683783378361982965150449277 ;
 0.362683783378361982965150449277 ; 0.313706645877887287337962201987 ; 0.222381034453374470544355994426 ; 0.101228536290376259152531354310 |] ;;


let gauss_legendre_9_x = [| -0.968160239507626089835576203 ; -0.836031107326635794299429788 ; -0.613371432700590397308702039 ; -0.324253423403808929038538015 ;
 0.0 ; 0.324253423403808929038538015 ; 0.613371432700590397308702039 ; 0.836031107326635794299429788 ; 0.968160239507626089835576203 |] ;;

let gauss_legendre_9_w = [| 0.081274388361574411971892158111 ; 0.18064816069485740405847203124 ; 0.26061069640293546231874286942 ; 0.31234707704000284006863040658 ;
 0.33023935500125976316452506929 ; 0.31234707704000284006863040658 ; 0.26061069640293546231874286942 ; 0.18064816069485740405847203124 ; 0.081274388361574411971892158111 |] ;;


let gauss_legendre_10_x = [| -0.973906528517171720077964012 ; -0.865063366688984510732096688 ; -0.679409568299024406234327365 ; -0.433395394129247190799265943 ;
 -0.148874338981631210884826001 ; 0.148874338981631210884826001 ; 0.433395394129247190799265943 ; 0.679409568299024406234327365 ; 0.865063366688984510732096688 ;
 0.973906528517171720077964012 |] ;;

let gauss_legendre_10_w = [| 0.066671344308688137593568809893 ; 0.14945134915058059314577633966 ; 0.21908636251598204399553493423 ; 0.26926671930999635509122692157 ;
 0.29552422471475287017389299465 ; 0.29552422471475287017389299465 ; 0.26926671930999635509122692157 ; 0.21908636251598204399553493423 ; 0.14945134915058059314577633966 ;
 0.066671344308688137593568809893 |] ;;


let gauss_legendre_11_x = [| -0.978228658146056992803938001 ; -0.887062599768095299075157769 ; -0.730152005574049324093416252 ; -0.519096129206811815925725669 ;
 -0.269543155952344972331531985 ; 0.0 ; 0.269543155952344972331531985 ; 0.519096129206811815925725669 ; 0.730152005574049324093416252 ;
 0.887062599768095299075157769 ; 0.978228658146056992803938001 |] ;;

let gauss_legendre_11_w = [| 0.055668567116173666482753720443 ; 0.12558036946490462463469429922 ; 0.18629021092773425142609764143 ; 0.23319376459199047991852370484 ;
 0.26280454451024666218068886989 ; 0.27292508677790063071448352834 ; 0.26280454451024666218068886989 ; 0.23319376459199047991852370484 ; 0.18629021092773425142609764143 ;
 0.12558036946490462463469429922 ; 0.055668567116173666482753720443 |] ;;


let gauss_legendre_12_x = [| -0.981560634246719250690549090 ; -0.904117256370474856678465866 ; -0.769902674194304687036893833 ; -0.587317954286617447296702419 ;
 -0.367831498998180193752691537 ; -0.125233408511468915472441369 ; 0.125233408511468915472441369 ; 0.367831498998180193752691537 ; 0.587317954286617447296702419 ;
 0.769902674194304687036893833 ; 0.904117256370474856678465866 ; 0.981560634246719250690549090 |] ;;

let gauss_legendre_12_w = [| 0.047175336386511827194615961485 ; 0.10693932599531843096025471819 ; 0.16007832854334622633465252954 ; 0.20316742672306592174906445581 ;
 0.23349253653835480876084989892 ; 0.24914704581340278500056243604 ; 0.24914704581340278500056243604 ; 0.23349253653835480876084989892 ; 0.20316742672306592174906445581 ;
 0.16007832854334622633465252954 ; 0.10693932599531843096025471819 ; 0.047175336386511827194615961485 |] ;;


let gauss_legendre_13_x = [| -0.984183054718588149472829449 ; -0.917598399222977965206547837 ; -0.801578090733309912794206490 ; -0.642349339440340220643984607 ;
 -0.448492751036446852877912852 ; -0.230458315955134794065528121 ; 0.0 ; 0.230458315955134794065528121 ; 0.448492751036446852877912852 ; 0.642349339440340220643984607 ;
 0.80157809073330991279420649 ; 0.91759839922297796520654784 ; 0.98418305471858814947282945 |] ;;

let gauss_legendre_13_w = [| 0.040484004765315879520021592201 ; 0.092121499837728447914421775954 ; 0.13887351021978723846360177687 ; 0.17814598076194573828004669200 ;
 0.20781604753688850231252321931 ; 0.22628318026289723841209018604 ; 0.23255155323087391019458951527 ; 0.22628318026289723841209018604 ; 0.20781604753688850231252321931 ;
 0.17814598076194573828004669200 ; 0.13887351021978723846360177687 ; 0.092121499837728447914421775954 ; 0.040484004765315879520021592201 |] ;;


let gauss_legendre_14_x = [| -0.986283808696812338841597267 ; -0.928434883663573517336391139 ; -0.827201315069764993189794743 ; -0.687292904811685470148019803 ;
 -0.515248636358154091965290719 ; -0.319112368927889760435671824 ; -0.108054948707343662066244650 ; 0.108054948707343662066244650 ; 0.31911236892788976043567182 ;
 0.51524863635815409196529072 ; 0.68729290481168547014801980 ; 0.82720131506976499318979474 ; 0.92843488366357351733639114 ; 0.98628380869681233884159727 |] ;;

let gauss_legendre_14_w = [| 0.035119460331751863031832876138 ; 0.08015808715976020980563327706 ; 0.12151857068790318468941480907 ; 0.15720316715819353456960193862 ;
 0.18553839747793781374171659013 ; 0.20519846372129560396592406566 ; 0.21526385346315779019587644332 ; 0.21526385346315779019587644332 ; 0.20519846372129560396592406566 ;
 0.18553839747793781374171659013 ; 0.15720316715819353456960193862 ; 0.12151857068790318468941480907 ; 0.08015808715976020980563327706 ; 0.035119460331751863031832876138 |] ;;


let gauss_legendre_15_x = [| -0.987992518020485428489565719 ; -0.937273392400705904307758948 ; -0.848206583410427216200648321 ; -0.724417731360170047416186055 ;
 -0.570972172608538847537226737 ; -0.394151347077563369897207371 ; -0.201194093997434522300628303 ; 0.0 ; 0.20119409399743452230062830 ; 0.39415134707756336989720737 ;
 0.57097217260853884753722674 ; 0.72441773136017004741618605 ; 0.84820658341042721620064832 ; 0.93727339240070590430775895 ; 0.98799251802048542848956572 |] ;;

let gauss_legendre_15_w = [| 0.030753241996117268354628393577 ; 0.070366047488108124709267416451 ; 0.107159220467171935011869546686 ; 0.13957067792615431444780479451 ;
 0.16626920581699393355320086048 ; 0.18616100001556221102680056187 ; 0.19843148532711157645611832644 ; 0.20257824192556127288062019997 ; 0.19843148532711157645611832644 ;
 0.18616100001556221102680056187 ; 0.16626920581699393355320086048 ; 0.13957067792615431444780479451 ; 0.107159220467171935011869546686 ; 0.070366047488108124709267416451 ;
 0.030753241996117268354628393577 |] ;;


let gauss_legendre_16_x = [| -0.989400934991649932596154173 ; -0.944575023073232576077988416 ; -0.865631202387831743880467898 ; -0.755404408355003033895101195 ;
 -0.617876244402643748446671764 ; -0.458016777657227386342419443 ; -0.281603550779258913230460501 ; -0.09501250983763744018531934 ; 0.09501250983763744018531934 ;
 0.28160355077925891323046050 ; 0.45801677765722738634241944 ; 0.61787624440264374844667176 ; 0.75540440835500303389510119 ; 0.86563120238783174388046790 ;
 0.94457502307323257607798842 ; 0.98940093499164993259615417 |] ;;

let gauss_legendre_16_w = [| 0.027152459411754094851780572456 ; 0.062253523938647892862843836994 ; 0.09515851168249278480992510760 ; 0.12462897125553387205247628219 ;
 0.14959598881657673208150173055 ; 0.16915651939500253818931207903 ; 0.18260341504492358886676366797 ; 0.18945061045506849628539672321 ; 0.18945061045506849628539672321 ;
 0.18260341504492358886676366797 ; 0.16915651939500253818931207903 ; 0.14959598881657673208150173055 ; 0.12462897125553387205247628219 ; 0.09515851168249278480992510760 ;
 0.062253523938647892862843836994 ; 0.027152459411754094851780572456 |] ;;


let gauss_legendre_17_x = [| -0.990575475314417335675434020 ; -0.950675521768767761222716958 ; -0.880239153726985902122955694 ; -0.781514003896801406925230056 ;
 -0.657671159216690765850302217 ; -0.512690537086476967886246569 ; -0.35123176345387631529718552 ; -0.17848418149584785585067749 ; 0.0 ; 0.17848418149584785585067749 ;
 0.35123176345387631529718552 ; 0.51269053708647696788624657 ; 0.65767115921669076585030222 ; 0.78151400389680140692523006 ; 0.88023915372698590212295569 ;
 0.95067552176876776122271696 ; 0.99057547531441733567543402 |] ;;

let gauss_legendre_17_w = [| 0.024148302868547931960110026288 ; 0.055459529373987201129440165359 ; 0.085036148317179180883535370191 ; 0.111883847193403971094788385626 ;
 0.13513636846852547328631998170 ; 0.15404576107681028808143159480 ; 0.16800410215645004450997066379 ; 0.17656270536699264632527099011 ; 0.17944647035620652545826564426 ;
 0.17656270536699264632527099011 ; 0.16800410215645004450997066379 ; 0.15404576107681028808143159480 ; 0.13513636846852547328631998170 ; 0.111883847193403971094788385626 ;
 0.085036148317179180883535370191 ; 0.055459529373987201129440165359 ; 0.024148302868547931960110026288 |] ;;


let gauss_legendre_18_x = [| -0.991565168420930946730016005 ; -0.955823949571397755181195893 ; -0.892602466497555739206060591 ; -0.803704958972523115682417455 ;
 -0.691687043060353207874891081 ; -0.55977083107394753460787155 ; -0.41175116146284264603593179 ; -0.25188622569150550958897285 ; -0.08477501304173530124226185 ;
 0.08477501304173530124226185 ; 0.25188622569150550958897285 ; 0.41175116146284264603593179 ; 0.55977083107394753460787155 ; 0.69168704306035320787489108 ;
 0.80370495897252311568241746 ; 0.89260246649755573920606059 ; 0.95582394957139775518119589 ; 0.99156516842093094673001600 |] ;;

let gauss_legendre_18_w = [| 0.021616013526483310313342710266 ; 0.049714548894969796453334946203 ; 0.07642573025488905652912967762 ; 0.10094204410628716556281398492 ;
 0.12255520671147846018451912680 ; 0.14064291467065065120473130375 ; 0.15468467512626524492541800384 ; 0.16427648374583272298605377647 ; 0.16914238296314359184065647013 ;
 0.16914238296314359184065647013 ; 0.16427648374583272298605377647 ; 0.15468467512626524492541800384 ; 0.14064291467065065120473130375 ; 0.12255520671147846018451912680 ;
 0.10094204410628716556281398492 ; 0.07642573025488905652912967762 ; 0.049714548894969796453334946203 ; 0.021616013526483310313342710266 |] ;;


let gauss_legendre_19_x = [| -0.992406843843584403189017670 ; -0.960208152134830030852778841 ; -0.903155903614817901642660929 ; -0.822714656537142824978922487 ;
 -0.72096617733522937861709586 ; -0.60054530466168102346963816 ; -0.46457074137596094571726715 ; -0.31656409996362983199011733 ; -0.16035864564022537586809612 ;
 0.0 ; 0.16035864564022537586809612 ; 0.31656409996362983199011733 ; 0.46457074137596094571726715 ; 0.60054530466168102346963816 ; 0.72096617733522937861709586 ;
 0.82271465653714282497892249 ; 0.90315590361481790164266093 ; 0.96020815213483003085277884 ; 0.99240684384358440318901767 |] ;;

let gauss_legendre_19_w = [| 0.019461788229726477036312041464 ; 0.044814226765699600332838157402 ; 0.069044542737641226580708258006 ; 0.091490021622449999464462094124 ;
 0.111566645547333994716023901682 ; 0.12875396253933622767551578486 ; 0.14260670217360661177574610944 ; 0.15276604206585966677885540090 ; 0.15896884339395434764995643946 ;
 0.16105444984878369597916362532 ; 0.15896884339395434764995643946 ; 0.15276604206585966677885540090 ; 0.14260670217360661177574610944 ; 0.12875396253933622767551578486 ;
 0.111566645547333994716023901682 ; 0.091490021622449999464462094124 ; 0.069044542737641226580708258006 ; 0.044814226765699600332838157402 ; 0.019461788229726477036312041464 |] ;;


let gauss_legendre_20_x = [| -0.993128599185094924786122388 ; -0.963971927277913791267666131 ; -0.912234428251325905867752441 ; -0.83911697182221882339452906 ;
 -0.74633190646015079261430507 ; -0.63605368072651502545283670 ; -0.51086700195082709800436405 ; -0.37370608871541956067254818 ; -0.22778585114164507808049620 ;
 -0.07652652113349733375464041 ; 0.07652652113349733375464041 ; 0.22778585114164507808049620 ; 0.37370608871541956067254818 ; 0.51086700195082709800436405 ;
 0.63605368072651502545283670 ; 0.74633190646015079261430507 ; 0.83911697182221882339452906 ; 0.91223442825132590586775244 ; 0.96397192727791379126766613 ;
 0.99312859918509492478612239 |] ;;

let gauss_legendre_20_w = [| 0.017614007139152118311861962352 ; 0.040601429800386941331039952275 ; 0.062672048334109063569506535187 ; 0.08327674157670474872475814322 ;
 0.10193011981724043503675013548 ; 0.11819453196151841731237737771 ; 0.13168863844917662689849449975 ; 0.14209610931838205132929832507 ; 0.14917298647260374678782873700 ;
 0.15275338713072585069808433195 ; 0.15275338713072585069808433195 ; 0.14917298647260374678782873700 ; 0.14209610931838205132929832507 ; 0.13168863844917662689849449975 ;
 0.11819453196151841731237737771 ; 0.10193011981724043503675013548 ; 0.08327674157670474872475814322 ; 0.062672048334109063569506535187 ; 0.040601429800386941331039952275 ;
 0.017614007139152118311861962352 |] ;;


let gauss_legendre_21_x = [| -0.99375217062038950026024204 ; -0.96722683856630629431662221 ; -0.92009933415040082879018713 ; -0.85336336458331728364725064 ;
 -0.76843996347567790861587785 ; -0.66713880419741231930596667 ; -0.55161883588721980705901880 ; -0.42434212020743878357366889 ; -0.28802131680240109660079252 ;
 -0.14556185416089509093703098 ; 0.0 ; 0.14556185416089509093703098 ; 0.28802131680240109660079252 ; 0.42434212020743878357366889 ; 0.55161883588721980705901880 ;
 0.66713880419741231930596667 ; 0.76843996347567790861587785 ; 0.85336336458331728364725064 ; 0.92009933415040082879018713 ; 0.96722683856630629431662221 ;
 0.99375217062038950026024204 |] ;;

let gauss_legendre_21_w = [| 0.016017228257774333324224616858 ; 0.036953789770852493799950668299 ; 0.057134425426857208283635826472 ; 0.076100113628379302017051653300 ;
 0.093444423456033861553289741114 ; 0.108797299167148377663474578070 ; 0.12183141605372853419536717713 ; 0.13226893863333746178105257450 ; 0.13988739479107315472213342387 ;
 0.14452440398997005906382716655 ; 0.14608113364969042719198514768 ; 0.14452440398997005906382716655 ; 0.13988739479107315472213342387 ;  0.13226893863333746178105257450 ;
 0.12183141605372853419536717713 ; 0.108797299167148377663474578070 ; 0.093444423456033861553289741114 ; 0.076100113628379302017051653300 ; 0.057134425426857208283635826472 ;
 0.036953789770852493799950668299 ; 0.016017228257774333324224616858 |] ;;


let gauss_legendre_22_x = [| -0.99429458548239929207303142 ; -0.97006049783542872712395099 ; -0.92695677218717400052069294 ; -0.86581257772030013653642564 ;
 -0.78781680597920816200427796 ; -0.69448726318668278005068984 ; -0.58764040350691159295887693 ; -0.46935583798675702640633071 ; -0.34193582089208422515814742 ;
 -0.20786042668822128547884653 ; -0.06973927331972222121384180 ; 0.06973927331972222121384180 ; 0.20786042668822128547884653 ; 0.34193582089208422515814742 ;
 0.46935583798675702640633071 ; 0.58764040350691159295887693 ; 0.69448726318668278005068984 ; 0.78781680597920816200427796 ; 0.86581257772030013653642564 ;
 0.92695677218717400052069294 ; 0.97006049783542872712395099 ; 0.99429458548239929207303142 |] ;;
 
let gauss_legendre_22_w = [| 0.014627995298272200684991098047 ; 0.033774901584814154793302246866 ; 0.052293335152683285940312051273 ; 0.06979646842452048809496141893 ;
 0.08594160621706772741444368137 ; 0.10041414444288096493207883783 ; 0.11293229608053921839340060742 ; 0.12325237681051242428556098615 ; 0.13117350478706237073296499253 ;
 0.13654149834601517135257383123 ; 0.13925187285563199337541024834 ; 0.13925187285563199337541024834 ; 0.13654149834601517135257383123 ; 0.13117350478706237073296499253 ;
 0.12325237681051242428556098615 ; 0.11293229608053921839340060742 ; 0.10041414444288096493207883783 ; 0.08594160621706772741444368137 ; 0.06979646842452048809496141893 ;
 0.052293335152683285940312051273 ; 0.033774901584814154793302246866 ; 0.014627995298272200684991098047 |] ;;


let gauss_legendre_23_x = [| -0.99476933499755212352392572 ; -0.97254247121811523195602408 ; -0.93297108682601610234919699 ; -0.87675235827044166737815689 ;
 -0.80488840161883989215111841 ; -0.71866136313195019446162448 ; -0.61960987576364615638509731 ; -0.50950147784600754968979305 ; -0.39030103803029083142148887 ;
 -0.26413568097034493053386954 ; -0.13325682429846611093174268 ; 0.0 ; 0.13325682429846611093174268 ; 0.26413568097034493053386954 ; 0.39030103803029083142148887 ;
 0.50950147784600754968979305 ; 0.61960987576364615638509731 ; 0.71866136313195019446162448 ; 0.80488840161883989215111841 ; 0.87675235827044166737815689 ;
 0.93297108682601610234919699 ; 0.97254247121811523195602408 ; 0.99476933499755212352392572 |] ;;

let gauss_legendre_23_w = [| 0.013411859487141772081309493459 ; 0.030988005856979444310694219642 ; 0.048037671731084668571641071632 ; 0.064232421408525852127169615159 ;
 0.079281411776718954922892524742 ; 0.092915766060035147477018617370 ; 0.104892091464541410074086185015 ; 0.11499664022241136494164351293 ; 0.12304908430672953046757840067 ;
 0.12890572218808214997859533940 ; 0.13246203940469661737164246470 ; 0.13365457218610617535145711055 ; 0.13246203940469661737164246470 ; 0.12890572218808214997859533940 ;
 0.12304908430672953046757840067 ; 0.11499664022241136494164351293 ; 0.104892091464541410074086185015 ; 0.092915766060035147477018617370 ; 0.079281411776718954922892524742 ;
 0.064232421408525852127169615159 ; 0.048037671731084668571641071632 ; 0.030988005856979444310694219642 ; 0.013411859487141772081309493459 |] ;;


let gauss_legendre_24_x = [| -0.99518721999702136017999741 ; -0.97472855597130949819839199 ; -0.93827455200273275852364900 ; -0.88641552700440103421315434 ;
 -0.82000198597390292195394987 ; -0.74012419157855436424382810 ; -0.64809365193697556925249579 ; -0.54542147138883953565837562 ; -0.43379350762604513848708423 ;
 -0.31504267969616337438679329 ; -0.19111886747361630915863982 ; -0.06405689286260562608504308 ; 0.06405689286260562608504308 ; 0.19111886747361630915863982 ;
 0.31504267969616337438679329 ; 0.43379350762604513848708423 ; 0.54542147138883953565837562 ; 0.64809365193697556925249579 ; 0.74012419157855436424382810 ;
 0.82000198597390292195394987 ; 0.88641552700440103421315434 ; 0.93827455200273275852364900 ; 0.97472855597130949819839199 ; 0.99518721999702136017999741 |] ;;

let gauss_legendre_24_w = [| 0.012341229799987199546805667070 ; 0.028531388628933663181307815952 ; 0.044277438817419806168602748211 ; 0.059298584915436780746367758500 ;
 0.07334648141108030573403361525 ; 0.08619016153195327591718520298 ; 0.09761865210411388826988066446 ; 0.10744427011596563478257734245 ; 0.11550566805372560135334448391 ;
 0.12167047292780339120446315348 ; 0.12583745634682829612137538251 ; 0.12793819534675215697405616522 ; 0.12793819534675215697405616522 ; 0.12583745634682829612137538251 ;
 0.12167047292780339120446315348 ; 0.11550566805372560135334448391 ; 0.10744427011596563478257734245 ; 0.09761865210411388826988066446 ; 0.08619016153195327591718520298 ;
 0.07334648141108030573403361525 ; 0.059298584915436780746367758500 ; 0.044277438817419806168602748211 ; 0.028531388628933663181307815952 ; 0.012341229799987199546805667070 |] ;;


let gauss_legendre_25_x = [| -0.99555696979049809790878495 ; -0.97666392145951751149831539 ; -0.94297457122897433941401117 ; -0.89499199787827536885104201 ;
 -0.83344262876083400142102111 ; -0.75925926303735763057728287 ; -0.67356636847346836448512063 ; -0.57766293024122296772368984 ; -0.47300273144571496052218212 ;
 -0.36117230580938783773582173 ; -0.24386688372098843204519036 ; -0.12286469261071039638735982 ; 0.0 ; 0.12286469261071039638735982 ; 0.24386688372098843204519036 ;
 0.36117230580938783773582173 ; 0.47300273144571496052218212 ; 0.57766293024122296772368984 ; 0.67356636847346836448512063 ; 0.75925926303735763057728287 ;
 0.83344262876083400142102111 ; 0.89499199787827536885104201 ; 0.94297457122897433941401117 ; 0.97666392145951751149831539 ; 0.99555696979049809790878495 |] ;;

let gauss_legendre_25_w = [| 0.0113937985010262879479029641132 ; 0.026354986615032137261901815295 ; 0.040939156701306312655623487712 ; 0.054904695975835191925936891541 ;
 0.068038333812356917207187185657 ; 0.080140700335001018013234959669 ; 0.091028261982963649811497220703 ; 0.100535949067050644202206890393 ; 0.108519624474263653116093957050 ;
 0.11485825914571164833932554587 ; 0.11945576353578477222817812651 ; 0.12224244299031004168895951895 ; 0.12317605372671545120390287308 ; 0.12224244299031004168895951895 ;
 0.11945576353578477222817812651 ; 0.11485825914571164833932554587 ; 0.108519624474263653116093957050 ; 0.100535949067050644202206890393 ; 0.091028261982963649811497220703 ;
 0.080140700335001018013234959669 ; 0.068038333812356917207187185657 ; 0.054904695975835191925936891541 ; 0.040939156701306312655623487712 ; 0.026354986615032137261901815295 ;
 0.0113937985010262879479029641132 |] ;;


let gauss_legendre_26_x = [| -0.99588570114561692900321696 ; -0.97838544595647099110058035 ; -0.94715906666171425013591528 ; -0.90263786198430707421766560 ;
 -0.84544594278849801879750706 ; -0.77638594882067885619296725 ; -0.69642726041995726486381391 ; -0.60669229301761806323197875 ; -0.50844071482450571769570306 ;
 -0.40305175512348630648107738 ; -0.29200483948595689514283538 ; -0.17685882035689018396905775 ; -0.05923009342931320709371858 ; 0.05923009342931320709371858 ;
 0.17685882035689018396905775 ; 0.29200483948595689514283538 ; 0.40305175512348630648107738 ; 0.50844071482450571769570306 ; 0.60669229301761806323197875 ;
 0.69642726041995726486381391 ; 0.77638594882067885619296725 ; 0.84544594278849801879750706 ; 0.90263786198430707421766560 ; 0.94715906666171425013591528 ;
 0.97838544595647099110058035 ; 0.99588570114561692900321696 |] ;;

let gauss_legendre_26_w = [| 0.010551372617343007155651187685 ; 0.024417851092631908789615827520 ; 0.037962383294362763950303141249 ; 0.050975825297147811998319900724 ;
 0.063274046329574835539453689907 ; 0.07468414976565974588707579610 ; 0.08504589431348523921044776508 ; 0.09421380035591414846366488307 ; 0.10205916109442542323841407025 ;
 0.10847184052857659065657942673 ; 0.11336181654631966654944071844 ; 0.11666044348529658204466250754 ; 0.11832141527926227651637108570 ; 0.11832141527926227651637108570 ;
 0.11666044348529658204466250754 ; 0.11336181654631966654944071844 ; 0.10847184052857659065657942673 ; 0.10205916109442542323841407025 ; 0.09421380035591414846366488307 ;
 0.08504589431348523921044776508 ; 0.07468414976565974588707579610 ; 0.063274046329574835539453689907 ; 0.050975825297147811998319900724 ; 0.037962383294362763950303141249 ;
 0.024417851092631908789615827520 ; 0.010551372617343007155651187685 |] ;;


let gauss_legendre_27_x = [| -0.99617926288898856693888721 ; -0.97992347596150122285587336 ; -0.95090055781470500685190803 ; -0.90948232067749110430064502 ;
 -0.85620790801829449030273722 ; -0.79177163907050822714439734 ; -0.71701347373942369929481621 ; -0.63290797194649514092773464 ; -0.54055156457945689490030094 ;
 -0.44114825175002688058597416 ; -0.33599390363850889973031903 ; -0.22645936543953685885723911 ; -0.11397258560952996693289498 ; 0.0 ; 0.11397258560952996693289498 ;
 0.22645936543953685885723911 ; 0.33599390363850889973031903 ; 0.44114825175002688058597416 ; 0.54055156457945689490030094 ; 0.63290797194649514092773464 ;
 0.71701347373942369929481621 ; 0.79177163907050822714439734 ; 0.85620790801829449030273722 ; 0.90948232067749110430064502 ; 0.95090055781470500685190803 ;
 0.97992347596150122285587336 ; 0.99617926288898856693888721 |] ;;

let gauss_legendre_27_w = [| 0.0097989960512943602611500550912 ; 0.022686231596180623196034206447 ; 0.035297053757419711022578289305 ; 0.047449412520615062704096710114 ;
 0.058983536859833599110300833720 ; 0.069748823766245592984322888357 ; 0.079604867773057771263074959010 ; 0.088423158543756950194322802854 ; 0.096088727370028507565652646558 ;
 0.102501637817745798671247711533 ; 0.107578285788533187212162984427 ; 0.111252488356845192672163096043 ; 0.113476346108965148620369948092 ; 0.11422086737895698904504573690 ;
 0.113476346108965148620369948092 ; 0.111252488356845192672163096043 ; 0.107578285788533187212162984427 ; 0.102501637817745798671247711533 ; 0.096088727370028507565652646558 ;
 0.088423158543756950194322802854 ; 0.079604867773057771263074959010 ; 0.069748823766245592984322888357 ; 0.058983536859833599110300833720 ; 0.047449412520615062704096710114 ;
 0.035297053757419711022578289305 ; 0.022686231596180623196034206447 ; 0.0097989960512943602611500550912 |] ;;


let gauss_legendre_28_x = [| -0.99644249757395444995043639 ; -0.98130316537087275369455995 ; -0.95425928062893819725410184 ; -0.91563302639213207386968942 ;
 -0.86589252257439504894225457 ; -0.80564137091717917144788596 ; -0.73561087801363177202814451 ; -0.65665109403886496121989818 ; -0.56972047181140171930800328 ;
 -0.47587422495511826103441185 ; -0.37625151608907871022135721 ; -0.27206162763517807767682636 ; -0.16456928213338077128147178 ; -0.05507928988403427042651653 ;
 0.05507928988403427042651653 ; 0.16456928213338077128147178 ; 0.27206162763517807767682636 ; 0.37625151608907871022135721 ; 0.47587422495511826103441185 ;
 0.56972047181140171930800328 ; 0.65665109403886496121989818 ; 0.73561087801363177202814451 ; 0.80564137091717917144788596 ; 0.86589252257439504894225457 ;
 0.91563302639213207386968942 ; 0.95425928062893819725410184 ; 0.98130316537087275369455995 ; 0.99644249757395444995043639 |] ;;

let gauss_legendre_28_w = [| 0.009124282593094517738816153923 ; 0.021132112592771259751500380993 ; 0.032901427782304379977630819171 ; 0.044272934759004227839587877653 ;
 0.055107345675716745431482918227 ; 0.06527292396699959579339756678 ; 0.07464621423456877902393188717 ; 0.08311341722890121839039649824 ; 0.09057174439303284094218603134 ;
 0.09693065799792991585048900610 ; 0.10211296757806076981421663851 ; 0.10605576592284641791041643700 ; 0.10871119225829413525357151930 ; 0.11004701301647519628237626560 ;
 0.11004701301647519628237626560 ; 0.10871119225829413525357151930 ; 0.10605576592284641791041643700 ; 0.10211296757806076981421663851 ; 0.09693065799792991585048900610 ;
 0.09057174439303284094218603134 ; 0.08311341722890121839039649824 ; 0.07464621423456877902393188717 ; 0.06527292396699959579339756678 ; 0.055107345675716745431482918227 ;
 0.044272934759004227839587877653 ; 0.032901427782304379977630819171 ; 0.021132112592771259751500380993 ; 0.009124282593094517738816153923 |] ;;


let gauss_legendre_29_x = [| -0.99667944226059658616319153 ; -0.98254550526141317487092602 ; -0.95728559577808772579820804 ; -0.92118023295305878509375344 ;
 -0.87463780492010279041779342 ; -0.81818548761525244498957221 ; -0.75246285173447713391261008 ; -0.67821453760268651515618501 ; -0.59628179713822782037958621 ;
 -0.50759295512422764210262792 ; -0.41315288817400866389070659 ; -0.31403163786763993494819592 ; -0.21135228616600107450637573 ; -0.10627823013267923017098239 ;
 0.0 ; 0.10627823013267923017098239 ; 0.21135228616600107450637573 ; 0.31403163786763993494819592 ; 0.41315288817400866389070659 ; 0.50759295512422764210262792 ;
 0.59628179713822782037958621 ; 0.67821453760268651515618501 ; 0.75246285173447713391261008 ; 0.81818548761525244498957221 ; 0.87463780492010279041779342 ;
 0.92118023295305878509375344 ; 0.95728559577808772579820804 ; 0.98254550526141317487092602 ; 0.99667944226059658616319153 |] ;;

let gauss_legendre_29_w = [| 0.0085169038787464096542638133022 ; 0.019732085056122705983859801640 ; 0.030740492202093622644408525375 ; 0.041402062518682836104830010114 ;
 0.051594826902497923912594381180 ; 0.061203090657079138542109848024 ; 0.070117933255051278569581486949 ; 0.078238327135763783828144888660 ; 0.085472257366172527545344849297 ;
 0.091737757139258763347966411077 ; 0.096963834094408606301900074883 ; 0.101091273759914966121820546907 ; 0.104073310077729373913328471285 ; 0.105876155097320941406591327852 ;
 0.10647938171831424424651112691 ; 0.105876155097320941406591327852 ; 0.104073310077729373913328471285 ; 0.101091273759914966121820546907 ; 0.096963834094408606301900074883 ;
 0.091737757139258763347966411077 ; 0.085472257366172527545344849297 ; 0.078238327135763783828144888660 ; 0.070117933255051278569581486949 ; 0.061203090657079138542109848024 ;
 0.051594826902497923912594381180 ; 0.041402062518682836104830010114 ; 0.030740492202093622644408525375 ; 0.019732085056122705983859801640 ; 0.0085169038787464096542638133022 |] ;;


let gauss_legendre_30_x = [| -0.99689348407464954027163005 ; -0.98366812327974720997003258 ; -0.96002186496830751221687103 ; -0.92620004742927432587932428 ;
 -0.88256053579205268154311646 ; -0.82956576238276839744289812 ; -0.76777743210482619491797734 ; -0.69785049479331579693229239 ; -0.62052618298924286114047756 ;
 -0.53662414814201989926416979 ; -0.44703376953808917678060990 ; -0.35270472553087811347103721 ; -0.25463692616788984643980513 ; -0.15386991360858354696379467 ;
 -0.05147184255531769583302521 ; 0.05147184255531769583302521 ; 0.15386991360858354696379467 ; 0.25463692616788984643980513 ; 0.35270472553087811347103721 ;
 0.44703376953808917678060990 ; 0.53662414814201989926416979 ; 0.62052618298924286114047756 ; 0.69785049479331579693229239 ; 0.76777743210482619491797734 ;
 0.82956576238276839744289812 ; 0.88256053579205268154311646 ; 0.92620004742927432587932428 ; 0.96002186496830751221687103 ; 0.98366812327974720997003258 ;
 0.99689348407464954027163005 |] ;;

let gauss_legendre_30_w = [| 0.007968192496166605615465883475 ; 0.018466468311090959142302131912 ; 0.028784707883323369349719179611 ; 0.038799192569627049596801936446 ;
 0.048402672830594052902938140423 ; 0.057493156217619066481721689402 ; 0.06597422988218049512812851512 ; 0.07375597473770520626824385002 ; 0.08075589522942021535469493846 ;
 0.08689978720108297980238753072 ; 0.09212252223778612871763270709 ; 0.09636873717464425963946862635 ; 0.09959342058679526706278028210 ; 0.10176238974840550459642895217 ;
 0.10285265289355884034128563671 ; 0.10285265289355884034128563671 ; 0.10176238974840550459642895217 ; 0.09959342058679526706278028210 ; 0.09636873717464425963946862635 ;
 0.09212252223778612871763270709 ; 0.08689978720108297980238753072 ; 0.08075589522942021535469493846 ; 0.07375597473770520626824385002 ; 0.06597422988218049512812851512 ;
 0.057493156217619066481721689402 ; 0.048402672830594052902938140423 ; 0.038799192569627049596801936446 ; 0.028784707883323369349719179611 ; 0.018466468311090959142302131912 ;
 0.007968192496166605615465883475 |] ;;


let gauss_legendre_31_x = [| -0.99708748181947707405562655 ; -0.98468590966515248400246517 ; -0.96250392509294966178905240 ; -0.93075699789664816495694576 ;
 -0.88976002994827104337419201 ; -0.83992032014626734008690454 ; -0.78173314841662494040636002 ; -0.71577678458685328390597087 ; -0.64270672292426034618441820 ;
 -0.56324916140714926272094492 ; -0.47819378204490248044059404 ; -0.38838590160823294306135146 ; -0.29471806998170161661790390 ; -0.19812119933557062877241300 ;
 -0.09955531215234152032517479 ; 0.0 ; 0.09955531215234152032517479 ; 0.19812119933557062877241300 ; 0.29471806998170161661790390 ; 0.38838590160823294306135146 ;
 0.47819378204490248044059404 ; 0.56324916140714926272094492 ; 0.64270672292426034618441820 ; 0.71577678458685328390597087 ; 0.78173314841662494040636002 ;
 0.83992032014626734008690454 ; 0.88976002994827104337419201 ; 0.93075699789664816495694576 ; 0.96250392509294966178905240 ; 0.98468590966515248400246517 ;
 0.99708748181947707405562655 |] ;;

let gauss_legendre_31_w = [| 0.0074708315792487758586968750322 ; 0.017318620790310582463157996087 ; 0.027009019184979421800608708092 ; 0.036432273912385464024392010468 ;
 0.045493707527201102902315857895 ; 0.054103082424916853711666259087 ; 0.062174786561028426910343543687 ; 0.069628583235410366167756126255 ; 0.076390386598776616426357674901 ;
 0.082392991761589263903823367432 ; 0.087576740608477876126198069695 ; 0.091890113893641478215362871607 ; 0.095290242912319512807204197488 ; 0.097743335386328725093474010979 ;
 0.099225011226672307874875514429 ; 0.09972054479342645142753383373 ; 0.099225011226672307874875514429 ; 0.097743335386328725093474010979 ; 0.095290242912319512807204197488 ;
 0.091890113893641478215362871607 ; 0.087576740608477876126198069695 ; 0.082392991761589263903823367432 ; 0.076390386598776616426357674901 ; 0.069628583235410366167756126255 ;
 0.062174786561028426910343543687 ; 0.054103082424916853711666259087 ; 0.045493707527201102902315857895 ; 0.036432273912385464024392010468 ; 0.027009019184979421800608708092 ;
 0.017318620790310582463157996087 ; 0.0074708315792487758586968750322 |] ;;


let gauss_legendre_32_x = [| -0.99726386184948156354498113 ; -0.98561151154526833540017504 ; -0.96476225558750643077381193 ; -0.93490607593773968917091913 ;
 -0.89632115576605212396530724 ; -0.84936761373256997013369300 ; -0.79448379596794240696309730 ; -0.73218211874028968038742667 ; -0.66304426693021520097511517 ;
 -0.58771575724076232904074548 ; -0.50689990893222939002374747 ; -0.42135127613063534536411944 ; -0.33186860228212764977991681 ; -0.23928736225213707454460321 ;
 -0.14447196158279649348518637 ; -0.04830766568773831623481257 ; 0.04830766568773831623481257 ; 0.14447196158279649348518637 ; 0.23928736225213707454460321 ;
 0.33186860228212764977991681 ; 0.42135127613063534536411944 ; 0.50689990893222939002374747 ; 0.58771575724076232904074548 ; 0.66304426693021520097511517 ;
 0.73218211874028968038742667 ; 0.79448379596794240696309730 ; 0.84936761373256997013369300 ; 0.89632115576605212396530724 ; 0.93490607593773968917091913 ;
 0.96476225558750643077381193 ; 0.98561151154526833540017504 ; 0.99726386184948156354498113 |] ;;

let gauss_legendre_32_w = [| 0.007018610009470096600407063739 ; 0.016274394730905670605170562206 ; 0.025392065309262059455752589789 ; 0.034273862913021433102687732252 ;
 0.042835898022226680656878646606 ; 0.050998059262376176196163244690 ; 0.058684093478535547145283637300 ; 0.06582222277636184683765006371 ; 0.07234579410884850622539935648 ;
 0.07819389578707030647174091883 ; 0.08331192422694675522219907460 ; 0.08765209300440381114277146275 ; 0.09117387869576388471286857711 ; 0.09384439908080456563918023767 ;
 0.09563872007927485941908200220 ; 0.09654008851472780056676483006 ; 0.09654008851472780056676483006 ; 0.09563872007927485941908200220 ; 0.09384439908080456563918023767 ;
 0.09117387869576388471286857711 ; 0.08765209300440381114277146275 ; 0.08331192422694675522219907460 ; 0.07819389578707030647174091883 ; 0.07234579410884850622539935648 ;
 0.06582222277636184683765006371 ; 0.058684093478535547145283637300 ; 0.050998059262376176196163244690 ; 0.042835898022226680656878646606 ; 0.034273862913021433102687732252 ;
 0.025392065309262059455752589789 ; 0.016274394730905670605170562206 ; 0.007018610009470096600407063739 |] ;;


let gauss_legendre_33_x = [| -0.99742469424645521726616802 ; -0.98645572623064248811037570 ; -0.96682290968999276892837771 ; -0.93869437261116835035583512 ;
 -0.90231676774343358304053133 ; -0.85800965267650406464306148 ; -0.80616235627416658979620087 ; -0.74723049644956215785905512 ; -0.68173195996974278626821595 ;
 -0.61024234583637902730728751 ; -0.53338990478634764354889426 ; -0.45185001727245069572599328 ; -0.36633925774807334107022062 ; -0.27760909715249702940324807 ;
 -0.18643929882799157233579876 ; -0.09363106585473338567074292 ; 0.0 ; 0.09363106585473338567074292 ; 0.18643929882799157233579876 ; 0.27760909715249702940324807 ;
 0.36633925774807334107022062 ; 0.45185001727245069572599328 ; 0.53338990478634764354889426 ; 0.61024234583637902730728751 ; 0.68173195996974278626821595 ;
 0.74723049644956215785905512 ; 0.80616235627416658979620087 ; 0.85800965267650406464306148 ; 0.90231676774343358304053133 ; 0.93869437261116835035583512 ;
 0.96682290968999276892837771 ; 0.98645572623064248811037570 ; 0.99742469424645521726616802 |] ;;

let gauss_legendre_33_w = [| 0.0066062278475873780586492352085 ; 0.015321701512934676127945768534 ; 0.023915548101749480350533257529 ; 0.032300358632328953281561447250 ;
 0.040401541331669591563409790527 ; 0.048147742818711695670146880138 ; 0.055470846631663561284944495439 ; 0.062306482530317480031627725771 ; 0.068594572818656712805955073015 ;
 0.074279854843954149342472175919 ; 0.079312364794886738363908384942 ; 0.083647876067038707613928014518 ; 0.087248287618844337607281670945 ; 0.090081958660638577239743705500 ;
 0.092123986643316846213240977717 ; 0.093356426065596116160999126274 ; 0.09376844616020999656730454155 ; 0.093356426065596116160999126274 ; 0.092123986643316846213240977717 ;
 0.090081958660638577239743705500 ; 0.087248287618844337607281670945 ; 0.083647876067038707613928014518 ; 0.079312364794886738363908384942 ; 0.074279854843954149342472175919 ;
 0.068594572818656712805955073015 ; 0.062306482530317480031627725771 ; 0.055470846631663561284944495439 ; 0.048147742818711695670146880138 ; 0.040401541331669591563409790527 ;
 0.032300358632328953281561447250 ; 0.023915548101749480350533257529 ; 0.015321701512934676127945768534 ; 0.0066062278475873780586492352085 |] ;;


let gauss_legendre_63_x = [| -0.99928298402912378037893614 ; -0.99622401277797010860219336 ; -0.99072854689218946681089467 ; -0.98280881059372723486251141 ;
 -0.97248403469757002280196068 ; -0.95977944975894192707035417 ; -0.94472613404100980296637532 ; -0.92736092062184320544703138 ; -0.90772630277853155803695313 ;
 -0.88587032850785342629029846 ; -0.86184648236412371953961184 ; -0.83571355431950284347180777 ; -0.80753549577345676005146599 ; -0.7773812629903723355633302 ;
 -0.7453246483178474178293217 ; -0.7114440995848458078514315 ; -0.6758225281149860901311033 ; -0.6385471058213653850003070 ; -0.5997090518776252357390089 ;
 -0.5594034094862850132676978 ; -0.5177288132900332481244776 ; -0.4747872479948043999222123 ; -0.4306837987951116006620889 ; -0.3855263942122478924776150 ;
 -0.3394255419745844024688344 ; -0.2924940585862514400361572 ; -0.2448467932459533627484046 ; -0.1966003467915066845576275 ; -0.1478727863578719685698391 ;
 -0.0987833564469452795297037 ; -0.0494521871161596272342338 ; 0.0 ; 0.0494521871161596272342338 ; 0.0987833564469452795297037 ; 0.1478727863578719685698391 ;
 0.1966003467915066845576275 ; 0.2448467932459533627484046 ; 0.2924940585862514400361572 ; 0.3394255419745844024688344 ; 0.3855263942122478924776150 ;
 0.4306837987951116006620889 ; 0.4747872479948043999222123 ; 0.5177288132900332481244776 ; 0.5594034094862850132676978 ; 0.5997090518776252357390089 ;
 0.6385471058213653850003070 ; 0.6758225281149860901311033 ; 0.7114440995848458078514315 ; 0.7453246483178474178293217 ; 0.7773812629903723355633302 ;
 0.8075354957734567600514660 ; 0.8357135543195028434718078 ; 0.8618464823641237195396118 ; 0.8858703285078534262902985 ; 0.9077263027785315580369531 ;
 0.9273609206218432054470314 ; 0.9447261340410098029663753 ; 0.9597794497589419270703542 ; 0.9724840346975700228019607 ; 0.9828088105937272348625114 ;
 0.9907285468921894668108947 ; 0.9962240127779701086021934 ; 0.9992829840291237803789361 |] ;;

let gauss_legendre_63_w = [| 0.0018398745955770841170924455540 ; 0.0042785083468637618660784110826 ; 0.0067102917659601362519069307298 ; 0.0091259686763266563540586454218 ;
 0.011519376076880041750750606149 ; 0.013884612616115610824866086368 ; 0.016215878410338338882283672975 ; 0.018507464460161270409260545805 ; 0.020753761258039090775341953421 ;
 0.022949271004889933148942319562 ; 0.025088620553344986618630138068 ; 0.027166574359097933225189839439 ; 0.029178047208280526945551502154 ; 0.031118116622219817508215988557 ;
 0.032982034883779341765683179672 ; 0.034765240645355877697180504643 ; 0.036463370085457289630452409788 ; 0.038072267584349556763638324928 ; 0.039587995891544093984807928149 ;
 0.041006845759666398635110037009 ; 0.042325345020815822982505485403 ; 0.043540267083027590798964315704 ; 0.044648638825941395370332669517 ; 0.045647747876292608685885992609 ;
 0.046535149245383696510395418747 ; 0.047308671312268919080604988339 ; 0.047966421137995131411052756195 ; 0.048506789097883847864090099146 ; 0.048928452820511989944709361549 ;
 0.049230380423747560785043116988 ; 0.049411833039918178967039646117 ; 0.04947236662393102088866936042 ; 0.049411833039918178967039646117 ; 0.049230380423747560785043116988 ;
 0.048928452820511989944709361549 ; 0.048506789097883847864090099146 ; 0.047966421137995131411052756195 ; 0.047308671312268919080604988339 ; 0.046535149245383696510395418747 ;
 0.045647747876292608685885992609 ; 0.044648638825941395370332669517 ; 0.043540267083027590798964315704 ; 0.042325345020815822982505485403 ; 0.041006845759666398635110037009 ;
 0.039587995891544093984807928149 ; 0.038072267584349556763638324928 ; 0.036463370085457289630452409788 ; 0.034765240645355877697180504643 ; 0.032982034883779341765683179672 ;
 0.031118116622219817508215988557 ; 0.029178047208280526945551502154 ; 0.027166574359097933225189839439 ; 0.025088620553344986618630138068 ; 0.022949271004889933148942319562 ;
 0.020753761258039090775341953421 ; 0.018507464460161270409260545805 ; 0.016215878410338338882283672975 ; 0.013884612616115610824866086368 ; 0.011519376076880041750750606149 ;
 0.0091259686763266563540586454218 ; 0.0067102917659601362519069307298 ; 0.0042785083468637618660784110826 ; 0.0018398745955770841170924455540 |] ;;
 

let gauss_legendre_64_x = [| -0.99930504173577213945690562 ; -0.99634011677195527934692450 ; -0.99101337147674432073938238 ; -0.98333625388462595693129930 ;
 -0.97332682778991096374185351 ; -0.96100879965205371891861412 ; -0.94641137485840281606248149 ; -0.92956917213193957582149015 ; -0.91052213707850280575638067 ;
 -0.88931544599511410585340404 ; -0.86599939815409281976078339 ; -0.8406292962525803627516915 ; -0.8132653151227975597419233 ; -0.7839723589433414076102205 ;
 -0.7528199072605318966118638 ; -0.7198818501716108268489402 ; -0.6852363130542332425635584 ; -0.6489654712546573398577612 ; -0.6111553551723932502488530 ;
 -0.5718956462026340342838781 ; -0.5312794640198945456580139 ; -0.4894031457070529574785263 ; -0.4463660172534640879849477 ; -0.4022701579639916036957668 ;
 -0.3572201583376681159504426 ; -0.3113228719902109561575127 ; -0.2646871622087674163739642 ; -0.2174236437400070841496487 ; -0.1696444204239928180373136 ;
 -0.1214628192961205544703765 ; -0.0729931217877990394495429 ; -0.0243502926634244325089558 ; 0.0243502926634244325089558 ; 0.0729931217877990394495429 ;
 0.1214628192961205544703765 ; 0.1696444204239928180373136 ; 0.2174236437400070841496487 ; 0.2646871622087674163739642 ; 0.3113228719902109561575127 ;
 0.3572201583376681159504426 ; 0.4022701579639916036957668 ; 0.4463660172534640879849477 ; 0.4894031457070529574785263 ; 0.5312794640198945456580139 ;
 0.5718956462026340342838781 ; 0.6111553551723932502488530 ; 0.6489654712546573398577612 ; 0.6852363130542332425635584 ; 0.7198818501716108268489402 ;
 0.7528199072605318966118638 ; 0.7839723589433414076102205 ; 0.8132653151227975597419233 ; 0.8406292962525803627516915 ; 0.8659993981540928197607834 ;
 0.8893154459951141058534040 ; 0.9105221370785028057563807 ; 0.9295691721319395758214902 ; 0.9464113748584028160624815 ; 0.9610087996520537189186141 ;
 0.9733268277899109637418535 ; 0.9833362538846259569312993 ; 0.9910133714767443207393824 ; 0.9963401167719552793469245 ; 0.9993050417357721394569056 |] ;;

let gauss_legendre_64_w = [| 0.0017832807216964329472960791450 ; 0.0041470332605624676352875357286 ; 0.006504457968978362856117360400 ; 0.008846759826363947723030914660 ;
 0.011168139460131128818590493019 ; 0.013463047896718642598060766686 ; 0.015726030476024719321965995298 ; 0.017951715775697343085045302001 ; 0.020134823153530209372340316729 ;
 0.022270173808383254159298330384 ; 0.024352702568710873338177550409 ; 0.026377469715054658671691792625 ; 0.028339672614259483227511305200 ; 0.030234657072402478867974059820 ;
 0.032057928354851553585467504348 ; 0.033805161837141609391565482111 ; 0.035472213256882383810693146715 ; 0.037055128540240046040415101810 ; 0.038550153178615629128962496947 ;
 0.039953741132720341386656926128 ; 0.041262563242623528610156297474 ; 0.042473515123653589007339767909 ; 0.043583724529323453376827860974 ; 0.044590558163756563060134710031 ;
 0.045491627927418144479770996971 ; 0.046284796581314417295953249232 ; 0.046968182816210017325326285755 ; 0.047540165714830308662282206944 ; 0.04799938859645830772812617987 ;
 0.04834476223480295716976952716 ; 0.04857546744150342693479906678 ; 0.04869095700913972038336539073 ; 0.04869095700913972038336539073 ; 0.04857546744150342693479906678 ;
 0.04834476223480295716976952716 ; 0.04799938859645830772812617987 ; 0.047540165714830308662282206944 ; 0.046968182816210017325326285755 ; 0.046284796581314417295953249232 ;
 0.045491627927418144479770996971 ; 0.044590558163756563060134710031 ; 0.043583724529323453376827860974 ; 0.042473515123653589007339767909 ; 0.041262563242623528610156297474 ;
 0.039953741132720341386656926128 ; 0.038550153178615629128962496947 ; 0.037055128540240046040415101810 ; 0.035472213256882383810693146715 ; 0.033805161837141609391565482111 ;
 0.032057928354851553585467504348 ; 0.030234657072402478867974059820 ; 0.028339672614259483227511305200 ; 0.026377469715054658671691792625 ; 0.024352702568710873338177550409 ;
 0.022270173808383254159298330384 ; 0.020134823153530209372340316729 ; 0.017951715775697343085045302001 ; 0.015726030476024719321965995298 ; 0.013463047896718642598060766686 ;
 0.011168139460131128818590493019 ; 0.008846759826363947723030914660 ; 0.006504457968978362856117360400 ; 0.0041470332605624676352875357286 ; 0.0017832807216964329472960791450 |] ;;


let gauss_legendre_65_x = [| -0.99932609707541287726569361 ; -0.99645094806184916305579494 ; -0.99128527617680166872182118 ; -0.98383981218703494137763778 ;
 -0.97413153983355116907496789 ; -0.96218275471805523771198375 ; -0.94802092816840750637376974 ; -0.93167862822874933796567699 ; -0.91319344054284626173654692 ;
 -0.89260788050473893142328554 ; -0.8699692949264070361941320 ; -0.8453297528999302839424500 ; -0.8187459259226514534339191 ; -0.7902789574921218430473804 ;
 -0.7599943224419997868739828 ; -0.7279616763294246790119737 ; -0.6942546952139916335526225 ; -0.6589509061936251330409408 ; -0.6221315090854002415825996 ;
 -0.5838811896604873133271545 ; -0.5442879248622271385455725 ; -0.5034427804550068823410431 ; -0.4614397015691450576978341 ; -0.4183752966234090092641990 ;
 -0.3743486151220660120087939 ; -0.3294609198374864076452867 ; -0.2838154539022487306176554 ; -0.2375172033464168065707124 ; -0.1906726556261427697749124 ;
 -0.1433895546989751711312496 ; -0.0957766532091975056522186 ; -0.0479434623531718575225298 ; 0.0 ; 0.0479434623531718575225298 ; 0.0957766532091975056522186 ;
 0.1433895546989751711312496 ; 0.1906726556261427697749124 ; 0.2375172033464168065707124 ; 0.2838154539022487306176554 ; 0.3294609198374864076452867 ;
 0.3743486151220660120087939 ; 0.4183752966234090092641990 ; 0.4614397015691450576978341 ; 0.5034427804550068823410431 ; 0.5442879248622271385455725 ;
 0.5838811896604873133271545 ; 0.6221315090854002415825996 ; 0.6589509061936251330409408 ; 0.6942546952139916335526225 ; 0.7279616763294246790119737 ;
 0.7599943224419997868739828 ; 0.7902789574921218430473804 ; 0.8187459259226514534339191 ; 0.8453297528999302839424500 ; 0.8699692949264070361941320 ;
 0.8926078805047389314232855 ; 0.9131934405428462617365469 ; 0.9316786282287493379656770 ; 0.9480209281684075063737697 ; 0.9621827547180552377119837 ;
 0.9741315398335511690749679 ; 0.9838398121870349413776378 ; 0.9912852761768016687218212 ; 0.9964509480618491630557949 ; 0.9993260970754128772656936 |] ;;

let gauss_legendre_65_w = [| 0.0017292582513002508983395851463 ; 0.0040215241720037363470786599528 ; 0.0063079425789717545501888719039 ; 0.0085801482668814598936358121592 ;
 0.0108326787895979686215140551272 ; 0.013060311639994846336168342922 ; 0.015257912146448310349265388145 ; 0.017420421997670248495365759969 ; 0.019542865836750062826837429313 ;
 0.021620361284934062841654274667 ; 0.023648129691287236698780978994 ; 0.025621506938037758214084978694 ; 0.027535954088450343942499722327 ; 0.029387067789310668062644859210 ;
 0.031170590380189142464431845777 ; 0.032882419676368574984049638008 ; 0.034518618398549058625221276859 ; 0.036075423225565273932166270524 ; 0.037549253448257709809772223198 ;
 0.038936719204051197616673806364 ; 0.040234629273005533815446337743 ; 0.041439998417240293022686299233 ; 0.042550054246755802719217150803 ; 0.043562243595800486532284821661 ;
 0.044474238395082974427323504000 ; 0.045283941026300230657128240574 ; 0.045989489146651696963893390818 ; 0.046589259972233498302255136790 ; 0.047081874010454522246006808290 ;
 0.047466198232885503152644458740 ; 0.047741348681240621559038972227 ; 0.047906692500495862031347289176 ; 0.04796184939446661812070762137 ; 0.047906692500495862031347289176 ;
 0.047741348681240621559038972227 ; 0.047466198232885503152644458740 ; 0.047081874010454522246006808290 ; 0.046589259972233498302255136790 ; 0.045989489146651696963893390818 ;
 0.045283941026300230657128240574 ; 0.044474238395082974427323504000 ; 0.043562243595800486532284821661 ; 0.042550054246755802719217150803 ; 0.041439998417240293022686299233 ;
 0.040234629273005533815446337743 ; 0.038936719204051197616673806364 ; 0.037549253448257709809772223198 ; 0.036075423225565273932166270524 ; 0.034518618398549058625221276859 ;
 0.032882419676368574984049638008 ; 0.031170590380189142464431845777 ; 0.029387067789310668062644859210 ; 0.027535954088450343942499722327 ; 0.025621506938037758214084978694 ;
 0.023648129691287236698780978994 ; 0.021620361284934062841654274667 ; 0.019542865836750062826837429313 ; 0.017420421997670248495365759969 ; 0.015257912146448310349265388145 ;
 0.013060311639994846336168342922 ; 0.0108326787895979686215140551272 ; 0.0085801482668814598936358121592 ; 0.0063079425789717545501888719039 ; 0.0040215241720037363470786599528 ;
 0.0017292582513002508983395851463 |] ;;
    
  
let gauss_legendre_127_x = [| -0.9998221304153061462673512 ; -0.9990629343553118951383159 ; -0.9976975661898046210744170 ; -0.9957265513520272266354334 ;
 -0.9931510492545171473611308 ; -0.9899726145914841576077867 ; -0.9861931740169316667104383 ; -0.9818150208038141100334631 ; -0.9768408123430703268174439 ;
 -0.9712735681615291922889469 ; -0.9651166679452921210908251 ; -0.9583738494252387711491029 ; -0.9510492060778803105479076 ; -0.9431471846248148273454496 ;
 -0.9346725823247379685736349 ; -0.9256305440562338491274647 ; -0.9160265591914658093130886 ; -0.9058664582618213828024613 ; -0.8951564094170837089690438 ;
 -0.8839029146800265699452579 ; -0.8721128059985607114196375 ; -0.8597932410977408098120313 ; -0.8469516991340975984533393 ; -0.8335959761548995143795572 ;
 -0.8197341803650786741551191 ; -0.8053747272046802146665608 ; -0.7905263342398137999454500 ; -0.7751980158702023824449628 ; -0.7593990778565366715566637 ;
 -0.7431391116709545129205669 ; -0.7264279886740726855356929 ; -0.7092758541221045609994446 ; -0.6916931210077006701564414 ; -0.6736904637382504853466825 ;
 -0.6552788116554826302767651 ; -0.6364693424002972413476082 ; -0.6172734751268582838576392 ; -0.5977028635700652293844120 ; -0.5777693889706125800032517 ;
 -0.5574851528619322329218619 ; -0.5368624697233975674581664 ; -0.5159138595042493572772773 ; -0.4946520400227821173949402 ; -0.4730899192454052416450999 ;
 -0.4512405874502662273318986 ; -0.4291173092801933762625441 ; -0.4067335156897825634086729 ; -0.3841027957915169357790778 ; -0.3612388886058697060709248 ;
 -0.3381556747203985013760003 ; -0.3148671678628949814860148 ; -0.2913875063937056207945188 ; -0.2677309447223886208883435 ; -0.2439118446539178579707132 ;
 -0.2199446666696875424545234 ; -0.1958439611486108515042816 ; -0.1716243595336421650083449 ; -0.1473005654490856693893293 ; -0.1228873457740829717260337 ;
 -0.0983995216776989707510918 ; -0.0738519596210485452734404 ; -0.0492595623319266303153793 ; -0.0246372597574209446148971 ; 0.0 ; 0.0246372597574209446148971 ;
 0.0492595623319266303153793 ; 0.0738519596210485452734404 ; 0.0983995216776989707510918 ; 0.1228873457740829717260337 ; 0.1473005654490856693893293 ;
 0.1716243595336421650083449 ; 0.1958439611486108515042816 ; 0.2199446666696875424545234 ; 0.2439118446539178579707132 ; 0.2677309447223886208883435 ;
 0.2913875063937056207945188 ; 0.3148671678628949814860148 ; 0.3381556747203985013760003 ; 0.3612388886058697060709248 ; 0.3841027957915169357790778 ;
 0.4067335156897825634086729 ; 0.4291173092801933762625441 ; 0.4512405874502662273318986 ; 0.4730899192454052416450999 ; 0.4946520400227821173949402 ;
 0.5159138595042493572772773 ; 0.5368624697233975674581664 ; 0.5574851528619322329218619 ; 0.5777693889706125800032517 ; 0.5977028635700652293844120 ;
 0.6172734751268582838576392 ; 0.6364693424002972413476082 ; 0.6552788116554826302767651 ; 0.6736904637382504853466825 ; 0.6916931210077006701564414 ;
 0.7092758541221045609994446 ; 0.7264279886740726855356929 ; 0.7431391116709545129205669 ; 0.7593990778565366715566637 ; 0.7751980158702023824449628 ;
 0.7905263342398137999454500 ; 0.8053747272046802146665608 ; 0.8197341803650786741551191 ; 0.8335959761548995143795572 ; 0.8469516991340975984533393 ;
 0.8597932410977408098120313 ; 0.8721128059985607114196375 ; 0.8839029146800265699452579 ; 0.8951564094170837089690438 ; 0.9058664582618213828024613 ;
 0.9160265591914658093130886 ; 0.9256305440562338491274647 ; 0.9346725823247379685736349 ; 0.9431471846248148273454496 ; 0.9510492060778803105479076 ;
 0.9583738494252387711491029 ; 0.965116667945292121090825 ; 0.971273568161529192288947 ; 0.976840812343070326817444 ; 0.981815020803814110033463 ;
 0.986193174016931666710438 ; 0.989972614591484157607787 ; 0.993151049254517147361131 ; 0.995726551352027226635433 ; 0.997697566189804621074417 ;
 0.999062934355311895138316 ; 0.999822130415306146267351 |] ;;

let gauss_legendre_127_w = [| 0.00045645726109586662791936519265 ; 0.00106227668695384869596523598532 ; 0.0016683488125171936761028862915 ; 0.0022734860707492547802810840776 ;
 0.0028772587656289004082883197514 ; 0.0034792893810051465908910894100 ; 0.0040792095178254605327114733457 ; 0.0046766539777779034772638165663 ; 0.0052712596565634400891303815906 ;
 0.0058626653903523901033648343751 ; 0.0064505120486899171845442463869 ; 0.0070344427036681608755685893033 ; 0.0076141028256526859356393930849 ; 0.0081891404887415730817235884719 ;
 0.0087592065795403145773316804234 ; 0.0093239550065309714787536985834 ; 0.0098830429087554914716648010900 ; 0.0104361308631410052256731719977 ; 0.0109828830900689757887996573761 ;
 0.011522967656921087154811609735 ; 0.012056056679400848183529562145 ; 0.012581826520465013101514365424 ; 0.013099957986718627426172681913 ; 0.013610136522139249906034237534 ;
 0.014112052399003395774044161634 ; 0.014605400905893418351737288079 ; 0.015089882532666922992635733981 ; 0.015565203152273955098532590263 ; 0.016031074199309941802254151843 ;
 0.016487212845194879399346060358 ; 0.016933342169871654545878815295 ; 0.017369191329918731922164721250 ; 0.017794495722974774231027912900 ; 0.018208997148375106468721469154 ;
 0.018612443963902310429440419899 ; 0.019004591238555646611148901045 ; 0.019385200901246454628112623489 ; 0.019754041885329183081815217323 ; 0.020110890268880247225644623956 ;
 0.020455529410639508279497065713 ; 0.020787750081531811812652137291 ; 0.021107350591688713643523847922 ; 0.021414136912893259295449693234 ; 0.021707922796373466052301324695 ;
 0.021988529885872983756478409759 ; 0.022255787825930280235631416460 ; 0.022509534365300608085694429903 ; 0.022749615455457959852242553241 ; 0.022975885344117206754377437839 ;
 0.023188206663719640249922582982 ; 0.023386450514828194170722043497 ; 0.023570496544381716050033676844 ; 0.023740233018760777777714726703 ; 0.023895556891620665983864481754 ;
 0.024036373866450369675132086026 ; 0.024162598453819584716522917711 ; 0.024274154023278979833195063937 ; 0.024370972849882214952813561907 ; 0.024452996155301467956140198472 ;
 0.024520174143511508275183033290 ; 0.024572466031020653286354137335 ; 0.024609840071630254092545634003 ; 0.024632273575707679066033370218 ; 0.02463975292396109441957941748 ;
 0.024632273575707679066033370218 ; 0.024609840071630254092545634003 ; 0.024572466031020653286354137335 ; 0.024520174143511508275183033290 ; 0.024452996155301467956140198472 ;
 0.024370972849882214952813561907 ; 0.024274154023278979833195063937 ; 0.024162598453819584716522917711 ; 0.024036373866450369675132086026 ; 0.023895556891620665983864481754 ;
 0.023740233018760777777714726703 ; 0.023570496544381716050033676844 ; 0.023386450514828194170722043497 ; 0.023188206663719640249922582982 ; 0.022975885344117206754377437839 ;
 0.022749615455457959852242553241 ; 0.022509534365300608085694429903 ; 0.022255787825930280235631416460 ; 0.021988529885872983756478409759 ; 0.021707922796373466052301324695 ;
 0.021414136912893259295449693234 ; 0.021107350591688713643523847922 ; 0.020787750081531811812652137291 ; 0.020455529410639508279497065713 ; 0.020110890268880247225644623956 ;
 0.019754041885329183081815217323 ; 0.019385200901246454628112623489 ; 0.019004591238555646611148901045 ; 0.018612443963902310429440419899 ; 0.018208997148375106468721469154 ;
 0.017794495722974774231027912900 ; 0.017369191329918731922164721250 ; 0.016933342169871654545878815295 ; 0.016487212845194879399346060358 ; 0.016031074199309941802254151843 ;
 0.015565203152273955098532590263 ; 0.015089882532666922992635733981 ; 0.014605400905893418351737288079 ; 0.014112052399003395774044161634 ; 0.013610136522139249906034237534 ;
 0.013099957986718627426172681913 ; 0.012581826520465013101514365424 ; 0.012056056679400848183529562145 ; 0.011522967656921087154811609735 ; 0.0109828830900689757887996573761 ;
 0.0104361308631410052256731719977 ; 0.0098830429087554914716648010900 ; 0.0093239550065309714787536985834 ; 0.0087592065795403145773316804234 ; 0.0081891404887415730817235884719 ;
 0.0076141028256526859356393930849 ; 0.0070344427036681608755685893033 ; 0.0064505120486899171845442463869 ; 0.0058626653903523901033648343751 ; 0.0052712596565634400891303815906 ;
 0.0046766539777779034772638165663 ; 0.0040792095178254605327114733457 ; 0.0034792893810051465908910894100 ; 0.0028772587656289004082883197514 ; 0.0022734860707492547802810840776 ;
 0.0016683488125171936761028862915 ; 0.00106227668695384869596523598532 ; 0.00045645726109586662791936519265 |] ;;
 

let gauss_legendre_128_x = [| -0.9998248879471319144736081 ; -0.9990774599773758950119878 ; -0.9977332486255140198821574 ; -0.9957927585349811868641612 ;
 -0.9932571129002129353034372 ; -0.9901278184917343833379303 ; -0.9864067427245862088712355 ; -0.9820961084357185360247656 ; -0.9771984914639073871653744 ;
 -0.9717168187471365809043384 ; -0.9656543664319652686458290 ; -0.9590147578536999280989185 ; -0.9518019613412643862177963 ; -0.9440202878302201821211114 ;
 -0.9356743882779163757831268 ; -0.9267692508789478433346245 ; -0.9173101980809605370364836 ; -0.9073028834017568139214859 ; -0.8967532880491581843864474 ;
 -0.8856677173453972174082924 ; -0.8740527969580317986954180 ; -0.8619154689395484605906323 ; -0.8492629875779689691636001 ; -0.8361029150609068471168753 ;
 -0.8224431169556438424645942 ; -0.8082917575079136601196422 ; -0.7936572947621932902433329 ; -0.7785484755064119668504941 ; -0.7629743300440947227797691 ;
 -0.7469441667970619811698824 ; -0.7304675667419088064717369 ; -0.7135543776835874133438599 ; -0.6962147083695143323850866 ; -0.6784589224477192593677557 ;
 -0.6602976322726460521059468 ; -0.6417416925623075571535249 ; -0.6228021939105849107615396 ; -0.6034904561585486242035732 ; -0.5838180216287630895500389 ;
 -0.5637966482266180839144308 ; -0.5434383024128103634441936 ; -0.5227551520511754784539479 ; -0.5017595591361444642896063 ; -0.4804640724041720258582757 ;
 -0.4588814198335521954490891 ; -0.4370245010371041629370429 ; -0.4149063795522750154922739 ; -0.3925402750332674427356482 ; -0.3699395553498590266165917 ;
 -0.3471177285976355084261628 ; -0.3240884350244133751832523 ; -0.3008654388776772026671541 ; -0.2774626201779044028062316 ; -0.2538939664226943208556180 ;
 -0.2301735642266599864109866 ; -0.2063155909020792171540580 ; -0.1823343059853371824103826 ; -0.1582440427142249339974755 ; -0.1340591994611877851175753 ;
 -0.1097942311276437466729747 ; -0.0854636405045154986364980 ; -0.0610819696041395681037870 ; -0.0366637909687334933302153 ; -0.0122236989606157641980521 ;
 0.0122236989606157641980521 ; 0.0366637909687334933302153 ; 0.0610819696041395681037870 ; 0.0854636405045154986364980 ; 0.1097942311276437466729747 ;
 0.1340591994611877851175753 ; 0.1582440427142249339974755 ; 0.1823343059853371824103826 ; 0.2063155909020792171540580 ; 0.2301735642266599864109866 ;
 0.2538939664226943208556180 ; 0.2774626201779044028062316 ; 0.3008654388776772026671541 ; 0.3240884350244133751832523 ; 0.3471177285976355084261628 ;
 0.3699395553498590266165917 ; 0.3925402750332674427356482 ; 0.4149063795522750154922739 ; 0.4370245010371041629370429 ; 0.4588814198335521954490891 ;
 0.4804640724041720258582757 ; 0.5017595591361444642896063 ; 0.5227551520511754784539479 ; 0.5434383024128103634441936 ; 0.5637966482266180839144308 ;
 0.5838180216287630895500389 ; 0.6034904561585486242035732 ; 0.6228021939105849107615396 ; 0.6417416925623075571535249 ; 0.6602976322726460521059468 ;
 0.6784589224477192593677557 ; 0.6962147083695143323850866 ; 0.7135543776835874133438599 ; 0.7304675667419088064717369 ; 0.7469441667970619811698824 ;
 0.7629743300440947227797691 ; 0.7785484755064119668504941 ; 0.7936572947621932902433329 ; 0.8082917575079136601196422 ; 0.8224431169556438424645942 ;
 0.8361029150609068471168753 ; 0.8492629875779689691636001 ; 0.8619154689395484605906323 ; 0.8740527969580317986954180 ; 0.8856677173453972174082924 ;
 0.8967532880491581843864474 ; 0.9073028834017568139214859 ; 0.9173101980809605370364836 ; 0.926769250878947843334625 ; 0.935674388277916375783127 ;
 0.944020287830220182121111 ; 0.951801961341264386217796 ; 0.959014757853699928098919 ; 0.965654366431965268645829 ; 0.971716818747136580904338 ;
 0.977198491463907387165374 ; 0.982096108435718536024766 ; 0.986406742724586208871236 ; 0.990127818491734383337930 ; 0.993257112900212935303437 ;
 0.995792758534981186864161 ; 0.997733248625514019882157 ; 0.999077459977375895011988 ; 0.999824887947131914473608 |] ;;

let gauss_legendre_128_w = [| 0.00044938096029209037639429223999 ; 0.0010458126793403487793128516001 ; 0.0016425030186690295387908755948 ; 0.0022382884309626187436220542727 ;
 0.0028327514714579910952857346468 ; 0.0034255260409102157743377846601 ; 0.0040162549837386423131943434863 ; 0.0046045842567029551182905419803 ; 0.0051901618326763302050707671348 ;
 0.0057726375428656985893346176261 ; 0.006351663161707188787214327826 ; 0.006926892566898813563426670360 ; 0.007497981925634728687671962688 ; 0.008064589890486057972928598698 ;
 0.008626377798616749704978843782 ; 0.009183009871660874334478743688 ; 0.009734153415006805863548266094 ; 0.010279479015832157133215340326 ; 0.010818660739503076247659646277 ;
 0.011351376324080416693281668453 ; 0.011877307372740279575891106926 ; 0.012396139543950922968821728197 ; 0.012907562739267347220442834004 ; 0.013411271288616332314488951616 ;
 0.013906964132951985244288007396 ; 0.014394345004166846176823892009 ; 0.014873122602147314252385498520 ; 0.015343010768865144085990853741 ; 0.015803728659399346858965631687 ;
 0.016255000909785187051657456477 ; 0.016696557801589204589091507954 ; 0.017128135423111376830680987619 ; 0.017549475827117704648706925634 ; 0.017960327185008685940196927525 ;
 0.018360443937331343221289290991 ; 0.018749586940544708650919548474 ; 0.019127523609950945486518531668 ; 0.019494028058706602823021918681 ; 0.019848881232830862219944413265 ;
 0.020191871042130041180673158406 ; 0.020522792486960069432284967788 ; 0.020841447780751149113583948423 ; 0.021147646468221348537019535180 ; 0.021441205539208460137111853878 ;
 0.021721949538052075375260957768 ; 0.021989710668460491434122106599 ; 0.022244328893799765104629133607 ; 0.022485652032744966871824603941 ; 0.022713535850236461309712635923 ;
 0.022927844143686846920410987209 ; 0.023128448824387027879297902403 ; 0.023315229994062760122415671273 ; 0.023488076016535913153025273282 ; 0.023646883584447615143651392303 ;
 0.023791557781003400638780709885 ; 0.023922012136703455672450408817 ; 0.024038168681024052637587316820 ; 0.024139957989019284997716653890 ; 0.024227319222815248120093308442 ;
 0.024300200167971865323442606364 ; 0.024358557264690625853268520246 ; 0.024402355633849582093297989694 ; 0.02443156909785004505484856143 ; 0.02444618019626251821132585261 ;
 0.02444618019626251821132585261 ; 0.02443156909785004505484856143 ; 0.024402355633849582093297989694 ; 0.024358557264690625853268520246 ; 0.024300200167971865323442606364 ;
 0.024227319222815248120093308442 ; 0.024139957989019284997716653890 ; 0.024038168681024052637587316820 ; 0.023922012136703455672450408817 ; 0.023791557781003400638780709885 ;
 0.023646883584447615143651392303 ; 0.023488076016535913153025273282 ; 0.023315229994062760122415671273 ; 0.023128448824387027879297902403 ; 0.022927844143686846920410987209 ;
 0.022713535850236461309712635923 ; 0.022485652032744966871824603941 ; 0.022244328893799765104629133607 ; 0.021989710668460491434122106599 ; 0.021721949538052075375260957768 ;
 0.021441205539208460137111853878 ; 0.021147646468221348537019535180 ; 0.020841447780751149113583948423 ; 0.020522792486960069432284967788 ; 0.020191871042130041180673158406 ;
 0.019848881232830862219944413265 ; 0.019494028058706602823021918681 ; 0.019127523609950945486518531668 ; 0.018749586940544708650919548474 ; 0.018360443937331343221289290991 ;
 0.017960327185008685940196927525 ; 0.017549475827117704648706925634 ; 0.017128135423111376830680987619 ; 0.016696557801589204589091507954 ; 0.016255000909785187051657456477 ;
 0.015803728659399346858965631687 ; 0.015343010768865144085990853741 ; 0.014873122602147314252385498520 ; 0.014394345004166846176823892009 ; 0.013906964132951985244288007396 ;
 0.013411271288616332314488951616 ; 0.012907562739267347220442834004 ; 0.012396139543950922968821728197 ; 0.011877307372740279575891106926 ; 0.011351376324080416693281668453 ;
 0.010818660739503076247659646277 ; 0.010279479015832157133215340326 ; 0.009734153415006805863548266094 ; 0.009183009871660874334478743688 ; 0.008626377798616749704978843782 ;
 0.008064589890486057972928598698 ; 0.007497981925634728687671962688 ; 0.006926892566898813563426670360 ; 0.006351663161707188787214327826 ; 0.0057726375428656985893346176261 ;
 0.0051901618326763302050707671348 ; 0.0046045842567029551182905419803 ; 0.0040162549837386423131943434863 ; 0.0034255260409102157743377846601 ; 0.0028327514714579910952857346468 ;
 0.0022382884309626187436220542727 ; 0.0016425030186690295387908755948 ; 0.0010458126793403487793128516001 ; 0.00044938096029209037639429223999 |] ;;


let gauss_legendre_129_x = [| -0.9998275818477487191077441 ; -0.9990916504696409986514389 ; -0.9977681080525852721429460 ; -0.9958574393142831982149111 ;
 -0.9933607326210712814854011 ; -0.9902794486488178389207689 ; -0.9866153978313475022005761 ; -0.9823707352517413115507418 ; -0.9775479582993672474447814 ;
 -0.9721499048427034297274163 ; -0.9661797514202097197778763 ; -0.9596410113101918904168119 ; -0.9525375324342090471027732 ; -0.9448734950776734726784764 ;
 -0.9366534094216514605284616 ; -0.9278821128840036204317296 ; -0.9185647672698286252225115 ; -0.9087068557320696331245539 ; -0.8983141795436338850435985 ;
 -0.8873928546826803665034968 ; -0.8759493082329433892035217 ; -0.8639902746011257878940216 ; -0.8515227915535356930243826 ; -0.8385541960742664442975407 ;
 -0.8250921200473358809210133 ; -0.8111444857653120742087717 ; -0.7967195012670592680339606 ; -0.7818256555073413245387500 ; -0.7664717133611208816717785 ;
 -0.7506667104654910227632368 ; -0.7344199479022727047791516 ; -0.7177409867244055767721220 ; -0.7006396423293521790044710 ; -0.6831259786828258512462248 ;
 -0.6652103023962409818802202 ; -0.6469031566613704719753373 ; -0.6282153150457794374886895 ; -0.6091577751526861909563306 ; -0.5897417521489813916767844 ;
 -0.5699786721652138894754096 ; -0.5498801655714271702189358 ; -0.5294580601328034000099406 ; -0.5087243740491428186199463 ; -0.4876913088822746111853066 ;
 -0.4663712423755613514331869 ; -0.4447767211697226217818454 ; -0.4229204534192644388475065 ; -0.4008153013138596117693121 ; -0.3784742735090801012801265 ;
 -0.3559105174709357969672656 ; -0.3331373117387248575049982 ; -0.3101680581107488341147318 ; -0.2870162737574911929568755 ; -0.2636955832669005409666949 ;
 -0.2402197106264598167721148 ; -0.2166024711467599103221439 ; -0.1928577633313305998663880 ; -0.1689995606975133227390302 ; -0.1450419035531891084328306 ;
 -0.1209988907342009817690539 ; -0.0968846713073332753086909 ; -0.0727134362437305599118207 ; -0.0484994100676562986191764 ; -0.0242568424855058415749954 ;
 0.0 ; 0.0242568424855058415749954 ; 0.0484994100676562986191764 ; 0.0727134362437305599118207 ; 0.0968846713073332753086909 ; 0.1209988907342009817690539 ;
 0.1450419035531891084328306 ; 0.1689995606975133227390302 ; 0.1928577633313305998663880 ; 0.2166024711467599103221439 ; 0.2402197106264598167721148 ;
 0.2636955832669005409666949 ; 0.2870162737574911929568755 ; 0.3101680581107488341147318 ; 0.3331373117387248575049982 ; 0.3559105174709357969672656 ;
 0.3784742735090801012801265 ; 0.4008153013138596117693121 ; 0.4229204534192644388475065 ; 0.4447767211697226217818454 ; 0.4663712423755613514331869 ;
 0.4876913088822746111853066 ; 0.5087243740491428186199463 ; 0.5294580601328034000099406 ; 0.5498801655714271702189358 ; 0.5699786721652138894754096 ;
 0.5897417521489813916767844 ; 0.6091577751526861909563306 ; 0.6282153150457794374886895 ; 0.6469031566613704719753373 ; 0.6652103023962409818802202 ;
 0.6831259786828258512462248 ; 0.7006396423293521790044710 ; 0.7177409867244055767721220 ; 0.7344199479022727047791516 ; 0.7506667104654910227632368 ;
 0.7664717133611208816717785 ; 0.7818256555073413245387500 ; 0.7967195012670592680339606 ; 0.8111444857653120742087717 ; 0.8250921200473358809210133 ;
 0.8385541960742664442975407 ; 0.8515227915535356930243826 ; 0.8639902746011257878940216 ; 0.875949308232943389203522 ; 0.887392854682680366503497 ;
 0.898314179543633885043599 ; 0.908706855732069633124554 ; 0.918564767269828625222511 ; 0.927882112884003620431730 ; 0.936653409421651460528462 ;
 0.944873495077673472678476 ; 0.952537532434209047102773 ; 0.959641011310191890416812 ; 0.966179751420209719777876 ; 0.972149904842703429727416 ;
 0.977547958299367247444781 ; 0.982370735251741311550742 ; 0.986615397831347502200576 ; 0.990279448648817838920769 ; 0.993360732621071281485401 ;
 0.995857439314283198214911 ; 0.997768108052585272142946 ; 0.999091650469640998651439 ; 0.999827581847748719107744 |] ;;

let gauss_legendre_129_w = [| 0.00044246794182939296923668005717 ; 0.00102972844619622394463273519315 ; 0.0016172530556785534682413679271 ; 0.0022039015180966937075786419741 ;
 0.0027892681877797554940944677057 ; 0.0033729979506246246117755709288 ; 0.0039547444682113562172392974765 ; 0.0045341644298525434513226874954 ; 0.0051109164669246267289761565766 ;
 0.0056846609912469045788016012203 ; 0.0062550602724461408889348709586 ; 0.0068217785893519121070498527769 ; 0.0073844824072454014447165055698 ; 0.0079428405646668029041114107832 ;
 0.0084965244635723279730542832506 ; 0.0090452082602137316404219313819 ; 0.0095885690555104190787301294510 ; 0.0101262870842733548093160774580 ; 0.0106580459029055185304204093001 ;
 0.0111835325753305049735380697538 ; 0.011702437856964778185746436834 ; 0.012214456376582979416221105914 ; 0.012719286815944623465099036330 ; 0.013216632087061724231482387345 ;
 0.013706199506993971244060563234 ; 0.014187700970062900419317230938 ; 0.014660853117380060971041027493 ; 0.015125377503587024690403432771 ; 0.015581000760707523415881287558 ;
 0.016027454759014214436403950465 ; 0.016464476764814667467169189640 ; 0.016891809595063204177526208819 ; 0.017309201768707240731293596444 ; 0.017716407654678809269702031810 ;
 0.018113187616443980503999783812 ; 0.018499308153024985727791918518 ; 0.018874542036411948181617592169 ; 0.019238668445283284085199492202 ; 0.019591473094956024580283987216 ;
 0.019932748363489542089706675388 ; 0.020262293413868438317104423081 ; 0.020579914312192665948185517085 ; 0.020885424141805311409990024684 ; 0.021178643113290860912881038703 ;
 0.021459398670279205389981598196 ; 0.021727525590993110687305178710 ; 0.021982866085479386179554968899 ; 0.022225269888466526554736910919 ; 0.022454594347794176432066564511 ;
 0.022670704508362374313093970958 ; 0.022873473191551169638592083492 ; 0.023062781070063872924670495006 ; 0.023238516738149892544490435771 ; 0.023400576777165831146714346635 ;
 0.023548865816436258377269094263 ; 0.023683296589378342897341543485 ; 0.023803789984857314051325299744 ; 0.023910275093742530302367230296 ; 0.024002689250636756075547029720 ;
 0.024080978070754089272959634041 ; 0.024145095481924836783843156014 ; 0.024195003751708503129818111597 ; 0.024230673509598936275508460625 ; 0.024252083764308562906498864071 ;
 0.02425922191612154143202867472 ; 0.024252083764308562906498864071 ; 0.024230673509598936275508460625 ; 0.024195003751708503129818111597 ; 0.024145095481924836783843156014 ;
 0.024080978070754089272959634041 ; 0.024002689250636756075547029720 ; 0.023910275093742530302367230296 ; 0.023803789984857314051325299744 ; 0.023683296589378342897341543485 ;
 0.023548865816436258377269094263 ; 0.023400576777165831146714346635 ; 0.023238516738149892544490435771 ; 0.023062781070063872924670495006 ; 0.022873473191551169638592083492 ;
 0.022670704508362374313093970958 ; 0.022454594347794176432066564511 ; 0.022225269888466526554736910919 ; 0.021982866085479386179554968899 ; 0.021727525590993110687305178710 ;
 0.021459398670279205389981598196 ; 0.021178643113290860912881038703 ; 0.020885424141805311409990024684 ; 0.020579914312192665948185517085 ; 0.020262293413868438317104423081 ;
 0.019932748363489542089706675388 ; 0.019591473094956024580283987216 ; 0.019238668445283284085199492202 ; 0.018874542036411948181617592169 ; 0.018499308153024985727791918518 ;
 0.018113187616443980503999783812 ; 0.017716407654678809269702031810 ; 0.017309201768707240731293596444 ; 0.016891809595063204177526208819 ; 0.016464476764814667467169189640 ;
 0.016027454759014214436403950465 ; 0.015581000760707523415881287558 ; 0.015125377503587024690403432771 ; 0.014660853117380060971041027493 ; 0.014187700970062900419317230938 ;
 0.013706199506993971244060563234 ; 0.013216632087061724231482387345 ; 0.012719286815944623465099036330 ; 0.012214456376582979416221105914 ; 0.011702437856964778185746436834 ;
 0.0111835325753305049735380697538 ; 0.0106580459029055185304204093001 ; 0.0101262870842733548093160774580 ; 0.0095885690555104190787301294510 ; 0.0090452082602137316404219313819 ;
 0.0084965244635723279730542832506 ; 0.0079428405646668029041114107832 ; 0.0073844824072454014447165055698 ; 0.0068217785893519121070498527769 ; 0.0062550602724461408889348709586 ;
 0.0056846609912469045788016012203 ; 0.0051109164669246267289761565766 ; 0.0045341644298525434513226874954 ; 0.0039547444682113562172392974765 ; 0.0033729979506246246117755709288 ;
 0.0027892681877797554940944677057 ; 0.0022039015180966937075786419741 ; 0.0016172530556785534682413679271 ; 0.00102972844619622394463273519315 ; 0.00044246794182939296923668005717 |] ;;


let gauss_legendre_255_x = [| -0.999955705317563751730191 ; -0.999766621312000569367063 ; -0.999426474680169959344386 ; -0.998935241284654635142155 ;
 -0.998292986136967889228248 ; -0.997499804126615814044844 ; -0.996555814435198617028738 ; -0.995461159480026294089975 ; -0.994216004616630164799381 ;
 -0.992820538021989138984811 ; -0.991274970630385567164523 ; -0.989579536085920123498574 ; -0.987734490699732356281248 ; -0.985740113407419277752900 ;
 -0.983596705724776358640192 ; -0.981304591701017185126565 ; -0.978864117869068155239121 ; -0.976275653192735980815246 ; -0.973539589010643617645393 ;
 -0.970656338976880365477697 ; -0.967626338998338798105523 ; -0.964450047168726298761719 ; -0.961127943699247839572910 ; -0.957660530845962076295490 ;
 -0.954048332833816317950921 ; -0.950291895777368285733522 ; -0.946391787598204251752103 ; -0.942348597939064408301480 ; -0.938162938074687317626793 ;
 -0.933835440819386124349338 ; -0.929366760431369935739045 ; -0.924757572513824425220425 ; -0.920008573912766315142721 ; -0.915120482611686961035103 ;
 -0.910094037623000801254172 ; -0.904929998876314959753358 ; -0.899629147103536800144342 ; -0.894192283720836729335637 ; -0.888620230707484040924981 ;
 -0.882913830481574073645470 ; -0.877073945772665439532627 ; -0.871101459491346550796200 ; -0.864997274595751144137121 ; -0.858762313955042966785823 ;
 -0.852397520209890250084237 ; -0.845903855629951054143931 ; -0.839282301968391021084600 ; -0.832533860313455524647230 ; -0.825659550937118650611534 ;
 -0.818660413140831885432406 ; -0.811537505098395829833580 ; -0.804291903695978689734633 ; -0.796924704369305728807154 ; -0.789437020938044295117764 ;
 -0.781829985437409458675147 ; -0.774104747947015717207115 ; -0.766262476417000644100858 ; -0.758304356491446765092016 ; -0.750231591329128358931528 ;
 -0.742045401421610281838045 ; -0.733747024408726316001889 ; -0.725337714891464938687812 ; -0.716818744242290800531501 ; -0.708191400412930589382399 ;
 -0.699456987739652339456557 ; -0.690616826746067624571761 ; -0.681672253943486448787259 ; -0.672624621628855017806731 ; -0.663475297680306939970658 ;
 -0.654225665350358766508700 ; -0.644877123056781136890077 ; -0.635431084171177146547142 ; -0.625888976805299900901619 ; -0.616252243595141561442344 ;
 -0.606522341482826526536576 ; -0.596700741496341721653202 ; -0.586788928527137300685706 ; -0.576788401105631382036211 ; -0.566700671174652760010815 ;
 -0.556527263860855843833077 ; -0.546269717244142383159817 ; -0.535929582125124840335150 ; -0.525508421790666565699453 ; -0.515007811777534223035005 ;
 -0.504429339634198197635551 ; -0.493774604680816999489812 ; -0.483045217767441948626854 ; -0.472242801030478698742627 ; -0.461368987647442418771401 ;
 -0.450425421590043710043279 ; -0.439413757375642589040685 ; -0.428335659817108112494341 ; -0.417192803771121462605751 ; -0.405986873884960545511889 ;
 -0.394719564341804385683361 ; -0.383392578604595822734854 ; -0.372007629158501235092510 ; -0.360566437252006227074021 ; -0.349070732636686422161576 ;
 -0.337522253305692705554261 ; -0.325922745230990453444769 ; -0.314273962099392474845918 ; -0.302577665047425574167140 ; -0.290835622395070819082047 ;
 -0.279049609378417768508970 ; -0.267221407881273079721012 ; -0.255352806165764071686080 ; -0.243445598601977973686482 ; -0.231501585396677734059116 ;
 -0.219522572321135403508985 ; -0.207510370438124240859625 ; -0.195466795828110816293869 ; -0.183393669314688508087976 ; -0.171292816189293903533225 ;
 -0.159166065935247723154292 ; -0.147015251951161989456661 ; -0.134842211273755257250625 ; -0.122648784300117812092492 ; -0.110436814509468826540991 ;
 -0.098208148184447540736015 ; -0.085964634131980604256000 ; -0.073708123403767780288977 ; -0.061440469016428270850728 ; -0.049163525671349973093019 ;
 -0.036879149474284021657652 ; -0.024589197654727010541405 ; -0.012295528285133320036860 ; 0.0 ; 0.012295528285133320036860 ; 0.024589197654727010541405 ;
 0.036879149474284021657652 ; 0.049163525671349973093019 ; 0.061440469016428270850728 ; 0.073708123403767780288977 ; 0.085964634131980604256000 ;
 0.098208148184447540736015 ; 0.110436814509468826540991 ; 0.122648784300117812092492 ; 0.134842211273755257250625 ; 0.147015251951161989456661 ;
 0.159166065935247723154292 ; 0.171292816189293903533225 ; 0.183393669314688508087976 ; 0.195466795828110816293869 ; 0.207510370438124240859625 ;
 0.219522572321135403508985 ; 0.231501585396677734059116 ; 0.243445598601977973686482 ; 0.255352806165764071686080 ; 0.267221407881273079721012 ;
 0.279049609378417768508970 ; 0.290835622395070819082047 ; 0.302577665047425574167140 ; 0.314273962099392474845918 ; 0.325922745230990453444769 ;
 0.337522253305692705554261 ; 0.349070732636686422161576 ; 0.360566437252006227074021 ; 0.372007629158501235092510 ; 0.383392578604595822734854 ;
 0.394719564341804385683361 ; 0.405986873884960545511889 ; 0.417192803771121462605751 ; 0.428335659817108112494341 ; 0.439413757375642589040685 ;
 0.450425421590043710043279 ; 0.461368987647442418771401 ; 0.472242801030478698742627 ; 0.483045217767441948626854 ; 0.493774604680816999489812 ;
 0.504429339634198197635551 ; 0.515007811777534223035005 ; 0.525508421790666565699453 ; 0.535929582125124840335150 ; 0.546269717244142383159817 ;
 0.556527263860855843833077 ; 0.566700671174652760010815 ; 0.576788401105631382036211 ; 0.586788928527137300685706 ; 0.596700741496341721653202 ;
 0.606522341482826526536576 ; 0.616252243595141561442344 ; 0.625888976805299900901619 ; 0.635431084171177146547142 ; 0.644877123056781136890077 ;
 0.654225665350358766508700 ; 0.663475297680306939970658 ; 0.672624621628855017806731 ; 0.681672253943486448787259 ; 0.690616826746067624571761 ;
 0.699456987739652339456557 ; 0.708191400412930589382399 ; 0.716818744242290800531501 ; 0.725337714891464938687812 ; 0.733747024408726316001889 ;
 0.742045401421610281838045 ; 0.750231591329128358931528 ; 0.758304356491446765092016 ; 0.766262476417000644100858 ; 0.774104747947015717207115 ;
 0.781829985437409458675147 ; 0.789437020938044295117764 ; 0.796924704369305728807154 ; 0.804291903695978689734633 ; 0.811537505098395829833580 ;
 0.818660413140831885432406 ; 0.825659550937118650611534 ; 0.832533860313455524647230 ; 0.839282301968391021084600 ; 0.845903855629951054143931 ;
 0.852397520209890250084237 ; 0.858762313955042966785823 ; 0.864997274595751144137121 ; 0.871101459491346550796200 ; 0.877073945772665439532627 ;
 0.882913830481574073645470 ; 0.888620230707484040924981 ; 0.894192283720836729335637 ; 0.899629147103536800144342 ; 0.904929998876314959753358 ;
 0.910094037623000801254172 ; 0.915120482611686961035103 ; 0.920008573912766315142721 ; 0.924757572513824425220425 ; 0.929366760431369935739045 ;
 0.933835440819386124349338 ; 0.938162938074687317626793 ; 0.942348597939064408301480 ; 0.946391787598204251752103 ; 0.950291895777368285733522 ;
 0.954048332833816317950921 ; 0.957660530845962076295490 ; 0.961127943699247839572910 ; 0.964450047168726298761719 ; 0.967626338998338798105523 ;
 0.970656338976880365477697 ; 0.973539589010643617645393 ; 0.976275653192735980815246 ; 0.978864117869068155239121 ; 0.981304591701017185126565 ;
 0.983596705724776358640192 ; 0.985740113407419277752900 ; 0.987734490699732356281248 ; 0.989579536085920123498574 ; 0.991274970630385567164523 ;
 0.992820538021989138984811 ; 0.994216004616630164799381 ; 0.995461159480026294089975 ; 0.996555814435198617028738 ; 0.997499804126615814044844 ;
 0.998292986136967889228248 ; 0.998935241284654635142155 ; 0.999426474680169959344386 ; 0.999766621312000569367063 ; 0.999955705317563751730191 |] ;;

let gauss_legendre_255_w = [| 0.00011367361999142272115645954414 ; 0.00026459387119083065532790838855 ; 0.00041569762526823913616284210066 ; 0.00056675794564824918946626058353 ;
 0.00071773647800611087798371518325 ; 0.00086860766611945667949717690640 ; 0.00101934797642732530281229369360 ; 0.0011699343729388079886897709773 ; 0.0013203439900221692090523602144 ;
 0.0014705540427783843160097204304 ; 0.0016205417990415653896921100325 ; 0.0017702845706603213070421243905 ; 0.0019197597117132050055085980675 ; 0.0020689446195015801533643667413 ;
 0.0022178167367540171700373764020 ; 0.0023663535543962867157201855305 ; 0.0025145326145997073931298921370 ; 0.0026623315139717112732749157331 ; 0.0028097279068204407457332299361 ;
 0.0029566995084575002760043344138 ; 0.0031032240985191112621977893133 ; 0.0032492795242943133198690930777 ; 0.0033948437040533928255056951665 ; 0.0035398946303722552150296713510 ;
 0.0036844103734499176530742235517 ; 0.0038283690844171626400743524999 ; 0.0039717489986349171988699773906 ; 0.0041145284389812475901826468094 ; 0.0042566858191260658425395494472 ;
 0.0043981996467927779838546384780 ; 0.0045390485270061921259394035112 ; 0.0046792111653260640506279893190 ; 0.0048186663710656988918572043815 ; 0.0049573930604950563104281084148 ;
 0.0050953702600278273039420404117 ; 0.0052325771093919661294970523234 ; 0.0053689928647831724787741258653 ; 0.0055045969020008281904902120813 ; 0.0056393687195659001929970994675 ;
 0.0057732879418203275712033691864 ; 0.0059063343220074160130475409466 ; 0.0060384877453327676663371666884 ; 0.0061697282320052788060812561217 ; 0.0063000359402577418025981070425 ;
 0.0064293911693465917826140832500 ; 0.0065577743625303421548456356354 ; 0.0066851661100262568757892743568 ; 0.0068115471519448109954345674817 ; 0.0069368983812014946719507501243 ;
 0.0070612008464055194979848418291 ; 0.0071844357547249896530757997058 ; 0.0073065844747281040972736443146 ; 0.0074276285391999597581348419714 ; 0.0075475496479345294426435656724 ;
 0.0076663296705013920315933272426 ; 0.0077839506489867963897419914623 ; 0.0079003948007086443529587296692 ; 0.0080156445209049821352946484008 ; 0.0081296823853955935356080649925 ;
 0.0082424911532162924158504385939 ; 0.0083540537692255160718568405530 ; 0.0084643533666828253227353760036 ; 0.0085733732697989214067758505840 ; 0.0086810969962567940901133439612 ;
 0.0087875082597036197689825483144 ; 0.0088925909722130327769834298578 ; 0.0089963292467173975949700110383 ; 0.0090987073994097142025303711406 ; 0.0091997099521147934060534414075 ;
 0.0092993216346293436285393234867 ; 0.0093975273870306153500305317074 ; 0.0094943123619532541442165010292 ; 0.0095896619268340180657610209655 ; 0.0096835616661240200035669970076 ;
 0.0097759973834681605268499842249 ; 0.0098669551038514217128483481814 ; 0.0099564210757116974565448593910 ; 0.0100443817730188408231888789497 ; 0.0101308238973196141129538950955 ;
 0.0102157343797482324629939488415 ; 0.0102991003830021970147153502911 ; 0.0103809093032831189224876935085 ; 0.0104611487722022407735015844669 ; 0.0105398066586503673262517188088 ;
 0.0106168710706319228563864391054 ; 0.0106923303570628578226139809571 ; 0.0107661731095321330311788312990 ; 0.0108383881640265149842990798832 ; 0.0109089646026184216450603134401 ;
 0.0109778917551165634377595759712 ; 0.0110451592006791299277436662993 ; 0.0111107567693892782875426356195 ; 0.0111746745437926853557086684962 ; 0.0112369028603969308303734810332 ;
 0.0112974323111324849102690558722 ; 0.0113562537447750795009464486204 ; 0.011413358268329247942299599697 ; 0.011468737248372824084374355981 ; 0.011522382312362197440930930031 ;
 0.011574285349898127083439539046 ; 0.011624438513951922901227922331 ; 0.011672834222051808845465154244 ; 0.011719465157429288794653489478 ; 0.011764324270125341726399410909 ;
 0.011807404778056278953532930501 ; 0.011848700168039102281222824051 ; 0.011888204196776208064673282076 ; 0.011925910891799288293359117699 ; 0.011961814552372285996633285380 ;
 0.011995909750353268455989686823 ; 0.012028191331015087920350431142 ; 0.012058654413824705751531083631 ; 0.012087294393181062176578184854 ; 0.012114106939111380091025793650 ;
 0.012139087997925797641334635250 ; 0.012162233792830230614908682534 ; 0.012183540824497371981177306326 ; 0.012203005871595742256331865516 ; 0.012220625991276710706457005806 ;
 0.012236398519619413758040249691 ; 0.012250321072033503350218104906 ; 0.012262391543619664338660618398 ; 0.012272608109487846445745237751 ; 0.012280969225033162644659793962 ;
 0.012287473626169412265336919908 ; 0.012292120329520193516690694701 ; 0.012294908632567576531532225710 ; 0.01229583811375831445681490730 ; 0.012294908632567576531532225710 ;
 0.012292120329520193516690694701 ; 0.012287473626169412265336919908 ; 0.012280969225033162644659793962 ; 0.012272608109487846445745237751 ; 0.012262391543619664338660618398 ;
 0.012250321072033503350218104906 ; 0.012236398519619413758040249691 ; 0.012220625991276710706457005806 ; 0.012203005871595742256331865516 ; 0.012183540824497371981177306326 ;
 0.012162233792830230614908682534 ; 0.012139087997925797641334635250 ; 0.012114106939111380091025793650 ; 0.012087294393181062176578184854 ; 0.012058654413824705751531083631 ;
 0.012028191331015087920350431142 ; 0.011995909750353268455989686823 ; 0.011961814552372285996633285380 ; 0.011925910891799288293359117699 ; 0.011888204196776208064673282076 ;
 0.011848700168039102281222824051 ; 0.011807404778056278953532930501 ; 0.011764324270125341726399410909 ; 0.011719465157429288794653489478 ; 0.011672834222051808845465154244 ;
 0.011624438513951922901227922331 ; 0.011574285349898127083439539046 ; 0.011522382312362197440930930031 ; 0.011468737248372824084374355981 ; 0.011413358268329247942299599697 ;
 0.0113562537447750795009464486204 ; 0.0112974323111324849102690558722 ; 0.0112369028603969308303734810332 ; 0.0111746745437926853557086684962 ; 0.0111107567693892782875426356195 ;
 0.0110451592006791299277436662993 ; 0.0109778917551165634377595759712 ; 0.0109089646026184216450603134401 ; 0.0108383881640265149842990798832 ; 0.0107661731095321330311788312990 ;
 0.0106923303570628578226139809571 ; 0.0106168710706319228563864391054 ; 0.0105398066586503673262517188088 ; 0.0104611487722022407735015844669 ; 0.0103809093032831189224876935085 ;
 0.0102991003830021970147153502911 ; 0.0102157343797482324629939488415 ; 0.0101308238973196141129538950955 ; 0.0100443817730188408231888789497 ; 0.0099564210757116974565448593910 ;
 0.0098669551038514217128483481814 ; 0.0097759973834681605268499842249 ; 0.0096835616661240200035669970076 ; 0.0095896619268340180657610209655 ; 0.0094943123619532541442165010292 ;
 0.0093975273870306153500305317074 ; 0.0092993216346293436285393234867 ; 0.0091997099521147934060534414075 ; 0.0090987073994097142025303711406 ; 0.0089963292467173975949700110383 ;
 0.0088925909722130327769834298578 ; 0.0087875082597036197689825483144 ; 0.0086810969962567940901133439612 ; 0.0085733732697989214067758505840 ; 0.0084643533666828253227353760036 ;
 0.0083540537692255160718568405530 ; 0.0082424911532162924158504385939 ; 0.0081296823853955935356080649925 ; 0.0080156445209049821352946484008 ; 0.0079003948007086443529587296692 ;
 0.0077839506489867963897419914623 ; 0.0076663296705013920315933272426 ; 0.0075475496479345294426435656724 ; 0.0074276285391999597581348419714 ; 0.0073065844747281040972736443146 ;
 0.0071844357547249896530757997058 ; 0.0070612008464055194979848418291 ; 0.0069368983812014946719507501243 ; 0.0068115471519448109954345674817 ; 0.0066851661100262568757892743568 ;
 0.0065577743625303421548456356354 ; 0.0064293911693465917826140832500 ; 0.0063000359402577418025981070425 ; 0.0061697282320052788060812561217 ; 0.0060384877453327676663371666884 ;
 0.0059063343220074160130475409466 ; 0.0057732879418203275712033691864 ; 0.0056393687195659001929970994675 ; 0.0055045969020008281904902120813 ; 0.0053689928647831724787741258653 ;
 0.0052325771093919661294970523234 ; 0.0050953702600278273039420404117 ; 0.0049573930604950563104281084148 ; 0.0048186663710656988918572043815 ; 0.0046792111653260640506279893190 ;
 0.0045390485270061921259394035112 ; 0.0043981996467927779838546384780 ; 0.0042566858191260658425395494472 ; 0.0041145284389812475901826468094 ; 0.0039717489986349171988699773906 ;
 0.0038283690844171626400743524999 ; 0.0036844103734499176530742235517 ; 0.0035398946303722552150296713510 ; 0.0033948437040533928255056951665 ; 0.0032492795242943133198690930777 ;
 0.0031032240985191112621977893133 ; 0.0029566995084575002760043344138 ; 0.0028097279068204407457332299361 ; 0.0026623315139717112732749157331 ; 0.0025145326145997073931298921370 ;
 0.0023663535543962867157201855305 ; 0.0022178167367540171700373764020 ; 0.0020689446195015801533643667413 ; 0.0019197597117132050055085980675 ; 0.0017702845706603213070421243905 ;
 0.0016205417990415653896921100325 ; 0.0014705540427783843160097204304 ; 0.0013203439900221692090523602144 ; 0.0011699343729388079886897709773 ; 0.00101934797642732530281229369360 ;
 0.00086860766611945667949717690640 ; 0.00071773647800611087798371518325 ; 0.00056675794564824918946626058353 ; 0.00041569762526823913616284210066 ; 0.00026459387119083065532790838855 ;
 0.00011367361999142272115645954414 |] ;;


let gauss_legendre_256_x = [| -0.999956050018992230734801 ; -0.999768437409263186104879 ; -0.999430937466261408240854 ; -0.998943525843408856555026 ;
 -0.998306266473006444055500 ; -0.997519252756720827563409 ; -0.996582602023381540430504 ; -0.995496454481096356592647 ; -0.994260972922409664962878 ;
 -0.992876342608822117143534 ; -0.991342771207583086922189 ; -0.989660488745065218319244 ; -0.987829747564860608916488 ; -0.985850822286125956479245 ;
 -0.983724009760315496166686 ; -0.981449629025464405769303 ; -0.979028021257622038824238 ; -0.976459549719234155621011 ; -0.973744599704370405266079 ;
 -0.970883578480743029320923 ; -0.967876915228489454909004 ; -0.964725060975706430932612 ; -0.961428488530732144006407 ; -0.957987692411178129365790 ;
 -0.954403188769716241764448 ; -0.950675515316628276363852 ; -0.946805231239127481372052 ; -0.942792917117462443183076 ; -0.938639174837814804981926 ;
 -0.934344627502003094292477 ; -0.929909919334005641180246 ; -0.925335715583316202872730 ; -0.920622702425146495505047 ; -0.915771586857490384526670 ;
 -0.910783096595065011890907 ; -0.905657979960144647082682 ; -0.900397005770303544771620 ; -0.895000963223084577441223 ; -0.889470661777610888828677 ;
 -0.883806931033158284859826 ; -0.878010620604706543986435 ; -0.872082599995488289130046 ; -0.866023758466554519297515 ; -0.859835004903376350696173 ;
 -0.853517267679502965073036 ; -0.847071494517296207187072 ; -0.840498652345762713895068 ; -0.833799727155504894348444 ; -0.826975723850812514289093 ;
 -0.820027666098917067403478 ; -0.812956596176431543136410 ; -0.805763574812998623257389 ; -0.798449681032170758782543 ; -0.791016011989545994546707 ;
 -0.783463682808183820750670 ; -0.775793826411325739132053 ; -0.768007593352445635975891 ; -0.760106151642655454941907 ; -0.752090686575492059587530 ;
 -0.743962400549111568455683 ; -0.735722512885917834620373 ; -0.727372259649652126586894 ; -0.718912893459971448372640 ; -0.710345683304543313394566 ;
 -0.701671914348685159406084 ; -0.692892887742576960105342 ; -0.684009920426075953124877 ; -0.675024344931162763855919 ; -0.665937509182048559906408 ;
 -0.656750776292973221887500 ; -0.647465524363724862617016 ; -0.638083146272911368668689 ; -0.628605049469014975432210 ; -0.619032655759261219430968 ;
 -0.609367401096333939522311 ; -0.599610735362968321730388 ; -0.589764122154454300785786 ; -0.579829038559082944921832 ; -0.569806974936568759057668 ;
 -0.559699434694481145136907 ; -0.549507934062718557042427 ; -0.539234001866059181127936 ; -0.528879179294822261951476 ; -0.518445019673674476221662 ;
 -0.507933088228616036231925 ; -0.497344961852181477119512 ; -0.486682228866890350103621 ; -0.475946488786983306390738 ; -0.465139352078479313645570 ;
 -0.454262439917589998774455 ; -0.443317383947527357216926 ; -0.432305826033741309953441 ; -0.421229418017623824976812 ; -0.410089821468716550006434 ;
 -0.398888707435459127713463 ; -0.387627756194515583637985 ; -0.376308656998716390283056 ; -0.364933107823654018533465 ; -0.353502815112969989537790 ;
 -0.342019493522371636480730 ; -0.330484865662416976229187 ; -0.318900661840106275631683 ; -0.307268619799319076258610 ; -0.295590484460135614563787 ;
 -0.283868007657081741799766 ; -0.272102947876336609505245 ; -0.260297069991942541978561 ; -0.248452145001056666833243 ; -0.236569949758284018477508 ;
 -0.224652266709131967147878 ; -0.212700883622625957937040 ; -0.200717593323126670068001 ; -0.188704193421388826461504 ; -0.176662486044901997403722 ;
 -0.164594277567553849829285 ; -0.152501378338656395374607 ; -0.140385602411375885913025 ; -0.128248767270607094742050 ; -0.116092693560332804940735 ;
 -0.103919204810509403639197 ; -0.091730127163519552031146 ; -0.079527289100232965903227 ; -0.067312521165716400242290 ; -0.055087655694633984104561 ;
 -0.042854526536379098381242 ; -0.030614968779979029366279 ; -0.018370818478813665117926 ; -0.006123912375189529501170 ; 0.006123912375189529501170 ;
 0.018370818478813665117926 ; 0.030614968779979029366279 ; 0.042854526536379098381242 ; 0.055087655694633984104561 ; 0.067312521165716400242290 ;
 0.079527289100232965903227 ; 0.091730127163519552031146 ; 0.103919204810509403639197 ; 0.116092693560332804940735 ; 0.128248767270607094742050 ;
 0.140385602411375885913025 ; 0.152501378338656395374607 ; 0.164594277567553849829285 ; 0.176662486044901997403722 ; 0.188704193421388826461504 ;
 0.200717593323126670068001 ; 0.212700883622625957937040 ; 0.224652266709131967147878 ; 0.236569949758284018477508 ; 0.248452145001056666833243 ;
 0.260297069991942541978561 ; 0.272102947876336609505245 ; 0.283868007657081741799766 ; 0.295590484460135614563787 ; 0.307268619799319076258610 ;
 0.318900661840106275631683 ; 0.330484865662416976229187 ; 0.342019493522371636480730 ; 0.353502815112969989537790 ; 0.364933107823654018533465 ;
 0.376308656998716390283056 ; 0.387627756194515583637985 ; 0.398888707435459127713463 ; 0.410089821468716550006434 ; 0.421229418017623824976812 ;
 0.432305826033741309953441 ; 0.443317383947527357216926 ; 0.454262439917589998774455 ; 0.465139352078479313645570 ; 0.475946488786983306390738 ;
 0.486682228866890350103621 ; 0.497344961852181477119512 ; 0.507933088228616036231925 ; 0.518445019673674476221662 ; 0.528879179294822261951476 ;
 0.539234001866059181127936 ; 0.549507934062718557042427 ; 0.559699434694481145136907 ; 0.569806974936568759057668 ; 0.579829038559082944921832 ;
 0.589764122154454300785786 ; 0.599610735362968321730388 ; 0.609367401096333939522311 ; 0.619032655759261219430968 ; 0.628605049469014975432210 ;
 0.638083146272911368668689 ; 0.647465524363724862617016 ; 0.656750776292973221887500 ; 0.665937509182048559906408 ; 0.675024344931162763855919 ;
 0.684009920426075953124877 ; 0.692892887742576960105342 ; 0.701671914348685159406084 ; 0.710345683304543313394566 ; 0.718912893459971448372640 ;
 0.727372259649652126586894 ; 0.735722512885917834620373 ; 0.743962400549111568455683 ; 0.752090686575492059587530 ; 0.760106151642655454941907 ;
 0.768007593352445635975891 ; 0.775793826411325739132053 ; 0.783463682808183820750670 ; 0.791016011989545994546707 ; 0.798449681032170758782543 ;
 0.805763574812998623257389 ; 0.812956596176431543136410 ; 0.820027666098917067403478 ; 0.826975723850812514289093 ; 0.833799727155504894348444 ;
 0.840498652345762713895068 ; 0.847071494517296207187072 ; 0.853517267679502965073036 ; 0.859835004903376350696173 ; 0.866023758466554519297515 ;
 0.872082599995488289130046 ; 0.878010620604706543986435 ; 0.883806931033158284859826 ; 0.889470661777610888828677 ; 0.895000963223084577441223 ;
 0.900397005770303544771620 ; 0.905657979960144647082682 ; 0.910783096595065011890907 ; 0.915771586857490384526670 ; 0.920622702425146495505047 ;
 0.925335715583316202872730 ; 0.929909919334005641180246 ; 0.934344627502003094292477 ; 0.938639174837814804981926 ; 0.942792917117462443183076 ;
 0.946805231239127481372052 ; 0.950675515316628276363852 ; 0.954403188769716241764448 ; 0.957987692411178129365790 ; 0.961428488530732144006407 ;
 0.964725060975706430932612 ; 0.967876915228489454909004 ; 0.970883578480743029320923 ; 0.973744599704370405266079 ; 0.976459549719234155621011 ;
 0.979028021257622038824238 ; 0.981449629025464405769303 ; 0.983724009760315496166686 ; 0.985850822286125956479245 ; 0.987829747564860608916488 ;
 0.989660488745065218319244 ; 0.991342771207583086922189 ; 0.992876342608822117143534 ; 0.994260972922409664962878 ; 0.995496454481096356592647 ;
 0.996582602023381540430504 ; 0.997519252756720827563409 ; 0.998306266473006444055500 ; 0.998943525843408856555026 ; 0.999430937466261408240854 ;
 0.999768437409263186104879 ; 0.999956050018992230734801 |] ;;

let gauss_legendre_256_w = [| 0.00011278901782227217551253887725 ; 0.00026253494429644590628745756250 ; 0.00041246325442617632843218583774 ; 0.00056234895403140980281523674759 ;
 0.0007121541634733206669089891511 ; 0.0008618537014200890378140934163 ; 0.0010114243932084404526058128414 ; 0.0011608435575677247239705981135 ; 0.0013100886819025044578316804271 ;
 0.0014591373333107332010883864996 ; 0.0016079671307493272424499395690 ; 0.0017565557363307299936069145295 ; 0.0019048808534997184044191411746 ; 0.0020529202279661431745487818492 ;
 0.0022006516498399104996848834189 ; 0.0023480529563273120170064609087 ; 0.0024951020347037068508395354372 ; 0.0026417768254274905641208292516 ; 0.0027880553253277068805747610763 ;
 0.0029339155908297166460123254142 ; 0.0030793357411993375832053528316 ; 0.0032242939617941981570107134269 ; 0.0033687685073155510120191062489 ; 0.0035127377050563073309710549844 ;
 0.0036561799581425021693892413052 ; 0.0037990737487662579981170192082 ; 0.0039413976414088336277290349840 ; 0.0040831302860526684085997759212 ; 0.0042242504213815362723565049060 ;
 0.0043647368779680566815684200621 ; 0.0045045685814478970686417923159 ; 0.0046437245556800603139790923525 ; 0.0047821839258926913729317340448 ; 0.0049199259218138656695587765655 ;
 0.0050569298807868423875578160762 ; 0.0051931752508692809303287536296 ; 0.0053286415939159303170811114788 ; 0.0054633085886443102775705318566 ; 0.0055971560336829100775514452572 ;
 0.005730163850601437177384417555 ; 0.005862312086922653060661598801 ; 0.005993580919115338221127696870 ; 0.006123950655567932542389081187 ; 0.006253401739542401272063645975 ;
 0.006381914752107880570375164275 ; 0.006509470415053660267809899951 ; 0.006636049593781065044590038355 ; 0.006761633300173798780927861108 ; 0.006886202695446320346713323775 ;
 0.007009739092969822621234436194 ; 0.007132223961075390071672422986 ; 0.007253638925833913783829137214 ; 0.007373965773812346437572440695 ; 0.007493186454805883358599761133 ;
 0.007611283084545659461618719618 ; 0.007728237947381555631110194958 ; 0.007844033498939711866810316151 ; 0.007958652368754348353613161227 ; 0.008072077362873499500946974804 ;
 0.008184291466438269935619761004 ; 0.008295277846235225425171412553 ; 0.008405019853221535756180301698 ; 0.008513501025022490693838354790 ; 0.008620705088401014305368838410 ;
 0.008726615961698807140336632217 ; 0.008831217757248750025318272685 ; 0.008934494783758207548408417085 ; 0.009036431548662873680227775572 ; 0.009137012760450806402000472219 ;
 0.009236223330956302687378716714 ; 0.009334048377623269712466014486 ; 0.009430473225737752747352764482 ; 0.009525483410629284811829685754 ; 0.009619064679840727857162164401 ;
 0.009711202995266279964249670496 ; 0.009801884535257327825498800250 ; 0.009891095696695828602630683809 ; 0.009978823097034910124733949495 ; 0.010065053576306383309460978930 ;
 0.010149774199094865654634066042 ; 0.010232972256478219656954857160 ; 0.010314635267934015068260713997 ; 0.010394750983211728997101725205 ; 0.010473307384170403003569566927 ;
 0.010550292686581481517533575536 ; 0.010625695341896561133961681801 ; 0.010699504038979785603048200583 ; 0.010771707705804626636653631927 ; 0.010842295511114795995293477058 ;
 0.010911256866049039700796847788 ; 0.010978581425729570637988203448 ; 0.011044259090813901263517571044 ; 0.011108280009009843630460815451 ; 0.011170634576553449462710881938 ;
 0.011231313439649668572656802083 ; 0.011290307495875509508367594121 ; 0.011347607895545491941625714297 ; 0.011403206043039185964847059552 ; 0.011457093598090639152334392298 ;
 0.011509262477039497958586392439 ; 0.011559704854043635772668656950 ; 0.011608413162253105722084706677 ; 0.011655380094945242121298939730 ; 0.011700598606620740288189823359 ;
 0.011744061914060550305376732759 ; 0.011785763497343426181690117627 ; 0.011825697100823977771160737958 ; 0.011863856734071078731904572908 ; 0.011900236672766489754287204237 ;
 0.011934831459563562255873201696 ; 0.011967635904905893729007282670 ; 0.011998645087805811934536710071 ; 0.012027854356582571161267533498 ; 0.012055259329560149814347085327 ;
 0.012080855895724544655975183976 ; 0.012104640215340463097757829736 ; 0.012126608720527321034718492205 ; 0.012146758115794459815559837664 ; 0.012165085378535502061307291839 ;
 0.012181587759481772174047585032 ; 0.012196262783114713518180974196 ; 0.012209108248037240407514094371 ; 0.012220122227303969191708737227 ; 0.012229303068710278904146266083 ;
 0.012236649395040158109242574767 ; 0.012242160104272800769728083260 ; 0.012245834369747920142463857550 ; 0.01224767164028975590407032649 ; 0.01224767164028975590407032649 ;
 0.012245834369747920142463857550 ; 0.012242160104272800769728083260 ; 0.012236649395040158109242574767 ; 0.012229303068710278904146266083 ; 0.012220122227303969191708737227 ;
 0.012209108248037240407514094371 ; 0.012196262783114713518180974196 ; 0.012181587759481772174047585032 ; 0.012165085378535502061307291839 ; 0.012146758115794459815559837664 ;
 0.012126608720527321034718492205 ; 0.012104640215340463097757829736 ; 0.012080855895724544655975183976 ; 0.012055259329560149814347085327 ; 0.012027854356582571161267533498 ;
 0.011998645087805811934536710071 ; 0.011967635904905893729007282670 ; 0.011934831459563562255873201696 ; 0.011900236672766489754287204237 ; 0.011863856734071078731904572908 ;
 0.011825697100823977771160737958 ; 0.011785763497343426181690117627 ; 0.011744061914060550305376732759 ; 0.011700598606620740288189823359 ; 0.011655380094945242121298939730 ;
 0.011608413162253105722084706677 ; 0.011559704854043635772668656950 ; 0.011509262477039497958586392439 ; 0.011457093598090639152334392298 ; 0.011403206043039185964847059552 ;
 0.011347607895545491941625714297 ; 0.011290307495875509508367594121 ; 0.011231313439649668572656802083 ; 0.011170634576553449462710881938 ; 0.011108280009009843630460815451 ;
 0.011044259090813901263517571044 ; 0.010978581425729570637988203448 ; 0.010911256866049039700796847788 ; 0.010842295511114795995293477058 ; 0.010771707705804626636653631927 ;
 0.010699504038979785603048200583 ; 0.010625695341896561133961681801 ; 0.010550292686581481517533575536 ; 0.010473307384170403003569566927 ; 0.010394750983211728997101725205 ;
 0.010314635267934015068260713997 ; 0.010232972256478219656954857160 ; 0.010149774199094865654634066042 ; 0.010065053576306383309460978930 ; 0.009978823097034910124733949495 ;
 0.009891095696695828602630683809 ; 0.009801884535257327825498800250 ; 0.009711202995266279964249670496 ; 0.009619064679840727857162164401 ; 0.009525483410629284811829685754 ;
 0.009430473225737752747352764482 ; 0.009334048377623269712466014486 ; 0.009236223330956302687378716714 ; 0.009137012760450806402000472219 ; 0.009036431548662873680227775572 ;
 0.008934494783758207548408417085 ; 0.008831217757248750025318272685 ; 0.008726615961698807140336632217 ; 0.008620705088401014305368838410 ; 0.008513501025022490693838354790 ;
 0.008405019853221535756180301698 ; 0.008295277846235225425171412553 ; 0.008184291466438269935619761004 ; 0.008072077362873499500946974804 ; 0.007958652368754348353613161227 ;
 0.007844033498939711866810316151 ; 0.007728237947381555631110194958 ; 0.007611283084545659461618719618 ; 0.007493186454805883358599761133 ; 0.007373965773812346437572440695 ;
 0.007253638925833913783829137214 ; 0.007132223961075390071672422986 ; 0.007009739092969822621234436194 ; 0.006886202695446320346713323775 ; 0.006761633300173798780927861108 ;
 0.006636049593781065044590038355 ; 0.006509470415053660267809899951 ; 0.006381914752107880570375164275 ; 0.006253401739542401272063645975 ; 0.006123950655567932542389081187 ;
 0.005993580919115338221127696870 ; 0.005862312086922653060661598801 ; 0.005730163850601437177384417555 ; 0.0055971560336829100775514452572 ; 0.0054633085886443102775705318566 ;
 0.0053286415939159303170811114788 ; 0.0051931752508692809303287536296 ; 0.0050569298807868423875578160762 ; 0.0049199259218138656695587765655 ; 0.0047821839258926913729317340448 ;
 0.0046437245556800603139790923525 ; 0.0045045685814478970686417923159 ; 0.0043647368779680566815684200621 ; 0.0042242504213815362723565049060 ; 0.0040831302860526684085997759212 ;
 0.0039413976414088336277290349840 ; 0.0037990737487662579981170192082 ; 0.0036561799581425021693892413052 ; 0.0035127377050563073309710549844 ; 0.0033687685073155510120191062489 ;
 0.0032242939617941981570107134269 ; 0.0030793357411993375832053528316 ; 0.0029339155908297166460123254142 ; 0.0027880553253277068805747610763 ; 0.0026417768254274905641208292516 ;
 0.0024951020347037068508395354372 ; 0.0023480529563273120170064609087 ; 0.0022006516498399104996848834189 ; 0.0020529202279661431745487818492 ; 0.0019048808534997184044191411746 ;
 0.0017565557363307299936069145295 ; 0.0016079671307493272424499395690 ; 0.0014591373333107332010883864996 ; 0.0013100886819025044578316804271 ; 0.0011608435575677247239705981135 ;
 0.0010114243932084404526058128414 ; 0.0008618537014200890378140934163 ; 0.0007121541634733206669089891511 ; 0.00056234895403140980281523674759 ; 0.00041246325442617632843218583774 ;
 0.00026253494429644590628745756250 ; 0.00011278901782227217551253887725 |] ;;


let gauss_legendre_257_x = [| -0.999956390712330402472857 ; -0.999770232390338019056053 ; -0.999435348366365078441838 ; -0.998951714093223210129834 ;
 -0.998319392445383847808766 ; -0.997538475365520218731818 ; -0.996609078365487004512326 ; -0.995531339486830143483750 ; -0.994305419008553630362377 ;
 -0.992931499332908653172844 ; -0.991409784923101705201254 ; -0.989740502257507526030375 ; -0.987923899788618253106809 ; -0.985960247902290665366669 ;
 -0.983849838875444644048531 ; -0.981592986831381877693095 ; -0.979190027692327124191591 ; -0.976641319128992592610888 ; -0.973947240507062326750976 ;
 -0.971108192830542793021113 ; -0.968124598681952354372943 ; -0.964996902159337170373447 ; -0.961725568810109767190665 ; -0.958311085561711847074814 ;
 -0.954753960649106318830855 ; -0.951054723539105826691801 ; -0.947213924851546682950881 ; -0.943232136277318328151464 ; -0.939109950493259404355123 ;
 -0.934847981073932324370129 ; -0.930446862400288909805510 ; -0.925907249565240289235888 ; -0.921229818276144817520964 ; -0.916415264754228313295468 ;
 -0.911464305630951423630955 ; -0.906377677841339419411308 ; -0.901156138514290206476301 ; -0.895800464859876809085345 ; -0.890311454053661045810287 ;
 -0.884689923118035575018750 ; -0.878936708800611938658765 ; -0.873052667449672679799858 ; -0.867038674886706051812473 ; -0.860895626276042275514686 ;
 -0.854624435991610735314055 ; -0.848226037480837936478636 ; -0.841701383125706473284556 ; -0.835051444100995681967937 ; -0.828277210229725073186687 ;
 -0.821379689835822056081139 ; -0.814359909594035880004229 ; -0.807218914377120130552073 ; -0.799957767100306523636066 ; -0.792577548563093144962574 ;
 -0.785079357288370682385816 ; -0.777464309358910595129671 ; -0.769733538251239556788216 ; -0.761888194666924898264210 ; -0.753929446361296162339238 ;
 -0.745858477969628263337895 ; -0.737676490830812123299244 ; -0.729384702808539030149808 ; -0.720984348110025333531072 ; -0.712476677102304460118510 ;
 -0.703862956126113592426171 ; -0.695144467307402713168813 ; -0.686322508366494071200553 ; -0.677398392424920474813593 ; -0.668373447809971163711735 ;
 -0.659249017856974352220492 ; -0.650026460709345873208532 ; -0.640707149116433684724434 ; -0.631292470229188329449219 ; -0.621783825393689760680446 ;
 -0.612182629942561267650033 ; -0.602490312984301547488097 ; -0.592708317190566281032495 ; -0.582838098581430874902446 ; -0.572881126308666332759406 ;
 -0.562838882437060514424546 ; -0.552712861723817332466074 ; -0.542504571396066721967792 ; -0.532215530926518500400434 ; -0.521847271807293510797499 ;
 -0.511401337321965712746629 ; -0.500879282315849152005553 ; -0.490282672964564000798817 ; -0.479613086540916117008992 ; -0.468872111180124821505728 ;
 -0.458061345643433838720630 ; -0.447182399080140586238810 ; -0.436236890788079234603398 ; -0.425226449972593188682213 ; -0.414152715504032866791986 ;
 -0.403017335673814873281489 ; -0.391821967949078874408131 ; -0.380568278725978696070941 ; -0.369257943081644365255611 ; -0.357892644524852014873858 ;
 -0.346474074745438764010632 ; -0.335003933362499872399782 ; -0.323483927671405649204085 ; -0.311915772389675771851948 ; -0.300301189401748840754520 ;
 -0.288641907502685160168097 ; -0.276939662140840894253032 ; -0.265196195159551900488370 ; -0.253413254537865690008131 ; -0.241592594130360106108882 ;
 -0.229735973406087448117604 ; -0.217845157186682897983880 ; -0.205921915383676231351599 ; -0.193968022735045913454182 ; -0.181985258541054792946197 ;
 -0.169975406399406713716337 ; -0.157940253939763465806087 ; -0.145881592557661591770148 ; -0.133801217147868654144405 ; -0.121700925837218653121859 ;
 -0.109582519716966361063898 ; -0.097447802574700412082119 ; -0.085298580625855050603929 ; -0.073136662244860502573600 ; -0.060963857695971986730406 ;
 -0.048781978863817431238958 ; -0.036592838983704002816750 ; -0.024398252371723591403953 ; -0.012200034154697423345412 ; 0.0 ; 0.012200034154697423345412 ;
 0.024398252371723591403953 ; 0.036592838983704002816750 ; 0.048781978863817431238958 ; 0.060963857695971986730406 ; 0.073136662244860502573600 ;
 0.085298580625855050603929 ; 0.097447802574700412082119 ; 0.109582519716966361063898 ; 0.121700925837218653121859 ; 0.133801217147868654144405 ;
 0.145881592557661591770148 ; 0.157940253939763465806087 ; 0.169975406399406713716337 ; 0.181985258541054792946197 ; 0.193968022735045913454182 ;
 0.205921915383676231351599 ; 0.217845157186682897983880 ; 0.229735973406087448117604 ; 0.241592594130360106108882 ; 0.253413254537865690008131 ;
 0.265196195159551900488370 ; 0.276939662140840894253032 ; 0.288641907502685160168097 ; 0.300301189401748840754520 ; 0.311915772389675771851948 ;
 0.323483927671405649204085 ; 0.335003933362499872399782 ; 0.346474074745438764010632 ; 0.357892644524852014873858 ; 0.369257943081644365255611 ;
 0.380568278725978696070941 ; 0.391821967949078874408131 ; 0.403017335673814873281489 ; 0.414152715504032866791986 ; 0.425226449972593188682213 ;
 0.436236890788079234603398 ; 0.447182399080140586238810 ; 0.458061345643433838720630 ; 0.468872111180124821505728 ; 0.479613086540916117008992 ;
 0.490282672964564000798817 ; 0.500879282315849152005553 ; 0.511401337321965712746629 ; 0.521847271807293510797499 ; 0.532215530926518500400434 ;
 0.542504571396066721967792 ; 0.552712861723817332466074 ; 0.562838882437060514424546 ; 0.572881126308666332759406 ; 0.582838098581430874902446 ;
 0.592708317190566281032495 ; 0.602490312984301547488097 ; 0.612182629942561267650033 ; 0.621783825393689760680446 ; 0.631292470229188329449219 ;
 0.640707149116433684724434 ; 0.650026460709345873208532 ; 0.659249017856974352220492 ; 0.668373447809971163711735 ; 0.677398392424920474813593 ;
 0.686322508366494071200553 ; 0.695144467307402713168813 ; 0.703862956126113592426171 ; 0.712476677102304460118510 ; 0.720984348110025333531072 ;
 0.729384702808539030149808 ; 0.737676490830812123299244 ; 0.745858477969628263337895 ; 0.753929446361296162339238 ; 0.761888194666924898264210 ;
 0.769733538251239556788216 ; 0.777464309358910595129671 ; 0.785079357288370682385816 ; 0.792577548563093144962574 ; 0.799957767100306523636066 ;
 0.807218914377120130552073 ; 0.814359909594035880004229 ; 0.821379689835822056081139 ; 0.828277210229725073186687 ; 0.835051444100995681967937 ;
 0.841701383125706473284556 ; 0.848226037480837936478636 ; 0.854624435991610735314055 ; 0.860895626276042275514686 ; 0.867038674886706051812473 ;
 0.873052667449672679799858 ; 0.878936708800611938658765 ; 0.884689923118035575018750 ; 0.890311454053661045810287 ; 0.895800464859876809085345 ;
 0.901156138514290206476301 ; 0.906377677841339419411308 ; 0.911464305630951423630955 ; 0.916415264754228313295468 ; 0.921229818276144817520964 ;
 0.925907249565240289235888 ; 0.930446862400288909805510 ; 0.934847981073932324370129 ; 0.939109950493259404355123 ; 0.943232136277318328151464 ;
 0.947213924851546682950881 ; 0.951054723539105826691801 ; 0.954753960649106318830855 ; 0.958311085561711847074814 ; 0.961725568810109767190665 ;
 0.964996902159337170373447 ; 0.968124598681952354372943 ; 0.971108192830542793021113 ; 0.973947240507062326750976 ; 0.976641319128992592610888 ;
 0.979190027692327124191591 ; 0.981592986831381877693095 ; 0.983849838875444644048531 ; 0.985960247902290665366669 ; 0.987923899788618253106809 ;
 0.989740502257507526030375 ; 0.991409784923101705201254 ; 0.992931499332908653172844 ; 0.994305419008553630362377 ; 0.995531339486830143483750 ;
 0.996609078365487004512326 ; 0.997538475365520218731818 ; 0.998319392445383847808766 ; 0.998951714093223210129834 ; 0.999435348366365078441838 ;
 0.999770232390338019056053 ; 0.999956390712330402472857 |] ;;

let gauss_legendre_257_w = [| 0.00011191470145601756450862287886 ; 0.00026049995580176964436806680831 ; 0.00040926648283531339591138751432 ; 0.00055799120546880640169677292533 ;
 0.00070663671051592291949335494247 ; 0.00085517818446696565626595950963 ; 0.00100359280467969441299468763292 ; 0.0011518582377826677880963146741 ; 0.0012999523174235227389668643832 ;
 0.0014478529559255120065233994722 ; 0.0015955381166175133369701690235 ; 0.0017429858051468299509941139300 ; 0.0018901740676190104269878470891 ; 0.0020370809914723626741694800322 ;
 0.0021836847075455253317921866057 ; 0.0023299633927021828561308282641 ; 0.0024758952727301488651840215879 ; 0.0026214586253808109266552781372 ; 0.0027666317834818283552560256501 ;
 0.0029113931380877846359302447381 ; 0.0030557211416493711130936102459 ; 0.0031995943111899437356540290142 ; 0.0033429912314827618499065991316 ; 0.0034858905582247143702551557840 ;
 0.0036282710212037760873102463983 ; 0.0037701114274582873548537007645 ; 0.0039113906644266662571543468015 ; 0.0040520877030864825223229951262 ; 0.0041921816010820254766367595011 ;
 0.0043316515058396297504806208252 ; 0.0044704766576701092218388764046 ; 0.0046086363928577081326523656522 ; 0.0047461101467350184936945641585 ; 0.0048828774567433411142588306018 ;
 0.0050189179654779878773297516544 ; 0.0051542114237180378340642003713 ; 0.0052887376934400710240953933529 ; 0.0054224767508154127788846727083 ; 0.0055554086891904284012033890901 ;
 0.0056875137220494140577838938236 ; 0.0058187721859596348346566361185 ; 0.0059491645434980654366600347567 ; 0.0060786713861593931405204596709 ; 0.0062072734372448464599330978665 ;
 0.0063349515547314166407936938524 ; 0.0064616867341210426397202932350 ; 0.0065874601112693336961737372300 ; 0.0067122529651934070221351960200 ; 0.0068360467208584215286561508406 ;
 0.0069588229519423919043121805236 ; 0.0070805633835788707705149901066 ; 0.0072012498950770900730828552207 ; 0.0073208645226191563361371026044 ; 0.0074393894619338979090297315972 ;
 0.0075568070709469658838993300454 ; 0.0076730998724067939537782250476 ; 0.0077882505564860261212726654404 ; 0.0079022419833580248574070864277 ; 0.0080150571857480760504667455353 ;
 0.0081266793714589108764118189068 ; 0.0082370919258701685661946145361 ; 0.0083462784144114279413811886655 ; 0.0084542225850084395379670551258 ; 0.0085609083705021941391459209280 ;
 0.0086663198910404675908861979240 ; 0.0087704414564414858792445834744 ; 0.0088732575685293586050755892934 ; 0.0089747529234409331997949023068 ; 0.0090749124139037264846862498962 ;
 0.0091737211314845944854270065178 ; 0.0092711643688088057725325917169 ; 0.0093672276217491880067391857021 ; 0.0094618965915850218253881576301 ; 0.0095551571871303607110514249099 ;
 0.0096469955268314600363329731559 ; 0.0097373979408330030783691793250 ; 0.0098263509730128164423854701706 ; 0.0099138413829847720250916955489 ; 0.0099998561480695773850435626986 ;
 0.0100843824652331611676814627839 ; 0.0101674077529923650568895461852 ; 0.0102489196532876585918958554047 ; 0.0103289060333225980974485876288 ; 0.0104073549873697559257355517893 ;
 0.0104842548385428511997370260353 ; 0.0105595941405348182788823332058 ; 0.0106333616793215542382761147904 ; 0.0107055464748310917616231511294 ; 0.0107761377825779489945556541150 ;
 0.0108451250952624130885928632830 ; 0.0109124981443345193856719616965 ; 0.0109782469015224934483083029166 ; 0.0110423615803254284301924654946 ; 0.0111048326374699756056269264803 ;
 0.0111656507743308312328559850485 ; 0.0112248069383148083152535688671 ; 0.0112822923242082872447042603128 ; 0.0113380983754878447625379269120 ; 0.011392216785593866154247619654 ;
 0.011444639499166951104119199270 ; 0.011495358713246929174010288914 ; 0.011544366878434306436012137033 ; 0.011591656700013970380783131035 ; 0.011637221139040985841125311445 ;
 0.011681053413388320313049670635 ; 0.011723146998756342723302879656 ; 0.011763495629643945382264331878 ; 0.011802093300281144573421477037 ; 0.011838934265523020964443424791 ;
 0.011874013041704866779344562066 ; 0.011907324407458412445505183140 ; 0.011938863404489011222535627643 ; 0.011968625338313666131272065445 ; 0.011996605778959789329711050159 ;
 0.012022800561624589927558893338 ; 0.012047205787294992091420946532 ; 0.012069817823327991167612855626 ; 0.012090633303991361438266420912 ; 0.012109649130964635027950450318 ;
 0.012126862473800277391553601370 ; 0.012142270770344990738801546574 ; 0.012155871727121082685623083829 ; 0.012167663319667843366755737416 ; 0.012177643792842880196606249581 ;
 0.012185811661083365425569178819 ; 0.012192165708627157605870499188 ; 0.012196704989693764053654538465 ; 0.012199428828625117371582840212 ; 0.01220033681998614507777289232 ;
 0.012199428828625117371582840212 ; 0.012196704989693764053654538465 ; 0.012192165708627157605870499188 ; 0.012185811661083365425569178819 ; 0.012177643792842880196606249581 ;
 0.012167663319667843366755737416 ; 0.012155871727121082685623083829 ; 0.012142270770344990738801546574 ; 0.012126862473800277391553601370 ; 0.012109649130964635027950450318 ;
 0.012090633303991361438266420912 ; 0.012069817823327991167612855626 ; 0.012047205787294992091420946532 ; 0.012022800561624589927558893338 ; 0.011996605778959789329711050159 ;
 0.011968625338313666131272065445 ; 0.011938863404489011222535627643 ; 0.011907324407458412445505183140 ; 0.011874013041704866779344562066 ; 0.011838934265523020964443424791 ;
 0.011802093300281144573421477037 ; 0.011763495629643945382264331878 ; 0.011723146998756342723302879656 ; 0.011681053413388320313049670635 ; 0.011637221139040985841125311445 ;
 0.011591656700013970380783131035 ; 0.011544366878434306436012137033 ; 0.011495358713246929174010288914 ; 0.011444639499166951104119199270 ; 0.011392216785593866154247619654 ;
 0.0113380983754878447625379269120 ; 0.0112822923242082872447042603128 ; 0.0112248069383148083152535688671 ; 0.0111656507743308312328559850485 ; 0.0111048326374699756056269264803 ;
 0.0110423615803254284301924654946 ; 0.0109782469015224934483083029166 ; 0.0109124981443345193856719616965 ; 0.0108451250952624130885928632830 ; 0.0107761377825779489945556541150 ;
 0.0107055464748310917616231511294 ; 0.0106333616793215542382761147904 ; 0.0105595941405348182788823332058 ; 0.0104842548385428511997370260353 ; 0.0104073549873697559257355517893 ;
 0.0103289060333225980974485876288 ; 0.0102489196532876585918958554047 ; 0.0101674077529923650568895461852 ; 0.0100843824652331611676814627839 ; 0.0099998561480695773850435626986 ;
 0.0099138413829847720250916955489 ; 0.0098263509730128164423854701706 ; 0.0097373979408330030783691793250 ; 0.0096469955268314600363329731559 ; 0.0095551571871303607110514249099 ;
 0.0094618965915850218253881576301 ; 0.0093672276217491880067391857021 ; 0.0092711643688088057725325917169 ; 0.0091737211314845944854270065178 ; 0.0090749124139037264846862498962 ;
 0.0089747529234409331997949023068 ; 0.0088732575685293586050755892934 ; 0.0087704414564414858792445834744 ; 0.0086663198910404675908861979240 ; 0.0085609083705021941391459209280 ;
 0.0084542225850084395379670551258 ; 0.0083462784144114279413811886655 ; 0.0082370919258701685661946145361 ; 0.0081266793714589108764118189068 ; 0.0080150571857480760504667455353 ;
 0.0079022419833580248574070864277 ; 0.0077882505564860261212726654404 ; 0.0076730998724067939537782250476 ; 0.0075568070709469658838993300454 ; 0.0074393894619338979090297315972 ;
 0.0073208645226191563361371026044 ; 0.0072012498950770900730828552207 ; 0.0070805633835788707705149901066 ; 0.0069588229519423919043121805236 ; 0.0068360467208584215286561508406 ;
 0.0067122529651934070221351960200 ; 0.0065874601112693336961737372300 ; 0.0064616867341210426397202932350 ; 0.0063349515547314166407936938524 ; 0.0062072734372448464599330978665 ;
 0.0060786713861593931405204596709 ; 0.0059491645434980654366600347567 ; 0.0058187721859596348346566361185 ; 0.0056875137220494140577838938236 ; 0.0055554086891904284012033890901 ;
 0.0054224767508154127788846727083 ; 0.0052887376934400710240953933529 ; 0.0051542114237180378340642003713 ; 0.0050189179654779878773297516544 ; 0.0048828774567433411142588306018 ;
 0.0047461101467350184936945641585 ; 0.0046086363928577081326523656522 ; 0.0044704766576701092218388764046 ; 0.0043316515058396297504806208252 ; 0.0041921816010820254766367595011 ;
 0.0040520877030864825223229951262 ; 0.0039113906644266662571543468015 ; 0.0037701114274582873548537007645 ; 0.0036282710212037760873102463983 ; 0.0034858905582247143702551557840 ;
 0.0033429912314827618499065991316 ; 0.0031995943111899437356540290142 ; 0.0030557211416493711130936102459 ; 0.0029113931380877846359302447381 ; 0.0027666317834818283552560256501 ;
 0.0026214586253808109266552781372 ; 0.0024758952727301488651840215879 ; 0.0023299633927021828561308282641 ; 0.0021836847075455253317921866057 ; 0.0020370809914723626741694800322 ;
 0.0018901740676190104269878470891 ; 0.0017429858051468299509941139300 ; 0.0015955381166175133369701690235 ; 0.0014478529559255120065233994722 ; 0.0012999523174235227389668643832 ;
 0.0011518582377826677880963146741 ; 0.00100359280467969441299468763292 ; 0.00085517818446696565626595950963 ; 0.00070663671051592291949335494247 ; 0.00055799120546880640169677292533 ;
 0.00040926648283531339591138751432 ; 0.00026049995580176964436806680831 ; 0.00011191470145601756450862287886 |] ;;


let gauss_patterson_1_x = [| 0.0 |] ;;

let gauss_patterson_1_w = [| 2.0 |] ;;


let gauss_patterson_3_x = [| -0.77459666924148337704 ; 0.0 ; 0.77459666924148337704 |] ;;

let gauss_patterson_3_w = [| 0.555555555555555555556 ; 0.888888888888888888889 ; 0.555555555555555555556 |] ;;


let gauss_patterson_7_x = [| -0.96049126870802028342 ; -0.77459666924148337704 ; -0.43424374934680255800 ;
 0.0 ; 0.43424374934680255800 ; 0.77459666924148337704 ; 0.96049126870802028342 |] ;;

let gauss_patterson_7_w = [| 0.104656226026467265194 ; 0.268488089868333440729 ; 0.401397414775962222905 ;
 0.450916538658474142345 ; 0.401397414775962222905 ; 0.268488089868333440729 ; 0.104656226026467265194 |] ;;


let gauss_patterson_15_x = [| -0.99383196321275502221 ; -0.96049126870802028342 ; -0.88845923287225699889 ;
 -0.77459666924148337704 ; -0.62110294673722640294 ; -0.43424374934680255800 ; -0.22338668642896688163 ;
 0.0 ;  0.22338668642896688163 ;  0.43424374934680255800 ;  0.62110294673722640294 ;
 0.77459666924148337704 ;  0.88845923287225699889 ;  0.96049126870802028342 ;  0.99383196321275502221 |] ;;

let gauss_patterson_15_w = [| 0.0170017196299402603390 ; 0.0516032829970797396969 ;  0.0929271953151245376859 ;
 0.134415255243784220360 ; 0.171511909136391380787 ; 0.200628529376989021034 ; 0.219156858401587496404 ;
 0.225510499798206687386 ; 0.219156858401587496404 ; 0.200628529376989021034 ; 0.171511909136391380787 ; 
 0.134415255243784220360 ; 0.0929271953151245376859 ; 0.0516032829970797396969 ; 0.0170017196299402603390 |] ;; 


let gauss_patterson_31_x = [| -0.99909812496766759766 ; -0.99383196321275502221 ; -0.98153114955374010687 ;
 -0.96049126870802028342 ; -0.92965485742974005667 ; -0.88845923287225699889 ; -0.83672593816886873550 ;
 -0.77459666924148337704 ; -0.70249620649152707861 ; -0.62110294673722640294 ; -0.53131974364437562397 ;
 -0.43424374934680255800 ; -0.33113539325797683309 ; -0.22338668642896688163 ; -0.11248894313318662575 ; 
 0.0 ; 0.11248894313318662575 ; 0.22338668642896688163 ; 0.33113539325797683309 ; 0.43424374934680255800 ;
 0.53131974364437562397 ; 0.62110294673722640294 ; 0.70249620649152707861 ; 0.77459666924148337704 ;
 0.83672593816886873550 ; 0.88845923287225699889 ; 0.92965485742974005667 ; 0.96049126870802028342 ; 
 0.98153114955374010687 ; 0.99383196321275502221 ; 0.99909812496766759766 |] ;;

let gauss_patterson_31_w = [| 0.00254478079156187441540 ; 0.00843456573932110624631 ; 0.0164460498543878109338 ;
 0.0258075980961766535646 ; 0.0359571033071293220968 ; 0.0464628932617579865414 ; 0.0569795094941233574122 ;
 0.0672077542959907035404 ; 0.0768796204990035310427 ; 0.0857559200499903511542 ; 0.0936271099812644736167 ;
 0.100314278611795578771 ; 0.105669893580234809744 ; 0.109578421055924638237 ; 0.111956873020953456880 ;
 0.112755256720768691607 ; 0.111956873020953456880 ; 0.109578421055924638237 ; 0.105669893580234809744 ;
 0.100314278611795578771 ; 0.0936271099812644736167 ; 0.0857559200499903511542 ; 0.0768796204990035310427 ;
 0.0672077542959907035404 ; 0.0569795094941233574122 ; 0.0464628932617579865414 ; 0.0359571033071293220968 ;
 0.0258075980961766535646 ; 0.0164460498543878109338 ; 0.00843456573932110624631 ; 0.00254478079156187441540 |] ;;


let gauss_patterson_63_x = [| -0.99987288812035761194 ; -0.99909812496766759766 ; -0.99720625937222195908 ;
 -0.99383196321275502221 ; -0.98868475754742947994 ; -0.98153114955374010687 ; -0.97218287474858179658 ;
 -0.96049126870802028342 ; -0.94634285837340290515 ; -0.92965485742974005667 ; -0.91037115695700429250 ;
 -0.88845923287225699889 ; -0.86390793819369047715 ; -0.83672593816886873550 ; -0.80694053195021761186 ;
 -0.77459666924148337704 ; -0.73975604435269475868 ; -0.70249620649152707861 ; -0.66290966002478059546 ;
 -0.62110294673722640294 ; -0.57719571005204581484 ; -0.53131974364437562397 ; -0.48361802694584102756 ;
 -0.43424374934680255800 ; -0.38335932419873034692 ; -0.33113539325797683309 ; -0.27774982202182431507 ;
 -0.22338668642896688163 ; -0.16823525155220746498 ; -0.11248894313318662575 ; -0.056344313046592789972 ;
 0.0 ; 0.056344313046592789972 ; 0.11248894313318662575 ; 0.16823525155220746498 ; 0.22338668642896688163 ;
 0.27774982202182431507 ; 0.33113539325797683309 ; 0.38335932419873034692 ; 0.43424374934680255800 ;
 0.48361802694584102756 ; 0.53131974364437562397 ; 0.57719571005204581484 ; 0.62110294673722640294 ;
 0.66290966002478059546 ; 0.70249620649152707861 ; 0.73975604435269475868 ; 0.77459666924148337704 ;
 0.80694053195021761186 ; 0.83672593816886873550 ; 0.86390793819369047715 ; 0.88845923287225699889 ;
 0.91037115695700429250 ; 0.92965485742974005667 ; 0.94634285837340290515 ; 0.96049126870802028342 ;
 0.97218287474858179658 ; 0.98153114955374010687 ; 0.98868475754742947994 ; 0.99383196321275502221 ;
 0.99720625937222195908 ; 0.99909812496766759766 ; 0.99987288812035761194 |] ;;

let gauss_patterson_63_w = [| 0.000363221481845530659694 ; 0.00126515655623006801137 ; 0.00257904979468568827243 ;
 0.00421763044155885483908 ; 0.00611550682211724633968 ; 0.00822300795723592966926 ; 0.0104982469096213218983 ;
 0.0129038001003512656260 ; 0.0154067504665594978021 ; 0.0179785515681282703329 ; 0.0205942339159127111492 ;
 0.0232314466399102694433 ; 0.0258696793272147469108 ; 0.0284897547458335486125 ; 0.0310735511116879648799 ;
 0.0336038771482077305417 ; 0.0360644327807825726401 ; 0.0384398102494555320386 ; 0.0407155101169443189339 ;
 0.0428779600250077344929 ; 0.0449145316536321974143 ; 0.0468135549906280124026 ; 0.0485643304066731987159 ;
 0.0501571393058995374137 ; 0.0515832539520484587768 ; 0.0528349467901165198621 ; 0.0539054993352660639269 ;
 0.0547892105279628650322 ; 0.0554814043565593639878 ; 0.0559784365104763194076 ; 0.0562776998312543012726 ;
 0.0563776283603847173877 ; 0.0562776998312543012726 ; 0.0559784365104763194076 ; 0.0554814043565593639878 ;
 0.0547892105279628650322 ; 0.0539054993352660639269 ; 0.0528349467901165198621 ; 0.0515832539520484587768 ;
 0.0501571393058995374137 ; 0.0485643304066731987159 ; 0.0468135549906280124026 ; 0.0449145316536321974143 ;
 0.0428779600250077344929 ; 0.0407155101169443189339 ; 0.0384398102494555320386 ; 0.0360644327807825726401 ;
 0.0336038771482077305417 ; 0.0310735511116879648799 ; 0.0284897547458335486125 ; 0.0258696793272147469108 ;
 0.0232314466399102694433 ; 0.0205942339159127111492 ; 0.0179785515681282703329 ; 0.0154067504665594978021 ;
 0.0129038001003512656260 ; 0.0104982469096213218983 ; 0.00822300795723592966926 ; 0.00611550682211724633968 ;
 0.00421763044155885483908 ; 0.00257904979468568827243 ; 0.00126515655623006801137 ; 0.000363221481845530659694 |] ;;


let gauss_patterson_127_x = [| -0.99998243035489159858 ; -0.99987288812035761194 ; -0.99959879967191068325 ;
 -0.99909812496766759766 ; -0.99831663531840739253 ; -0.99720625937222195908 ; -0.99572410469840718851 ;
 -0.99383196321275502221 ; -0.99149572117810613240 ; -0.98868475754742947994 ; -0.98537149959852037111 ;
 -0.98153114955374010687 ; -0.97714151463970571416 ; -0.97218287474858179658 ; -0.96663785155841656709 ;
 -0.96049126870802028342 ; -0.95373000642576113641 ; -0.94634285837340290515 ; -0.93832039777959288365 ;
 -0.92965485742974005667 ; -0.92034002547001242073 ; -0.91037115695700429250 ; -0.89974489977694003664 ;
 -0.88845923287225699889 ; -0.87651341448470526974 ; -0.86390793819369047715 ; -0.85064449476835027976 ;
 -0.83672593816886873550 ; -0.82215625436498040737 ; -0.80694053195021761186 ; -0.79108493379984836143 ;
 -0.77459666924148337704 ; -0.75748396638051363793 ; -0.73975604435269475868 ; -0.72142308537009891548 ;
 -0.70249620649152707861 ; -0.68298743109107922809 ; -0.66290966002478059546 ; -0.64227664250975951377 ;
 -0.62110294673722640294 ; -0.59940393024224289297 ; -0.57719571005204581484 ; -0.55449513263193254887 ;
 -0.53131974364437562397 ; -0.50768775753371660215 ; -0.48361802694584102756 ; -0.45913001198983233287 ;
 -0.43424374934680255800 ; -0.40897982122988867241 ; -0.38335932419873034692 ; -0.35740383783153215238 ;
 -0.33113539325797683309 ; -0.30457644155671404334 ; -0.27774982202182431507 ; -0.25067873030348317661 ;
 -0.22338668642896688163 ; -0.19589750271110015392 ; -0.16823525155220746498 ; -0.14042423315256017459 ;
 -0.11248894313318662575 ; -0.084454040083710883710 ; -0.056344313046592789972 ; -0.028184648949745694339 ;
 0.0 ; 0.028184648949745694339 ; 0.056344313046592789972 ; 0.084454040083710883710 ; 0.11248894313318662575 ;
 0.14042423315256017459 ; 0.16823525155220746498 ; 0.19589750271110015392 ; 0.22338668642896688163 ;
 0.25067873030348317661 ; 0.27774982202182431507 ; 0.30457644155671404334 ; 0.33113539325797683309 ;
 0.35740383783153215238 ; 0.38335932419873034692 ; 0.40897982122988867241 ; 0.43424374934680255800 ;
 0.45913001198983233287 ; 0.48361802694584102756 ; 0.50768775753371660215 ; 0.53131974364437562397 ;
 0.55449513263193254887 ; 0.57719571005204581484 ; 0.59940393024224289297 ; 0.62110294673722640294 ;
 0.64227664250975951377 ; 0.66290966002478059546 ; 0.68298743109107922809 ; 0.70249620649152707861 ;
 0.72142308537009891548 ; 0.73975604435269475868 ; 0.75748396638051363793 ; 0.77459666924148337704 ;
 0.79108493379984836143 ; 0.80694053195021761186 ; 0.82215625436498040737 ; 0.83672593816886873550 ;
 0.85064449476835027976 ; 0.86390793819369047715 ; 0.87651341448470526974 ; 0.88845923287225699889 ;
 0.89974489977694003664 ; 0.91037115695700429250 ; 0.92034002547001242073 ; 0.92965485742974005667 ;
 0.93832039777959288365 ; 0.94634285837340290515 ; 0.95373000642576113641 ; 0.96049126870802028342 ;
 0.96663785155841656709 ; 0.97218287474858179658 ; 0.97714151463970571416 ; 0.98153114955374010687 ;
 0.98537149959852037111 ; 0.98868475754742947994 ; 0.99149572117810613240 ; 0.99383196321275502221 ;
 0.99572410469840718851 ; 0.99720625937222195908 ; 0.99831663531840739253 ; 0.99909812496766759766 ;
 0.99959879967191068325 ; 0.99987288812035761194 ; 0.99998243035489159858 |] ;;

let gauss_patterson_127_w = [| 0.0000505360952078625176247 ; 0.000180739564445388357820 ; 0.000377746646326984660274 ;
 0.000632607319362633544219 ; 0.000938369848542381500794 ; 0.00128952408261041739210 ; 0.00168114286542146990631 ;
 0.00210881524572663287933 ; 0.00256876494379402037313 ; 0.00305775341017553113613 ; 0.00357289278351729964938 ;
 0.00411150397865469304717 ; 0.00467105037211432174741 ; 0.00524912345480885912513 ; 0.00584344987583563950756 ;
 0.00645190005017573692280 ; 0.00707248999543355546805 ; 0.00770337523327974184817 ; 0.00834283875396815770558 ;
 0.00898927578406413572328 ; 0.00964117772970253669530 ; 0.0102971169579563555237 ; 0.0109557333878379016480 ;
 0.0116157233199551347270 ; 0.0122758305600827700870 ; 0.0129348396636073734547 ; 0.0135915710097655467896 ;
 0.0142448773729167743063 ; 0.0148936416648151820348 ; 0.0155367755558439824399 ; 0.0161732187295777199419 ;
 0.0168019385741038652709 ; 0.0174219301594641737472 ; 0.0180322163903912863201 ; 0.0186318482561387901863 ;
 0.0192199051247277660193 ; 0.0197954950480974994880 ; 0.0203577550584721594669 ; 0.0209058514458120238522 ;
 0.0214389800125038672465 ; 0.0219563663053178249393 ; 0.0224572658268160987071 ; 0.0229409642293877487608 ;
 0.0234067774953140062013 ; 0.0238540521060385400804 ; 0.0242821652033365993580 ; 0.0246905247444876769091 ;
 0.0250785696529497687068 ; 0.0254457699654647658126 ; 0.0257916269760242293884 ; 0.0261156733767060976805 ;
 0.0264174733950582599310 ; 0.0266966229274503599062 ; 0.0269527496676330319634 ; 0.0271855132296247918192 ;
 0.0273946052639814325161 ; 0.0275797495664818730349 ; 0.0277407021782796819939 ; 0.0278772514766137016085 ;
 0.0279892182552381597038 ; 0.0280764557938172466068 ; 0.0281388499156271506363 ; 0.0281763190330166021307 ;
 0.0281888141801923586938 ; 0.0281763190330166021307 ; 0.0281388499156271506363 ; 0.0280764557938172466068 ;
 0.0279892182552381597038 ; 0.0278772514766137016085 ; 0.0277407021782796819939 ; 0.0275797495664818730349 ;
 0.0273946052639814325161 ; 0.0271855132296247918192 ; 0.0269527496676330319634 ; 0.0266966229274503599062 ;
 0.0264174733950582599310 ; 0.0261156733767060976805 ; 0.0257916269760242293884 ; 0.0254457699654647658126 ;
 0.0250785696529497687068 ; 0.0246905247444876769091 ; 0.0242821652033365993580 ; 0.0238540521060385400804 ;
 0.0234067774953140062013 ; 0.0229409642293877487608 ; 0.0224572658268160987071 ; 0.0219563663053178249393 ;
 0.0214389800125038672465 ; 0.0209058514458120238522 ; 0.0203577550584721594669 ; 0.0197954950480974994880 ;
 0.0192199051247277660193 ; 0.0186318482561387901863 ; 0.0180322163903912863201 ; 0.0174219301594641737472 ;
 0.0168019385741038652709 ; 0.0161732187295777199419 ; 0.0155367755558439824399 ; 0.0148936416648151820348 ;
 0.0142448773729167743063 ; 0.0135915710097655467896 ; 0.0129348396636073734547 ; 0.0122758305600827700870 ;
 0.0116157233199551347270 ; 0.0109557333878379016480 ; 0.0102971169579563555237 ; 0.00964117772970253669530 ;
 0.00898927578406413572328 ; 0.00834283875396815770558 ; 0.00770337523327974184817 ; 0.00707248999543355546805 ;
 0.00645190005017573692280 ; 0.00584344987583563950756 ; 0.00524912345480885912513 ; 0.00467105037211432174741 ;
 0.00411150397865469304717 ; 0.00357289278351729964938 ; 0.00305775341017553113613 ; 0.00256876494379402037313 ;
 0.00210881524572663287933 ; 0.00168114286542146990631 ; 0.00128952408261041739210 ; 0.000938369848542381500794 ;
 0.000632607319362633544219 ; 0.000377746646326984660274 ; 0.000180739564445388357820 ; 0.0000505360952078625176247 |] ;;


let gauss_patterson_255_x = [| -0.99999759637974846462 ; -0.99998243035489159858 ; -0.99994399620705437576 ; -0.99987288812035761194 ;
 -0.99976049092443204733 ; -0.99959879967191068325 ; -0.99938033802502358193 ; -0.99909812496766759766 ; -0.99874561446809511470 ;
 -0.99831663531840739253 ; -0.99780535449595727456 ; -0.99720625937222195908 ; -0.99651414591489027385 ; -0.99572410469840718851 ;
 -0.99483150280062100052 ; -0.99383196321275502221 ; -0.99272134428278861533 ; -0.99149572117810613240 ; -0.99015137040077015918 ;
 -0.98868475754742947994 ; -0.98709252795403406719 ; -0.98537149959852037111 ; -0.98351865757863272876 ; -0.98153114955374010687 ;
 -0.97940628167086268381 ; -0.97714151463970571416 ; -0.97473445975240266776 ; -0.97218287474858179658 ; -0.96948465950245923177 ;
 -0.96663785155841656709 ; -0.96364062156981213252 ; -0.96049126870802028342 ; -0.95718821610986096274 ; -0.95373000642576113641 ;
 -0.95011529752129487656 ; -0.94634285837340290515 ; -0.94241156519108305981 ; -0.93832039777959288365 ; -0.93406843615772578800 ;
 -0.92965485742974005667 ; -0.92507893290707565236 ; -0.92034002547001242073 ; -0.91543758715576504064 ; -0.91037115695700429250 ;
 -0.90514035881326159519 ; -0.89974489977694003664 ; -0.89418456833555902286 ; -0.88845923287225699889 ; -0.88256884024734190684 ;
 -0.87651341448470526974 ; -0.87029305554811390585 ; -0.86390793819369047715 ; -0.85735831088623215653 ; -0.85064449476835027976 ;
 -0.84376688267270860104 ; -0.83672593816886873550 ; -0.82952219463740140018 ; -0.82215625436498040737 ; -0.81462878765513741344 ;
 -0.80694053195021761186 ; -0.79909229096084140180 ; -0.79108493379984836143 ; -0.78291939411828301639 ; -0.77459666924148337704 ;
 -0.76611781930376009072 ; -0.75748396638051363793 ; -0.74869629361693660282 ; -0.73975604435269475868 ; -0.73066452124218126133 ;
 -0.72142308537009891548 ; -0.71203315536225203459 ; -0.70249620649152707861 ; -0.69281376977911470289 ; -0.68298743109107922809 ;
 -0.67301883023041847920 ; -0.66290966002478059546 ; -0.65266166541001749610 ; -0.64227664250975951377 ; -0.63175643771119423041 ;
 -0.62110294673722640294 ; -0.61031811371518640016 ; -0.59940393024224289297 ; -0.58836243444766254143 ; -0.57719571005204581484 ;
 -0.56590588542365442262 ; -0.55449513263193254887 ; -0.54296566649831149049 ; -0.53131974364437562397 ; -0.51955966153745702199 ;
 -0.50768775753371660215 ; -0.49570640791876146017 ; -0.48361802694584102756 ; -0.47142506587165887693 ; -0.45913001198983233287 ;
 -0.44673538766202847374 ; -0.43424374934680255800 ; -0.42165768662616330006 ; -0.40897982122988867241 ; -0.39621280605761593918 ;
 -0.38335932419873034692 ; -0.37042208795007823014 ; -0.35740383783153215238 ; -0.34430734159943802278 ; -0.33113539325797683309 ;
 -0.31789081206847668318 ; -0.30457644155671404334 ; -0.29119514851824668196 ; -0.27774982202182431507 ; -0.26424337241092676194 ;
 -0.25067873030348317661 ; -0.23705884558982972721 ; -0.22338668642896688163 ; -0.20966523824318119477 ; -0.19589750271110015392 ;
 -0.18208649675925219825 ; -0.16823525155220746498 ; -0.15434681148137810869 ; -0.14042423315256017459 ; -0.12647058437230196685 ;
 -0.11248894313318662575 ; -0.098482396598119202090 ; -0.084454040083710883710 ; -0.070406976042855179063 ; -0.056344313046592789972 ;
 -0.042269164765363603212 ; -0.028184648949745694339 ; -0.014093886410782462614 ;  0.0 ;  0.014093886410782462614 ;  0.028184648949745694339 ;
 0.042269164765363603212 ;  0.056344313046592789972 ;  0.070406976042855179063 ;  0.084454040083710883710 ;  0.098482396598119202090 ; 
 0.11248894313318662575 ;  0.12647058437230196685 ;  0.14042423315256017459 ;  0.15434681148137810869 ;  0.16823525155220746498 ;
 0.18208649675925219825 ;  0.19589750271110015392 ;  0.20966523824318119477 ;  0.22338668642896688163 ;  0.23705884558982972721 ;
 0.25067873030348317661 ;  0.26424337241092676194 ;  0.27774982202182431507 ;  0.29119514851824668196 ;  0.30457644155671404334 ;
 0.31789081206847668318 ;  0.33113539325797683309 ;  0.34430734159943802278 ;  0.35740383783153215238 ;  0.37042208795007823014 ;
 0.38335932419873034692 ;  0.39621280605761593918 ;  0.40897982122988867241 ;  0.42165768662616330006 ;  0.43424374934680255800 ;
 0.44673538766202847374 ;  0.45913001198983233287 ;  0.47142506587165887693 ;  0.48361802694584102756 ;  0.49570640791876146017 ;
 0.50768775753371660215 ;  0.51955966153745702199 ;  0.53131974364437562397 ;  0.54296566649831149049 ;  0.55449513263193254887 ;
 0.56590588542365442262 ;  0.57719571005204581484 ;  0.58836243444766254143 ;  0.59940393024224289297 ;  0.61031811371518640016 ;
 0.62110294673722640294 ;  0.63175643771119423041 ;  0.64227664250975951377 ;  0.65266166541001749610 ;  0.66290966002478059546 ;
 0.67301883023041847920 ;  0.68298743109107922809 ;  0.69281376977911470289 ;  0.70249620649152707861 ;  0.71203315536225203459 ;
 0.72142308537009891548 ;  0.73066452124218126133 ;  0.73975604435269475868 ;  0.74869629361693660282 ;  0.75748396638051363793 ;
 0.76611781930376009072 ;  0.77459666924148337704 ;  0.78291939411828301639 ;  0.79108493379984836143 ;  0.79909229096084140180 ;
 0.80694053195021761186 ;  0.81462878765513741344 ;  0.82215625436498040737 ;  0.82952219463740140018 ;  0.83672593816886873550 ;
 0.84376688267270860104 ;  0.85064449476835027976 ;  0.85735831088623215653 ;  0.86390793819369047715 ;  0.87029305554811390585 ;
 0.87651341448470526974 ;  0.88256884024734190684 ;  0.88845923287225699889 ;  0.89418456833555902286 ;  0.89974489977694003664 ;
 0.90514035881326159519 ;  0.91037115695700429250 ;  0.91543758715576504064 ;  0.92034002547001242073 ;  0.92507893290707565236 ;
 0.92965485742974005667 ;  0.93406843615772578800 ;  0.93832039777959288365 ;  0.94241156519108305981 ;  0.94634285837340290515 ;
 0.95011529752129487656 ;  0.95373000642576113641 ;  0.95718821610986096274 ;  0.96049126870802028342 ;  0.96364062156981213252 ;
 0.96663785155841656709 ;  0.96948465950245923177 ;  0.97218287474858179658 ;  0.97473445975240266776 ;  0.97714151463970571416 ;
 0.97940628167086268381 ;  0.98153114955374010687 ;  0.98351865757863272876 ;  0.98537149959852037111 ;  0.98709252795403406719 ;
 0.98868475754742947994 ;  0.99015137040077015918 ;  0.99149572117810613240 ;  0.99272134428278861533 ;  0.99383196321275502221 ;
 0.99483150280062100052 ;  0.99572410469840718851 ;  0.99651414591489027385 ;  0.99720625937222195908 ;  0.99780535449595727456 ;
 0.99831663531840739253 ;  0.99874561446809511470 ;  0.99909812496766759766 ;  0.99938033802502358193 ;  0.99959879967191068325 ;
 0.99976049092443204733 ; 0.99987288812035761194 ; 0.99994399620705437576 ; 0.99998243035489159858 ; 0.99999759637974846462 |] ;;


let gauss_patterson_255_w = [| 0.69379364324108267170e-5 ; 0.25157870384280661489e-4 ; 0.53275293669780613125e-4 ; 0.90372734658751149261e-4 ; 0.13575491094922871973e-3
 ; 0.18887326450650491366e-3 ; 0.24921240048299729402e-3 ; 0.31630366082226447689e-3 ; 0.38974528447328229322e-3 ; 0.46918492424785040975e-3
 ; 0.55429531493037471492e-3 ; 0.64476204130572477933e-3 ; 0.74028280424450333046e-3 ; 0.84057143271072246365e-3 ; 0.94536151685852538246e-3
 ; 0.10544076228633167722e-2 ; 0.11674841174299594077e-2 ; 0.12843824718970101768e-2 ; 0.14049079956551446427e-2 ; 0.15288767050877655684e-2
 ; 0.16561127281544526052e-2 ; 0.17864463917586498247e-2 ; 0.19197129710138724125e-2 ; 0.20557519893273465236e-2 ; 0.21944069253638388388e-2
 ; 0.23355251860571608737e-2 ; 0.24789582266575679307e-2 ; 0.26245617274044295626e-2 ; 0.27721957645934509940e-2 ; 0.29217249379178197538e-2
 ; 0.30730184347025783234e-2 ; 0.32259500250878684614e-2 ; 0.33803979910869203823e-2 ; 0.35362449977167777340e-2 ; 0.36933779170256508183e-2
 ; 0.38516876166398709241e-2 ; 0.40110687240750233989e-2 ; 0.41714193769840788528e-2 ; 0.43326409680929828545e-2 ; 0.44946378920320678616e-2
 ; 0.46573172997568547773e-2 ; 0.48205888648512683476e-2 ; 0.49843645647655386012e-2 ; 0.51485584789781777618e-2 ; 0.53130866051870565663e-2
 ; 0.54778666939189508240e-2 ; 0.56428181013844441585e-2 ; 0.58078616599775673635e-2 ; 0.59729195655081658049e-2 ; 0.61379152800413850435e-2
 ; 0.63027734490857587172e-2 ; 0.64674198318036867274e-2 ; 0.66317812429018878941e-2 ; 0.67957855048827733948e-2 ; 0.69593614093904229394e-2
 ; 0.71224386864583871532e-2 ; 0.72849479805538070639e-2 ; 0.74468208324075910174e-2 ; 0.76079896657190565832e-2 ; 0.77683877779219912200e-2
 ; 0.79279493342948491103e-2 ; 0.80866093647888599710e-2 ; 0.82443037630328680306e-2 ; 0.84009692870519326354e-2 ; 0.85565435613076896192e-2
 ; 0.87109650797320868736e-2 ; 0.88641732094824942641e-2 ; 0.90161081951956431600e-2 ; 0.91667111635607884067e-2 ; 0.93159241280693950932e-2
 ; 0.94636899938300652943e-2 ; 0.96099525623638830097e-2 ; 0.97546565363174114611e-2 ; 0.98977475240487497440e-2 ; 0.10039172044056840798e-1
 ; 0.10178877529236079733e-1 ; 0.10316812330947621682e-1 ; 0.10452925722906011926e-1 ; 0.10587167904885197931e-1 ; 0.10719490006251933623e-1
 ; 0.10849844089337314099e-1 ; 0.10978183152658912470e-1 ; 0.11104461134006926537e-1 ; 0.11228632913408049354e-1 ; 0.11350654315980596602e-1
 ; 0.11470482114693874380e-1 ; 0.11588074033043952568e-1 ; 0.11703388747657003101e-1 ; 0.11816385890830235763e-1 ; 0.11927026053019270040e-1
 ; 0.12035270785279562630e-1 ; 0.12141082601668299679e-1 ; 0.12244424981611985899e-1 ; 0.12345262372243838455e-1 ; 0.12443560190714035263e-1
 ; 0.12539284826474884353e-1 ; 0.12632403643542078765e-1 ; 0.12722884982732382906e-1 ; 0.12810698163877361967e-1 ; 0.12895813488012114694e-1
 ; 0.12978202239537399286e-1 ; 0.13057836688353048840e-1 ; 0.13134690091960152836e-1 ; 0.13208736697529129966e-1 ; 0.13279951743930530650e-1
 ; 0.13348311463725179953e-1 ; 0.13413793085110098513e-1 ; 0.13476374833816515982e-1 ; 0.13536035934956213614e-1 ; 0.13592756614812395910e-1
 ; 0.13646518102571291428e-1 ; 0.13697302631990716258e-1 ; 0.13745093443001896632e-1 ; 0.13789874783240936517e-1 ; 0.13831631909506428676e-1
 ; 0.13870351089139840997e-1 ; 0.13906019601325461264e-1 ; 0.13938625738306850804e-1 ; 0.13968158806516938516e-1 ; 0.13994609127619079852e-1
 ; 0.14017968039456608810e-1 ; 0.14038227896908623303e-1 ; 0.14055382072649964277e-1 ; 0.14069424957813575318e-1 ; 0.14080351962553661325e-1
 ; 0.14088159516508301065e-1 ; 0.14092845069160408355e-1 ; 0.14094407090096179347e-1 ; 0.14092845069160408355e-1 ; 0.14088159516508301065e-1
 ; 0.14080351962553661325e-1 ; 0.14069424957813575318e-1 ; 0.14055382072649964277e-1 ; 0.14038227896908623303e-1 ; 0.14017968039456608810e-1
 ; 0.13994609127619079852e-1 ; 0.13968158806516938516e-1 ; 0.13938625738306850804e-1 ; 0.13906019601325461264e-1 ; 0.13870351089139840997e-1
 ; 0.13831631909506428676e-1 ; 0.13789874783240936517e-1 ; 0.13745093443001896632e-1 ; 0.13697302631990716258e-1 ; 0.13646518102571291428e-1
 ; 0.13592756614812395910e-1 ; 0.13536035934956213614e-1 ; 0.13476374833816515982e-1 ; 0.13413793085110098513e-1 ; 0.13348311463725179953e-1
 ; 0.13279951743930530650e-1 ; 0.13208736697529129966e-1 ; 0.13134690091960152836e-1 ; 0.13057836688353048840e-1 ; 0.12978202239537399286e-1
 ; 0.12895813488012114694e-1 ; 0.12810698163877361967e-1 ; 0.12722884982732382906e-1 ; 0.12632403643542078765e-1 ; 0.12539284826474884353e-1
 ; 0.12443560190714035263e-1 ; 0.12345262372243838455e-1 ; 0.12244424981611985899e-1 ; 0.12141082601668299679e-1 ; 0.12035270785279562630e-1
 ; 0.11927026053019270040e-1 ; 0.11816385890830235763e-1 ; 0.11703388747657003101e-1 ; 0.11588074033043952568e-1 ; 0.11470482114693874380e-1
 ; 0.11350654315980596602e-1 ; 0.11228632913408049354e-1 ; 0.11104461134006926537e-1 ; 0.10978183152658912470e-1 ; 0.10849844089337314099e-1
 ; 0.10719490006251933623e-1 ; 0.10587167904885197931e-1 ; 0.10452925722906011926e-1 ; 0.10316812330947621682e-1 ; 0.10178877529236079733e-1
 ; 0.10039172044056840798e-1 ; 0.98977475240487497440e-2 ; 0.97546565363174114611e-2 ; 0.96099525623638830097e-2 ; 0.94636899938300652943e-2
 ; 0.93159241280693950932e-2 ; 0.91667111635607884067e-2 ; 0.90161081951956431600e-2 ; 0.88641732094824942641e-2 ; 0.87109650797320868736e-2
 ; 0.85565435613076896192e-2 ; 0.84009692870519326354e-2 ; 0.82443037630328680306e-2 ; 0.80866093647888599710e-2 ; 0.79279493342948491103e-2
 ; 0.77683877779219912200e-2 ; 0.76079896657190565832e-2 ; 0.74468208324075910174e-2 ; 0.72849479805538070639e-2 ; 0.71224386864583871532e-2
 ; 0.69593614093904229394e-2 ; 0.67957855048827733948e-2 ; 0.66317812429018878941e-2 ; 0.64674198318036867274e-2 ; 0.63027734490857587172e-2
 ; 0.61379152800413850435e-2 ; 0.59729195655081658049e-2 ; 0.58078616599775673635e-2 ; 0.56428181013844441585e-2 ; 0.54778666939189508240e-2
 ; 0.53130866051870565663e-2 ; 0.51485584789781777618e-2 ; 0.49843645647655386012e-2 ; 0.48205888648512683476e-2 ; 0.46573172997568547773e-2
 ; 0.44946378920320678616e-2 ; 0.43326409680929828545e-2 ; 0.41714193769840788528e-2 ; 0.40110687240750233989e-2 ; 0.38516876166398709241e-2
 ; 0.36933779170256508183e-2 ; 0.35362449977167777340e-2 ; 0.33803979910869203823e-2 ; 0.32259500250878684614e-2 ; 0.30730184347025783234e-2
 ; 0.29217249379178197538e-2 ; 0.27721957645934509940e-2 ; 0.26245617274044295626e-2 ; 0.24789582266575679307e-2 ; 0.23355251860571608737e-2
 ; 0.21944069253638388388e-2 ; 0.20557519893273465236e-2 ; 0.19197129710138724125e-2 ; 0.17864463917586498247e-2 ; 0.16561127281544526052e-2
 ; 0.15288767050877655684e-2 ; 0.14049079956551446427e-2 ; 0.12843824718970101768e-2 ; 0.11674841174299594077e-2 ; 0.10544076228633167722e-2
 ; 0.94536151685852538246e-3 ; 0.84057143271072246365e-3 ; 0.74028280424450333046e-3 ; 0.64476204130572477933e-3 ; 0.55429531493037471492e-3
 ; 0.46918492424785040975e-3 ; 0.38974528447328229322e-3 ; 0.31630366082226447689e-3 ; 0.24921240048299729402e-3 ; 0.18887326450650491366e-3
 ; 0.13575491094922871973e-3 ; 0.90372734658751149261e-4 ; 0.53275293669780613125e-4 ; 0.25157870384280661489e-4 ; 0.69379364324108267170e-5 |] ;;


let lobatto_2_x = [|  - 1.0 ; 1.0 |] ;;

let lobatto_2_w = [|  1.0 ; 1.0 |] ;;


let lobatto_3_x = [|  - 1.0 ; 0.0 ; 1.0 |] ;;

let lobatto_3_w = [|  1.0 /. 3.0 ; 4.0 /. 3.0 ; 1.0 /. 3.0 |] ;;


let lobatto_4_x = [|  - 1.0 ; - 0.447213595499957939281834733746 ; 0.447213595499957939281834733746 ; 1.0 |] ;;

let lobatto_4_w = [|  1.0 /. 6.0 ; 5.0 /. 6.0 ; 5.0 /. 6.0 ; 1.0 /. 6.0 |] ;;


let lobatto_5_x = [|  - 1.0 ; - 0.654653670707977143798292456247 ; 0.0 ; 0.654653670707977143798292456247 ; 1.0 |] ;;

let lobatto_5_w = [|  9.0 /. 90.0 ; 49.0 /. 90.0 ; 64.0 /. 90.0 ; 49.0 /. 90.0 ; 9.0 /. 90.0 |] ;;


let lobatto_6_x = [|  - 1.0 ; - 0.765055323929464692851002973959 ; - 0.285231516480645096314150994041 ;
 0.285231516480645096314150994041 ; 0.765055323929464692851002973959 ; 1.0 |] ;;

let lobatto_6_w = [|  0.066666666666666666666666666667 ; 0.378474956297846980316612808212 ; 0.554858377035486353016720525121 ;
 0.554858377035486353016720525121 ; 0.378474956297846980316612808212 ; 0.066666666666666666666666666667 |] ;;


let lobatto_7_x = [|  - 1.0 ; - 0.830223896278566929872032213967 ; - 0.468848793470714213803771881909 ;
 0.0 ; 0.468848793470714213803771881909 ; 0.830223896278566929872032213967 ; 1.0 |] ;;

let lobatto_7_w = [|  0.476190476190476190476190476190e-1 ; 0.276826047361565948010700406290 ; 0.431745381209862623417871022281 ;
 0.487619047619047619047619047619 ; 0.431745381209862623417871022281 ; 0.276826047361565948010700406290 ; 0.476190476190476190476190476190e-1 |] ;;


let lobatto_8_x = [|  - 1.0 ; - 0.871740148509606615337445761221 ; - 0.591700181433142302144510731398 ; - 0.209299217902478868768657260345 ;
 0.209299217902478868768657260345 ; 0.591700181433142302144510731398 ; 0.871740148509606615337445761221 ; 1.0 |] ;;

let lobatto_8_w = [|  0.357142857142857142857142857143e-1 ; 0.210704227143506039382991065776 ; 0.341122692483504364764240677108 ;
 0.412458794658703881567052971402 ; 0.412458794658703881567052971402 ; 0.341122692483504364764240677108 ; 0.210704227143506039382991065776 ;
 0.357142857142857142857142857143e-1 |] ;;


let lobatto_9_x = [|  - 1.0 ; - 0.899757995411460157312345244418 ; - 0.677186279510737753445885427091 ; - 0.363117463826178158710752068709 ; 0.0 ;
 0.363117463826178158710752068709 ; 0.677186279510737753445885427091 ; 0.899757995411460157312345244418 ; 1.0 |] ;;

let lobatto_9_w = [|  0.277777777777777777777777777778e-1 ; 0.165495361560805525046339720029 ; 0.274538712500161735280705618579 ;
 0.346428510973046345115131532140 ; 0.371519274376417233560090702948 ; 0.346428510973046345115131532140 ; 0.274538712500161735280705618579 ;
 0.165495361560805525046339720029 ; 0.277777777777777777777777777778e-1 |] ;;


let lobatto_9_x = [|  - 1.0 ; - 0.919533908166458813828932660822 ; - 0.738773865105505075003106174860 ; - 0.477924949810444495661175092731 ;
 - 0.165278957666387024626219765958 ; 0.165278957666387024626219765958 ; 0.477924949810444495661175092731 ; 0.738773865105505075003106174860 ;
 0.919533908166458813828932660822 ; 1.0 |] ;;

let lobatto_10_w = [|  0.222222222222222222222222222222e-1 ; 0.133305990851070111126227170755 ; 0.224889342063126452119457821731 ;
 0.292042683679683757875582257374 ; 0.327539761183897456656510527917 ; 0.327539761183897456656510527917 ; 0.292042683679683757875582257374 ;
 0.224889342063126452119457821731 ; 0.133305990851070111126227170755 ; 0.222222222222222222222222222222e-1 |] ;;


let lobatto_11_x = [|  - 1.0 ; - 0.934001430408059134332274136099 ; - 0.784483473663144418622417816108 ; - 0.565235326996205006470963969478 ;
 - 0.295758135586939391431911515559 ; 0.0 ; 0.295758135586939391431911515559 ; 0.565235326996205006470963969478 ;
 0.784483473663144418622417816108 ; 0.934001430408059134332274136099 ; 1.0 |] ;;

let lobatto_11_w = [|  0.181818181818181818181818181818e-1 ; 0.109612273266994864461403449580 ; 0.187169881780305204108141521899 ;
 0.248048104264028314040084866422 ; 0.286879124779008088679222403332 ; 0.300217595455690693785931881170 ; 0.286879124779008088679222403332 ;
 0.248048104264028314040084866422 ; 0.187169881780305204108141521899 ; 0.109612273266994864461403449580 ; 0.181818181818181818181818181818e-1 |] ;;


let lobatto_12_x = [|  - 1.0 ; - 0.944899272222882223407580138303 ; - 0.819279321644006678348641581717 ; - 0.632876153031869677662404854444 ;
 - 0.399530940965348932264349791567 ; - 0.136552932854927554864061855740 ; 0.136552932854927554864061855740 ; 0.399530940965348932264349791567 ;
 0.632876153031869677662404854444 ; 0.819279321644006678348641581717 ; 0.944899272222882223407580138303 ; 1.0 |] ;;

let lobatto_12_w = [|  0.151515151515151515151515151515e-1 ; 0.916845174131961306683425941341e-1 ; 0.157974705564370115164671062700 ;
 0.212508417761021145358302077367 ; 0.251275603199201280293244412148 ; 0.271405240910696177000288338500 ; 0.271405240910696177000288338500 ;
 0.251275603199201280293244412148 ; 0.212508417761021145358302077367 ; 0.157974705564370115164671062700 ; 0.916845174131961306683425941341e-1 ;
 0.151515151515151515151515151515e-1 |] ;;


let lobatto_13_x = [|  - 1.0 ; - 0.953309846642163911896905464755 ; - 0.846347564651872316865925607099 ; - 0.686188469081757426072759039566 ;
 - 0.482909821091336201746937233637 ; - 0.249286930106239992568673700374 ; 0.0 ; 0.249286930106239992568673700374 ; 0.482909821091336201746937233637 ;
 0.686188469081757426072759039566 ; 0.846347564651872316865925607099 ; 0.953309846642163911896905464755 ; 1.0 |] ;;

let lobatto_13_w = [|  0.128205128205128205128205128205e-1 ; 0.778016867468189277935889883331e-1 ; 0.134981926689608349119914762589 ;
 0.183646865203550092007494258747 ; 0.220767793566110086085534008379 ; 0.244015790306676356458578148360 ; 0.251930849333446736044138641541 ;
 0.244015790306676356458578148360 ; 0.220767793566110086085534008379 ; 0.183646865203550092007494258747 ; 0.134981926689608349119914762589 ;
 0.778016867468189277935889883331e-1 ; 0.128205128205128205128205128205e-1 |] ;;


let lobatto_14_x = [|  - 1.0 ; - 0.959935045267260901355100162015 ; - 0.867801053830347251000220202908 ; - 0.728868599091326140584672400521 ;
 - 0.550639402928647055316622705859 ; - 0.342724013342712845043903403642 ; - 0.116331868883703867658776709736 ; 0.116331868883703867658776709736 ;
 0.342724013342712845043903403642 ; 0.550639402928647055316622705859 ; 0.728868599091326140584672400521 ; 0.867801053830347251000220202908 ;
 0.959935045267260901355100162015 ; 1.0 |] ;;

let lobatto_14_w = [|  0.109890109890109890109890109890e-1 ; 0.668372844976812846340706607461e-1 ; 0.116586655898711651540996670655 ;
 0.160021851762952142412820997988 ; 0.194826149373416118640331778376 ; 0.219126253009770754871162523954 ; 0.231612794468457058889628357293 ;
 0.231612794468457058889628357293 ; 0.219126253009770754871162523954 ; 0.194826149373416118640331778376 ; 0.160021851762952142412820997988 ;
 0.116586655898711651540996670655 ; 0.668372844976812846340706607461e-1 ; 0.109890109890109890109890109890e-1 |] ;;


let lobatto_15_x = [|  - 1.0 ; - 0.965245926503838572795851392070 ; - 0.885082044222976298825401631482 ; - 0.763519689951815200704118475976 ;
 - 0.606253205469845711123529938637 ; - 0.420638054713672480921896938739 ; - 0.215353955363794238225679446273 ; 0.0 ;
 0.215353955363794238225679446273 ; 0.420638054713672480921896938739 ; 0.606253205469845711123529938637 ; 0.763519689951815200704118475976 ;
 0.885082044222976298825401631482 ; 0.965245926503838572795851392070 ; 1.0 |] ;;

let lobatto_15_w = [|  0.952380952380952380952380952381e-2 ; 0.580298930286012490968805840253e-1 ; 0.101660070325718067603666170789 ;
 0.140511699802428109460446805644 ; 0.172789647253600949052077099408 ; 0.196987235964613356092500346507 ; 0.211973585926820920127430076977 ;
 0.217048116348815649514950214251 ; 0.211973585926820920127430076977 ; 0.196987235964613356092500346507 ; 0.172789647253600949052077099408 ;
 0.140511699802428109460446805644 ; 0.101660070325718067603666170789 ; 0.580298930286012490968805840253e-1 ; 0.952380952380952380952380952381e-2 |] ;;


let lobatto_16_x = [|  - 1.0 ; - 0.969568046270217932952242738367 ; - 0.899200533093472092994628261520 ; - 0.792008291861815063931088270963 ;
 - 0.652388702882493089467883219641 ; - 0.486059421887137611781890785847 ; - 0.299830468900763208098353454722 ; - 0.101326273521949447843033005046 ;
 0.101326273521949447843033005046 ; 0.299830468900763208098353454722 ; 0.486059421887137611781890785847 ; 0.652388702882493089467883219641 ;
 0.792008291861815063931088270963 ; 0.899200533093472092994628261520 ; 0.969568046270217932952242738367 ; 1.0 |] ;;

let lobatto_16_w = [|  0.833333333333333333333333333333e-2 ; 0.508503610059199054032449195655e-1 ; 0.893936973259308009910520801661e-1 ;
 0.124255382132514098349536332657 ; 0.154026980807164280815644940485 ; 0.177491913391704125301075669528 ; 0.193690023825203584316913598854 ;
 0.201958308178229871489199125411 ; 0.201958308178229871489199125411 ; 0.193690023825203584316913598854 ; 0.177491913391704125301075669528 ;
 0.154026980807164280815644940485 ; 0.124255382132514098349536332657 ; 0.893936973259308009910520801661e-1 ; 0.508503610059199054032449195655e-1 ;
 0.833333333333333333333333333333e-2 |] ;;


let lobatto_17_x = [|  - 1.0 ; - 0.973132176631418314156979501874 ; - 0.910879995915573595623802506398 ; - 0.815696251221770307106750553238 ;
 - 0.691028980627684705394919357372 ; - 0.541385399330101539123733407504 ; - 0.372174433565477041907234680735 ; - 0.189511973518317388304263014753 ;
 0.0 ; 0.189511973518317388304263014753 ; 0.372174433565477041907234680735 ; 0.541385399330101539123733407504 ; 0.691028980627684705394919357372 ;
 0.815696251221770307106750553238 ; 0.910879995915573595623802506398 ; 0.973132176631418314156979501874 ; 1.0 |] ;;

let lobatto_17_w = [|  0.735294117647058823529411764706e-2 ; 0.449219405432542096474009546232e-1 ; 0.791982705036871191902644299528e-1 ;
 0.110592909007028161375772705220 ; 0.137987746201926559056201574954 ; 0.160394661997621539516328365865 ; 0.177004253515657870436945745363 ;
 0.187216339677619235892088482861 ; 0.190661874753469433299407247028 ; 0.187216339677619235892088482861 ; 0.177004253515657870436945745363 ;
 0.160394661997621539516328365865 ; 0.137987746201926559056201574954 ; 0.110592909007028161375772705220 ; 0.791982705036871191902644299528e-1 ;
 0.449219405432542096474009546232e-1 ; 0.735294117647058823529411764706e-2 |] ;;


let lobatto_18_x = [|  - 1.0 ; - 0.976105557412198542864518924342 ; - 0.920649185347533873837854625431 ; - 0.835593535218090213713646362328 ;
 - 0.723679329283242681306210365302 ; - 0.588504834318661761173535893194 ; - 0.434415036912123975342287136741 ; - 0.266362652878280984167665332026 ;
 - 0.897490934846521110226450100886e-1 ; 0.897490934846521110226450100886e-1 ; 0.266362652878280984167665332026 ; 0.434415036912123975342287136741 ;
 0.588504834318661761173535893194 ; 0.723679329283242681306210365302 ; 0.835593535218090213713646362328 ; 0.920649185347533873837854625431 ;
 0.976105557412198542864518924342 ; 1.0 |] ;;

let lobatto_18_w = [|  0.653594771241830065359477124183e-2 ; 0.399706288109140661375991764101e-1 ; 0.706371668856336649992229601678e-1 ;
 0.990162717175028023944236053187e-1 ; 0.124210533132967100263396358897 ; 0.145411961573802267983003210494 ; 0.161939517237602489264326706700 ;
 0.173262109489456226010614403827 ; 0.179015863439703082293818806944 ; 0.179015863439703082293818806944 ; 0.173262109489456226010614403827 ;
 0.161939517237602489264326706700 ; 0.145411961573802267983003210494 ; 0.124210533132967100263396358897 ; 0.990162717175028023944236053187e-1 ;
 0.706371668856336649992229601678e-1 ; 0.399706288109140661375991764101e-1 ; 0.653594771241830065359477124183e-2 |] ;;


let lobatto_19_x = [|  - 1.0 ; - 0.978611766222080095152634063110 ; - 0.928901528152586243717940258797 ; - 0.852460577796646093085955970041 ;
 - 0.751494202552613014163637489634 ; - 0.628908137265220497766832306229 ; - 0.488229285680713502777909637625 ; - 0.333504847824498610298500103845 ;
 - 0.169186023409281571375154153445 ; 0.0 ; 0.169186023409281571375154153445 ; 0.333504847824498610298500103845 ; 0.488229285680713502777909637625 ;
 0.628908137265220497766832306229 ; 0.751494202552613014163637489634 ; 0.852460577796646093085955970041 ; 0.928901528152586243717940258797 ;
 0.978611766222080095152634063110 ; 1.0 |] ;;

let lobatto_19_w = [|  0.584795321637426900584795321637e-2 ; 0.357933651861764771154255690351e-1 ; 0.633818917626297368516956904183e-1 ;
 0.891317570992070844480087905562e-1 ; 0.112315341477305044070910015464 ; 0.132267280448750776926046733910 ; 0.148413942595938885009680643668 ;
 0.160290924044061241979910968184 ; 0.167556584527142867270137277740 ; 0.170001919284827234644672715617 ; 0.167556584527142867270137277740 ;
 0.160290924044061241979910968184 ; 0.148413942595938885009680643668 ; 0.132267280448750776926046733910 ; 0.112315341477305044070910015464 ;
 0.891317570992070844480087905562e-1 ; 0.633818917626297368516956904183e-1 ; 0.357933651861764771154255690351e-1 ; 0.584795321637426900584795321637e-2 |] ;;


let lobatto_20_x = [| - 1.0 ; - 0.980743704893914171925446438584 ; - 0.935934498812665435716181584931 ; - 0.866877978089950141309847214616 ;
 - 0.775368260952055870414317527595 ; - 0.663776402290311289846403322971 ; - 0.534992864031886261648135961829 ; - 0.392353183713909299386474703816 ;
 - 0.239551705922986495182401356927 ; - 0.805459372388218379759445181596e-1 ; 0.805459372388218379759445181596e-1 ; 0.239551705922986495182401356927 ;
 0.392353183713909299386474703816 ; 0.534992864031886261648135961829 ; 0.663776402290311289846403322971 ; 0.775368260952055870414317527595 ;
 0.866877978089950141309847214616 ; 0.935934498812665435716181584931 ; 0.980743704893914171925446438584 ; 1.0 |] ;;

let lobatto_20_w = [|  0.526315789473684210526315789474e-2 ; 0.322371231884889414916050281173e-1 ; 0.571818021275668260047536271732e-1 ;
 0.806317639961196031447768461137e-1 ; 0.101991499699450815683781205733 ; 0.120709227628674725099429705002 ; 0.136300482358724184489780792989 ;
 0.148361554070916825814713013734 ; 0.156580102647475487158169896794 ; 0.160743286387845749007726726449 ; 0.160743286387845749007726726449 ;
 0.156580102647475487158169896794 ; 0.148361554070916825814713013734 ; 0.136300482358724184489780792989 ; 0.120709227628674725099429705002 ;
 0.101991499699450815683781205733 ; 0.806317639961196031447768461137e-1 ; 0.571818021275668260047536271732e-1 ; 0.322371231884889414916050281173e-1 ;
 0.526315789473684210526315789474e-2 |] ;;


let newton_cotes_1_x = [| 0. |] ;;

let newton_cotes_1_w = [| 2. |] ;;


let newton_cotes_2_w = [| 1. ; 1. |] ;;

let newton_cotes_2_x = [| -1. ; 1. |] ;;


let newton_cotes_3_w = [| 1. /. 3. ; 4. /. 3. ; 1. /. 3. |] ;;

let newton_cotes_3_x = [| -1. ; 0. ; 1.0 |] ;;


let newton_cotes_4_w = [| 1. /. 4. ; 3. /. 4. ; 3. /. 4. ; 1. /. 4. |] ;;

let newton_cotes_4_x = [| -1. ; -0.33333333333333333333 ; 0.33333333333333333333 ; 1.0 |] ;;


let newton_cotes_5_w = [| 7. /. 45. ; 32. /. 45. ; 12. /. 45. ; 32. /. 45. ; 7. /. 45. |] ;;

let newton_cotes_5_x = [| -1. ; -0.5 ; 0. ; 0.5 ; 1.0 |] ;;


let newton_cotes_6_w = [| 19. /. 144. ; 75. /. 144. ; 50. /. 144. ; 50. /. 144. ; 75. /. 144. ; 19. /. 144. |] ;;

let newton_cotes_6_x = [| -1. ; -0.6 ; -0.2 ; 0.2 ; 0.6 ; 1.0 |] ;;


let newton_cotes_7_w = [| 41. /. 420. ; 216. /. 420. ; 27. /. 420. ; 272. /. 420. ; 27. /. 420. ; 216. /. 420. ; 41. /. 420. |] ;;

let newton_cotes_7_x = [| -1. ; -0.66666666666666666667 ; -0.33333333333333333333 ; 0. ; 0.33333333333333333333 ; 0.66666666666666666667 ; 1.0 |] ;;


let newton_cotes_8_w = [| 751. /. 8640. ; 3577. /. 8640. ; 1323. /. 8640. ; 2989. /. 8640. ; 2989. /. 8640. ;
 1323. /. 8640. ; 3577. /. 8640. ; 751. /. 8640. |] ;;

let newton_cotes_8_x = [| -1. ; -0.71428571428571428571 ; -0.42857142857142857143 ; -0.14285714285714285714 ;
 0.14285714285714285714 ; 0.42857142857142857143 ; 0.71428571428571428571 ; 1.0 |] ;;


let newton_cotes_9_w = [| 989. /. 14175. ; 5888. /. 14175. ; -928. /. 14175. ; 10496. /. 14175. ; -4540. /. 14175. ;
 10496. /. 14175. ; -928. /. 14175. ; 5888. /. 14175. ; 989. /. 14175. |] ;;

let newton_cotes_9_x = [| -1. ; -0.75 ; -0.5 ; -0.25 ; 0. ; 0.25 ; 0.5 ; 0.75 ; 1.0 |] ;;


let newton_cotes_10_w = [| 2857. /. 44800. ; 15741. /. 44800. ; 1080. /. 44800. ; 19344. /. 44800. ; 5778. /. 44800. ;
 5778. /. 44800. ; 19344. /. 44800. ; 1080. /. 44800. ; 15741. /. 44800. ; 2857. /. 44800. |] ;;

let newton_cotes_10_x = [| -1. ; -0.77777777777777777778 ; -0.55555555555555555556 ; -0.33333333333333333333 ; -0.11111111111111111111 ;
 0.11111111111111111111 ; 0.33333333333333333333 ; 0.55555555555555555556 ; 0.77777777777777777778 ; 1.0 |] ;;


let newton_cotes_11_w = [| 16067. /. 299376. ; 106300. /. 299376. ; - 48525. /. 299376. ; 272400. /. 299376. ; - 260550. /. 299376. ;
 427368. /. 299376. ; - 260550. /. 299376. ; 272400. /. 299376. ; - 48525. /. 299376. ; 106300. /. 299376. ; 16067. /. 299376. |] ;;

let newton_cotes_11_x = [| -1. ; -0.8 ; -0.6 ; -0.4 ; -0.2 ; 0. ; 0.2 ; 0.4 ; 0.6 ; 0.8 ; 1.0 |] ;;


let newton_cotes_12_w = [| 2171465. /. 43545600. ; 13486539. /. 43545600. ; - 3237113. /. 43545600. ; 25226685. /. 43545600. ;
 - 9595542. /. 43545600. ; 15493566. /. 43545600. ; 15493566. /. 43545600. ; - 9595542. /. 43545600. ; 25226685. /. 43545600. ;
 - 3237113. /. 43545600. ; 13486539. /. 43545600. ; 2171465. /. 43545600. |] ;;

let newton_cotes_12_x = [| -1. ; -0.81818181818181818182 ; -0.63636363636363636364 ; -0.45454545454545454545 ; -0.27272727272727272727 ;
 -0.090909090909090909091 ; 0.090909090909090909091 ; 0.27272727272727272727 ; 0.45454545454545454545 ; 0.63636363636363636364 ; 0.81818181818181818182 ; 1.0 |] ;;


let newton_cotes_13_w = [| 1364651. /. 31531500. ; 9903168. /. 31531500. ; - 7587864. /. 31531500. ; 35725120. /. 31531500. ;
 - 51491295. /. 31531500. ; 87516288. /. 31531500. ; - 87797136. /. 31531500. ; 87516288. /. 31531500. ; - 51491295. /. 31531500. ;
 35725120. /. 31531500. ; - 7587864. /. 31531500. ; 9903168. /. 31531500. ; 1364651. /. 31531500. |] ;;

let newton_cotes_13_x = [| -1. ; -0.83333333333333333333 ; -0.66666666666666666667 ; -0.5 ; -0.33333333333333333333 ; -0.16666666666666666667 ;
 0. ; 0.16666666666666666667 ; 0.33333333333333333333 ; 0.5 ; 0.66666666666666666667 ; 0.83333333333333333333 ; 1.0 |] ;;


let newton_cotes_14_w = [| 6137698213. /. 150885504000. ; 42194238652. /. 150885504000. ; - 23361540993. /. 150885504000. ;
 116778274403. /. 150885504000. ; - 113219777650. /. 150885504000. ; 154424590209. /. 150885504000. ; - 32067978834. /. 150885504000. ;
 - 32067978834. /. 150885504000. ; 154424590209. /. 150885504000. ; - 113219777650. /. 150885504000. ; 116778274403. /. 150885504000. ;
 - 23361540993. /. 150885504000. ; 42194238652. /. 150885504000. ; 6137698213. /. 150885504000. |] ;;

let newton_cotes_14_x = [| -1. ; -0.84615384615384615385 ; -0.69230769230769230769 ; -0.53846153846153846154 ; -0.38461538461538461538 ;
 -0.23076923076923076923 ; -0.076923076923076923077 ; 0.076923076923076923077 ; 0.23076923076923076923 ; 0.38461538461538461538 ;
 0.53846153846153846154 ; 0.69230769230769230769 ; 0.84615384615384615385 ; 1.0 |] ;;


let newton_cotes_15_w = [| 90241897. /. 2501928000. ; 710986864. /. 2501928000. ; - 770720657. /. 2501928000. ; 3501442784. /. 2501928000. ;
 - 6625093363. /. 2501928000. ; 12630121616. /. 2501928000. ; - 16802270373. /. 2501928000. ; 19534438464. /. 2501928000. ;
 - 16802270373. /. 2501928000. ; 12630121616. /. 2501928000. ; - 6625093363. /. 2501928000. ; 3501442784. /. 2501928000. ;
 - 770720657. /. 2501928000. ; 710986864. /. 2501928000. ; 90241897. /. 2501928000. |] ;;

let newton_cotes_15_x = [| -1. ; -0.85714285714285714286 ; -0.71428571428571428571 ; -0.57142857142857142857 ; -0.42857142857142857143 ;
 -0.28571428571428571429 ; -0.14285714285714285714 ; 0. ; 0.14285714285714285714 ; 0.28571428571428571429 ;
 0.42857142857142857143 ; 0.57142857142857142857 ; 0.71428571428571428571 ; 0.85714285714285714286 ; 1.0 |] ;;


let newton_cotes_16_w = [| 105930069. /. 3099672576. ; 796661595. /. 3099672576. ; - 698808195. /. 3099672576. ;
 3143332755. /. 3099672576. ; - 4688522055. /. 3099672576. ; 7385654007. /. 3099672576. ; - 6000998415. /. 3099672576. ;
 3056422815. /. 3099672576. ; 3056422815. /. 3099672576. ; - 6000998415. /. 3099672576. ; 7385654007. /. 3099672576. ;
 - 4688522055. /. 3099672576. ; 3143332755. /. 3099672576. ; - 698808195. /. 3099672576. ; 796661595. /. 3099672576. ; 105930069. /. 3099672576. |] ;;

let newton_cotes_16_x = [| -1. ; -0.86666666666666666667 ; -0.73333333333333333333 ; -0.6 ; -0.46666666666666666667 ; -0.33333333333333333333 ; -0.2 ;
 -0.066666666666666666667 ; 0.066666666666666666667 ; 0.2 ; 0.33333333333333333333 ; 0.46666666666666666667 ; 0.6 ; 0.73333333333333333333 ;
 0.86666666666666666667 ; 1.0 |] ;;


let newton_cotes_17_w = [| 15043611773. /. 488462349375. ; 127626606592. /. 488462349375. ; - 179731134720. /. 488462349375. ;
 832211855360. /. 488462349375. ; - 1929498607520. /. 488462349375. ; 4177588893696. /. 488462349375. ; - 6806534407936. /. 488462349375. ;
 9368875018240. /. 488462349375. ; - 10234238972220. /. 488462349375. ; 9368875018240. /. 488462349375. ; - 6806534407936. /. 488462349375. ;
 4177588893696. /. 488462349375. ; - 1929498607520. /. 488462349375. ; 832211855360. /. 488462349375. ;
 - 179731134720. /. 488462349375. ; 127626606592. /. 488462349375. ; 15043611773. /. 488462349375. |] ;;

let newton_cotes_17_x = [| -1. ; -0.875 ; -0.75 ; -0.625 ; -0.5 ; -0.375 ; -0.25 ; -0.125 ; 0. ; 0.125 ; 0.25 ; 0.375 ; 0.5 ; 0.625 ; 0.75 ; 0.875 ; 1.0 |] ;;


let newton_cotes_18_w = [| 55294720874657. /. 1883051089920000. ; 450185515446285. /. 1883051089920000. ; - 542023437008852. /. 1883051089920000. ; 2428636525764260. /. 1883051089920000. ;
 - 4768916800123440. /. 1883051089920000. ; 8855416648684984. /. 1883051089920000. ; - 10905371859796660. /. 1883051089920000. ; 10069615750132836. /. 1883051089920000. ;
 - 3759785974054070. /. 1883051089920000. ; - 3759785974054070. /. 1883051089920000. ; 10069615750132836. /. 1883051089920000. ; - 10905371859796660. /. 1883051089920000. ;
 8855416648684984. /. 1883051089920000. ; - 4768916800123440. /. 1883051089920000. ; 2428636525764260. /. 1883051089920000. ; - 542023437008852. /. 1883051089920000. ;
 450185515446285. /. 1883051089920000. ; 55294720874657. /. 1883051089920000. |] ;;

let newton_cotes_18_x = [| -1. ; -0.88235294117647058824 ; -0.76470588235294117647 ; -0.64705882352941176471 ; -0.52941176470588235294 ;
 -0.41176470588235294118 ; -0.29411764705882352941 ; -0.17647058823529411765 ; -0.058823529411764705882 ; 0.058823529411764705882 ;
 0.17647058823529411765 ; 0.29411764705882352941 ; 0.41176470588235294118 ; 0.52941176470588235294 ; 0.64705882352941176471 ;
 0.76470588235294117647 ; 0.88235294117647058824 ; 1.0 |] ;;


let newton_cotes_19_w = [| 203732352169. /. 7604556960000. ; 1848730221900. /. 7604556960000. ; - 3212744374395. /. 7604556960000. ; 15529830312096. /. 7604556960000. ;
 - 42368630685840. /. 7604556960000. ; 103680563465808. /. 7604556960000. ; - 198648429867720. /. 7604556960000. ; 319035784479840. /. 7604556960000. ;
 - 419127951114198. /. 7604556960000. ; 461327344340680. /. 7604556960000. ; - 419127951114198. /. 7604556960000. ; 319035784479840. /. 7604556960000. ;
 - 198648429867720. /. 7604556960000. ; 103680563465808. /. 7604556960000. ; - 42368630685840. /. 7604556960000. ; 15529830312096. /. 7604556960000. ;
 - 3212744374395. /. 7604556960000. ; 1848730221900. /. 7604556960000. ; 203732352169. /. 7604556960000. |] ;;

let newton_cotes_19_x = [| -1. ; -0.88888888888888888889 ; -0.77777777777777777778 ; -0.66666666666666666667 ; -0.55555555555555555556 ;
 -0.44444444444444444444 ; -0.33333333333333333333 ; -0.22222222222222222222 ; -0.11111111111111111111 ; 0. ; 0.11111111111111111111 ;
 0.22222222222222222222 ; 0.33333333333333333333 ; 0.44444444444444444444 ; 0.55555555555555555556 ; 0.66666666666666666667 ;
 0.77777777777777777778 ; 0.88888888888888888889 ; 1. |] ;;


let newton_cotes_20_w = [| 69028763155644023. /. 2688996956405760000. ; 603652082270808125. /. 2688996956405760000. ; - 926840515700222955. /. 2688996956405760000. ;
 4301581538450500095. /. 2688996956405760000. ; - 10343692234243192788. /. 2688996956405760000. ; 22336420328479961316. /. 2688996956405760000. ;
 - 35331888421114781580. /. 2688996956405760000. ; 43920768370565135580. /. 2688996956405760000. ; - 37088370261379851390. /. 2688996956405760000. ;
 15148337305921759574. /. 2688996956405760000. ; 15148337305921759574. /. 2688996956405760000. ; - 37088370261379851390. /. 2688996956405760000. ;
 43920768370565135580. /. 2688996956405760000. ; - 35331888421114781580. /. 2688996956405760000. ; 22336420328479961316. /. 2688996956405760000. ;
 - 10343692234243192788. /. 2688996956405760000. ; 4301581538450500095. /. 2688996956405760000. ; - 926840515700222955. /. 2688996956405760000. ;
 603652082270808125. /. 2688996956405760000. ; 69028763155644023. /. 2688996956405760000. |] ;;

let newton_cotes_20_x = [| -1. ; -0.89473684210526315789 ; -0.78947368421052631579 ; -0.68421052631578947368 ;
 -0.57894736842105263158 ; -0.47368421052631578947 ; -0.36842105263157894737 ; -0.26315789473684210526 ;
 -0.15789473684210526316 ; -0.052631578947368421053 ; 0.052631578947368421053 ; 0.15789473684210526316 ;
 0.26315789473684210526 ; 0.36842105263157894737 ; 0.47368421052631578947 ; 0.57894736842105263158 ;
 0.68421052631578947368 ; 0.78947368421052631579 ; 0.89473684210526315789 ; 1.0 |] ;;


let newton_cotes_21_x = [| -1. ; -0.9 ; -0.8 ; -0.7 ; -0.6 ; -0.5 ; -0.4 ; -0.3 ; -0.2 ; -0.1 ; 0. ; 0.1 ; 0.2 ; 0.3 ; 0.4 ; 0.5 ; 0.6 ; 0.7 ; 0.8 ; 0.9 ; 1. |] ;;

let newton_cotes_21_w = [| 0.023650546498063206389 ; 0.22827543528921394997 ; -0.47295674102285392846 ; 2.4123737869637513288 ;
 -7.5420634534306609355 ; 20.673596439879602287 ; -45.417631687959024596 ; 83.656114844387109207 ; -128.1505589803080093 ;
 165.59456694494570344 ; -180.01073427048578932 ; 165.59456694494570344 ; -128.1505589803080093 ; 83.656114844387109207 ;
 -45.417631687959024596 ; 20.673596439879602287 ; -7.5420634534306609355 ; 2.4123737869637513288 ; -0.47295674102285392846 ;
 0.22827543528921394997 ; 0.023650546498063206389 |] ;;


let radau_1_x = [|  - 1.0 |] ;;

let radau_1_w = [|  2.0 |] ;;


let radau_2_x = [| - 1.0 ;
 1.0 /. 3.0 |] ;;

let radau_2_w = [| 0.5 ;
 1.5 |] ;;


let radau_3_x = [|  - 1.0 ; - 0.289897948556635619639456814941 ; 0.689897948556635619639456814941 |] ;;

let radau_3_w = [| 0.222222222222222222222222222222 ; 1.02497165237684322767762689304 ; 0.752806125400934550100150884739 |] ;;


let radau_4_x = [| - 1.0 ; - 0.575318923521694112050483779752 ; 0.181066271118530578270147495862 ; 0.822824080974592105208907712461 |] ;;

let radau_4_w = [| 0.125 ; 0.657688639960119487888578442146 ; 0.776386937686343761560464613780 ; 0.440924422353536750550956944074 |] ;;


let radau_5_x = [| - 1.0 ; - 0.720480271312438895695825837750 ; - 0.167180864737833640113395337326 ; 0.446313972723752344639908004629 ; 0.885791607770964635613757614892 |] ;;

let radau_5_w = [| 0.08 ; 0.446207802167141488805120436457 ; 0.623653045951482508163709823153 ; 0.562712030298924120384345300681 ; 0.287427121582451882646824439708 |] ;;


let radau_6_x = [| - 1.0 ; - 0.802929828402347147753002204224 ; - 0.390928546707272189029229647442 ; 0.124050379505227711989974959990 ; 0.603973164252783654928415726409 ;
 0.920380285897062515318386619813 |] ;;

let radau_6_w = [| 0.555555555555555555555555555556e-1 ; 0.319640753220510966545779983796 ; 0.485387188468969916159827915587 ; 0.520926783189574982570229406570 ;
 0.416901334311907738959406382743 ; 0.201588385253480840209200755749 |] ;;


let radau_7_x = [| - 1.0 ; - 0.853891342639482229703747931639 ; - 0.538467724060109001833766720231 ; - 0.117343037543100264162786683611 ; 0.326030619437691401805894055838 ;
 0.703842800663031416300046295008 ; 0.941367145680430216055899446174 |] ;;

let radau_7_w = [| 0.408163265306122448979591836735e-1 ; 0.239227489225312405787077480770 ; 0.380949873644231153805938347876 ; 0.447109829014566469499348953642 ;
 0.424703779005955608398308039150 ; 0.318204231467301481744870434470 ; 0.148988471112020635866497560418 |] ;;


let radau_8_x = [| - 1.0 ; - 0.887474878926155707068695617935 ; - 0.639518616526215270024840114382 ; - 0.294750565773660725252184459658 ; 0.943072526611107660028971153047e-1 ;
 0.468420354430821063046421216613 ; 0.770641893678191536180719525865 ; 0.955041227122575003782349000858 |] ;;

let radau_8_w = [| 0.03125 ; 0.185358154802979278540728972699 ; 0.304130620646785128975743291400 ; 0.376517545389118556572129261442 ; 0.391572167452493593082499534004 ;
 0.347014795634501280228675918422 ; 0.249647901329864963257869293513 ; 0.114508814744257199342353728520 |] ;;


let radau_9_x = [| - 1.0 ; - 0.910732089420060298533757956283 ; - 0.711267485915708857029562959544 ; - 0.426350485711138962102627520502 ; - 0.903733696068532980645444599064e-1 ;
 0.256135670833455395138292079035 ; 0.571383041208738483284917464837 ; 0.817352784200412087992517083851 ; 0.964440169705273096373589797925 |] ;;

let radau_9_w = [| 0.246913580246913580246913580247e-1 ; 0.147654019046315385819588499802 ; 0.247189378204593052361239794969 ; 0.316843775670437978338000849642 ;
 0.348273002772966594071991031186 ; 0.337693966975929585803724239792 ; 0.286386696357231171146705637752 ; 0.200553298024551957421165090417 ; 0.907145049232829170128934984159e-1 |] ;;


let radau_10_x = [| - 1.0 ; - 0.927484374233581078117671398464 ; - 0.763842042420002599615429776011 ; - 0.525646030370079229365386614293 ; - 0.236234469390588049278459503207 ;
 0.760591978379781302337137826389e-1 ; 0.380664840144724365880759065541 ; 0.647766687674009436273648507855 ; 0.851225220581607910728163628088 ; 0.971175180702246902734346518378 |] ;;

let radau_10_w = [| 0.02 ; 0.120296670557481631517310522702 ; 0.204270131879000675555788672223 ; 0.268194837841178696058554475262 ; 0.305859287724422621016275475401 ;
 0.313582457226938376695902847302 ; 0.290610164832918311146863077963 ; 0.239193431714379713376571966160 ; 0.164376012736921475701681668908 ; 0.736170054867584989310512940790e-1 |] ;;


let radau_11_x = [| - 1.0 ; - 0.939941935677027005913871284731 ; - 0.803421975580293540697597956820 ; - 0.601957842073797690275892603234 ; - 0.351888923353330214714301017870 ;
 - 0.734775314313212657461903554238e-1 ; 0.210720306228426314076095789845 ; 0.477680647983087519467896683890 ; 0.705777100713859519144801128840 ; 0.876535856245703748954741265611 ;
 0.976164773135168806180508826082 |] ;;

let radau_11_w = [| 0.165289256198347107438016528926e-1 ; 0.998460819079680638957534695802e-1 ; 0.171317619206659836486712649042 ; 0.228866123848976624401683231126 ;
 0.267867086189684177806638163355 ; 0.285165563941007337460004408915 ; 0.279361333103383045188962195720 ; 0.250925377697128394649140267633 ; 0.202163108540024418349931754266 ;
 0.137033682133202256310153880580 ; 0.609250978121311347072183268883e-1 |] ;;


let radau_12_x = [| - 1.0 ; - 0.949452759204959300493337627077 ; - 0.833916773105189706586269254036 ; - 0.661649799245637148061133087811 ; - 0.444406569781935851126642615609 ;
 - 0.196994559534278366455441427346 ; 0.637247738208319158337792384845e-1 ; 0.319983684170669623532789532206 ; 0.554318785912324288984337093085 ; 0.750761549711113852529400825472 ;
 0.895929097745638894832914608454 ; 0.979963439076639188313950540264 |] ;;

let radau_12_w = [| 0.138888888888888888888888888888e-1 ; 0.841721349386809762415796536813e-1 ; 0.145563668853995128522547654706 ; 0.196998534826089634656049637969 ;
 0.235003115144985839348633985940 ; 0.256991338152707776127974253598 ; 0.261465660552133103438074715743 ; 0.248121560804009959403073107079 ; 0.217868879026192438848747482023 ;
 0.172770639313308564306065766966 ; 0.115907480291738392750341908272 ; 0.512480992072692974680229451351e-1 |] ;;


let radau_13_x = [| - 1.0 ; - 0.956875873668299278183813833834 ; - 0.857884202528822035697620310269 ; - 0.709105087529871761580423832811 ; - 0.519197779050454107485205148087 ;
 - 0.299201300554509985532583446686 ; - 0.619016986256353412578604857936e-1 ; 0.178909837597084635021931298881 ; 0.409238231474839556754166331248 ; 0.615697890940291918017885487543 ;
 0.786291018233046684731786459135 ; 0.911107073689184553949066402429 ; 0.982921890023145161262671078244 |] ;;

let radau_13_w = [| 0.118343195266272189349112426036e-1 ; 0.719024162924955289397537405641e-1 ; 0.125103834331152358133769287976 ; 0.171003460470616642463758674512 ;
 0.206960611455877074631132560829 ; 0.230888862886995434012203758668 ; 0.241398342287691148630866924129 ; 0.237878547660712031342685189180 ; 0.220534229288451464691077164199 ;
 0.190373715559631732254759820746 ; 0.149150950090000205151491864242 ; 0.992678068818470859847363877478e-1 ; 0.437029032679020748288533846051e-1 |] ;;


let radau_14_x = [| - 1.0 ; - 0.962779269978024297120561244319 ; - 0.877048918201462024795266773531 ; - 0.747389642613378838735429134263 ; - 0.580314056546874971105726664999 ;
 - 0.384202003439203313794083903375 ; - 0.168887928042680911008441695622 ; 0.548312279917645496498107146428e-1 ; 0.275737205435522399182637403545 ; 0.482752918588474966820418534355 ;
 0.665497977216884537008955042481 ; 0.814809550601994729434217249123 ; 0.923203722520643299246334950272 ; 0.985270697947821356698617003172 |] ;;

let radau_14_w = [| 0.102040816326530612244897959184e-1 ; 0.621220169077714601661329164668e-1 ; 0.108607722744362826826720935229 ; 0.149620539353121355950520836946 ;
 0.183127002125729654123867302103 ; 0.207449763335175672668082886489 ; 0.221369811499570948931671683021 ; 0.224189348002707794238414632220 ; 0.215767100604618851381187446115 ;
 0.196525518452982430324613091930 ; 0.167429727891086278990102277038 ; 0.129939668737342347807425737146 ; 0.859405354429804030893077310866e-1 ; 0.377071632698969142774627282919e-1 |] ;;


let radau_15_x = [| - 1.0 ; - 0.967550468197200476562456018282 ; - 0.892605400120550767066811886849 ; - 0.778685617639031079381743321893 ; - 0.630779478886949283946148437224 ;
 - 0.455352905778529370872053455981 ; - 0.260073376740807915768961188263 ; - 0.534757226797460641074538896258e-1 ; 0.155410685384859484319182024964 ; 0.357456512022127651195319205174 ;
 0.543831458701484016930711802760 ; 0.706390264637572540152679669478 ; 0.838029000636089631215097384520 ; 0.932997190935973719928072142859 ; 0.987166478414363086378359071811 |] ;;

let radau_15_w = [| 0.888888888888888888888888888889e-2 ; 0.542027800486444943382142368018e-1 ; 0.951295994604808992038477266346e-1 ; 0.131875462504951632186262157944 ;
 0.162854477303832629448732245828 ; 0.186715145839450908083795103799 ; 0.202415187030618429872703310435 ; 0.209268608147694581430889790306 ; 0.206975960249553755479027321787 ;
 0.195637503045116116473556617575 ; 0.175748872642447685670310440476 ; 0.148179527003467253924682058743 ; 0.114135203489752753013075582569 ; 0.751083927605064397329716653914e-1 ;
 0.328643915845935322530428528231e-1 |] ;;




(** {C  } *)



(** {v float_weighted_int_minus1_1 abscissae weights function a b v} The standard abscissae are spread over the interval [-1 ; 1].
The ends of the integration interval must be filled in.

Les abscisses normalisées sont réparties sur l'interavlle [-1 ; 1].
Les bornes d'intégration doivent être précisées. *)
let float_weighted_int_minus1_1 = fun (x:float array) (w:float array) (f:float -> float) (a:float) (b:float) ->
 let l = Array.length x
 and accu = ref 0.
 and center = ( a +. b ) /. 2.
 and halflength = ( b -. a ) /. 2. in
  for i = 0 to l - 1 do
   accu := !accu +. w.(i) *. f ( center +. halflength *. x.(i) ) ;
  done ;
  !accu *. halflength ;;




(** {C § } *)
(** 
{2 Méthodes adaptatives variées}
{2 Miscellaneous adaptative methods}
*)
(** {C  } *)




(** {v float_int_dichot_adapt methode tolerance function a b v} The method must contain the integrating method, included
the parameters, as in the following example. 
The tolerance is the relative error allowed between two steps in order to stop the dichotomy.

{v float_int_romberg 11 4 v}

La méthode doit contenir la méthode d'intégration, y compris les paramètres, 
comme dans l'exemple ci-dessus. La tolérance est l'erreur relative admise entre deux pas pour arrêter la dichotomie.
*)
let rec float_int_dichot_adapt = fun methode (tol:float) (f:float -> float) (a:float) (b:float) ->
 let first = methode f a b
 and c = ( a +. b ) *. 0.5 in
  let second = ref ( methode f a c +. methode f c b ) in
   if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
    begin
     second := float_int_dichot_adapt methode tol f a c +. float_int_dichot_adapt methode tol f c b
    end ;
   !second ;;

(** {v float_int_dichot_bounded maxstages methode tolerance function a b v} The method must contain the integrating method, included
the parameters, as in the following example. 
The tolerance is the relative error allowed between two steps in order to stop the dichotomy.

{v float_int_romberg 11 4 v}

La méthode doit contenir la méthode d'intégration, y compris les paramètres, 
comme dans l'exemple ci-dessus. La tolérance est l'erreur relative admise entre deux pas pour arrêter la dichotomie.
*)
let rec float_int_dichot_bounded = fun (maxstages:int) methode (tol:float) (f:float -> float) (a:float) (b:float) ->
 match maxstages with 
 | 0 -> methode f a b
 | _ -> 
  let first = methode f a b
  and c = ( a +. b ) *. 0.5 in
   let second = ref ( methode f a c +. methode f c b ) in
    if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
     begin
      second := float_int_dichot_bounded ( ( abs maxstages ) - 1 ) methode tol f a c
       +. float_int_dichot_bounded ( ( abs maxstages ) - 1 ) methode tol f c b
     end ;
    !second ;;


(** {v float_int_multi_adapt methode nslices tolerance function a b v} The method must contain the integrating method, included
the parameters, as in the following example. 
The tolerance is the relative error allowed between two steps in order to stop the slicing.

{v float_int_romberg 11 4 v}

La méthode doit contenir la méthode d'intégration, y compris les paramètres, 
comme dans l'exemple ci-dessus. La tolérance est l'erreur relative admise entre deux pas pour arrêter le tranchage.
*)
let rec float_int_multi_adapt = fun methode (nslices:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 let first = methode f a b
 and length = ( b -. a ) /. ( float nslices ) in
  let c = ref ( a +. length )
  and d = ref 0. in
   let second = ref ( methode f a !c ) in
   for i = 2 to nslices do
    d := !c +. length ;
    second := !second +. methode f !c !d ;
    c := !d ;
   done ;
   if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
    begin
     c := a +. length ;
     second := float_int_multi_adapt methode nslices tol f a !c ;
     for i = 2 to nslices do
      d := !c +. length ;
      second := !second +. float_int_multi_adapt methode nslices tol f !c !d ;
      c := !d ;
     done ;
    end ;
    !second ;;


(** {v float_int_multi_bounded maxstages methode nslices tolerance function a b v} The method must contain the integrating method, included
the parameters, as in the following example. 
The tolerance is the relative error allowed between two steps in order to stop the slicing.

{v float_int_romberg 11 4 v}

La méthode doit contenir la méthode d'intégration, y compris les paramètres, 
comme dans l'exemple ci-dessus. La tolérance est l'erreur relative admise entre deux pas pour arrêter le tranchage.
*)
let rec float_int_multi_bounded = fun (maxstages:int) methode (nslices:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 match maxstages with 
 | 0 -> methode f a b
 | _ -> 
  let first = methode f a b
  and length = ( b -. a ) /. ( float nslices ) in
   let c = ref ( a +. length )
   and d = ref 0. in
    let second = ref ( methode f a !c ) in
    for i = 2 to nslices do
     d := !c +. length ;
     second := !second +. methode f !c !d ;
     c := !d ;
    done ;
    if ( abs_float ( !second -. first ) ) > tol *. ( abs_float !second ) then
     begin
      c := a +. length ;
      second := float_int_multi_bounded ( ( abs maxstages ) - 1 ) methode nslices tol f a !c ;
      for i = 2 to nslices do
       d := !c +. length ;
       second := !second +. float_int_multi_bounded ( ( abs maxstages ) - 1 ) methode nslices tol f !c !d ;
       c := !d ;
      done ;
     end ;
     !second ;;


(** {v float_int_adapt_trapez_simpson nintervals tolerance function a b v} The tolerance is the maximal 
relative error for every step whose overshoot triggers an integral over the sub-interval.

La tolérance est l'erreur relative maximale pour chaque pas dont le dépassement 
entraîne une intégrale sur le sous-intervalle. *)
let float_int_adapt_trapez_simpson = fun (n:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and x = ref a
 and y = ref ( f a )
 and length = ( b -. a ) /. ( float n )
 and nn = n - 2
 and nnn = int_of_float ( sqrt ( float n ) )  in
  let xx = ref ( a +. length ) in
   let yy = ref ( f !xx ) in
    let z = ref ( ( !y +. !yy ) *. 0.5 ) in
     if abs_float ( !yy -. !y ) <= tol *. abs_float !z then 
      accu := !z *. length
     else accu := float_int_simpson nnn f a !xx ;
     for i = 1 to nn do
      x := !xx ;
      xx := !xx +. length ;
      y := f !x ;
      yy := f !xx ;
      z := ( !y +. !yy ) *. 0.5 ;
      if abs_float ( !yy -. !y ) <= tol *. abs_float !z then 
       accu := !accu +. !z *. length
      else accu := !accu +. float_int_simpson nnn f !x !xx ;
     done ;
     x := !xx ;
     y := f !x ;
     yy := f b ;
     z := ( !y +. !yy ) *. 0.5 ;
     if abs_float ( !yy -. !y ) <= tol *. abs_float !z then 
      accu := !accu +. !z *. length
     else accu := !accu +. float_int_simpson nnn f !x b ;
     !accu ;;


(** {v float_int_adapt methode nintervals tolerance function a b v} The method must contain the integrating method, 
included the parameters, as in the following example. 
The tolerance is the maximal relative error for every step 
whose overshoot triggers an integral over the sub-interval.

{v float_int_simpson 100 v}

La méthode doit contenir la méthode d'intégration, y compris les paramètres, comme dans l'exemple ci-dessus. 
La tolérance est l'erreur relative maximale pour chaque pas dont le dépassement 
entraîne une intégrale sur le sous-intervalle.
*)
let float_int_adapt = fun methode (n:int) (tol:float) (f:float -> float) (a:float) (b:float) ->
 let accu = ref 0.
 and x = ref a
 and y = ref ( f a )
 and length = ( b -. a ) /. ( float n )
 and nn = n - 2 in
  let xx = ref ( a +. length ) in
   let yy = ref ( f !xx ) in
    let z = ref ( ( !y +. !yy ) *. 0.5 ) in
     if abs_float ( !yy -. !y ) <= tol *. abs_float !z then 
      accu := !z *. length
     else accu := methode f a !xx ;
     for i = 1 to nn do
      x := !xx ;
      xx := !xx +. length ;
      y := f !x ;
      yy := f !xx ;
      z := ( !y +. !yy ) *. 0.5 ;
      if abs_float ( !yy -. !y ) <= tol *. abs_float !z then 
       accu := !accu +. !z *. length
      else accu := !accu +. methode f !x !xx ;
     done ;
     x := !xx ;
     y := f !x ;
     yy := f b ;
     z := ( !y +. !yy ) *. 0.5 ;
     if abs_float ( !yy -. !y ) <= tol *. abs_float !z then 
      accu := !accu +. !z *. length
     else accu := !accu +. methode f !x b ;
     !accu ;;




(** {C § } *)
(** 
{2 Intégrales multiples et multidimensionnelles}
{2 Multidimensional and multiple integrals}
*)
(** {C  } *)




(** The multiple integrals are calculated over rectangular parallelepipedes.

Les intégrales multiples sont calculées sur des pavés. *)


(** {v float_int_double methode function a b v} *)
let float_int_double = fun methode (f:float array -> float) (a:float array) (b:float array) ->
 let g = function y -> methode ( function x -> f [| x ; y |] ) a.(0) b.(0) in
  methode g a.(1) b.(1) ;;

(** {v float_int_triple methode function a b v} *)
let float_int_triple = fun methode (f:float array -> float) (a:float array) (b:float array) ->
 let g = function v -> methode ( function x -> f ( Array.append [| x |] v ) ) a.(0) b.(0) in
  float_int_double methode g ( Array.sub a 1 2 ) ( Array.sub b 1 2 ) ;;

(** {v float_int_mult methode function a b v} *)
let rec float_int_mult = fun methode (f:float array -> float) (a:float array) (b:float array) ->
 let r = Array.length a in
  match r  with
  | 0 -> 0.
  | 1 -> methode ( function x -> f [| x |] ) a.(0) b.(0)
  | 2 -> float_int_double methode f a b 
  | 3 -> float_int_triple methode f a b
  | _ -> 
   let g = function v -> methode ( function x -> f ( Array.append [| x |] v ) ) a.(0) b.(0)
   and rr = r - 1 in
    float_int_mult methode g ( Array.sub a 1 rr ) ( Array.sub b 1 rr ) ;;


(** {v matrix_float_int_mult methode function a b v} *)
let rec matrix_float_int_mult = fun methode (f:float array array -> float) (a:float array array) (b:float array array) ->
 let r = Array.length a in
  match r  with
  | 0 -> 0.
  | 1 -> float_int_mult methode ( function x -> f [| x |] ) a.(0) b.(0)
  | _ -> 
   let g = function v -> float_int_mult methode ( function x -> f ( Array.append [| x |] v ) ) a.(0) b.(0)
   and rr = r - 1 in
    matrix_float_int_mult methode g ( Array.sub a 1 rr ) ( Array.sub b 1 rr ) ;;


(** {v float_vector_int methode function a b v} *)
let float_vector_int = fun methode (f:float -> float array) (a:float) (b:float) ->
 let r = Array.length ( f a ) in
  let v = Array.make r 0. in
   for i = 0 to r - 1 do
    v.(i) <- methode ( function x -> ( f x ).(i) ) a b
   done ;
   v ;;

(** {v float_matrix_int methode function a b v} *)
let float_matrix_int = fun methode (f:float -> float array array) (a:float) (b:float) ->
 let aa = ( f a ) in
  let r = Array.length aa
  and c = Array.length aa.(0) in
   let m = Array.make_matrix r c 0. in
    for i = 0 to r - 1 do
     m.(i) <- float_vector_int methode ( function x -> ( f x ).(i) ) a b
    done ;
    m ;;

(** {v vector_int_mult methode function a b v} *)
let vector_int_mult = fun methode (f:float array -> float array) (a:float array) (b:float array) ->
 let r = Array.length ( f a ) in
  let v = Array.make r 0. in
   for i = 0 to r - 1 do
    v.(i) <- float_int_mult methode ( function x -> ( f x ).(i) ) a b
   done ;
   v ;;

(** {v vector_matrix_int_mult methode function a b v} *)
let vector_matrix_int_mult = fun methode (f:float array -> float array array) (a:float array) (b:float array) ->
 let aa = ( f a ) in
  let r = Array.length aa
  and c = Array.length aa.(0) in
   let m = Array.make_matrix r c 0. in
    for i = 0 to r - 1 do
     m.(i) <- vector_int_mult methode ( function x -> ( f x ).(i) ) a b
    done ;
    m ;;


(** {v matrix_vector_int_mult methode function a b v} *)
let rec matrix_vector_int_mult = fun methode (f:float array array -> float array) (a:float array array) (b:float array array) ->
 let r = Array.length a in
  match r  with
  | 0 -> [| 0. |]
  | 1 -> vector_int_mult methode ( function x -> f [| x |] ) a.(0) b.(0)
  | _ -> 
   let g = function v -> vector_int_mult methode ( function x -> f ( Array.append [| x |] v ) ) a.(0) b.(0)
   and rr = r - 1 in
    matrix_vector_int_mult methode g ( Array.sub a 1 rr ) ( Array.sub b 1 rr ) ;;


(** {v matrix_int_mult methode function a b v} *)
let rec matrix_int_mult = fun methode (f:float array array -> float array array) (a:float array array) (b:float array array) ->
 let r = Array.length a in
  match r  with
  | 0 -> [| [| 0. |] |]
  | 1 -> vector_matrix_int_mult methode ( function x -> f [| x |] ) a.(0) b.(0)
  | _ -> 
   let g = function v -> vector_matrix_int_mult methode ( function x -> f ( Array.append [| x |] v ) ) a.(0) b.(0)
   and rr = r - 1 in
    matrix_int_mult methode g ( Array.sub a 1 rr ) ( Array.sub b 1 rr ) ;;



(** {v float_int_mult_monte_carlo samples function center range v} *)
let rec float_int_mult_monte_carlo = fun (samples:int) (f:float array -> float) (a:float array) (r:float) ->
 let l = Array.length a
 and s = float samples
 and x = ref a
 and accu = ref 0. in
  for i = 1 to samples do
   x := Matrix.vector_float_plus a ( Matrix.vector_float_bal_random l r ) ;
   accu := !accu +. f !x ;
  done ;
  !accu *. ( ( 2. *. r ) ** ( float l ) ) /. s ;;


(** {v float_compensated_int_mult_monte_carlo accelerator samples function center range v} The [accelerator] is appied to real numbers. The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux réels. Le facteur [factor] doit être choisi entre 0 et 1. *)
let float_compensated_int_mult_monte_carlo = fun accelerator (stages:int) (factor:float) (samples:int) (f:float array -> float) (a:float array) (r:float) ->
 let seq = Array.make stages 0.
 and aux = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) ) in
  for i = 0 to pred stages do
   seq.(i) <- float_int_mult_monte_carlo ( aux samples ( stages - i - 1 ) ) f a r
  done ;
  accelerator seq ;;


(** {v vector_int_mult_monte_carlo samples function center range v} *)
let vector_int_mult_monte_carlo = fun (samples:int) (f:float array -> float array) (a:float array) (r:float) ->
 let l = Array.length a
 and s = float samples
 and x = ref a
 and accu = ref ( Array.make ( Array.length ( f a ) ) 0. ) in
  for i = 1 to samples do
   x := Matrix.vector_float_plus a ( Matrix.vector_float_bal_random l r ) ;
   accu := Matrix.vector_float_plus !accu ( f !x ) ;
  done ;
  Matrix.vector_float_scal_mult ( ( ( 2. *. r ) ** ( float l ) ) /. s ) !accu ;;


(** {v vector_compensated_int_mult_monte_carlo accelerator samples function center range v} The [accelerator] is appied to real vectors. The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux vecteurs réels. Le facteur [factor] doit être choisi entre 0 et 1. *)
let vector_compensated_int_mult_monte_carlo = fun accelerator (stages:int) (factor:float) (samples:int) (f:float array -> float array) (a:float array) (r:float) ->
 let seq = Array.make stages ( f a )
 and aux = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) ) in
  for i = 0 to pred stages do
   seq.(i) <- vector_int_mult_monte_carlo ( aux samples ( stages - i - 1 ) ) f a r
  done ;
  accelerator seq ;;


(** {v vector_matrix_int_mult_monte_carlo samples function center range v} *)
let vector_matrix_int_mult_monte_carlo = fun (samples:int) (f:float array -> float array array) (a:float array) (r:float) ->
 let l = Array.length a
 and s = float samples
 and x = ref a
 and fa = f a in
  let accu = ref ( Array.make_matrix ( Array.length fa ) ( Array.length fa.(0) ) 0. ) in
  for i = 1 to samples do
   x := Matrix.vector_float_plus a ( Matrix.vector_float_bal_random l r ) ;
   accu := Matrix.matrix_float_plus !accu ( f !x ) ;
  done ;
  Matrix.matrix_float_scal_mult ( ( ( 2. *. r ) ** ( float l ) ) /. s ) !accu ;;


(** {v vector_matrix_compensated_int_mult_monte_carlo accelerator samples function center range v} The [accelerator] is appied to real matrices. The [factor] must be chosen 0 and 1.

L'accélérateur [accelerator] s'applique aux matrices réelles. Le facteur [factor] doit être choisi entre 0 et 1. *)
let vector_matrix_compensated_int_mult_monte_carlo = fun accelerator (stages:int) (factor:float) (samples:int) (f:float array -> float array array) (a:float array) (r:float) ->
 let seq = Array.make stages ( f a )
 and aux = fun n p -> int_of_float ( ( factor ** ( float p ) ) *. ( float n ) ) in
  for i = 0 to pred stages do
   seq.(i) <- vector_matrix_int_mult_monte_carlo ( aux samples ( stages - i - 1 ) ) f a r
  done ;
  accelerator seq ;;



(** {v surface_int_3 methode_diff methode_int f phi a b v} The first function [f] is a parametrization of the surface from R^2 to R^3,
the second [phi] is a function from R^3 to R. The integration method [methode_int] calculates a double integral.

La première fonction [f] est un paramétrage de la surface de R^2 dans R^3, la seconde [phi] est une fonction de R^3 dans R.
La méthode d'intégration [methode_int] calcule une intégrale double. *)
let surface_int_3 = fun methode_diff methode_int (f:float array -> float array) (phi:float array -> float) (a:float array) (b:float array) ->
 let g = function x -> ( phi ( f x ) ) *. ( surface_area_element_3 methode_diff f x ) in
  methode_int g a b ;;


(** {v surface_int_2_3 methode_diff methode_int f phi a b v} The first function [f] is a parametrization of the surface from R^2 to R^3,
the second [phi] is a function from R^2 to R. The integration method [methode_int] calculates a double integral.

La première fonction [f] est un paramétrage de la surface de R^2 dans R^3, la seconde [phi] est une fonction de R^2 dans R.
La méthode d'intégration [methode_int] calcule une intégrale double. *)
let surface_int_2_3 = fun methode_diff methode_int (f:float array -> float array) (phi:float array -> float) (a:float array) (b:float array) ->
 let g = function x -> ( phi x ) *. ( surface_area_element_3 methode_diff f x ) in
  methode_int g a b ;;


(** {v surface_vector_int_3 methode_diff methode_int f phi a b v} The first function [f] is a parametrization of the surface from R^2 to R^3,
the second [phi] is a function from R^3 to R^n. The integration method [methode_int] calculates a vector double integral.

La première fonction [f] est un paramétrage de la surface de R^2 dans R^3, la seconde [phi] est une fonction de R^3 dans R^n.
La méthode d'intégration [methode_int] calcule une intégrale double à valeurs vectorielles. *)
let surface_vector_int_3 = fun methode_diff methode_int (f:float array -> float array) (phi:float array -> float array) (a:float array) (b:float array) ->
 let g = function x -> Matrix.vector_float_scal_mult ( surface_area_element_3 methode_diff f x ) ( phi ( f x ) ) in
  methode_int g a b ;;


(** {v surface_vector_int_2_3 methode_diff methode_int f phi a b v} The first function [f] is a parametrization of the surface from R^2 to R^3,
the second [phi] is a function from R^2 to R^n. The integration method [methode_int] calculates a vector double integral.

La première fonction [f] est un paramétrage de la surface de R^2 dans R^3, la seconde [phi] est une fonction de R^2 dans R^n.
La méthode d'intégration [methode_int] calcule une intégrale double à valeurs vectorielles. *)
let surface_vector_int_2_3 = fun methode_diff methode_int (f:float array -> float array) (phi:float array -> float array) (a:float array) (b:float array) ->
 let g = function x -> Matrix.vector_float_scal_mult ( surface_area_element_3 methode_diff f x ) ( phi x ) in
  methode_int g a b ;;


(** {v surface_matrix_int_3 methode_diff methode_int f phi a b v} The first function [f] is a parametrization of the surface from R^2 to R^3,
the second [phi] is a function from R^3 to M(n,p,R). The integration method [methode_int] calculates a matrix double integral.

La première fonction [f] est un paramétrage de la surface de R^2 dans R^3, la seconde [phi] est une fonction de R^3 dans M(n,p,R).
La méthode d'intégration [methode_int] calcule une intégrale double à valeurs matricielles. *)
let surface_matrix_int_3 = fun methode_diff methode_int (f:float array -> float array) (phi:float array -> float array array) (a:float array) (b:float array) ->
 let g = function x -> Matrix.matrix_float_scal_mult ( surface_area_element_3 methode_diff f x ) ( phi ( f x ) ) in
  methode_int g a b ;;


(** {v surface_matrix_int_2_3 methode_diff methode_int f phi a b v} The first function [f] is a parametrization of the surface from R^2 to R^3,
the second [phi] is a function from R^2 to M(n,p,R). The integration method [methode_int] calculates a matrix double integral.

La première fonction [f] est un paramétrage de la surface de R^2 dans R^3, la seconde [phi] est une fonction de R^2 dans M(n,p,R).
La méthode d'intégration [methode_int] calcule une intégrale double à valeurs matricielles. *)
let surface_matrix_int_2_3 = fun methode_diff methode_int (f:float array -> float array) (phi:float array -> float array array) (a:float array) (b:float array) ->
 let g = function x -> Matrix.matrix_float_scal_mult ( surface_area_element_3 methode_diff f x ) ( phi x ) in
  methode_int g a b ;;


(** {v surface_vector_flux_3 methode_diff methode_int f phi a b v} The first function [f] is a parametrization of the surface from R^2 to R^3,
the second [phi] is a vector field from R^3 to R^3. The integration method [methode_int] calculates a double integral.

La première fonction [f] est un paramétrage de la surface de R^2 dans R^3, la seconde [phi] est une champ de vecteurs de R^3 dans R^3.
La méthode d'intégration [methode_int] calcule une intégrale double. *)
let surface_vector_flux_3 = fun methode_diff methode_int (f:float array -> float array) (phi:float array -> float array) (a:float array) (b:float array) ->
 let g = function x -> Matrix.vector_float_scal_prod ( surface_area_vector_3 methode_diff f x ) ( phi ( f x ) ) in
  methode_int g a b ;;


(** {v surface_vector_flux_2_3 methode_diff methode_int f phi a b v} The first function [f] is a parametrization of the surface from R^2 to R^3,
the second [phi] is a vector field from R^2 to R^3. The integration method [methode_int] calculates a double integral.

La première fonction [f] est un paramétrage de la surface de R^2 dans R^3, la seconde [phi] est une champ de vecteurs de R^2 dans R^3.
La méthode d'intégration [methode_int] calcule une intégrale double. *)
let surface_vector_flux_2_3 = fun methode_diff methode_int (f:float array -> float array) (phi:float array -> float array) (a:float array) (b:float array) ->
 let g = function x -> Matrix.vector_float_scal_prod ( surface_area_vector_3 methode_diff f x ) ( phi x ) in
  methode_int g a b ;;




(** {C § } *)
(** 
{2 Intégrales discrètes}
{2 Discrete integrals}
*)
(** {C  } *)




(** {v float_discrete_int_rect length vector v} *)
let float_discrete_int_rect = fun (length:float) (v:float array) ->
 let accu = ref 0.
 and n = Array.length v in
  for i = 0 to n - 1 do
   accu := !accu +. v.(i)
  done ;
  !accu  *. length /. ( float n ) ;;

(** {v float_discrete_int_trapez length vector v} *)
let float_discrete_int_trapez = fun (length:float) (v:float array) ->
 let nn = ( Array.length v ) - 1 in
  let accu = ref ( (v.(0) +. v.(nn) ) *. 0.5 ) in
   for i = 1 to nn - 1 do
    accu := !accu +. v.(i)
   done ;
   !accu  *. length /. ( float nn ) ;;

(** {v float_discrete_int_simpson length vector v} *)
let float_discrete_int_simpson = fun (length:float) (v:float array) ->
 let n = Array.length v in
  let vv = Array.make ( n mod 2 ) 0. in
   let w = Array.append v vv in
    let nn = Array.length w in
     let nnn = nn / 2
     and accu = ref ( ( w.(0) +. w.( nn - 1 ) ) *. 0.5 +. 2. *. w.( nn - 2 ) ) in
      for i = 1 to nnn - 1 do
       accu := !accu +. w.( 2 * i ) +. 2. *. w.( 2 * i - 1 )
      done ;
      !accu  *. length /. ( 1.5 *. float n ) ;;


(** {v float_discrete_int_interpol methode_interp methode_int length vector v} The integration method
[methode_int] is aimed at functions and must contain all the parameters. The interpolation
method [methode_interpol] must contain all the parameters.

La méthode d'intégration [methode_int] est destinée aux fonctions et doit contenir tous les paramètres.
La méthode d'interpolation [methode_interpol] doit contenir tous les paramètres. *)
let float_discrete_int_interpol = fun methode_interpol methode_int (length:float) (v:float array) ->
 let f = methode_interpol v in
  length *. ( methode_int f 0.5 ( ( float ( Array.length v ) ) -. 0.5 ) ) ;;


(** {v vector_discrete_trans_int methode length matrix v} The data are given by coordinates.

Les données sont présentées coordonnée par coordonnée. *)
let vector_discrete_trans_int = fun methode (length:float) (v:float array array) ->
 let dim = Array.length v in
  let w = Array.make dim 0. in
   for i = 0 to dim - 1 do
    w.(i) <- methode v.(i)
   done ;
   w ;;


(** {v vector_discrete_int methode length matrix v} *)
let vector_discrete_int = fun methode (length:float) (v:float array array) ->
 vector_discrete_trans_int methode length ( Matrix.float_transpose v ) ;;


(** {v float_discrete_int_double methode length_x length_y vector v} The row numbers match the ordinates
and the column numbers match the abscissae.

Les numéros de ligne de v correspondent aux ordonnées et les numéros de colonne aux abscisses. *)
let float_discrete_int_double = fun methode (length_x:float) (length_y:float) (v:float array array) ->
 let w = Array.map ( methode length_x ) v in
  methode length_y w ;;


(** {v vector_discrete_int_double methode length_x length_y vector v} The row numbers match the ordinates
and the column numbers match the abscissae.

Les numéros de ligne de v correspondent aux ordonnées et les numéros de colonne aux abscisses. *)
let vector_discrete_int_double = fun methode (length_x:float) (length_y:float) (v:float array array array) ->
 let w = Array.map ( vector_discrete_int methode length_x ) v in
  vector_discrete_int methode length_y w ;;




(** {C § } *)
(** 
{1 Transformées}
{1 Transforms}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions}
*)
(** {C  } *)




(** {v float_fourier_coefficient methode function pulsation v} The method 
must contain the integrating method, included the parameters.

La méthode doit contenir la méthode d'intégration, y compris les paramètres. *)
let float_fourier_coefficient = fun methode (f:float -> float) (n:int) ->
 let g = function x -> ( f x ) *. cos ( ( float n ) *. x )
 and h = function x -> ( f x ) *. sin ( ( float n ) *. x ) in
  let coefficient = match n with
   | 0 -> inv_doublepi
   | _ -> 1. /. pi in
    [| coefficient *. methode g 0. doublepi ; inv_doublepi *. methode h 0. doublepi |] ;;


(** {v float_fourier_coefficient_general methode beginning ending function pulsation v} The method 
must contain the integrating method, included the parameters.

La méthode doit contenir la méthode d'intégration, y compris les paramètres. *)
let float_fourier_coefficient_general = fun methode (a:float) (b:float) (f:float -> float) (n:int) ->
 let g = function x -> f ( a +. ( b -. a ) *. x *. inv_doublepi ) in
  float_fourier_coefficient methode g n ;;


(** {v float_fourier_series coefficients real v} *)
let float_fourier_series = fun (coefficients:float array array) (x:float) ->
 let accu = ref coefficients.(0).(0) in
  for i = 1 to ( Array.length coefficients ) - 1 do
   accu := !accu +. coefficients.(i).(0) *. cos ( ( float i ) *. x ) +. coefficients.(i).(1) *. sin ( ( float i ) *. x )
  done ;
  !accu ;;


(** {v float_fourier_transform methode beginning ending function pulsation v} The method 
must contain the integrating method, included the parameters.

La méthode doit contenir la méthode d'intégration, y compris les paramètres. *)
let float_fourier_transform = fun methode (a:float) (b:float) (f:float -> float) (omega:float) ->
 let g = function x -> ( f x ) *. cos ( omega *. x )
 and h = function x -> ( f x ) *. sin ( omega *. x ) in
  [| methode g a b ; -. methode h a b |] ;;


(** {v float_inv_fourier_transform methode beginning ending function pulsation v} The method 
must contain the integrating method, included the parameters.

La méthode doit contenir la méthode d'intégration, y compris les paramètres. *)
let float_inv_fourier_transform = fun methode (a:float) (b:float) (f:float -> float array) (x:float) ->
 let g = function omega -> ( f omega ).(0)
 and h = function omega -> ( f omega ).(1) in
  let k = fun omega -> ( g omega ) *. cos ( omega *. x ) -. ( h omega ) *. sin ( omega *. x ) in
   inv_doublepi *. methode k a b ;;


(** {v float_laplace_complex methode ending function parameter v} The method 
must contain the integrating method, included the parameters.

La méthode doit contenir la méthode d'intégration, y compris les paramètres. *)
let float_laplace_complex = fun methode (b:float) (f:float -> float) (p:float array) ->
 let r = -. p.(0)
 and omega = -. p.(1) in
  let g = function x -> ( f x ) *. exp ( r *. x ) *. cos ( omega *. x )
  and h = function x -> ( f x ) *. exp ( r *. x ) *. sin ( omega *. x ) in
   [| methode g 0. b ; methode h 0. b |] ;;


(** {v float_laplace_real methode ending function parameter v} The method 
must contain the integrating method, included the parameters.

La méthode doit contenir la méthode d'intégration, y compris les paramètres. *)
let float_laplace_real = fun methode (b:float) (f:float -> float) (p:float) ->
 let g = function x -> ( f x ) *. exp ( -. p *. x ) in
  methode g 0. b ;;


(** {v float_inv_laplace_complex methode abscissa ending function parameter v} The method 
must contain the integrating method, included the parameters.

La méthode doit contenir la méthode d'intégration, y compris les paramètres. *)
let float_inv_laplace_complex = fun methode (a:float) (b:float) (f:float array -> float array) (x:float) ->
  let g = function omega -> ( f [| a ; omega |] ).(0) *. exp ( a *. x ) *. cos ( omega *. x )
   -. ( f [| a ; omega |] ).(1) *. exp ( a *. x ) *. sin ( omega *. x )
  and h = function omega -> ( f [| a ; omega |] ).(1) *. exp ( a *. x ) *. cos ( omega *. x )
   +. ( f [| a ; omega |] ).(0) *. exp ( a *. x ) *. sin ( omega *. x ) in
   [| inv_doublepi *. methode g ( -. b ) b ; inv_doublepi *. methode h ( -. b ) b |] ;;


(** {v float_sumudu_real methode ending function parameter v} The method 
must contain the integrating method, included the parameters.

La méthode doit contenir la méthode d'intégration, y compris les paramètres. *)
let float_sumudu_real = fun methode (b:float) (f:float -> float) (p:float) ->
 let g = function x -> ( f x ) *. exp ( -. x /. p ) /. p in
  methode g 0. b ;;




(** {C § } *)
(** 
{2 Transformées discrètes}
{2 Discrete transforms}
*)
(** {C  } *)




(** These transforms need some normalization of variables and parameters from the user.

Ces transformées nécessitent une normalisation des variables et paramètres de la part de l'utilisateur. *)

(** {C  } *)

(** {v discrete_float_causal_z_sequence vector complex v} *)
let discrete_float_causal_z_sequence = fun (v:float array) (z:float array) ->
 let n = Array.length v
 and coeff = 1. /. ( z.(0) *. z.(0) +. z.(1) *. z.(1) )
 and z0 = ref 1.
 and z1 = ref 0.
 and zz0 = ref 1.
 and zz1 = ref 0. in
  let w = Array.make_matrix n 2 0.
  and y = [| coeff *. z.(0) ; -. coeff *. z.(1) |] in
    for i = 0 to ( n - 1 ) do
     let row = w.(i)
     and vv = v.(i) in
      row.(0) <- vv *. !z0 ;
      row.(1) <- vv *. !z1 ;
      w.(i) <- row ;
      zz0 := !z0 *. y.(0) -. !z1 *. y.(1) ;
      zz1 := ( !z1 *. y.(0) ) +. ( !z0 *. y.(1) ) ;
      z0 := !zz0 ;
      z1 := !zz1 ;
    done ;
    w ;;

(** {v discrete_float_symmetric_z_sequence vector complex v} *)
let discrete_float_symmetric_z_sequence = fun (v:float array) (z:float array) ->
 let n = Array.length v
 and coeff = 1. /. ( z.(0) *. z.(0) +. z.(1) *. z.(1) )
 and z0 = ref 1.
 and z1 = ref 0.
 and zz0 = ref 1.
 and zz1 = ref 0. in
  let w = Array.make_matrix n 2 0.
  and nn = n / 2
  and y = [| coeff *. z.(0) ; -. coeff *. z.(1) |] in
    for i = nn to ( n - 1 ) do
     let row = w.(i)
     and vv = v.(i) in
      row.(0) <- vv *. !z0 ;
      row.(1) <- vv *. !z1 ;
      w.(i) <- row ;
      zz0 := !z0 *. y.(0) -. !z1 *. y.(1) ;
      zz1 := ( !z1 *. y.(0) ) +. ( !z0 *. y.(1) ) ;
      z0 := !zz0 ;
      z1 := !zz1 ;
    done ;
    z0 := z.(0) ;
    z1 := z.(1) ;
    zz0 := z.(0) ;
    zz1 := z.(1) ;
    for i = ( nn - 1 ) downto 0 do
     let row = w.(i)
     and vv = v.(i) in
      row.(0) <- vv *. !z0 ;
      row.(1) <- vv *. !z1 ;
      w.(i) <- row ;
      zz0 := !z0 *. z.(0) -. !z1 *. z.(1) ;
      zz1 := ( !z1 *. z.(0) ) +. ( !z0 *. z.(1) ) ;
      z0 := !zz0 ;
      z1 := !zz1 ;
    done ;
    w ;;

(** {v discrete_float_causal_z_transform vector complex v} *)
let discrete_float_causal_z_transform = fun (v:float array) (z:float array) ->
 Matrix.matrix_float_sum_by_column ( discrete_float_causal_z_sequence v z ) ;;

(** {v discrete_float_symmetric_z_transform vector complex v} *)
let discrete_float_symmetric_z_transform = fun (v:float array) (z:float array) ->
 Matrix.matrix_float_sum_by_column ( discrete_float_symmetric_z_sequence v z ) ;;

(** {v discrete_float_causal_fourier_sequence vector pulsation v} *)
let discrete_float_causal_fourier_sequence = fun (v:float array) (omega:float) ->
 discrete_float_causal_z_sequence v [| cos omega ; sin omega |] ;;

(** {v discrete_float_causal_fourier_transform vector pulsation v} This transform could be seen as a DTFT :
discrete time Fourier transform. The inversion may use [float_inv_fourier_transform].

Cette transformée pourrait se ranger dans la catégorie DTFT. 
L'inversion peut utiliser [float_inv_fourier_transform]. *)
let discrete_float_causal_fourier_transform = fun (v:float array) (omega:float) ->
 discrete_float_causal_z_transform v [| cos omega ; sin omega |] ;;

(** {v discrete_float_symmetric_fourier_sequence vector pulsation v} *)
let discrete_float_symmetric_fourier_sequence = fun (v:float array) (omega:float) ->
 discrete_float_symmetric_z_sequence v [| cos omega ; sin omega |] ;;


(** {v discrete_float_symmetric_fourier_transform vector complex v} This transform could be seen as a DTFT :
discrete time Fourier transform. The inversion may use [float_inv_fourier_transform].

Cette transformée pourrait se ranger dans la catégorie DTFT. 
L'inversion peut utiliser [float_inv_fourier_transform]. *)
let discrete_float_symmetric_fourier_transform = fun (v:float array) (omega:float) ->
 discrete_float_symmetric_z_transform v [| cos omega ; sin omega |] ;;




(** {C § } *)
(** 
{3 Remarque}
{3 Remark}
*)
(** {C  } *)




(** Every transform defined on functions may be applied to discrete data via the interpolation.

Toute transformée définie sur des fonctions peut s'appliquer aux données discrètes via l'interpolation. *)

(** {C  } *)

(** {v discrete_float_transform_int methode_interpol methode_transform vector integer v} *)
let discrete_float_transform_int = fun methode_interpol methode_transform (v:float array) (n:int) ->
 let f = methode_interpol v in
  methode_transform f n ;;

(** {v discrete_float_transform_real methode_interpol methode_transform vector real v} *)
let discrete_float_transform_real = fun methode_interpol methode_transform (v:float array) (omega:float) ->
 let f = methode_interpol v in
  methode_transform f omega ;;

(** {v discrete_float_transform_complex methode_interpol methode_transform vector complex v} *)
let discrete_float_transform_complex = fun methode_interpol methode_transform (v:float array) (z:float array) ->
 let f = methode_interpol v in
  methode_transform f z ;;






(** {C § } *)
(** 
{1 Points critiques et inversions locales}
{1 Critical points and local inversions}
*)
(** {C  } *)



(** {v float_critical methode_zero methode_diff function start v} *)
let float_critical = fun methode_zero methode_diff (f:float -> float) (a:float) ->
 let g = methode_diff f in
  methode_zero g a ;;

(** {v vector_float_critical methode_zero methode_diff function start v} *)
let vector_float_critical = fun methode_zero methode_diff (f:float array -> float) (a:float array) ->
 let g = gradient methode_diff f in
  methode_zero g a ;;

(** {v vector_critical methode_zero methode_diff function start v} *)
let vector_critical = fun methode_zero methode_diff (f:float array -> float array) (a:float array) ->
 let g = function vector -> Matrix.float_slow_invertibility_evaluation ( tlm methode_diff f vector ) in
  methode_zero g a ;;

(** {v float_local_inverse methode_zero guess function real v} *)
let float_local_inverse = fun methode_zero (a:float) (f:float -> float) (x:float) ->
 let g = function y -> x -. f y in
  methode_zero g a ;;

(** {v vector_local_inverse methode_zero guess function real v} An exception
probably means that the local inversion passed through a critical point.
In this case, the start point [guess] has to be changed, or the method of zero finding 
should not use a tangent linear application.

Une exception signifie probablement que l'inversion locale est passée par un point critique.
Dans ce cas, il faut changer le point de départ [guess] ou prendre une méthode de recherche de zéro
qui n'utilise pas d'application linéaire tangente. *)
let vector_local_inverse = fun methode_zero (guess:float array) (f:float array -> float array ) (x:float array) ->
 let g = function y -> Matrix.vector_float_minus x ( f y ) in
  methode_zero g guess ;;

(** {v float_implicit_function methode_zero guess function real v} *)
let float_implicit_function = fun methode_zero (a:float) (f:float -> float -> float) (x:float) ->
 let g = function y -> f x y in
  methode_zero g a ;;

(** {v vector_implicit_function methode_zero guess function real v} *)
let vector_implicit_function = fun methode_zero (a:float array) (f:float array-> float array -> float array ) (x:float array) ->
 let g = function y -> f x y in
  methode_zero g a ;;

(** {v float_flat_search methode_zero methode_int function start length v} It is question of
searching an interval of length [length] and upon which the integral
of the absolute value of the function is zero.
The method of zero finding, the method of integration and a start point [x] have to be stated.

Il s'agit de rechercher un intervalle de longueur [length] sur lequel 
l'intégrale de la valeur absolue de la fonction est nulle.
Il faut préciser la méthode de recherche de zéro, la méthode d'intégration et un point de départ [x]. *)
let float_flat_search = fun methode_zero methode_int (f:float -> float) (x:float) (length:float) ->
 let g = function y -> abs_float ( f y ) in
  let h = function y -> methode_int g y ( length +. y ) in
   methode_zero h x ;;








(** {C § } *)
(** 
{1 Équations différentielles ordinaires explicites y' = f(x,y)}
{1 Explicit ordinary differential equations y' = f(x,y)}
*)
(** {C  } *)




(** {C  } *)
(** 
{2 Méthodes explicites}
{2 Explicit methods}
*)
(** {C  } *)




(** {v float_ode_euler function nsteps value beginning ending v} *)
let float_ode_euler = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  for i = 1 to nsteps do
   let z = y.(i - 1) in
    y.(i) <- z +. step *. f !x z ;
    x := !x +. step ;
  done ;
  y ;;

(** {v vector_ode_euler function nsteps value beginning ending v} *)
let vector_ode_euler = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  for i = 1 to nsteps do
   let z = y.(i - 1) in
    y.(i) <- Matrix.vector_float_plus z ( Matrix.vector_float_scal_mult step ( f !x z ) ) ;
    x := !x +. step ;
  done ;
  y ;;

(** {v matrix_ode_euler function nsteps value beginning ending v} *)
let matrix_ode_euler = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  for i = 1 to nsteps do
   let z = y.(i - 1) in
    y.(i) <- Matrix.matrix_float_plus z ( Matrix.matrix_float_scal_mult step ( f !x z ) ) ;
    x := !x +. step ;
  done ;
  y ;;


(** {v float_end_ode_euler function nsteps value beginning ending v} *)
let float_end_ode_euler = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0 in
  for i = 1 to nsteps do
   y := !y +. step *. f !x !y ;
   x := !x +. step ;
  done ;
  !y ;;

(** {v vector_end_ode_euler function nsteps value beginning ending v} *)
let vector_end_ode_euler = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0 in
  for i = 1 to nsteps do
   y := Matrix.vector_float_plus !y ( Matrix.vector_float_scal_mult step ( f !x !y ) ) ;
   x := !x +. step ;
  done ;
  !y ;;

(** {v matrix_end_ode_euler function nsteps value beginning ending v} *)
let matrix_end_ode_euler = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0 in
  for i = 1 to nsteps do
   y := Matrix.matrix_float_plus !y ( Matrix.matrix_float_scal_mult step ( f !x !y ) ) ;
   x := !x +. step ;
  done ;
  !y ;;



(** {v float_ode_adams_bashforth_2 function nsteps value beginning ending v} *)
let float_ode_adams_bashforth_2 = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and accu = ref y0
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5 in
   accu := f beginning y0 ;
   y.(1) <- y0 +. step *. !accu ;
   for i = 2 to nsteps do
    x := !x +. step ;
    let z = y.( i - 1 ) in
     let zz = f !x z in
      y.(i) <- z +. halfstep *. ( 3. *. zz -. !accu ) ;
      accu := zz ;
   done ;
   y ;;

(** {v vector_ode_adams_bashforth_2 function nsteps value beginning ending v} *)
let vector_ode_adams_bashforth_2 = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and accu = ref y0
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5 in
   accu := f beginning y0 ;
   y.(1) <- Matrix.vector_float_plus y0 ( Matrix.vector_float_scal_mult step !accu ) ;
   for i = 2 to nsteps do
    x := !x +. step ;
    let z = y.( i - 1 ) in
     let zz = f !x z in
      y.(i) <- Matrix.vector_float_plus z ( Matrix.vector_float_scal_mult halfstep ( ( Matrix.vector_float_minus ( Matrix.vector_float_scal_mult 3. zz ) !accu ) ) ) ;
      accu := zz ;
   done ;
   y ;;

(** {v matrix_ode_adams_bashforth_2 function nsteps value beginning ending v} *)
let matrix_ode_adams_bashforth_2 = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and accu = ref y0
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5 in
   accu := f beginning y0 ;
   y.(1) <- Matrix.matrix_float_plus y0 ( Matrix.matrix_float_scal_mult step !accu ) ;
   for i = 2 to nsteps do
    x := !x +. step ;
    let z = y.( i - 1 ) in
     let zz = f !x z in
      y.(i) <- Matrix.matrix_float_plus z ( Matrix.matrix_float_scal_mult halfstep ( ( Matrix.matrix_float_minus ( Matrix.matrix_float_scal_mult 3. zz ) !accu ) ) ) ;
      accu := zz ;
   done ;
   y ;;


(** {v float_end_ode_adams_bashforth_2 function nsteps value beginning ending v} *)
let float_end_ode_adams_bashforth_2 = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and accu = ref y0
 and y = ref y0
 and yy = ref y0
 and z = ref y0 in
  let halfstep = step *. 0.5 in
   accu := f beginning y0 ;
   y := y0 +. step *. !accu ;
   for i = 2 to nsteps do
    x := !x +. step ;
    let zz = f !x !y in
     z := !y +. halfstep *. ( 3. *. zz -. !accu ) ;
     accu := zz ;
     yy := !y ;
     y := !z ;
   done ;
   !y ;;

(** {v vector_end_ode_adams_bashforth_2 function nsteps value beginning ending v} *)
let vector_end_ode_adams_bashforth_2 = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and r = Array.length y0
 and x = ref beginning
 and y = Matrix.vector_float_copy y0
 and yy = Matrix.vector_float_copy y0
 and z = Matrix.vector_float_copy y0 in
  let halfstep = step *. 0.5
  and accu = Array.make_matrix 2 r 0.
  and rr = r - 1 in
   let zzz = f beginning y0 in
    for j = 0 to rr do 
     accu.(0).(j) <- zzz.(j) ;
     y.(j) <- y0.(j) +. step *. accu.(0).(j) ;
    done ;
    for i = 2 to nsteps do
     x := !x +. step ;
     let zz = f !x y in
      for j = 0 to rr do
       z.(j) <- y.(j) +. halfstep *. ( 3. *. zz.(j) -. accu.(0).(j) ) ;
       accu.(1).(j) <- zz.(j) ;
      done ;
      for j = 0 to rr do
       yy.(j) <- y.(j) ;
       y.(j) <- z.(j) ;
       accu.(0).(j) <- accu.(1).(j) ;
      done ;
    done ;
    y ;;

(** {v matrix_end_ode_adams_bashforth_2 function nsteps value beginning ending v} *)
let matrix_end_ode_adams_bashforth_2 = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and r = Array.length y0
 and c = Array.length y0.(0)
 and x = ref beginning
 and y = Matrix.matrix_float_copy y0
 and yy = Matrix.matrix_float_copy y0
 and z = Matrix.matrix_float_copy y0 in
  let halfstep = step *. 0.5
  and accu = [| Array.make_matrix r c 0. ; Array.make_matrix r c 0. |]
  and rr = r - 1
  and cc = c - 1 in
   let init = f beginning y0 in
    for j = 0 to rr do
     let row_input = init.(j)
     and row = y0.(j)
     and ligne = accu.(0).(j)
     and row_output = y.(j) in
      for k = 0 to cc do
       row_output.(k) <- row.(k) +. step *. row_input.(k) ;
       ligne.(k) <- row_input.(k) ;
      done ;
    done ;
    for i = 2 to nsteps do
     x := !x +. step ;
     let first = f !x y in
      for j = 0 to rr do
       let row_first = first.(j)
       and row_second = accu.(0).(j)
       and row = y.(j)
       and ligne = accu.(1).(j)
       and row_output = z.(j) in
        for k = 0 to cc do
         row_output.(k) <- row.(k) +. halfstep *. ( 3. *. row_first.(k) -. row_second.(k) ) ;
         ligne.(k) <- row_first.(k) ;
        done ;
      done ;
      for j = 0 to rr do
       yy.(j) <- Matrix.vector_float_copy y.(j) ;
       y.(j) <- Matrix.vector_float_copy z.(j) ;
       accu.(0).(j) <- accu.(1).(j) ;
      done ;
    done ;
    y ;;



(** {v float_ode_adams_bashforth_3 function nsteps value beginning ending v} *)
let float_ode_adams_bashforth_3 = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and accu = Array.make 2 0.
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5 in
   let twelfthstep = halfstep /. 6. in
    accu.(0) <- f beginning y0 ;
    y.(1) <- y0 +. step *. accu.(0) ;
    x := !x +. step ;
    accu.(1) <- f !x y.(1) ;
    y.(2) <- y.(1) +. halfstep *. ( 3. *. accu.(1) -. accu.(0) ) ;
    for i = 3 to nsteps do
     x := !x +. step ;
     let z = y.( i - 1 ) in
      let zzz = f !x z in
       y.(i) <- z +. twelfthstep *. ( 23. *. zzz -. 16. *. accu.(1) +. 5. *. accu.(0) ) ;
       accu.(0) <- accu.(1) ;
       accu.(1) <- zzz ;
    done ;
    y ;;

(** {v vector_ode_adams_bashforth_3 function nsteps value beginning ending v} *)
let vector_ode_adams_bashforth_3 = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and r = Array.length y0
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5
  and accu = Array.make_matrix 2 r 0. in
   let twelfthstep = halfstep /. 6.
   and zzz = f beginning y0 in
    y.(1) <- Matrix.vector_float_plus y0 ( Matrix.vector_float_scal_mult step zzz ) ;
    accu.(0) <- zzz ;
    x := !x +. step ;
    let zz = ( f !x y.(1) ) in
     y.(2) <- Matrix.vector_float_plus y.(1)
      ( Matrix.vector_float_scal_mult halfstep ( Matrix.vector_float_minus ( Matrix.vector_float_scal_mult 3. zz ) accu.(0) ) ) ;
     accu.(1) <- zz ;
     for i = 3 to nsteps do
      x := !x +. step ;
      let z = y.( i - 1 ) in
       let eval = f !x z in
        let term1 = Matrix.vector_float_scal_mult 23. eval
        and term2 = Matrix.vector_float_scal_mult (-16.) accu.(1)
        and term3 = Matrix.vector_float_scal_mult 5. accu.(0) in
         let sum = Matrix.vector_float_plus term1 ( Matrix.vector_float_plus term2 term3 ) in
          y.(i) <- Matrix.vector_float_plus z ( Matrix.vector_float_scal_mult twelfthstep sum ) ;
          accu.(0) <- accu.(1) ;
          accu.(1) <- eval ;
     done ;
     y ;;

(** {v matrix_ode_adams_bashforth_3 function nsteps value beginning ending v} *)
let matrix_ode_adams_bashforth_3 = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and r = Array.length y0
 and c = Array.length y0.(0)
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5
  and accu = [| Array.make_matrix r c 0. ; Array.make_matrix r c 0. |] in
   let twelfthstep = halfstep /. 6.
   and zzz = f beginning y0 in
    y.(1) <- Matrix.matrix_float_plus y0 ( Matrix.matrix_float_scal_mult step zzz ) ;
    accu.(0) <- zzz ;
    x := !x +. step ;
    let zz = f !x y.(1) in
     y.(2) <- Matrix.matrix_float_plus y.(1)
      ( Matrix.matrix_float_scal_mult halfstep ( Matrix.matrix_float_minus ( Matrix.matrix_float_scal_mult 3. zz ) accu.(0) ) ) ;
     accu.(1) <- zz ;
     for i = 3 to nsteps do
      x := !x +. step ;
      let z = y.( i - 1 ) in
       let eval = f !x z in
        let term1 = Matrix.matrix_float_scal_mult 23. eval
        and term2 = Matrix.matrix_float_scal_mult (-16.) accu.(1)
        and term3 = Matrix.matrix_float_scal_mult 5. accu.(0) in
         let sum = Matrix.matrix_float_plus term1 ( Matrix.matrix_float_plus term2 term3 ) in
          y.(i) <- Matrix.matrix_float_plus z ( Matrix.matrix_float_scal_mult twelfthstep sum ) ;
          accu.(0) <- accu.(1) ;
          accu.(1) <- eval ;
     done ;
     y ;;


(** {v float_end_ode_adams_bashforth_3 function nsteps value beginning ending v} *)
let float_end_ode_adams_bashforth_3 = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and accu = Array.make 2 0.
 and y = ref y0
 and yy = ref y0
 and z = ref y0 in
  let halfstep = step *. 0.5 in
   let twelfthstep = halfstep /. 6. in
    let zzz = f beginning y0 in
     yy := y0 +. step *. zzz ;
     accu.(0) <- zzz ;
     x := !x +. step ;
     let zz = f !x !yy in
      y := !yy +. halfstep *. ( 3. *. zz -. accu.(0) ) ;
      accu.(1) <- zz ;
      for i = 3 to nsteps do
       x := !x +. step ;
       let eval = f !x !y in
        z := !y +. twelfthstep *. ( 23. *. eval -. 16. *. accu.(1) +. 5. *. accu.(0) ) ;
        yy := !y ;
        y := !z ;
        accu.(0) <- accu.(1) ;
        accu.(1) <- eval ;
      done ;
      !y ;;

(** {v vector_end_ode_adams_bashforth_3 function nsteps value beginning ending v} *)
let vector_end_ode_adams_bashforth_3 = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and r = Array.length y0
 and pointeur = ref 1
 and x = ref beginning in
  let halfstep = step *. 0.5
  and rr = r - 1
  and y = Array.make r 0.
  and carrousel = Array.make_matrix 3 r 0. in
   let twelfthstep = halfstep /. 6. in
    let z = f beginning y0 in
     for j = 0 to rr do
      carrousel.(0).(j) <- z.(j) ;
      y.(j) <- y0.(j) +. step *. carrousel.(0).(j) ;
     done ;
     x := !x +. step ;
     let z = f !x y in
      for j = 0 to rr do
       carrousel.(1).(j) <- z.(j) ;
       y.(j) <- y.(j) +. halfstep *. ( 3. *. carrousel.(1).(j) -. carrousel.(0).(j) ) ;
      done ;
      for i = 3 to nsteps do
       x := !x +. step ;
       pointeur := ( !pointeur + 1 ) mod 3 ;
       let z = f !x y in
        for j = 0 to rr do
         carrousel.(!pointeur).(j) <- z.(j) ;
         y.(j) <- y.(j) +. twelfthstep *. ( 23. *. carrousel.(!pointeur).(j) -. 16. *. carrousel.( ( !pointeur + 2 ) mod 3 ).(j) +. 5. *. carrousel.( ( !pointeur + 1 ) mod 3 ).(j) ) ;
        done ;
      done ;
      y ;;

(** {v matrix_end_ode_adams_bashforth_3 function nsteps value beginning ending v} *)
let matrix_end_ode_adams_bashforth_3 = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and pointeur = ref 1
 and r = Array.length y0
 and c = Array.length y0.(0)
 and x = ref beginning in
  let halfstep = step *. 0.5
  and rr = r - 1
  and cc = c - 1
  and carrousel = [| Array.make_matrix r c 0. ; Array.make_matrix r c 0. ; Array.make_matrix r c 0. |]
  and y = Matrix.matrix_float_copy y0 in
   let twelfthstep = halfstep /. 6.
   and z = f beginning y0 in
    for j = 0 to rr do
     let row_0 = y.(j)
     and ligne_0 = carrousel.(0).(j)
     and row_0_input = z.(j) in
     for k = 0 to cc do
      ligne_0.(k) <- row_0_input.(k) ;
      row_0.(k) <- row_0.(k) +. step *. row_0_input.(k) ;
     done ;
    done ;
    x := !x +. step ;
    let zz = f !x y in
     for j = 0 to rr do
      let row_1 = y.(j)
      and row_1_input = zz.(j)
      and ligne_1 = carrousel.(1).(j)
      and ligne_1_entree = carrousel.(0).(j) in
      for k = 0 to cc do
       ligne_1.(k) <- row_1_input.(k) ;
       row_1.(k) <- row_1.(k) +. halfstep *. ( 3. *. row_1_input.(k) -. ligne_1_entree.(k) ) ;
      done ;
     done ;
     for i = 3 to nsteps do
      x := !x +. step ;
      pointeur := ( !pointeur + 1 ) mod 3 ;
      let zzz = f !x y in
       for j = 0 to rr do
        let term3 = carrousel.( ( !pointeur + 1 ) mod 3 ).(j)
        and term2 = carrousel.( ( !pointeur + 2 ) mod 3 ).(j)
        and term1 = carrousel.(!pointeur).(j)
        and row_input = zzz.(j)
        and row = y.(j) in
         for k = 0 to cc do
          term1.(k) <- row_input.(k) ;
          row.(k) <- row.(k) +. twelfthstep *. ( 23. *. row_input.(k) -. 16. *. term2.(k) +. 5. *. term3.(k) ) ;
         done ;
       done ;
     done ;
     y ;;



(** {v float_ode_adams_bashforth order function nsteps value beginning ending v} *)
let float_ode_adams_bashforth = fun (order:int) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 if log ( float nsteps ) < ( float order ) *. ( log 2. ) then failwith "The number of steps [nsteps] must be at least 2 ^ [order] in Infinitesimal.float_ode_adams_bashforth." ;
 let step = ( ending -. beginning ) /. ( float nsteps )
 and oo = order - 1
 and x = ref beginning
 and carrousel = Array.make order 0.
 and y = Array.make ( nsteps + 1 ) y0 in
  let w = adams_bashforth_m.(oo) in
   for i = 1 to oo do
    let ii = i - 1 in
     carrousel.(ii) <- f !x y.(ii) ;
     y.(i) <- y.(ii) +. step *. ( Matrix.vector_float_scal_prod ( adams_bashforth_m.(ii) ) ( Array.sub carrousel 0 i ) ) ;
     x := !x +. step ;
   done ;
   for i = order to nsteps do
    let ii = i - 1 in
     carrousel.(oo) <- f !x y.(ii) ;
     y.(i) <- y.(ii) +. step *. ( Matrix.vector_float_scal_prod w carrousel ) ;
     x := !x +. step ;
     for i = 0 to oo - 1 do
      carrousel.(i) <- carrousel.( i + 1 ) ;
     done ;
   done ;
   y ;;

(** {v vector_ode_adams_bashforth order function nsteps value beginning ending v} *)
let vector_ode_adams_bashforth = fun (order:int) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 if log ( float nsteps ) < ( float order ) *. ( log 2. ) then failwith "The number of steps [nsteps] must be at least 2 ^ [order] in Infinitesimal.vector_ode_adams_bashforth." ;
 let step = ( ending -. beginning ) /. ( float nsteps )
 and ii = ref 0
 and oo = order - 1
 and l = Array.length y0
 and x = ref beginning in
  let ll = l - 1
  and carrousel = Array.make_matrix order l 0.
  and y = Array.make_matrix ( nsteps + 1 ) l 0. in
   let w = adams_bashforth_m.(oo) in
    let row = y.(0) in
     for j = 0 to ll do
      row.(j) <- y0.(j) ;
     done ;
    for i = 1 to oo do
     ii := i - 1 ;
     let row_input = y.(!ii) in
      let z = f !x row_input
      and row = carrousel.(!ii) in
       for j = 0 to ll do
        row.(j) <- z.(j) ;
       done ;
      let row = y.(i)
      and ligne = adams_bashforth_m.(!ii) in
       for j = 0 to ll do
        row.(j) <- row_input.(j) ;
        for k = 0 to !ii do
         row.(j) <- row.(j) +. step *. ligne.(k) *. carrousel.(k).(j) ;
        done ;
       done ;
     x := !x +. step ;
    done ;
    for i = order to nsteps do
     ii := i - 1 ;
     let row_input = y.(!ii) in
      let z = f !x row_input
      and row = carrousel.(oo) in
       for j = 0 to ll do
        row.(j) <- z.(j) ;
       done ;
      let row = y.(i) in
       for j = 0 to ll do
        row.(j) <- row_input.(j) ;
        for k = 0 to oo do
         row.(j) <- row.(j) +. step *. w.(k) *. carrousel.(k).(j) ;
        done ;
       done ;
      x := !x +. step ;
      for i = 0 to oo - 1 do
       let row_output = carrousel.(i)
       and row_input = carrousel.( i + 1 ) in
        for j = 0 to ll do
         row_output.(j) <- row_input.(j) ;
        done ;
      done ;
    done ;
    y ;;

(** {v matrix_ode_adams_bashforth order function nsteps value beginning ending v} *)
let matrix_ode_adams_bashforth = fun (order:int) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 if log ( float nsteps ) < ( float order ) *. ( log 2. ) then failwith "The number of steps [nsteps] must be at least 2 ^ [order] in Infinitesimal.matrix_ode_adams_bashforth." ;
 let step = ( ending -. beginning ) /. ( float nsteps )
 and ii = ref 0
 and oo = order - 1
 and l = Array.length y0
 and c = Array.length y0.(0)
 and x = ref beginning in
  let ll = l - 1
  and carrousel = Array.map ( Array.make_matrix l c ) ( Array.make order 0. )
  and y = Array.map ( Array.make_matrix l c ) ( Array.make ( nsteps + 1 ) 0. )
  and w = adams_bashforth_m.(oo) in
    let row = y.(0) in
     for j = 0 to ll do
      row.(j) <- y0.(j) ;
     done ;
    for i = 1 to oo do
     ii := i - 1 ;
     let row_input = y.(!ii) in
      let z = f !x row_input
      and row = carrousel.(!ii) in
       for j = 0 to ll do
        row.(j) <- z.(j) ;
       done ;
      let row = y.(i)
      and ligne = adams_bashforth_m.(!ii) in
       for j = 0 to ll do
        row.(j) <- row_input.(j) ;
        for k = 0 to !ii do
         row.(j) <- Matrix.vector_float_plus row.(j) ( Matrix.vector_float_scal_mult ( step *. ligne.(k) ) carrousel.(k).(j) ) ;
        done ;
       done ;
       x := !x +. step ;
    done ;
    for i = order to nsteps do
     ii := i - 1 ;
     let row_input = y.(!ii) in
      let z = f !x row_input
      and row = carrousel.(oo) in
       for j = 0 to ll do
        row.(j) <- z.(j) ;
       done ;
       let row = y.(i) in
        for j = 0 to ll do
         row.(j) <- row_input.(j) ;
         for k = 0 to oo do
          row.(j) <- Matrix.vector_float_plus row.(j) ( Matrix.vector_float_scal_mult ( step *. w.(k) ) carrousel.(k).(j) ) ;
         done ;
        done ;
       x := !x +. step ;
       for i = 0 to oo - 1 do
        let row_output = carrousel.(i)
        and row_input = carrousel.( i + 1 ) in
         for j = 0 to ll do
          row_output.(j) <- row_input.(j) ;
         done ;
       done ;
    done ;
    y ;;


(** {v float_end_ode_adams_bashforth order function nsteps value beginning ending v} *)
let float_end_ode_adams_bashforth = fun (order:int) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 if log ( float nsteps ) < ( float order ) *. ( log 2. ) then failwith "The number of steps [nsteps] must be at least 2 ^ [order] in Infinitesimal.float_end_ode_adams_bashforth." ;
 let step = ( ending -. beginning ) /. ( float nsteps )
 and oo = order - 1
 and x = ref beginning
 and carrousel = Array.make order 0.
 and y = Array.make ( order + 1 ) y0 in
  let w = adams_bashforth_m.(oo) in
   for i = 1 to oo do
    let ii = i - 1 in
     carrousel.(ii) <- f !x y.(ii) ;
     y.(i) <- y.(ii) +. step *. ( Matrix.vector_float_scal_prod ( adams_bashforth_m.(ii) ) ( Array.sub carrousel 0 i ) ) ;
     x := !x +. step ;
   done ;
   for i = order to nsteps do
    carrousel.(oo) <- f !x y.(oo) ;
    y.(order) <- y.(oo) +. step *. ( Matrix.vector_float_scal_prod w carrousel ) ;
    x := !x +. step ;
    for i = 0 to oo - 1 do
     carrousel.(i) <- carrousel.( i + 1 ) ;
    done ;
    y.(oo) <- y.(order) ;
   done ;
   y.(order) ;;


(** {v vector_end_ode_adams_bashforth order function nsteps value beginning ending v} *)
let vector_end_ode_adams_bashforth = fun (order:int) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 if log ( float nsteps ) < ( float order ) *. ( log 2. ) then failwith "The number of steps [nsteps] must be at least 2 ^ [order] in Infinitesimal.vector_end_ode_adams_bashforth." ;
 let step = ( ending -. beginning ) /. ( float nsteps )
 and l = Array.length y0
 and oo = order - 1
 and ii = ref 0
 and x = ref beginning in
  let carrousel = Array.make_matrix order l 0.
  and ll = l - 1
  and y = Array.make_matrix ( order + 1 ) l 0.
  and w = adams_bashforth_m.(oo) in
   let row = y.(0) in
    for j = 0 to ll do
     row.(j) <- y0.(j) ;
    done ;
   for i = 1 to oo do
    ii := i - 1 ;
    let row = carrousel.(!ii)
    and row_input = y.(!ii) in
     let row_output = f !x row_input in
      for j = 0 to ll do
       row.(j) <- row_output.(j) ;
      done ;
     let row = y.(i)
     and ligne = adams_bashforth_m.(!ii) in
      for j = 0 to ll do
       row.(j) <- row_input.(j) ;
       for k = 0 to !ii do
        row.(j) <- row.(j) +. step *. ligne.(k) *. carrousel.(k).(j) ;
       done ;
      done ;
    x := !x +. step ;
   done ;
   for i = order to nsteps do
    let row = carrousel.(oo)
    and z = f !x y.(oo) in
     for j = 0 to ll do
      row.(j) <- z.(j) ;
     done ;
    let row = y.(order)
    and row_input = y.(oo) in
     for j = 0 to ll do
      row.(j) <- row_input.(j) ;
      for k = 0 to oo do
       row.(j) <- row.(j) +. step *. w.(k) *. carrousel.(k).(j) ;
      done ;
     done ;
    x := !x +. step ;
    for i = 0 to oo - 1 do
     let row_output = carrousel.(i)
     and row_input = carrousel.( i + 1 ) in
      for j = 0 to ll do
       row_output.(j) <- row_input.(j) ;
      done ;
    done ;
    let row_input = y.(order)
    and row_output = y.(oo) in
     for j = 0 to ll do
      row_output.(j) <- row_input.(j) ;
     done ;
   done ;
   y.(order) ;;


(** {v matrix_end_ode_adams_bashforth order function nsteps value beginning ending v} *)
let matrix_end_ode_adams_bashforth = fun (order:int) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 if log ( float nsteps ) < ( float order ) *. ( log 2. ) then failwith "The number of steps [nsteps] must be at least 2 ^ [order] in Infinitesimal.matrix_end_ode_adams_bashforth." ;
 let step = ( ending -. beginning ) /. ( float nsteps )
 and l = Array.length y0
 and c= Array.length y0.(0)
 and oo = order - 1
 and ii = ref 0
 and x = ref beginning in
  let carrousel = Array.map ( Array.make_matrix l c ) ( Array.make order 0. )
  and ll = l - 1
  and y = Array.map ( Array.make_matrix l c ) ( Array.make ( order + 1 ) 0. )
  and w = adams_bashforth_m.(oo) in
   let row = y.(0) in
    for j = 0 to ll do
     row.(j) <- y0.(j) ;
    done ;
   for i = 1 to oo do
    ii := i - 1 ;
    let row = carrousel.(!ii)
    and row_input = y.(!ii) in
     let row_output = f !x row_input in
      for j = 0 to ll do
       row.(j) <- row_output.(j) ;
      done ;
     let row = y.(i)
     and ligne = adams_bashforth_m.(!ii) in
      for j = 0 to ll do
       row.(j) <- row_input.(j) ;
       for k = 0 to !ii do
        row.(j) <- Matrix.vector_float_plus row.(j) ( Matrix.vector_float_scal_mult ( step *. ligne.(k) ) carrousel.(k).(j) ) ;
       done ;
      done ;
    x := !x +. step ;
   done ;
   for i = order to nsteps do
    let row = carrousel.(oo)
    and z = f !x y.(oo) in
     for j = 0 to ll do
      row.(j) <- z.(j) ;
     done ;
    let row = y.(order)
    and row_input = y.(oo) in
     for j = 0 to ll do
      row.(j) <- row_input.(j) ;
      for k = 0 to oo do
       row.(j) <- Matrix.vector_float_plus row.(j) ( Matrix.vector_float_scal_mult ( step *. w.(k) ) carrousel.(k).(j) ) ;
      done ;
     done ;
    x := !x +. step ;
    for i = 0 to oo - 1 do
     let row_output = carrousel.(i)
     and row_input = carrousel.( i + 1 ) in
      for j = 0 to ll do
       row_output.(j) <- row_input.(j) ;
      done ;
    done ;
    let row_input = y.(order)
    and row_output = y.(oo) in
     for j = 0 to ll do
      row_output.(j) <- row_input.(j) ;
     done ;
   done ;
   y.(order) ;;



(** {v float_ode_nystroem_3 function nsteps value beginning ending v} *)
let float_ode_nystroem_3 = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and carrousel = Array.make 3 0.
 and pointeur = ref 1
 and y = Array.make ( nsteps + 1 ) y0 in
  let thirdstep = step /. 3. in
   carrousel.(0) <- f beginning y0 ;
   y.(1) <- y0 +. step *. carrousel.(0) ;
   x := !x +. step ;
   carrousel.(1) <- f !x y.(1) ;
   y.(2) <- y0 +. step *. 2. *. carrousel.(1) ;
   for i = 3 to nsteps do
    pointeur := ( !pointeur + 1 ) mod 3 ;
    carrousel.(!pointeur) <- f !x y.( i - 1 ) ;
    x := !x +. step ;
    let z = y.( i - 2 ) in
     y.(i) <- z +. thirdstep *. ( 7. *. carrousel.(!pointeur) -. 2. *. carrousel.( ( !pointeur + 2 ) mod 3 ) +. carrousel.( ( !pointeur + 1 ) mod 3 ) ) ;
   done ;
   y ;;

(** {v vector_ode_nystroem_3 function nsteps value beginning ending v} *)
let vector_ode_nystroem_3 = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and l = Array.length y0
 and pointeur = ref 1 in
  let carrousel = Array.make_matrix 3 l 0.
  and y = Array.make_matrix ( nsteps + 1 ) l 0.
  and ll = l - 1
  and thirdstep = step /. 3. in
   let row_input = f beginning y0
   and row = carrousel.(0) in
   for j = 0 to ll do
    row.(j) <- row_input.(j) ;
   done ;
   y.(1) <- Matrix.vector_float_plus y0 ( Matrix.vector_float_scal_mult step carrousel.(0) ) ;
   x := !x +. step ;
   let row_input = f !x y.(1)
   and row = carrousel.(1) in
    for j = 0 to ll do
     row.(j) <- row_input.(j) ;
    done ;
   y.(2) <- Matrix.vector_float_plus y0 ( Matrix.vector_float_scal_mult ( step *. 2. ) carrousel.(1) ) ;
   for i = 3 to nsteps do
    pointeur := ( !pointeur + 1 ) mod 3 ;
    let row_input = f !x y.( i - 1 )
    and row = carrousel.(!pointeur) in
     for j = 0 to ll do
      row.(j) <- row_input.(j) ;
     done ;
    x := !x +. step ;
    let z = y.( i - 2 ) in
     y.(i) <- Matrix.vector_float_plus ( Matrix.vector_float_plus z ( Matrix.vector_float_scal_mult ( thirdstep *. 7. ) carrousel.(!pointeur) ) )
      ( ( Matrix.vector_float_plus ( Matrix.vector_float_scal_mult ( -. 2. *. thirdstep ) carrousel.( ( !pointeur + 2 ) mod 3 ) ) ( Matrix.vector_float_scal_mult thirdstep carrousel.( ( !pointeur + 1 ) mod 3 ) ) ) ) ;
   done ;
   y ;;

(** {v matrix_ode_nystroem_3 function nsteps value beginning ending v} *)
let matrix_ode_nystroem_3 = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and l = Array.length y0
 and c = Array.length y0.(0)
 and pointeur = ref 1 in
  let carrousel = Array.map ( Array.make_matrix l c ) ( Array.make 3 0. )
  and y = Array.map ( Array.make_matrix l c ) ( Array.make ( nsteps + 1 ) 0. )
  and ll = l - 1
  and thirdstep = step /. 3. in
   let row_input = f beginning y0
   and row = carrousel.(0) in
   for j = 0 to ll do
    row.(j) <- row_input.(j) ;
   done ;
   y.(1) <- Matrix.matrix_float_plus y0 ( Matrix.matrix_float_scal_mult step carrousel.(0) ) ;
   x := !x +. step ;
   let row_input = f !x y.(1)
   and row = carrousel.(1) in
    for j = 0 to ll do
     row.(j) <- row_input.(j) ;
    done ;
   y.(2) <- Matrix.matrix_float_plus y0 ( Matrix.matrix_float_scal_mult ( step *. 2. ) carrousel.(1) ) ;
   for i = 3 to nsteps do
    pointeur := ( !pointeur + 1 ) mod 3 ;
    let row_input = f !x y.( i - 1 )
    and row = carrousel.(!pointeur) in
     for j = 0 to ll do
      row.(j) <- row_input.(j) ;
     done ;
    x := !x +. step ;
    let z = y.( i - 2 ) in
     y.(i) <- Matrix.matrix_float_plus ( Matrix.matrix_float_plus z ( Matrix.matrix_float_scal_mult ( thirdstep *. 7. ) carrousel.(!pointeur) ) )
      ( ( Matrix.matrix_float_plus ( Matrix.matrix_float_scal_mult ( -. 2. *. thirdstep ) carrousel.( ( !pointeur + 2 ) mod 3 ) ) ( Matrix.matrix_float_scal_mult thirdstep carrousel.( ( !pointeur + 1 ) mod 3 ) ) ) ) ;
   done ;
   y ;;



(** {v float_end_ode_nystroem_3 function nsteps value beginning ending v} *)
let float_end_ode_nystroem_3 = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and carrousel = Array.make 3 0.
 and pointeur = ref 1
 and y = ref y0
 and yy = ref y0
 and z = ref y0 in
  let thirdstep = step /. 3. in
   carrousel.(0) <- f beginning y0 ;
   yy := y0 +. step *. carrousel.(0) ;
   x := !x +. step ;
   carrousel.(1) <- f !x !yy ;
   y := y0 +. step *. 2. *. carrousel.(1) ;
   for i = 3 to nsteps do
    x := !x +. step ;
    pointeur := ( !pointeur + 1 ) mod 3 ;
    carrousel.(!pointeur) <- f !x !y ;
    z := !yy +. thirdstep *. ( 7. *. carrousel.(!pointeur) -. 2. *. carrousel.( ( !pointeur + 2 ) mod 3 ) +. carrousel.( ( !pointeur + 1 ) mod 3 ) ) ;
    yy := !y ;
    y := !z ;
   done ;
   !y ;;

(** {v vector_end_ode_nystroem_3 function nsteps value beginning ending v} *)
let vector_end_ode_nystroem_3 = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and l = Array.length y0
 and pointeur = ref 1 in
  let carrousel = Array.make_matrix 3 l 0.
  and ll = l - 1
  and y = Array.make l 0.
  and yy = Array.make l 0.
  and z = Array.make l 0.
  and thirdstep = step /. 3. in
   let row = carrousel.(0)
   and row_input = f beginning y0 in
    for j = 0 to ll do
     row.(j) <- row_input.(j) ;
     yy.(j) <- y0.(j) +. step *. row.(j) ;
    done ;
   x := !x +. step ;
   let row = carrousel.(1)
   and row_input = f !x yy in
    for j = 0 to ll do
     row.(j) <- row_input.(j) ;
     y.(j) <- y0.(j) +. step *. 2. *. row.(j) ;
    done ;
   for i = 3 to nsteps do
    x := !x +. step ;
    pointeur := ( !pointeur + 1 ) mod 3 ;
    let row = carrousel.(!pointeur)
    and row_input = f !x y in
     for j = 0 to ll do
      row.(j) <- row_input.(j) ;
      z.(j) <- yy.(j) +. thirdstep *. ( 7. *. row.(j) -. 2. *. carrousel.( ( !pointeur + 2 ) mod 3 ).(j) +. carrousel.( ( !pointeur + 1 ) mod 3 ).(j) ) ;
     done ;
    for j = 0 to ll do
     yy.(j) <- y.(j) ;
     y.(j) <- z.(j) ;
    done ;
   done ;
   y ;;

(** {v matrix_end_ode_nystroem_3 function nsteps value beginning ending v} *)
let matrix_end_ode_nystroem_3 = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and l = Array.length y0
 and c = Array.length y0.(0)
 and pointeur = ref 1 in
  let carrousel = Array.map ( Array.make_matrix l c ) ( Array.make 3 0. )
  and ll = l - 1
  and y = Array.make_matrix l c 0.
  and yy = Array.make_matrix l c 0.
  and z = Array.make_matrix l c 0.
  and thirdstep = step /. 3. in
   let row = carrousel.(0)
   and row_input = f beginning y0 in
    for j = 0 to ll do
     row.(j) <- row_input.(j) ;
     yy.(j) <- Matrix.vector_float_plus y0.(j) ( Matrix.vector_float_scal_mult step row.(j) ) ;
    done ;
   x := !x +. step ;
   let row = carrousel.(1)
   and row_input = f !x yy in
    for j = 0 to ll do
     row.(j) <- row_input.(j) ;
     y.(j) <- Matrix.vector_float_plus y0.(j) ( Matrix.vector_float_scal_mult ( step *. 2. ) row.(j) ) ;
    done ;
   for i = 3 to nsteps do
    x := !x +. step ;
    pointeur := ( !pointeur + 1 ) mod 3 ;
    let row = carrousel.(!pointeur)
    and row_input = f !x y in
     for j = 0 to ll do
      row.(j) <- row_input.(j) ;
      z.(j) <- Matrix.vector_float_plus ( Matrix.vector_float_plus yy.(j) ( Matrix.vector_float_scal_mult ( thirdstep *. 7. ) row.(j) ) )
                                        ( Matrix.vector_float_plus ( Matrix.vector_float_scal_mult ( -2. *. thirdstep ) carrousel.( ( !pointeur + 2 ) mod 3 ).(j) )
                                        ( Matrix.vector_float_scal_mult thirdstep carrousel.( ( !pointeur + 1 ) mod 3 ).(j) ) ) ;
     done ;
    for j = 0 to ll do
     yy.(j) <- y.(j) ;
     y.(j) <- z.(j) ;
    done ;
   done ;
   y ;;



(** {v float_ode_mid_point function nsteps value beginning ending v} *)
let float_ode_mid_point = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and z = ref y0
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5 in
   for i = 1 to nsteps do
    z := !z +. halfstep *. ( f !x !z ) ;
    xx := !x +. halfstep ;
    z := y.( i - 1 ) +. step *. ( f !xx !z ) ;
    x := !x +. step ;
    y.(i) <- !z ;
   done ;
   y ;;

(** {v vector_ode_mid_point function nsteps value beginning ending v} *)
let vector_ode_mid_point = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and l = Array.length y0 in
  let y = Array.make_matrix ( nsteps + 1 ) l 0.
  and ll = l - 1
  and z = Array.make l 0.
  and halfstep = step *. 0.5 in
   let row = y.(0) in
    for j = 0 to ll do
     let zz = y0.(j) in
      z.(j) <- zz ;
      row.(j) <- zz ;
    done ;
   for i = 1 to nsteps do
    let row = f !x z in
     for j = 0 to ll do
      z.(j) <- z.(j) +. halfstep *. row.(j) ;
     done ;
    xx := !x +. halfstep ;
    let row = y.( i - 1 )
    and row_output = y.(i)
    and row_input = f !xx z in
     for j = 0 to ll do
      z.(j) <- row.(j) +. step *. row_input.(j) ;
      row_output.(j) <- z.(j) ;
     done ;
    x := !x +. step ;
   done ;
   y ;;

(** {v matrix_ode_mid_point function nsteps value beginning ending v} *)
let matrix_ode_mid_point = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and l = Array.length y0
 and c = Array.length y0.(0) in
  let y = Array.map ( Array.make_matrix l c ) ( Array.make ( nsteps + 1 ) 0. )
  and ll = l - 1
  and z = Array.make_matrix l c 0.
  and halfstep = step *. 0.5 in
   let row = y.(0) in
    for j = 0 to ll do
     let zz = y0.(j) in
      z.(j) <- zz ;
      row.(j) <- zz ;
    done ;
   for i = 1 to nsteps do
    let row = f !x z in
     for j = 0 to ll do
      z.(j) <- Matrix.vector_float_plus z.(j) ( Matrix.vector_float_scal_mult halfstep row.(j) ) ;
     done ;
    xx := !x +. halfstep ;
    let row = y.( i - 1 )
    and row_output = y.(i)
    and row_input = f !xx z in
     for j = 0 to ll do
      z.(j) <- Matrix.vector_float_plus row.(j) ( Matrix.vector_float_scal_mult step row_input.(j) ) ;
      row_output.(j) <- z.(j) ;
     done ;
    x := !x +. step ;
   done ;
   y ;;


(** {v float_end_ode_mid_point function nsteps value beginning ending v} *)
let float_end_ode_mid_point = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and y = ref y0
 and z = ref y0 in
  let halfstep = step *. 0.5 in
   for i = 1 to nsteps do
    z := !y +. halfstep *. ( f !x !y ) ;
    xx := !x +. halfstep ;
    y := !y +. step *. ( f !xx !z ) ;
    x := !x +. step ;
   done ;
   !y ;;

(** {v vector_end_ode_mid_point function nsteps value beginning ending v} *)
let vector_end_ode_mid_point = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and y = Matrix.vector_float_copy y0
 and l = Array.length y0
 and z = Matrix.vector_float_copy y0 in
  let halfstep = step *. 0.5
  and ll = l - 1 in
   for i = 1 to nsteps do
    let row = f !x y in
     for j = 0 to ll do
      z.(j) <- y.(j) +. halfstep *. row.(j) ;
     done ;
    xx := !x +. halfstep ;
    let row = f !xx z in
     for j = 0 to ll do
      y.(j) <- y.(j) +. step *. row.(j) ;
     done ;
    x := !x +. step ;
   done ;
   y ;;

(** {v matrix_end_ode_mid_point function nsteps value beginning ending v} *)
let matrix_end_ode_mid_point = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and y = Matrix.matrix_float_copy y0
 and l = Array.length y0
 and z = Matrix.matrix_float_copy y0 in
  let halfstep = step *. 0.5
  and ll = l - 1 in
   for i = 1 to nsteps do
    let row = f !x y in
     for j = 0 to ll do
      z.(j) <- Matrix.vector_float_plus y.(j) ( Matrix.vector_float_scal_mult halfstep row.(j) ) ;
     done ;
    xx := !x +. halfstep ;
    let row = f !xx z in
     for j = 0 to ll do
      y.(j) <- Matrix.vector_float_plus y.(j) ( Matrix.vector_float_scal_mult step row.(j) ) ;
     done ;
    x := !x +. step ;
   done ;
   y ;;



(** {v float_ode_rk4 function nsteps value beginning ending v} *)
let float_ode_rk4 = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5 in
   for i = 1 to nsteps do
    let z = y.(i - 1) in
     let k1 = f !x z
     and xx = !x +. halfstep in
      let k2 = f xx ( z +. halfstep *. k1 ) in
       let k3 = f xx ( z +. halfstep *. k2 ) in
        x := !x +. step ;
        let k4 = f !x ( z +. step *. k3 ) in
        y.(i) <- z +. ( halfstep *. ( k1 +. k4 ) +. step *. ( k2 +. k3 ) ) /. 3. ;
   done ;
   y ;;

(** {v vector_ode_rk4 function nsteps value beginning ending v} *)
let vector_ode_rk4 = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and l = Array.length y0 in
  let y = Array.make_matrix ( nsteps + 1 ) l 0.
  and ll = l - 1
  and halfstep = step *. 0.5
  and thirdstep = step /. 3. in
   let sixthstep = thirdstep *. 0.5 in
    let row = y.(0) in
     for j = 0 to ll do
      row.(j) <- y0.(j) ;
     done ;
    for i = 1 to nsteps do
     let z = y.(i - 1) in
      let k1 = f !x z
      and xx = !x +. halfstep in
       let k2 = f xx ( Matrix.vector_float_plus z ( Matrix.vector_float_scal_mult halfstep k1 ) ) in
        let k3 = f xx ( Matrix.vector_float_plus z ( Matrix.vector_float_scal_mult halfstep k2 ) ) in
         x := !x +. step ;
         let k4 = f !x ( Matrix.vector_float_plus z ( Matrix.vector_float_scal_mult step k3 ) ) in
         y.(i) <- Matrix.vector_float_plus z ( Matrix.vector_float_plus ( Matrix.vector_float_scal_mult sixthstep ( Matrix.vector_float_plus k1 k4 ) )
                                                                        ( Matrix.vector_float_scal_mult thirdstep ( Matrix.vector_float_plus k2 k3 ) ) ) ;
   done ;
   y ;;

(** {v matrix_ode_rk4 function nsteps value beginning ending v} *)
let matrix_ode_rk4 = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and l = Array.length y0
 and c = Array.length y0.(0) in
  let y = Array.map ( Array.make_matrix l c ) ( Array.make ( nsteps + 1 ) 0. )
  and ll = l - 1
  and halfstep = step *. 0.5
  and thirdstep = step /. 3. in
   let sixthstep = thirdstep *. 0.5 in
    let row = y.(0) in
     for j = 0 to ll do
      row.(j) <- y0.(j) ;
     done ;
    for i = 1 to nsteps do
     let z = y.(i - 1) in
      let k1 = f !x z
      and xx = !x +. halfstep in
       let k2 = f xx ( Matrix.matrix_float_plus z ( Matrix.matrix_float_scal_mult halfstep k1 ) ) in
        let k3 = f xx ( Matrix.matrix_float_plus z ( Matrix.matrix_float_scal_mult halfstep k2 ) ) in
         x := !x +. step ;
         let k4 = f !x ( Matrix.matrix_float_plus z ( Matrix.matrix_float_scal_mult step k3 ) ) in
         y.(i) <- Matrix.matrix_float_plus z ( Matrix.matrix_float_plus ( Matrix.matrix_float_scal_mult sixthstep ( Matrix.matrix_float_plus k1 k4 ) )
                                                                        ( Matrix.matrix_float_scal_mult thirdstep ( Matrix.matrix_float_plus k2 k3 ) ) ) ;
    done ;
    y ;;


(** {v float_end_ode_rk4 function nsteps value beginning ending v} *)
let float_end_ode_rk4 = fun (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0
 and z = ref y0 in
  let halfstep = step *. 0.5 in
   for i = 1 to nsteps do
    let k1 = f !x !y
    and xx = !x +. halfstep in
     let k2 = f xx ( !y +. halfstep *. k1 ) in
      let k3 = f xx ( !y +. halfstep *. k2 ) in
       x := !x +. step ;
       let k4 = f !x ( !y +. step *. k3 ) in
        z := !y +. ( halfstep *. ( k1 +. k4 ) +. step *. ( k2 +. k3 ) ) /. 3. ;
        y := !z ;
   done ;
   !y ;;

(** {v vector_end_ode_rk4 function nsteps value beginning ending v} *)
let vector_end_ode_rk4 = fun (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and l = Array.length y0
 and y = Matrix.vector_float_copy y0 in
  let z = Array.make l 0.
  and ll = l - 1
  and halfstep = step *. 0.5 in
   for i = 1 to nsteps do
    let k1 = f !x y
    and xx = !x +. halfstep in
     let k2 = f xx ( Matrix.vector_float_plus y ( Matrix.vector_float_scal_mult halfstep k1 ) ) in
      let k3 = f xx ( Matrix.vector_float_plus y ( Matrix.vector_float_scal_mult halfstep k2 ) ) in
       x := !x +. step ;
       let k4 = f !x ( Matrix.vector_float_plus y ( Matrix.vector_float_scal_mult step k3 ) ) in
        for j = 0 to ll do
         z.(j) <- y.(j) +. ( halfstep *. ( k1.(j) +. k4.(j) ) +. step *. ( k2.(j) +. k3.(j) ) ) /. 3. ;
         y.(j) <- z.(j) ;
        done ;
   done ;
   y ;;

(** {v matrix_end_ode_rk4 function nsteps value beginning ending v} *)
let matrix_end_ode_rk4 = fun (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and l = Array.length y0
 and c = Array.length y0.(0)
 and y = Matrix.matrix_float_copy y0 in
  let z = Array.make_matrix l c 0.
  and ll = l - 1
  and thirdstep = step /. 3.
  and halfstep = step *. 0.5 in
   let sixthstep = thirdstep *. 0.5 in
    for i = 1 to nsteps do
     let k1 = f !x y
     and xx = !x +. halfstep in
      let k2 = f xx ( Matrix.matrix_float_plus y ( Matrix.matrix_float_scal_mult halfstep k1 ) ) in
       let k3 = f xx ( Matrix.matrix_float_plus y ( Matrix.matrix_float_scal_mult halfstep k2 ) ) in
        x := !x +. step ;
        let k4 = f !x ( Matrix.matrix_float_plus y ( Matrix.matrix_float_scal_mult step k3 ) ) in
         for j = 0 to ll do
          z.(j) <- Matrix.vector_float_plus y.(j) 
                                            ( Matrix.vector_float_plus ( Matrix.vector_float_scal_mult sixthstep ( Matrix.vector_float_plus k1.(j) k4.(j) ) )
                                                                       ( Matrix.vector_float_scal_mult thirdstep ( Matrix.vector_float_plus k2.(j) k3.(j) ) ) ) ;
          y.(j) <- z.(j) ;
         done ;
    done ;
    y ;;



(** {v float_ode_runge_kutta butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let float_ode_runge_kutta = fun (a:float array array) (b:float array) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and y = Array.make ( nsteps + 1 ) y0 in
  let c = Array.make l 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = Array.make ( l + 1 ) 0. in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
     xx := x.(0) ;
     z := y.( i - 1 ) ;
     k.(0) <- f !xx !z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      yy.(j) <- !z ;
      let row = a.(j) in
       for index = 0 to j do
        zz := !zz +. row.(index) *. k.(index) ;
       done ;
       yy.(j) <- yy.(j) +. step *. !zz ;
       zz := 0. ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     for j = 0 to l do
      zz := !zz +. b.(j) *. k.(j) ;
     done ;
     y.(i) <- !z +. step *. !zz ;
     zz := 0. ;
   done ;
   y ;;


(** {v vector_ode_runge_kutta butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let vector_ode_runge_kutta = fun (a:float array array) (b:float array) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and dim = Array.length y0 in
  let c = Array.make l 0.
  and z = Matrix.vector_float_copy y0
  and zz = Array.make dim 0.
  and y = Array.make_matrix ( nsteps + 1 ) dim 0.
  and ll = l - 1
  and dd = dim - 1
  and x = Array.make l beginning
  and yy = Array.make_matrix l dim 0.
  and k = Array.make_matrix ( l + 1 ) dim 0. in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   let row = y.(0) in
    for j = 0 to dd do
     row.(j) <- y0.(j)
    done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    let row = y.( i - 1 )
    and ligne = k.(0) in
     let image = f !xx row in
      for j = 0 to dd do
       z.(j) <- row.(j) ;
       ligne.(j) <- image.(j)
      done ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- z.(indice) ;
       done ;
      let row = a.(j) in
       for index = 0 to j do
        let coeff = row.(index)
        and ligne = k.(index) in
         for indice = 0 to dd do
          zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
         done ;
       done ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- row.(indice) +. step *. zz.(indice) ;
       done ;
      let row = k.( j + 1 )
      and image = f x.(j) yy.(j) in
       for indice = 0 to dd do
        zz.(indice) <- 0. ;
        row.(indice) <- image.(indice) ;
       done ;
     done ;
     x.(0) <- !xx +. step ;
     for j = 0 to l do
      let row = k.(j)
      and coeff = b.(j) in
       for indice = 0 to dd do
        zz.(indice) <- zz.(indice) +. coeff *. row.(indice) ;
       done ;
     done ;
     let row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) +. step *. zz.(indice) ;
       zz.(indice) <- 0. ;
      done ;
   done ;
   y ;;


(** {v matrix_ode_runge_kutta butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let matrix_ode_runge_kutta = fun (a:float array array) (b:float array) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and dim_r = Array.length y0
 and dim_c = Array.length y0.(0) in
  let c = Array.make l 0.
  and z = Matrix.matrix_float_copy y0
  and zz = Array.make_matrix dim_r dim_c 0.
  and y = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( nsteps + 1 ) 0. )
  and ll = l - 1
  and dd = dim_r - 1
  and cc = dim_c - 1
  and x = Array.make l beginning
  and yy = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make l 0. )
  and k = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( l + 1 ) 0. ) in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   let row = y.(0) in
    for j = 0 to dd do
     row.(j) <- Matrix.vector_float_copy y0.(j)
    done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    let row = y.( i - 1 )
    and ligne = k.(0) in
     let image = f !xx row in
      for j = 0 to dd do
       z.(j) <- row.(j) ;
       ligne.(j) <- image.(j)
      done ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- Matrix.vector_float_copy z.(indice) ;
       done ;
      let row = a.(j) in
       for index = 0 to j do
        let coeff = row.(index)
        and ligne = k.(index) in
         for indice = 0 to dd do
          zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
         done ;
       done ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- Matrix.vector_float_plus row.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
       done ;
      let row = k.( j + 1 )
      and image = f x.(j) yy.(j) in
       for indice = 0 to dd do
        let ligne = zz.(indice) in
         for numero = 0 to cc do
          ligne.(numero) <- 0. ;
         done ;
        row.(indice) <- image.(indice) ;
       done ;
     done ;
     x.(0) <- !xx +. step ;
     for j = 0 to l do
      let row = k.(j)
      and coeff = b.(j) in
       for indice = 0 to dd do
        zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff row.(indice) ) ;
       done ;
     done ;
     let row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- Matrix.vector_float_plus z.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
       let row = zz.(indice) in
        for numero = 0 to cc do
         row.(numero) <- 0. ;
        done ;
      done ;
   done ;
   y ;;



(** {v float_end_ode_runge_kutta butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let float_end_ode_runge_kutta = fun (a:float array array) (b:float array) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and y = ref y0 in
  let c = Array.make l 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = Array.make ( l + 1 ) 0. in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    z := !y ;
    k.(0) <- f !xx !z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     yy.(j) <- !z ;
     let row = a.(j) in
      for index = 0 to j do
       zz := !zz +. row.(index) *. k.(index) ;
      done ;
      yy.(j) <- yy.(j) +. step *. !zz ;
      zz := 0. ;
      k.( j + 1 ) <- f x.(j) yy.(j) ;
    done ;
    x.(0) <- !xx +. step ;
    for j = 0 to l do
     zz := !zz +. b.(j) *. k.(j) ;
    done ;
    y := !z +. step *. !zz ;
    zz := 0. ;
   done ;
   !y ;;

(** {v vector_end_ode_runge_kutta butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let vector_end_ode_runge_kutta = fun (a:float array array) (b:float array) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and dim = Array.length y0
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = Matrix.vector_float_copy y0
 and y = Matrix.vector_float_copy y0 in
  let c = Array.make l 0.
  and zz = Array.make dim 0.
  and ll = l - 1
  and dd = dim - 1
  and x = Array.make l beginning
  and yy = Array.make_matrix l dim 0.
  and k = Array.make_matrix ( l + 1 ) dim 0. in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    for indice = 0 to dd do
     z.(indice) <- y.(indice) ;
    done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     let yyy = yy.(j) in
      for indice = 0 to dd do
       yyy.(indice) <- z.(indice) ;
      done ;
     let row = a.(j) in
      for index = 0 to j do
       let ligne = k.(index)
       and coeff = row.(index) in
        for indice = 0 to dd do
         zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
        done ;
      done ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- row.(indice) +. step *. zz.(indice) ;
        zz.(indice) <- 0. ;
       done ;
       k.( j + 1 ) <- f x.(j) row ;
     done ;
     x.(0) <- !xx +. step ;
     for j = 0 to l do
      let row = k.(j)
      and coeff = b.(j) in
       for indice = 0 to dd do
        zz.(indice) <- zz.(indice) +. coeff *. row.(indice) ;
       done ;
     done ;
     for indice = 0 to dd do
      y.(indice) <- z.(indice) +. step *. zz.(indice) ;
      zz.(indice) <- 0. ;
     done ;
   done ;
   y ;;

(** {v matrix_end_ode_runge_kutta butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let matrix_end_ode_runge_kutta = fun (a:float array array) (b:float array) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and dim_r = Array.length y0
 and dim_c = Array.length y0.(0)
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = Matrix.matrix_float_copy y0
 and y = Matrix.matrix_float_copy y0 in
  let c = Array.make l 0.
  and zz = Array.make_matrix dim_r dim_c 0.
  and ll = l - 1
  and dd = dim_r - 1
  and cc = dim_c - 1
  and x = Array.make l beginning
  and yy = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make l 0. )
  and k = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( l + 1 ) 0. ) in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    for indice = 0 to dd do
     z.(indice) <- y.(indice) ;
    done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     let yyy = yy.(j) in
      for indice = 0 to dd do
       yyy.(indice) <- z.(indice) ;
      done ;
     let row = a.(j) in
      for index = 0 to j do
       let ligne = k.(index)
       and coeff = row.(index) in
        for indice = 0 to dd do
         zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
        done ;
      done ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- Matrix.vector_float_plus row.(indice) ( Matrix.vector_float_scal_mult  step zz.(indice) ) ;
        let ligne = zz.(indice) in
         for numero = 0 to cc do
          ligne.(numero) <- 0. ;
         done ;
       done ;
       k.( j + 1 ) <- f x.(j) row ;
     done ;
     x.(0) <- !xx +. step ;
     for j = 0 to l do
      let row = k.(j)
      and coeff = b.(j) in
       for indice = 0 to dd do
        zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff row.(indice) ) ;
       done ;
     done ;
     for indice = 0 to dd do
      y.(indice) <- Matrix.vector_float_plus z.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
      let ligne = zz.(indice) in
       for numero = 0 to cc do
        ligne.(numero) <- 0. ;
       done ;
     done ;
   done ;
   y ;;



(** {C § } *)


(** Some Butcher tableaus are following. They have been harvested on the internet.
The square matrix must be lower triangular in the strict sense.
The tableaus of the explicit adaptative methods quoted further may also be used.

Suivent quelques tableaux de Butcher. Ils ont été glanés sur internet. 
La matrice carrée doit être triangulaire inférieure au sens strict.
Les tableaux des méthodes adaptatives explicites cités plus loin peuvent aussi être exploités. 

{C  } *)

let euler_a = ([| [| |] |]:float array array) ;;
(** *)
let euler_b = [| 1. |] ;;

let mid_point_a = [| [| 0.5 |] |] ;;
(** *)
let mid_point_b = [| 0. ; 1. |] ;;

let imex_ssp2_2_2_2_a = [| [| 1. |] |] ;;
(** The previous method is also called Lobatto III for s = 2.

La méthode ci-dessus est aussi appelée Lobatto III pour s = 2. *)
let imex_ssp2_2_2_2_b = [| 0.5 ; 0.5 |] ;;

let heun_a = [| [| 1. /. 3. |] ; [| 0. ; 2. /. 3. |] |] ;;
(** *)
let heun_b = [| 0.25 ; 0. ; 0.75 |] ;;

let rk2_runge_a = [| [| 1. |] ; [| 0. ; 1. |] |] ;;
(** *)
let rk2_runge_b = [| 0.5 ; 0. ; 0.5 |] ;;

let imex_ssp2_3_2_2_a = [| [| 0. |] ; [| 0. ; 1. |] |] ;;
(** *)
let imex_ssp2_3_2_2_b = [| 0. ; 0.5 ; 0.5 |] ;;

let imex_ssp2_3_3_2_a = [| [| 0.5 |] ; [| 0.5 ; 0.5 |] |] ;;
(** *)
let imex_ssp2_3_3_2_b = [| 1. /. 3. ; 1. /. 3. ; 1. /. 3. |] ;;

let imex_ssp3_3_3_2_a = [| [| 1. |] ; [| 0.25 ; 0.25 |] |] ;;
(** *)
let imex_ssp3_3_3_2_b = [| 1. /. 6. ; 1. /. 6. ; 2. /. 3. |] ;;

let example_a = [| [| 2. /. 3. |] ; [| -1. ; 1. |] |] ;;
(** *)
let example_b = [| 0. ; 0.75 ; 0.25 |] ;;

let imex_ssp3_4_3_3_a = [| [| 0. |] ; [| 0. ; 1. |] ; [| 0. ; 0.25 ; 0.25 |] |] ;;
(** *)
let imex_ssp3_4_3_3_b = [| 0. ; 1. /. 6. ; 1. /. 6. ; 2. /. 3. |] ;;

let rk4_a = [| [| 0.5 |] ; [| 0. ; 0.5 |] ; [| 0. ; 0. ; 1. |] |] ;;
(** *)
let rk4_b = [| 1./.6. ; 1. /. 3. ; 1. /. 3. ; 1. /. 6. |] ;;

let rk4_3_8_a = [| [| 1. /. 3. |] ; [| -1. /. 3. ; 1. |] ; [| 1. ; -1. ; 1. |] |] ;;
(** *)
let rk4_3_8_b = [| 1. /. 8. ; 3. /. 8. ; 3. /. 8. ; 1. /. 8. |] ;;

let hem_4_5_a = [| [| 0.3 |] ; [| ( 1. +. sqrt_of_6 ) /. 30. ; ( 11. -. 4. *. sqrt_of_6 ) /. 30. |] ;
 [| (-79. -. 31. *. sqrt_of_6 ) /. 150. ; ( -1. -. 4. *. sqrt_of_6 ) /. 30. ; ( 24. +. 11. *. sqrt_of_6 ) /. 25. |] ;
 [| ( 14. +. 5. *. sqrt_of_6 ) /. 6. ; ( 7. *. sqrt_of_6 -. 8. ) /. 6. ; ( -9. -. 7. *. sqrt_of_6 ) /. 4. ; ( 9. -. sqrt_of_6 ) /. 4. |] |] ;;
(** *)
let hem_4_5_b = [| 0. ; 0. ; ( 16. -. sqrt_of_6 ) /. 36. ; ( 16. +. sqrt_of_6 ) /. 36. ; 1. /. 9. |] ;;

let rk5_kutta_first_a = [| [| 0.2 |] ; [| 0. ; 0.4 |] ; [| 2.25 ; -5. ; 3.75 |] ; [| -0.63 ; 1.8 ; -0.65 ; 0.08 |] ; [| -0.24 ; 0.8 ; 2. /. 15. ; 8. /. 75. ; 0. |] |] ;;
(** *)
let rk5_kutta_first_b = [| 17. /. 144. ; 0. ; 100. /. 144. ; 2. /. 144. ; -50. /. 144. ; 75. /. 144. |] ;;

let rk5_kutta_second_a = [| [| 1. /. 3. |] ; [| 0.16 ; 0.24 |] ; [| 0.25 ; -3. ; 3.75 |] ; [| 2. /. 27. ; 10. /. 9. ; -50. /. 81. ; 8. /. 81. |] ;
 [| 0.08 ; 0.48 ; 2. /. 15. ; 8. /. 75. ; 0. |] |] ;;
(** *)
let rk5_kutta_second_b = [| 23. /. 192. ; 0. ; 125. /. 192. ; 0. ; -27. /. 64. ; 125. /. 192. |] ;;

let rk5_cassity_a = [| [| 1. /. 7. |] ; [| -367. /. 4088. ; 261. /. 584. |] ; [| 41991. /. 2044. ; -2493. /. 73. ; 57. /. 4. |] ;
 [| -108413. /. 196224. ; 58865. /. 65408. ; 5. /. 16. ; 265. /. 1344. |] ; [| -204419. /. 58984. ; 143829. /. 58984. ; 171. /. 202. ; 2205. /. 404. ; -432. /. 101. |] |] ;;
(** *)
let rk5_cassity_b = [| 1. /. 9. ; 7. /. 2700. ; 413. /. 810. ; 7. /. 450. ; 28. /. 75. ; -101. /. 8100. |] ;;

let rk6_butcher_a = [| [| 0.5 -. sqrt_of_5 /. 10. |] ; [| -. sqrt_of_5 /. 10. ; 0.5 +. sqrt_of_5 /. 5. |] ;
 [| ( -15. +. 7. *. sqrt_of_5 ) /. 20. ; -0.25 +. sqrt_of_5 /. 4. ; ( 15. -. 7. *. sqrt_of_5 ) /. 10. |] ;
 [| ( 5. -. sqrt_of_5 ) /. 60. ; 0. ; 1. /. 6. ; ( 15. -. 7. *. sqrt_of_5 ) /. 60. |] ;
 [| ( 5. -. sqrt_of_5 ) /. 60. ; 0. ; ( 9. -. 5. *. sqrt_of_5 ) /. 12. ; 1. /. 6. ; -0.5 +. 0.3 *. sqrt_of_5 |] ;
 [| 1. /. 6. ; 0. ; ( -55. +. 25. *. sqrt_of_5 ) /. 12. ; ( -25. -. 7. *. sqrt_of_5 ) /. 12. ; 5. -. 2. *. sqrt_of_5 ; 2.5 +. 0.5 *. sqrt_of_5 |] |] ;;
(** *)
let rk6_butcher_b = [| 1. /. 12. ; 0. ; 0. ; 0. ; 5. /. 12. ; 5. /. 12. ; 1. /. 12. |] ;;




(** {C § } *)
(** 
{2 Méthodes implicites avec résolution unidimensionnelle}
{2 Implicit Methods with unidimensional resolution}
*)
(** {C  } *)




(** {v float_ode_back_euler methode function nsteps value beginning ending v} The method must contain the solving method, included
the method of derivation and all the parameters, as in the following example.

{v  y' = f(x,y) float_zero_householder ( float_richardson_deriv 3. 4 1e-3 ) 3 100 v}

La méthode doit contenir la méthode de résolution, y compris la méthode de dérivation et tous les paramètres, 
comme dans l'exemple ci-dessus.
*)
let float_ode_back_euler = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  for i = 1 to nsteps do
   x := !x +. step ;
   let z = y.(i - 1) in
    let g = function t -> z +. step *. ( f !x t ) -. t in
     y.(i) <- methode g z ;
  done ;
  y ;;


(** {v float_end_ode_back_euler methode function nsteps value beginning ending v} The method must contain the solving method, included
the method of derivation and all the parameters, as in the following example.

{v  y' = f(x,y) float_zero_householder ( float_richardson_deriv 3. 4 1e-3 ) 3 100 v}

La méthode doit contenir la méthode de résolution, y compris la méthode de dérivation et tous les paramètres, 
comme dans l'exemple ci-dessus.
*)
let float_end_ode_back_euler = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0 in
  for i = 1 to nsteps do
   x := !x +. step ;
   let g = function t -> !y +. step *. ( f !x t ) -. t in
    y := methode g !y ;
  done ;
  !y ;;


(** {v float_ode_trapezoid methode function nsteps value beginning ending v} The method must contain the solving method, included
the method of derivation and all the parameters, as in the following example.

{v float_zero_householder ( float_richardson_deriv 3. 4 1e-3 ) 3 100 v}

La méthode doit contenir la méthode de résolution, y compris la méthode de dérivation et tous les paramètres, 
comme dans l'exemple ci-dessus.
*)
let float_ode_trapezoid = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  let halfstep = step *. 0.5 in
   for i = 1 to nsteps do
    let z = y.(i - 1) in
     let zz = f !x z in
      x := !x +. step ;
      let g = function t -> z +. halfstep *. ( zz +. f !x t ) -. t in
       y.(i) <- methode g z ;
   done ;
   y ;;


(** {v float_end_ode_trapezoid methode function nsteps value beginning ending v} The method must contain the solving method, included
the method of derivation and all the parameters, as in the following example.

{v float_zero_householder ( float_richardson_deriv 3. 4 1e-3 ) 3 100 v}

La méthode doit contenir la méthode de résolution, y compris la méthode de dérivation et tous les paramètres, 
comme dans l'exemple ci-dessus.
*)
let float_end_ode_trapezoid = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0 in
  let halfstep = step *. 0.5 in
   for i = 1 to nsteps do
    let z = !y in
     let zz = f !x z in
      x := !x +. step ;
      let g = function t -> z +. halfstep *. ( zz +. f !x t ) -. t in
       y := methode g z ;
   done ;
   !y ;;


(** {v float_ode_adams_moulton_2 methode function nsteps value beginning ending v} *)
let float_ode_adams_moulton_2 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0
 and zz = Array.make ( nsteps + 1 ) ( f beginning y0 ) in
  let halfstep = step *. 0.5 in
   let twelfthstep = halfstep /. 6. in
    x := !x +. step ;
    let g = function t -> y0 -. t +. step *. f !x t in
     y.(1) <- methode g y0 ;
    zz.(1) <- f !x y.(1) ;
    for i = 2 to nsteps do
     x := !x +. step ;
     let z = y.( i - 1 ) in
      let g = function t -> z -. t +. twelfthstep *. ( 5. *. ( f !x t ) +. 8. *. zz.( i - 1 ) -. zz.( i - 2 ) ) in
       y.(i) <- methode g z ;
      zz.(i) <- f !x y.(i) ; 
    done ;
    y ;;


(** {v float_end_ode_adams_moulton_2 methode function nsteps value beginning ending v} *)
let float_end_ode_adams_moulton_2 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0
 and yy = ref y0
 and zzz = ref ( f beginning y0 )
 and zz = ref 0. in
  let halfstep = step *. 0.5 in
   let twelfthstep = halfstep /. 6. in
    x := !x +. step ;
    let g = function t -> y0 -. t +. step *. f !x t in
     y := methode g y0 ;
    zz := f !x !y ;
    for i = 2 to nsteps do
     x := !x +. step ;
     let z = !y in
      let g = function t -> z -. t +. twelfthstep *. ( 5. *. ( f !x t ) +. 8. *. ( f ( !x -. step ) z ) -. ( f ( !x -. 2. *. step ) !yy ) ) in
       y := methode g z ;
       yy := z ;
     zzz := !zz ;
     zz := f !x !y ;
    done ;
    !y ;;


(** {v float_ode_milne_simpson_2 methode function nsteps value beginning ending v} *)
let float_ode_milne_simpson_2 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and zzz = Array.make ( nsteps + 1 ) ( f beginning y0 )
 and y = Array.make ( nsteps + 1 ) y0 in
  let thirdstep = step /. 3. in
   x := !x +. step ;
   let g = function t -> y0 -. t +. step *. f !x t in
    y.(1) <- methode g y0 ;
   zzz.(1) <- f !x y.(1) ;
   for i = 2 to nsteps do
    x := !x +. step ;
    let z = y.( i - 2 )
    and zz = y.( i - 1 ) in
     let g = function t -> z -. t +. thirdstep *. ( ( f !x t ) +. 4. *. zzz.( i - 1 ) +. zzz.( i - 2 ) ) in
      y.(i) <- methode g zz ;
     zzz.(i) <- f !x y.(i) ;
   done ;
   y ;;


(** {v float_end_ode_milne_simpson_2 methode function nsteps value beginning ending v} *)
let float_end_ode_milne_simpson_2 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0 
 and yy = ref y0
 and zz = ref 0.
 and zzz = ref ( f beginning y0 )
 and z = ref y0 in
  let thirdstep = step /. 3. in
   x := !x +. step ;
   let g = function t -> y0 -. t +. step *. f !x t in
    y := methode g y0 ;
   zz := f !x !y ;
   for i = 2 to nsteps do
    x := !x +. step ;
    let g = function t -> !yy -. t +. thirdstep *. ( ( f !x t ) +. 4. *. !zz +. !zzz ) in
     z := methode g !y ;
     yy := !y ;
     y := !z ;
    zzz := !zz ;
    zz := f !x !y ;
   done ;
   !y ;;


(** {v float_ode_bdf_2 methode function nsteps value beginning ending v} *)
let float_ode_bdf_2 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  x := !x +. step ;
  let g = function t -> y0 -. t +. step *. f !x t in
   y.(1) <- methode g y0 ;
  for i = 2 to nsteps do
   x := !x +. step ;
   let z = y.( i - 1 ) in
    let g = function t -> 3. *. t -. 4. *. z +. y.( i - 2 ) -. 2. *. step *. ( f !x t ) in
     y.(i) <- methode g z ;
  done ;
  y ;;


(** {v float_end_ode_bdf_2 methode function nsteps value beginning ending v} *)
let float_end_ode_bdf_2 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0
 and yy = ref y0
 and z = ref y0 in
  x := !x +. step ;
  let g = function t -> y0 -. t +. step *. f !x t in
   y := methode g y0 ;
  for i = 2 to nsteps do
   x := !x +. step ;
   let g = function t -> 3. *. t -. 4. *. !y +. !yy -. 2. *. step *. ( f !x t ) in
    z := methode g !y ;
    yy := !y ;
    y := !z ;
  done ;
  !y ;;


(** {v float_ode_bdf_3 methode function nsteps value beginning ending v} *)
let float_ode_bdf_3 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  x := !x +. step ;
  let g = function t -> y0 -. t +. step *. f !x t in
   y.(1) <- methode g y0 ;
  x := !x +. step ;
  let g = function t -> 3. *. t -. 4. *. y.(1) +. y0 -. 2. *. step *. ( f !x t ) in
   y.(2) <- methode g y.(1) ;
  for i = 3 to nsteps do
   x := !x +. step ;
   let z = y.( i - 1 ) in
    let g = function t -> 11. *. t -. 18. *. z +. 9. *. y.( i - 2 ) -. 2. *. y.( i - 3 ) -. 6. *. step *. ( f !x t ) in
     y.(i) <- methode g z ;
  done ;
  y ;;


(** {v float_end_ode_bdf_3 methode function nsteps value beginning ending v} *)
let float_end_ode_bdf_3 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0
 and yy = ref y0
 and yyy = ref y0
 and z = ref y0 in
  x := !x +. step ;
  let g = function t -> y0 -. t +. step *. f !x t in
   yy := methode g y0 ;
  x := !x +. step ;
  let g = function t -> 3. *. t -. 4. *. !yy +. y0 -. 2. *. step *. ( f !x t ) in
   y := methode g !yy ;
  for i = 3 to nsteps do
   x := !x +. step ;
   let g = function t -> 11. *. t -. 18. *. !y +. 9. *. !yy -. 2. *. !yyy -. 6. *. step *. ( f !x t ) in
    z := methode g !y ;
    yyy := !yy ;
    yy := !y ;
    y := !z ;
  done ;
  !y ;;




(** {v float_ode_bdf_4 methode function nsteps value beginning ending v} *)
let float_ode_bdf_4 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = Array.make ( nsteps + 1 ) y0 in
  x := !x +. step ;
  let g = function t -> y0 -. t +. step *. f !x t in
   y.(1) <- methode g y0 ;
  x := !x +. step ;
  let g = function t -> 3. *. t -. 4. *. y.(1) +. y0 -. 2. *. step *. ( f !x t ) in
   y.(2) <- methode g y.(1) ;
  x := !x +. step ;
  let g = function t -> 11. *. t -. 18. *. y.(2) +. 9. *. y.(1) -. 2. *. y0 -. 6. *. step *. ( f !x t ) in
   y.(3) <- methode g y.(2) ;
  for i = 4 to nsteps do
   x := !x +. step ;
   let z = y.( i - 1 ) in
    let g = function t -> 25. *. t -. 48. *. z +. 36. *. y.( i - 2 ) -. 16. *. y.( i - 3 ) +. 3. *. y.( i - 4 ) -. 12. *. step *. ( f !x t ) in
     y.(i) <- methode g z ;
  done ;
  y ;;


(** {v float_end_ode_bdf_4 methode function nsteps value beginning ending v} *)
let float_end_ode_bdf_4 = fun methode (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and y = ref y0
 and yy = ref y0
 and yyy = ref y0
 and yyyy = ref y0
 and z = ref y0 in
  x := !x +. step ;
  let g = function t -> y0 -. t +. step *. f !x t in
   yyy := methode g y0 ;
  x := !x +. step ;
  let g = function t -> 3. *. t -. 4. *. !yyy +. y0 -. 2. *. step *. ( f !x t ) in
   yy := methode g !yyy ;
  x := !x +. step ;
  let g = function t -> 11. *. t -. 18. *. !y +. 9. *. !yy -. 2. *. !yyy -. 6. *. step *. ( f !x t ) in
   y := methode g !yy ;
  for i = 4 to nsteps do
   x := !x +. step ;
   let g = function t -> 25. *. t -. 48. *. !y +. 36. *. !yy -. 16. *. !yyy +. 3. *. !yyyy -. 12. *. step *. ( f !x t ) in
    z := methode g !y ;
    yyy := !yy ;
    yy := !y ;
    y := !z ;
  done ;
  !y ;;


(** {v float_ode_runge_kutta_impl methode butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let float_ode_runge_kutta_impl = fun methode (a:float array array) (b:float array) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and y = Array.make ( nsteps + 1 ) y0 in
  let c = Array.make l 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = Array.make l 0. in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
     xx := x.(0) ;
     z := y.( i - 1 ) ;
     let g = function t -> f ( !xx +. c.(0) *. step ) ( !z +. step *. a.(0).(0) *. t ) -. t in
     k.(0) <- methode g !z ;
     for j = 1 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      yy.(j) <- !z ;
      let row = a.(j) in
       for index = 0 to j - 1 do
        zz := !zz +. row.(index) *. k.(index) *. step ;
       done ;
       let g = function t -> ( f ( !xx +. c.(j) *. step ) ( !z +. !zz +. row.(j) *. step *. t ) ) -. t in
       k.( j ) <- methode g !zz ;
     done ;
     x.(0) <- !xx +. step ;
     for j = 0 to ll do
      zz := !zz +. b.(j) *. k.(j) ;
     done ;
     y.(i) <- !z +. step *. !zz ;
     zz := 0. ;
   done ;
   y ;;


(** {v float_end_ode_runge_kutta_impl methode butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let float_end_ode_runge_kutta_impl = fun methode (a:float array array) (b:float array) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and y = ref y0 in
  let c = Array.make l 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = Array.make l 0. in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
     xx := x.(0) ;
     z := !y ;
     let g = function t -> f ( !xx +. c.(0) *. step ) ( !z +. step *. a.(0).(0) *. t ) -. t in
     k.(0) <- methode g !z ;
     for j = 1 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      yy.(j) <- !z ;
      let row = a.(j) in
       for index = 0 to j - 1 do
        zz := !zz +. row.(index) *. k.(index) *. step ;
       done ;
       let g = function t -> ( f ( !xx +. c.(j) *. step ) ( !z +. !zz +. row.(j) *. step *. t ) ) -. t in
       k.( j ) <- methode g !zz ;
     done ;
     x.(0) <- !xx +. step ;
     for j = 0 to ll do
      zz := !zz +. b.(j) *. k.(j) ;
     done ;
     y := !z +. step *. !zz ;
     zz := 0. ;
   done ;
   !y ;;



(** {C § } *)


(** Some Butcher tableaus are following. They have been harvested on the internet.
The square matrix must be lower triangular in the loose sense.
(excepted in the particular case of the trapezoidal method).
Some tableaus quoted in the methods with multidimensional resolution
may be used if they satisfy the condition.

Suivent quelques tableaux de Butcher. Ils ont été glanés sur internet.
La matrice carrée doit être triangulaire inférieure au sens large.
(sauf dans le cas particulier de la méthode trapézoïdale).
Certains tableaux cités dans les méthodes à résolution multidimansionnelle
peuvent donc être utilisés s'ils répondent à la condition.

{C  } *)

let backward_euler_a = [| [| 1. |] |] ;;
(** *)
let backward_euler_b = [| 1. |] ;;

let rk1_gauss_a = [| [| 0.5 |] |] ;;
(** *)
let rk1_gauss_b = [| 1. |] ;;

let rk2_burrage_a = [| [| 0.25 ; 0. |] ; [| 0.5 ; 0.25 |] |] ;;
(** *)
let rk2_burrage_b = [| 0.5 ; 0.5 |] ;;

let rk2_radauI_a = [| [| 0. ; 0. |] ; [| 1. /. 3. ; 1. /. 3. |] |] ;;
(** *)
let rk2_radauI_b = [| 0.25 ; 0.75 |] ;;

let rk2_radauII_a = [| [| 1. /. 3. ; 0. |] ; [| 1. ; 0. |] |] ;;
(** *)
let rk2_radauII_b = [| 0.75 ; 0.25 |] ;;

let rk2_sdirk_2A_b  = [| inv_sqrt_of_2 ; 1. -. inv_sqrt_of_2 |] ;;
(** *)
let rk2_sdirk_2A_a = [| [| 1. -. inv_sqrt_of_2 ; 0. |] ; rk2_sdirk_2A_b |] ;;

let rk2_sdirk_2B_b  = [| -. inv_sqrt_of_2 ; 1. +. inv_sqrt_of_2 |] ;;
(** *)
let rk2_sdirk_2B_a = [| [| 1. +. inv_sqrt_of_2 ; 0. |] ; rk2_sdirk_2B_b |] ;;

let rk2_sdirk_3A_b  = [| 0.5 -. 0.5 *. inv_sqrt_of_3 ; inv_sqrt_of_3 ; 0.5 -. 0.5 *. inv_sqrt_of_3 |] ;;
(** *)
let rk2_sdirk_3A_a = [| [| rk2_sdirk_3A_b.(0) ; 0. ; 0. |] ; [| rk2_sdirk_3A_b.(0) ; rk2_sdirk_3A_b.(0) ; 0. |] ; rk2_sdirk_3A_b |] ;; 

let rk3_example_a = [| [| 1. /. 3. ; 0. |] ; [| 1. ; 0. |] |] ;;
(** *)
let rk3_example_b = [| 0.75 ; 0.25 |] ;;

let imex_ssp2_2_2_2_impl_a = [| [| 1. -. 1. /. sqrt_of_2 ; 0. |] ; [| sqrt_of_2 -. 1. ; 1. -. 1. /. sqrt_of_2 |] |] ;;
(** *)
let imex_ssp2_2_2_2_impl_b = imex_ssp2_2_2_2_b ;;

let imex_ssp2_3_2_2_impl_a = [| [| 0.5 ; 0. ; 0. |] ; [| -0.5 ; 0.5 ; 0. |] ; [| 0. ; 0.5 ; 0.5 |] |] ;;
(** *)
let imex_ssp2_3_2_2_impl_b = imex_ssp2_3_2_2_b ;;

let imex_ssp2_3_3_2_impl_a = [| [| 0.25 ; 0. ; 0. |] ; [| 0. ; 0.25 ; 0. |] ; [| 1. /. 3. ; 1. /. 3. ; 1. /. 3. |] |] ;;
(** *)
let imex_ssp2_3_3_2_impl_b = imex_ssp2_3_3_2_b ;;

let imex_ssp3_3_3_2_impl_a = [| [| 1. -. 1. /. sqrt_of_2 ; 0. ; 0. |] ; [| sqrt_of_2 -. 1. ; 1. -. 1. /. sqrt_of_2 ; 0. |] ;
 [| 1. /. sqrt_of_2 -. 0.5 ; 0. ; 1. -. 1. /. sqrt_of_2 |] |] ;;
(** *)
let imex_ssp3_3_3_2_impl_b = imex_ssp3_3_3_2_b ;;

let mod_ext_bdf_a = [| [| 1. ; 0. ; 0. |] ; [| 1. ; 1. ; 0. |] ; [| 0.5 ; -0.5 ; 1. |] |] ;;
(** *)
let mod_ext_bdf_b = [| 0.5 ; -0.5 ; 1. |] ;;

let lambda_example_start = 0.4358665215 ;;
let lambda_example_polynom = function x -> ( ( 18. -. 6. *. x ) *. x -. 9. ) *. x +. 1. ;;
let lambda_example = float_zero_general ( float_richardson_binary_deriv 4 1e-3 ) 3 100 lambda_example_polynom lambda_example_start ;;
let lambda_example_b = [| ( 4. -. 1.5 *. lambda_example ) *. lambda_example -. 0.25 ;
 ( 1.5 *. lambda_example -. 5. ) *. lambda_example  +. 1.25 ; lambda_example |] ;;
(** *)
let lambda_example_a = [| [| lambda_example ; 0. ; 0. |] ; [| ( 1. -. lambda_example ) *. 0.5 ; lambda_example ; 0. |] ; lambda_example_b |] ;;

let imex = [| 0.24169426078821 ;  0.06042356519705 ;  0.12915286960590 |] ;;
let imex_ssp3_4_3_3_impl_a = [| [| imex.(0) ; 0. ; 0. ; 0. |] ; [| -. imex.(0) ; imex.(0) ; 0. ; 0. |] ; [| 0. ; 1. -. imex.(0) ; imex.(0) ; 0. |] ;
 [| imex.(1) ; imex.(2) ; 0.5 -. imex.(0) -. imex.(1) -. imex.(2) ; imex.(0) |] |] ;;
(** *)
let imex_ssp3_4_3_3_impl_b = imex_ssp3_4_3_3_b

let rk5_try_a = [| [| 0. ; 0. ; 0. ; 0. |] ; [| 0.125 ; 0.125 ; 0. ; 0. |] ; [| -0.01 ; 0.56 ; 0.15 ; 0. |] ; [| 2. /. 7. ; 0. ; 5. /. 7. ; 0. |] |] ;;
(** *)
let rk5_try_b = [| 1. /. 14. ; 32. /. 81. ; 250. /. 567. ; 5. /. 54. |] ;;




(** {C § } *)
(** 
{2 Méthodes implicites avec résolution multidimensionnelle}
{2 Implicit Methods with multidimensional resolution}
*)
(** {C  } *)




(** {v float_ode_runge_kutta_impl_multi methode butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let float_ode_runge_kutta_impl_multi = fun methode (a:float array array) (b:float array) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and y = Array.make ( nsteps + 1 ) y0 in
  let c = Array.make l 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = ref ( Array.make l 0. ) in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to ll do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
     xx := x.(0) ;
     z := y.( i - 1 ) ;
     let g = function t ->
      begin
       let u = ref ( Matrix.matrix_vector_float_prod a t ) in
        u := Matrix.vector_float_scal_mult step !u ;
        u := Matrix.vector_float_scal_add !z !u ; 
        for j = 0 to ll do
         x.(j) <- !xx +. step *. c.(j) ;
         !u.(j) <- t.(j) -. f x.(j) !u.(j) ;
         yy.(j) <- !z ;
        done ;
        !u
      end in
      k := methode g yy ;
      x.(0) <- !xx +. step ;
      for j = 0 to ll do
       zz := !zz +. b.(j) *. !k.(j) ;
      done ;
      y.(i) <- !z +. step *. !zz ;
      zz := 0. ;
   done ;
   y ;;


(** {v float_end_ode_runge_kutta_impl_multi methode butcher_matrix butcher_vector function nsteps value beginning ending v} *)
let float_end_ode_runge_kutta_impl_multi = fun methode (a:float array array) (b:float array) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and y = ref y0 in
  let c = Array.make l 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = ref ( Array.make l 0. ) in
   for i = 0 to ll do
    let row = a.(i) in
     for j = 0 to ll do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
     xx := x.(0) ;
     z := !y ;
     let g = function t ->
      begin
       let u = ref ( Matrix.matrix_vector_float_prod a t ) in
        u := Matrix.vector_float_scal_mult step !u ;
        u := Matrix.vector_float_scal_add !z !u ; 
        for j = 0 to ll do
         x.(j) <- !xx +. step *. c.(j) ;
         !u.(j) <- t.(j) -. f x.(j) !u.(j) ;
         yy.(j) <- !z ;
        done ;
        !u
      end in
      k := methode g ( yy ) ;
      x.(0) <- !xx +. step ;
      for j = 0 to ll do
       zz := !zz +. b.(j) *. !k.(j) ;
      done ;
      y := !z +. step *. !zz ;
      zz := 0. ;
   done ;
   !y ;;



(** {C § } *)


(** Some Butcher tableaus are following. They have been harvested on the internet.

Suivent quelques tableaux de Butcher. Ils ont été glanés sur internet. 

{C  } *)

let trapezoid_a = [| [| 0.5 ; 0.5 |] ; [| 0. ; 0. |] |] ;;
(** *)
let trapezoid_b = [| 0.5 ; 0.5 |] ;;

let rk2_gauss_a = [| [| 0.25 ; 0.25 -. sqrt 3. /. 6. |] ; [| 0.25 +. sqrt 3. /. 6. ; 0.25 |] |] ;;
(** *)
let rk2_gauss_b = [| 0.5 ; 0.5 |] ;;

let rk2_radauIA_a = [| [| 0.25 ; -0.25 |] ; [| 0.25 ; 5. /. 12. |] |] ;;
(** *)
let rk2_radauIA_b = [| 0.25 ; 0.75 |] ;;

let rk2_radauIIA_a = [| [| 5. /. 12. ; -1. /. 12. |] ; [| 0.75 ; 0.25 |] |] ;;
(** *)
let rk2_radauIIA_b = [| 0.75 ; 0.25 |] ;;

let rk3_gauss_a = [| [| 5. /. 36. ; 2. /. 9. -. 1. /. sqrt_of_15 ; 5. /. 36. -. 0.5 /. sqrt_of_15 |] ;
 [|  5. /. 36. +. sqrt_of_15 /. 24. ; 2. /. 9. ; 5. /. 36. -. sqrt_of_15 /. 24. |] ; 
 [| 5. /. 36. +. 0.5 /. sqrt_of_15 ; 2. /. 9. +. 1. /. sqrt_of_15 ; 5. /. 36. |] |] ;;
(** *)
let rk3_gauss_b = [| 5. /. 18. ; 4. /. 9. ; 5. /. 18. |] ;;

let rk3_radauI_a = [| [| 0. ; 0. ; 0. |] ; [| ( 9. +. sqrt_of_6 ) /. 75. ; 0.2 +. sqrt_of_6 /. 120. ; 0.28 -. 73. *. sqrt_of_6 /. 600. |] ;
 [| ( 9. -. sqrt_of_6 ) /. 75. ; 0.28 +. 73. *. sqrt_of_6 /. 600. ; 0.2 -. sqrt_of_6 /. 120. |] |] ;; 
(** *)
let rk3_radauI_b = [| 1. /. 9. ; ( 16. +. sqrt_of_6 ) /. 36. ; ( 16. -. sqrt_of_6 ) /. 36. |] ;;

let rk5_radauIA_a = [| [| 1. /. 9. ; ( -1. -. sqrt_of_6 ) /. 18. ; ( -1. +. sqrt_of_6 ) /. 18. |] ;
 [| 1. /. 9. ; ( 88. +. 7. *. sqrt_of_6 ) /. 360. ; ( 88. -. 43. *. sqrt_of_6 ) /. 360. |] ;
 [| 1. /. 9. ; ( 88. +. 43. *. sqrt_of_6 ) /. 360. ; ( 88. -. 7. *. sqrt_of_6 ) /. 360. |] |] ;;
(** *)
let rk5_radauIA_b = rk3_radauI_b ;;

let rk3_radauII_a = [| [| 0.2 -. sqrt_of_6 /. 120. ; 0.2 -. 11. *. sqrt_of_6 /. 120. ; 0. |] ; [| 0.2 +. 11. *. sqrt_of_6 /. 120. ; 0.2 +. sqrt_of_6 /. 120. ; 0. |] ; 
 [| 0.5 -. 0.5 /. sqrt_of_6 ; 0.5 +. 0.5 /. sqrt_of_6 ; 0. |] |] ;;
(** *)
let rk3_radauII_b = [| ( 16. -. sqrt_of_6 ) /. 36. ; ( 16. +. sqrt_of_6 ) /. 36. ; 1. /. 9. |] ;;

let rk3_radauIIA_b = [| ( 16. -. sqrt_of_6 ) /. 36. ; ( 16. +. sqrt_of_6 ) /. 36. ; 1. /. 9. |] ;;
(** *)
let rk3_radauIIA_a = [| [| ( 88. -. 7. *. sqrt_of_6 ) /. 360. ; ( 296. -. 169. *. sqrt_of_6 ) /. 1800. ; ( -2. +. 3. *. sqrt_of_6 ) /. 225. |] ; 
[| ( 296. +. 169. *. sqrt_of_6 ) /. 1800. ; ( 88. +. 7. *. sqrt_of_6 ) /. 360. ; ( -2. -. 3. *. sqrt_of_6 ) /. 225. |] ; rk3_radauIIA_b |] ;;

let rk5_radauIIA_a = [| [| 9.1232394870892942792e-2 ; -0.14125529502095420843 ; -3.0029194105147424492e-2 |] ; 
[| 0.24171793270710701896 ; 0.20412935229379993199 ; 0.38294211275726193779 |] ; 
[| 0.96604818261509293619 ; 1. ; 0. |] |] ;;

let rk5_radauIIA_a_inv = [| [| 4.3255798900631553510 ; 0.33919925181580986954 ; 0.54177053993587487119 |] ; 
[| -4.1787185915519047273 ; -0.32768282076106238708 ; 0.47662355450055045196 |] ; 
[| -0.50287263494578687595 ; 2.5719269498556054292 ; -0.59603920482822492497 |] |] ;;

let rk5_radauIIA_b = [| -. ( 13. +. 7. *. sqrt_of_6 ) /. 3. ; ( -13. +. 7. *. sqrt_of_6 ) /. 3. ; -1. /. 3. |] ;;


let rk9_radauIIA_b = [| -0.2778093394406463730479e2 ; 0.3641478498049213152712e1 ;
-0.1252547721169118720491e1 ; 0.5920031671845428725662 ; -0.2 |] ;;

let rk9_radauIIA_a = [| [| -0.1251758622050104589014e-1 ; -0.1024204781790882707009e-1 ;
 0.4767387729029572386318e-1 ; -0.1147851525522951470794e-1 ; -0.1401985889287541028108e-1 |] ; 
[| -0.1491670151895382429004e-2 ; 0.5017286451737105816299e-1 ; -0.9433181918161143698066e-1 ; 
-0.7668830749180162885157e-2 ; 0.2470857842651852681253e-1 |] ; 
[| -0.7298187638808714862266e-1 ; -0.2305395340434179467214 ; 0.1027030453801258997922 ; 
0.1939846399882895091122e-1 ; 0.8180035370375117083639e-1 |] ; 
[| -0.3800914400035681041264 ; 0.3778939022488612495439 ; 0.4667441303324943592896 ; 
0.4076011712801990666217 ; 0.1996824278868025259365 |] ; 
[| -0.9219789736812104884883 ; 1. ; 0.0 ; 1. ; 0.0 |] |] ;;

let rk9_radauIIA_a_inv = [| [| -0.3004156772154440162771e2 ; -0.1386510785627141316518e2 ; 
-0.3480002774795185561828e1 ; 0.1032008797825263422771e1 ; -0.8043030450739899174753 |] ; 
[| 0.5344186437834911598895e1 ; 0.4593615567759161004454e1 ; -0.3036360323459424298646e1 ; 
0.1050660190231458863860e1 ; -0.2727786118642962705386 |] ; 
[| 0.3748059807439804860051e1 ; -0.3984965736343884667252e1 ; -0.1044415641608018792942e1 ; 
0.1184098568137948487231e1 ; -0.4499177701567803688988 |] ; 
[| -0.3304188021351900000806e2 ; -0.1737695347906356701945e2 ; -0.1721290632540055611515 ; 
-0.9916977798254264258817e-1 ; 0.5312281158383066671849 |] ; 
[| -0.8611443979875291977700e1 ; 0.9699991409528808231336e1 ; 0.1914728639696874284851e1 ; 
0.2418692006084940026427e1 ; -0.1047463487935337418694e1 |] |] ;;


let rk13_radauIIA_b = [| -0.5437443689412861451458e2 ; 0.7000024004259186512041e1 ; -0.2355661091987557192256e1 ; 
0.1132289066106134386384e1 ; -0.6468913267673587118673 ; 0.3875333853753523774248 ; -0.1428571428571428571429 |] ;;

let rk13_radauIIA_a = [| [| -0.2153754627310526422828e-2 ; 0.2156755135132077338691e-1 ; 0.8783567925144144407326e-2 ; 
-0.4055161452331023898198e-2 ; 0.4427232753268285479678e-2 ; -0.1238646187952874056377e-2 ; -0.2760617480543852499548e-2 |] ; 
[| 0.1600025077880428526831e-2 ; -0.3813164813441154669442e-1 ; -0.2152556059400687552385e-1 ; 0.8415568276559589237177e-2 ; 
-0.4031949570224549492304e-2 ; -0.6666635339396338181761e-4 ; 0.3185474825166209848748e-2 |] ; 
[| -0.4059107301947683091650e-2 ; 0.5739650893938171539757e-1 ; 0.5885052920842679105612e-1 ; -0.8560431061603432060177e-2 ; 
-0.6923212665023908924141e-2 ; -0.2352180982943338340535e-2 ; 0.4169077725297562691409e-3 |] ; 
[| -0.1575048807937684420346e-1 ; -0.3821469359696835048464e-1 ; -0.1657368112729438512412 ; -0.3737124230238445741907e-1 ; 
0.8239007298507719404499e-2 ; 0.3115071152346175252726e-2 ; 0.2511660491343882192836e-1 |] ; 
[| -0.1129776610242208076086 ; -0.2491742124652636863308 ; 0.2735633057986623212132 ; 0.5366761379181770094279e-2 ; 
0.1932111161012620144312 ; 0.1017177324817151468081 ; 0.9504502035604622821039e-1 |] ; 
[| -0.4583810431839315010281 ; 0.5315846490836284292051 ; 0.4863228366175728940567 ; 0.5265742264584492629141 ; 
0.2755343949896258141929 ; 0.5217519452747652852946 ; 0.1280719446355438944141 |] ; 
[| -0.8813915783538183763135 ; 1. ; 0. ; 1. ; 0. ; 1. ; 0. |] |] ;;

let rk13_radauIIA_a_inv = [| [| -0.2581319263199822292761e3 ; -0.1890737630813985089520e3 ; -0.4908731481793013119445e2 ; 
-0.4110647469661428418112e1 ; -0.4053447889315563304175e1 ; 0.3112755366607346076554e1 ; -0.1646774913558444650169e1 |] ; 
[| -0.3007390169451292131731e1 ; -0.1101586607876577132911e2 ; 0.1487799456131656281486e1 ; 0.2130388159559282459432e1 ; 
-0.1816141086817565624822e1 ; 0.1134325587895161100083e1 ; -0.4146990459433035319930 |] ; 
[| -0.8441963188321084681757e1 ; -0.6505252740575150028169 ; 0.6940670730369876478804e1 ; -0.3205047525597898431565e1 ; 
0.1071280943546478589783e1 ; -0.3548507491216221879730 ; 0.9198549132786554154409e-1 |] ; 
[| 0.7467833223502269977153e2 ; 0.8740858897990081640204e2 ; 0.4024158737379997877014e1 ; -0.3714806315158364186639e1 ; 
-0.3430093985982317350741e1 ; 0.2696604809765312378853e1 ; -0.9386927436075461933568 |] ; 
[| 0.5835652885190657724237e2 ; -0.1006877395780018096325e2 ; -0.3036638884256667120811e2 ; -0.1020020865184865985027e1 ; 
-0.1124175003784249621267 ; 0.1890640831000377622800e1 ; -0.9716486393831482282172 |] ; 
[| -0.2991862480282520966786e3 ; -0.2430407453687447911819e3 ; -0.4877710407803786921219e2 ; -0.2038671905741934405280e1 ; 
0.1673560239861084944268e1 ; -0.1087374032057106164456e1 ; 0.9019382492960993738427 |] ; 
[| -0.9307650289743530591157e2 ; 0.2388163105628114427703e2 ; 0.3927888073081384382710e2 ; 0.1438891568549108006988e2 ; 
-0.3510438399399361221087e1 ; 0.4863284885566180701215e1 ; -0.2246482729591239916400e1 |] |] ;;


let rk2_lobattoIIIA_a = [| [| 0. ; 0. |] ; [| 0.5 ; 0.5 |] |] ;;
(** *)
let rk2_lobattoIIIA_b = [| 0.5 ; 0.5 |] ;;

let rk4_lobattoIIIA_a = [| [| 0. ; 0. ; 0. |] ; [| 5. /. 24. ; 1. /. 3. ; -1. /. 24. |] ; [| 1. /. 6. ; 2. /. 3. ; 1. /. 6. |] |] ;;
(** The previous method is also quoted as Hermite-Simpson method. *)
let rk4_lobattoIIIA_b = [| 1. /. 6. ; 2. /. 3. ; 1. /. 6. |] ;;

let rk6_lobattoIIIA_b = [| 1. /. 12. ; 5. /. 12. ; 5. /. 12. ; 1. /. 12. |] ;;
(** *)
let rk6_lobattoIIIA_a = [| [| 0. ; 0. ; 0. ; 0. |] ; [| ( 11. +.  sqrt_of_5 ) /. 120. ; ( 25. -.  sqrt_of_5 ) /. 120. ; ( 25. -. 13. *.  sqrt_of_5 ) /. 120. ; ( sqrt_of_5 -. 1. ) /. 120. |] ;
 [| ( 11. -.  sqrt_of_5 ) /. 120. ; ( 25. +. 13. *.  sqrt_of_5 ) /. 120. ; ( 25. +.  sqrt_of_5 ) /. 120. ; ( -1. -. sqrt_of_5 ) /. 120. |] ; rk6_lobattoIIIA_b |] ;;

let rk2_lobattoIIIB_a = [| [| 0.5 ; 0. |] ; [| 0.5 ; 0. |] |] ;;
(** *)
let rk2_lobattoIIIB_b = [| 0.5 ; 0.5 |] ;;

let rk4_lobattoIIIB_a = [| [| 1. /. 6. ; -1. /. 6. ; 0. |] ; [| 1. /. 6. ; 1. /. 3. ; 0. |] ; [| 1. /. 6. ; 5. /. 6. ; 0. |] |] ;;
(** *)
let rk4_lobattoIIIB_b = [| 1. /. 6. ; 2. /. 3. ; 1. /. 6. |] ;;

let rk6_lobattoIIIB_b = rk6_lobattoIIIA_b ;;
(** *)
let rk6_lobattoIIIB_a = [| [| 1. /. 12. ; ( -1. -. sqrt_of_5 ) /. 24. ; ( sqrt_of_5 -. 1. ) /. 24. ; 0. |] ;
 [| 1. /. 12. ; ( 25. +.  sqrt_of_5 ) /. 120. ; ( 25. -. 13. *.  sqrt_of_5 ) /. 120. ; 0. |] ;
 [| 1. /. 12. ; ( 25. +. 13. *.  sqrt_of_5 ) /. 120. ; ( 25. -.  sqrt_of_5 ) /. 120. ; 0. |] ; 
[| 1. /. 12. ; ( 11. -.  sqrt_of_5 ) /. 24. ; ( 11. +.  sqrt_of_5 ) /. 24. ; 0. |] |] ;;

let rk2_lobattoIIIC_a = [| [| 0.5 ; -0.5 |] ; [| 0.5 ; 0.5 |] |] ;;
(** *)
let rk2_lobattoIIIC_b = [| 0.5 ; 0.5 |] ;;

let rk4_lobattoIIIC_a = [| [| 1. /. 6. ; -1. /. 3. ; 1. /. 6. |] ; [| 1. /. 6. ; 5. /. 12. ; -1. /. 12. |] ; [| 1. /. 6. ; 2. /. 3. ; 1. /. 6. |] |] ;;
(** *)
let rk4_lobattoIIIC_b = [| 1. /. 6. ; 2. /. 3. ; 1. /. 6. |] ;;

let rk2_lobattoIIICstar_a = [| [| 0. ; 0. |] ; [| 1. ; 0. |] |] ;;
(** *)
let rk2_lobattoIIICstar_b = [| 0.5 ; 0.5 |] ;;

let rk4_lobattoIIICstar_a = [| [| 0. ; 0. ; 0. |] ; [| 0.25 ; 0.25 ; 0. |] ; [| 0. ; 1. ; 0. |] |] ;;
(** The previous method is also called Lobatto III for s = 3. *)
let rk4_lobattoIIICstar_b = [| 1. /. 6. ; 2. /. 3. ; 1. /. 6. |] ;;

let rk2_lobattoIIID_a = [| [| 0.25 ; -0.25 |] ; [| 0.75 ; 0.25 |] |] ;;
(** *)
let rk2_lobattoIIID_b = [| 0.5 ; 0.5 |] ;;

let rk4_lobattoIIID_a = [| [| 1. /. 12. ; -1. /. 6. ; 1. /. 12. |] ; [| 5. /. 24. ; 1. /. 3. ; -1. /. 24. |] ; [| 1. /. 12. ; 5. /. 6. ; 1. /. 12. |] |] ;;
(** *)
let rk4_lobattoIIID_b = [| 1. /. 6. ; 2. /. 3. ; 1. /. 6. |] ;;

let example_DESIRE_b = [| ( 16. +. sqrt_of_2 ) /. 40. ; ( 16. -. sqrt_of_2 ) /. 40. ; 1. /. 5. |] ;;
(** *)
let example_DESIRE_a = [| [| ( 4. -. sqrt_of_2 ) /. 20. ; ( 4. -. 3. *. sqrt_of_2 ) /. 20. ; 0. |] ;
 [| ( 4. +. 3. *. sqrt_of_2 ) /. 20. ; ( 4. +. sqrt_of_2 ) /. 20. ; 0. |] ; example_DESIRE_b |] ;;




(** {C § } *)
(** 
{2 Méthodes adaptatives explicites}
{2 Explicit adaptative methods}
*)
(** {C  } *)




(** {v float_ode_runge_kutta_simple_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let float_ode_runge_kutta_simple_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and error = ref 0.
 and y = Array.make ( nsteps + 1 ) y0 in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = Array.make ( l + 1 ) 0. in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
     xx := x.(0) ;
     z := y.( i - 1 ) ;
     k.(0) <- f !xx !z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      yy.(j) <- !z ;
      let row = a.(j) in
       for index = 0 to j do
        zz := !zz +. row.(index) *. k.(index) ;
       done ;
       yy.(j) <- yy.(j) +. step *. !zz ;
       zz := 0. ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     y.(i) <- !z ;
     for j = 0 to l do
      zz := !zz +. b.(j) *. k.(j) ;
      error := !error +. bb.(j) *. k.(j) ;
     done ;
     if abs_float ( !error ) > tol *. !zz then
      y.(i) <- float_end_ode_runge_kutta a b f nsteps y.( i - 1 ) !xx x.(0)
     else 
      y.(i) <- !z +. step *. !zz ;
     zz := 0. ;
     error := 0. ;
   done ;
   y ;;


(** {v vector_ode_runge_kutta_simple_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let vector_ode_runge_kutta_simple_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and dim = Array.length y0
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning in
  let c = Array.make l 0.
  and dd = dim - 1
  and error = Array.make dim 0.
  and zz = Array.make dim 0.
  and z = Array.make dim 0.
  and y = Array.make_matrix ( nsteps + 1 ) dim 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make_matrix l dim 0.
  and k = Array.make_matrix ( l + 1 ) dim 0. in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   let row = y.(0) in
    for indice = 0 to dd do
     row.(indice) <- y0.(indice)
    done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    let row = y.( i - 1 ) in
     for indice = 0 to dd do
      z.(indice) <- row.(indice) ;
     done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     yy.(j) <- z ;
     let row = a.(j) in
      for index = 0 to j do
       let coeff =  row.(index)
       and ligne = k.(index) in
        for indice = 0 to dd do
         zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
        done ;
      done ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- row.(indice) +. step *. zz.(indice) ;
       zz.(indice) <- 0. ;
      done ;
     k.( j + 1 ) <- f x.(j) yy.(j) ;
    done ;
    x.(0) <- !xx +. step ;
    y.(i) <- z ;
    for j = 0 to l do
     let row = k.(j)
     and coefficient = bb.(j)
     and coeff = b.(j) in
      for indice = 0 to dd do
       let kk = row.(indice) in
       zz.(indice) <- zz.(indice) +. coeff *. kk ;
       error.(indice) <- error.(indice) +. coefficient *. kk ;
     done ;
    done ;
    if ( Matrix.vector_float_norm_inf error ) > tol *. ( Matrix.vector_float_norm_inf zz ) then
     y.(i) <- vector_end_ode_runge_kutta a b f nsteps y.( i - 1 ) !xx x.(0)
    else 
     let row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) +. step *. zz.(indice) ;
     done ;
    for indice = 0 to dd do
     zz.(indice) <- 0. ;
     error.(indice) <- 0. ;
    done ;
   done ;
   y ;;


(** {v matrix_ode_runge_kutta_simple_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let matrix_ode_runge_kutta_simple_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and dim_r = Array.length y0
 and dim_c = Array.length y0.(0)
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning in
  let c = Array.make l 0.
  and dd = dim_r - 1
  and cc = dim_c - 1
  and error = Array.make_matrix dim_r dim_c 0.
  and zz = Array.make_matrix dim_r dim_c 0.
  and z = Array.make_matrix dim_r dim_c 0.
  and y = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( nsteps + 1 ) 0. )
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make l 0. )
  and k = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( l + 1 ) 0. ) in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   let row = y.(0) in
    for indice = 0 to dd do
     row.(indice) <- y0.(indice)
    done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    let row = y.( i - 1 ) in
     for indice = 0 to dd do
      z.(indice) <- row.(indice) ;
     done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     yy.(j) <- z ;
     let row = a.(j) in
      for index = 0 to j do
       let coeff =  row.(index)
       and ligne = k.(index) in
        for indice = 0 to dd do
         zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
        done ;
      done ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- Matrix.vector_float_plus row.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
       let ligne = zz.(indice) in
        for numero = 0 to cc do
         ligne.(numero) <- 0. ;
        done ;
      done ;
     k.( j + 1 ) <- f x.(j) yy.(j) ;
    done ;
    x.(0) <- !xx +. step ;
    y.(i) <- z ;
    for j = 0 to l do
     let row = k.(j)
     and coefficient = bb.(j)
     and coeff = b.(j) in
      for indice = 0 to dd do
       let kk = row.(indice) in
       zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff kk ) ;
       error.(indice) <- Matrix.vector_float_plus error.(indice) ( Matrix.vector_float_scal_mult coefficient kk ) ;
     done ;
    done ;
    if ( Matrix.matrix_float_norm_inf error ) > tol *. ( Matrix.matrix_float_norm_inf zz ) then
     y.(i) <- matrix_end_ode_runge_kutta a b f nsteps y.( i - 1 ) !xx x.(0)
    else 
     let row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- Matrix.vector_float_plus z.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
      done ;
    for indice = 0 to dd do
     let ligne = zz.(indice)
     and row = error.(indice) in
      for numero = 0 to cc do
       ligne.(numero) <- 0. ;
       row.(numero) <- 0. ;
      done ;
    done ;
   done ;
   y ;;



(** {v float_end_ode_runge_kutta_simple_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let float_end_ode_runge_kutta_simple_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and error = ref 0.
 and y = ref y0 in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = Array.make ( l + 1 ) 0. in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
     xx := x.(0) ;
     z := !y ;
     k.(0) <- f !xx !z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      yy.(j) <- !z ;
      let row = a.(j) in
       for index = 0 to j do
        zz := !zz +. row.(index) *. k.(index) ;
       done ;
       yy.(j) <- yy.(j) +. step *. !zz ;
       zz := 0. ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     y := !z ;
     for j = 0 to l do
      zz := !zz +. b.(j) *. k.(j) ;
      error := !error +. bb.(j) *. k.(j) ;
     done ;
     if abs_float ( !error ) > tol *. !zz then
      y := float_end_ode_runge_kutta a b f nsteps !y !xx x.(0)
     else 
      y := !z +. step *. !zz ;
     zz := 0. ;
     error := 0. ;
   done ;
   !y ;;


(** {v vector_end_ode_runge_kutta_simple_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let vector_end_ode_runge_kutta_simple_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and dim = Array.length y0
 and y = Matrix.vector_float_copy y0 in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and dd = dim - 1
  and z = Array.make dim 0.
  and zz = Array.make dim 0.
  and error = Array.make dim 0.
  and x = Array.make l beginning
  and yy = Array.make_matrix l dim 0.
  and k = Array.make_matrix ( l + 1 ) dim 0. in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    for indice = 0 to dd do
     z.(indice) <- y.(indice) ;
    done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) ;
      done ;
     let row = a.(j) in
      for index = 0 to j do
       let coeff = row.(index)
       and ligne = k.(index) in
        for indice = 0 to dd do
         zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
        done ;
      done ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- row.(indice) +. step *. zz.(indice) ;
        zz.(indice) <- 0. ;
       done ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
    done ;
    x.(0) <- !xx +. step ;
    for indice = 0 to dd do
     y.(indice) <- z.(indice) ;
    done ;
    for j = 0 to l do
     let ligne = k.(j)
     and coeff = b.(j)
     and coefficient = bb.(j) in
      for indice = 0 to dd do
       let kk = ligne.(indice) in
        zz.(indice) <- zz.(indice) +. coeff *. kk ;
        error.(indice) <- error.(indice) +. coefficient *. kk ;
      done ;
    done ;
     if ( Matrix.vector_float_norm_inf error ) > tol *. ( Matrix.vector_float_norm_inf zz ) then
      let zzz = vector_end_ode_runge_kutta a b f nsteps y !xx x.(0) in
       for indice = 0 to dd do
        y.(indice) <- zzz.(indice) ;
       done ;
     else 
      for indice = 0 to dd do
       y.(indice) <- z.(indice) +. step *. zz.(indice) ;
      done ;
     for indice = 0 to dd do
      zz.(indice) <- 0. ;
      error.(indice) <- 0. ;
     done ;
   done ;
   y ;;


(** {v matrix_end_ode_runge_kutta_simple_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let matrix_end_ode_runge_kutta_simple_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and dim_r = Array.length y0
 and dim_c = Array.length y0.(0)
 and y = Matrix.matrix_float_copy y0 in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and dd = dim_r - 1
  and cc = dim_c - 1
  and z = Array.make_matrix dim_r dim_c 0.
  and zz = Array.make_matrix dim_r dim_c 0.
  and error = Array.make_matrix dim_r dim_c 0.
  and x = Array.make l beginning
  and yy = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make l 0. )
  and k = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( l + 1 ) 0. ) in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    for indice = 0 to dd do
     z.(indice) <- y.(indice) ;
    done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) ;
      done ;
     let row = a.(j) in
      for index = 0 to j do
       let coeff = row.(index)
       and ligne = k.(index) in
        for indice = 0 to dd do
         zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
        done ;
      done ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- Matrix.vector_float_plus row.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
        let ligne = zz.(indice) in
         for numero = 0 to cc do
          ligne.(numero) <- 0. ;
         done ;
       done ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
    done ;
    x.(0) <- !xx +. step ;
    for indice = 0 to dd do
     y.(indice) <- z.(indice) ;
    done ;
    for j = 0 to l do
     let ligne = k.(j)
     and coeff = b.(j)
     and coefficient = bb.(j) in
      for indice = 0 to dd do
       let kk = ligne.(indice) in
        zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff kk ) ;
        error.(indice) <- Matrix.vector_float_plus error.(indice) ( Matrix.vector_float_scal_mult coefficient kk ) ;
      done ;
    done ;
     if ( Matrix.matrix_float_norm_inf error ) > tol *. ( Matrix.matrix_float_norm_inf zz ) then
      let zzz = matrix_end_ode_runge_kutta a b f nsteps y !xx x.(0) in
       for indice = 0 to dd do
        y.(indice) <- zzz.(indice) ;
       done ;
     else 
      for indice = 0 to dd do
       y.(indice) <- Matrix.vector_float_plus z.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
      done ;
     for indice = 0 to dd do
      let ligne = zz.(indice)
      and row = error.(indice) in
       for numero = 0 to cc do
        ligne.(numero) <- 0. ;
        row.(numero) <- 0. ;
       done ;
     done ;
   done ;
   y ;;



(** {v float_end_ode_runge_kutta_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let rec float_end_ode_runge_kutta_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and error = ref 0.
 and y = ref y0 in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = Array.make ( l + 1 ) 0. in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
     xx := x.(0) ;
     z := !y ;
     k.(0) <- f !xx !z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      yy.(j) <- !z ;
      let row = a.(j) in
       for index = 0 to j do
        zz := !zz +. row.(index) *. k.(index) ;
       done ;
       yy.(j) <- yy.(j) +. step *. !zz ;
       zz := 0. ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     y := !z ;
     for j = 0 to l do
      zz := !zz +. b.(j) *. k.(j) ;
      error := !error +. bb.(j) *. k.(j) ;
     done ;
     if abs_float ( !error ) > tol *. !zz then
      y := float_end_ode_runge_kutta a b f nsteps !y !xx x.(0)
     else y := !z +. step *. !zz ;
     zz := 0. ;
     error := 0. ;
   done ;
   !y ;;


(** {v vector_end_ode_runge_kutta_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let rec vector_end_ode_runge_kutta_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and dim = Array.length y0
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and y = Matrix.vector_float_copy y0 in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and dd = dim - 1
  and x = Array.make l beginning
  and z = Array.make dim 0.
  and zz = Array.make dim 0.
  and error = Array.make dim 0.
  and yy = Array.make_matrix l dim 0.
  and k = Array.make_matrix ( l + 1 ) dim 0. in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    for indice = 0 to dd do
     z.(indice) <- y.(indice) ;
    done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) ;
      done ;
      let row = a.(j) in
       for index = 0 to j do
        let coeff = row.(index)
        and ligne = k.(index) in
         for indice = 0 to dd do
          zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
         done ;
       done ;
       let row = yy.(j) in
        for indice = 0 to dd do
         row.(indice) <- row.(indice) +. step *. zz.(indice) ;
         zz.(indice) <- 0. ;
        done ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     for indice = 0 to dd do
      y.(indice) <- z.(indice) ;
     done ;
     for j = 0 to l do
      let ligne = k.(j)
      and coeff = b.(j)
      and coefficient = bb.(j) in
       for indice = 0 to dd do
        zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
        error.(indice) <- error.(indice) +. coefficient *. ligne.(indice) ;
       done ;
     done ;
     if ( Matrix.vector_float_norm_inf error ) > tol *. ( Matrix.vector_float_norm_inf zz ) then
      let yyy = vector_end_ode_runge_kutta a b f nsteps y !xx x.(0) in
       for indice = 0 to dd do
        y.(indice) <- yyy.(indice)
       done ;
     else
      for indice = 0 to dd do
       y.(indice) <- z.(indice) +. step *. zz.(indice) ;
      done ;
     for indice = 0 to dd do
      zz.(indice) <- 0. ;
      error.(indice) <- 0. ;
     done ;
   done ;
   y ;;


(** {v matrix_end_ode_runge_kutta_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let rec matrix_end_ode_runge_kutta_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and dim_r = Array.length y0
 and dim_c = Array.length y0.(0)
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and y = Matrix.matrix_float_copy y0 in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and dd = dim_r - 1
  and cc = dim_c - 1
  and x = Array.make l beginning
  and z = Array.make_matrix dim_r dim_c 0.
  and zz = Array.make_matrix dim_r dim_c 0.
  and error = Array.make_matrix dim_r dim_c 0.
  and yy = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make l 0. )
  and k = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( l + 1 ) 0. ) in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    for indice = 0 to dd do
     z.(indice) <- y.(indice) ;
    done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) ;
      done ;
      let row = a.(j) in
       for index = 0 to j do
        let coeff = row.(index)
        and ligne = k.(index) in
         for indice = 0 to dd do
          zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
         done ;
       done ;
       let row = yy.(j) in
        for indice = 0 to dd do
         row.(indice) <- Matrix.vector_float_plus row.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
         let ligne = zz.(indice) in
          for numero = 0 to cc do
           ligne.(numero) <- 0. ;
          done ;
        done ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     for indice = 0 to dd do
      y.(indice) <- z.(indice) ;
     done ;
     for j = 0 to l do
      let ligne = k.(j)
      and coeff = b.(j)
      and coefficient = bb.(j) in
       for indice = 0 to dd do
        zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
        error.(indice) <- Matrix.vector_float_plus error.(indice) ( Matrix.vector_float_scal_mult coefficient ligne.(indice) ) ;
       done ;
     done ;
     if ( Matrix.matrix_float_norm_inf error ) > tol *. ( Matrix.matrix_float_norm_inf zz ) then
      let yyy = matrix_end_ode_runge_kutta a b f nsteps y !xx x.(0) in
       for indice = 0 to dd do
        y.(indice) <- yyy.(indice)
       done ;
     else
      for indice = 0 to dd do
       y.(indice) <- Matrix.vector_float_plus z.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
      done ;
     for indice = 0 to dd do
      let ligne = zz.(indice)
      and row = error.(indice) in
       for numero = 0 to cc do
        ligne.(numero) <- 0. ;
        row.(numero) <- 0. ;
       done ;
     done ;
   done ;
   y ;;



(** {v float_end_ode_runge_kutta_bounded maxstages butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let rec float_end_ode_runge_kutta_bounded = fun (maxstages:int) (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> float_end_ode_runge_kutta_simple_adapt a b bstar tol f nsteps y0 beginning ending
 | _ -> 
  let l = Array.length a
  and step = ( ending -. beginning ) /. ( float nsteps )
  and xx = ref beginning
  and z = ref y0
  and zz = ref 0.
  and error = ref 0.
  and y = ref y0 in
   let c = Array.make l 0.
   and bb = Array.make ( l + 1 ) 0.
   and ll = l - 1
   and x = Array.make l beginning
   and yy = Array.make l y0
   and k = Array.make ( l + 1 ) 0. in
    bb.(l) <- b.(l) -. bstar.(l) ;
    for i = 0 to ll do
     bb.(i) <- b.(i) -. bstar.(i) ;
     let row = a.(i) in
      for j = 0 to i do
       c.(i) <- c.(i) +. row.(j) ;
      done ;
    done ;
    for i = 1 to nsteps do
      xx := x.(0) ;
      z := !y ;
      k.(0) <- f !xx !z ;
      for j = 0 to ll do
       x.(j) <- !xx +. step *. c.(j) ;
       yy.(j) <- !z ;
       let row = a.(j) in
        for index = 0 to j do
         zz := !zz +. row.(index) *. k.(index) ;
        done ;
        yy.(j) <- yy.(j) +. step *. !zz ;
        zz := 0. ;
        k.( j + 1 ) <- f x.(j) yy.(j) ;
      done ;
      x.(0) <- !xx +. step ;
      y := !z ;
      for j = 0 to l do
       zz := !zz +. b.(j) *. k.(j) ;
       error := !error +. bb.(j) *. k.(j) ;
      done ;
      if abs_float ( !error ) > tol *. !zz then
       y := float_end_ode_runge_kutta_bounded ( ( abs maxstages ) - 1 ) a b bstar tol f nsteps !y !xx x.(0)
      else y := !z +. step *. !zz ;
      zz := 0. ;
      error := 0. ;
    done ;
    !y ;;


(** {v vector_end_ode_runge_kutta_bounded maxstages butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let rec vector_end_ode_runge_kutta_bounded = fun (maxstages:int) (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> vector_end_ode_runge_kutta_simple_adapt a b bstar tol f nsteps y0 beginning ending
 | _ -> 
  let l = Array.length a
  and dim = Array.length y0
  and step = ( ending -. beginning ) /. ( float nsteps )
  and xx = ref beginning
  and y = Matrix.vector_float_copy y0 in
   let c = Array.make l 0.
   and bb = Array.make ( l + 1 ) 0.
   and ll = l - 1
   and dd = dim - 1
   and x = Array.make l beginning
   and z = Array.make dim 0.
   and zz = Array.make dim 0.
   and error = Array.make dim 0.
   and yy = Array.make_matrix l dim 0.
   and k = Array.make_matrix ( l + 1 ) dim 0. in
    bb.(l) <- b.(l) -. bstar.(l) ;
    for i = 0 to ll do
     bb.(i) <- b.(i) -. bstar.(i) ;
     let row = a.(i) in
      for j = 0 to i do
       c.(i) <- c.(i) +. row.(j) ;
      done ;
    done ;
    for i = 1 to nsteps do
     xx := x.(0) ;
     for indice = 0 to dd do
      z.(indice) <- y.(indice) ;
     done ;
     k.(0) <- f !xx z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- z.(indice) ;
       done ;
      let row = a.(j) in
       for index = 0 to j do
        let coeff = row.(index)
        and ligne = k.(index) in
         for indice = 0 to dd do
          zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
         done ;
        done ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- row.(indice) +. step *. zz.(indice) ;
        zz.(indice) <- 0. ;
       done ;
      k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     for indice = 0 to dd do
      y.(indice) <- z.(indice) ;
     done ;
     for j = 0 to l do
      let coeff = b.(j)
      and coefficient = bb.(j)
      and ligne = k.(j) in
      for indice = 0 to dd do
       zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
       error.(indice) <- error.(indice) +. coefficient *. ligne.(indice) ;
      done ;
     done ;
     if ( Matrix.vector_float_norm_inf error ) > tol *. ( Matrix.vector_float_norm_inf zz ) then
      let yyy = vector_end_ode_runge_kutta_bounded ( ( abs maxstages ) - 1 ) a b bstar tol f nsteps y !xx x.(0) in
       for indice = 0 to dd do
        y.(indice) <- yyy.(indice) ;
       done ;
     else
       for indice = 0 to dd do
        y.(indice) <- z.(indice) +. step *. zz.(indice) ;
       done ;
     for indice = 0 to dd do
      zz.(indice) <- 0. ;
      error.(indice) <- 0. ;
     done ;
    done ;
    y ;;


(** {v matrix_end_ode_runge_kutta_bounded maxstages butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let rec matrix_end_ode_runge_kutta_bounded = fun (maxstages:int) (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> matrix_end_ode_runge_kutta_simple_adapt a b bstar tol f nsteps y0 beginning ending
 | _ -> 
  let l = Array.length a
  and dim_r = Array.length y0
  and dim_c = Array.length y0.(0)
  and step = ( ending -. beginning ) /. ( float nsteps )
  and xx = ref beginning
  and y = Matrix.matrix_float_copy y0 in
   let c = Array.make l 0.
   and bb = Array.make ( l + 1 ) 0.
   and ll = l - 1
   and dd = dim_r - 1
   and cc = dim_c - 1
   and x = Array.make l beginning
   and z = Array.make_matrix dim_r dim_c 0.
   and zz = Array.make_matrix dim_r dim_c 0.
   and error = Array.make_matrix dim_r dim_c 0.
   and yy = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make l 0. )
   and k = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( l + 1 ) 0. ) in
    bb.(l) <- b.(l) -. bstar.(l) ;
    for i = 0 to ll do
     bb.(i) <- b.(i) -. bstar.(i) ;
     let row = a.(i) in
      for j = 0 to i do
       c.(i) <- c.(i) +. row.(j) ;
      done ;
    done ;
    for i = 1 to nsteps do
     xx := x.(0) ;
     for indice = 0 to dd do
      z.(indice) <- y.(indice) ;
     done ;
     k.(0) <- f !xx z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- z.(indice) ;
       done ;
      let row = a.(j) in
       for index = 0 to j do
        let coeff = row.(index)
        and ligne = k.(index) in
         for indice = 0 to dd do
          zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
         done ;
        done ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- Matrix.vector_float_plus row.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
        let ligne = zz.(indice) in
         for numero = 0 to cc do
          ligne.(numero) <- 0. ;
         done ;
       done ;
      k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     for indice = 0 to dd do
      y.(indice) <- z.(indice) ;
     done ;
     for j = 0 to l do
      let coeff = b.(j)
      and coefficient = bb.(j)
      and ligne = k.(j) in
      for indice = 0 to dd do
       zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
       error.(indice) <- Matrix.vector_float_plus error.(indice) ( Matrix.vector_float_scal_mult coefficient ligne.(indice) ) ;
      done ;
     done ;
     if ( Matrix.matrix_float_norm_inf error ) > tol *. ( Matrix.matrix_float_norm_inf zz ) then
      let yyy = matrix_end_ode_runge_kutta_bounded ( ( abs maxstages ) - 1 ) a b bstar tol f nsteps y !xx x.(0) in
       for indice = 0 to dd do
        y.(indice) <- yyy.(indice) ;
       done ;
     else
      for indice = 0 to dd do
       y.(indice) <- Matrix.vector_float_plus z.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
      done ;
     for indice = 0 to dd do
      let ligne = zz.(indice)
      and row = error.(indice) in
       for numero = 0 to cc do
        ligne.(numero) <- 0. ;
        row.(numero) <- 0. ;
       done ;
     done ;
    done ;
    y ;;



(** {v float_ode_runge_kutta_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let float_ode_runge_kutta_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let l = Array.length a
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning
 and z = ref y0
 and zz = ref 0.
 and error = ref 0.
 and y = Array.make ( nsteps + 1 ) y0 in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and x = Array.make l beginning
  and yy = Array.make l y0
  and k = Array.make ( l + 1 ) 0. in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    z := y.( i - 1 ) ;
    k.(0) <- f !xx !z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     yy.(j) <- !z ;
     let row = a.(j) in
      for index = 0 to j do
       zz := !zz +. row.(index) *. k.(index) ;
      done ;
      yy.(j) <- yy.(j) +. step *. !zz ;
      zz := 0. ;
      k.( j + 1 ) <- f x.(j) yy.(j) ;
    done ;
    x.(0) <- !xx +. step ;
    y.(i) <- !z ;
    for j = 0 to l do
     zz := !zz +. b.(j) *. k.(j) ;
     error := !error +. bb.(j) *. k.(j) ;
    done ;
    if abs_float ( !error ) > tol *. !zz then
     y.(i) <- float_end_ode_runge_kutta_adapt a b bstar tol f nsteps y.( i - 1 ) !xx x.(0)
    else y.(i) <- !z +. step *. !zz ;
    zz := 0. ;
    error := 0. ;
  done ;
  y ;;


(** {v vector_ode_runge_kutta_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let vector_ode_runge_kutta_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and dim = Array.length y0
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and dd = dim - 1
  and x = Array.make l beginning
  and z = Array.make dim 0.
  and zz = Array.make dim 0.
  and error = Array.make dim 0.
  and y = Array.make_matrix ( nsteps + 1 ) dim 0.
  and yy = Array.make_matrix l dim 0.
  and k = Array.make_matrix ( l + 1 ) dim 0. in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   let row = y.(0) in
    for indice = 0 to dd do
     row.(indice) <- y0.(indice) ;
    done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    let row = y.( i - 1 ) in
     for indice = 0 to dd do
      z.(indice) <- row.(indice) ;
     done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) ;
      done ;
     let row = a.(j) in
      for index = 0 to j do
       let coeff = row.(index)
       and ligne = k.(index) in
        for indice = 0 to dd do
         zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
        done ;
      done ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- row.(indice) +. step *. zz.(indice) ;
       zz.(indice) <- 0.
      done ;
     k.( j + 1 ) <- f x.(j) yy.(j) ;
    done ;
    x.(0) <- !xx +. step ;
    let row = y.(i) in
     for indice = 0 to dd do
      row.(indice) <- z.(indice) ;
     done ;
    for j = 0 to l do
     let ligne = k.(j)
     and coeff = b.(j)
     and coefficient = bb.(j) in
      for indice = 0 to dd do
       zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
       error.(indice) <- error.(indice) +. coefficient *. ligne.(indice) ;
      done ;
    done ;
    if ( Matrix.vector_float_norm_inf error ) > tol *. ( Matrix.vector_float_norm_inf zz ) then
     let yyy = vector_end_ode_runge_kutta_adapt a b bstar tol f nsteps y.( i - 1 ) !xx x.(0)
     and row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- yyy.(indice) ;
      done ;
    else
     let row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) +. step *. zz.(indice) ;
      done ;
    for indice = 0 to dd do
     zz.(indice) <- 0. ;
     error.(indice) <- 0. ;
    done ;
   done ;
   y ;;


(** {v matrix_ode_runge_kutta_adapt butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let matrix_ode_runge_kutta_adapt = fun (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let l = Array.length a
 and dim_r = Array.length y0
 and dim_c = Array.length y0.(0)
 and step = ( ending -. beginning ) /. ( float nsteps )
 and xx = ref beginning in
  let c = Array.make l 0.
  and bb = Array.make ( l + 1 ) 0.
  and ll = l - 1
  and dd = dim_r - 1
  and cc = dim_c - 1
  and x = Array.make l beginning
  and z = Array.make_matrix dim_r dim_c 0.
  and zz = Array.make_matrix dim_r dim_c 0.
  and error = Array.make_matrix dim_r dim_c 0.
  and y = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( nsteps + 1 ) 0. )
  and yy = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make l 0. )
  and k = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( l + 1 ) 0. ) in
   bb.(l) <- b.(l) -. bstar.(l) ;
   for i = 0 to ll do
    bb.(i) <- b.(i) -. bstar.(i) ;
    let row = a.(i) in
     for j = 0 to i do
      c.(i) <- c.(i) +. row.(j) ;
     done ;
   done ;
   let row = y.(0) in
    for indice = 0 to dd do
     row.(indice) <- y0.(indice) ;
    done ;
   for i = 1 to nsteps do
    xx := x.(0) ;
    let row = y.( i - 1 ) in
     for indice = 0 to dd do
      z.(indice) <- row.(indice) ;
     done ;
    k.(0) <- f !xx z ;
    for j = 0 to ll do
     x.(j) <- !xx +. step *. c.(j) ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) ;
      done ;
     let row = a.(j) in
      for index = 0 to j do
       let coeff = row.(index)
       and ligne = k.(index) in
        for indice = 0 to dd do
         zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
        done ;
      done ;
     let row = yy.(j) in
      for indice = 0 to dd do
       row.(indice) <- Matrix.vector_float_plus row.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
       let ligne = zz.(indice) in
        for numero = 0 to cc do
         ligne.(numero) <- 0. ;
        done ;
      done ;
     k.( j + 1 ) <- f x.(j) yy.(j) ;
    done ;
    x.(0) <- !xx +. step ;
    let row = y.(i) in
     for indice = 0 to dd do
      row.(indice) <- z.(indice) ;
     done ;
    for j = 0 to l do
     let ligne = k.(j)
     and coeff = b.(j)
     and coefficient = bb.(j) in
      for indice = 0 to dd do
       zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
       error.(indice) <- Matrix.vector_float_plus error.(indice) ( Matrix.vector_float_scal_mult coefficient ligne.(indice) ) ;
      done ;
    done ;
    if ( Matrix.matrix_float_norm_inf error ) > tol *. ( Matrix.matrix_float_norm_inf zz ) then
     let yyy = matrix_end_ode_runge_kutta_adapt a b bstar tol f nsteps y.( i - 1 ) !xx x.(0)
     and row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- yyy.(indice) ;
      done ;
    else
     let row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- Matrix.vector_float_plus z.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
      done ;
    for indice = 0 to dd do
     let ligne = zz.(indice)
     and row = error.(indice) in
      for numero = 0 to cc do
       ligne.(numero) <- 0. ;
       row.(numero) <- 0. ;
      done ;
    done ;
   done ;
   y ;;



(** {v float_ode_runge_kutta_bounded maxstages butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let float_ode_runge_kutta_bounded = fun (maxstages:int) (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> float_ode_runge_kutta_simple_adapt a b bstar tol f nsteps y0 beginning ending
 | _ -> 
  let l = Array.length a
  and step = ( ending -. beginning ) /. ( float nsteps )
  and xx = ref beginning
  and z = ref y0
  and zz = ref 0.
  and error = ref 0.
  and y = Array.make ( nsteps + 1 ) y0 in
   let c = Array.make l 0.
   and bb = Array.make ( l + 1 ) 0.
   and ll = l - 1
   and x = Array.make l beginning
   and yy = Array.make l y0
   and k = Array.make ( l + 1 ) 0. in
    bb.(l) <- b.(l) -. bstar.(l) ;
    for i = 0 to ll do
     bb.(i) <- b.(i) -. bstar.(i) ;
     let row = a.(i) in
      for j = 0 to i do
       c.(i) <- c.(i) +. row.(j) ;
      done ;
    done ;
    for i = 1 to nsteps do
     xx := x.(0) ;
     z := y.( i - 1 ) ;
     k.(0) <- f !xx !z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      yy.(j) <- !z ;
      let row = a.(j) in
       for index = 0 to j do
        zz := !zz +. row.(index) *. k.(index) ;
       done ;
       yy.(j) <- yy.(j) +. step *. !zz ;
       zz := 0. ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     y.(i) <- !z ;
     for j = 0 to l do
      zz := !zz +. b.(j) *. k.(j) ;
      error := !error +. bb.(j) *. k.(j) ;
     done ;
     if abs_float ( !error ) > tol *. !zz then
      y.(i) <- float_end_ode_runge_kutta_bounded ( ( abs maxstages ) - 1 ) a b bstar tol f nsteps y.( i - 1 ) !xx x.(0)
     else y.(i) <- !z +. step *. !zz ;
     zz := 0. ;
     error := 0. ;
   done ;
   y ;;


(** {v vector_ode_runge_kutta_bounded maxstages butcher_matrix butcher_vector_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let vector_ode_runge_kutta_bounded = fun (maxstages:int) (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> vector_ode_runge_kutta_simple_adapt a b bstar tol f nsteps y0 beginning ending
 | _ -> 
  let l = Array.length a
  and dim = Array.length y0
  and step = ( ending -. beginning ) /. ( float nsteps )
  and xx = ref beginning in
   let c = Array.make l 0.
   and bb = Array.make ( l + 1 ) 0.
   and ll = l - 1
   and dd = dim - 1
   and x = Array.make l beginning
   and z = Array.make dim 0.
   and zz = Array.make dim 0.
   and error = Array.make dim 0.
   and y = Array.make_matrix ( nsteps + 1 ) dim 0.
   and yy = Array.make_matrix l dim 0.
   and k = Array.make_matrix ( l + 1 ) dim 0. in
    bb.(l) <- b.(l) -. bstar.(l) ;
    for i = 0 to ll do
     bb.(i) <- b.(i) -. bstar.(i) ;
     let row = a.(i) in
      for j = 0 to i do
       c.(i) <- c.(i) +. row.(j) ;
      done ;
    done ;
    let row = y.(0) in
     for indice = 0 to dd do
      row.(indice) <- y0.(indice) ;
     done ;
    for i = 1 to nsteps do
     xx := x.(0) ;
     let row = y.( i - 1 ) in
      for indice = 0 to dd do
       z.(indice) <- row.(indice) ;
      done ;
     k.(0) <- f !xx z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- z.(indice) ;
       done ;
      let row = a.(j) in
       for index = 0 to j do
        let ligne = k.(index)
        and coeff = row.(index) in
         for indice = 0 to dd do
          zz.(indice) <- zz.(indice) +. coeff *. ligne.(indice) ;
         done ;
       done ;
       let row = yy.(j) in
        for indice = 0 to dd do
         row.(indice) <- row.(indice) +. step *. zz.(indice) ;
         zz.(indice) <- 0. ;
        done ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     let row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) ;
      done ;
     for j = 0 to l do
      let ligne = k.(j)
      and coeff = b.(j)
      and coefficient = bb.(j) in
       for indice = 0 to dd do
        let kk = ligne.(indice) in
         zz.(indice) <- zz.(indice) +. coeff *. kk ;
         error.(indice) <- error.(indice) +. coefficient *. kk ;
       done ;
     done ;
     if ( Matrix.vector_float_norm_inf error ) > tol *. ( Matrix.vector_float_norm_inf zz ) then
      let yyy = vector_end_ode_runge_kutta_bounded ( ( abs maxstages ) - 1 ) a b bstar tol f nsteps y.( i - 1 ) !xx x.(0)
      and row = y.(i) in
       for indice = 0 to dd do
        row.(indice) <- yyy.(indice) ;
       done ;
     else
      let row = y.(i) in
       for indice = 0 to dd do
        row.(indice) <- z.(indice) +. step *. zz.(indice) ;
       done ;
     for indice = 0 to dd do
      zz.(indice) <- 0. ;
      error.(indice) <- 0. ;
     done ;
   done ;
   y ;;


(** {v matrix_ode_runge_kutta_bounded maxstages butcher_matrix butcher_matrix_fine butcher_vector_raw tolerance function nsteps value beginning ending v} *)
let matrix_ode_runge_kutta_bounded = fun (maxstages:int) (a:float array array) (b:float array) (bstar:float array) (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> matrix_ode_runge_kutta_simple_adapt a b bstar tol f nsteps y0 beginning ending
 | _ -> 
  let l = Array.length a
  and dim_r = Array.length y0
  and dim_c = Array.length y0.(0)
  and step = ( ending -. beginning ) /. ( float nsteps )
  and xx = ref beginning in
   let c = Array.make l 0.
   and bb = Array.make ( l + 1 ) 0.
   and ll = l - 1
   and dd = dim_r - 1
   and cc = dim_c - 1
   and x = Array.make l beginning
   and z = Array.make_matrix dim_r dim_c 0.
   and zz = Array.make_matrix dim_r dim_c 0.
   and error = Array.make_matrix dim_r dim_c 0.
   and y = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( nsteps + 1 ) 0. )
   and yy = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make l 0. )
   and k = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( l + 1 ) 0. ) in
    bb.(l) <- b.(l) -. bstar.(l) ;
    for i = 0 to ll do
     bb.(i) <- b.(i) -. bstar.(i) ;
     let row = a.(i) in
      for j = 0 to i do
       c.(i) <- c.(i) +. row.(j) ;
      done ;
    done ;
    let row = y.(0) in
     for indice = 0 to dd do
      row.(indice) <- y0.(indice) ;
     done ;
    for i = 1 to nsteps do
     xx := x.(0) ;
     let row = y.( i - 1 ) in
      for indice = 0 to dd do
       z.(indice) <- row.(indice) ;
      done ;
     k.(0) <- f !xx z ;
     for j = 0 to ll do
      x.(j) <- !xx +. step *. c.(j) ;
      let row = yy.(j) in
       for indice = 0 to dd do
        row.(indice) <- z.(indice) ;
       done ;
      let row = a.(j) in
       for index = 0 to j do
        let ligne = k.(index)
        and coeff = row.(index) in
         for indice = 0 to dd do
          zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff ligne.(indice) ) ;
         done ;
       done ;
       let row = yy.(j) in
        for indice = 0 to dd do
         row.(indice) <- Matrix.vector_float_plus row.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
         let ligne = zz.(indice) in
          for numero = 0 to cc do
           ligne.(numero) <- 0. ;
          done ;
        done ;
       k.( j + 1 ) <- f x.(j) yy.(j) ;
     done ;
     x.(0) <- !xx +. step ;
     let row = y.(i) in
      for indice = 0 to dd do
       row.(indice) <- z.(indice) ;
      done ;
     for j = 0 to l do
      let ligne = k.(j)
      and coeff = b.(j)
      and coefficient = bb.(j) in
       for indice = 0 to dd do
        let kk = ligne.(indice) in
         zz.(indice) <- Matrix.vector_float_plus zz.(indice) ( Matrix.vector_float_scal_mult coeff kk ) ;
         error.(indice) <- Matrix.vector_float_plus error.(indice) ( Matrix.vector_float_scal_mult coefficient kk ) ;
       done ;
     done ;
     if ( Matrix.matrix_float_norm_inf error ) > tol *. ( Matrix.matrix_float_norm_inf zz ) then
      let yyy = matrix_end_ode_runge_kutta_bounded ( ( abs maxstages ) - 1 ) a b bstar tol f nsteps y.( i - 1 ) !xx x.(0)
      and row = y.(i) in
       for indice = 0 to dd do
        row.(indice) <- yyy.(indice) ;
       done ;
     else
      let row = y.(i) in
       for indice = 0 to dd do
        row.(indice) <- Matrix.vector_float_plus z.(indice) ( Matrix.vector_float_scal_mult step zz.(indice) ) ;
       done ;
     for indice = 0 to dd do
      let ligne = zz.(indice)
      and row = error.(indice) in
       for numero = 0 to cc do
        ligne.(numero) <- 0. ;
        row.(numero) <- 0. ;
       done ;
     done ;
   done ;
   y ;;



(** {C § } *)



(** Some Butcher tableaus are following. They have been harvested on the internet.

Suivent quelques tableaux de Butcher. Ils ont été glanés sur internet. 

{C  } *)

let rk2_a = [| [| 1. |] |] ;;
let rk2_b_fine = [| 0.5 ; 0.5 |] ;;
(** *)
let rk2_b_raw = [| 1. ; 0. |] ;;

let rkf_2_3_a = [| [| 1. |] ; [| 0.25 ; 0.25 |] |] ;;
let rkf_2_3_b_fine = [| 1. /. 6. ; 1. /. 6. ; 2. /. 3. |] ;;
(** *)
let rkf_2_3_b_raw = [| 0.5 ; 0.5 ; 0. |] ;;

let bogacki_shampine_a = [| [| 0.5 |] ; [| 0. ; 0.75 |] ; [| 2. /. 9. ; 1. /. 3. ; 4. /. 9. |] |] ;;
let bogacki_shampine_b_raw = [| 2. /. 9. ; 1. /. 3. ; 4. /. 9. ; 0. |] ;;
(** *)
let bogacki_shampine_b_fine = [| 7. /. 24. ; 0.25 ; 1. /. 3. ; 0.125 |] ;;

let rk4_bogacki_shampine_a = [| [| 1.0 /. 6.0 |] ; [| 2.0 /. 27.0 ; 4.0 /. 27.0 |] ; [| 183.0 /. 1372.0 ; -162.0 /. 343.0 ; 1053.0 /. 1372.0 |] ;
 [| 68.0 /. 297.0 ; -4.0 /. 11.0 ; 42.0 /. 143.0 ; 1960.0 /. 3861.0 |] ;
 [| 597.0 /. 22528.0 ; 81.0 /. 352.0 ; 63099.0 /. 585728.0 ; 58653.0 /. 366080.0 ; 4617.0 /. 20480.0 |] ;
 [| 174197.0 /. 959244.0 ; -30942.0 /. 79937.0 ; 8152137.0 /. 19744439.0 ; 666106.0 /. 1039181.0 ; -29421.0 /. 29068.0 ; 482048.0 /. 414219.0 |] ;
 [| 587.0 /. 8064.0 ; 0.0 ; 4440339.0 /. 15491840.0 ; 24353.0 /. 124800.0 ; 387.0 /. 44800.0 ; 2152.0 /. 5985.0 ; 7267.0 /. 94080.0 |] |] ;;
let rk4_bogacki_shampine_b_fine = [| 2479.0 /. 34992.0 ; 0.0 ; 123.0 /. 416.0 ; 612941.0 /. 3411720.0 ; 43.0 /. 1440.0 ; 2272.0 /. 6561.0 ; 79937.0 /. 1113912.0 ; 3293.0 /. 556956.0 |] ;;
let rk4_bogacki_shampine_b_err = [| -3.0 /. 1280.0 ; 0.0 ; 6561.0 /. 632320.0 ; -343.0 /. 20800.0 ; 243.0 /. 12800.0 ; -1.0 /. 95.0 ; 0.0 |] ;;
(** *)
let rk4_bogacki_shampine_b_raw = Matrix.vector_float_plus rk4_bogacki_shampine_b_err rk4_bogacki_shampine_b_fine ;;

let dormand_prince_4_5_a = [| [| 0.2 |] ; [| 0.075 ; 0.225 |] ; [| 44. /. 45. ;  -56. /. 15. ; 32. /. 9. |] ;
 [| 19372. /. 6561. ; -25360. /. 2187. ; 64448. /. 6561. ; -212. /. 729. ; |] ;
 [| 9017. /. 3168. ; -355. /. 33. ; 46732. /. 5247. ; 49. /. 176. ; -5103. /. 18656. |] ;
 [| 35. /. 384. ; 0. ; 500. /. 1113. ; 125. /. 192. ; -2187. /. 6784. ; 11. /. 84. |] |] ;;
let dormand_prince_4_5_b_raw = [| 5179. /. 57600. ; 0. ; 7571. /. 16695. ; 0.6140625 ; -92097. /. 339200. ; 187. /. 2100. ; 0.025 |] ;;
(** *)
let dormand_prince_4_5_b_fine = [| 35. /. 384. ; 0. ; 500. /. 1113. ; 125. /. 192. ; -2187. /. 6784. ; 11. /. 84. ; 0. |] ;;

let runge_kutta_fehlberg_a = [| [| 0.25 |] ; [| 3. /. 32. ; 9. /. 32. |] ;
 [| 1932. /. 2197. ; -7200. /. 2197. ;  7296. /. 2197. |] ; [| 439. /. 216. ; -8. ; 3680. /. 513. ; -845. /. 4104. |] ;
 [| -8. /. 27. ; 2. ; -3544. /. 2565. ; -1859. /. 4104. ; -0.275 |] |] ;;
let runge_kutta_fehlberg_b_fine = [| 16. /. 135. ; 0. ; 6656. /. 12825. ; 28561. /. 56430. ; -0.18 ; 2. /. 55. |] ;;
(** *)
let runge_kutta_fehlberg_b_raw = [| 25. /. 216. ; 0. ; 1408. /. 2565. ; 2197. /. 4104. ; -0.2 ; 0. |] ;;

let cash_karp_a = [| [| 0.2 |] ; [| 0.075 ; 0.225 |] ; [| 0.3 ; -0.9 ; 1.2 |] ;
 [| -11. /. 54. ; 2.5 ; -70. /. 27. ; 35. /. 27. |] ;
 [| 1631. /. 55296. ; 0.341796875 ; 575. /. 13824. ; 44275. /. 110592. ; 0.061767578125 |] |] ;;
let cash_karp_b_fine = [| 37. /. 378. ; 0. ; 250. /. 621. ; 125. /. 594. ; 0. ;  512. /. 1771. |]
(** *)
let cash_karp_b_raw = [| 2825. /. 27648. ; 0. ; 18575. /. 48384. ; 13525. /. 55296. ; 277. /. 14336. ; 0.25 |] ;;

let hem_5_3_a = [| [| 0.1 |] ;
[| 0.375e-1 ; 0.1125 |] ;
[| 0.3222169236216038 ; -0.1188883322987607e1 ; 0.1228145134023366e1 |] ;
[| -0.3501123898129943e-1 ; 0.3725420601086163 ; -0.2721053535582034 ; 0.1993037578575077e-1 |] ;
[| -0.5576547055042005 ; 0.1367307289645883e1 ; -0.1732236360460725e1 ; 0.4587772007467548 ; 0.9638065755722880 |] ;
[| 0.8654517193566155e-1 ; -0.8810082847945416e-1 ; 0.1981275547329404 ; 
-0.4645422679331083 ; 0.1615170091109488 ; 0.9064533606330119 |] ;
[| 0. ; 0. ; 0. ; 0.3624477248753816e1 ; -0.4617724189181256 ; -0.3198024628164272e1 ; 0.1035319798328740e1 |] |] ;;
let hem_5_3_b_fine = [| 0. ; 0. ; 0. ; 0.2467760667636791 ; 0.2106594087489728 ;
0.1769218149125021 ; 0.3064446444147922 ; 0.5919806516005373e-1 |] ;;
let hem_5_3_b_raw = [| 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 2.5 ; -1.5 |] ;;

let rk5_4_cash669_first_a = [| [| 0.25 |] ; [| 3. /. 32. ; 9. /. 32. |] ; [| 1932. /. 2197. ; -7200. /. 2197. ; 7296. /. 2197. |] ;
 [| 439. /. 216. ; -8. ; 3680. /. 513. ; -845. /. 4104. |] ; [| -8. /. 27. ; 2. ; -3544. /. 2565. ; 1859. /. 4104. ; -0.275 |] |] ;;
let rk5_4_cash669_first_b_fine = [| 16. /. 135. ; 0. ; 6656. /. 12825. ; 28561. /. 56430. ; -0.18 ; 2. /. 55. |] ;;
let rk5_4_cash669_first_b_halfdiff = [| 1. /. 360. ; 0. ; -128. /. 4275. ; -2197. /. 75240. ; 0.02 ; 0. |] ;;
(** *)
let rk5_4_cash669_first_b_raw = [| 0. ; 0. ; 1067091077380. /. 1829119027671. ; 3284168845918. /. 21339721989495. ;
 110317750789. /. 240996319200. -. 4448925830089. /. 12329617149531. ; 0.04 ; 0.2 ; 239992027043. /. 361494478800. ; 1273. /. 7800. |] ;;
rk5_4_cash669_first_b_raw.(0) <- 2. -. Matrix.vector_float_sum rk5_4_cash669_first_b_raw ;;

(** Ce qui suit pose prblème. What follows causes trouble. *)
let rk6_4_cash669_second_b_fine = [| 931. /. 6480. ; 0. ; 315392. /. 1500525. ; 371293. /. 615600. ; 0.02 ; 0.4 ; -4. /. 15. ; 85006. /. 115425. ; 239. /. 1560. |] ;;
let rk6_4_cash669_second_a = [| rk5_4_cash669_first_a.(0) ; rk5_4_cash669_first_a.(1) ; rk5_4_cash669_first_a.(2) ;
 rk5_4_cash669_first_a.(3) ; rk5_4_cash669_first_a.(4) ; Array.make 6 0. ;
 [| -119397029895. /. 151948225000. ; 78390. /. 29081. ; -51517464. /. 132821875. ; -3780749193. /. 1168832500. ; 79268193. /. 55925000. ; -11370591. /. 15379375. ; 5670. /. 2237. |] ;
 [| 23406188597. /. 8429231250. ; -62928. /. 13623. ; -31066887488. /. 5747203125. ; 164486461399. /. 8429231250. ;
 -70336084. /. 11203125. ; 185680664. /. 24646875. ; -3385330161. /. 243117160. ; 232648. /. 96795. |] |] ;;
let rk6_4_cash669_second_b_halfdiff = [| 0. ; 0. ; 0. ; 0.224620349650850 ; -0.038462277720213 ; 0.18 ; -7. /. 30. ; 0.036286203014893 ; -0.005 |] ;;
(** *)
let rk6_4_cash669_second_b_raw = rk5_4_cash669_first_b_raw ;;

let rk7_dormand_prince_a = [| [| 5.55555555555555555555555555556e-2 |] ;
 [| 2.08333333333333333333333333333e-2 ; 6.25e-2 |] ;
 [| 3.125e-2 ; 0.0 ; 9.375e-2 |] ;
 [| 3.125e-1 ; 0.0 ; -1.171875 ; 1.171875 |] ;
 [| 3.75e-2 ; 0.0 ; 0.0 ; 1.875e-1 ; 1.5e-1 |] ;
 [| 4.79101371111111111111111111111e-2 ; 0.0 ; 0.0 ; 1.12248712777777777777777777778e-1 ; -2.55056737777777777777777777778e-2 ;
 1.28468238888888888888888888889e-2 |] ;
 [| 1.6917989787292281181431107136e-2 ; 0.0 ; 0.0 ; 3.87848278486043169526545744159e-1 ;
 3.59773698515003278967008896348e-2 ; 1.96970214215666060156715256072e-1 ; -1.72713852340501838761392997002e-1 |] ;
 [| 6.90957533591923006485645489846e-2 ; 0.0 ; 0.0 ; -6.34247976728854151882807874972e-1 ; -1.61197575224604080366876923982e-1 ;
 1.38650309458825255419866950133e-1 ; 9.4092861403575626972423968413e-1 ; 2.11636326481943981855372117132e-1 |] ;
 [| 1.83556996839045385489806023537e-1 ; 0.0 ; 0.0 ; -2.46876808431559245274431575997 ; -2.91286887816300456388002572804e-1 ;
 -2.6473020233117375688439799466e-2 ; 2.84783876419280044916451825422 ; 2.81387331469849792539403641827e-1 ;
 1.23744899863314657627030212664e-1 |] ;
 [| -1.21542481739588805916051052503 ; 0.0 ; 0.0 ; 1.66726086659457724322804132886e1 ; 9.15741828416817960595718650451e-1 ;
 -6.05660580435747094755450554309 ; -1.60035735941561781118417064101e1 ; 1.4849303086297662557545391898e1 ;
 -1.33715757352898493182930413962e1 ; 5.13418264817963793317325361166 |] ;
 [| 2.58860916438264283815730932232e-1 ; 0.0 ; 0.0 ; -4.77448578548920511231011750971 ; -4.3509301377703250944070041181e-1 ;
 -3.04948333207224150956051286631 ; 5.57792003993609911742367663447 ; 6.15583158986104009733868912669 ;
 -5.06210458673693837007740643391 ; 2.19392617318067906127491429047 ; 1.34627998659334941535726237887e-1 |] ;
 [| 8.22427599626507477963168204773e-1 ; 0.0 ; 0.0 ; -1.16586732572776642839765530355e1 ; -7.57622116690936195881116154088e-1 ;
 7.13973588159581527978269282765e-1 ; 1.20757749868900567395661704486e1 ; -2.12765911392040265639082085897 ;
 1.99016620704895541832807169835 ; -2.34286471544040292660294691857e-1 ; 1.7589857770794226507310510589e-1 ; 0.0 |] |] ;;
let rk7_dormand_prince_b_raw = [| 4.17474911415302462220859284685e-2 ; 0.0 ; 0.0 ; 0.0 ; 0.0 ; -5.54523286112393089615218946547e-2
 ; 2.39312807201180097046747354249e-1 ; 7.0351066940344302305804641089e-1 ; -7.59759613814460929884487677085e-1 ;
 6.60563030922286341461378594838e-1 ; 1.58187482510123335529614838601e-1 ; -2.38109538752862804471863555306e-1 ; 2.5e-1 |] ;;
let rk7_dormand_prince_b_fine = [| 2.9553213676353496981964883112e-2 ; 0.0 ; 0.0 ; 0.0 ; 0.0 ; -8.28606276487797039766805612689e-1 ;
 3.11240900051118327929913751627e-1 ; 2.46734519059988698196468570407 ; -2.54694165184190873912738007542 ;
 1.44354858367677524030187495069 ; 7.94155958811272872713019541622e-2 ; 4.44444444444444444444444444445e-2 ; 0.0 |] ;;

let dormand_prince_8_5_3_a = [| [| 5.26001519587677318785587544488e-2 |] ; 
 [| 1.97250569845378994544595329183e-2 ; 5.91751709536136983633785987549e-2 |] ; 
 [| 2.95875854768068491816892993775e-2 ; 0. ; 8.87627564304205475450678981324e-2 |] ; 
 [| 2.41365134159266685502369798665e-1 ; 0. ; -8.84549479328286085344864962717e-1 ; 9.24834003261792003115737966543e-1 |] ; 
 [| 3.7037037037037037037037037037e-2 ; 0. ; 0. ; 1.70828608729473871279604482173e-1 ; 1.25467687566822425016691814123e-1 |] ; 
 [| 3.7109375e-2 ; 0. ; 0. ; 1.70252211019544039314978060272e-1 ; 6.02165389804559606850219397283e-2 ; -1.7578125e-2 |] ;
 [| 3.70920001185047927108779319836e-2 ; 0. ; 0. ; 1.70383925712239993810214054705e-1 ; 1.07262030446373284651809199168e-1 ; 
 -1.53194377486244017527936158236e-2 ; 8.27378916381402288758473766002e-3 |] ; 
 [| 6.24110958716075717114429577812e-1 ; 0. ; 0. ; -3.36089262944694129406857109825 ; -8.68219346841726006818189891453e-1 ; 
 2.75920996994467083049415600797e1 ; 2.01540675504778934086186788979e1 ; -4.34898841810699588477366255144e1 |] ; 
 [| 4.77662536438264365890433908527e-1 ; 0. ; 0. ; -2.48811461997166764192642586468 ;
 -5.90290826836842996371446475743e-1 ; 2.12300514481811942347288949897e1 ; 1.52792336328824235832596922938e1 ;
 -3.32882109689848629194453265587e1 ; -2.03312017085086261358222928593e-2 |] ;
 [| -9.3714243008598732571704021658e-1 ; 0. ; 0. ; 5.18637242884406370830023853209 ; 1.09143734899672957818500254654 ; 
 -8.14978701074692612513997267357 ; -1.85200656599969598641566180701e1 ; 2.27394870993505042818970056734e1 ; 
 2.49360555267965238987089396762 ; -3.0467644718982195003823669022 |] ; 
 [| 2.27331014751653820792359768449 ; 0. ; 0. ; -1.05344954667372501984066689879e1 ; -2.00087205822486249909675718444 ; 
 -1.79589318631187989172765950534e1 ; 2.79488845294199600508499808837e1 ; -2.85899827713502369474065508674 ; 
 -8.87285693353062954433549289258 ; 1.23605671757943030647266201528e1 ; 6.43392746015763530355970484046e-1 |] ;
 [| 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. |] ;
 [| 5.61675022830479523392909219681e-2 ; 0. ; 0. ; 0. ; 0. ; 0. ; 2.53500210216624811088794765333e-1 ; 
 -2.46239037470802489917441475441e-1 ; -1.24191423263816360469010140626e-1 ; 1.5329179827876569731206322685e-1 ; 
 8.20105229563468988491666602057e-3 ; 7.56789766054569976138603589584e-3 ; -8.298e-3 |] ;
 [| 3.18346481635021405060768473261e-2 ; 0. ; 0. ; 0. ; 0. ; 2.83009096723667755288322961402e-2 ; 
 5.35419883074385676223797384372e-2 ; -5.49237485713909884646569340306e-2 ; 0. ; 0. ; -1.08347328697249322858509316994e-4 ;
 3.82571090835658412954920192323e-4 ; -3.40465008687404560802977114492e-4 ; 1.41312443674632500278074618366e-1 |] ; 
 [| -4.28896301583791923408573538692e-1 ; 0. ; 0. ; 0. ; 0. ; -4.69762141536116384314449447206 ; 
 7.68342119606259904184240953878 ; 4.06898981839711007970213554331 ; 3.56727187455281109270669543021e-1 ; 0. ; 0. ;
 0. ; -1.39902416515901462129418009734e-3 ; 2.9475147891527723389556272149 ; -9.15095847217987001081870187138 |] |] ;;
let dormand_prince_8_5_3_b_fine = [| 5.42937341165687622380535766363e-2 ; 0. ; 0. ; 0. ; 0. ; 4.45031289275240888144113950566 ; 
 1.89151789931450038304281599044 ; -5.8012039600105847814672114227 ; 3.1116436695781989440891606237e-1 ; 
 -1.52160949662516078556178806805e-1 ; 2.01365400804030348374776537501e-1 ; 4.47106157277725905176885569043e-2 ; 0. ; 0. ; 0. ; 0. |] ;;
let dormand_prince_8_5_3_b_err = [| 0.1312004499419488073250102996e-1 ; 0. ; 0. ; 0. ; 0. ; -0.1225156446376204440720569753e1 ; 
-0.4957589496572501915214079952 ; 0.1664377182454986536961530415e1 ; -0.3503288487499736816886487290 ; 
0.3341791187130174790297318841 ; 0.8192320648511571246570742613e-1 ; -0.2235530786388629525884427845e-1 ; 0. ; 0. ; 0. ; 0. |] ;;
let dormand_prince_8_5_3_b_raw = Matrix.vector_float_plus dormand_prince_8_5_3_b_err dormand_prince_8_5_3_b_fine ;;




(** {C § } *)
(** 
{2 Méthodes adaptatives quelconques}
{2 Unspecialized adaptative methods}
*)
(** {C  } *)




(** The following adaptative methods fit to any Runge-Kutta method, wether it is implicit or explicit.

Les méthodes adaptatives qui suivent sont adaptées à n'importe quelle méthode de Runge-Kutta, qu'elle soit implicite ou explicite. *)

(** {C  } *)


(** {v float_end_ode_adapt methode tolerance function nsteps value beginning ending v} The [methode] must
give the final value of a solution.

La méthode [methode] doit donner la valeur finale d'une solution. *)
let rec float_end_ode_adapt = fun methode (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and y = ref y0
 and yy = ref y0
 and z = ref y0
 and zz = ref y0 in
  for i = 1 to nsteps do
   xx := !x +. step ;
   z := methode f 1 !yy !x !xx ;
   zz := methode f nsteps !yy !x !xx ;
   if abs_float ( !zz -. !z ) < tol *. abs_float !zz then yy := !zz
   else yy := float_end_ode_adapt methode tol f nsteps !yy !x !xx ;
   x := !xx ;
   y := !yy ;
  done ;
  !y ;;


(** {v vector_end_ode_adapt methode tolerance function nsteps value beginning ending v} The [methode] must
give the final value of a solution.

La méthode [methode] doit donner la valeur finale d'une solution. *)
let rec vector_end_ode_adapt = fun methode (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and y = ref ( Matrix.vector_float_copy y0 )
 and yy = ref ( Matrix.vector_float_copy y0 )
 and z = ref ( Matrix.vector_float_copy y0 )
 and zz = ref ( Matrix.vector_float_copy y0 ) in
  for i = 1 to nsteps do
   xx := !x +. step ;
   z := methode f 1 !yy !x !xx ;
   zz := methode f nsteps !yy !x !xx ;
   if ( Matrix.vector_float_norm_inf ( Matrix.vector_float_minus !zz !z ) ) < tol *. ( Matrix.vector_float_norm_inf !zz ) then yy := !zz
   else yy := vector_end_ode_adapt methode tol f nsteps !yy !x !xx ;
   x := !xx ;
   y := !yy ;
  done ;
  !y ;;


(** {v matrix_end_ode_adapt methode tolerance function nsteps value beginning ending v} The [methode] must
give the final value of a solution.

La méthode [methode] doit donner la valeur finale d'une solution. *)
let rec matrix_end_ode_adapt = fun methode (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and x = ref beginning
 and xx = ref beginning
 and y = ref ( Matrix.matrix_float_copy y0 )
 and yy = ref ( Matrix.matrix_float_copy y0 )
 and z = ref ( Matrix.matrix_float_copy y0 )
 and zz = ref ( Matrix.matrix_float_copy y0 ) in
  for i = 1 to nsteps do
   xx := !x +. step ;
   z := methode f 1 !yy !x !xx ;
   zz := methode f nsteps !yy !x !xx ;
   if ( Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus !zz !z ) ) < tol *. ( Matrix.matrix_float_norm_inf !zz ) then yy := !zz
   else yy := matrix_end_ode_adapt methode tol f nsteps !yy !x !xx ;
   x := !xx ;
   y := !yy ;
  done ;
  !y ;;



(** {v float_end_ode_bounded maxstages methode tolerance function nsteps value beginning ending v} The [methode] must only give the final value
of the solution of the ordinary differential equation.

La méthode [methode] doit donner seulement la valeur finale de la solution de l'équation différentielle ordinaire. *)
let rec float_end_ode_bounded = fun (maxstages:int) methode (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> methode f nsteps y0 beginning ending
 | _ -> 
  let step = ( ending -. beginning ) /. ( float nsteps )
  and y = ref y0
  and x = ref beginning
  and xx = ref beginning
  and yy = ref y0
  and z = ref y0
  and zz = ref y0 in
   for i = 1 to nsteps do
    xx := !x +. step ;
    z := methode f 1 !yy !x !xx ;
    zz := methode f nsteps !yy !x !xx ;
    if abs_float ( !zz -. !z ) < tol *. abs_float !zz then yy := !zz
    else yy := float_end_ode_bounded ( ( abs maxstages ) - 1 ) methode tol f nsteps !yy !x !xx ;
    x := !xx ;
    y := !yy ;
   done ;
   !y ;;


(** {v vector_end_ode_bounded maxstages methode tolerance function nsteps value beginning ending v} The [methode] must only give the final value
of the solution of the ordinary differential equation.

La méthode [methode] doit donner seulement la valeur finale de la solution de l'équation différentielle ordinaire. *)
let rec vector_end_ode_bounded = fun (maxstages:int) methode (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> methode f nsteps y0 beginning ending
 | _ -> 
  let step = ( ending -. beginning ) /. ( float nsteps )
  and y = ref ( Matrix.vector_float_copy y0 )
  and x = ref beginning
  and xx = ref beginning
  and yy = ref ( Matrix.vector_float_copy y0 )
  and z = ref ( Matrix.vector_float_copy y0 )
  and zz = ref ( Matrix.vector_float_copy y0 ) in
   for i = 1 to nsteps do
    xx := !x +. step ;
    z := methode f 1 !yy !x !xx ;
    zz := methode f nsteps !yy !x !xx ;
    if ( Matrix.vector_float_norm_inf ( Matrix.vector_float_minus !zz !z ) ) < tol *. ( Matrix.vector_float_norm_inf !zz ) then yy := !zz
    else yy := vector_end_ode_bounded ( ( abs maxstages ) - 1 ) methode tol f nsteps !yy !x !xx ;
    x := !xx ;
    y := !yy ;
   done ;
   !y ;;


(** {v matrix_end_ode_bounded maxstages methode tolerance function nsteps value beginning ending v} The [methode] must only give the final value
of the solution of the ordinary differential equation.

La méthode [methode] doit donner seulement la valeur finale de la solution de l'équation différentielle ordinaire. *)
let rec matrix_end_ode_bounded = fun (maxstages:int) methode (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> methode f nsteps y0 beginning ending
 | _ -> 
  let step = ( ending -. beginning ) /. ( float nsteps )
  and y = ref ( Matrix.matrix_float_copy y0 )
  and x = ref beginning
  and xx = ref beginning
  and yy = ref ( Matrix.matrix_float_copy y0 )
  and z = ref ( Matrix.matrix_float_copy y0 )
  and zz = ref ( Matrix.matrix_float_copy y0 ) in
   for i = 1 to nsteps do
    xx := !x +. step ;
    z := methode f 1 !yy !x !xx ;
    zz := methode f nsteps !yy !x !xx ;
    if ( Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus !zz !z ) ) < tol *. ( Matrix.matrix_float_norm_inf !zz ) then yy := !zz
    else yy := matrix_end_ode_bounded ( ( abs maxstages ) - 1 ) methode tol f nsteps !yy !x !xx ;
    x := !xx ;
    y := !yy ;
   done ;
   !y ;;



(** {v float_ode_adapt methode tolerance function nsteps value beginning ending v} *)
let rec float_ode_adapt = fun methode (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and y = Array.make ( nsteps + 1 ) y0
 and x = ref beginning
 and xx = ref beginning
 and yy = ref y0
 and z = ref y0
 and zz = ref y0 in
  for i = 1 to nsteps do
   xx := !x +. step ;
   z := ( methode f 1 !yy !x !xx ).(1) ;
   zz := ( methode f nsteps !yy !x !xx ).(nsteps) ;
   if abs_float ( !zz -. !z ) < tol *. abs_float !zz then yy := !zz
   else yy := ( float_ode_adapt methode tol f nsteps !yy !x !xx ).(nsteps) ;
   x := !xx ;
   y.(i) <- !yy ;
  done ;
  y ;;


(** {v vector_ode_adapt methode tolerance function nsteps value beginning ending v} *)
let rec vector_ode_adapt = fun methode (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and dim = Array.length y0
 and x = ref beginning
 and xx = ref beginning
 and yy = ref ( Matrix.vector_float_copy y0 )
 and z = ref ( Matrix.vector_float_copy y0 )
 and zz = ref ( Matrix.vector_float_copy y0 ) in
  let y = Array.make_matrix ( nsteps + 1 ) dim 0. in
   let row = y.(0) in
    for i = 0 to dim - 1 do
     row.(i) <- y0.(i) ;
    done ;
   for i = 1 to nsteps do
    xx := !x +. step ;
    z := ( methode f 1 !yy !x !xx ).(1) ;
    zz := ( methode f nsteps !yy !x !xx ).(nsteps) ;
    if ( Matrix.vector_float_norm_inf ( Matrix.vector_float_minus !zz !z ) ) < tol *. ( Matrix.vector_float_norm_inf !zz ) then yy := !zz
    else yy := ( vector_ode_adapt methode tol f nsteps !yy !x !xx ).(nsteps) ;
    x := !xx ;
    y.(i) <- !yy ;
   done ;
   y ;;


(** {v matrix_ode_adapt methode tolerance function nsteps value beginning ending v} *)
let rec matrix_ode_adapt = fun methode (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 let step = ( ending -. beginning ) /. ( float nsteps )
 and dim_r = Array.length y0
 and dim_c = Array.length y0.(0)
 and x = ref beginning
 and xx = ref beginning
 and yy = ref ( Matrix.matrix_float_copy y0 )
 and z = ref ( Matrix.matrix_float_copy y0 )
 and zz = ref ( Matrix.matrix_float_copy y0 ) in
  let y = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( nsteps + 1 ) 0. ) in
   let row = y.(0) in
    for i = 0 to dim_r - 1 do
     row.(i) <- y0.(i) ;
    done ;
   for i = 1 to nsteps do
    xx := !x +. step ;
    z := ( methode f 1 !yy !x !xx ).(1) ;
    zz := ( methode f nsteps !yy !x !xx ).(nsteps) ;
    if ( Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus !zz !z ) ) < tol *. ( Matrix.matrix_float_norm_inf !zz ) then yy := !zz
    else yy := ( matrix_ode_adapt methode tol f nsteps !yy !x !xx ).(nsteps) ;
    x := !xx ;
    y.(i) <- !yy ;
   done ;
   y ;;



(** {v float_ode_bounded methode tolerance function nsteps value beginning ending v} *)
let rec float_ode_bounded = fun (maxstages:int) methode (tol:float) (f:float -> float -> float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> methode f nsteps y0 beginning ending
 | _ -> 
  let step = ( ending -. beginning ) /. ( float nsteps )
  and y = Array.make ( nsteps + 1 ) y0
  and x = ref beginning
  and xx = ref beginning
  and yy = ref y0
  and z = ref y0
  and zz = ref y0 in
   for i = 1 to nsteps do
    xx := !x +. step ;
    z := ( methode f 1 !yy !x !xx ).(1) ;
    zz := ( methode f nsteps !yy !x !xx ).(nsteps) ;
    if abs_float ( !zz -. !z ) < tol *. abs_float !zz then yy := !zz
    else yy := ( float_ode_bounded ( ( abs maxstages ) - 1 ) methode tol f nsteps !yy !x !xx ).(nsteps) ;
    x := !xx ;
    y.(i) <- !yy ;
   done ;
   y ;;


(** {v vector_ode_bounded methode tolerance function nsteps value beginning ending v} *)
let rec vector_ode_bounded = fun (maxstages:int) methode (tol:float) (f:float -> float array -> float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> methode f nsteps y0 beginning ending
 | _ -> 
  let step = ( ending -. beginning ) /. ( float nsteps )
  and dim = Array.length y0
  and x = ref beginning
  and xx = ref beginning
  and yy = ref ( Matrix.vector_float_copy y0 )
  and z = ref ( Matrix.vector_float_copy y0 )
  and zz = ref ( Matrix.vector_float_copy y0 ) in
   let y = Array.make_matrix ( nsteps + 1 ) dim 0. in
    let row = y.(0) in
     for i = 0 to dim - 1 do
      row.(i) <- y0.(i) ;
     done ;
   for i = 1 to nsteps do
    xx := !x +. step ;
    z := ( methode f 1 !yy !x !xx ).(1) ;
    zz := ( methode f nsteps !yy !x !xx ).(nsteps) ;
    if ( Matrix.vector_float_norm_inf ( Matrix.vector_float_minus !zz !z ) ) < tol *. ( Matrix.vector_float_norm_inf !zz ) then yy := !zz
    else yy := ( vector_ode_bounded ( ( abs maxstages ) - 1 ) methode tol f nsteps !yy !x !xx ).(nsteps) ;
    x := !xx ;
    y.(i) <- !yy ;
   done ;
   y ;;


(** {v matrix_ode_bounded methode tolerance function nsteps value beginning ending v} *)
let rec matrix_ode_bounded = fun (maxstages:int) methode (tol:float) (f:float -> float array array -> float array array) (nsteps:int) (y0:float array array) (beginning:float) (ending:float) ->
 match maxstages with 
 | 0 -> methode f nsteps y0 beginning ending
 | _ -> 
  let step = ( ending -. beginning ) /. ( float nsteps )
  and dim_r = Array.length y0
  and dim_c = Array.length y0.(0)
  and x = ref beginning
  and xx = ref beginning
  and yy = ref ( Matrix.matrix_float_copy y0 )
  and z = ref ( Matrix.matrix_float_copy y0 )
  and zz = ref ( Matrix.matrix_float_copy y0 ) in
   let y = Array.map ( Array.make_matrix dim_r dim_c ) ( Array.make ( nsteps + 1 ) 0. ) in
    let row = y.(0) in
     for i = 0 to dim_r - 1 do
      row.(i) <- y0.(i) ;
     done ;
   for i = 1 to nsteps do
    xx := !x +. step ;
    z := ( methode f 1 !yy !x !xx ).(1) ;
    zz := ( methode f nsteps !yy !x !xx ).(nsteps) ;
    if ( Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus !zz !z ) ) < tol *. ( Matrix.matrix_float_norm_inf !zz ) then yy := !zz
    else yy := ( matrix_ode_bounded ( ( abs maxstages ) - 1 ) methode tol f nsteps !yy !x !xx ).(nsteps) ;
    x := !xx ;
    y.(i) <- !yy ;
   done ;
   y ;;


let phem_5_6_a = [| [| 0. |] ; [| 3. /. 40. ; 9. /. 40. |] ;
[| 21. /. 256. ; 45. /. 256. ; 15. /. 128. |] ;
[| 19235. /. 98784. ; -225. /. 392. ; 6235. /. 6174. ; 2755. /. 32928. |] ;
[| -293. /. 1152. ; 85. /. 48. ; -2275. /. 1908. ; 35. /. 32. ; -2835. /. 6784. |] ;
( Array.sub dormand_prince_4_5_b_fine 0 6 ) ;
[| 10752038939. /. 66307507200. ; -19259. /. 50160. ; 19581770468. /. 24023520675. ;
 9941999. /. 650073600. ; 180060831. /. 4820710400. ; -11331659. /. 439538400. ; 1. /. 76. |] |] ;;
let phem_5_6_b_raw = dormand_prince_4_5_b_raw ;;
let phem_5_6_b_fine = [| 1812923. /. 20736000. ; 0. ; 45847. /. 100170. ; 56677. /. 414720. ;
-289161. /. 13568000. ; -67507. /. 4536000. ; -3971. /. 324000. |] ;;




(** {C § } *)
(** 
{1 Équations différentielles ordinaires implicites f(x,y,y') = 0.}
{1 Implicit ordinary differential equations f(x,y,y') = 0.}
*)
(** {C  } *)




(** The resolutions presented here work if it is possible to extract an implicit function in order to give y'.

Les présentes résolutions fonctionnent s'il est possible d'extraire une fonction implicite pour donner y'. *)

(** {C  } *)


(** {v float_ode_implicit methode_ode methode_zero function guess nsteps value beginning ending v} The ordinary 
differential equations solving method [methode_ode] must apply to explicit equations y' = f(x,y) as in the following example.
The hint [guess] permits to proceed to an initial seek for y'(beginning).
Beware ! this method is polymorphic and gives at end either a vector or a real, mimicing the behaviour of [methode_ode].

{v float_ode_adapt ( float_ode_runge_kutta_adapt cash_karp_a cash_karp_b_fine cash_karp_b_raw 1e-1 ) 1e-2 v}

La méthode [methode_ode] de résolution d'équations différentielles ordinaires doit s'appliquer 
aux équations explicites y' = f(x,y) comme dans l'exemple ci-dessus. 
L'indice [guess] permet de faire une recherche initiale pour y'(beginning).
Attention ! cette méthode est polymorphe et donne à la fin soit un vecteur soit un réel
calquant le ccomportement de [methode_ode]. *)
 let float_ode_implicit = fun methode_ode methode_zero (f:float -> float -> float -> float) (guess:float) (nsteps:int) (y0:float) (beginning:float) (ending:float) ->
  let z0 = float_local_inverse methode_zero guess ( f beginning y0 ) 0. in
   let g = fun x y -> float_local_inverse methode_zero z0 ( function z -> f x y z ) 0. in
    methode_ode g nsteps y0 beginning ending ;;



(** {v vector_ode_implicit methode_ode methode_zero function guess nsteps value beginning ending v} The ordinary 
differential equations solving method [methode_ode] must apply to explicit equations y' = f(x,y) as in the following example.
The hint [guess] permits to proceed to an initial seek for y'(beginning).
Beware ! this method is polymorphic and gives at end either a vector or a real, mimicing the behaviour of [methode_ode].

{v vector_ode_adapt ( vector_ode_runge_kutta_adapt cash_karp_a cash_karp_b_fine cash_karp_b_raw 1e-1 ) 1e-2 v}

La méthode [methode_ode] de résolution d'équations différentielles ordinaires doit s'appliquer 
aux équations explicites y' = f(x,y) comme dans l'exemple ci-dessus. 
L'indice [guess] permet de faire une recherche initiale pour y'(beginning).
Attention ! cette méthode est polymorphe et donne à la fin soit un vecteur soit un réel
calquant le ccomportement de [methode_ode]. *)
 let vector_ode_implicit = fun methode_ode methode_zero (f:float -> float array -> float array -> float array) (guess:float array) (nsteps:int) (y0:float array) (beginning:float) (ending:float) ->
  let nullvector = ( Array.make ( Array.length y0 ) 0. ) in
   let z0 = vector_local_inverse methode_zero guess ( f beginning y0 ) nullvector in
    let g = fun x y -> vector_local_inverse methode_zero z0 ( function z -> f x y z ) nullvector in
     methode_ode g nsteps y0 beginning ending ;;




(** {C § } *)
(** 
{1 Distributions}
*)
(** {C  } *)




(** {v gen_dirac_mass v} *)
let gen_dirac_mass = fun a f -> f a ;;


(** {v float_dirac_mass function v} *)
let float_dirac_mass = function (f:float -> 'a) ->
 f 0. ;;

(** {v float_gen_dirac_mass position function v} *)
let float_gen_dirac_mass = fun (a:float) (f:float -> 'a) ->
 f a ;;


(** {v float_dirac_comb size function v} *)
let float_dirac_comb = fun (size:int) (f:float -> float) ->
 let accu = ref ( f 0. ) in
  for i = 1 to abs size do
   accu := !accu +. f ( float i ) +. f ( float ( - i ) ) ;
  done ;
  !accu ;;

(** {v float_vector_dirac_comb size function v} *)
let float_vector_dirac_comb = fun (size:int) (f:float -> float array) ->
 let accu = ref ( f 0. ) in
  for i = 1 to abs size do
   accu := Matrix.vector_float_plus !accu ( Matrix.vector_float_plus ( f ( float i ) ) ( f ( float ( - i ) ) ) ) ;
  done ;
  !accu ;;

(** {v float_matrix_dirac_comb size function v} *)
let float_matrix_dirac_comb = fun (size:int) (f:float -> float array array) ->
 let accu = ref ( f 0. ) in
  for i = 1 to abs size do
   accu := Matrix.matrix_float_plus !accu ( Matrix.matrix_float_plus ( f ( float i ) ) ( f ( float ( - i ) ) ) ) ;
  done ;
  !accu ;;


(** {v vector_dirac_mass dimension function v} *)
let vector_dirac_mass = fun n (f:float array -> 'a) ->
 f ( Array.make n 0. ) ;;

(** {v vector_gen_dirac_mass position function v} *)
let vector_gen_dirac_mass = fun (a:float array) (f:float array -> 'a) ->
 f a ;;

(** {v matrix_dirac_mass n_rows n_columns function v} *)
let matrix_dirac_mass = fun r c (f:float array array -> 'a) ->
 f ( Array.make_matrix r c 0. ) ;;

(** {v matrix_gen_dirac_mass position function v} *)
let matrix_gen_dirac_mass = fun (a:float array array) (f:float array array -> 'a) ->
 f a ;;


(** {v float_dirac_family_bell parameter real v} *)
let float_dirac_family_bell = fun (n:float) (x:float) ->
 inv_sqrt_doublepi *. ( gauss_bell ( x *. n ) ) *. n ;;

(** {v float_dirac_family_rectangle parameter real v} *)
let float_dirac_family_rectangle = fun (n:float) (x:float) ->
 if abs_float x <= 1. /. n then 0.5 *. n
 else 0. ;;

(** {v float_dirac_family_triangle parameter real v} *)
let float_dirac_family_triangle = fun (n:float) (x:float) ->
 let xx = abs_float x and nn = 1. /. n in
  if xx <= nn then n *. n *. ( nn -. xx )
  else 0. ;;


(** {v float_distrib_deriv v} *)
let float_distrib_deriv = fun methode distribution ->
 function (f:'a -> float) -> -. ( distribution ( methode f ) ) ;;

(** {v vector_distrib_deriv v} *)
let vector_distrib_deriv = fun methode distribution ->
 function (f:'a -> float array) -> Matrix.vector_float_opp ( distribution ( methode f ) ) ;;

(** {v matrix_distrib_deriv v} *)
let matrix_distrib_deriv = fun methode distribution ->
 function (f:'a -> float array array) -> Matrix.matrix_float_opp ( distribution ( methode f ) ) ;;


(** {v float_variable_translation value function real v} *)
let float_variable_translation = fun (a:float) (f:float -> 'a) (x:float) ->
 f ( x +. a ) ;;

(** {v vector_float_variable_translation value function real v} *)
let vector_float_variable_translation = fun (a:float array) (f:float array -> 'a) (x:float array) ->
 f ( Matrix.vector_float_plus x a ) ;;

(** {v matrix_float_variable_translation value function real v} *)
let matrix_float_variable_translation = fun (a:float array array) (f:float array array -> 'a) (x:float array array) ->
 f ( Matrix.matrix_float_plus x a ) ;;


(** {v float_distrib_transform v} *)
let float_distrib_transform = fun transform distribution ->
 ( function (f:'a -> 'b) -> ( distribution ( transform f ) ) ) ;;




(** {C § } *)
(** 
{1 Constructions supplémentaires}
{1 Further constructions}
*)
(** {C  } *)




(** {v normal_cumul_distribution real v} *)
let normal_cumul_distribution = function (x:float) ->
 ( sqrt inv_doublepi ) *. float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 gauss_bell (-38.7) x ;;

(** {v normal_cumul_distribution_complem real v} *)
let normal_cumul_distribution_complem = function (x:float) ->
 ( sqrt inv_doublepi ) *. float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 gauss_bell x 38.7 ;;

(** {v erf real v} *)
let erf = function (x:float) ->
 let f = function y -> gauss_bell ( y *. sqrt_of_2) in
 2. *. inv_sqrt_pi *. float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 f 0. x ;;

(** {v erf_complem real v} *)
let erf_complem = function (x:float) ->
 let f = function y -> gauss_bell ( y *. sqrt_of_2) in
 2. *. inv_sqrt_pi *. float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 f x 38.7 ;;


(** {v normal_quantile real v} This function must be equal to [probit].

Cette fonction doit être égale à [probit]. *)
let normal_quantile = function (x:float) ->
 let g = function y -> x -. normal_cumul_distribution y in
  float_zero_general ( float_richardson_binary_deriv 2 1e-3 ) 2 5 g 0.5 ;;


(** {v probit real v} This function must be equal to [normal_quantile].

Cette fonction doit être égale à [normal_quantile]. *)
let probit = function (x:float) ->
 let g = function y -> 2. *. x -. erf_complem ( y ) in
  (-. sqrt_of_2 ) *. ( float_zero_general ( float_richardson_binary_deriv 2 1e-3 ) 2 5 g 0.5 ) ;;


(** {v sin_int real v} *)
let sin_int = function (x:float) ->
 float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 sinc 0. x ;;

(** {v si real v} *)
let si = function (x:float) ->
 float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 sinc 1000. x ;;

(** {v shi real v} *)
let shi = function (x:float) ->
 let f = function y ->
  begin
   match y with
   | 0. -> 1.
   | _ -> ( sinh y ) /. y
  end in
   float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 f 0. x ;;


(** {v elliptic_integral_first_kind k x v} *)
let elliptic_integral_first_kind = fun (k:float) (x:float) ->
 let f = function y ->
  let z = k *. ( sin y ) in
   1. /. ( sqrt ( 1. -. z *. z ) ) in
    float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 f 0. x ;;

(** {v elliptic_integral_second_kind k x v} *)
let elliptic_integral_second_kind = fun (k:float) (x:float) ->
 let f = function y ->
  let z = k *. ( sin y ) in
   sqrt ( 1. -. z *. z ) in
    float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 f 0. x ;;

(** {v elliptic_integral_secondBis_kind k x v} *)
let elliptic_integral_secondBis_kind = fun (k:float) (x:float) ->
 let f = function y ->
  let w = sin y in
   let z = k *. w in
    ( w *. w )  /. ( sqrt ( 1. -. z *. z ) ) in
     float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 f 0. x ;;

(** {v elliptic_integral_third_kind k c x v} *)
let elliptic_integral_third_kind = fun (k:float) (c:float) (x:float) ->
 let f = function y ->
  let w = sin y in
   let z = k *. w in
    1. /. ( ( w -. c ) *. ( sqrt ( 1. -. z *. z ) ) ) in
     float_int_multi_adapt float_simple_step_gauss_kronrod 100 1e-3 f 0. x ;;


(** {v elliptic_integral methode coefficients_up coefficients_down root_coefficients x v} The integration method [methode]
must contain all the parameters.

La méthode d'intégration [methode] doit contenir tous les paramètres. *)
let elliptic_integral = fun methode (num:float array array) (denom:float array array) (c:float array) (a:float) (x:float) ->
 let f = function y ->
  let z = float_polynomial_1 c y in
   float_rational_2 num denom y z in
    methode f a x ;;



(** {v expm_ode methode nsteps matrix v} *)
let expm_ode = fun methode (nsteps:int) (m:float array array) ->
 let f = fun x y -> Matrix.matrix_float_prod m y in
  methode f nsteps ( Matrix.eye_float m ) 0. 1. ;;


(** {v expm_ode_bis methode nsteps matrix v} *)
let expm_ode_bis = fun methode (nsteps:int) (m:float array array) ->
 let f = fun x y -> Matrix.matrix_float_plus ( Matrix.matrix_float_prod m y ) ( Matrix.matrix_float_prod y m ) in
  methode f nsteps ( Matrix.eye_float m ) 0. 0.5 ;;


(** {v slow_tune_expm threshold matrix v} *)
let slow_tune_expm = fun (threshold:float) (m:float array array) ->
 let n = Array.length m
 and ww = ref m
 and mm = ref m
 and mmm = ref m
 and z = Matrix.float_trace m in
  let zz = z /. ( float n ) in
   if zz <> 0. then
    mm := Matrix.matrix_float_minus m ( Matrix.scal_float n n zz )
   else
    mm := m ;
   let zzz = Matrix.matrix_float_norm_inf !mm
(** The bias is tuned by experimenatl adjustment.

Le biais est ajusté empiriquement. *)
   and biais = Util.float_pos_part ( 6. -. ( floor ( log_bin ( float n ) ) ) ) in
    let iteration = Util.float_pos_part ( ( ceil ( log_bin zzz ) ) -. biais ) in
     if iteration >= 1. then
      mmm := Matrix.matrix_float_scal_mult ( 2. ** ( -. iteration ) ) !mm
     else
      mmm := !mm ;
     let methode = matrix_end_ode_runge_kutta_simple_adapt dormand_prince_8_5_3_a dormand_prince_8_5_3_b_fine dormand_prince_8_5_3_b_raw threshold in
      let w = expm_ode methode 15 !mmm in
       ww := w ;
       if iteration <> 0. then
        begin
         for i = 1 to int_of_float iteration do
          ww := Matrix.matrix_float_prod !ww !ww ;
         done ; 
         ww := Matrix.matrix_float_scal_mult ( exp zz ) !ww ;
        end ;
        !ww ;;


(** {v slow_tune_expm_bis threshold matrix v} *)
let slow_tune_expm_bis = fun (threshold:float) (m:float array array) ->
 let n = Array.length m
 and ww = ref m
 and mm = ref m
 and mmm = ref m
 and z = Matrix.float_trace m in
  let zz = z /. ( float n ) in
   if zz <> 0. then
    mm := Matrix.matrix_float_minus m ( Matrix.scal_float n n zz )
   else
    mm := m ;
   let zzz = Matrix.matrix_float_norm_inf !mm
(** The bias is tuned by experimenatl adjustment.

Le biais est ajusté empiriquement. *)
   and biais = Util.float_pos_part ( 6. -. ( floor ( log_bin ( float n ) ) ) ) in
    let iteration = Util.float_pos_part ( ( ceil ( log_bin zzz ) ) -. biais ) in
     if iteration >= 1. then
      mmm := Matrix.matrix_float_scal_mult ( 2. ** ( -. iteration ) ) !mm
     else
      mmm := !mm ;
     let methode = matrix_end_ode_runge_kutta_simple_adapt dormand_prince_8_5_3_a dormand_prince_8_5_3_b_fine dormand_prince_8_5_3_b_raw threshold in
      let w = expm_ode_bis methode 15 !mmm in
       ww := w ;
       if iteration <> 0. then
        begin
         for i = 1 to int_of_float iteration do
          ww := Matrix.matrix_float_prod !ww !ww ;
         done ; 
         ww := Matrix.matrix_float_scal_mult ( exp zz ) !ww ;
        end ;
        !ww ;;


(** {v tune_expm threshold matrix v} *)
let tune_expm = fun (threshold:float) (m:float array array) ->
 let n = Array.length m
 and ww = ref m
 and mm = ref m
 and mmm = ref m
 and z = Matrix.float_trace m in
  let zz = z /. ( float n ) in
   if zz <> 0. then
    mm := Matrix.matrix_float_minus m ( Matrix.scal_float n n zz )
   else
    mm := m ;
   let zzz = Matrix.matrix_float_norm_inf !mm
(** The bias is tuned by experimenatl adjustment.

Le biais est ajusté empiriquement. *)
   and biais = Util.float_pos_part ( 6. -. ( floor ( log_bin ( float n ) ) ) ) in
    let iteration = Util.float_pos_part ( ( ceil ( log_bin zzz ) ) -. biais ) in
     if iteration >= 1. then
      mmm := Matrix.matrix_float_scal_mult ( 2. ** ( -. iteration ) ) !mm
     else
      mmm := !mm ;
     let methode = matrix_end_ode_runge_kutta_simple_adapt rk7_dormand_prince_a rk7_dormand_prince_b_fine rk7_dormand_prince_b_raw threshold in
      let w = expm_ode methode 36 !mmm in
       ww := w ;
       if iteration <> 0. then
        begin
         for i = 1 to int_of_float iteration do
          ww := Matrix.matrix_float_prod !ww !ww ;
         done ; 
         ww := Matrix.matrix_float_scal_mult ( exp zz ) !ww ;
        end ;
        !ww ;;


(** {v tune_expm_bis threshold matrix v} *)
let tune_expm_bis = fun (threshold:float) (m:float array array) ->
 let n = Array.length m
 and ww = ref m
 and mm = ref m
 and mmm = ref m
 and z = Matrix.float_trace m in
  let zz = z /. ( float n ) in
   if zz <> 0. then
    mm := Matrix.matrix_float_minus m ( Matrix.scal_float n n zz )
   else
    mm := m ;
   let zzz = Matrix.matrix_float_norm_inf !mm
(** The bias is tuned by experimenatl adjustment.

Le biais est ajusté empiriquement. *)
   and biais = Util.float_pos_part ( 6. -. ( floor ( log_bin ( float n ) ) ) ) in
    let iteration = Util.float_pos_part ( ( ceil ( log_bin zzz ) ) -. biais ) in
     if iteration >= 1. then
      mmm := Matrix.matrix_float_scal_mult ( 2. ** ( -. iteration ) ) !mm
     else
      mmm := !mm ;
     let methode = matrix_end_ode_runge_kutta_simple_adapt rk7_dormand_prince_a rk7_dormand_prince_b_fine rk7_dormand_prince_b_raw threshold in
      let w = expm_ode_bis methode 36 !mmm in
       ww := w ;
       if iteration <> 0. then
        begin
         for i = 1 to int_of_float iteration do
          ww := Matrix.matrix_float_prod !ww !ww ;
         done ; 
         ww := Matrix.matrix_float_scal_mult ( exp zz ) !ww ;
        end ;
        !ww ;;


(** {v slow_direct_expm matrix v} *)
let slow_direct_expm = function (m:float array array) ->
 let n = Array.length m
 and ww = ref m
 and mm = ref m
 and mmm = ref m
 and z = Matrix.float_trace m in
  let zz = z /. ( float n )
  and nn = float n in
   if zz <> 0. then
    mm := Matrix.matrix_float_minus m ( Matrix.scal_float n n zz )
   else
    mm := m ;
   let zzz = Matrix.matrix_float_norm_inf !mm
(** The bias is tuned by experimenatl adjustment.

Le biais est ajusté empiriquement. *)
   and biais = Util.float_pos_part ( ceil ( 9.9 -. ( log_bin nn ) ) ) in
    let iteration = Util.float_pos_part ( ( ceil ( log_bin zzz ) ) -. biais ) in
     if iteration >= 1. then
      mmm := Matrix.matrix_float_scal_mult ( 2. ** ( -. iteration ) ) !mm
     else
      mmm := !mm ;
(** The threshold is tuned by experimenatl adjustment.

Le seuil est ajusté empiriquement. *)
     let threshold = min 9e-1 ( zzz *. epsilon_float *. 2. ** ( 1e1 *. nn ) ) in
      let methode = matrix_end_ode_runge_kutta_simple_adapt dormand_prince_8_5_3_a dormand_prince_8_5_3_b_fine dormand_prince_8_5_3_b_raw threshold in
       let w = [| expm_ode methode 14 !mmm ; expm_ode methode 20 !mmm ; expm_ode methode 28 !mmm |] in
        ww := Matrix.matrix_trans_float_approx w ;
        if iteration <> 0. then
         begin
          for i = 1 to int_of_float iteration do
           ww := Matrix.matrix_float_prod !ww !ww ;
          done ; 
          ww := Matrix.matrix_float_scal_mult ( exp zz ) !ww ;
         end ;
         !ww ;;


(** {v slow_direct_expm_bis matrix v} *)
let slow_direct_expm_bis = function (m:float array array) ->
 let n = Array.length m
 and ww = ref m
 and mm = ref m
 and mmm = ref m
 and z = Matrix.float_trace m in
  let zz = z /. ( float n )
  and nn = float n in
   if zz <> 0. then
    mm := Matrix.matrix_float_minus m ( Matrix.scal_float n n zz )
   else
    mm := m ;
   let zzz = Matrix.matrix_float_norm_inf !mm
(** The bias is tuned by experimenatl adjustment.

Le biais est ajusté empiriquement. *)
   and biais = Util.float_pos_part ( ceil ( 9.9 -. ( log_bin nn ) ) ) in
    let iteration = Util.float_pos_part ( ( ceil ( log_bin zzz ) ) -. biais ) in
     if iteration >= 1. then
      mmm := Matrix.matrix_float_scal_mult ( 2. ** ( -. iteration ) ) !mm
     else
      mmm := !mm ;
(** The threshold is tuned by experimenatl adjustment.

Le seuil est ajusté empiriquement. *)
     let threshold = min 9e-1 ( zzz *. epsilon_float *. 2. ** ( 1e1 *. nn ) ) in
      let methode = matrix_end_ode_runge_kutta_simple_adapt dormand_prince_8_5_3_a dormand_prince_8_5_3_b_fine dormand_prince_8_5_3_b_raw threshold in
       let w = [| expm_ode_bis methode 14 !mmm ; expm_ode_bis methode 20 !mmm ; expm_ode_bis methode 28 !mmm |] in
        ww := Matrix.matrix_trans_float_approx w ;
        if iteration <> 0. then
         begin
          for i = 1 to int_of_float iteration do
           ww := Matrix.matrix_float_prod !ww !ww ;
          done ; 
          ww := Matrix.matrix_float_scal_mult ( exp zz ) !ww ;
         end ;
         !ww ;;


(** {v direct_expm matrix v} *)
let direct_expm = function (m:float array array) ->
 let n = Array.length m
 and ww = ref m
 and mm = ref m
 and mmm = ref m
 and z = Matrix.float_trace m in
  let zz = z /. ( float n )
  and nn = float n in
   if zz <> 0. then
    mm := Matrix.matrix_float_minus m ( Matrix.scal_float n n zz )
   else
    mm := m ;
   let zzz = Matrix.matrix_float_norm_inf !mm
(** The bias is tuned by experimenatl adjustment.

Le biais est ajusté empiriquement. *)
   and biais = Util.float_pos_part ( 6. -. ( floor ( log_bin nn ) ) ) in
    let iteration = Util.float_pos_part ( ( ceil ( log_bin zzz ) ) -. biais ) in
     if iteration >= 1. then
      mmm := Matrix.matrix_float_scal_mult ( 2. ** ( -. iteration ) ) !mm
     else
      mmm := !mm ;
(** The threshold is tuned by experimenatl adjustment.

Le seuil est ajusté empiriquement. *)
     let threshold = min 9e-1 ( zzz *. epsilon_float *. 2. ** nn ) in
      let methode = matrix_end_ode_runge_kutta_simple_adapt rk7_dormand_prince_a rk7_dormand_prince_b_fine rk7_dormand_prince_b_raw threshold in
       let w = expm_ode methode 36 !mmm in
        ww := w ;
        if iteration <> 0. then
         begin
          for i = 1 to int_of_float iteration do
           ww := Matrix.matrix_float_prod !ww !ww ;
          done ; 
          ww := Matrix.matrix_float_scal_mult ( exp zz ) !ww ;
         end ;
         !ww ;;


(** {v direct_expm_bis matrix v} *)
let direct_expm_bis = function (m:float array array) ->
 let n = Array.length m
 and ww = ref m
 and mm = ref m
 and mmm = ref m
 and z = Matrix.float_trace m in
  let zz = z /. ( float n )
  and nn = float n in
   if zz <> 0. then
    mm := Matrix.matrix_float_minus m ( Matrix.scal_float n n zz )
   else
    mm := m ;
   let zzz = Matrix.matrix_float_norm_inf !mm
(** The bias is tuned by experimenatl adjustment.

Le biais est ajusté empiriquement. *)
   and biais = Util.float_pos_part ( 6. -. ( floor ( log_bin nn ) ) ) in
    let iteration = Util.float_pos_part ( ( ceil ( log_bin zzz ) ) -. biais ) in
     if iteration >= 1. then
      mmm := Matrix.matrix_float_scal_mult ( 2. ** ( -. iteration ) ) !mm
     else
      mmm := !mm ;
(** The threshold is tuned by experimenatl adjustment.

Le seuil est ajusté empiriquement. *)
     let threshold = min 9e-1 ( zzz *. epsilon_float *. 2. ** nn ) in
      let methode = matrix_end_ode_runge_kutta_simple_adapt rk7_dormand_prince_a rk7_dormand_prince_b_fine rk7_dormand_prince_b_raw threshold in
       let w = expm_ode_bis methode 36 !mmm in
        ww := w ;
        if iteration <> 0. then
         begin
          for i = 1 to int_of_float iteration do
           ww := Matrix.matrix_float_prod !ww !ww ;
          done ; 
          ww := Matrix.matrix_float_scal_mult ( exp zz ) !ww ;
         end ;
         !ww ;;


(** {v slow_expm matrix v} *)
let slow_expm = function (m:float array array) ->
 let mm = Matrix.matrix_float_scal_mult 0.5 m
 and mmm = Matrix.matrix_float_scal_mult (-0.5) m in
  let ww = slow_direct_expm mm
  and www = slow_direct_expm mmm in
   let inverse = Matrix.aggressive_inv www in
    Matrix.matrix_float_prod inverse ww ;;

(** {v slow_expm_bis matrix v} *)
let slow_expm_bis = function (m:float array array) ->
 let mm = Matrix.matrix_float_scal_mult 0.5 m
 and mmm = Matrix.matrix_float_scal_mult (-0.5) m in
  let ww = slow_direct_expm_bis mm
  and www = slow_direct_expm_bis mmm in
   let inverse = Matrix.aggressive_inv www in
    Matrix.matrix_float_prod inverse ww ;;


(** {v slow_expm_ter matrix v} *)
let slow_expm_ter = function (m:float array array) ->
 let mm = Matrix.matrix_float_scal_mult 0.25 m
 and mmm = Matrix.matrix_float_scal_mult (-0.5) m in
  let ww = slow_direct_expm mm
  and www = slow_direct_expm mmm in
   let inverse = Matrix.aggressive_inv www in
    Matrix.matrix_float_triple_prod ww inverse ww ;;

(** {v slow_expm_quater matrix v} *)
let slow_expm_quater = function (m:float array array) ->
 let mm = Matrix.matrix_float_scal_mult 0.25 m
 and mmm = Matrix.matrix_float_scal_mult (-0.5) m in
  let ww = slow_direct_expm_bis mm
  and www = slow_direct_expm_bis mmm in
   let inverse = Matrix.aggressive_inv www in
    Matrix.matrix_float_triple_prod ww inverse ww ;;





(** {v expm matrix v} *)
let expm = function (m:float array array) ->
 let mm = Matrix.matrix_float_scal_mult 0.5 m
 and mmm = Matrix.matrix_float_scal_mult (-0.5) m in
  let ww = direct_expm mm
  and www = direct_expm mmm in
   let inverse = Matrix.clean_inv www in
    Matrix.matrix_float_prod inverse ww ;;

(** {v expm_bis matrix v} *)
let expm_bis = function (m:float array array) ->
 let mm = Matrix.matrix_float_scal_mult 0.5 m
 and mmm = Matrix.matrix_float_scal_mult (-0.5) m in
  let ww = direct_expm_bis mm
  and www = direct_expm_bis mmm in
   let inverse = Matrix.clean_inv www in
    Matrix.matrix_float_prod inverse ww ;;


(** {v expm_ter matrix v} *)
let expm_ter = function (m:float array array) ->
 let mm = Matrix.matrix_float_scal_mult 0.25 m
 and mmm = Matrix.matrix_float_scal_mult (-0.5) m in
  let ww = direct_expm mm
  and www = direct_expm mmm in
   let inverse = Matrix.clean_inv www in
    Matrix.matrix_float_triple_prod ww inverse ww ;;

(** {v expm_quater matrix v} *)
let expm_quater = function (m:float array array) ->
 let mm = Matrix.matrix_float_scal_mult 0.25 m
 and mmm = Matrix.matrix_float_scal_mult (-0.5) m in
  let ww = direct_expm_bis mm
  and www = direct_expm_bis mmm in
   let inverse = Matrix.clean_inv www in
    Matrix.matrix_float_triple_prod ww inverse ww ;;



(** {v curv_absc methode_diff methode_int function x y v} The (unidimensional) derivating method [methode_diff] must contain the parameters, including the step.
The integrating method [methode_int] must contain all the needed parameters.

La méthode de dérivation (unidimensionnelle) [methode_diff] doit contenir les paramètres, y compris le pas.
La méthode d'intégration [methode_int] doit contenir tous les paramaètres nécessaires. *)
let curv_absc = fun methode_diff methode_int (f:float -> float array) (x:float) (y:float) ->
 let g = vector_speed methode_diff f in
  let h = function z -> Matrix.vector_float_norm_2 ( g z ) in
   methode_int h x y ;;


(** {v developpante methode_diff methode_int function parameter beginning real v} The (unidimensional) derivating method [methode_diff] must contain the parameters, including the step.
The integrating method [methode_int] must contain all the needed parameters.

La méthode de dérivation (unidimensionnelle) [methode_diff] doit contenir les paramètres, y compris le pas.
La méthode d'intégration [methode_int] doit contenir tous les paramaètres nécessaires. *)
let developpante = fun methode_diff methode_int (f:float -> float array) (parameter:float) (beginning:float) (x:float) ->
 let g = vector_speed methode_diff f in
  let h = function z -> Matrix.vector_float_norm_2 ( g z ) in
   let k = function z -> Matrix.vector_float_scal_left_div ( h z ) ( g z )
   and s = methode_int h beginning x in
    Matrix.vector_float_plus ( f x ) ( Matrix.vector_float_scal_mult ( parameter -. s ) ( k x ) ) ;;


(** {v prescribed_curvature_2 methode function nsteps beginning ending v} The vectorial ordinary differential equations solving method [methode] may either
give all the steps or only the final value.

La méthode de résolution d'équations différentielles ordinaires vectorielles [methode] peut soit donner tous les pas soit donner seulement la valeur finale. *)
let prescribed_curvature_2 = fun methode (kappa:float -> float) nsteps (beginning:float) (ending:float) ->
 let f = fun x v -> [| v.(2) ; v.(3) ; -. ( kappa x ) *. v.(3) ; ( kappa x ) *. v.(2) |] in
  methode f nsteps [| 0. ; 0. ; 1. ; 0. |] beginning ending ;;


(** {v prescribed_curvature_torsion_3 methode function nsteps beginning ending v} The vectorial ordinary differential equations solving method [methode] may either
give all the steps or only the final value.

La méthode de résolution d'équations différentielles ordinaires vectorielles [methode] peut soit donner tous les pas soit donner seulement la valeur finale. *)
let prescribed_curvature_torsion_3 = fun methode (kappa:float -> float) (tau:float -> float) nsteps (beginning:float) (ending:float) ->
 let f = fun x v -> [| v.(3) ; v.(4) ; v.(5) ; ( kappa x ) *. v.(4) ; -. ( kappa x ) *. v.(3) +. ( tau x ) *. v.(5) ; -. ( tau x ) *. v.(4) |] in
  methode f nsteps [| 0. ; 0. ; 0. ; 1. ; 0. ; 0. |] beginning ending ;;


(** {v prescribed_multicurvature methode function nsteps beginning ending v} The vectorial ordinary differential equations solving method [methode] may either
give all the steps or only the final value.

La méthode de résolution d'équations différentielles ordinaires vectorielles [methode] peut soit donner tous les pas soit donner seulement la valeur finale. *)
let prescribed_multicurvature = fun methode (kappa:float -> float array) nsteps (beginning:float) (ending:float) ->
 let l = Array.length ( kappa beginning ) in
  let ll = l + 1
  and lll = l - 1 in
   let r = 2 * ll in
    let f = fun x v ->
     begin
      let w = Array.make r 0. in
       for i = 0 to l do
        w.(i) <- v.( ll + i ) ;
       done ;
       w.(ll) <- ( kappa x ).(0) *. v.( ll + 1 ) ;
       w.( ll + l ) <- -. ( kappa x ).(lll) *. v.( l + l ) ;
       for i = 1 to lll do
        w.( ll + i ) <- -. ( kappa x ).( i - 1 ) *. v.( l + i ) +. ( kappa x ).(i) *. v.( ll + i + 1 ) ;
       done ;
      w
     end
    and init = Array.make r 0. in
     init.(ll) <- 1. ;
     methode f nsteps init beginning ending ;;


(** {v clothoid methode real v} The method [methode] is that of one-dimensional integration.

La méthode [methode] est celle d'intégration unidimensionnelle. *)
let clothoid = fun methode (x:float) ->
 let f = function s -> cos ( s *. s )
 and g = function s -> sin (s *. s ) in
  [| methode f 0. x ; methode g 0. x |] ;;




(** {C § § § } *)




end ;;

