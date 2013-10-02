



module Sci = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module:

- constructions in order to do the elementary operations with complex numbers in scientific notation whose mantissas are greater than fifty three bits,

+ either with absolute precision, mantissas being big rational numbers, 
+ or with mantissas truncated at 1024 bits,

- common convergence acceleration methods,

- functions on complex numbers in scientific notation whose mantissas are truncated at 1024 bits
(the last figures of the mantissa may be wrong):

+ root functions,
+ solutions of polynomial equations of degree 2 or 3 or 4,
+ trigonometric functions.


{2 Conventions}


A (complex) number in scientific notation is an array [[| x ; y ; w |]] of three numbers of type [Num.num] 
which represent [z = ( x + i y ) * 2 ^ w]. They satisfy [x = y = w = 0] or [1 <= x ^ 2 + y ^ 2 < 4], and [w] is a big integer.


Some constants come from the module {{!module:Data.Classical} Data.Classical}.


This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module :

- des constructions pour effectuer les opérations élémentaires avec des nombres complexes en notation scientifique dont la mantisse dépasse cinquante-trois bits :

+ en précision absolue, les mantisses étant de grands nombres rationnels,
+ ou avec des mantisses tronquées à 1024 bits,

- des méthodes usuelles d'accélération de convergence,

- des fonctions sur des nombres complexes en notation scientifique dont la mantisse est tronquée à 1024 bits
(les derniers chiffres de la mantisse peuvent être faux) : 

+ fonctions racines,
+ solutions d'équations polynomiales de degré 2, 3 ou 4,
+ fonctions trigonométriques.


{2 Conventions}


Un nombre (complexe) en notation scientifique est un tableau [[| x ; y ; w |]] de trois nombres de type [Num.num] 
qui représentent [z = ( x + i y ) * 2 ^ w]. Ils vérifient [x = y = w = 0] ou bien [1 <= x ^ 2 + y ^ 2 < 4], et [w] est un grand entier.


Des constantes proviennent du module {{!module:Data.Classical} Data.Classical}.


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
open Data ;;
open Hash ;;
open Matrix ;;




(** {C § } *)
(** 
{1 Constantes diverses et mises en forme}
{1 Miscellaneous constants and formatting}
*)
(** {C  } *)




(** {v num_0 v} *)
let num_0 = Num.num_of_int 0 ;;

(** {v num_1 v} *)
let num_1 = Num.num_of_int 1 ;;

(** {v num_minus_1 v} *)
let num_minus_1 = Num.minus_num num_1 ;;

(** {v num_2 v} *)
let num_2 = Num.num_of_int 2 ;;

(** {v num_minus_2 v} *)
let num_minus_2 = Num.num_of_int ( - 2 ) ;;

(** {v num_3 v} *)
let num_3 = Num.num_of_int 3 ;;

(** {v num_minus_3 v} *)
let num_minus_3 = Num.num_of_int ( - 3 ) ;;

(** {v num_3_over_2 v} *)
let num_3_over_2 = Num.div_num num_3 num_2 ;;

(** {v num_minus_3_over_2 v} *)
let num_minus_3_over_2 = Num.div_num num_minus_3 num_2 ;;

(** {v num_4 v} *)
let num_4 = Num.num_of_int 4 ;;

(** {v num_minus_4 v} *)
let num_minus_4 = Num.num_of_int ( - 4 )

(** {v num_5 v} *)
let num_5 = Num.num_of_int 5

(** {v num_minus_5 v} *)
let num_minus_5 = Num.num_of_int ( - 5 )

(** {v num_6 v} *)
let num_6 = Num.num_of_int 6

(** {v num_minus_6 v} *)
let num_minus_6 = Num.num_of_int ( - 6 )

(** {v num_7 v} *)
let num_7 = Num.num_of_int 7

(** {v num_minus_7 v} *)
let num_minus_7 = Num.num_of_int ( - 7 )

(** {v num_8 v} *)
let num_8 = Num.num_of_int 8

(** {v num_minus_8 v} *)
let num_minus_8 = Num.num_of_int ( - 8 )

(** {v num_9 v} *)
let num_9 = Num.num_of_int 9

(** {v num_minus_9 v} *)
let num_minus_9 = Num.num_of_int ( - 9 )

(** {v num_10 v} *)
let num_10 = Num.num_of_int 10 ;;

(** {v num_minus_10 v} *)
let num_minus_10 = Num.num_of_int ( - 10 )

(** {v num_12 v} *)
let num_12 = Num.num_of_int 12 ;;

(** {v num_minus_12 v} *)
let num_minus_12 = Num.num_of_int ( - 12 )

(** {v num_16 v} *)
let num_16 = Num.num_of_int 16 ;;

(** {v num_minus_16 v} *)
let num_minus_16 = Num.num_of_int ( - 16 )

(** {v num_27 v} *)
let num_27 = Num.num_of_int 27 ;;

(** {v num_minus_27 v} *)
let num_minus_27 = Num.num_of_int ( - 27 ) ;;

(** {v num_32 v} *)
let num_32 = Num.num_of_int 32 ;;

(** {v num_minus_32 v} *)
let num_minus_32 = Num.num_of_int ( - 32 )

(** {v num_64 v} *)
let num_64 = Num.num_of_int 64 ;;

(** {v num_minus_64 v} *)
let num_minus_64 = Num.num_of_int ( - 64 )

(** {v num_128 v} *)
let num_128 = Num.num_of_int 128 ;;

(** {v num_minus_128 v} *)
let num_minus_128 = Num.num_of_int ( - 128 )

(** {v num_256 v} *)
let num_256 = Num.num_of_int 256 ;;

(** {v num_minus_256 v} *)
let num_minus_256 = Num.num_of_int ( - 256 )

(** {v num_512 v} *)
let num_512 = Num.num_of_int 512 ;;

(** {v num_minus_512 v} *)
let num_minus_512 = Num.num_of_int ( - 512 )

(** {v num_1024 v} *)
let num_1024 = Num.num_of_int 1024 ;;

(** {v num_minus_1024 v} *)
let num_minus_1024 = Num.num_of_int ( - 1024 )

(** {v num_2048 v} *)
let num_2048 = Num.num_of_int 2048 ;;

(** {v num_minus_2048 v} *)
let num_minus_2048 = Num.num_of_int ( - 2048 )


(** {v num_2_pow_16 v} *)
let num_2_pow_16 = Num.power_num num_2 num_16 ;;

(** {v num_2_pow_32 v} *)
let num_2_pow_32 = Num.power_num num_2 num_32 ;;

(** {v num_2_pow_64 v} *)
let num_2_pow_64 = Num.power_num num_2 num_64 ;;

(** {v num_2_pow_128 v} *)
let num_2_pow_128 = Num.power_num num_2 num_128 ;;

(** {v num_2_pow_256 v} *)
let num_2_pow_256 = Num.power_num num_2 num_256 ;;

(** {v num_2_pow_512 v} *)
let num_2_pow_512 = Num.power_num num_2 num_512 ;;

(** {v num_2_pow_1024 v} *)
let num_2_pow_1024 = Num.power_num num_2 num_1024 ;;

(** {v num_2_pow_2048 v} *)
let num_2_pow_2048 = Num.power_num num_2 num_2048 ;;


(** {v num_05 v} *)
let num_05 =  Num.div_num num_1 num_2 ;;

(** {v num_minus_05 v} *)
let num_minus_05 =  Num.div_num num_minus_1 num_2 ;;


(** {v num_2_pow_minus_16 v} *)
let num_2_pow_minus_16 = Num.power_num num_2 num_minus_16 ;;

(** {v num_2_pow_minus_32 v} *)
let num_2_pow_minus_32 = Num.power_num num_2 num_minus_32 ;;

(** {v num_2_pow_minus_64 v} *)
let num_2_pow_minus_64 = Num.power_num num_2 num_minus_64 ;;

(** {v num_2_pow_minus_128 v} *)
let num_2_pow_minus_128 = Num.power_num num_2 num_minus_128 ;;

(** {v num_2_pow_minus_256 v} *)
let num_2_pow_minus_256 = Num.power_num num_2 num_minus_256 ;;

(** {v num_2_pow_minus_512 v} *)
let num_2_pow_minus_512 = Num.power_num num_2 num_minus_512 ;;

(** {v num_2_pow_minus_1024 v} *)
let num_2_pow_minus_1024 = Num.power_num num_2 num_minus_1024 ;;


(** The [num_of_float] function is not provided by the Ocaml distribution.

La fonction [num_of_float] n'est pas fournie par la distribution [Ocaml]. *)

(** {v num_of_float real v} *)
let num_of_float = function (x:float) ->
 if abs_float x > max_float then failwith "Too big a float for Sci.num_of_float."
 else
  begin
   let s = ( string_of_float x ) ^ "e" in
    let i = String.index s 'e'
    and l = String.length s in
     let j = try String.index s '.' with Not_found -> i in
      let n = String.sub s 0 j
      and nn = if ( l <= 3 || i = j ) then "" else String.sub s ( j + 1 ) ( i - j - 1 )
      and e = if i = l - 1 then "0" else String.sub s ( i + 1 ) ( l - i - 2 )
      and d = String.make ( max 1 ( i - j ) ) '0' in
       String.set d 0 '1' ;
       let m = Num.num_of_string ( n ^ nn ^ "/" ^ d )
       and ee = Num.num_of_string e in
        let p = Num.power_num num_10 ee in
         Num.mult_num m p
  end ;;   


(** {v num_epsilon_float v} *)
let num_epsilon_float = num_of_float epsilon_float ;;

(** {v num_min_float v} *)
let num_min_float = num_of_float min_float ;;

(** {v num_max_float v} *)
let num_max_float = num_of_float max_float ;;

(** {v num_min_int v} *)
let num_min_int = Num.num_of_int min_int ;;

(** {v num_max_int v} *)
let num_max_int = Num.num_of_int max_int ;;


(** {v sci_0 v} *)
let sci_0 = [| num_0 ; num_0 ; num_0 |] ;;

(** {v sci_1 v} *)
let sci_1 = [| num_1 ; num_0 ; num_0 |] ;;

(** {v sci_minus_1 v} *)
let sci_minus_1 = [| num_minus_1 ; num_0 ; num_0 |] ;;

(** {v sci_i v} *)
let sci_i = [| num_0 ; num_1 ; num_0 |] ;;

(** {v sci_minus_i v} *)
let sci_minus_i = [| num_0 ; num_minus_1 ; num_0 |] ;;


(** {v sci_2 v} *)
let sci_2 = [| num_1 ; num_0 ; num_1 |] ;;

(** {v sci_minus_2 v} *)
let sci_minus_2 = [| num_minus_1 ; num_0 ; num_1 |] ;;

(** {v sci_2i v} *)
let sci_2i = [| num_0 ; num_1 ; num_1 |] ;;

(** {v sci_minus_2i v} *)
let sci_minus_2i = [| num_0 ; num_minus_1 ; num_1 |] ;;

(** {v sci_4 v} *)
let sci_4 = [| num_1 ; num_0 ; num_2 |] ;;

(** {v sci_minus_4 v} *)
let sci_minus_4 = [| num_minus_1 ; num_0 ; num_2 |] ;;

(** {v sci_4i v} *)
let sci_4i = [| num_0 ; num_1 ; num_2 |] ;;

(** {v sci_minus_4i v} *)
let sci_minus_4i = [| num_0 ; num_minus_1 ; num_2 |] ;;

(** {v sci_8 v} *)
let sci_8 = [| num_1 ; num_0 ; num_3 |] ;;

(** {v sci_minus_8 v} *)
let sci_minus_8 = [| num_minus_1 ; num_0 ; num_3 |] ;;

(** {v sci_8i v} *)
let sci_8i = [| num_0 ; num_1 ; num_3 |] ;;

(** {v sci_minus_8i v} *)
let sci_minus_8i = [| num_0 ; num_minus_1 ; num_3 |] ;;

(** {v sci_16 v} *)
let sci_16 = [| num_1 ; num_0 ; num_4 |] ;;

(** {v sci_minus_16 v} *)
let sci_minus_16 = [| num_minus_1 ; num_0 ; num_4 |] ;;

(** {v sci_16i v} *)
let sci_16i = [| num_0 ; num_1 ; num_4 |] ;;

(** {v sci_minus_16i v} *)
let sci_minus_16i = [| num_0 ; num_minus_1 ; num_4 |] ;;

(** {v sci_32 v} *)
let sci_32 = [| num_1 ; num_0 ; num_5 |] ;;

(** {v sci_minus_32 v} *)
let sci_minus_32 = [| num_minus_1 ; num_0 ; num_5 |] ;;

(** {v sci_32i v} *)
let sci_32i = [| num_0 ; num_1 ; num_5 |] ;;

(** {v sci_minus_32i v} *)
let sci_minus_32i = [| num_0 ; num_minus_1 ; num_5 |] ;;

(** {v sci_64 v} *)
let sci_64 = [| num_1 ; num_0 ; num_6 |] ;;

(** {v sci_minus_64 v} *)
let sci_minus_64 = [| num_minus_1 ; num_0 ; num_6 |] ;;

(** {v sci_64i v} *)
let sci_64i = [| num_0 ; num_1 ; num_6 |] ;;

(** {v sci_minus_64i v} *)
let sci_minus_64i = [| num_0 ; num_minus_1 ; num_6 |] ;;

(** {v sci_128 v} *)
let sci_128 = [| num_1 ; num_0 ; num_7 |] ;;

(** {v sci_minus_128 v} *)
let sci_minus_128 = [| num_minus_1 ; num_0 ; num_7 |] ;;

(** {v sci_128i v} *)
let sci_128i = [| num_0 ; num_1 ; num_7 |] ;;

(** {v sci_minus_128i v} *)
let sci_minus_128i = [| num_0 ; num_minus_1 ; num_7 |] ;;

(** {v sci_256 v} *)
let sci_256 = [| num_1 ; num_0 ; num_8 |] ;;

(** {v sci_minus_256 v} *)
let sci_minus_256 = [| num_minus_1 ; num_0 ; num_8 |] ;;

(** {v sci_256i v} *)
let sci_256i = [| num_0 ; num_1 ; num_8 |] ;;

(** {v sci_minus_256i v} *)
let sci_minus_256i = [| num_0 ; num_minus_1 ; num_8 |] ;;

(** {v sci_512 v} *)
let sci_512 = [| num_1 ; num_0 ; num_9 |] ;;

(** {v sci_minus_512 v} *)
let sci_minus_512 = [| num_minus_1 ; num_0 ; num_9 |] ;;

(** {v sci_512i v} *)
let sci_512i = [| num_0 ; num_1 ; num_9 |] ;;

(** {v sci_minus_512i v} *)
let sci_minus_512i = [| num_0 ; num_minus_1 ; num_9 |] ;;

(** {v sci_1024 v} *)
let sci_1024 = [| num_1 ; num_0 ; num_10 |] ;;

(** {v sci_minus_1024 v} *)
let sci_minus_1024 = [| num_minus_1 ; num_0 ; num_10 |] ;;

(** {v sci_1024i v} *)
let sci_1024i = [| num_0 ; num_1 ; num_10 |] ;;

(** {v sci_minus_1024i v} *)
let sci_minus_1024i = [| num_0 ; num_minus_1 ; num_10 |] ;;


(** {v sci_05 v} *)
let sci_05 = [| num_1 ; num_0 ; num_minus_1 |] ;;

(** {v sci_minus_05 v} *)
let sci_minus_05 = [| num_minus_1 ; num_0 ; num_minus_1 |] ;;

(** {v sci_05i v} *)
let sci_05i = [| num_0 ; num_1 ; num_minus_1 |] ;;

(** {v sci_minus_05i v} *)
let sci_minus_05i = [| num_0 ; num_minus_1 ; num_minus_1 |] ;;

(** {v sci_025 v} *)
let sci_025 = [| num_1 ; num_0 ; num_minus_2 |] ;;

(** {v sci_minus_025 v} *)
let sci_minus_2 = [| num_minus_1 ; num_0 ; num_minus_2 |] ;;

(** {v sci_025i v} *)
let sci_025i = [| num_0 ; num_1 ; num_minus_2 |] ;;

(** {v sci_minus_025i v} *)
let sci_minus_025i = [| num_0 ; num_minus_1 ; num_minus_2 |] ;;

(** {v sci_0125 v} *)
let sci_0125 = [| num_1 ; num_0 ; num_minus_3 |] ;;

(** {v sci_minus_0125 v} *)
let sci_minus_2 = [| num_minus_1 ; num_0 ; num_minus_3 |] ;;

(** {v sci_0125i v} *)
let sci_025i = [| num_0 ; num_1 ; num_minus_3 |] ;;

(** {v sci_minus_0125i v} *)
let sci_minus_025i = [| num_0 ; num_minus_1 ; num_minus_3 |] ;;

(** {v sci_2_pow_minus_4 v} *)
let sci_2_pow_minus_4 = [| num_1 ; num_0 ; num_minus_4 |] ;;

(** {v sci_minus_2_pow_minus_4 v} *)
let sci_minus_2_pow_minus_4 = [| num_minus_1 ; num_0 ; num_minus_4 |] ;;

(** {v sci_i_2_pow_minus_4 v} *)
let sci_i_2_pow_minus_4 = [| num_0 ; num_1 ; num_minus_4 |] ;;

(** {v sci_minus_i_2_pow_minus_4 v} *)
let sci_minus_i_2_pow_minus_4 = [| num_0 ; num_minus_1 ; num_minus_4 |] ;;

(** {v sci_2_pow_minus_5 v} *)
let sci_2_pow_minus_5 = [| num_1 ; num_0 ; num_minus_5 |] ;;

(** {v sci_minus_2_pow_minus_5 v} *)
let sci_minus_2_pow_minus_5 = [| num_minus_1 ; num_0 ; num_minus_5 |] ;;

(** {v sci_i_2_pow_minus_5 v} *)
let sci_i_2_pow_minus_5 = [| num_0 ; num_1 ; num_minus_5 |] ;;

(** {v sci_minus_i_2_pow_minus_5 v} *)
let sci_minus_i_2_pow_minus_5 = [| num_0 ; num_minus_1 ; num_minus_5 |] ;;

(** {v sci_2_pow_minus_6 v} *)
let sci_2_pow_minus_6 = [| num_1 ; num_0 ; num_minus_6 |] ;;

(** {v sci_minus_2_pow_minus_6 v} *)
let sci_minus_2_pow_minus_6 = [| num_minus_1 ; num_0 ; num_minus_6 |] ;;

(** {v sci_i_2_pow_minus_6 v} *)
let sci_i_2_pow_minus_6 = [| num_0 ; num_1 ; num_minus_6 |] ;;

(** {v sci_minus_i_2_pow_minus_6 v} *)
let sci_minus_i_2_pow_minus_6 = [| num_0 ; num_minus_1 ; num_minus_6 |] ;;

(** {v sci_2_pow_minus_7 v} *)
let sci_2_pow_minus_7 = [| num_1 ; num_0 ; num_minus_7 |] ;;

(** {v sci_minus_2_pow_minus_7 v} *)
let sci_minus_2_pow_minus_7 = [| num_minus_1 ; num_0 ; num_minus_7 |] ;;

(** {v sci_i_2_pow_minus_7 v} *)
let sci_i_2_pow_minus_7 = [| num_0 ; num_1 ; num_minus_7 |] ;;

(** {v sci_minus_i_2_pow_minus_7 v} *)
let sci_minus_i_2_pow_minus_7 = [| num_0 ; num_minus_1 ; num_minus_7 |] ;;

(** {v sci_2_pow_minus_8 v} *)
let sci_2_pow_minus_8 = [| num_1 ; num_0 ; num_minus_8 |] ;;

(** {v sci_minus_2_pow_minus_8 v} *)
let sci_minus_2_pow_minus_8 = [| num_minus_1 ; num_0 ; num_minus_8 |] ;;

(** {v sci_i_2_pow_minus_8 v} *)
let sci_i_2_pow_minus_8 = [| num_0 ; num_1 ; num_minus_8 |] ;;

(** {v sci_minus_i_2_pow_minus_8 v} *)
let sci_minus_i_2_pow_minus_8 = [| num_0 ; num_minus_1 ; num_minus_8 |] ;;

(** {v sci_2_pow_minus_9 v} *)
let sci_2_pow_minus_9 = [| num_1 ; num_0 ; num_minus_9 |] ;;

(** {v sci_minus_2_pow_minus_9 v} *)
let sci_minus_2_pow_minus_9 = [| num_minus_1 ; num_0 ; num_minus_9 |] ;;

(** {v sci_i_2_pow_minus_9 v} *)
let sci_i_2_pow_minus_9 = [| num_0 ; num_1 ; num_minus_9 |] ;;

(** {v sci_minus_i_2_pow_minus_9 v} *)
let sci_minus_i_2_pow_minus_9 = [| num_0 ; num_minus_1 ; num_minus_9 |] ;;

(** {v sci_2_pow_minus_10 v} *)
let sci_2_pow_minus_10 = [| num_1 ; num_0 ; num_minus_10 |] ;;

(** {v sci_minus_2_pow_minus_10 v} *)
let sci_minus_2_pow_minus_10 = [| num_minus_1 ; num_0 ; num_minus_10 |] ;;

(** {v sci_i_2_pow_minus_10 v} *)
let sci_i_2_pow_minus_10 = [| num_0 ; num_1 ; num_minus_10 |] ;;

(** {v sci_minus_i_2_pow_minus_10 v} *)
let sci_minus_i_2_pow_minus_10 = [| num_0 ; num_minus_1 ; num_minus_10 |] ;;


(** The formatting function of numbers in scientific notation is delicate 
and is useful to the definitions of the elementary operations.

La fonction de mise en forme des nombres en notation scientifique est délicate 
et sert aux définitions des opérations élémentaires. *)

(** {v format number v} *)
let rec format = function (a:Num.num array) ->
 let x = ref a.(0)
 and y = ref a.(1)
 and w = ref a.(2) in
  let m = ref ( Num.add_num ( Num.square_num !x ) ( Num.square_num !y ) ) in
   if Num.eq_num !m num_0 then [| num_0 ; num_0 ; num_0 |]
   else
    begin
     while Num.ge_num !m num_2_pow_1024 do
      w := Num.add_num !w num_512 ;
      x := Num.mult_num !x num_2_pow_minus_512 ;
      y := Num.mult_num !y num_2_pow_minus_512 ;
      m := Num.mult_num !m num_2_pow_minus_1024 ;
     done ;
     if Num.ge_num !m num_2_pow_512 then
      begin
       w := Num.add_num !w num_256 ;
       x := Num.mult_num !x num_2_pow_minus_256 ;
       y := Num.mult_num !y num_2_pow_minus_256 ;
       m := Num.mult_num !m num_2_pow_minus_512 ;
      end ;
     if Num.ge_num !m num_2_pow_256 then
      begin
       w := Num.add_num !w num_128 ;
       x := Num.mult_num !x num_2_pow_minus_128 ;
       y := Num.mult_num !y num_2_pow_minus_128 ;
       m := Num.mult_num !m num_2_pow_minus_256 ;
      end ;
     if Num.ge_num !m num_2_pow_128 then
      begin
       w := Num.add_num !w num_64 ;
       x := Num.mult_num !x num_2_pow_minus_64 ;
       y := Num.mult_num !y num_2_pow_minus_64 ;
       m := Num.mult_num !m num_2_pow_minus_128 ;
     end ;
     if Num.ge_num !m num_2_pow_64 then
      begin
       w := Num.add_num !w num_32 ;
       x := Num.mult_num !x num_2_pow_minus_32 ;
       y := Num.mult_num !y num_2_pow_minus_32 ;
       m := Num.mult_num !m num_2_pow_minus_64 ;
     end ;
     if Num.ge_num !m num_2_pow_32 then
      begin
       w := Num.add_num !w num_16 ;
       x := Num.mult_num !x num_2_pow_minus_16 ;
       y := Num.mult_num !y num_2_pow_minus_16 ;
       m := Num.mult_num !m num_2_pow_minus_32 ;
     end ;
     while Num.le_num !m num_2_pow_minus_1024 do
      w := Num.sub_num !w num_512 ;
      x := Num.mult_num !x num_2_pow_512 ;
      y := Num.mult_num !y num_2_pow_512 ;
      m := Num.mult_num !m num_2_pow_1024 ;
     done ;
     if Num.le_num !m num_2_pow_minus_512 then
      begin
       w := Num.sub_num !w num_256 ;
       x := Num.mult_num !x num_2_pow_256 ;
       y := Num.mult_num !y num_2_pow_256 ;
       m := Num.mult_num !m num_2_pow_512 ;
      end ;
     if Num.le_num !m num_2_pow_minus_256 then
      begin
       w := Num.sub_num !w num_128 ;
       x := Num.mult_num !x num_2_pow_128 ;
       y := Num.mult_num !y num_2_pow_128 ;
       m := Num.mult_num !m num_2_pow_256 ;
      end ;
     if Num.le_num !m num_2_pow_minus_128 then
      begin
       w := Num.sub_num !w num_64 ;
       x := Num.mult_num !x num_2_pow_64 ;
       y := Num.mult_num !y num_2_pow_64 ;
       m := Num.mult_num !m num_2_pow_128 ;
      end ;
     if Num.le_num !m num_2_pow_minus_64 then
      begin
       w := Num.sub_num !w num_32 ;
       x := Num.mult_num !x num_2_pow_32 ;
       y := Num.mult_num !y num_2_pow_32 ;
       m := Num.mult_num !m num_2_pow_64 ;
      end ;
     if Num.le_num !m num_2_pow_minus_32 then
      begin
       w := Num.sub_num !w num_16 ;
       x := Num.mult_num !x num_2_pow_16 ;
       y := Num.mult_num !y num_2_pow_16 ;
       m := Num.mult_num !m num_2_pow_32 ;
      end ;
     let mm = Num.float_of_num !m in
      let mmm = Num.num_of_int ( ( int_of_float ( floor ( ( log mm ) /. log 4. ) ) ) ) in
       let coeff = Num.power_num num_05 mmm in
        x := Num.mult_num !x coeff ;
        y := Num.mult_num !y coeff ;
        w := Num.add_num !w mmm ;
        m := Num.add_num ( Num.square_num !x ) ( Num.square_num !y ) ;
        if Num.ge_num !m num_4 then
         begin
          x := Num.div_num !x num_2 ;
          y := Num.div_num !y num_2 ;
          w := Num.succ_num !w ;
         end ;
         [| !x ; !y ; !w |]
    end ;;


(** {v sci_3 v} *)
let sci_3 = format [| num_3 ; num_0 ; num_0 |] ;;

(** {v sci_minus_3 v} *)
let sci_minus_3 = format [| num_minus_3 ; num_0 ; num_0 |] ;;

(** {v sci_3i v} *)
let sci_3i = format [| num_0 ; num_3 ; num_0 |] ;;

(** {v sci_6 v} *)
let sci_6 = format [| num_3 ; num_0 ; num_1 |] ;;

(** {v sci_minus_6 v} *)
let sci_minus_6 = format [| num_minus_3 ; num_0 ; num_1 |] ;;

(** {v sci_27 v} *)
let sci_27 = format [| num_27 ; num_0 ; num_0 |] ;;

(** {v sci_minus_27 v} *)
let sci_minus_27 = format [| num_minus_27 ; num_0 ; num_0 |] ;;

(** {v sci_10 v} *)
let sci_10 = format [| num_10 ; num_0 ; num_0 |] ;;

(** {v sci_10i v} *)
let sci_10i = format [| num_0 ; num_10 ; num_0 |] ;;








(** {C § } *)
(** 
{1 Fonctions élémentaires pour Num.num}
{1 Elementary functions for Num.num}
*)
(** {C  } *)




(** {v num_sqrt integer v} *)
let num_sqrt = function (x:int) ->
 Num.num_of_big_int ( Big_int.sqrt_big_int ( Big_int.big_int_of_int x ) ) ;;

(** {v num_add_int integer v} *)
let num_add_int = fun (x:int) (y:int) ->
 Num.add_num ( Num.num_of_int x ) ( Num.num_of_int y ) ;;

(** {v num_sub_int integer v} *)
let num_sub_int = fun (x:int) (y:int) ->
 Num.sub_num ( Num.num_of_int x ) ( Num.num_of_int y ) ;;

(** {v num_mult_int integer v} *)
let num_mult_int = fun (x:int) (y:int) ->
 Num.mult_num ( Num.num_of_int x ) ( Num.num_of_int y ) ;;

(** {v num_div_int integer v} *)
let num_div_int = fun (x:int) (y:int) ->
 Num.div_num ( Num.num_of_int x ) ( Num.num_of_int y ) ;;

(** {v num_mod_int integer v} *)
let num_mod_int = fun (x:int) (y:int) ->
 Num.mod_num ( Num.num_of_int x ) ( Num.num_of_int y ) ;;


(** {v make_fraction num denom v} *)
let make_fraction = fun (numer:int) (denom:int) ->
 Num.num_of_ratio ( Ratio.create_ratio ( Big_int.big_int_of_int numer ) ( Big_int.big_int_of_int denom ) ) ;;

(** {v print_make_fraction num denom v} *)
let print_make_fraction = fun (num:int) (denom:int) ->
 print_string ( Num.string_of_num ( make_fraction num denom ) ) ;;






(** {C § } *)
(** 
{1 Fonctions élémentaires pour les nombres complexes en notation scientifique}
{1 Elementary functions for complex numbers in scientific notation}
*)
(** {C  } *)




(** {v sci_copy number v} *) 
let sci_copy = function (a:Num.num array) ->
 let b = Array.make 3 num_0 in
  for i = 0 to 2 do
   b.(i) <- Num.add_num num_0 a.(i) ;
  done ;
  b ;;

(** {v float_of_sci number v} *)
let float_of_sci = function (a:Num.num array) ->
 Array.map Num.float_of_num a ;;


(** {v complex_of_sci number v} *)
let complex_of_sci = function (a:Num.num array) ->
 let x = Num.mult_num a.(0) ( Num.power_num num_2 a.(2) )
 and y = Num.mult_num a.(1) ( Num.power_num num_2 a.(2) ) in
  let xx = Num.float_of_num x
  and yy = Num.float_of_num y in
   [| [| xx ; -. yy |] ; [| yy ; xx |] |] ;;


(** {v sci_of_num number v} *)
let sci_of_num = function (x:Num.num) ->
 format [| x ; num_0 ; num_0 |] ;;

(** {v sci_to_string_array number_array v} *)
let sci_to_string_array = function (z:Num.num array) ->
 let s = Array.map Num.string_of_num z in
  [| s.(0) ; s.(1) ; s.(2) |] ;;

(** {v sci_to_string number_array v} *)
let sci_to_string = function (z:Num.num array) ->
 let s = Array.map Num.string_of_num z in
  s.(0) ^ " ; " ^ s.(1) ^ " ; " ^ s.(2) ;;


(** {v sci_of_string_array string_array v} *)
let sci_of_string_array = function (s:string array) ->
 let x = Array.map Num.num_of_string s in
  format [| x.(0) ; x.(1) ; x.(2) |] ;;

(** {v sci_of_string string v} *)
let sci_of_string = function (s:string) ->
 let listing = Str.split ( Str.regexp " ; " ) s in
  let x = Array.map Num.num_of_string ( Array.of_list listing ) in
   format [| x.(0) ; x.(1) ; x.(2) |] ;;


(** {v sci_of_float real v} *)
let sci_of_float = function (x:float) ->
 sci_of_num ( num_of_float x ) ;;

(** {v sci_of_int integer v} *)
let sci_of_int = function (x:int) ->
 sci_of_num ( Num.num_of_int x ) ;;

(** {v sci_of_complex real v} *)
let sci_of_complex = function (z:float array array) ->
 let x = z.(0).(0)
 and y = z.(1).(0)
 and yy = z.(0).(1)
 and xx = z.(1).(1) in
  let a = num_of_float x
  and b = num_of_float y
  and aa = num_of_float xx
  and bb = num_of_float yy in
   format [| Num.add_num a aa ; Num.sub_num b bb ; num_minus_1 |] ;;


(** {v square_module number v} *)
let square_module = function (a:Num.num array) ->
 let x = a.(0)
 and y = a.(1)
 and w = a.(2) in
  let m = Num.add_num ( Num.square_num x ) ( Num.square_num y ) in
   format [| m ; num_0 ; Num.mult_num num_2 w |] ;;

(** {v norm_inf number v} *)
let norm_inf = function (a:Num.num array) ->
 let x = a.(0)
 and y = a.(1)
 and w = a.(2) in
  let m = Num.max_num ( Num.abs_num x ) ( Num.abs_num y ) in
   format [| m ; num_0 ; w |] ;;

(** {v norm_1 number v} *)
let norm_1 = function (a:Num.num array) ->
 let x = a.(0)
 and y = a.(1)
 and w = a.(2) in
  let m = Num.add_num ( Num.abs_num x ) ( Num.abs_num y ) in
   format [| m ; num_0 ; w |] ;;


(** {v real_part number v} *)
let real_part = function (a:Num.num array) ->
 format [| a.(0) ; num_0 ; a.(2) |] ;;

(** {v imag_part number v} *)
let imag_part = function (a:Num.num array) ->
 format [| a.(1) ; num_0 ; a.(2) |] ;;

(** {v real_part_abs number v} *)
let real_part_abs = function (a:Num.num array) ->
 format [| Num.abs_num a.(0) ; num_0 ; a.(2) |] ;;

(** {v imag_part_abs number v} *)
let imag_part_abs = function (a:Num.num array) ->
 format [| num_0 ; Num.abs_num a.(1) ; a.(2) |] ;;


(** {v opp number v} *)
let opp = function (a:Num.num array) ->
[| Num.minus_num a.(0) ; Num.minus_num a.(1) ; a.(2) |] ;;


(** {v itimes number v} *)
let itimes = function (a:Num.num array) ->
[| Num.minus_num a.(1) ; a.(0) ; a.(2) |] ;;


(** {v plus number1 number2 v} *)
let rec plus = fun (a:Num.num array) (b:Num.num array) ->
 let w = a.(2)
 and ww = b.(2) in
  if Num.ge_num ww w then
   begin
    let www = Num.sub_num w ww in
     let w_w = Num.power_num num_2 www in
      let x = Num.mult_num a.(0) w_w
      and y = Num.mult_num a.(1) w_w in
       let xx = Num.add_num x b.(0)
       and yy = Num.add_num y b.(1) in
        format [| xx ; yy ; ww |]
   end
  else plus b a ;;

(** {v minus number1 number2 v} *)
let rec minus = fun (a:Num.num array) (b:Num.num array) ->
 plus a ( opp b ) ;;

(** {v mult number1 number2 v} *)
let mult = fun (a:Num.num array) (b:Num.num array) ->
 let w = a.(2)
 and ww = b.(2) in
  let www = Num.add_num ww w in
   let x = Num.mult_num a.(0) b.(0)
   and xx = Num.mult_num a.(1) b.(1)
   and y = Num.mult_num a.(0) b.(1)
   and yy = Num.mult_num a.(1) b.(0) in
    format [| Num.sub_num x xx ; Num.add_num y yy ; www |] ;;


(** {v conj number v} *)
let conj = function (a:Num.num array) ->
 [| a.(0) ; Num.minus_num a.(1) ; a.(2) |] ;;


(** {v inv number v} *)
let inv = function (a:Num.num array) ->
 let x = a.(0)
 and y = a.(1)
 and w = a.(2) in
  let m = Num.add_num ( Num.square_num x ) ( Num.square_num y ) in
   format [| Num.div_num x m ; Num.div_num ( Num.minus_num y ) m ;  Num.minus_num w |] ;;


(** {v div number1 number2 v} *)
let div = fun (a:Num.num array) (b:Num.num array) ->
 mult a ( inv b ) ;;


(** {v eq_0 number v} *)
let eq_0 = function (a:Num.num array) ->
 let b = norm_inf a in
  b.(0) = num_0 ;;


(** {v not_eq_0 number v} *)
let not_eq_0 = function (a:Num.num array) ->
 let b = norm_inf a in
  b.(0) <> num_0 ;;


(** {v eq number1 number2 v} *)
let eq = fun (a:Num.num array) (b:Num.num array) ->
 eq_0 ( minus a b ) ;;


(** {v not_eq number1 number2 v} *)
let not_eq = fun (a:Num.num array) (b:Num.num array) ->
 not_eq_0 ( minus a b ) ;;


(** {v int_pow exponent number v} *)
let rec int_pow = fun (n:int) (a:Num.num array) -> match n with
 | 0 -> sci_1
 | 1 -> a
 | -1 -> inv a
 | _ ->
  begin
   if n < 0 then int_pow ( abs n ) ( inv a )
   else
    begin
     let b = int_pow ( n / 2 ) a in
      if n mod 2 = 0 then mult b b
      else mult ( mult b b ) a
    end
  end ;;


(** {v sci_epsilon_float v} *)
let sci_epsilon_float = sci_of_float epsilon_float ;;

(** {v sci_min_float v} *)
let sci_min_float = sci_of_float min_float ;;

(** {v sci_max_float v} *)
let sci_max_float = sci_of_float max_float ;;

(** {v sci_min_int v} *)
let sci_min_int = sci_of_int min_int ;;

(** {v sci_max_int v} *)
let sci_max_int = sci_of_int max_int ;;




(** {C § } *)
(** 
{1 Utilisation de structures creuses en arithmétique}
{1 Use of sparse structures in arithmetic}
*)
(** {C  } *)






(** The module [Z] is used as an argument for the functor [Hashtbl.Make].

Le module [Z] sert d'argument au foncteur [Hashtbl.Make]. *)

module Z = struct

type t = int ;;
let equal = ( = ) ;;
let hash = Hashtbl.hash ;;

end ;;


(** The module [P] is used to construct hash tables where to store the prime numbers.

Le module [P] sert à construire des tables de hachage où stocker les nombres premiers. *)

module P = ( Hashtbl.Make (Z)
: sig
  include module type of Hashtbl.Make (Z)
 end
  with type key = int ) ;;

(** {v left_dump table v} *)
let left_dump = fun (x:'a P.t) ->
 let accu = P.fold ( fun key coeff liste -> key :: liste ) x [] in
 Array.of_list ( List.fast_sort compare accu ) ;;

(** {v primes_init size v} *)
let primes_init = fun (n:int) ->
 P.create n ;;

(** {v first_compare x y v} *)
let first_compare = fun x y ->
 compare ( fst x ) ( fst y ) ;;


(** The module [H] is used to construct hash tables where to store the prime factors of the factorial with multiplicity.

Le module [H] sert à construire des tables de hachage où stocker les facteurs premiers de la factorielle avec multiplicité. *)

module H = ( Hash.Make (Data.Zindex) (Hash.Z) (Data.Zcoeff)
: sig
  include module type of Hash.Make (Data.Zindex) (Hash.Z) (Data.Zcoeff)
 end
  with type index := int with type weight := int ) ;;

let powers_init = fun (n:int) ->
 H.create n ;;

let flush = function (h:H.t) ->
 Array.of_list ( H.flush h ) ;;


(** {v contract list_of_power_pairs v} The list must be ordered according to the first factors.

La liste doit être ordonnée selon les premiers facteurs. *)
let contract = function z ->
 let rec contract_aux = fun accu z ->
  match z with
  | [] -> accu
  | head :: tail ->
   begin
    match tail with
    | [] -> head :: accu
    | x :: queue ->
     begin
      if fst head = fst x then
       contract_aux accu ( ( fst head , ( snd head ) + ( snd x ) ) :: queue )
      else
       contract_aux ( head :: accu ) tail
     end
   end in
   List.rev ( contract_aux [] z ) ;;

(** {v complete_sieve_aux number step factor powers source factors primes v} *)
let complete_sieve_aux = fun (n:int) (step:int) (factor:int) (powers:H.t) (source:int array) (factors:(int * int) list array) (primes:int P.t) ->
 let nn = n / step in
  if not ( P.mem primes factor ) then
   P.add primes factor 0 ;
  for i = 1 to nn do
   let index = step * i in
    begin
     try
      source.(index) <- source.(index) / factor
     with _ ->
      ()
    end ;
    factors.(index) <- contract ( List.merge first_compare [ ( factor , 1 ) ] factors.(index ) ) ;
  done ;
  H.add powers ( factor , nn ) ;;

(** {v complete_sieve table_size number v} *)
let complete_sieve = fun (p:int) (n:int) ->
 let sn = succ n
 and powers = powers_init p
 and primes = primes_init p  in
  let factors = Array.make sn []
  and source = Array.make sn 1 in
   for i = 2 to n do
    source.(i) <- i
   done ;
   for i = 2 to n do
    try
     begin
      let factor = source.(i) in
       if factor <> 1 then
        complete_sieve_aux n i factor powers source factors primes
     end
    with _ ->
     ()
   done ;
   ( left_dump primes , factors , flush powers ) ;;

(** {v primes_sieve_aux number step factor source primes v} *)
let primes_sieve_aux = fun (n:int) (step:int) (factor:int) (source:int array) (primes:int P.t) ->
 let nn = n / step in
  if not ( P.mem primes factor ) then
   P.add primes factor 0 ;
  for i = 1 to nn do
   let index = step * i in
    begin
     try
      source.(index) <- source.(index) / factor
     with _ ->
      ()
    end ;
  done ;;


(** {v primes_sieve table_size number v} This function is ten to one hundred times slower than the unix function [primes] on moderate numbers.

Cette fonction est dix à cent fois plus lente que la fonction unix [primes] sur des nombres modérés. *)
let primes_sieve = fun (p:int) (n:int) ->
 let sn = succ n
 and primes = primes_init p in
  let source = Array.make sn 1 in
   for i = 2 to n do
    source.(i) <- i
   done ;
   for i = 2 to n do
    try
     begin
      let factor = source.(i) in
      if factor <> 1 then
       begin
        primes_sieve_aux n i factor source primes ;
       end
     end
    with _ ->
     ()
   done ;
   left_dump primes ;;

(** {v sieve number v} *)
let sieve = function (n:int) ->
 primes_sieve ( n / 64 ) n ;;

(** {v factorial_sieve_aux number step factor powers source primes v} *)
let factorial_sieve_aux = fun (n:int) (step:int) (factor:int) (powers:H.t) (source:int array) (primes:int P.t) ->
 let nn = n / step in
  if not ( P.mem primes factor ) then
   P.add primes factor 0 ;
  for i = 1 to nn do
   let index = step * i in
    begin
     try
      source.(index) <- source.(index) / factor
     with _ ->
      ()
    end ;
  done ;
  H.add powers ( factor , nn ) ;;

(** {v factorial_sieve table_size number v} *)
let factorial_sieve = fun (p:int) (n:int) ->
 let sn = succ n
 and powers = powers_init p
 and primes = primes_init p in
  let source = Array.make sn 1 in
   for i = 2 to n do
    source.(i) <- i
   done ;
   for i = 2 to n do
    try
     begin
      let factor = source.(i) in
       if factor <> 1 then
        factorial_sieve_aux n i factor powers source primes ;
     end
    with _ ->
     ()
   done ;
   ( left_dump primes , flush powers ) ;;


(** {v naive_factors number v} This function is slower than the following one but reacts well with big prime factors. 
In case of a prime number, the resulting list contains a unique element.

Cette fonction est plus lente que la suivante mais réagit bien avec de grands facteurs premiers. 
Dans le cas d'un nombre premier, la liste résultat ne contient qu'un élément. *)
let naive_factors = function (n:int) ->
 let p = sieve n
 and power = ref 0
 and dividend = ref n
 and accu = ref [] in
  let i = ref ( pred ( Array.length p ) ) in
   while !i >= 0 do
    let factor = p.(!i) in
     while ( !dividend mod factor == 0 ) do
      dividend := !dividend / factor ;
      incr power ;
     done ;
     if !power > 0 then
      accu := ( factor , !power ) :: !accu ;
     if !dividend = 1 then
      i := -1
     else
      begin
       power := 0 ;
       decr i ;
      end ;
   done ;
   !accu ;;


(** {v factors number v} This function calls the former one when it has not been able to factorize the number with small prime numbers.

Cett fonction appelle la précédente quand elle n'a pas pu factoriser entièrement le nombre avec de petits premiers.*)
let factors = fun (n:int) ->
 let p = sieve ( Util.int_sqrt n )
 and listing = ref []
 and power = ref 0
 and dividend = ref n
 and accu = ref [] in
  for i = 0 to pred ( Array.length p ) do
   let factor = p.(i) in
    while ( !dividend mod factor == 0 ) do
     dividend := !dividend / factor ;
     incr power ;
    done ;
    if !power > 0 then
     accu := ( factor , !power ) :: !accu ;
    power := 0 ;
  done ;
  if !dividend > 1 then
   begin
    listing := naive_factors !dividend ;
   end ;
  List.rev_append !accu !listing ;;

(** {v is_prime number v} *)
let is_prime = function (n:int) ->
 Util.list_is_empty ( List.tl ( factors n ) ) ;;


(** {v fact table_size number v} This way of calculating the factorial is quicker than that of the following chapter (inside the toplevel).

Cette manière de calculer la factorielle est plus rapide que celle du chapitre suivant (dans le toplevel). *)
let fact = fun (size:int) (n:int) ->
 let s = factorial_sieve size n
 and f = function ( x , y ) -> Big_int.power_int_positive_int x y in
  Array.fold_left Big_int.mult_big_int ( Util.big 1 ) ( Array.map f ( snd s ) ) ;;

(** {v approx_decimal_fact table_size number v} *)
let approx_decimal_fact = fun (size:int) (n:int) ->
 let f = float_of_sci ( sci_of_num ( Num.num_of_big_int ( fact size n ) ) ) in
  let expo = f.(2) *. ( log 2. ) /. ( log 10. ) in
   let expo_dec = float ( int_of_float expo ) in
    let mantissa = f.(0) *. ( 10. ** ( expo -. expo_dec ) ) in
     if mantissa < 10. then
      [| mantissa ; expo_dec |]
     else
      [| mantissa /. 10. ; expo_dec +. 1. |] ;;

(** {v phi_euler number v} *)
let phi_euler = function (n:int) ->
 let liste = ref ( factors n )
 and accu = ref 1 in
  while Util.list_non_empty !liste do
   let ( p , a ) = List.hd !liste in
    accu := !accu * ( Util.int_power ( pred a ) p ) * ( pred p ) ;
    liste := List.tl !liste ;
  done ;
  !accu ;;




(** {C § } *)
(** 
{1 Autour de PI}
{1 Around PI}
*)
(** {C  } *)




(**
Source : 

The CAML Numbers Reference Manual by Valérie Ménissier-Morain, technical
report 141, INRIA, july 1992 (available electronically,
ftp://ftp.inria.fr/INRIA/publication/RT/RT-0141.ps.gz).
*)
(** {C § } *)


(** {v num_fact_aux num_integer1 num_integer2 v} *)
let rec num_fact_aux = fun (x:Num.num) (y:Num.num) -> match Num.int_of_num y with
 | 0 -> x
 | 1 -> x
 | _ -> num_fact_aux ( Num.mult_num x y ) ( Num.pred_num y ) ;;

(** {v num_fact num_integer v} *)
let num_fact = function (x:Num.num) -> num_fact_aux num_1 ( Num.abs_num x ) ;;

(** {v print_num_fact num_integer v} *)
let print_num_fact = function (x:Num.num) -> print_string ( Num.string_of_num ( num_fact x ) ) ;;

(** {v sci_fact num_integer v} *)
let sci_fact = function (x:Num.num) -> format [| num_fact x ; num_0 ; num_0 |] ;;


(** {v num_sqrt640320 digits v} *)
let num_sqrt640320 = function (digits:int) ->
 let pow = Num.power_num ( Num.num_of_int 10 ) ( Num.num_of_int digits ) in
  ( Num.num_of_big_int ( Big_int.sqrt_big_int ( Num.big_int_of_num ( Num.mult_num ( Num.mult_num ( Num.num_of_int 640320 ) pow ) pow ) ) ) , pow ) ;;


(** {v num_approx_pi digits v} *)
let num_approx_pi = function (digits:int) ->
 let prod = ref num_12
 and sum = ref ( Num.num_of_int 13591409 )
 and d = ref ( Num.num_of_int 640320 )
 and n = ref ( Num.num_of_int ( 12 * 13591409 ) )
 and sixn = ref num_0
 and binom = ref num_1 (** 3n^2+3n+1 *)
 and pown3 = ref num_0 (** n^3 *)
 and ( sqrt , pow ) = num_sqrt640320 ( digits - 2 )
 and pow3 = Num.power_num ( Num.num_of_int 640320 ) num_3 in
  let pow_test = Num.mult_num pow num_10 in
   while Num.gt_num ( Num.abs_num ( Num.mult_num ( Num.mult_num !prod !sum ) pow_test ) ) !d do
    prod := Num.mult_num ( Num.mult_num ( Num.mult_num ( Num.num_of_int ( - 8 ) ) ( Num.succ_num !sixn ) ) ( Num.mult_num ( Num.add_num !sixn num_3 ) ( Num.add_num !sixn num_5 ) ) ) !prod ;
    sum := Num.add_num ( Num.num_of_int 545140134 ) !sum ;
    pown3 := Num.add_num !binom !pown3 ;
    d := Num.mult_num ( Num.mult_num !pown3 pow3 ) !d ;
    n := Num.add_num ( Num.mult_num ( Num.mult_num !pown3  pow3 ) !n ) ( Num.mult_num !prod !sum ) ;
    sixn := Num.add_num !sixn num_6 ;
    binom := Num.add_num !sixn !binom
   done ;
   Num.div_num ( Num.mult_num sqrt !d ) ( Num.mult_num !n pow ) ;;


(** {v num_show_pi digits v} *)
let num_show_pi = function (digits:int) ->
 Num.approx_num_fix digits ( num_approx_pi digits ) ;;


(** {v sci_approx_pi digits v} *)
let sci_approx_pi = function (digits:int) ->
 format [| num_approx_pi digits ; num_0 ; num_0 |] ;;


(** {v pi_1000_10 v} *)
let pi_1000_10 = sci_approx_pi 1000 ;;

(** {v half_pi_1000_10 v} *)
let half_pi_1000_10 = mult sci_05 pi_1000_10 ;;

(** {v sci_approx_ipi digits v} *)
let sci_approx_ipi = function (digits:int) ->
 format [| num_0 ; num_approx_pi digits ; num_0 |] ;;




(** {C § } *)
(** 
{1 Fonctions de base en précision fixe}
{1 Fixed precision basic functions}
*)
(** {C  } *)




(** The mantissa is truncated at 1024 bits.

La mantisse est tronquée à 1024 bits. *)


(** {v format_1024 number v} *)
let format_1024 = function (a:Num.num array ) ->
 let aa = format a in
  let x = Num.mult_num aa.(0) num_2_pow_1024
  and y = Num.mult_num aa.(1) num_2_pow_1024 in
   let xx = Util.round_num x
   and yy = Util.round_num y in
    format [| xx ; yy ; Num.sub_num aa.(2) num_1024 |] ;;

(** {v mantissa_threshold v} *)
let mantissa_threshold = [| num_1 ; num_0 ; num_minus_1024 |] ;;

(** {v print_sci_1024_2 number v} *)
let print_sci_1024_2 = function (a:Num.num array) ->
 print_string ( "real mantissa : " ^ ( Num.approx_num_exp 310 a.(0) ) ) ;
 print_newline () ;
 print_string ( "imaginary mantissa : " ^ ( Num.approx_num_exp 310 a.(1) ) ) ;
 print_newline () ;
 print_string ( "binary exponent : " ^ ( Num.string_of_num a.(2) ) ) ;
 print_newline () ;;


(** {v print_sci_1024_10 number v} *)
let print_sci_1024_10 = function (a:Num.num array) ->
 let b = Num.power_num num_2 a.(2) in
  print_string ( "real part : " ^ ( Num.approx_num_exp 310 ( Num.mult_num a.(0) b ) ) ) ;
  print_newline () ;
  print_string ( "imaginary part : " ^ ( Num.approx_num_exp 310 ( Num.mult_num a.(1) b ) ) ) ;
  print_newline () ;;

(** {v plus_1024 number1 number2 v} *)
let plus_1024 = fun (a:Num.num array) (b:Num.num array) ->
 let p = plus ( format_1024 a ) ( format_1024 b ) in
  let x = Num.mult_num p.(0) num_2_pow_1024
  and y = Num.mult_num p.(1) num_2_pow_1024 in
   let xx = Util.round_num x
   and yy = Util.round_num y in
    format [| xx ; yy ; Num.sub_num p.(2) num_1024 |] ;;
     
(** {v minus_1024 number1 number2 v} *)
let minus_1024 = fun (a:Num.num array) (b:Num.num array) ->
 let p = minus ( format_1024 a ) ( format_1024 b ) in
  let x = Num.mult_num p.(0) num_2_pow_1024
  and y = Num.mult_num p.(1) num_2_pow_1024 in
   let xx = Util.round_num x
   and yy = Util.round_num y in
    format [| xx ; yy ; Num.sub_num p.(2) num_1024 |] ;;

(** {v mult_1024 number1 number2 v} *)
let mult_1024 = fun (a:Num.num array) (b:Num.num array) ->
 let aa = format a
 and bb = format b in
  let xa = Util.round_num ( Num.mult_num aa.(0) num_2_pow_1024 )
  and ya = Util.round_num ( Num.mult_num aa.(1) num_2_pow_1024 )
  and xb = Util.round_num ( Num.mult_num bb.(0) num_2_pow_1024 )
  and yb = Util.round_num ( Num.mult_num bb.(1) num_2_pow_1024 )
  and w = Num.add_num aa.(2) bb.(2) in
   let x = Num.mult_num xa xb
   and xx = Num.mult_num ya yb
   and y = Num.mult_num xa yb
   and yy = Num.mult_num ya xb in
    format_1024 [| Num.sub_num x xx ; Num.add_num y yy ; Num.sub_num w num_2048 |] ;;

(** {v square_module_1024 number v} *)
let square_module_1024 = function (a:Num.num array) ->
 let x = real_part a
 and y = imag_part a in
  plus_1024 ( mult_1024 x x ) ( mult_1024 y y ) ;;

(** {v norm_inf_1024 number v} *)
let norm_inf_1024 = function (a:Num.num array) ->
 let x = a.(0)
 and y = a.(1)
 and w = a.(2) in
  let m = Num.max_num ( Num.abs_num x ) ( Num.abs_num y ) in
   format_1024 [| m ; num_0 ; w |] ;;

(** {v norm_1_1024 number v} *)
let norm_1_1024 = function (a:Num.num array) ->
 let x = a.(0)
 and y = a.(1)
 and w = a.(2) in
  let m = Num.add_num ( Num.abs_num x ) ( Num.abs_num y ) in
   format_1024 [| m ; num_0 ; w |] ;;

(** {v div_1024 dividend divisor v} *)
let div_1024 = fun (a:Num.num array) (b:Num.num array) ->
 let aa = format_1024 a
 and bb = format_1024 b in
  format_1024 ( div aa bb ) ;;


(** {v real_sqrt_1024 number v} This function operates on the real part.

Cette fonction agit sur la partie réelle. *)
let real_sqrt_1024 = function (a:Num.num array) ->
 let aa = real_part a in
  let w = aa.(2) in
   let ww = Num.quo_num w num_2
   and www = Num.mod_num w num_2
   and x = ref ( Num.abs_num aa.(0) ) in
    if Num.sign_num www > 0 then x := Num.mult_num !x num_2 ;
    if Num.sign_num www < 0 then x := Num.mult_num !x num_05 ;
    let y = Num.mult_num !x num_2_pow_2048 in
     let z = Util.round_num y in
      let r = Num.num_of_big_int ( Big_int.sqrt_big_int ( Num.big_int_of_num z ) ) in
       let s = Num.mult_num r num_2_pow_minus_1024 in
        [| s ; num_0 ; ww |] ;;


(** {v module_1024 number v} *)
let module_1024 = function (a:Num.num array) ->
 real_sqrt_1024 ( square_module_1024 a ) ;;

(** {v real_part_compare_1024 number1 number2 v} *)
let real_part_compare_1024 = fun (x:Num.num array) (y:Num.num array) ->
 let z = minus_1024 x y in
  Num.compare_num z.(0) num_0 ;;

(** {v imag_part_compare_1024 number1 number2 v} *)
let imag_part_compare_1024 = fun (x:Num.num array) (y:Num.num array) ->
 let z = minus_1024 x y in
  Num.compare_num z.(1) num_0 ;;

(** {v eq_0_1024 number v} *)
let eq_0_1024 = function (z:Num.num array) ->
 ( ( Num.compare_num z.(0) num_0 ) = 0 ) && ( ( Num.compare_num z.(1) num_0 ) = 0 ) ;;

(** {v sqrt_1024 number v} *)
let sqrt_1024 = function (a:Num.num array) ->
 let m = square_module a
 and test = complex_of_sci [| a.(0) ; a.(1) ; num_0 |]
 and d = real_part a
 and dp = imag_part a
 and x = ref sci_0
 and y = ref sci_0 in
  if abs_float test.(0).(0) +. abs_float test.(0).(1) < epsilon_float then
   sci_0
  else
  let s = real_sqrt_1024 m in
   let pp = plus s d
   and mm = minus s d in
    let p_p = square_module pp
    and m_m = square_module mm in
     let c = Num.sign_num ( minus p_p m_m ).(0) in
      if c >= 0 then 
       begin
        x := real_sqrt_1024 ( mult sci_05 pp ) ;
        y := div dp ( mult sci_2 !x ) ;
       end
      else
       begin
        y := real_sqrt_1024 ( mult sci_05 mm ) ;
        x := div dp ( mult sci_2 !y ) ;
       end ;
      plus_1024 !x ( itimes !y ) ;;

(** {v inv_1024 number v} *)
let inv_1024 = function (a:Num.num array) ->
 let aa = format_1024 a in
  let x = aa.(0)
  and y = aa.(1)
  and w = aa.(2) in
   let m = Num.add_num ( Num.square_num x ) ( Num.square_num y ) in
    format_1024 [| Num.div_num x m ; Num.div_num ( Num.minus_num y ) m ;  Num.minus_num w |] ;;

(** {v int_pow_1024 exponent number v} *)
let rec int_pow_1024 = fun (n:int) (a:Num.num array) -> match n with
 | 0 -> sci_1
 | 1 -> a
 | -1 -> inv_1024 a
 | _ ->
  begin
   if n < 0 then int_pow_1024 ( abs n ) ( inv_1024 a )
   else
    begin
     let b = int_pow_1024 ( n / 2 ) a in
      if n mod 2 = 0 then mult_1024 b b
      else mult_1024 ( mult_1024 b b ) a
    end
  end ;;


(** {v solve_degree_2_1024 a b c v} Gives the two solutions of [a x ^ 2 + b x + c = 0].

Donne les deux solutions x de [a x ^ 2 + b x + c = 0]. *)
let solve_degree_2_1024 = fun (a:Num.num array) (b:Num.num array) (c:Num.num array) ->
 let bb = mult_1024 b sci_minus_05 in
  let d = minus_1024 ( mult_1024 bb bb ) ( mult_1024 a c ) in
   if eq_0 d then
    begin
     let x = div_1024 bb a in
      [| x ; x |]
    end
   else
    begin
     let dd = sqrt_1024 d in
      let m = minus_1024 bb dd
      and p = plus_1024 bb dd in
       let mm = square_module_1024 m
       and pp = square_module_1024 p in
        let comp = Num.sign_num ( minus_1024 pp mm ).(0) in
         if comp >= 0 then 
          begin
           let x = div_1024 p a in
            [| x ; div_1024 c p |]
          end
         else
          begin
           let x = div_1024 m a in
            [| div_1024 c m ; x |]
          end
    end ;;


(** {v real_cubic_root_1024 threshold number v} This function operates on the real part.

Cette fonction agit sur la partie réelle. *)
let real_cubic_root_1024 = function (a:Num.num array) ->
 let aa = real_part a in
  let w = aa.(2)
  and x = ref ( Num.abs_num aa.(0) ) in
   let ww = Num.quo_num w num_3
   and www = Num.int_of_num ( Num.mod_num w num_3 ) in
    if www = 1 then x := Num.mult_num !x num_2 ;
    if www = 2 then x := Num.mult_num !x num_4 ;
    if www = -1 then x := Num.mult_num !x num_05 ;
    if www = -2 then x := Num.div_num !x num_4 ;
    let xx = Num.float_of_num !x in
     if abs_float xx <= min_float then sci_0
     else
      begin
       let yy = exp ( ( log xx ) /. 3. ) in
        let y = ref ( num_of_float yy )
        and i = ref 0
        and error = ref max_float in
         while !error > min_float do
          let z = !y in
           let zz = Num.div_num ( Num.add_num ( Num.mult_num num_2 z ) ( Num.div_num !x ( Num.square_num z ) ) ) num_3 in
            y := zz ;
            error := Num.float_of_num ( Num.abs_num ( Num.sub_num z zz ) ) ;
            i := succ !i
         done ;
         format_1024 [| Num.mult_num ( Num.num_of_int ( Num.sign_num aa.(0) ) ) !y ; num_0 ; ww |]
      end ;;


(** {v cubic_root_1024 number v} *)
let cubic_root_1024 = function (a:Num.num array) ->
 let w = a.(2)
 and z = ref [| a.(0) ; a.(1) ; num_0 |] in
  let ww = Num.quo_num w num_3
  and www = Num.int_of_num ( Num.mod_num w num_3 ) in
   if www = 1 then z := mult_1024 !z sci_2 ;
   if www = 2 then z := mult_1024 !z sci_4 ;
   if www = -1 then z := mult_1024 !z sci_05 ;
   if www = -2 then z := div_1024 !z sci_4 ;
   let zz = complex_of_sci !z in
    if ( abs_float zz.(0).(0) ) +. ( abs_float zz.(1).(0) ) <= epsilon_float then sci_0
    else
     begin
      let z_z = { Complex.re = zz.(0).(0) ; Complex.im = zz.(1).(0) } in
       let zzz = Complex.exp ( Complex.div ( Complex.log z_z ) { Complex.re = 3. ; Complex.im = 0. } ) in
        let xz = zzz.Complex.re
        and yz = zzz.Complex.im in
         let y = ref ( sci_of_complex [| [| xz ; -. yz |] ; [| yz ; xz |] |] )
         and i = ref 0
         and error = ref max_float in
          while !error > min_float do
           let x = !y in
            let xx = div_1024 ( plus_1024 ( mult_1024 sci_2 x ) ( div_1024 !z ( mult_1024 x x ) ) ) sci_3 in
             y := xx ;
             i := succ !i ;
             let e = complex_of_sci ( minus_1024 x xx ) in
              let ee = e.(0).(0)
              and eee = e.(1).(0) in
               error := sqrt ( ee *. ee +. eee *. eee ) ;
          done ;
          mult_1024 !y [| num_1 ; num_0 ; ww |]
     end ;;


(** {v nth_root_1024 order number v} *)
let nth_root_1024 = fun (n:int) (a:Num.num array) ->
 let w = a.(2)
 and nm1 = pred n
 and nn = Num.num_of_int n
 and n_n = sci_of_int n
 and z = ref [| a.(0) ; a.(1) ; num_0 |] in
  let ww = Num.quo_num w nn
  and n__n = sci_of_int nm1
  and www = Num.int_of_num ( Num.mod_num w nn ) in
   if www <> 0 then z := mult_1024 !z ( int_pow www sci_2 ) ;
   let zz = complex_of_sci !z in
    if ( abs_float zz.(0).(0) ) +. ( abs_float zz.(1).(0) ) <= epsilon_float then sci_0
    else
     begin
      let z_z = { Complex.re = zz.(0).(0) ; Complex.im = zz.(1).(0) } in
       let zzz = Complex.exp ( Complex.div ( Complex.log z_z ) { Complex.re = float n ; Complex.im = 0. } ) in
        let xz = zzz.Complex.re
        and yz = zzz.Complex.im in
         let y = ref ( sci_of_complex [| [| xz ; -. yz |] ; [| yz ; xz |] |] )
         and i = ref 0
         and error = ref max_float in
          while !error > min_float do
           let x = !y in
            let xx = div_1024 ( plus_1024 ( mult_1024 n__n x ) ( div_1024 !z ( int_pow_1024 nm1 x ) ) ) n_n in
             y := xx ;
             i := succ !i ;
             let e = complex_of_sci ( minus_1024 x xx ) in
              let ee = e.(0).(0)
              and eee = e.(1).(0) in
               error := sqrt ( ee *. ee +. eee *. eee ) ;
          done ;
          mult_1024 !y [| num_1 ; num_0 ; ww |]
     end ;;


(** {v sci_1024_jj v} Primitive sixth root of unity.

Racine sixième primitive de l'unité. *)
let sci_1024_jj = cubic_root_1024 sci_minus_1 ;;


(** {v sci_1024_j v} Primitive cubic root of unity.

Racine cubique primitive de l'unité. *)
let sci_1024_j = mult_1024 sci_1024_jj sci_1024_jj ;;


(** {v sci_1024_j2 v} Second primitive cubic root of unity.

Deuxième racine cubique primitive de l'unité. *)
let sci_1024_j2 = conj sci_1024_j


(** {v sci_1024_primitive_root_of_unity integer v} Primitive nth root of unity.

Racine n-ème primitive de l'unité. *)
let sci_1024_primitive_root_of_unity = function (n:int) ->
 if n mod 2 = 0 then
  nth_root_1024 ( n / 2 ) sci_minus_1
 else
  let x = nth_root_1024 n sci_minus_1 in
   mult_1024 x x ;;


(** {v sqrt_1024_of_3 v} *)
let sqrt_1024_of_3 = sqrt_1024 sci_3 ;;

(** {v sqrt_1024_of_27 v} *)
let sqrt_1024_of_27 = sqrt_1024 sci_27 ;;


(** {v solve_degree_3_1024 a b c d v} Gives the three solutions of [a x ^ 3 + b x ^ 2 + c x + d = 0].

Donne les trois solutions x de [a x ^ 3 + b x ^ 2 + c x + d = 0]. *)
let solve_degree_3_1024 = fun (a:Num.num array) (b:Num.num array) (c:Num.num array) (d:Num.num array) ->
 let b_a = div_1024 b a
 and c_a = div_1024 c a
 and d_a = div_1024 d a
 and f = fun x y -> Num.sign_num ( minus_1024 x.(0) y.(0) ).(0) in
  let p = minus_1024 c_a ( div_1024 ( mult_1024 b_a b_a ) sci_3 )
  and q = plus_1024 d_a ( minus_1024 ( div_1024 ( mult_1024 sci_2 ( int_pow_1024 3 b_a ) ) sci_27 ) ( div_1024 ( mult_1024 b_a c_a ) sci_3 ) ) in
   let pp = mult_1024 sci_minus_27 ( int_pow_1024 3 p )
   and qq = mult_1024 sci_27 q in
    let t = solve_degree_2_1024 sci_1 qq pp in
     let m1 = cubic_root_1024 t.(0)
     and m2 = cubic_root_1024 t.(1) in
      let mm1 = square_module_1024 m1
      and mm2 = square_module_1024 m2
      and a1 = ref m1
      and a2 = ref m2 in
       let diff = minus_1024 mm1 mm2
       and m3p = mult_1024 sci_minus_3 p in
        let s = Num.sign_num diff.(0) in
         if s >= 0 then a2 := div_1024 m3p m1
         else a1 := div_1024 m3p m2 ;
         let x1 = ref ( div_1024 ( plus_1024 !a1 !a2 ) sci_3 )
         and x2 = ref ( div_1024 ( plus_1024 ( mult_1024 sci_1024_j2 !a1 ) ( mult_1024 sci_1024_j !a2 ) ) sci_3 )
         and x3 = ref ( div_1024 ( plus_1024 ( mult_1024 sci_1024_j !a1 ) ( mult_1024 sci_1024_j2 !a2 ) ) sci_3 ) in
          let m = Array.map ( function x -> [| square_module_1024 x ; sci_copy x |] ) [| !x1 ; !x2 ; !x3 |] in
           Array.sort f m ;
           x2 := m.(1).(1) ;
           x3 := m.(2).(1) ;
           x1 := div_1024 ( opp q ) ( mult_1024 !x2 !x3 ) ;
           Array.map ( function x -> plus_1024 x ( div_1024 b_a sci_minus_3 ) ) [| !x1 ; !x2 ; !x3 |] ;;


(** {v solve_degree_4_1024 a b c d e v} Gives the four solutions of [a x ^ 4 + b x ^ 3 + c x ^ 2 + d x + e = 0].

Donne les quatre solutions x de [a x ^ 4 + b x ^ 3 + c x ^ 2 + d x + e = 0]. *)
let solve_degree_4_1024 = fun (a:Num.num array) (b:Num.num array) (c:Num.num array) (d:Num.num array) (e:Num.num array) ->
 let b_a = div_1024 b a
 and c_a = div_1024 c a
 and d_a = div_1024 d a
 and e_a = div_1024 e a
 and f = fun x y -> Num.sign_num ( minus_1024 x.(0) y.(0) ).(0) in
  let b_4a = div b_a sci_4 in
   let sq_b_4a = mult_1024 b_4a b_4a in
   let p = minus_1024 c_a ( mult_1024 sq_b_4a sci_6 )
   and q = plus_1024 d_a ( minus_1024 ( int_pow_1024 3 ( mult sci_05 b_a ) ) ( mult sci_05 ( mult_1024 b_a c_a ) ) )
   and r = plus_1024 e_a ( minus_1024 ( mult_1024 c_a sq_b_4a ) ( plus_1024 ( mult_1024 sci_3 ( mult_1024 sq_b_4a sq_b_4a ) ) ( mult_1024 b_4a d_a ) ) )
   and g = function x -> minus_1024 ( mult sci_05 x ) b_4a in
    let pp = mult sci_2 p
    and qq = minus_1024 ( mult_1024 p p ) ( mult_1024 sci_4 r )
    and rr = opp ( mult_1024 q q ) in
     let t = solve_degree_3_1024 sci_1 pp qq rr in
      let m = Array.map ( function x -> [| square_module_1024 x ; sci_copy x |] ) t in
       Array.sort f m ;
       let m3 = sqrt_1024 m.(2).(1)
       and m2 = sqrt_1024 m.(1).(1) in
        let m1 = div_1024 ( opp q ) ( mult_1024 m2 m3 ) in
         let x1 = plus_1024 m1 ( plus_1024 m2 m3 )
         and x2 = minus_1024 m1 ( plus_1024 m2 m3 )
         and x3 = minus_1024 m2 ( plus_1024 m1 m3 )
         and x4 = minus_1024 m3 ( plus_1024 m1 m2 ) in
         Array.map g [| x1 ; x2 ; x3 ; x4 |] ;;


(** {v det_2_1024 sci_2x2_matrix v} *)
let det_2_1024 = fun (m:Num.num array array array) ->
 minus_1024 ( mult_1024 m.(0).(0) m.(1).(1) ) ( mult_1024 m.(0).(1) m.(1).(0) ) ;;

(** {v det_3_1024 sci_3x3_matrix v} *)
let det_3_1024 = fun (m:Num.num array array array) ->
 let a = mult_1024 m.(0).(0) ( det_2_1024 [| [| m.(1).(1) ; m.(1).(2) |] ; [| m.(2).(1) ; m.(2).(2) |] |] )
 and b = mult_1024 m.(0).(1) ( det_2_1024 [| [| m.(1).(2) ; m.(1).(0) |] ; [| m.(2).(2) ; m.(2).(0) |] |] )
 and c = mult_1024 m.(0).(2) ( det_2_1024 [| [| m.(1).(0) ; m.(1).(1) |] ; [| m.(2).(0) ; m.(2).(1) |] |] ) in
  plus_1024 ( plus_1024 a b ) c ;;




(** {C § } *)
(** 
{1 Accélérateurs de convergence en précision fixe}
{1 Fixed precision convergence accelerators}
*)
(** {C  } *)




(** {v aitken_seki_1024 u(n) u(n+1) u(n+2) v} *)
let aitken_seki_1024 = fun (a:Num.num array) (b:Num.num array) (c:Num.num array) ->
(** alternative definition
 let numer = det_2_1024 [| [| a ; b |] ; [| b ; c |] |]
 and denom = minus_1024 ( plus_1024 a c ) ( mult_1024 sci_2 b ) in
  div_1024 numer denom ;;
*)
 let d = minus_1024 b a
 and e = minus_1024 c b in
  let f = mult_1024 d e
  and g = minus_1024 d e in
   let h = div_1024 f g in
    plus_1024 b h ;;


(** {v aitken_seki_rec_1024 k n value_array v} *)
let rec aitken_seki_rec_1024 = fun (k:int) (n:int) (s:Num.num array array) ->
 if k < -1 then failwith "Needed k >= -1 in Sci.aitken_seki_rec_1024." ;
 if n < 0 then failwith "Negative index of sequence in Sci.aitken_seki_rec_1024." ;
 if Array.length s <= n + 2 * k then failwith "Too short sequence in Sci.aitken_seki_rec_1024." ;
 match k with
 | 0 -> s.(n)
 | 1 ->
  begin
   try
    aitken_seki_1024 s.(n) s.( n + 1 ) s.( n + 2 )
   with Failure unknown ->
    s.( n + 2 )
  end
 | _ ->
  let kk = pred k in
   let a = aitken_seki_rec_1024 kk n s
   and b = aitken_seki_rec_1024 kk ( n + 1 ) s
   and c = aitken_seki_rec_1024 kk ( n + 2 ) s in
    if eq_0 ( minus_1024 b a ) || eq_0 ( minus_1024 c b ) || eq_0 ( minus_1024 a c ) then c
    else
     begin
      try
       aitken_seki_1024 a b c
      with Failure unknown ->
       c
     end ;;


(** {v shanks2_1024 u(n) u(n+1) u(n+2) u(n+3) u(n+4) v} *)
let shanks2_1024 = fun (a:Num.num array) (b:Num.num array) (c:Num.num array) (d:Num.num array) (e:Num.num array) ->
 let delta0 = minus_1024 b a
 and delta1 = minus_1024 c b
 and delta2 = minus_1024 d c
 and delta3 = minus_1024 e d in
  let dd0 = minus_1024 delta1 delta0
  and dd1 = minus_1024 delta2 delta1
  and dd2 = minus_1024 delta3 delta2 in
   let denom = det_2_1024 [| [| dd0 ; dd1 |] ; [| dd1 ; dd2 |] |]
   and numer = det_3_1024 [| [| a ; b ; c |] ; [| b ; c ; d |] ; [| c ; d ; e |] |] in
    div_1024 numer denom ;;


(** {v wynn_1024 k n value_array v} *)
let rec wynn_1024 = fun (k:int) (n:int) (s:Num.num array array) ->
 if k < -1 then failwith "Needed k >= -1 in Sci.wynn_1024." ;
 let km1 = pred k
 and km2 = k - 2
 and np = succ n in
  match k with
  | -1 -> sci_0
  | 0 ->
   begin
    if n < 0 then failwith "Negative index of sequence in Sci.wynn_1024." ;
    if n > pred ( Array.length s ) then failwith "Too short sequence in Sci.wynn_1024." ;
    s.(n)
   end
  | _ -> 
   begin
    let a = wynn_1024 km2 np s
    and b = wynn_1024 km1 np s
    and c = wynn_1024 km1 n s in
     let d = minus_1024 b c in
      if eq_0 d then b
      else plus_1024 a ( div_1024 sci_1 d )
   end ;;


(** {v wynn_rho_1024 k n value_array v} *)
let rec wynn_rho_1024 = fun (k:int) (n:int) (s:Num.num array array) ->
 if k < -1 then failwith "Needed k >= -1 in Sci.wynn_rho_1024." ;
  let km1 = pred k
  and km2 = k - 2
  and np = succ n in
   match k with
   | -1 -> sci_0
   | 0 ->
    begin
     if n < 0 then failwith "Negative index of sequence in Sci.wynn_rho_1024." ;
     if n > pred ( Array.length s ) then failwith "Too short sequence in Sci.wynn_rho_1024." ;
     s.(n)
    end
   | _ -> 
    begin
     let a = wynn_rho_1024 km2 np s
     and b = wynn_rho_1024 km1 np s
     and c = wynn_rho_1024 km1 n s in
      let d = minus_1024 b c in
       if eq_0 d then b
       else plus_1024 a ( div_1024 ( sci_of_int k ) d )
    end ;;


(** {v brezinski_1024 k n value_array v} *)
let rec brezinski_1024 = fun (k:int) (n:int) (s:Num.num array array) ->
 if k < -1 then failwith "Needed k >= -1 in Sci.brezinski_1024." ;
 match k with
 | -1 -> sci_0
 | 0 ->
  begin
   if n < 0 then failwith "Negative index of sequence in Sci.brezinski_1024." ;
   if n > pred ( Array.length s ) then failwith "Too short sequence in Sci.brezinski_1024." ;
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
      let a = brezinski_1024 km2 np s
      and b = brezinski_1024 km1 np s
      and c = brezinski_1024 km1 n s in
       let d = minus_1024 b c in
        if eq_0 d then b
        else plus_1024 a ( div_1024 sci_1 d )
     end
    | _ ->
     begin
      let a = brezinski_1024 km2 np s
      and np2 = succ np in
       let b = brezinski_1024 km1 np2 s
       and bb = brezinski_1024 km1 np s
       and c = brezinski_1024 km2 np2 s
       and cc = brezinski_1024 km2 np s in
        let d = minus_1024 b bb
        and dd = minus_1024 c cc in
         let ee = mult_1024 d dd
         and eee = plus_1024 ( brezinski_1024 km1 n s ) ( brezinski_1024 km1 np2 s ) in
          let eeee = minus_1024 eee ( mult_1024 sci_2 ( brezinski_1024 km1 np s ) ) in
           if eq_0 eeee then b
           else plus_1024 a ( div_1024 ee eeee )
     end
  end ;;


(** {v approx_1024 value_array v} *)
let approx_1024 = function (s:Num.num array array) ->
 let rr = pred ( Array.length s ) in
  let kk = rr / 2
  and parity = rr mod 2 in
   if parity = 0 then ( aitken_seki_rec_1024 kk 0 s )
   else ( aitken_seki_rec_1024 kk 1 s ) ;;




(** {C § } *)
(** 
{1 Fonctions classiques en précision fixe}
{1 Fixed precision classical functions}
*)
(** {C  } *)




(** {v pi_1024 v} *)
let pi_1024 = format_1024 ( sci_approx_pi 310 ) ;;

(** {v minus_pi_1024 v} *)
let minus_pi_1024 = format_1024 ( opp ( sci_approx_pi 310 ) ) ;;

(** {v ipi_1024 v} *)
let ipi_1024 = format_1024 ( sci_approx_ipi 310 ) ;;

(** {v minus_ipi_1024 v} *)
let minus_ipi_1024 = format_1024 ( opp ( sci_approx_ipi 310 ) ) ;;

(** {v half_pi_1024 v} *)
let half_pi_1024 = mult_1024 sci_05 pi_1024 ;;

(** {v minus_half_pi_1024 v} *)
let minus_half_pi_1024 = mult_1024 sci_05 minus_pi_1024 ;;

(** {v half_ipi_1024 v} *)
let half_ipi_1024 = mult_1024 sci_05 ipi_1024 ;;

(** {v minus_half_ipi_1024 v} *)
let minus_half_ipi_1024 = mult_1024 sci_05 minus_ipi_1024 ;;

(** {v quarter_pi_1024 v} *)
let quarter_pi_1024 = mult_1024 sci_025 pi_1024 ;;

(** {v minus_quarter_pi_1024 v} *)
let minus_quarter_pi_1024 = mult_1024 sci_025 minus_pi_1024 ;;

(** {v quarter_ipi_1024 v} *)
let quarter_ipi_1024 = mult_1024 sci_025 ipi_1024 ;;

(** {v minus_quarter_ipi_1024 v} *)
let minus_quarter_ipi_1024 = mult_1024 sci_025 minus_ipi_1024 ;;

(** {v e_1024 v} *)
let e_1024 = format_1024 [| Data.Classical.num_e_1024 ; num_0 ; num_0 |] ;;

(** {v minus_e_1024 v} *)
let minus_e_1024 = opp e_1024 ;;

(** {v log_2_1024 v} *)
let log_2_1024 = format_1024 [| Data.Classical.num_log_2_1024 ; num_0 ; num_0 |] ;;

(** {v log_10_1024 v} *)
let log_10_1024 = format_1024 [| Data.Classical.num_log_10_1024 ; num_0 ; num_0 |] ;;

(** {v integer_part_1024 number v} *)
let integer_part_1024 = function (x:Num.num array) ->
 if Num.ge_num x.(2) num_1024 then failwith "Not an acceptable number in Sci.integer_part_1024." ;
 if Num.lt_num x.(2) num_0 then
  sci_0
 else
  begin
   let factor = Num.power_num num_2 x.(2) in
    let xx = Num.mult_num factor x.(0)
    and yy = Num.mult_num factor x.(1) in
     format_1024 [| Num.integer_num xx ; Num.integer_num yy ; num_0 |]
  end ;;

(** {v round_1024 number v} *)
let round_1024 = function (x:Num.num array) ->
 if Num.ge_num x.(2) num_1024 then failwith "Not an acceptable number in Sci.round_1024." ;
 if Num.lt_num x.(2) num_0 then
  sci_0
 else
  begin
   let factor = Num.power_num num_2 x.(2) in
    let xx = Num.mult_num factor x.(0)
    and yy = Num.mult_num factor x.(1) in
     format_1024 [| Util.round_num xx ; Util.round_num yy ; num_0 |]
  end ;;

(** {v fractional_part_1024 number v} *)
let fractional_part_1024 = function (x:Num.num array) ->
 let xx = integer_part_1024 x in
  minus_1024 x xx ;;

(** {v th_threshold v} *)
let th_threshold = 511.5 *. ( log 2. ) ;;

(** {v quadruple_th_real v} *)
let quadruple_th_real = function (x:Num.num array) ->
 let yy = mult x x
 and y4 = [| x.(0) ; num_0 ; Num.add_num num_2 x.(2) |] in
  let y_y = plus yy sci_1
  and yy4 = [| yy.(0) ; num_0 ; Num.add_num num_2 yy.(2) |] in
   let yyy = mult y_y y_y
   and n = mult y4 y_y in
    let d = plus yyy yy4 in
     try
      div_1024 n d
     with Failure unknown -> sci_0 ;;

(** {v quadruple_tan_real v} *)
let quadruple_tan_real = function (x:Num.num array) ->
 let yy = mult x x
 and y4 = [| x.(0) ; num_0 ; Num.add_num num_2 x.(2) |] in
  let y_y = minus sci_1 yy
  and yy4 = [| yy.(0) ; num_0 ; Num.add_num num_2 yy.(2) |] in
   let yyy = mult y_y y_y
   and n = mult y4 y_y in
    let d = minus yyy yy4 in
     try
      div_1024 n d
     with Failure unknown -> sci_0 ;;


(** Some of the following functions make use of a threshold.
The value of the threshold acts upon the speed and the precision of the calculus.
A good value for the threshold may be between 0.01 and 1e-6.

Certaines des fonctions suivantes utilisent un seuil.
La valeur du seuil influence la vitesse et la précision du calcul.
Une bonne valeur pour le seuil peut être entre 0.01 et 1e-6. *)

(** The two following functions have mutually recursive definitions.

les deux fonctions qui suivent ont des définitions mutuellement récursives. *)

(** {v th_of_real_1024 threshold number v} *)
let rec th_of_real_1024 = fun (threshold:float) (x:Num.num array) ->
 if Num.lt_num x.(0) num_0 then
  opp ( th_of_real_1024 threshold ( opp x ) )
 else
  begin
   let greater_than_threshold = ( complex_of_sci x ).(0).(0) >= th_threshold
   and seuil = abs_float threshold in
    match greater_than_threshold with
    | true -> sci_1
    | false ->
     begin
      if ( complex_of_sci x ).(0).(0) >= seuil then
       begin
        let y = th_of_real_1024 threshold [| x.(0) ; num_0 ; Num.add_num num_minus_2 x.(2) |] in
         quadruple_th_real y
       end
      else
       begin
        let y = expm1_1024 [| Num.minus_num x.(0) ; num_0 ; Num.succ_num x.(2) |] in
         let yy = plus sci_2 y in
           div_1024 ( opp y ) yy
       end
    end
  end

(** {v expm1_1024 number v} *)
and expm1_1024 = function (x:Num.num array) ->
 let xx = ref ( complex_of_sci x ).(0)
 and shift = ref x
 and i = ref num_1
 and result = ref sci_0
 and gauge = ref 1 in
  while !gauge >= 0 do
   result := plus !result !shift ;
   Num.incr_num i ;
   shift := div_1024 ( mult x !shift ) ( sci_of_num !i ) ;
   xx := ( complex_of_sci [| !shift.(0) ; !shift.(1) ; Num.add_num num_3 !shift.(2) |] ).(0) ;
   gauge := compare ( ( abs_float !xx.(0) ) +. ( abs_float !xx.(1) ) ) min_float ;
  done ;
  format_1024 !result ;;

(** {v tan_of_real_1024 threshold number v} *)
let rec tan_of_real_1024 = fun (threshold:float) (x:Num.num array) ->
 let xx = real_part x
 and seuil = abs_float threshold in
  let q = integer_part_1024 ( div xx pi_1024 ) in
   let x_x = minus x ( mult q pi_1024 ) in
    if Num.lt_num x_x.(0) num_0 then
     opp ( tan_of_real_1024 threshold ( opp x_x ) )
    else
     begin
      if real_part_compare_1024 x_x half_pi_1024 = 0 then failwith "Infinity" ;
      if ( complex_of_sci x_x ).(0).(0) >= seuil then
       begin
        let y = tan_of_real_1024 threshold [| x_x.(0) ; num_0 ; Num.add_num num_minus_2 x_x.(2) |] in
         quadruple_tan_real y
       end
      else
       begin
        let y = expm1_1024 [| num_0 ; Num.minus_num x_x.(0) ; Num.succ_num x_x.(2) |] in
         let yy = plus sci_2 y in
          div_1024 [| Num.minus_num y.(1) ; y.(0) ; y.(2) |] yy
       end
     end ;;

(** {v direct_tan_1024 threshold number v} *)
let direct_tan_1024 = fun (threshold:float) (z:Num.num array) ->
 let x = real_part z
 and y = imag_part z in
  try
   begin
    let tx = tan_of_real_1024 threshold x
    and ty = th_of_real_1024 threshold y in
     let ity = [| num_0 ; ty.(0) ; ty.(2) |] in
      let n = plus tx ity
      and d = minus sci_1 ( mult tx ity ) in
       try
        div_1024 n d
       with Failure unknown -> sci_0
   end
  with Failure "Infinity" ->
   begin
    if real_part_compare_1024 y sci_0 = 0 then failwith "Infinity" ;
    try
     div_1024 sci_i ( th_of_real_1024 threshold y )
    with
     Failure unknown -> sci_0
   end ;;

(** {v tan_1024 number v} *)
let tan_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_0
 else
  begin
   (* let t1 = direct_tan_1024 1e-2 z *)
   (* and t2 = direct_tan_1024 1e-3 z *)
   (* and t3 = direct_tan_1024 1e-4 z in *)
   let t1 = direct_tan_1024 1e-8 z
   and t2 = direct_tan_1024 1e-11 z
   and t3 = direct_tan_1024 1e-14 z in
    approx_1024 [| t1 ; t2 ; t3 |]
  end ;;

(** {v tan_dot_1024 number v} *)
let tan_dot_1024 = function (z:Num.num array) ->
 let t = tan_1024 z in
  plus_1024 sci_1 ( mult t t ) ;;

(** {v direct_th_1024 threshold number v} *)
let direct_th_1024 = fun (threshold:float) (z:Num.num array) ->
 let x = real_part z
 and y = imag_part z in
  try
   begin
    let tx = th_of_real_1024 threshold x
    and ty = tan_of_real_1024 threshold y in
     let ity = [| num_0 ; ty.(0) ; ty.(2) |] in
      let n = plus tx ity
      and d = plus sci_1 ( mult tx ity ) in
       try
        div_1024 n d
       with Failure unknown -> sci_0
   end
  with Failure "Infinity" ->
   begin
    if real_part_compare_1024 x sci_0 = 0 then failwith "Infinity" ;
    try
     inv_1024 ( th_of_real_1024 threshold x )
    with
     Failure unknown -> sci_0
   end ;;

(** {v th_1024 number v} *)
let th_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_0
 else
  begin
 (* let t1 = direct_th_1024 1e-2 z *)
 (* and t2 = direct_th_1024 1e-3 z *)
 (* and t3 = direct_th_1024 1e-4 z in *)
   let t1 = direct_th_1024 1e-8 z
   and t2 = direct_th_1024 1e-11 z
   and t3 = direct_th_1024 1e-14 z in
    approx_1024 [| t1 ; t2 ; t3 |]
  end ;;

(** {v th_dot_1024 number v} *)
let th_dot_1024 = function (z:Num.num array) ->
 let t = th_1024 z in
  minus_1024 sci_1 ( mult t t ) ;;

(** {v cos_1024 number v} *)
let cos_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_1
 else
  begin
   try
    begin
     let t = tan_1024 [| z.(0) ; z.(1) ; Num.pred_num z.(2) |] in
      let t2 = mult t t in
       let n = minus sci_1 t2
       and d = plus sci_1 t2 in
        try
         div_1024 n d
        with Failure unknown -> sci_0
    end
   with Failure "Infinity" -> sci_minus_1
  end ;;

(** {v direct_sin_1024 number v} *)
let direct_sin_1024 = function (x:Num.num array) ->
 let xx = ref ( complex_of_sci x ).(0)
 and x2 = ( mult x x)
 and shift = ref x
 and i = ref num_1
 and d = ref num_1
 and result = ref x
 and gauge = ref 1 in
  while !gauge >= 0 do
   Num.incr_num i ;
   d := Num.mult_num !d !i ;
   shift := mult x2 !shift ;
   Num.incr_num i ;
   d := Num.mult_num !d ( Num.minus_num !i ) ;
   shift := div_1024 !shift ( sci_of_num !d ) ;
   xx := ( complex_of_sci [| !shift.(0) ; !shift.(1) ; Num.add_num num_3 !shift.(2) |] ).(0) ;
   gauge := compare ( ( abs_float !xx.(0) ) +. ( abs_float !xx.(1) ) ) min_float ;
   result := plus_1024 !result !shift ;
  done ;
  !result ;;

(** {v sin_1024 number v} *)
let sin_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_0
 else
  begin
   let test = complex_of_sci z in
    if log ( abs_float test.(0).(0) +. abs_float test.(0).(1) ) < -3. then
     direct_sin_1024 z
    else
     try
      begin
       let t = tan_1024 [| z.(0) ; z.(1) ; Num.pred_num z.(2) |] in
        let t2 = mult t t in
         let n = mult sci_2 t
         and d = plus sci_1 t2 in
          try
           div_1024 n d
          with Failure unknown -> sci_0
      end
     with Failure "Infinity" -> sci_0
  end ;;

(** {v ch_1024 number v} *)
let ch_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_1
 else
  begin
   try
    begin
     let t = th_1024 [| z.(0) ; z.(1) ; Num.pred_num z.(2) |] in
      let t2 = mult t t in
       let n = plus sci_1 t2
       and d = minus sci_1 t2 in
        try
         div_1024 n d
        with Failure unknown -> sci_0
    end
   with Failure "Infinity" -> sci_minus_1
  end ;;

(** {v direct_sh_1024 number v} *)
let direct_sh_1024 = function (x:Num.num array) ->
 let xx = ref ( complex_of_sci x ).(0)
 and x2 = ( mult x x)
 and shift = ref x
 and i = ref num_1
 and d = ref num_1
 and result = ref x
 and gauge = ref 1 in
  while !gauge > 0 do
   Num.incr_num i ;
   d := Num.mult_num !d !i ;
   shift := mult x2 !shift ;
   Num.incr_num i ;
   d := Num.mult_num !d !i ;
   shift := div_1024 !shift ( sci_of_num !d ) ;
   xx := ( complex_of_sci [| !shift.(0) ; !shift.(1) ; Num.add_num num_3 !shift.(2) |] ).(0) ;
   gauge := compare ( ( abs_float !xx.(0) ) +. ( abs_float !xx.(1) ) ) min_float ;
   result := plus_1024 !result !shift ;
  done ;
  !result ;;

(** {v sh_1024 number v} *)
let sh_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_0
 else
  begin
   let test = complex_of_sci z in
    if log ( abs_float test.(0).(0) +. abs_float test.(0).(1) ) < -3. then
     direct_sh_1024 z
    else
     try
      begin
       let t = th_1024 [| z.(0) ; z.(1) ; Num.pred_num z.(2) |] in
        let t2 = mult t t in
         let n = mult sci_2 t
         and d = minus sci_1 t2 in
          try
           div_1024 n d
          with Failure unknown -> sci_0
      end
     with Failure "Infinity" -> sci_0
  end ;;

(** {v exp_1024 number v} *)
let exp_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_1
 else
  begin
   let test = complex_of_sci z in
    if log ( abs_float test.(0).(0) +. abs_float test.(0).(1) ) < -3. then
     plus_1024 sci_1 ( expm1_1024 z )
    else
     try
      begin
       let t = th_1024 [| z.(0) ; z.(1) ; Num.pred_num z.(2) |] in
        let n = plus sci_1 t
        and d = minus sci_1 t in
         try
          div_1024 n d
         with Failure unknown -> sci_0
      end
     with Failure "Infinity" -> sci_minus_1
  end ;;

(** cotan_1024 number *)
let cotan_1024 = function (x:Num.num array) ->
 let t = ref sci_1
 and annihilate = ref false in
  begin
   try
    t := tan_1024 x
   with Failure "Infinity" -> ( annihilate := true )
  end ;
  if eq_0_1024 !t then failwith "Infinity" ;
  if !annihilate then
   sci_0
  else
   begin
    try
     inv_1024 !t
    with Failure unknown -> failwith "Infinity"
   end ;;

(** sec_1024 number *)
let sec_1024 = function (x:Num.num array) ->
 let c = ref sci_1
 and annihilate = ref false in
  begin
   try
    c := cos_1024 x
   with Failure "Infinity" -> ( annihilate := true )
  end ;
  if eq_0_1024 !c then failwith "Infinity" ;
  if !annihilate then
   sci_0
  else
   begin
    try
     inv_1024 !c
    with Failure unknown -> failwith "Infinity"
   end ;;

(** sech_1024 number *)
let sech_1024 = function (x:Num.num array) ->
 let c = ref sci_1
 and annihilate = ref false in
  begin
   try
    c := ch_1024 x
   with Failure "Infinity" -> ( annihilate := true )
  end ;
  if eq_0_1024 !c then failwith "Infinity" ;
  if !annihilate then
   sci_0
  else
   begin
    try
     inv_1024 !c
    with Failure unknown -> failwith "Infinity"
   end ;;

(** cosec_1024 number *)
let cosec_1024 = function (x:Num.num array) ->
 let s = ref sci_1
 and annihilate = ref false in
  begin
   try
    s := sin_1024 x
   with Failure "Infinity" -> ( annihilate := true )
  end ;
  if eq_0_1024 !s then failwith "Infinity" ;
  if !annihilate then
   sci_0
  else
   begin
    try
     inv_1024 !s
    with Failure unknown -> failwith "Infinity"
   end ;;

(** cosech_1024 number *)
let cosech_1024 = function (x:Num.num array) ->
 let s = ref sci_1
 and annihilate = ref false in
  begin
   try
    s := sh_1024 x
   with Failure "Infinity" -> ( annihilate := true )
  end ;
  if eq_0_1024 !s then failwith "Infinity" ;
  if !annihilate then
   sci_0
  else
   begin
    try
     inv_1024 !s
    with Failure unknown -> failwith "Infinity"
   end ;;


(** The algorithm of the arithmetic and geometric means comes from the pages 15 and 16 of the document [log2.ps]
at the following address.

[http://numbers.computation.free.fr/Constants/constants.html]

L'algorithme de la moyenne arithmétique et géométrique provient des pages 15 et 16
du document [log2.ps] à l'adresse précédente. *)

(** {v agm_1024 vector v} *)
let agm_1024 = fun (z:Num.num array array) ->
 let x = z.(0)
 and y = z.(1) in
  let s = plus_1024 x y in
   [| [| s.(0) ; s.(1) ; Num.pred_num s.(2) |] ; sqrt_1024 ( mult x y ) |] ;;

(** agm_sum_1024 vector *)
let agm_sum_1024 = fun (z:Num.num array array) ->
 let zz = ref z
 and i = ref num_minus_1
 and f = fun x y -> ( abs_float x ) +. ( abs_float y )
 and z_z = ref ( minus ( mult z.(0) z.(0) ) ( mult z.(1) z.(1) ) ) in
  let error = ref ( complex_of_sci [| !z_z.(0) ; !z_z.(1) ; Num.add_num num_5 !z_z.(2) |] )
  and zzz = ref [| !z_z.(0) ; !z_z.(1) ; Num.pred_num !z_z.(2) |] in
   let result = ref !zzz in
    while f !error.(0).(0) !error.(0).(1) > 32. *. min_float do
     Num.incr_num i ;
     zz := agm_1024 !zz ;
     z_z := minus ( mult !zz.(0) !zz.(0) ) ( mult !zz.(1) !zz.(1) ) ;
     zzz := [| !z_z.(0) ; !z_z.(1) ; Num.add_num !i !z_z.(2) |] ;
     error := complex_of_sci [| !zzz.(0) ; !zzz.(1) ; Num.add_num num_6 !zzz.(2) |] ;
     result := plus !result !zzz ;
   done ;
   format_1024 !result ;;

(** {v agm_log_1024 number v} *)
let agm_log_1024 = function (z:Num.num array) ->
 let factor = [| num_1 ; num_0 ; Num.num_of_int ( -514 ) |] in
  let s1 = agm_sum_1024 [| sci_1 ; factor |]
  and sz = agm_sum_1024 [| sci_1 ; ( mult factor z ) |] in
   let r1 = minus sci_1 s1
   and rz = minus sci_1 sz in
    let n = minus s1 sz
    and d = mult r1 rz in
     div_1024 n d ;;

(** {v log1p_1024 v} *)
let log1p_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_0
 else
  begin
   let error = ref ( complex_of_sci z )
   and i = ref num_2
   and coefficient = ref sci_0
   and sign = ref true
   and f = fun x y -> ( abs_float x ) +. ( abs_float y )
   and puissance = ref ( mult z z )
   and result = ref z in
    while f !error.(0).(0) !error.(0).(1) > min_float do
     coefficient := div !puissance [| !i ; num_0 ; num_0 |] ;
     if !sign then
      result := minus !result !coefficient
     else
      result := plus !result !coefficient ;
     Num.incr_num i ;
     sign := not !sign ;
     puissance := mult z !puissance ;
     error := complex_of_sci [| !coefficient.(0) ; !coefficient.(1) ; Num.add_num num_3 !coefficient.(2) |] ;
    done ;
    format_1024 !result
  end ;;

(** {v direct_log_1024 threshold number v} A good threshold is -100.

Un bon seuil est -100. *)
let direct_log_1024 = fun (threshold:float) (z:Num.num array) ->
 let diff = minus z sci_1 in
  let float_diff = complex_of_sci diff in
   if log ( abs_float float_diff.(0).(0) +. abs_float float_diff.(0).(1) ) < threshold then
    log1p_1024 diff
   else
    let x = agm_log_1024 [| z.(0) ; z.(1) ; num_minus_1 |]
    and y = mult log_2_1024 [| Num.succ_num z.(2) ; num_0 ; num_0 |] in
     plus_1024 x y ;;

(** {v log_1024 number v} *)
let log_1024 = function (z:Num.num array) ->
 let y = direct_log_1024 ( -100. ) z in
  let x = exp_1024 y
  and w0 = plus y sci_minus_1 in
   let w1 = div z x in
    let yy = plus w0 w1 in
     let xx = exp_1024 yy
     and w2 = plus yy sci_minus_1 in
      let w3 = div z xx in
       let yyy = plus w2 w3 in
        approx_1024 [| y ; yy ; yyy |] ;;

(** {v tune_arg function derivative argument candidate v} *)
let tune_arg = fun f fdot (x:Num.num array) (y:Num.num array) ->
 let z = minus_1024 ( f y ) x
 and zz = fdot y in
  minus_1024 y ( div z zz ) ;;

(** {v adapt_arg steps threshold function derivative argument candidate v} *)
let adapt_arg = fun (steps:int) (threshold:float) f fdot (x:Num.num array) (y:Num.num array) ->
 let nouveau = ref ( tune_arg f fdot x y )
 and i = ref 0
 and ancien = ref y in
  let error = ref ( complex_of_sci ( minus !nouveau !ancien ) ).(0) in
   while ( !i < steps ) && ( ( abs_float !error.(0) +. abs_float !error.(1) ) > threshold ) do
    ancien := !nouveau ;
    nouveau := tune_arg f fdot x !nouveau ;
    error := ( complex_of_sci ( minus !nouveau !ancien ) ).(0);
    incr i ;
   done ;
   !nouveau ;;

(** {v argth_1024 number v} *)
let argth_1024 = function (z:Num.num array) ->
 if ( eq_0_1024 ( plus_1024 z sci_minus_1 ) ) || ( eq_0_1024 ( plus_1024 z sci_1 ) ) then
  failwith "Infinity" ;
 if eq_0_1024 z then
  sci_0
 else
  begin
   let test = complex_of_sci z in
    if log ( abs_float test.(0).(0) +. abs_float test.(0).(1) ) < -3. then
     adapt_arg 100 ( 16. *. min_float ) th_1024 th_dot_1024 z z
    else
     let n = plus sci_1 z
     and d = minus sci_1 z in
      try
       begin
        let q = div n d in
         let y = log_1024 q in
          [| y.(0) ; y.(1) ; Num.pred_num y.(2) |]
       end
      with Failure unknown -> failwith "Infinity"
  end ;;

(** {v argch_1024 number v} *)
let argch_1024 = function (z:Num.num array) ->
 let diff = minus z sci_1 in
  if eq_0_1024 diff then
   sci_0
  else
   begin
    let float_test = complex_of_sci diff in
     if log ( abs_float float_test.(0).(0) +. abs_float float_test.(0).(1) ) < -3. then
      adapt_arg 100 ( 16. *. min_float ) ch_1024 sh_1024 z ( sqrt_1024 ( mult sci_2 diff ) )
     else
      let zz = mult z z in
       let s = sqrt_1024 ( plus zz sci_minus_1 ) in
        log_1024 ( plus z s )
   end ;;

(** {v direct_argsh_1024 steps number v} *)
let direct_argsh_1024 = fun (steps:int) (x:Num.num array) ->
 let xx = ref ( complex_of_sci x ).(0)
 and x2 = ( mult x x)
 and shift = ref x
 and power = ref x
 and accu = ref num_1
 and factor = ref num_1
 and i = ref num_0
 and d = ref num_1
 and result = ref x
 and gauge = ref 1 in
  while ( !gauge > 0 ) && ( Num.int_of_num !i < steps ) do
   Num.incr_num i ;
   d := Num.succ_num ( Num.mult_num num_2 !i ) ;
   accu := Num.mult_num !accu ( Num.div_num ( Num.add_num !i num_minus_05 ) ( Num.minus_num !i ) ) ;
   factor := Num.div_num !accu !d ;
   power := mult_1024 x2 !power ;
   shift := mult !power ( sci_of_num !factor ) ;
   xx := ( complex_of_sci [| !shift.(0) ; !shift.(1) ; Num.add_num num_3 !shift.(2) |] ).(0) ;
   gauge := compare ( ( abs_float !xx.(0) ) +. ( abs_float !xx.(1) ) ) min_float ;
   result := plus_1024 !result !shift ;
  done ;
  !result ;;

(** {v argsh_1024 number v} *)
let argsh_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_0
 else
  begin
   let test = complex_of_sci z in
    if log ( abs_float test.(0).(0) +. abs_float test.(0).(1) ) < -3. then
     adapt_arg 100 ( 16. *. min_float ) sh_1024 ch_1024 z ( direct_argsh_1024 2 z )
    else
     let zz = mult z z in
      let s = sqrt_1024 ( plus zz sci_1 ) in
       log_1024 ( plus z s )
  end ;;

(** {v arctan_1024 number v} *)
let arctan_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_0
 else
  begin
   let zz = mult sci_minus_i z in
    mult_1024 sci_i ( argth_1024 zz )
  end ;;

(** {v direct_arcsin_1024 steps number v} *)
let direct_arcsin_1024 = fun (steps:int) (x:Num.num array) ->
 let xx = ref ( complex_of_sci x ).(0)
 and x2 = ( mult_1024 x x)
 and shift = ref x
 and power = ref x
 and accu = ref num_1
 and factor = ref num_1
 and i = ref num_0
 and d = ref num_1
 and result = ref x
 and gauge = ref 1 in
  while ( !gauge >= 0 ) && ( Num.int_of_num !i < steps ) do
   Num.incr_num i ;
   d := Num.succ_num ( Num.mult_num num_2 !i ) ;
   accu := Num.mult_num !accu ( Num.div_num ( Num.add_num !i num_minus_05 ) !i ) ;
   factor := Num.div_num !accu !d ;
   power := mult x2 !power ;
   shift := mult !power ( sci_of_num !factor ) ;
   xx := ( complex_of_sci [| !shift.(0) ; !shift.(1) ; Num.add_num num_3 !shift.(2) |] ).(0) ;
   gauge := compare ( ( abs_float !xx.(0) ) +. ( abs_float !xx.(1) ) ) min_float ;
   result := plus !result !shift ;
  done ;
  !result ;;

(** {v arcsin_1024 number v} *)
let arcsin_1024 = function (z:Num.num array) ->
 if eq_0_1024 z then
  sci_0
 else
  begin
   let test = complex_of_sci z in
    if log ( abs_float test.(0).(0) +. abs_float test.(0).(1) ) < -3. then
     adapt_arg 100 ( 16. *. min_float ) sin_1024 ( function x -> sci_1 ) z ( direct_arcsin_1024 2 z )
    else
     let zz = mult sci_minus_i z in
      mult_1024 sci_i ( argsh_1024 zz )
  end ;;

(** {v arccos_1024 number v} *)
let arccos_1024 = function (z:Num.num array) ->
 let zz = argch_1024 z in
  mult_1024 sci_minus_i zz ;;

(** {v arcsec_1024 number v} *)
let arcsec_1024 = function (z:Num.num array) ->
 arccos_1024 ( inv z ) ;;

(** {v argsech_1024 number v} *)
let argsech_1024 = function (z:Num.num array) ->
 argch_1024 ( inv z ) ;;

(** {v arccosec_1024 number v} *)
let arccosec_1024 = function (z:Num.num array) ->
 arcsin_1024 ( inv z ) ;;

(** {v argcosech_1024 number v} *)
let argcosech_1024 = function (z:Num.num array) ->
 argsh_1024 ( inv z ) ;;

(** {v arccotan_1024 number v} *)
let arccotan_1024 = function (z:Num.num array) ->
 arctan_1024 ( inv z ) ;;

(** {v arccoth_1024 number v} *)
let argcoth_1024 = function (z:Num.num array) ->
 argth_1024 ( inv z ) ;;







(** {C § § § } *)




end
