



module Data2 = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module algebraic structures to use in the modules [sparse_vector.ml], [sparse_tensor.ml], [sparse_matrix.ml], [mat.ml], [fft.ml] as coefficients.

This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module des structures algébriques à utiliser dans les modules [sparse_vector.ml], [sparse_tensor.ml], [sparse_matrix.ml], [mat.ml], [fft.ml] comme coefficients.

Ce module est distribué selon la même licence qu'Ocaml.


{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.2}
*)
(**
@version 0.2
*)
(**
@author Stéphane Grognet
*)
(**
@since 2012, 2013
*)




open Util ;;
open Data ;;
open Matrix ;;
open Sci ;;
open Reduc ;;




module C_coeff = struct

type t = float array array ;;
type u = float ;;
let norm_inject = function (x:u) -> ([| [| x ; 0. |] ; [| 0. ; x |] |]:t) ;;
let norm_zero = function () -> 0. ;;
let norm_of_string = float_of_string ;;
let norm_to_string = string_of_float ;;
let norm_print = print_float ;;
let norm_eq = ( = ) ;;
let norm_eq_zero = ( = ) 0. ;;
let norm_compare = fun (x:u) (y:u) -> ((Pervasives.compare x y):int) ;;
let norm_add = ( +. ) ;;
let norm_int_mult = fun (x:int) (y:u) -> ( float x ) *. y ;;
let norm_mult = ( *. ) ;;
let norm_square = function x -> x *. x ;;
let zero = function () -> Array.make_matrix 2 2 0. ;;
let one = function () -> Matrix.identity_float 2 2 ;;
let of_string = Matrix.matrix_float_of_string
let to_string = Matrix.matrix_float_to_string
let print = Matrix.matrix_float_print ;;
let copy = Matrix.matrix_float_copy ;;
let eq_zero = function x -> Reduc.complex_square_module x == 0. ;;
let eq = fun x y -> eq_zero ( Matrix.matrix_float_minus x y ) ;;
let eq_one = eq ( one () ) ;;
let compare = fun x y -> compare ( Reduc.complex_square_module x ) ( Reduc.complex_square_module y ) ;;
let norm = Reduc.complex_module ;;
let opp = Matrix.matrix_float_opp ;;
let add = Matrix.matrix_float_plus ;;
let sub = Matrix.matrix_float_minus ;;
let int_mult = fun x y -> Matrix.matrix_float_scal_mult ( float x ) y ;;
let mult = Matrix.matrix_float_prod ;;
let square = fun x -> mult x x ;;
let inv = Reduc.complex_inv ;;
let div = Reduc.complex_div ;;
let int_div = fun x y -> Matrix.matrix_float_scal_left_div ( float x ) y ;;
let int_pow = Matrix.float_power ;;

end ;;




module Sci_bare_coeff = struct

include Sci ;;
open Sci ;;

type t = Num.num array ;;
type u = Num.num array ;;
type v = t ;;
let norm_inject = function (x:t) -> (x:v) ;;
let zero = function () -> Sci.sci_0 ;;
let one = function () -> Sci.sci_1 ;;
let of_string = Sci.sci_of_string ;;
let to_string = Sci.sci_to_string ;;
let print = function x ->
 begin
  let s = Sci.sci_to_string_array x in
   print_string ( s.(0) ^ "\n" ) ;
   print_string ( s.(1) ^ "\n" ) ;
   print_string s.(2)
 end
let copy = Sci.sci_copy ;;
let eq_zero = Sci.eq_0 ;;
let eq = Sci.eq ;;
let eq_one = eq ( one () ) ;;
let compare = fun x y ->
 begin
  let xx = Sci.square_module x
  and yy = Sci.square_module y in
   let z = Sci.minus xx yy in
    Num.compare_num Sci.num_0 z.(0)
 end ;;
let norm = Sci.square_module ;;
let add = Sci.plus ;;
let sub = Sci.minus ;;
let square = function x -> mult x x ;;
let int_div = fun x y -> div y ( sci_of_int x ) ;;
let int_mult = fun x y -> mult ( sci_of_int x ) y ;;

end ;;



module Sci_coeff = Data.Normalize_field_coefficient (Sci_bare_coeff) (Sci_bare_coeff) ;;



module Sci_1024_bare_coeff = struct

include Sci ;;

type t = Num.num array ;;
type u = Num.num array ;;
type v = t
let norm_inject = function (x:t) -> (x:v) ;;
let zero = function () -> Sci.sci_0 ;;
let one = function () -> Sci.sci_1 ;;
let of_string = Sci.sci_of_string ;;
let to_string = Sci.sci_to_string ;;
let print = Sci.print_sci_1024_10 ;;
let copy = Sci.sci_copy ;;
let eq_zero = eq_0_1024 ;;
let eq = Sci.eq ;;
let eq_one = eq ( one () ) ;;
let compare = fun x y ->
 begin
  let xx = Sci.square_module x
  and yy = Sci.square_module y in
   let z = Sci.minus_1024 xx yy in
    Num.compare_num Sci.num_0 z.(0)
 end ;;
let norm = Sci.module_1024 ;;
let add = Sci.plus_1024 ;;
let sub = Sci.minus_1024 ;;
let mult = Sci.mult_1024 ;;
let square = function x -> mult x x ;;
let int_mult = fun x y -> mult_1024 ( sci_of_int x ) y ;;
let inv = Sci.inv_1024 ;;
let div = Sci.div_1024 ;;
let int_div = fun x y -> div_1024 y ( sci_of_int x ) ;;
let int_pow = Sci.int_pow_1024 ;;

end ;;



module Sci_1024_coeff = Data.Normalize_field_coefficient (Sci_1024_bare_coeff) (Sci_1024_bare_coeff) ;;






end ;;
