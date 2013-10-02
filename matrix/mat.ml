




module Mat = struct


(**)
(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module functors
to practice calculations on vectors, tensors and matrices
with coefficients in a commutative rng or a field.


{2 Comments}


The objects may indifferently be full or sparse.


This module is distributed under the same licence as Ocaml.

{C § }


La mathématicienne ou le mathématicien trouvera dans ce module des foncteurs 
permettant de pratiquer le calcul sur les vecteurs, tenseurs et matrices
à coefficients dans un annau commutatif ou dans un corps commutatif.


{2 Commentaires}


Les objets peuvent être indifféremment pleins ou creux.


Ce module est distribué selon la même licence qu'Ocaml.


{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.1}
*)
(**
@version 0.1
*)
(**
@author Stéphane Grognet
*)
(**
@since 2012, 2013
*)



(** The following utilities are shared by the functors defined in this module.

Util ; Data ; Sparse ; Sparser

Les utilitaires précédents sont communs aux foncteurs définis dans ce module. *)
open Util ;;
open Data ;;
open Hash ;;
open Sparse_vector ;;
open Sparse_tensor ;;
open Sparse_matrix ;;



module Rng (R:Data.Rng_coeff_type) = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**

*)



(** {C § } *)
(** 
{1 Utilitaires}
{1 Utilities}
*)
(** {C  } *)




module V = Sparse_vector.Rng (Data.Zindex) (Hash.Z) (R) ;;
module T = Sparse_tensor.Rng (Data.Zindex) (Hash.Z) (R) ;;
module M = Sparse_matrix.Rng (Data.Zindex) (Hash.Z) (R) ;;


type coeff = R.t ;;
type index = int ;;
type elt = index * coeff ;;



(** {C § } *)
(** 
{1 Vecteurs}
{1 Vectors}
*)
(** {C  } *)




(** The [vector] type gathers all formats of vectors with coefficients of type [coeff].

Le type [vector] rassemble tous les formats de vecteurs à coefficients de type [coeff]. *)
type vector =
 | Full_vector of coeff array
 | Sparse_vector of V.t ;;

(** {v vector_copy vector v} *)
let vector_copy = function (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.map R.copy w )
 | Sparse_vector w -> Sparse_vector ( V.copy w ) ;;

(** {v vector_resize vector v} *)
let vector_resize = fun (n:int) (v:vector) ->
 match v with
 | Full_vector w -> ()
 | Sparse_vector w -> V.resize n w ;;

(** {v array_null dimension v} *)
let array_null = function (dim:int) ->
 Array.map R.zero ( Array.make dim () ) ;;

(** {v vector_full_null dimension v} *)
let vector_full_null = function (dim:int) ->
 Full_vector ( array_null dim ) ;;

(** {v vector_sparse_null dimension v} *)
let vector_sparse_null = function (dim:int) ->
 Sparse_vector ( V.null dim ) ;;

(** {v vector_null dimension v} *)
let vector_null = vector_sparse_null ;;

(** {v vector_zero unit v} *)
let vector_zero = function () ->
 vector_sparse_null 0 ;;

(** {v vector_full_demakeup vector v} *)
let vector_full_demakeup = function (v:vector) ->
 match v with
 | Full_vector w -> w
 | Sparse_vector w -> failwith "Not a full vector in Mat.vector_full_demakeup." ;;

(** {v vector_sparse_demakeup vector v} *)
let vector_sparse_demakeup = function (v:vector) ->
 match v with
 | Full_vector w -> failwith "Not a sparse vector in Mat.vector_sparse_demakeup."
 | Sparse_vector w -> w ;;


(** {v vector_to_full vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let vector_to_full = function (v:vector) ->
 match v with
 | Full_vector w -> v
 | Sparse_vector w -> Full_vector ( V.to_full w ) ;;


(** {v vector_to_sparse vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let vector_to_sparse = function (v:vector) ->
 match v with
 | Full_vector w -> Sparse_vector ( V.auto_to_sparse w )
 | Sparse_vector w -> v ;;

(** {v vector_dimension vector v} *)
let vector_dimension = function (v:vector) ->
 match v with
 | Full_vector w -> Array.length w
 | Sparse_vector w -> V.dimension w ;;

(** {v vector_nihil vector v} *)
let vector_nihil = function (v:vector) ->
 let d = vector_dimension v in
  match v with
  | Full_vector w -> vector_full_null d
  | Sparse_vector w ->
   begin
    let z = vector_sparse_null d in
     vector_resize ( V.size w ) z ;
     z
   end ;;

(** {v vector_filling vector v} *)
let vector_filling = function (v:vector) ->
 match v with
 | Full_vector w -> failwith "Full vector in Mat.vector_filling."
 | Sparse_vector w -> V.filling w ;;

(** {v vector_size vector v} *)
let vector_size = function (v:vector) ->
 match v with
 | Full_vector w -> failwith "Full vector in Mat.vector_size."
 | Sparse_vector w -> V.size w ;;

(** {v vector_quality vector v} *)
let rec vector_quality = function (v:vector) ->
 match v with
 | Full_vector w -> "Full vector"
 | Sparse_vector w -> "Sparse vector" ;;

(** {v vector_to_string vector v} *)
let rec vector_to_string = function (v:vector) ->
 match v with
 | Full_vector w -> "Full_vector " ^ ( Util.vector_to_string R.to_string "[|" " " "|]" w )
 | Sparse_vector w -> "Sparse_vector " ^ ( V.to_string w ) ;;

(** {v bare_vector_print vector v} *)
let bare_vector_print = function (v:vector) ->
 print_string ( vector_to_string v ) ;;

(** {v vector_print vector v} *)
let vector_print = function (v:vector) ->
 print_string ( vector_to_string v ) ;
 print_newline () ;;

(** {v vector_of_string string v} *)
let vector_of_string = function (s:string) ->
 let lst = String.length s
 and index = Str.search_forward ( Str.regexp " " ) s 0 in
  let qualif = String.sub s 0 index
  and rest = String.sub s ( succ index ) ( lst - index - 1 ) in
   match qualif with
   | "Full_vector" -> Full_vector ( Util.bare_vector_of_string R.of_string rest )
   | "Sparse_vector" -> Sparse_vector ( V.of_string rest )
   | _ -> failwith "Not a valid string in Mat.vector_of_string." ;;

(** {v vector_extract index vector v} *)
let vector_extract = fun (i:int) (v:vector) ->
 match v with
 | Full_vector w -> R.copy w.(i)
 | Sparse_vector w -> V.raw_extract i w ;;


(** {v vector_head index vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let vector_head = fun (i:int) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.sub w 0 ( succ i ) )
 | Sparse_vector w -> Sparse_vector ( V.sub_vector 0 i w ) ;;


(** {v vector_tail index vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let vector_tail = fun (i:int) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Util.array_end i w )
 | Sparse_vector w -> Sparse_vector ( V.sub_vector i ( pred ( V.dimension w ) ) w ) ;;


(** {v sub_vector beginning ending vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let sub_vector = fun (i:int) (j:int) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.sub w i ( j - i + 1 ) )
 | Sparse_vector w -> Sparse_vector ( V.sub_vector i j w ) ;;


(** {v mask_vector beginning ending vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let mask_vector = fun (i:int) (j:int) (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   let r = Array.length w in
    let result = array_null r in
     for k = i to j do
      result.(k) <- w.(k)
     done ;
     Full_vector result
  end
 | Sparse_vector w -> Sparse_vector ( V.mask_vector i j w ) ;;


(** {v vector_beginning index vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let vector_beginning = fun (i:int) (v:vector) ->
 match v with
 | Full_vector w -> mask_vector 0 i v
 | Sparse_vector w -> Sparse_vector ( V.beginning i w ) ;;


(** {v vector_ending index vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let vector_ending = fun (i:int) (v:vector) ->
 match v with
 | Full_vector w -> mask_vector i ( pred ( Array.length w ) ) v
 | Sparse_vector w -> Sparse_vector ( V.ending i w ) ;;


(** {v vector_embed dimension index vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let vector_embed = fun (dimension:int) (shift:int) (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   let t = max 0 shift in
    let tt = max 0 ( dimension - ( t + ( Array.length w ) ) ) in
     Full_vector ( Array.concat [ array_null t ; w ; array_null tt ] )
  end
 | Sparse_vector w -> Sparse_vector ( V.embed dimension shift w ) ;;

(** {v vector_find element vector v} *)
let vector_find = fun (x:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Util.vector_find_first R.eq x w
 | Sparse_vector w -> V.find x w ;;

(** {v vector_find_all element vector v} *)
let vector_find_all = fun (x:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Util.vector_find_all R.eq x w
 | Sparse_vector w -> Array.of_list ( V.index_list_find_all x w ) ;;

(** {v vector_list_find_all element vector v} *)
let vector_list_find_all = fun (x:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Array.to_list ( Util.vector_find_all R.eq x w )
 | Sparse_vector w -> V.index_list_find_all x w ;;

(** {v vector_filter predicate vector v} *)
let vector_filter = fun (p:index -> bool) (v:vector) ->
 match v with
 | Full_vector w -> Util.vector_filter p w
 | Sparse_vector w -> V.filter p w ;;

(** {v vector_first_non_zero vector v} *)
let vector_first_non_zero = fun (v:vector) ->
 match v with
 | Sparse_vector w -> V.first_non_zero w
 | Full_vector w ->
  begin
   let d = Array.length w in
    let i = ref 0
    and index = ref ( -1 ) in
     while !i < d do
      if R.eq_zero w.(!i) then
       incr i
      else
       begin
        index := !i ;
        i := d ;
       end
     done ;
     !index
  end ;;

(** {v vector_last_non_zero vector v} *)
let vector_last_non_zero = function (v:vector) ->
 match v with
 | Sparse_vector w -> V.last_non_zero w
 | Full_vector w ->
  begin
   let d = Array.length w in
    let i = ref ( pred d )
    and index = ref ( -1 ) in
     while !i >= 0 do
      if R.eq_zero w.(!i) then
       decr i
      else
       begin
        index := !i ;
        i := -1 ;
       end
     done ;
     !index
  end ;;

(** {v vector_exchange index1 index2 vector v} *)
let vector_exchange = fun (i:int) (j:int) (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   let tmp = R.copy w.(i) in
    w.(i) <- w.(j) ;
    w.(j) <- tmp ;
  end
 | Sparse_vector w -> V.exchange i j w ;;

(** {v vector_maximum vector v} *)
let vector_maximum = function (v:vector) ->
 match v with
 | Full_vector w -> Util.array_maximum R.compare w
 | Sparse_vector w -> V.max w ;;

(** {v vector_minimum vector v} *)
let vector_minimum = function (v:vector) ->
 match v with
 | Full_vector w -> Util.array_minimum R.compare w
 | Sparse_vector w -> V.min w ;;

(** {v vector_iter function vector v} *)
let vector_iter = fun (f:elt -> unit) (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   let g = fun i x -> f ( i , x ) in
    Array.iteri g w
  end
 | Sparse_vector w -> V.iter f w ;;

(** {v vector_fold function vector init v} *)
let vector_fold = fun f (v:vector) init ->
 match v with
 | Full_vector w ->
  begin
   let accu = ref init in
    for i = 0 to pred ( Array.length w ) do
     accu := f ( i , w.(i) ) !accu 
    done ;
    !accu
  end
 | Sparse_vector w -> V.fold f w init ;;

(** {v vector_in_place_map function vector v} *)
let vector_in_place_map = fun f (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   for i = 0 to pred ( Array.length w ) do
    w.(i) <- f w.(i)
   done ;
  end
 | Sparse_vector w -> V.in_place_map f w ;;

(** {v vector_in_place_mapi function vector v} *)
let vector_in_place_mapi = fun f (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   for i = 0 to pred ( Array.length w ) do
    w.(i) <- f i w.(i)
   done ;
  end
 | Sparse_vector w -> V.in_place_mapi f w ;;

(** {v vector_map function vector v} *)
let vector_map = fun f (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   let r = Array.length w in
    let ww = array_null r in
     for i = 0 to pred r do
      ww.(i) <- f w.(i)
     done ;
     Full_vector ww
  end
 | Sparse_vector w -> Sparse_vector ( V.map f w ) ;;

(** {v vector_mapi function vector v} *)
let vector_mapi = fun f (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   let r = Array.length w in
    let ww = array_null r in
     for i = 0 to pred r do
      ww.(i) <- f i w.(i)
     done ;
     Full_vector ww
  end
 | Sparse_vector w -> Sparse_vector ( V.mapi f w ) ;;

(** {v vector_insert_add coefficient index vector v} *)
let vector_insert_add = fun (x:coeff) (i:int) (v:vector) ->
 match v with
 | Full_vector w -> w.(i) <- R.add w.(i) x
 | Sparse_vector w -> V.insert_add x i w ;;

(** {v vector_insert_sub coefficient index vector v} *)
let vector_insert_sub = fun (x:coeff) (i:int) (v:vector) ->
 match v with
 | Full_vector w -> w.(i) <- R.sub w.(i) x
 | Sparse_vector w -> V.insert_sub x i w ;;

(** {v vector_remove index vector v} *)
let vector_remove = fun (i:int) (v:vector) ->
 match v with
 | Full_vector w -> w.(i) <- R.zero ()
 | Sparse_vector w -> V.remove i w ;;

(** {v vector_replace coefficient index vector v} *)
let vector_replace = fun (x:coeff) (i:int) (v:vector) ->
 match v with
 | Full_vector w -> w.(i) <- x
 | Sparse_vector w -> V.replace x i w ;;

(** {v vector_in_place_opp vector v} *)
let vector_in_place_opp = function (v:vector) ->
 match v with
 | Full_vector w -> Array.iteri ( fun i x -> w.(i) <- R.opp x ) w
 | Sparse_vector w -> V.in_place_opp w ;;

(** {v vector_opp vector v} *)
let vector_opp = function (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.map R.opp w )
 | Sparse_vector w -> Sparse_vector ( V.opp w ) ;;

(** {v vector_eq_zero vector v} *)
let vector_eq_zero = function (v:vector) ->
 match v with
 | Full_vector w -> Util.array_eq_zero R.eq_zero w
 | Sparse_vector w -> V.eq_zero w ;;


(** {v vector_in_place_add vector1 vector2 v} The first vector stores the result.

Le premier vecteur accueille le résultat. *)
let vector_in_place_add = fun (v:vector) (w:vector) ->
 if not ( vector_eq_zero w ) then
  begin
   match v with
   | Full_vector x ->
    begin
     match w with
     | Full_vector y ->
      begin
       let d = min ( Array.length x ) ( Array.length y ) in
        for i = 0 to pred d do
         x.(i) <- R.add x.(i) y.(i)
        done ;
      end
     | Sparse_vector y ->
      begin
       let f = function ( i , z ) -> x.(i) <- R.add x.(i) z in
        V.iter f y
      end
    end
   | Sparse_vector x ->
    begin
     match w with
     | Full_vector y ->
      begin
       for i = 0 to pred ( Array.length y ) do
        V.insert_add y.(i) i x
       done
      end
     | Sparse_vector y -> V.in_place_add x y
    end
  end ;;

(** {v vector_add vector1 vector2 v} *)
let rec vector_add = fun (v:vector) (w:vector) ->
 match v with
 | Full_vector x ->
  begin
   match w with
   | Full_vector y ->
    begin
     let d = Array.length x in
      assert ( d = Array.length y ) ;
      let result = array_null d in
       for i = 0 to pred d do
        result.(i) <- R.add x.(i) y.(i)
       done ;
       Full_vector result
    end
   | Sparse_vector y ->
    begin
     let d = Array.length x in
      assert ( d = V.dimension y ) ;
      let result = Array.map R.copy x in
       let f = function ( i , z ) -> result.(i) <- R.add x.(i) z in
        V.iter f y ;
        Full_vector result
    end
  end
 | Sparse_vector x ->
  begin
   match w with
   | Full_vector y -> vector_add w v
   | Sparse_vector y -> Sparse_vector ( V.add x y )
  end ;;


(** {v vector_in_place_sub vector1 vector2 v} The first vector stores the result.

Le premier vecteur accueille le résultat. *)
let vector_in_place_sub = fun (v:vector) (w:vector) ->
 if not ( vector_eq_zero w ) then
  begin
   match v with
   | Full_vector x ->
    begin
     match w with
     | Full_vector y ->
      begin
       let d = min ( Array.length x ) ( Array.length y ) in
        for i = 0 to pred d do
         x.(i) <- R.sub x.(i) y.(i)
        done ;
      end
     | Sparse_vector y ->
      begin
       let f = function ( i , z ) -> x.(i) <- R.sub x.(i) z in
        V.iter f y
      end
    end
   | Sparse_vector x ->
    begin
     match w with
     | Full_vector y ->
      begin
       for i = 0 to pred ( Array.length y ) do
        V.insert_sub y.(i) i x
       done
      end
     | Sparse_vector y -> V.in_place_sub x y
    end
  end ;;

(** {v vector_sub vector1 vector2 v} *)
let vector_sub = fun (v:vector) (w:vector) ->
 match v with
 | Full_vector x ->
  begin
   match w with
   | Full_vector y ->
    begin
     let d = Array.length x in
      assert ( d = Array.length y ) ;
      let result = array_null d in
       for i = 0 to pred d do
        result.(i) <- R.sub x.(i) y.(i)
       done ;
       Full_vector result
    end
   | Sparse_vector y ->
    begin
     let d = Array.length x in
      assert ( d = V.dimension y ) ;
      let result = Array.map R.copy x in
       let f = function ( i , z ) -> result.(i) <- R.sub x.(i) z in
        V.iter f y ;
        Full_vector result
    end
  end
 | Sparse_vector x ->
  begin
   match w with
   | Full_vector y ->
    begin
     let d = Array.length y in
      assert ( d = V.dimension x ) ;
      let result = Array.map R.opp y in
       let f = function ( i , z ) -> result.(i) <- R.add y.(i) z in
        V.iter f x ;
        Full_vector result
    end
   | Sparse_vector y -> Sparse_vector ( V.sub x y )
  end ;;

(** {v vector_eq vector1 vector2 v} *)
let vector_eq = fun (v:vector) (w:vector) ->
 match v with
 | Full_vector x ->
  begin
   match w with
   | Full_vector y -> Util.array_eq R.eq x y
   | Sparse_vector y -> vector_eq_zero ( vector_sub v w )
  end
 | Sparse_vector x ->
  begin
   match w with
   | Full_vector y -> vector_eq_zero ( vector_sub w v )
   | Sparse_vector y -> V.eq x y
  end ;;

(** {v vector_sum vector v} *)
let vector_sum = function (v:vector) ->
 match v with
 | Full_vector w -> Array.fold_left R.add ( R.zero () ) w
 | Sparse_vector w -> V.sum w ;;

(** {v vector_contraction init vector v} *)
let vector_contraction = fun (init:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Array.fold_left R.mult init w
 | Sparse_vector w -> V.contraction init w ;;

(** {v vector_in_place_scal_add scalar vector v} *)
let vector_in_place_scal_add = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Array.iteri ( fun i x -> w.(i) <- R.add y x ) w
 | Sparse_vector w -> V.in_place_scal_add y w ;;

(** {v vector_scal_add scalar vector v} *)
let vector_scal_add = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.map ( R.add y ) w )
 | Sparse_vector w -> Sparse_vector ( V.scal_add y w ) ;;

(** {v vector_in_place_scal_right_sub scalar vector v} *)
let vector_in_place_scal_right_sub = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Array.iteri ( fun i x -> w.(i) <- R.sub y x ) w
 | Sparse_vector w -> V.in_place_scal_right_sub y w ;;

(** {v vector_in_place_scal_left_sub scalar vector v} *)
let vector_in_place_scal_left_sub = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Array.iteri ( fun i x -> w.(i) <- R.sub x y ) w
 | Sparse_vector w -> V.in_place_scal_left_sub y w ;;

(** {v vector_scal_right_sub scalar vector v} *)
let vector_scal_right_sub = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.map ( R.sub y ) w )
 | Sparse_vector w -> Sparse_vector ( V.scal_right_sub y w ) ;;

(** {v vector_scal_left_sub scalar vector v} *)
let vector_scal_left_sub = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.map ( function x -> R.sub x y ) w )
 | Sparse_vector w -> Sparse_vector ( V.scal_left_sub y w ) ;;

(** {v vector_in_place_scal_mult scalar vector v} *)
let vector_in_place_scal_mult = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Array.iteri ( fun i x -> w.(i) <- R.mult y x ) w
 | Sparse_vector w -> V.in_place_scal_mult y w ;;

(** {v vector_scal_mult scalar vector v} *)
let vector_scal_mult = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.map ( R.mult y ) w )
 | Sparse_vector w -> Sparse_vector ( V.scal_mult y w ) ;;

(** {v vector_coeff_prod vector1 vector2 v} *)
let rec vector_coeff_prod = fun (v:vector) (w:vector) ->
 match v with
 | Full_vector x ->
  begin
   match w with
   | Full_vector y ->
    begin
     let d = Array.length x in
      assert ( d = Array.length y ) ;
      let result = array_null d in
       for i = 0 to pred d do
        result.(i) <- R.mult x.(i) y.(i)
       done ;
       Full_vector result
    end
   | Sparse_vector y ->
    begin
     let d = Array.length x in
      assert ( d = V.dimension y ) ;
      let result = V.null d in
       let f = function ( i , z ) -> V.insert_add ( R.mult x.(i) z ) i result in
        V.iter f y ;
        Sparse_vector result
    end
  end
 | Sparse_vector x ->
  begin
   match w with
   | Full_vector y -> vector_coeff_prod w v
   | Sparse_vector y -> Sparse_vector ( V.coeff_prod x y )
  end ;;

(** {v vector_scal_prod vector1 vector2 v} *)
let vector_scal_prod = fun (v:vector) (w:vector) ->
 match v with
 | Full_vector x ->
  begin
   match w with
   | Full_vector y ->
    begin
     let d = min ( Array.length x ) ( Array.length y ) in
      let z = ref ( R.zero () ) in
       for i = 0 to pred d do
        z := R.add !z ( R.mult x.(i) y.(i) )
       done ;
       !z
    end
   | Sparse_vector y -> V.sparse_full_scal_prod y x
  end
 | Sparse_vector x ->
  begin
   match w with
   | Full_vector y -> V.sparse_full_scal_prod x y
   | Sparse_vector y -> V.scal_prod x y
  end ;;

(** {v vector_norm_1 vector v} *)
let vector_norm_1 = function (v:vector) ->
 match v with
 | Full_vector w -> Array.fold_left R.norm_add ( R.norm_zero () ) ( Array.map R.norm w )
 | Sparse_vector w -> V.norm_1 w ;;

(** {v vector_norm_inf vector v} *)
let vector_norm_inf = function (v:vector) ->
 match v with
 | Full_vector w -> Util.array_maximum R.norm_compare ( Array.map R.norm w )
 | Sparse_vector w -> V.norm_inf w ;;

(** {v vector_square_norm_2 vector v} *)
let vector_square_norm_2 = function (v:vector) ->
 match v with
 | Full_vector w ->
  begin
   let f = function x -> R.norm ( R.mult x x ) in
    Array.fold_left R.norm_add ( R.norm_zero () ) ( Array.map f w )
  end
 | Sparse_vector w -> V.square_norm_2 w ;;

(** {v vector_square_sum vector v} *)
let vector_square_sum = function (v:vector) ->
 vector_scal_prod v v ;;

(** {v vector_compare_norm norm vector1 vector2 v} *)
let vector_compare_norm = fun n (v:vector) (w:vector) ->
 R.norm_compare ( n v ) ( n w ) ;;

(** {v vector_compare vector1 vector2 v} *)
let vector_compare = vector_compare_norm vector_norm_inf ;;

(** {v vector_int_mult integer vector v} *)
let vector_int_mult = fun (n:int) (v:vector) ->
 vector_map ( function x -> R.int_mult n x ) v ;;

(** {v vector_int_pow integer vector v} *)
let vector_int_pow = fun (n:int) (v:vector) ->
 vector_map ( function x -> R.int_pow n x ) v ;;

(** {v vector_norm_inject number v} *)
let vector_norm_inject = function (x:R.u) ->
 Full_vector [| R.norm_inject x |] ;;


(** {v vector_of_blocks block_array v} If one of the vectors is full, then so is the result.
If all are sparse, then so is the result.

Si l'un des vecteurs est plein, alors le résultat l'est. Si tous sont creux, alors le résultat l'est. *)
let vector_of_blocks = function (x:vector array) ->
 let lengths = Array.map vector_dimension x in
  let dim = Array.fold_left ( + ) 0 lengths
  and shifts = Array.append [| 0 |] lengths in
   for i = 0 to pred ( Array.length x ) do
    shifts.( succ i ) <- shifts.(i) + lengths.(i)
   done ;
    let g = fun i z -> vector_embed dim shifts.(i) z in
     let xx = Array.mapi g x
     and zz = vector_sparse_null dim in
      Array.fold_left vector_add zz xx ;;




(** {C § } *)
(** 
{1 Tenseurs}
{1 Tensors}
*)
(** {C  } *)




(** The type [tensor] gathers all formats of tensors with coefficients of type [coeff].

Le type [tensor] rassemble tous les formats de tenseurs à coefficients de type [coeff]. *)
type tensor =
 | Sparse_tensor of T.t
 | Vector of vector
 | Full_tensor of tensor array ;;


(** {v tensor_copy tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_copy = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.copy x )
 | Vector x -> Vector ( vector_copy x )
 | Full_tensor x -> Full_tensor ( Array.map tensor_copy x ) ;;


(** {v tensor_resize size tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_resize = fun (n:int) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.resize n x
 | Vector x -> vector_resize n x
 | Full_tensor x -> Array.iter ( tensor_resize n ) x ;;


(** {v thickness tensor v} The thickness represents the predecessor of 
the number of variables or of the degree of the tensor.
This function is not tail recursive.

Cette fonction n'est pas récursive terminale. 
L'épaisseur représente le prédécesseur du nombre de variables ou du degré du tenseur.*)
let rec thickness = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.thickness x
 | Vector x -> 0
 | Full_tensor x ->
  begin
   let t = Array.map thickness x in
    succ ( Util.array_maximum compare t )
  end ;;


(** The three following functions are not sealed.

Les trois fonctions suivantes ne sont pas étanches. *)


(** {v tensor_sparse_demakeup tensor v} *)
let tensor_sparse_demakeup = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> x
 | Vector x -> failwith "Vector instead of a sparse tensor in Mat.tensor_sparse_demakeup."
 | Full_tensor x -> failwith "Full_tensor instead of a sparse tensor in Mat.tensor_sparse_demakeup." ;;

(** {v tensor_vector_demakeup tensor v} *)
let tensor_vector_demakeup = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> failwith "Sparse tensor instead of a vector in Mat.tensor_vector_demakeup."
 | Vector x -> x
 | Full_tensor x -> failwith "Full_tensor instead of a vector in Mat.tensor_vector_demakeup." ;;

(** {v tensor_full_demakeup tensor v} *)
let tensor_full_demakeup = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> failwith "Sparse_tensor instead of a full thick tensor in Mat.tensor_full_demakeup."
 | Vector x -> failwith "Vector instead of a full thick tensor in Mat.tensor_full_demakeup."
 | Full_tensor x -> x ;;


(** {v tensor_full_null dimensions v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_full_null = function (dims:int array) ->
 match Array.length dims with
 | 0 -> Vector ( Full_vector ( Array.make 0 ( R.zero () ) ) )
 | 1 -> Vector ( Full_vector ( array_null dims.(0) ) )
 | _ -> Full_tensor ( Array.map tensor_full_null ( Array.make dims.(0) ( Util.array_tail dims ) ) ) ;;

(** {v tensor_sparse_null dimensions v} *)
let tensor_sparse_null = function (dims:int array) ->
 Sparse_tensor ( T.null dims ) ;;

(** {v tensor_null dimensions v} *)
let tensor_null = tensor_sparse_null ;;

(** {v tensor_zero unit v} *)
let tensor_zero = function () ->
 tensor_sparse_null [| 0 |] ;;

(** {v tensor_eq_zero tensor v} *)
let rec tensor_eq_zero = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.eq_zero x
 | Vector x -> vector_eq_zero x
 | Full_tensor x -> Util.array_eq_zero tensor_eq_zero x ;;


(** {v tensor_dimensions tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_dimensions = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.dimensions x
 | Vector x -> [| vector_dimension x |]
 | Full_tensor x ->
  begin
   let t = Array.map tensor_dimensions x in
    let tt = Util.array_maximum ( Util.lexico_compare compare ) t in
     Array.append [| Array.length x |] tt
  end ;;


(** {v tensor_nihil dimensions v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_nihil = function (v:tensor) ->
 match v with
 | Sparse_tensor x ->
  begin
   let z = T.null ( T.dimensions x ) in
    T.resize ( T.size x ) z ;
    Sparse_tensor ( z )
  end
 | Vector x -> Vector ( vector_nihil x )
 | Full_tensor x -> Full_tensor ( Array.map tensor_nihil x ) ;;

(** {v tensor_filling tensor v} *)
let tensor_filling = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.filling x
 | Vector x -> vector_filling x
 | Full_tensor x -> failwith "Full vector in Mat.tensor_filling." ;;

(** {v tensor_sizes tensor v} *)
let tensor_sizes = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.sizes x
 | Vector x -> failwith "Vector tensor in Mat.tensor_sizes."
 | Full_tensor x -> failwith "Full vector in Mat.tensor_sizes." ;;


(** {v tensor_to_sparse tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_to_sparse = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> v
 | Vector x -> Sparse_tensor ( T.Vector ( vector_sparse_demakeup ( vector_to_sparse x ) ) )
 | Full_tensor x ->
  begin
   let d = tensor_dimensions v in
    let result = T.null d in
     let f = fun i y ->
      begin
       let yy = tensor_sparse_demakeup ( tensor_to_sparse y )
       and g = function ( j , z ) -> T.insert_add z ( Array.append [| i |] j ) result in
        T.iter g yy
      end in
      Array.iteri f x ;
      Sparse_tensor result
  end ;;


(** {v tensor_extract indices tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_extract = fun (i:int array) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.raw_extract i x
 | Vector x -> vector_extract i.(0) x
 | Full_tensor x ->
  begin
   let z = x.( i.(0) )
   and j = Util.array_tail i in
    tensor_extract j z
  end ;;


(** {v sub_tensor_extract level index tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec sub_tensor_extract = fun (level:int) (i:int) (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.sub_tensor_extract level i x )
 | Vector x ->
  begin
   assert ( level = 0 ) ;
   Vector ( Full_vector [| vector_extract i x |] )
  end
 | Full_tensor x ->
  begin
   if level = 0 then
    x.(i)
   else
    Full_tensor ( Array.map ( sub_tensor_extract ( pred level ) i ) x )
  end ;;


(** {v tensor_insert_add coefficient indices tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_insert_add = fun (y:coeff) (i:int array) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.insert_add y i x
 | Vector x -> vector_insert_add y i.(0) x
 | Full_tensor x -> tensor_insert_add y ( Util.array_tail i ) x.(i.(0)) ;;


(** {v tensor_insert_sub coefficient indices tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_insert_sub = fun (y:coeff) (i:int array) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.insert_sub y i x
 | Vector x -> vector_insert_sub y i.(0) x
 | Full_tensor x -> tensor_insert_sub y ( Util.array_tail i ) x.(i.(0)) ;;


(** {v tensor_remove indices tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_remove = fun (i:int array) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.remove i x
 | Vector x -> vector_remove i.(0) x
 | Full_tensor x -> tensor_remove ( Util.array_tail i ) x.(i.(0)) ;;


(** {v sub_tensor_remove level index tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec sub_tensor_remove = fun (level:int) (i:int) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.sub_tensor_remove level i x
 | Vector x ->
  begin
   assert ( level = 0 ) ;
   vector_remove i x
  end
 | Full_tensor x ->
  begin
   if level = 0 then
    begin
     let d = tensor_dimensions v in
      x.(i) <- tensor_sparse_null ( Util.array_tail d )
    end
   else
    Array.iter ( sub_tensor_remove ( pred level ) i ) x
  end ;;


(** {v tensor_replace indices tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_replace = fun (y:coeff) (i:int array) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.replace y i x
 | Vector x -> vector_replace y i.(0) x
 | Full_tensor x -> tensor_replace y ( Util.array_tail i ) x.(i.(0)) ;;


(** {v sub_tensor_replace level index tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec sub_tensor_replace = fun (y:tensor) (level:int) (i:int) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.sub_tensor_replace ( tensor_sparse_demakeup ( tensor_to_sparse y ) ) level i x
 | Vector x -> failwith "Vector argument in Mat.sub_tensor_replace."
 | Full_tensor x ->
  begin
   if level = 0 then
    x.(i) <- y
   else
    Array.iter ( sub_tensor_replace y ( pred level ) i ) x
  end ;;


(** {v tensor_to_full tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_to_full = function (v:tensor) ->
 match v with
 | Full_tensor x -> v
 | Vector x -> Vector ( vector_to_full x )
 | Sparse_tensor x ->
  begin
   let d = T.dimensions x in
    let e = pred ( Array.length d ) in
     let result = ref ( Vector ( Full_vector ( array_null d.(e) ) ) ) in
      for i = pred e downto 0 do
       result := Full_tensor ( Array.map tensor_copy ( Array.make d.(i) !result ) )
      done ;
       let f = fun ( i , y ) ->
        begin
         tensor_insert_add y i !result
        end in
       T.iter f x ;
       !result
  end ;;

(** {v tensor_to_vector tensor v} *)
let tensor_to_vector = function (v:tensor) ->
 match v with
 | Vector x -> v
 | Sparse_tensor x -> Vector ( Sparse_vector ( T.vector_demakeup ( T.tensor_to_vector x ) ) )
 | Full_tensor x -> failwith "Full_tensor in Mat.tensor_to_vector." ;;

(** {v tensor_quality tensor v} *)
let rec tensor_quality = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> "Sparse tensor"
 | Vector x -> "Vector tensor"
 | Full_tensor x -> "Full tensor" ;;


(** {v tensor_to_string tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_to_string = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> "Sparse_tensor " ^ ( T.to_string x )
 | Vector x -> "Vector " ^ ( vector_to_string x )
 | Full_tensor x ->
  begin
   let e = string_of_int ( thickness v ) in
    let beginning = "[" ^ e ^ "|"
    and ending = "|" ^ e ^ "]"
    and separator = "°" ^ e ^ "°" in
     let s = Util.vector_to_string tensor_to_string beginning separator ending x in
      "Full_tensor" ^ e ^ " " ^ s
  end ;;


(** {v tensor_of_string string v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_of_string = function (s:string) ->
 let lst = String.length s
 and index = Str.search_forward ( Str.regexp " " ) s 0 in
  let qualif = String.sub s 0 index
  and rest = String.sub s ( succ index ) ( lst - index - 1 ) in
   match qualif with
   | "Vector" -> Vector ( vector_of_string rest )
   | "Sparse_tensor" -> Sparse_tensor ( T.of_string rest )
   | _ ->
    begin
     match ( String.sub qualif 0 11 ) with
     | "Full_vector" ->
      let e = String.sub qualif 11 ( String.length qualif - 11 ) in
       let beginning = "[" ^ e ^ "|"
       and ending = "|" ^ e ^ "]"
       and separator = "°" ^ e ^ "°" in
        Full_tensor ( Util.vector_of_string tensor_of_string beginning separator ending rest )
     | _ -> failwith "Not a valid string in Mat.tensor_of_string."
    end ;;


(** {v tensor_find coefficient tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_find = fun (y:coeff) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.find y x
 | Vector x -> [| vector_find y x |]
 | Full_tensor x ->
  begin
   let i = ref 0
   and e = thickness v
   and r = Array.length x in
    let index = Array.make ( succ e ) ( -1 ) in
     while !i < r do
      let j = tensor_find y x.(!i) in
       if j.(0) >= 0 then
        begin
         index.(0) <- !i ;
         for k = 1 to e do
          index.(k) <- j.( pred k )
         done ;
         i := r ;
        end
       else
        incr i ;
     done ;
     index
  end ;;


(** {v tensor_find_all coefficient tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_find_all = fun (y:coeff) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.index_list_find_all y x
 | Vector x -> List.rev_map ( Array.make 1 ) ( vector_list_find_all y x )
 | Full_tensor x ->
  begin
   let result = ref []
   and r = Array.length x in
    for i = 0 to pred r do
     let j = tensor_find_all y x.(i) in
      if Util.list_non_empty j then
       begin
        let jj = List.rev_map ( Array.append [| i |] ) j in
         result := List.rev_append jj !result
       end ;
    done ;
    !result
  end ;;


(** {v tensor_filter predicate tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_filter = fun (p:index array -> bool) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.filter p x
 | Vector x ->
  begin
   let pp = function i -> p [| i |]
   and f = function ( i , y ) -> ( [| i |] , y )  in
    let result = vector_filter pp x in
     List.rev_map f result
  end
 | Full_tensor x ->
  begin
   let result = ref []
   and r = Array.length x in
    for i = 0 to pred r do
     let pp = function k -> p ( Array.append [| i |] k )
     and f = function ( k , y ) -> ( Array.append [| i |] k , y ) in
      let j = tensor_filter pp x.(i) in
       if Util.list_non_empty j then
        begin
         let jj = List.rev_map f j in
          result := List.rev_append jj !result
        end ;
    done ;
    !result
  end ;;


(** {v tensor_opp tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_opp = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.opp x )
 | Vector x -> Vector ( vector_opp x )
 | Full_tensor x -> Full_tensor ( Array.map tensor_opp x ) ;;


(** {v tensor_iter function tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_iter = fun f (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.iter f x
 | Vector x ->
  begin
   let g = function ( i , y ) -> f ( [| i |] , y ) in
    vector_iter g x
  end
 | Full_tensor x -> Array.iter ( tensor_iter f ) x ;;


(** {v tensor_fold function tensor init v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_fold = fun f (v:tensor) init ->
 match v with
 | Sparse_tensor x -> T.fold f x init
 | Vector x ->
  begin
   let g = fun ( i , y ) z -> f ( [| i |] , y ) z in
    vector_fold g x init
  end
 | Full_tensor x ->
  begin
   let g = fun z y -> tensor_fold f y z in
    Array.fold_left g init x
  end ;;


(** {v tensor_in_place_map function tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_in_place_map = fun f (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.in_place_map f x
 | Vector x -> vector_in_place_map f x
 | Full_tensor x -> Array.iter ( tensor_in_place_map f ) x ;;


(** {v tensor_in_place_mapi function tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_in_place_mapi = fun f (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.in_place_mapi f x
 | Vector x ->
  begin
   let g = fun i y -> f [| i |] y in
    vector_in_place_mapi g x
  end
 | Full_tensor x ->
  begin
   let g = fun i y ->
    begin
     let ff = fun j z -> f ( Array.append [| i |] j ) z in
      tensor_in_place_mapi ff y
    end in
    Array.iteri g x
  end ;;


(** {v tensor_map function tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_map = fun f (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.map f x )
 | Vector x -> Vector ( vector_map f x )
 | Full_tensor x -> Full_tensor ( Array.map ( tensor_map f ) x ) ;;


(** {v tensor_mapi function tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_mapi = fun f (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.mapi f x )
 | Vector x ->
  begin
   let g = fun i y -> f [| i |] y in
     Vector ( vector_mapi g x )
  end
 | Full_tensor x -> Full_tensor ( Array.map ( tensor_mapi f ) x ) ;;


(** {v tensor_minimum tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_minimum = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.min x
 | Vector x -> vector_minimum x
 | Full_tensor x -> Util.array_minimum R.compare ( Array.map tensor_minimum x ) ;;


(** {v tensor_maximum tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_maximum = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.max x
 | Vector x -> vector_maximum x
 | Full_tensor x -> Util.array_maximum R.compare ( Array.map tensor_maximum x ) ;;


(** {v tensor_in_place_add function scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_in_place_add = fun (v:tensor) (w:tensor) ->
 match ( v , w ) with
 | ( Sparse_tensor x , Sparse_tensor y ) -> T.in_place_add x y
 | ( Sparse_tensor x , _ ) -> tensor_in_place_add v ( tensor_to_sparse w )
 | ( Vector x , Vector y ) -> vector_in_place_add x y
 | ( Vector x , Sparse_tensor y ) -> tensor_in_place_add v ( tensor_to_vector w )
 | ( Vector x , Full_tensor y ) -> failwith "Arguments incompatibility in Mat.tensor_in_place_add."
 | ( Full_tensor x , Vector y ) -> failwith "Arguments incompatibility in Mat.tensor_in_place_add."
 | ( Full_tensor x , Sparse_tensor y ) -> tensor_in_place_add v ( tensor_to_full w )
 | ( Full_tensor x , Full_tensor y ) ->
  begin
   let f = fun i z -> tensor_in_place_add x.(i) z in
    Array.iteri f y
  end ;;


(** {v tensor_add function scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_add = fun (v:tensor) (w:tensor) ->
 match ( v , w ) with
 | ( Sparse_tensor x , Sparse_tensor y ) -> Sparse_tensor ( T.add x y )
 | ( Sparse_tensor x , Full_tensor y ) -> tensor_add ( tensor_to_full v ) w
 | ( Sparse_tensor x , Vector y ) -> tensor_add ( tensor_to_vector v ) w
 | ( Vector x , Vector y ) -> Vector ( vector_add x y )
 | ( Vector x , Sparse_tensor y ) -> tensor_add w v
 | ( Vector x , Full_tensor y ) -> failwith "Arguments incompatibility in Mat.tensor_add."
 | ( Full_tensor x , Full_tensor y ) ->
  begin
   let f = fun i z -> tensor_add x.(i) z in
    Full_tensor ( Array.mapi f y )
  end
 | ( Full_tensor x , Sparse_tensor y ) -> tensor_add v ( tensor_to_full w )
 | ( Full_tensor x , Vector y ) -> failwith "Arguments incompatibility in Mat.tensor_add." ;;


(** {v tensor_in_place_sub function scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_in_place_sub = fun (v:tensor) (w:tensor) ->
 match ( v , w ) with
 | ( Sparse_tensor x , Sparse_tensor y ) -> T.in_place_sub x y
 | ( Sparse_tensor x , _ ) -> tensor_in_place_sub v ( tensor_to_sparse w )
 | ( Vector x , Vector y ) -> vector_in_place_sub x y
 | ( Vector x , Sparse_tensor y ) -> tensor_in_place_sub v ( tensor_to_vector w )
 | ( Vector x , Full_tensor y ) -> failwith "Arguments incompatibility in Mat.tensor_in_place_sub."
 | ( Full_tensor x , Vector y ) -> failwith "Arguments incompatibility in Mat.tensor_in_place_sub."
 | ( Full_tensor x , Sparse_tensor y ) -> tensor_in_place_sub v ( tensor_to_full w )
 | ( Full_tensor x , Full_tensor y ) ->
  begin
   let f = fun i z -> tensor_in_place_sub x.(i) z in
    Array.iteri f y
  end ;;


(** {v tensor_sub function scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_sub = fun (v:tensor) (w:tensor) ->
 match ( v , w ) with
 | ( Sparse_tensor x , Sparse_tensor y ) -> Sparse_tensor ( T.sub x y )
 | ( Sparse_tensor x , Full_tensor y ) -> tensor_sub ( tensor_to_full v ) w
 | ( Sparse_tensor x , Vector y ) -> tensor_sub ( tensor_to_vector v ) w
 | ( Vector x , Vector y ) -> Vector ( vector_sub x y )
 | ( Vector x , Sparse_tensor y ) -> tensor_sub v ( tensor_to_vector w )
 | ( Vector x , Full_tensor y ) -> failwith "Arguments incompatibility in Mat.tensor_sub."
 | ( Full_tensor x , Full_tensor y ) ->
  begin
   let f = fun i z -> tensor_sub x.(i) z in
    Full_tensor ( Array.mapi f y )
  end
 | ( Full_tensor x , Sparse_tensor y ) -> tensor_sub v ( tensor_to_full w )
 | ( Full_tensor x , Vector y ) -> failwith "Arguments incompatibility in Mat.tensor_sub." ;;

(** {v tensor_eq tensor1 tensor2 v} *)
let tensor_eq = fun (v:tensor) (w:tensor) ->
 tensor_eq_zero ( tensor_sub v w ) ;;


(** {v tensor_sum tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_sum = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.sum x
 | Vector x -> vector_sum x
 | Full_tensor x -> Array.fold_left R.add ( R.zero () ) ( Array.map tensor_sum x ) ;;


(** {v tensor_contraction init tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_contraction = fun init (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.contraction init x
 | Vector x -> vector_contraction init x
 | Full_tensor x ->
  begin
   let accu = ref init in
    let f = function y -> accu := tensor_contraction !accu y in
     Array.iter f x ;
     !accu
  end ;;


(** {v tensor_in_place_scal_add scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_in_place_scal_add = fun s (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.in_place_scal_add s x
 | Vector x -> vector_in_place_scal_add s x
 | Full_tensor x -> Array.iter ( tensor_in_place_scal_add s ) x ;;


(** {v tensor_scal_add scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_scal_add = fun s (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.scal_add s x )
 | Vector x -> Vector ( vector_scal_add s x )
 | Full_tensor x -> Full_tensor ( Array.map ( tensor_scal_add s ) x ) ;;


(** {v tensor_in_place_scal_mult scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_in_place_scal_mult = fun s (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.in_place_scal_mult s x
 | Vector x -> vector_in_place_scal_mult s x
 | Full_tensor x -> Array.iter ( tensor_in_place_scal_mult s ) x ;;


(** {v tensor_scal_mult scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_scal_mult = fun s (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.scal_mult s x )
 | Vector x -> Vector ( vector_scal_mult s x )
 | Full_tensor x -> Full_tensor ( Array.map ( tensor_scal_mult s ) x ) ;;


(** {v tensor_in_place_scal_left_sub scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_in_place_scal_left_sub = fun s (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.in_place_scal_left_sub s x
 | Vector x -> vector_in_place_scal_left_sub s x
 | Full_tensor x -> Array.iter ( tensor_in_place_scal_left_sub s ) x ;;


(** {v tensor_scal_left_sub scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_scal_left_sub = fun s (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.scal_left_sub s x )
 | Vector x -> Vector ( vector_scal_left_sub s x )
 | Full_tensor x -> Full_tensor ( Array.map ( tensor_scal_left_sub s ) x ) ;;


(** {v tensor_in_place_scal_right_sub scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_in_place_scal_right_sub = fun s (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.in_place_scal_right_sub s x
 | Vector x -> vector_in_place_scal_right_sub s x
 | Full_tensor x -> Array.iter ( tensor_in_place_scal_right_sub s ) x ;;


(** {v tensor_scal_right_sub scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_scal_right_sub = fun s (v:tensor) ->
 match v with
 | Sparse_tensor x -> Sparse_tensor ( T.scal_right_sub s x )
 | Vector x -> Vector ( vector_scal_right_sub s x )
 | Full_tensor x -> Full_tensor ( Array.map ( tensor_scal_right_sub s ) x ) ;;


(** {v tensor_coeff_prod function scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_coeff_prod = fun (v:tensor) (w:tensor) ->
 match ( v , w ) with
 | ( Sparse_tensor x , Sparse_tensor y ) -> Sparse_tensor ( T.coeff_prod x y )
 | ( Sparse_tensor x , Full_tensor y ) -> tensor_coeff_prod ( tensor_to_full v ) w
 | ( Sparse_tensor x , Vector y ) -> tensor_coeff_prod ( tensor_to_vector v ) w
 | ( Vector x , Vector y ) -> Vector ( vector_coeff_prod x y )
 | ( Vector x , Sparse_tensor y ) -> tensor_coeff_prod w v
 | ( Vector x , Full_tensor y ) -> failwith "Arguments incompatibility in Mat.tensor_coeff_prod."
 | ( Full_tensor x , Full_tensor y ) ->
  begin
   let f = fun i z -> tensor_coeff_prod x.(i) z in
    Full_tensor ( Array.mapi f y )
  end
 | ( Full_tensor x , Sparse_tensor y ) -> tensor_coeff_prod v ( tensor_to_full w )
 | ( Full_tensor x , Vector y ) -> failwith "Arguments incompatibility in Mat.tensor_coeff_prod." ;;


(** {v tensor_scal_prod function scalar tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_scal_prod = fun (v:tensor) (w:tensor) ->
 match ( v , w ) with
 | ( Sparse_tensor x , Sparse_tensor y ) -> T.scal_prod x y
 | ( Sparse_tensor x , _ ) -> tensor_scal_prod v ( tensor_to_sparse w )
 | ( Vector x , Vector y ) -> vector_scal_prod x y
 | ( Vector x , Sparse_tensor y ) -> tensor_scal_prod v ( tensor_to_vector w )
 | ( Vector x , Full_tensor y ) -> failwith "Arguments incompatibility in Mat.tensor_scal_prod."
 | ( Full_tensor x , Vector y ) -> failwith "Arguments incompatibility in Mat.tensor_scal_prod."
 | ( Full_tensor x , Sparse_tensor y ) -> tensor_scal_prod v ( tensor_to_full w )
 | ( Full_tensor x , Full_tensor y ) ->
  begin
   let f = fun i z -> tensor_scal_prod x.(i) z in
    Array.fold_left R.add ( R.zero () ) ( Array.mapi f y )
  end ;;


(** {v tensor_norm_1 tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_norm_1 = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.norm_1 x
 | Vector x -> vector_norm_1 x
 | Full_tensor x -> Array.fold_left R.norm_add ( R.norm_zero () ) ( Array.map tensor_norm_1 x ) ;;


(** {v tensor_norm_inf tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_norm_inf = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.norm_inf x
 | Vector x -> vector_norm_inf x
 | Full_tensor x -> Array.fold_left R.norm_add ( R.norm_zero () ) ( Array.map tensor_norm_inf x ) ;;


(** {v tensor_square_norm_2 tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_square_norm_2 = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.square_norm_2 x
 | Vector x -> vector_square_norm_2 x
 | Full_tensor x -> Array.fold_left R.norm_add ( R.norm_zero () ) ( Array.map tensor_square_norm_2 x ) ;;


(** {v tensor_square_sum tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_square_sum = function (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.square_sum x
 | Vector x -> vector_square_sum x
 | Full_tensor x -> Array.fold_left R.add ( R.zero () ) ( Array.map tensor_square_sum x ) ;;

(** {v tensor_compare_norm norm function scalar tensor v} *)
let tensor_compare_norm = fun n (v:tensor) (w:tensor) ->
 R.norm_compare ( n v ) ( n w ) ;;


(** {v tensor_exchange level index1 index2 tensor v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_exchange = fun (level:int) (i:int) (j:int) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.exchange level i j x
 | Vector x ->
  begin
   if level <> 0 then failwith "Bad level in Mat.tensor_exchange." ;
   vector_exchange i j x
  end
 | Full_tensor x ->
  begin
   if level == 0 then
    begin
     let y = Array.map tensor_copy x in
      let z = y.(i) in
       y.(i) <- y.(j) ;
       y.(j) <- z ;
    end
   else
    Array.iter ( tensor_exchange ( pred level ) i j ) x
  end ;;


(** {v tensor_level_exchange level1 level2 tensor v} *)
let tensor_level_exchange = fun (level1:int) (level2:int) (v:tensor) ->
 match v with
 | Sparse_tensor x -> T.level_exchange level1 level2 x
 | Vector x -> if ( level1 <> 0 ) || ( level2 <> 0 ) then failwith "Bad levels in Mat.tensor_level_exchange."
 | Full_tensor x -> failwith "Full tensor in Mat.tensor_level_exchange." ;;


(** {v tensor_mult tensor1 tensor2 v} This function is not tail recursive.

Cette fonction n'est pas récursive terminale. *)
let rec tensor_mult = fun (v:tensor) (w:tensor) ->
 match ( v , w ) with
 | ( Sparse_tensor x , Sparse_tensor y ) -> Sparse_tensor ( T.mult x y )
 | ( Sparse_tensor x , _ ) -> tensor_mult v ( tensor_to_sparse w )
 | ( Vector x , _ ) ->
  begin
   let d = tensor_dimensions w
   and r = vector_dimension x in
    let dim = Array.append [| r |] d in
     let result = tensor_null dim in
      let f = function ( i , x ) -> sub_tensor_replace ( tensor_scal_mult x w ) 0 i result in
       vector_iter f x ;
       result
  end
 | ( Full_tensor x , _ ) ->
  begin
   let f = function z -> tensor_mult z w in
    Full_tensor ( Array.map f x )
  end ;;




(** {C § } *)
(** 
{1 Matrices}
*)
(** {C  } *)




(** The [matrix] type gathers all formats of matrices with coefficients of type [coeff].

Le type [matrix] rassemble tous les formats de matrices à coefficients de type [coeff].*)
type matrix =
 | Full_matrix of coeff array array
 | Sparse_matrix of M.t ;;

(** {v matrix_dimensions matrix v} *)
let matrix_dimensions = function (m:matrix) ->
 match m with
 | Full_matrix w -> [| Array.length w ; Array.length w.(0) |]
 | Sparse_matrix w -> M.dimensions w ;;

(** {v matrix_filling matrix v} *)
let matrix_filling = function (m:matrix) ->
 match m with
 | Full_matrix w -> failwith "Full matrix in Mat.matrix_filling."
 | Sparse_matrix w -> M.filling w ;;

(** {v matrix_detailed_filling matrix v} *)
let matrix_detailed_filling = function (m:matrix) ->
 match m with
 | Full_matrix w -> failwith "Full matrix in Mat.matrix_detailed_filling."
 | Sparse_matrix w -> M.detailed_filling w ;;

(** {v matrix_sizes matrix v} *)
let matrix_sizes = function (m:matrix) ->
 match m with
 | Full_matrix w -> failwith "Full matrix in Mat.matrix_sizes."
 | Sparse_matrix w -> M.sizes w ;;

(** {v matrix_quality matrix v} *)
let matrix_quality = function (m:matrix) ->
 match m with
 | Full_matrix w -> "Full matrix"
 | Sparse_matrix w -> "Sparse matrix" ;;

(** {v matrix_sparse_null dimensions v} *)
let matrix_sparse_null = function (dims:int array) ->
 Sparse_matrix ( M.null dims ) ;;

(** {v array_array_null dimensions v} *)
let array_array_null = function (dims:int array) ->
 let m = Array.make_matrix dims.(0) dims.(1) () in
  Array.map ( Array.map R.zero ) m ;;

(** {v matrix_full_null dimensions v} *)
let matrix_full_null = function (dims:int array) ->
 Full_matrix ( array_array_null dims ) ;;

(** {v matrix_zero unit v} *)
let matrix_zero = function () ->
 matrix_sparse_null ( Array.make 2 1 ) ;;

(** {v matrix_nihil matrix v} *)
let matrix_nihil = function (m:matrix) ->
 let d = matrix_dimensions m in
  match m with
  | Full_matrix w -> matrix_full_null d
  | Sparse_matrix w -> 
   begin
    let z = M.T.null d in
     M.T.resize ( M.size w ) z ;
     Sparse_matrix ( M.Sparse_tensor_matrix z )
   end ;;

(** {v scal_matrix scalar dimension v} *)
let scal_matrix = fun (x:coeff) (dim:int) ->
 let dims = Array.make 2 dim in
  let t = T.null dims in
   Sparse_matrix ( M.Diff_to_scal_matrix ( x , t ) ) ;;

(** {v matrix_copy matrix v} *)
let matrix_copy = function (m:matrix) ->
 match m with
 | Full_matrix w -> Full_matrix ( Array.map ( Array.map R.copy ) w )
 | Sparse_matrix w -> Sparse_matrix ( M.copy w ) ;;

(** {v matrix_resize size matrix v} *)
let matrix_resize = fun (n:int) (m:matrix) ->
 match m with
 | Full_matrix w -> ()
 | Sparse_matrix w -> M.resize n w ;;

(** {v matrix_cleanup matrix v} *)
let matrix_cleanup = function (m:matrix) ->
 match m with
 | Full_matrix w -> ()
 | Sparse_matrix w -> M.cleanup w ;;

(** {v matrix_full_demakeup matrix v} *)
let matrix_full_demakeup = function (m:matrix) ->
 match m with
 | Full_matrix w -> w
 | Sparse_matrix w -> failwith "Not a full matrix in Mat.matrix_full_demakeup" ;;

(** {v matrix_sparse_demakeup matrix v} *)
let matrix_sparse_demakeup = function (m:matrix) ->
 match m with
 | Full_matrix w -> failwith "Not a sparse matrix in Mat.matrix_sparse_demakeup"
 | Sparse_matrix w -> w ;;

(** {v matrix_to_string matrix v} *)
let rec matrix_to_string = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let g = Util.bare_vector_to_string R.to_string in
    let s = ref ( "Full_matrix {| " ^ ( g w.(0) ) )
    and f = fun x y -> ( x ^ " $ " ^ ( g y ) ) in
     s := Array.fold_left f !s ( Util.array_tail w ) ;
     !s ^ " |}"
  end
 | Sparse_matrix w -> "Sparse_matrix " ^ ( M.to_string w ) ;;

(** {v matrix_of_string string v} *)
let matrix_of_string = function (s:string) ->
 let lst = String.length s
 and index = Str.search_forward ( Str.regexp_string " " ) s 0 in
  let qualif = String.sub s 0 index
  and rest = String.sub s ( succ index ) ( lst - index - 1 ) in
   match qualif with
   | "Full_matrix" ->
    begin
     let a = Util.vector_of_string ( Util.bare_vector_of_string R.of_string ) "{| " " $ " " |}" rest in
      Full_matrix a
    end
   | "Sparse_matrix" -> Sparse_matrix ( M.of_string rest )
   | _ -> failwith "Not a valid string in Mat.matrix_of_string." ;;

(** {v matrix_print matrix v} *)
let matrix_print = function (m:matrix) ->
 print_string ( matrix_to_string m ) ;;


(** {v matrix_to_sparse hash_size threshold matrix v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let matrix_to_sparse = fun (hash_size:int) (threshold:float) (m:matrix) ->
 match m with
 | Full_matrix w -> Sparse_matrix ( M.to_sparse hash_size threshold w )
 | Sparse_matrix w -> m ;;


(** {v matrix_auto_to_sparse threshold matrix v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let matrix_auto_to_sparse = fun (threshold:float) (m:matrix) ->
 match m with
 | Full_matrix w -> Sparse_matrix ( M.auto_to_sparse threshold w )
 | Sparse_matrix w -> m ;;


(** {v matrix_to_full matrix v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let matrix_to_full = function (m:matrix) ->
 match m with
 | Full_matrix w -> m
 | Sparse_matrix w -> Full_matrix ( M.to_full w ) ;;

(** {v matrix_description_eq_zero matrix v} *)
let matrix_description_eq_zero = function (m:matrix) ->
 match m with
 | Full_matrix w -> Util.array_eq_zero ( Util.array_eq_zero R.eq_zero ) w
 | Sparse_matrix w -> M.description_eq_zero w ;;

(** {v matrix_eq_zero matrix v} *)
let matrix_eq_zero = function (m:matrix) ->
 match m with
 | Full_matrix w -> Util.array_eq_zero ( Util.array_eq_zero R.eq_zero ) w
 | Sparse_matrix w -> M.eq_zero w ;;

(** {v matrix_description_eq matrix1 matrix2 v} *)
let rec matrix_description_eq = fun (m:matrix) (p:matrix) ->
 match m with
 | Full_matrix x ->
  begin
   match p with
   | Full_matrix y -> Util.array_eq ( Util.array_eq R.eq ) x y
   | Sparse_matrix y -> matrix_description_eq m ( matrix_to_full p )
  end
 | Sparse_matrix x ->
  begin
   match p with
   | Full_matrix y -> matrix_description_eq ( matrix_to_full m ) p
   | Sparse_matrix y -> M.description_eq x y
  end ;;

(** {v matrix_eq matrix1 matrix2 v} *)
let rec matrix_eq = fun (m:matrix) (p:matrix) ->
 match m with
 | Full_matrix x ->
  begin
   match p with
   | Full_matrix y -> Util.array_eq ( Util.array_eq R.eq ) x y
   | Sparse_matrix y -> matrix_eq m ( matrix_to_full p )
  end
 | Sparse_matrix x ->
  begin
   match p with
   | Full_matrix y -> matrix_eq ( matrix_to_full m ) p
   | Sparse_matrix y -> M.eq x y
  end ;;

(** {v matrix_row_extract index matrix v} *)
let matrix_row_extract = fun (i:int) (m:matrix) ->
 match m with
 | Full_matrix w -> Full_vector ( Array.map R.copy w.(i) ) 
 | Sparse_matrix w -> Sparse_vector ( M.row_extract i w ) ;;

(** {v matrix_column_extract index matrix v} *)
let matrix_column_extract = fun (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w in
    let c = array_null r in
     for j = 0 to pred r do
      c.(j) <- R.copy w.(j).(i)
     done ;
     Full_vector c
  end
 | Sparse_matrix w -> Sparse_vector ( M.column_extract i w ) ;;

(** {v matrix_extract row_index column_index matrix v} *)
let matrix_extract = fun (i:int) (j:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
    R.copy w.(i).(j)
  end
 | Sparse_matrix w -> M.extract i j w ;;

(** {v matrix_row_remove index matrix v} *)
let matrix_row_remove = fun (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let row = w.(i) in
    for j = 0 to pred ( Array.length row ) do
     row.(j) <- R.zero ()
    done
  end
 | Sparse_matrix w -> M.row_remove i w ;;

(** {v matrix_column_remove index matrix v} *)
let matrix_column_remove = fun (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w in
    for j = 0 to pred r do
     w.(j).(i) <- R.zero ()
    done ;
  end
 | Sparse_matrix w -> M.column_remove i w ;;

(** {v matrix_remove row_index column_index matrix v} *)
let matrix_remove = fun (i:int) (j:int) (m:matrix) ->
 match m with
 | Full_matrix w -> w.(i).(j) <- R.zero ()
 | Sparse_matrix w -> M.remove i j w ;;

(** {v matrix_row_replace vector index matrix v} *)
let matrix_row_replace = fun (x:vector) (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let row = w.(i) in
    for j = 0 to pred ( Array.length row ) do
     row.(j) <- vector_extract j x
    done
  end
 | Sparse_matrix w -> M.row_replace ( vector_sparse_demakeup ( vector_to_sparse x ) ) i w ;;

(** {v matrix_column_replace vector index matrix v} *)
let matrix_column_replace = fun (x:vector) (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w in
    for j = 0 to pred r do
     w.(j).(i) <- vector_extract j x
    done ;
  end
 | Sparse_matrix w -> M.column_replace ( vector_sparse_demakeup ( vector_to_sparse x ) ) i w ;;

(** {v matrix_replace coefficient row_index column_index matrix v} *)
let matrix_replace = fun (x:coeff) (i:int) (j:int) (m:matrix) ->
 match m with
 | Full_matrix w -> w.(i).(j) <- x
 | Sparse_matrix w -> M.replace x i j w ;;

(** {v matrix_row_insert_add vector index matrix v} *)
let matrix_row_insert_add = fun (x:vector) (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let row = w.(i) in
    for j = 0 to pred ( Array.length row ) do
     row.(j) <- R.add row.(j) ( vector_extract j x )
    done
  end
 | Sparse_matrix w -> M.row_insert_add ( vector_sparse_demakeup ( vector_to_sparse x ) ) i w ;;

(** {v matrix_column_insert_add vector index matrix v} *)
let matrix_column_insert_add = fun (x:vector) (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w in
    for j = 0 to pred r do
     w.(j).(i) <- R.add w.(j).(i) ( vector_extract j x )
    done ;
  end
 | Sparse_matrix w -> M.column_insert_add ( vector_sparse_demakeup ( vector_to_sparse x ) ) i w ;;

(** {v matrix_insert_add coefficient row_index column_index matrix v} *)
let matrix_insert_add = fun (x:coeff) (i:int) (j:int) (m:matrix) ->
 match m with
 | Full_matrix w -> w.(i).(j) <- R.add w.(i).(j) x
 | Sparse_matrix w -> M.insert_add x i j w ;;

(** {v matrix_row_insert_sub vector index matrix v} *)
let matrix_row_insert_sub = fun (x:vector) (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let row = w.(i) in
    for j = 0 to pred ( Array.length row ) do
     row.(j) <- R.sub row.(j) ( vector_extract j x )
    done
  end
 | Sparse_matrix w -> M.row_insert_sub ( vector_sparse_demakeup ( vector_to_sparse x ) ) i w ;;

(** {v matrix_column_insert_sub vector index matrix v} *)
let matrix_column_insert_sub = fun (x:vector) (i:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w in
    for j = 0 to pred r do
     w.(j).(i) <- R.sub w.(j).(i) ( vector_extract j x )
    done ;
  end
 | Sparse_matrix w -> M.column_insert_sub ( vector_sparse_demakeup ( vector_to_sparse x ) ) i w ;;

(** {v matrix_insert_sub coefficient row_index column_index matrix v} *)
let matrix_insert_sub = fun (x:coeff) (i:int) (j:int) (m:matrix) ->
 match m with
 | Full_matrix w -> w.(i).(j) <- R.sub w.(i).(j) x
 | Sparse_matrix w -> M.insert_sub x i j w ;;

(** {v matrix_full_diag_extract matrix v} *)
let matrix_full_diag_extract = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let d = min r c in
     let ww = array_null d in
      for i = 0 to pred d do
       ww.(i) <- R.copy w.(i).(i)
      done ;
      Full_vector ww
  end
 | Sparse_matrix w -> Full_vector ( M.full_diag_extract w ) ;;

(** {v matrix_sparse_diag_extract matrix v} *)
let matrix_sparse_diag_extract = function (m:matrix) ->
 match m with
 | Full_matrix w -> matrix_full_diag_extract m
 | Sparse_matrix w -> Sparse_vector ( M.sparse_diag_extract w ) ;;

(** {v matrix_diag_isolate matrix v} *)
let matrix_diag_isolate = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let d = min r c
    and ww = array_null r in
     for i = 0 to pred d do
      ww.(i) <- R.copy w.(i).(i)
     done ;
     Sparse_matrix ( M.Diff_to_diag_matrix ( ww , M.T.null [| r ; c |] ) )
  end
 | Sparse_matrix w -> Sparse_matrix ( M.diag_isolate w ) ;;

(** {v matrix_out_diag_isolate matrix v} *)
let matrix_out_diag_isolate = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let ww = array_array_null [| r ; c |]
    and cc = pred c in
     for i = 0 to pred r do
      let row_in = w.(i)
      and row_out = ww.(i) in
       for j = 0 to pred i do
        row_out.(j) <- R.copy row_in.(j)
       done ;
       for j = succ i to cc do
        row_out.(j) <- R.copy row_in.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.out_diag_isolate w ) ;;

(** {v matrix_upper_diag_isolate matrix v} *)
let matrix_upper_diag_isolate = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let ww = array_array_null [| r ; c |]
    and cc = pred c in
     for i = 0 to pred r do
      let row_in = w.(i)
      and row_out = ww.(i) in
       for j = succ i to cc do
        row_out.(j) <- R.copy row_in.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.upper_diag_isolate w ) ;;

(** {v matrix_lower_diag_isolate matrix v} *)
let matrix_lower_diag_isolate = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let ww = array_array_null [| r ; c |] in
     for i = 0 to pred r do
      let row_in = w.(i)
      and row_out = ww.(i) in
       for j = 0 to pred i do
        row_out.(j) <- R.copy row_in.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.lower_diag_isolate w ) ;;

(** {v vector_to_line_matrix vector v} *)
let vector_to_line_matrix = function (v:vector) ->
 match v with
 | Sparse_vector x -> Sparse_matrix ( M.sparse_vector_to_line_matrix x )
 | Full_vector x -> Sparse_matrix ( M.sparse_vector_to_line_matrix ( V.auto_to_sparse x ) ) ;;

(** {v vector_to_square_matrix vector v} *)
let vector_to_square_matrix = function (v:vector) ->
 match v with
 | Sparse_vector x -> Sparse_matrix ( M.sparse_vector_to_square_matrix x )
 | Full_vector x -> Sparse_matrix ( M.full_vector_to_square_matrix x ) ;;

(** {v matrix_in_place_transpose matrix v} *)
let matrix_in_place_transpose = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0)
   and f = function x -> Vector ( Full_vector x ) in
    if r <> c then failwith "Not a square full matrix in Mat.matrix_in_place_transpose." ;
    let ww = Full_tensor ( Array.map f w ) in
     tensor_level_exchange 0 1 ww ;
     for i = 0 to pred r do
      w.(i) <- vector_full_demakeup ( vector_to_full ( tensor_vector_demakeup ( sub_tensor_extract 0 i ww ) ) )
     done
  end
 | Sparse_matrix w -> M.in_place_transpose w ;;

(** {v matrix_transpose matrix v} *)
let matrix_transpose = function (m:matrix) ->
 match m with
 | Full_matrix w -> Full_matrix ( Util.transpose w )
 | Sparse_matrix w -> Sparse_matrix ( M.transpose w ) ;;

(** {v vector_to_column_matrix vector v} *)
let vector_to_column_matrix = function (v:vector) ->
 match v with
 | Sparse_vector x -> Sparse_matrix ( M.sparse_vector_to_column_matrix x )
 | Full_vector x -> Sparse_matrix ( M.sparse_vector_to_column_matrix ( V.auto_to_sparse x ) ) ;;

(** {v vector_to_diag vector v} *)
let vector_to_diag = function (v:vector) ->
 let w = vector_full_demakeup ( vector_to_full v ) in
  let dim = Array.length w in
   let z = M.T.null ( Array.make 2 dim ) in
    Sparse_matrix ( M.Diff_to_diag_matrix ( w , z ) ) ;;

(** {v matrix_find coefficient matrix v} *)
let matrix_find = fun (x:coeff) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let i = ref 0
   and r = Array.length w
   and row = ref ( -1 )
   and column = ref ( -1 ) in
    while ( !row < 0 ) && ( !i < r ) do
     column := Util.vector_find_first R.eq x w.(!i) ;
     if !column < 0 then
      incr i
     else
      row := !i
    done ;
    [| !row ; !column |]
  end
 | Sparse_matrix w -> M.find x w ;;

(** {v masked_hor_band beginning ending matrix v} *)
let masked_hor_band = fun (beginning:int) (ending:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let cc = pred c
    and ww = array_array_null [| r ; c |] in
     for i = beginning to ending do
      let row_output = ww.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- R.copy row_input.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.masked_hor_band beginning ending w ) ;;

(** {v masked_vert_band beginning ending matrix v} *)
let masked_vert_band = fun (beginning:int) (ending:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let rr = pred r
    and ww = array_array_null [| r ; c |] in
     for i = 0 to rr do
      let row_output = ww.(i)
      and row_input = w.(i) in
       for j = beginning to ending do
        row_output.(j) <- R.copy row_input.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.masked_vert_band beginning ending w ) ;;

(** {v masked_head vert_ending hor_ending matrix v} *)
let rec masked_head = fun (vert_ending:int) (hor_ending:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let ww = array_array_null [| r ; c |] in
     for i = 0 to vert_ending do
      let row_output = ww.(i)
      and row_input = w.(i) in
       for j = 0 to hor_ending do
        row_output.(j) <- R.copy row_input.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.masked_head vert_ending hor_ending w ) ;;

(** {v masked_tail beginning ending matrix v} *)
let masked_tail = fun (vert_beginning:int) (hor_beginning:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let rr = pred r
    and cc = pred c
    and ww = array_array_null [| r ; c |] in
     for i = vert_beginning to rr do
      let row_output = ww.(i)
      and row_input = w.(i) in
       for j = hor_beginning to cc do
        row_output.(j) <- R.copy row_input.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.masked_tail vert_beginning hor_beginning w ) ;;

(** {v masked_sample v} *)
let rec masked_sample = fun (vert_beginning:int) (vert_ending:int) (hor_beginning:int) (hor_ending:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let ww = array_array_null [| r ; c |] in
     for i = vert_beginning to vert_ending do
      let row_output = ww.(i)
      and row_input = w.(i) in
       for j = hor_beginning to hor_ending do
        row_output.(j) <- R.copy row_input.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.masked_sample vert_beginning vert_ending hor_beginning hor_ending w ) ;;

(** {v hor_band beginning ending matrix v} *)
let hor_band = fun (beginning:int) (ending:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let c = Array.length w.(0) in
    let cc = pred c
    and ww = array_array_null [| ending - beginning + 1 ; c |] in
     for i = beginning to ending do
      let row_output = ww.( i - beginning )
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- R.copy row_input.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.hor_band beginning ending w ) ;;

(** {v vert_band beginning ending matrix v} *)
let vert_band = fun (beginning:int) (ending:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w in
    let rr = pred r
    and ww = array_array_null [| r ; ending - beginning + 1 |] in
     for i = 0 to rr do
      let row_output = ww.(i)
      and row_input = w.(i) in
       for j = beginning to ending do
        row_output.( j - beginning ) <- R.copy row_input.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.vert_band beginning ending w ) ;;

(** {v head vert_ending hor_ending matrix v} *)
let rec head = fun (vert_ending:int) (hor_ending:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let ww = array_array_null [| vert_ending + 1 ; hor_ending + 1 |] in
    for i = 0 to vert_ending do
     let row_output = ww.(i)
     and row_input = w.(i) in
      for j = 0 to hor_ending do
       row_output.(j) <- R.copy row_input.(j)
      done ;
    done ;
    Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.head vert_ending hor_ending w ) ;;

(** {v tail beginning ending matrix v} *)
let tail = fun (vert_beginning:int) (hor_beginning:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let rr = pred r
    and cc = pred c
    and ww = array_array_null [| r - vert_beginning + 1 ; c - hor_beginning + 1 |] in
     for i = vert_beginning to rr do
      let row_output = ww.( i - vert_beginning )
      and row_input = w.(i) in
       for j = hor_beginning to cc do
        row_output.( j - hor_beginning ) <- R.copy row_input.(j)
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.tail vert_beginning hor_beginning w ) ;;

(** {v sample v} *)
let rec sample = fun (vert_beginning:int) (vert_ending:int) (hor_beginning:int) (hor_ending:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let ww = array_array_null [| vert_ending - vert_beginning + 1 ; hor_ending - hor_beginning + 1 |] in
    for i = vert_beginning to vert_ending do
     let row_output = ww.( i - vert_beginning )
     and row_input = w.(i) in
      for j = hor_beginning to hor_ending do
       row_output.( j - hor_beginning ) <- R.copy row_input.(j)
      done ;
    done ;
    Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.sample vert_beginning vert_ending hor_beginning hor_ending w ) ;;

(** {v matrix_iter function matrix v} *)
let matrix_iter = fun f (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let rr = pred r
    and cc = pred c in
     for i = 0 to rr do
      let row = w.(i) in
       for j = 0 to cc do
        f ( [| i ; j |] , row.(j) )
       done ;
    done ;
  end
 | Sparse_matrix w -> M.iter f w ;;

(** {v matrix_row_exchange index1 index2 matrix v} *)
let matrix_row_exchange = fun (i:int) (j:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let row = w.(i) in
    w.(i) <- w.(j) ;
    w.(j) <- row ;
  end
 | Sparse_matrix w -> M.row_exchange i j w ;;

(** {v matrix_column_exchange index1 index2 matrix v} *)
let matrix_column_exchange = fun (i:int) (j:int) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and accu = ref ( R.zero () ) in
    for k = 0 to pred r do
     let row = w.(k) in
      accu := row.(i) ;
      row.(i) <- row.(j) ;
      row.(j) <- !accu ;
    done ;
  end
 | Sparse_matrix w -> M.column_exchange i j w ;;

(** {v matrix_in_place_map function matrix v} *)
let matrix_in_place_map = fun f (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let rr = pred ( Array.length w )
   and cc = pred ( Array.length w.(0) ) in
    for i = 0 to rr do
     let row = w.(i) in
      for j = 0 to cc do
       row.(j) <- f row.(j) ;
      done ;
    done ;
  end
 | Sparse_matrix w -> M.in_place_map f w ;;

(** {v matrix_map function matrix v} *)
let matrix_map = fun f (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and c = Array.length w.(0) in
    let ww = array_array_null [| r ; c |]
    and rr = pred r
    and cc = pred c in
     for i = 0 to rr do
      let row_output = ww.(i)
      and row_input = w.(i) in
       for j = 0 to cc do
        row_output.(j) <- f row_input.(j) ;
       done ;
     done ;
     Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.map f w ) ;;


(** {v matrix_in_place_opp matrix v} *)
let matrix_in_place_opp = fun (m:matrix) ->
 matrix_in_place_map R.opp m ;;

(** {v matrix_opp matrix v} *)
let matrix_opp = fun (m:matrix) ->
 matrix_map R.opp m ;;

(** {v matrix_embed dimensions shifts matrix v} *)
let matrix_embed = fun (dimensions:int array) (shifts:int array) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let ww = array_array_null dimensions
   and v_shift = shifts.(0)
   and h_shift = shifts.(1)
   and p_width = pred ( Array.length w.(0) ) in
    for i = 0 to pred ( Array.length w ) do
     let row_in = w.(i)
     and row_out = ww.( v_shift + i ) in
      for j = 0 to p_width do
       row_out.( h_shift + j ) <- R.copy row_in.(j)
      done ;
    done ;
    Full_matrix ww
  end
 | Sparse_matrix w -> Sparse_matrix ( M.embed dimensions shifts w ) ;;

(** {v matrix_row_fold function init matrix v} *)
let matrix_row_fold = fun (f:coeff -> vector -> coeff) (init:vector) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w in
    let x = array_null r in
     for i = 0 to pred r do
      x.(i) <- f ( vector_extract i init ) ( Full_vector w.(i) )
     done ;
     Full_vector x
  end
 | Sparse_matrix w ->
  begin
   match init with
   | Sparse_vector initial ->
    begin
     let ff = fun initialization x -> f initialization ( Sparse_vector x ) in
      Sparse_vector ( M.sparse_row_fold ff initial w )
    end
   | Full_vector initial ->
    begin
     let ff = fun initialization x -> f initialization ( Sparse_vector x ) in
      Full_vector ( M.full_row_fold ff initial w )
    end
  end ;;

(** {v matrix_column_fold function init matrix v} *)
let matrix_column_fold = fun (f:coeff -> vector -> coeff) (init:vector) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let c = Array.length w.(0) in
    let x = array_null c in
     for i = 0 to pred c do
      x.(i) <- f ( vector_extract i init ) ( matrix_column_extract i m )
     done ;
     Full_vector x
  end
 | Sparse_matrix w ->
  begin
   match init with
   | Sparse_vector initial ->
    begin
     let ff = fun initialization x -> f initialization ( Sparse_vector x ) in
      Sparse_vector ( M.sparse_column_fold ff initial w )
    end
   | Full_vector initial ->
    begin
     let ff = fun initialization x -> f initialization ( Sparse_vector x ) in
      Full_vector ( M.full_column_fold ff initial w )
    end
  end ;;

(** {v matrix_row_sum matrix v} *)
let matrix_row_sum = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let f = function x -> vector_sum ( Full_vector x ) in
    Full_vector ( Array.map f w )
  end
 | Sparse_matrix w ->
  match w with
  | M.Sparse_tensor_matrix t -> Sparse_vector ( M.sparse_row_sum w )
  | _ -> Full_vector ( M.full_row_sum w ) ;;

(** {v matrix_column_sum matrix v} *)
let matrix_column_sum = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let c = Array.length w.(0) in
    let accu = array_null c
    and cc = pred c in
     for i = 0 to pred ( Array.length w ) do
      let row = w.(i) in
       for j = 0 to cc do
        accu.(j) <- R.add accu.(j) row.(j)
       done ;
     done ;
     Full_vector accu
  end
 | Sparse_matrix w ->
  match w with
  | M.Sparse_tensor_matrix t -> Sparse_vector ( M.sparse_column_sum w )
  | _ -> Full_vector ( M.full_column_sum w ) ;;

(** {v matrix_sum matrix v} *)
let matrix_sum = function (m:matrix) ->
 vector_sum ( matrix_row_sum m ) ;;

(** {v matrix_vector_prod matrix vector v} *)
let matrix_vector_prod = fun (m:matrix) (v:vector) ->
 match m with
 | Full_matrix w ->
  begin
   let f = function x -> vector_scal_prod v ( Full_vector x ) in
    Full_vector ( Array.map f w )
  end
 | Sparse_matrix w ->
  begin
   match w with
   | M.Sparse_tensor_matrix t ->
    begin
     match v with
     | Sparse_vector x -> Sparse_vector ( M.matrix_sparse_vector_sparse_prod w x )
     | Full_vector x -> Sparse_vector ( M.matrix_full_vector_sparse_prod w x )
    end
   | _ ->
    begin
     match v with
     | Sparse_vector x -> Full_vector ( M.matrix_sparse_vector_full_prod w x )
     | Full_vector x -> Full_vector ( M.matrix_full_vector_full_prod w x )
    end
  end ;;

(** {v vector_matrix_prod vector matrix v} *)
let vector_matrix_prod = fun (v:vector) (m:matrix) ->
 let f = fun fake x -> vector_scal_prod v x in
  matrix_row_fold f ( vector_zero () ) m ;;

(** {v matrix_row_max matrix v} *)
let matrix_row_max = function (m:matrix) ->
 match m with
 | Full_matrix w -> Full_vector ( Array.map ( Util.array_maximum R.compare ) w )
 | Sparse_matrix w ->
  begin
   match w with
   | M.Sparse_tensor_matrix t -> Sparse_vector ( M.sparse_row_max w )
   | _ -> Full_vector ( M.full_row_max w )
  end ;;

(** {v matrix_row_min matrix v} *)
let matrix_row_min = function (m:matrix) ->
 match m with
 | Full_matrix w -> Full_vector ( Array.map ( Util.array_minimum R.compare ) w )
 | Sparse_matrix w ->
  begin
   match w with
   | M.Sparse_tensor_matrix t -> Sparse_vector ( M.sparse_row_min w )
   | _ -> Full_vector ( M.full_row_min w )
  end ;;

(** {v matrix_column_max matrix v} *)
let matrix_column_max = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and result = Array.map R.copy w.(0) in
    let c = Array.length result in
     let cc = pred c in
      for i = 0 to pred r do
       let row = w.(i) in
        for j = 0 to cc do
         let test = row.(j) in
          if R.compare test result.(j) > 0 then
           result.(j) <- test
        done ;
      done ;
      Full_vector result
  end
 | Sparse_matrix w ->
  begin
   match w with
   | M.Sparse_tensor_matrix t -> Sparse_vector ( M.sparse_column_max w )
   | _ -> Full_vector ( M.full_column_max w )
  end ;;

(** {v matrix_column_min matrix v} *)
let matrix_column_min = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let r = Array.length w
   and result = Array.map R.copy w.(0) in
    let c = Array.length result in
     let cc = pred c in
      for i = 0 to pred r do
       let row = w.(i) in
        for j = 0 to cc do
         let test = row.(j) in
          if R.compare test result.(j) < 0 then
           result.(j) <- test
        done ;
      done ;
      Full_vector result
  end
 | Sparse_matrix w ->
  begin
   match w with
   | M.Sparse_tensor_matrix t -> Sparse_vector ( M.sparse_column_min w )
   | _ -> Full_vector ( M.full_column_min w )
  end ;;

(** {v matrix_max matrix v} *)
let matrix_max = function (m:matrix) ->
 match m with
 | Full_matrix w -> Util.array_maximum R.compare ( Array.map ( Util.array_maximum R.compare ) w )
 | Sparse_matrix w ->
  begin
   match w with
   | M.Sparse_tensor_matrix t -> M.sparse_matrix_max w
   | _ -> M.full_matrix_max w
  end ;;

(** {v matrix_min matrix v} *)
let matrix_min = function (m:matrix) ->
 match m with
 | Full_matrix w -> Util.array_minimum R.compare ( Array.map ( Util.array_minimum R.compare ) w )
 | Sparse_matrix w ->
  begin
   match w with
   | M.Sparse_tensor_matrix t -> M.sparse_matrix_min w
   | _ -> M.full_matrix_min w
  end ;;

(** {v matrix_norm_1 matrix v} *)
let matrix_norm_1 = function (m:matrix) ->
 match m with
 | Full_matrix w -> Array.fold_left R.norm_add ( R.norm_zero () ) ( Array.map ( Util.array_maximum R.norm_compare ) ( Array.map ( Array.map R.norm ) w ) ) 
 | Sparse_matrix w -> M.norm_1 w ;;

(** {v matrix_norm_inf matrix v} *)
let matrix_norm_inf = function (m:matrix) ->
 match m with
 | Full_matrix w -> Util.array_maximum R.norm_compare ( Array.map ( Array.fold_left R.norm_add ( R.norm_zero () ) ) ( Array.map ( Array.map R.norm ) w ) )
 | Sparse_matrix w -> M.norm_inf w ;;

(** {v matrix_norm_sum matrix v} *)
let matrix_norm_sum = function (m:matrix) ->
 match m with
 | Full_matrix w -> let f = Array.fold_left R.norm_add ( R.norm_zero () ) in f ( Array.map f ( Array.map ( Array.map R.norm ) w ) )
 | Sparse_matrix w -> M.norm_sum w ;;

(** {v matrix_square_norm_frobenius matrix v} *)
let matrix_square_norm_frobenius = function (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let g = function x ->
    begin
     let y = R.norm x in
      R.norm_mult y y
    end in
    let f = Array.fold_left R.norm_add ( R.norm_zero () ) in f ( Array.map f ( Array.map ( Array.map g ) w ) )
  end
 | Sparse_matrix w -> M.square_norm_frobenius w ;;

(** {v matrix_trace matrix v} *)
let matrix_trace = function (m:matrix) ->
 match m with
 | Full_matrix w -> Array.fold_left R.add ( R.zero () ) ( vector_full_demakeup ( matrix_full_diag_extract m ) )
 | Sparse_matrix w ->
  begin
   match w with
   | M.Sparse_tensor_matrix t -> M.sparse_trace w
   | _ -> M.full_trace w
  end ;;

(** {v matrix_scal_mult scalar matrix v} *)
let matrix_scal_mult = fun (x:coeff) (m:matrix) ->
 match m with
 | Full_matrix w ->
  begin
   let f = Array.map ( R.mult x ) in
    Full_matrix ( Array.map f w )
  end
 | Sparse_matrix w -> Sparse_matrix ( M.scal_mult x w ) ;;

(** {v matrix_add matrix1 matrix2 v} *)
let rec matrix_add = fun (m:matrix) (p:matrix) ->
 if matrix_eq_zero m then
  matrix_copy p
 else
  begin
   if matrix_eq_zero p then
    matrix_copy m
   else
    begin
     match m with
     | Full_matrix mm ->
      begin
       match p with
       | Full_matrix pp ->
        begin
         let r = min ( Array.length mm ) ( Array.length pp )
         and c = min ( Array.length mm.(0) ) ( Array.length pp.(0) ) in
          let q = array_array_null [| r ; c |]
          and cc = pred c in
           for i = 0 to pred r do
            let row_output = q.(i)
            and row_left = mm.(i)
            and row_right = pp.(i) in
             for j = 0 to cc do
              row_output.(j) <- R.add row_left.(j) row_right.(j)
             done ;
           done ;
           Full_matrix q
        end
       | Sparse_matrix pp ->
        begin
         let ppp = matrix_to_full p in
          matrix_add m ppp
        end
      end
     | Sparse_matrix mm ->
      begin
       match p with
       | Full_matrix pp -> matrix_add p m
       | Sparse_matrix pp -> Sparse_matrix ( M.add mm pp )
      end
    end
  end ;;

(** {v matrix_sub matrix1 matrix2 v} *)
let rec matrix_sub = fun (m:matrix) (p:matrix) ->
 if matrix_eq_zero m then
  matrix_opp p
 else
  begin
   if matrix_eq_zero p then
    matrix_copy m
   else
    begin
     match m with
     | Full_matrix mm ->
      begin
       match p with
       | Full_matrix pp ->
        begin
         let r = min ( Array.length mm ) ( Array.length pp )
         and c = min ( Array.length mm.(0) ) ( Array.length pp.(0) ) in
          let q = array_array_null [| r ; c |]
          and cc = pred c in
           for i = 0 to pred r do
            let row_output = q.(i)
            and row_left = mm.(i)
            and row_right = pp.(i) in
             for j = 0 to cc do
              row_output.(j) <- R.sub row_left.(j) row_right.(j)
             done ;
           done ;
           Full_matrix q
        end
       | Sparse_matrix pp ->
        begin
         let ppp = matrix_to_full p in
          matrix_sub m ppp
        end
      end
     | Sparse_matrix mm ->
      begin
       match p with
       | Full_matrix pp ->
        begin
         let mmm = matrix_to_full m in
          matrix_sub mmm p
        end
       | Sparse_matrix pp -> Sparse_matrix ( M.sub mm pp )
      end
    end
  end ;;

(** {v matrix_eq matrix1 matrix2 v} *)
let matrix_eq = fun (m:matrix) (p:matrix) ->
 match m with
 | Full_matrix mm ->
  begin
   match p with
   | Full_matrix pp -> Util.array_eq ( Util.array_eq R.eq ) mm pp
   | Sparse_matrix pp ->
    begin
     let ppp = matrix_to_full p in
      matrix_eq m ppp
    end
  end
 | Sparse_matrix mm ->
  begin
   match p with
   | Full_matrix pp -> matrix_eq p m
   | Sparse_matrix pp -> M.eq mm pp
  end ;;

(** {v matrix_twisted_mult matrix1 matrix2 v} *)
let matrix_twisted_mult = fun (m:matrix) (p:matrix) ->
 match m with
 | Full_matrix mm ->
  begin
   match p with
   | Full_matrix pp ->
    begin
     let r = Array.length mm
     and c = Array.length pp.(0) in
      let q = array_array_null [| r ; c |]
      and cc = pred c in
       for i = 0 to pred r do
        let row_left = Full_vector mm.(i)
        and row_output = q.(i) in
         for j = 0 to cc do
          row_output.(j) <- vector_scal_prod row_left ( Full_vector pp.(j) )
         done ;
       done ;
       Full_matrix q
    end
   | Sparse_matrix pp -> Sparse_matrix ( M.full_sparse_twisted_mult mm pp )
  end
 | Sparse_matrix mm ->
  begin
   match p with
   | Full_matrix pp -> Sparse_matrix ( M.sparse_full_twisted_mult mm pp )
   | Sparse_matrix pp -> Sparse_matrix ( M.twisted_mult mm pp )
  end ;;

(** {v matrix_square_sum matrix v} *)
let matrix_square_sum = function (m:matrix) ->
 matrix_trace ( matrix_twisted_mult m m ) ;;

(** {v matrix_mult matrix1 matrix2 v} *)
let matrix_mult = fun (m:matrix) (p:matrix) ->
 match m with
 | Full_matrix mm ->
  begin
   match p with
   | Full_matrix pp -> matrix_twisted_mult m ( matrix_transpose p )
   | Sparse_matrix pp -> Sparse_matrix ( M.full_sparse_mult mm pp )
  end
 | Sparse_matrix mm ->
  begin
   match p with
   | Full_matrix pp -> Sparse_matrix ( M.sparse_full_mult mm pp )
   | Sparse_matrix pp -> Sparse_matrix ( M.mult mm pp )
  end ;;

(** {v matrix_triple_mult matrix1 matrix2 matrix3 v} *)
let matrix_triple_mult = fun (m:matrix) (n:matrix) (p:matrix) ->
 match ( m , n , p ) with
 | ( Sparse_matrix mm , Sparse_matrix nn , Sparse_matrix pp ) -> Sparse_matrix ( M.triple_mult mm nn pp )
 | _ ->
  begin
   let pp = matrix_transpose p in
    matrix_twisted_mult m ( matrix_twisted_mult pp n )
  end ;;

(** {v matrix_commut matrix1 matrix2 v} *)
let matrix_commut = fun (m:matrix) (n:matrix) ->
 matrix_sub ( matrix_mult m n ) ( matrix_mult n m ) ;;


(** {v matrix_of_blocks matrices_matrix v} If one of the matrices is full, then so is the result.
If all are sparse, then so is the result.

Si l'une des matrices est pleine, alors le résultat l'est.
Si toutes sont creuses, alors le résultat l'est. *)
let matrix_of_blocks = function (m:matrix array array) ->
 let small_dims = Array.map ( Array.map matrix_dimensions ) m
 and r = Array.length m
 and c = Array.length m.(0) in
  let brute_widths = Array.map ( Array.map ( function x -> x.(1) ) ) small_dims
  and brute_heights = Array.map ( Array.map ( function x -> x.(0) ) ) small_dims
  and bands = Array.make r m.(0).(0)
  and mm = Array.make_matrix r c m.(0).(0) in
   let heights = Array.map ( Util.array_maximum compare ) brute_heights
   and widths =  Array.map ( Util.array_maximum compare ) ( Util.transpose brute_widths ) in
    let nrows = Array.fold_left ( + ) 0 heights
    and ncolumns = Array.fold_left ( + ) 0 widths
    and v_shifts = Array.append [| 0 |] heights
    and h_shifts = Array.append [| 0 |] widths in
     for i = 0 to pred r do
      v_shifts.( succ i ) <- v_shifts.(i) + heights.(i)
     done ;
     for i = 0 to pred c do
      h_shifts.( succ i ) <- h_shifts.(i) + widths.(i)
     done ;
     let f = fun i z -> matrix_embed [| nrows ; ncolumns |] [| v_shifts.(i) ; 0 |] z
     and g = fun i j z -> matrix_embed [| heights.(i) ; ncolumns |] [| 0 ; h_shifts.(j) |] z in
      for i = 0 to pred r do
       mm.(i) <- Array.mapi ( g i ) m.(i) ;
       bands.(i) <- Array.fold_left matrix_add ( matrix_sparse_null [| heights.(i) ; ncolumns |] ) mm.(i) ;
      done ;
      let mmm = Array.mapi f bands in
       Array.fold_left matrix_add ( matrix_sparse_null [| nrows ; ncolumns |] ) mmm ;;


(** {v matrix_sparse_of_blocks sparser matrices_matrix v} The resulting matrix is sparse.

La matrice résultat est creuse. *)
let matrix_sparse_of_blocks = fun sparser (m:matrix array array) ->
 let small_dims = Array.map ( Array.map matrix_dimensions ) m
 and r = Array.length m
 and c = Array.length m.(0) in
  let brute_widths = Array.map ( Array.map ( function x -> x.(1) ) ) small_dims
  and brute_heights = Array.map ( Array.map ( function x -> x.(0) ) ) small_dims
  and bands = Array.make r m.(0).(0)
  and mm = Array.make_matrix r c m.(0).(0) in
   let heights = Array.map ( Util.array_maximum compare ) brute_heights
   and widths =  Array.map ( Util.array_maximum compare ) ( Util.transpose brute_widths ) in
    let nrows = Array.fold_left ( + ) 0 heights
    and ncolumns = Array.fold_left ( + ) 0 widths
    and v_shifts = Array.append [| 0 |] heights
    and h_shifts = Array.append [| 0 |] widths in
     for i = 0 to pred r do
      v_shifts.( succ i ) <- v_shifts.(i) + heights.(i)
     done ;
     for i = 0 to pred c do
      h_shifts.( succ i ) <- h_shifts.(i) + widths.(i)
     done ;
     let f = fun i z -> matrix_embed [| nrows ; ncolumns |] [| v_shifts.(i) ; 0 |] z
     and g = fun i j z -> matrix_embed [| heights.(i) ; ncolumns |] [| 0 ; h_shifts.(j) |] ( sparser z ) in
      for i = 0 to pred r do
       mm.(i) <- Array.mapi ( g i ) m.(i) ;
       bands.(i) <- Array.fold_left matrix_add ( matrix_sparse_null [| heights.(i) ; ncolumns |] ) mm.(i) ;
      done ;
      let mmm = Array.mapi f bands in
       Array.fold_left matrix_add ( matrix_sparse_null [| nrows ; ncolumns |] ) mmm ;;

(** {v sparse_diag_matrix_of_blocks sparser matrices_vector v} *)
let sparse_diag_matrix_of_blocks = fun sparser (v:matrix array) ->
 let r = Array.length v in
  let m = Array.make_matrix r r ( matrix_zero () ) in
   for i = 0 to pred r do
    m.(i).(i) <- v.(i) ;
   done ;
   matrix_sparse_of_blocks sparser m ;;

(** {v diag_matrix_of_blocks matrices_vector v} *)
let diag_matrix_of_blocks = function (v:matrix array) ->
 let r = Array.length v in
  let m = Array.make_matrix r r ( matrix_zero () ) in
   for i = 0 to pred r do
    m.(i).(i) <- v.(i) ;
   done ;
   matrix_of_blocks m ;;




(** {C § § § } *)




end ;;




module Field (F:Data.Field_coeff_type) = struct




(** {C § } *)
(** 
{1 Utilitaires}
{1 Utilities}
*)
(** {C  } *)



(** The module resulting from the application of the functor {!module:Math.Mat.Rng} or {!module:Graphicmath.Mat.Rng} to the module [F] is included in the present module.

Le module résultant de l'application du foncteur {!module:Math.Mat.Rng} ou {!module:Graphicmath.Mat.Rng} au module [F] est inclus dans le présent module. *)

include ( Rng (F) ) ;;
(** {C include ( Rng (F) ) } *)



(** The module [U] realizes the calculation on the sparse vectors with coefficients
in the field [F] and indices of type [int].

Le module [U] réalise le calcul sur les vecteurs creux à coefficients 
dans le corps commutatif [F] et à indices de type [int].*)
module U = Sparse_vector.Field (Data.Zindex) (Hash.Z) (F) ;;



(** The module [S] realizes the calculation on the sparse tensors with coefficients
in the field [F] and indices of type [int].

Le module [S] réalise le calcul sur les tenseurs creux à coefficients 
dans le corps commutatif [F] et à indices de type [int].*)
module S = Sparse_tensor.Field (Data.Zindex) (Hash.Z) (F) ;;



(** The module [N] realizes the calculation on the sparse matrices with coefficients
in the field [F] and indices of type [int].

Le module [N] réalise le calcul sur les matrices creuses à coefficients 
dans le corps commutatif [F] et à indices de type [int].*)
module N = Sparse_matrix.Field (Data.Zindex) (Hash.Z) (F) ;;



(** {v scalar_matrix scalar dimensions v} *)
let scalar_matrix = fun (x:coeff) (d:int array) ->
 Sparse_matrix ( N.Diff_to_scal_matrix ( x , S.null d ) ) ;;

(** {v identity_matrix dimensions v} *)
let identity_matrix = function (d:int array) ->
  scalar_matrix ( F.one () ) d ;;

(** {v coeff_two unit v} *)
let coeff_two = function () ->
 N.coeff_two () ;;

(** {v coeff_half unit v} *)
let coeff_half = function () ->
 N.coeff_half () ;;




(** {C § } *)
(** 
{1 Opérations}
*)
(** {C  } *)




(** {v vector_in_place_scal_left_div scalar vector v} *)
let vector_in_place_scal_left_div = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Array.iteri ( fun i x -> w.(i) <- F.div x y ) w
 | Sparse_vector w -> U.in_place_scal_left_div y w ;;

(** {v vector_scal_left_div scalar vector v} *)
let vector_scal_left_div = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.map ( function x -> F.div x y ) w )
 | Sparse_vector w -> Sparse_vector ( U.scal_left_div y w ) ;;

(** {v vector_in_place_scal_right_div scalar vector v} *)
let vector_in_place_scal_right_div = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Array.iteri ( fun i x -> w.(i) <- F.div y x ) w
 | Sparse_vector w -> U.in_place_scal_right_div y w ;;

(** {v vector_scal_right_div scalar vector v} *)
let vector_scal_right_div = fun (y:coeff) (v:vector) ->
 match v with
 | Full_vector w -> Full_vector ( Array.map ( F.div y ) w )
 | Sparse_vector w -> Sparse_vector ( U.scal_right_div y w ) ;;

(** {v vector_reciprocal vector v} *)
let vector_reciprocal = function (v:vector) ->
 match v with
 | Full_vector w -> vector_scal_left_div ( F.norm_inject ( vector_square_norm_2 v ) ) v
 | Sparse_vector w -> Sparse_vector ( U.reciprocal w ) ;;

(** {v tensor_in_place_scal_left_div scalar tensor v} *)
let tensor_in_place_scal_left_div = fun (x:coeff) (v:tensor) ->
 match v with
 | Full_tensor w -> tensor_in_place_scal_mult ( F.inv x ) v
 | Vector w -> vector_in_place_scal_left_div x w
 | Sparse_tensor w -> S.in_place_scal_left_div x w ;;

(** {v tensor_scal_left_div scalar tensor v} *)
let tensor_scal_left_div = fun (x:coeff) (v:tensor) ->
 match v with
 | Full_tensor w -> tensor_scal_mult ( F.inv x ) v
 | Vector w -> Vector ( vector_scal_left_div x w )
 | Sparse_tensor w -> Sparse_tensor ( S.scal_left_div x w ) ;;

(** {v matrix_scal_left_div scalar matrix v} *)
let matrix_scal_left_div = fun (x:coeff) (v:matrix) ->
 match v with
 | Full_matrix w -> matrix_scal_mult ( F.inv x ) v
 | Sparse_matrix w -> Sparse_matrix ( N.scal_left_div x w ) ;;

(** {v matrix_scal_reciprocal matrix v} *)
let matrix_scal_reciprocal = function (m:matrix) ->
 match m with
 | Full_matrix w -> matrix_scal_left_div ( F.norm_inject ( matrix_square_norm_frobenius m ) ) m
 | Sparse_matrix w -> Sparse_matrix ( N.reciprocal w ) ;;

(** {v matrix_sym matrix v} *)
let matrix_sym = function (m:matrix) ->
 let mm = matrix_transpose m in
  matrix_scal_mult ( coeff_half () ) ( matrix_add m mm )

(** {v matrix_antisym matrix v} *)
let matrix_antisym = function (m:matrix) ->
 let mm = matrix_transpose m in
  matrix_scal_mult ( coeff_half () ) ( matrix_sub m mm )

(** {v in_place_pivot_downward matrix1 matrix2 v} *)
let in_place_pivot_downward = fun (m:matrix) (p:matrix) ->
 match ( m , p ) with
 | ( Sparse_matrix mm , Sparse_matrix pp ) -> N.in_place_pivot_downward mm pp
 | ( Full_matrix w , _ ) ->
  begin
   let index = ref 0
   and r = Array.length w
   and factor = ref ( F.one () )
   and coeff = ref ( F.one () )
   and row_left = ref ( vector_zero () )
   and row_right = ref ( vector_zero () )
   and row_output_left = ref ( vector_zero () )
   and row_output_right = ref ( vector_zero () )
   and first_column = ref ( vector_zero () ) in
    let rr = pred r in
     for i = 0 to rr do
      first_column := mask_vector i rr ( matrix_column_extract i m ) ;
      index := vector_first_non_zero !first_column ;
      if !index < 0 then
       failwith "Non invertible left matrix in Mat.Field.in_place_pivot_downward." ;
      if !index > i then
       begin
        vector_exchange i !index !first_column ;
        matrix_row_exchange i !index m ;
        matrix_row_exchange i !index p ;
       end ;
      factor := F.inv ( vector_extract i !first_column ) ;
      row_left := matrix_row_extract i m ;
      vector_in_place_scal_mult !factor !row_left ;
      vector_replace ( F.one () ) i !row_left ;
      matrix_row_replace !row_left i m ;
      row_right := matrix_row_extract i p ;
      vector_in_place_scal_mult !factor !row_right ;
      matrix_row_replace !row_right i p ;
      for j = succ i to rr do
       row_output_left := matrix_row_extract j m ;
       coeff := vector_extract i !row_output_left ;
       row_output_left := vector_sub !row_output_left ( vector_scal_mult !coeff !row_left ) ;
       vector_replace ( F.zero () ) i !row_output_left ;
       matrix_row_replace !row_output_left j m ;
       row_output_right := matrix_row_extract j p ;
       row_output_right := vector_sub !row_output_right ( vector_scal_mult !coeff !row_right ) ;
       matrix_row_replace !row_output_right j p ;
      done ;
     done ;
  end
 | _ -> failwith "Incompatible formats in Mat.Field.in_place_pivot_downward." ;;

(** {v pivot_downward matrix1 matrix2 v} *)
let rec pivot_downward = fun (m:matrix) (p:matrix) ->
 match ( m , p ) with
 | ( Sparse_matrix mm , Sparse_matrix pp ) ->
  begin
   let result = N.pivot_downward mm pp in
    [| Sparse_matrix result.(0) ; Sparse_matrix result.(1) |]
  end
 | ( Full_matrix w , _ ) ->
  begin
   let mm = matrix_copy m
   and pp = matrix_copy p in
    in_place_pivot_downward mm pp ;
    [| mm ; pp |]
  end
 | ( Sparse_matrix mm , Full_matrix pp ) ->
  begin
   let mmm = matrix_to_full m
   and ppp = matrix_copy p in
    in_place_pivot_downward mmm ppp ;
    [| mmm ; ppp |]
  end ;;

(** {v invertibility matrix v} *)
let invertibility = function (m:matrix) ->
 match m with
 | Sparse_matrix mm -> N.invertibility mm
 | Full_matrix ww ->
  begin
   let index = ref 0
   and r = Array.length ww
   and factor = ref ( F.one () )
   and coeff = ref ( F.one () )
   and row = ref ( vector_zero () )
   and row_output = ref ( vector_zero () )
   and first_column = ref ( vector_zero () ) in
    let rr = pred r
    and p = matrix_copy m in
     try
      begin
       for i = 0 to pred rr do
        first_column := mask_vector i rr ( matrix_column_extract i p ) ;
        index := vector_first_non_zero !first_column ;
        if !index < 0 then
         failwith "Non invertibility condition in Mat.Field.invertibility." ;
        if !index > i then
         begin
          vector_exchange i !index !first_column ;
          matrix_row_exchange i !index p ;
         end ;
        factor := F.inv ( vector_extract i !first_column ) ;
        row := matrix_row_extract i p ;
        vector_in_place_scal_mult !factor !row ;
        vector_replace ( F.one () ) i !row ;
        matrix_row_replace !row i p ;
        for j = succ i to rr do
         row_output := matrix_row_extract j p ;
         coeff := vector_extract i !row_output ;
         row_output := vector_sub !row_output ( vector_scal_mult !coeff !row ) ;
         matrix_row_replace !row_output j p ;
        done ;
       done ;
       not ( F.eq_zero ( matrix_extract rr rr p ) )
      end
     with _ ->
      false
  end ;;

(** {v det matrix v} *)
let det = function (m:matrix) ->
 match m with
 | Sparse_matrix mm -> N.det mm
 | Full_matrix ww ->
  begin
   let index = ref 0
   and result = ref ( F.one () )
   and r = Array.length ww
   and factor = ref ( F.one () )
   and coeff = ref ( F.one () )
   and row = ref ( vector_zero () )
   and row_output = ref ( vector_zero () )
   and first_column = ref ( vector_zero () ) in
    let rr = pred r
    and p = matrix_copy m in
     try
      begin
       for i = 0 to pred rr do
        first_column := mask_vector i rr ( matrix_column_extract i p ) ;
        index := vector_first_non_zero !first_column ;
        if !index < 0 then
         failwith "Non invertibility condition in Mat.Field.det." ;
        if !index > i then
         begin
          vector_exchange i !index !first_column ;
          matrix_row_exchange i !index p ;
         end ;
        factor := vector_extract i !first_column ;
        result := F.mult !result !factor ;
        factor := F.inv !factor ;
        row := matrix_row_extract i p ;
        vector_in_place_scal_mult !factor !row ;
        vector_replace ( F.one () ) i !row ;
        matrix_row_replace !row i p ;
        for j = succ i to rr do
         row_output := matrix_row_extract j p ;
         coeff := vector_extract i !row_output ;
         row_output := vector_sub !row_output ( vector_scal_mult !coeff !row ) ;
         matrix_row_replace !row_output j p ;
        done ;
       done ;
       F.mult !result ( matrix_extract rr rr p )
      end
     with _ ->
      F.zero ()
  end ;;


(** {v in_place_pivot_upward matrix1 matrix2 v} The left matrix [m] is supposed to be 
upper triangular with ones on the diagonal.

La matrice de gauche [m] est supposée triangulaire supérieure avec des 1 sur la diagonale. *)
let in_place_pivot_upward = fun (m:matrix) (p:matrix) ->
 match ( m , p ) with
 | ( Sparse_matrix mm , Sparse_matrix pp ) -> N.in_place_pivot_upward mm pp
 | ( Full_matrix w , _ ) ->
  begin
   let index = ref 0
   and r = Array.length w
   and coeff = ref ( F.one () )
   and row_right = ref ( vector_zero () )
   and row_output_left = ref ( vector_zero () )
   and row_output_right = ref ( vector_zero () )
   and last_column = ref ( vector_zero () ) in
    let rr = pred r in
     for i = rr downto 1 do
      index := pred i ;
      last_column := mask_vector 0 !index ( matrix_column_extract i m ) ;
      row_right := matrix_row_extract i p ;
      for j = !index downto 0 do
       row_output_left := matrix_row_extract j m ;
       coeff := vector_extract i !row_output_left ;
       matrix_replace ( F.zero () ) j i m ;
       row_output_right := matrix_row_extract j p ;
       row_output_right := vector_sub !row_output_right ( vector_scal_mult !coeff !row_right ) ;
       matrix_row_replace !row_output_right j p ;
      done ;
     done ;
  end
 | _ -> failwith "Incompatible formats in Mat.Field.in_place_pivot_upward." ;;

(** {v pivot_upward matrix1 matrix2 v} *)
let rec pivot_upward = fun (m:matrix) (p:matrix) ->
 match ( m , p ) with
 | ( Sparse_matrix mm , Sparse_matrix pp ) ->
  begin
   let result = N.pivot_upward mm pp in
    [| Sparse_matrix result.(0) ; Sparse_matrix result.(1) |]
  end
 | ( Full_matrix w , _ ) ->
  begin
   let mm = matrix_copy m
   and pp = matrix_copy p in
    in_place_pivot_upward mm pp ;
    [| mm ; pp |]
  end
 | ( Sparse_matrix mm , Full_matrix pp ) ->
  begin
   let mmm = matrix_to_full m
   and ppp = matrix_copy p in
    in_place_pivot_upward mmm ppp ;
    [| mmm ; ppp |]
  end ;;

(** {v inv matrix v} *)
let inv = function (m:matrix) ->
 let d = matrix_dimensions m
 and mm = matrix_copy m in
  let p = identity_matrix d in
   in_place_pivot_downward mm p ;
   in_place_pivot_upward mm p ;
   p ;;

(** {v left_quotient matrix1 matrix2 v} *)
let left_quotient = fun (m:matrix) (p:matrix) ->
 let mm = matrix_copy m
 and pp = matrix_copy p in
  in_place_pivot_downward mm pp ;
  in_place_pivot_upward mm pp ;
  pp ;;

(** {v right_quotient matrix1 matrix2 v} *)
let right_quotient = fun (m:matrix) (p:matrix) ->
 let mm = matrix_transpose m
 and pp = matrix_transpose p in
  matrix_transpose ( left_quotient pp mm ) ;;

(** {v cond matrix_norm invertor matrix v} *)
let cond = fun (norm:matrix -> F.u) (invertor:matrix -> matrix) (m:matrix) ->
 F.norm_mult ( norm m ) ( norm ( invertor m ) ) ;;

(** {v naive_solve matrix vector v} *)
let naive_solve = fun (m:matrix) (v:vector) ->
 matrix_vector_prod ( inv m ) v ;;

(** {v solve matrix vector v} *)
let solve = fun (m:matrix) (v:vector) ->
 let p = match v with
 | Full_vector x -> Full_matrix ( Array.map ( Array.make 1 ) x )
 | Sparse_vector x -> Sparse_matrix ( N.transpose ( N.sparse_vector_to_square_matrix x ) ) in
  let q = left_quotient m p in
   match p with
   | Full_matrix y -> Full_vector ( Array.map ( function z -> z.(0) ) ( matrix_full_demakeup q ) )
   | Sparse_matrix y -> Sparse_vector ( N.column_extract 0 ( matrix_sparse_demakeup q ) ) ;;

(** {v tune_inv matrix inverse_candidate v} *)
let tune_inv = fun (x:matrix) (y:matrix) ->
 let d = matrix_dimensions x in
  let double = scalar_matrix ( coeff_two () ) ( Array.make 2 d.(0) )
  and right_product = matrix_mult x y in
   let difference = matrix_sub double right_product in
    matrix_mult y difference ;;

(** {v approx_inv matrix_norm invertor matrix v} *)
let approx_inv = fun (norm:matrix -> F.u) (invertor:matrix -> matrix) (x:matrix) ->
 let y = invertor x
 and d = matrix_dimensions x in
  let product = matrix_mult x y
  and id = identity_matrix ( Array.make 2 d.(0) )
  and result = tune_inv x y in
   let error0 = norm ( matrix_sub product id )
   and new_product = matrix_mult x result in
    let error1 = norm ( matrix_sub new_product id ) in
     if F.norm_compare error1 error0 >= 0 then
      ( y , error0 )
     else
      ( result , error1 ) ;;

(** {v matrix_extrap_inv parameter matrix v} *)
let matrix_extrap_inv = fun (parameter:F.t) (x:matrix) ->
 let y = inv x in
  let yy = tune_inv x y in
   matrix_add yy ( matrix_scal_mult parameter ( matrix_sub yy y ) ) ;;

(** {v approx_solve matrix_norm matrix vector v} *)
let approx_solve = fun (norm:matrix -> F.u) (m:matrix) (v:vector) ->
 let mm = fst ( approx_inv norm inv m ) in
  matrix_vector_prod mm v ;;

(** {v matrix_iterate exponent matrix vector v} *)
let matrix_iterate = fun (s:int) (x:matrix) (v:vector) ->
 let y = ref v in
  if s >= 0 then
   begin
    for i = 1 to s do
     y := matrix_vector_prod x !y
    done
   end
  else
   begin
    let xx = fst ( approx_inv matrix_norm_inf inv x ) in
     for i = 1 to s do
      y := matrix_vector_prod xx !y
     done
   end ;
   !y

(** {v matrix_power exponent matrix v} *)
let rec matrix_power = fun (s:int) (x:matrix) ->
 if s >= 0 then
  begin
   if s = 0 then identity_matrix ( matrix_dimensions x )
   else
    let n = s / 2 in
     let factor = matrix_power n x in
      let prod = matrix_mult factor factor in
       if s mod 2 = 0 then prod
       else matrix_mult prod x
  end
 else
  begin
   let xx = fst ( approx_inv matrix_norm_inf inv x ) in
    matrix_power ( abs s ) xx
  end ;;









(** {C § § § } *)




end ;;










(** {C § § § } *)





end ;;



