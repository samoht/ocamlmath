




module Fft = struct



open Util ;;
open Data ;;
open Sparse_vector ;;
open Sparse_tensor ;;
open Sparse_matrix ;;
open Mat ;;


module Field (F:Data.Field_coeff_type) = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module:

- constructions of matrices of direct and inverse discrete Fourier transforms
for a primary number of samples (p^n where p is prime),

- functions of direct and inverse fast Fourier transforms
for a binary number of samples (2^n).



{2 Comments}



The coefficients must belong to a field and the user has to provide the necessary roots of unity.


This module is distributed under the same licence as Ocaml.



{C § }



La mathématicienne ou le mathématicien trouvera dans ce module :

- des constructions de matrices de transformées de Fourier discrètes, directes et inverses
pour un nombre d'échantillons primaire (p^n où p est premier),

- des fonctions de transformées de Fourier rapide directes et inverses
pour un nombre d'échantillons binaire (2^n).



{2 Commentaires}



Les coefficients doivent appartenir à un corps commutatif et l'utilisateur ou l'utilisatrice doit fournir les racines de l'unité nécessaires.


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




(** {C § } *)
(** 
{1 Constructions minimales}
{1 Minimal constructions}
*)
(** {C  } *)




open Util ;;
module M = Mat.Field (F) ;;


(** {v dft primitive_root_of_unity dimension v}

La racine primitive de l'unité [omega] est supposée d'ordre [n].
Dans le cas des nombres complexes, on prend exp ( - 2 * i * pi / n ). *)
let dft = fun (omega:F.t) (n:int) ->
 let m = Array.make_matrix n n omega
 and nn = pred n in
  for i = 0 to nn do
   let row = m.(i) in
    for j = 0 to nn do
     row.(j) <- F.int_pow ( i * j mod n ) omega ;
    done ;
  done ;
  M.Full_matrix m ;;

(** {v inverse_dft primitive_root_of_unity dimension v}

La racine primitive de l'unité [omega] est supposée d'ordre [n].
Dans le cas des nombres complexes, on prend exp ( - 2 * i * pi / n ). *)
let inverse_dft = fun (omega:F.t) (n:int) ->
 let m = Array.make_matrix n n omega
 and nn = pred n
 and d = F.int_div n in
  for i = 0 to nn do
   let row = m.(i) in
    for j = 0 to nn do
     row.(j) <- d ( F.int_pow ( - i * j mod n ) omega ) ;
    done ;
  done ;
  M.Full_matrix m ;;

(** {v twist_dft_vector twist_factor dimension v} *)
let twist_dft_vector = fun (twist:F.t) (n:int) ->
 let v = Array.map F.one ( Array.make n () ) in
  for i = 1 to pred n do
   v.(i) <- F.mult twist v.( pred i )
  done ;
  M.Full_vector v ;;

(** {v twist_dft_matrix twist_factor dimension v} *)
let twist_dft_matrix = fun (twist:F.t) (n:int) ->
 M.vector_to_diag ( twist_dft_vector twist n ) ;;

(** {v twisted_dft primitive_root_of_unity dimension twist_factor v} *)
let twisted_dft = fun (omega:F.t) (n:int) (twist:F.t) ->
 let m = dft omega n
 and mm = twist_dft_matrix twist n in
  M.matrix_to_full ( M.matrix_mult m mm ) ;;




(** {C § } *)
(** 
{1 Constructions matricielles}
{1 Matrix constructions}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Algorithme binaire de Gauss-Runge-Danielson-Lanczos-Cooley-Tukey}
{2 Binary algorithm of Gauss-Runge-Danielson-Lanczos-Cooley-Tukey}
*)
(** {C  } *)





(**

décrit dans le polycopié Baudoin-Bercher ESIEE 2001.
*)





(** {v scal_block_fft scalar dimension v} *)
let scal_block_fft = fun (x:F.t) (dim:int) ->
 M.scal_matrix x dim ;;

(** {v element_binary_fft primitive_root_of_unity degree exponent v} *)
let element_binary_fft = fun (omega:F.t) (n:int) (exponent:int) ->
 if n < 0 then failwith "Bad degree in Fft.element_binary_fft." ;
 let dim = Util.int_power n 2
 and x = F.int_pow exponent omega in
  let block_1 = scal_block_fft ( F.one () ) dim
  and block_2 = scal_block_fft x dim
  and block_3 = scal_block_fft ( F.opp x ) dim in
   M.matrix_of_blocks [| [| block_1 ; block_2 |] ; [| block_1 ; block_3 |] |] ;;

(** {v factor_binary_fft primitive_root_of_unity degree step v} *)
let factor_binary_fft = fun (omega:F.t) (n:int) (step:int) ->
 if n < 2 then failwith "Bad degree in Fft.element_binary_fft." ;
 let nn = pred n in
  if ( step < 0 ) || ( step > nn ) then failwith "Bad step in Fft.element_binary_fft." ;
  let slicing = Util.int_power step 2
  and complement = nn - step in
   let small_dim = Util.int_power complement 2 in
    let z = M.matrix_sparse_null ( Array.make 2 ( 2 * small_dim ) ) in
     let m = Array.make_matrix slicing slicing z in
      for i = 0 to pred slicing do
       let ii = Util.truncated_bit_reversal nn i in
        m.(i).(i) <- element_binary_fft omega complement ii ;
      done ;
      M.matrix_of_blocks m ;;

(** {v naive_aux_binary_fft primitive_root_of_unity degree v} *)
let naive_aux_binary_fft = fun (omega:F.t) (n:int) ->
 let accu = ref ( factor_binary_fft omega n 0 ) in
  for i = 1 to pred n do
   accu := M.matrix_mult ( factor_binary_fft omega n i ) !accu ;
  done ;
  M.matrix_to_full !accu ;;

(** {v aux_binary_fft_matrix primitive_root_of_unity degree v} *)
let aux_binary_fft_matrix = fun (omega:F.t) (n:int) ->
 let accu = ref ( M.matrix_transpose ( factor_binary_fft omega n 0 ) ) in
  for i = 1 to pred n do
   accu := M.matrix_twisted_mult !accu ( factor_binary_fft omega n i ) ;
  done ;
  M.matrix_to_full ( M.matrix_transpose !accu ) ;;

(** {v binary_row_mix degree matrix v} *)
let binary_row_mix = fun (n:int) (m:M.matrix) ->
 let dim = int_power n 2 in
  for i = 0 to pred dim do
   let ii = truncated_bit_reversal n i in
    if ( ii > i ) then
     M.matrix_row_exchange i ii m
  done ;;

(** {v binary_fft_matrix primitive_root_of_unity degree v} *)
let binary_fft_matrix = fun (omega:F.t) (n:int) ->
 let m = aux_binary_fft_matrix omega n in
  binary_row_mix n m ;
  m ;;

(** {v inverse_binary_fft_matrix primitive_root_of_unity degree v} *)
let inverse_binary_fft_matrix = fun (omega:F.t) (n:int) ->
 let root = F.inv omega
 and dim = int_power n 2 in
  let m = binary_fft_matrix root n in
   M.matrix_map ( F.int_div dim ) m ;;

(** {v binary_twist_fft_vector twist_factor degree v} *)
let binary_twist_fft_vector = fun (twist:F.t) (n:int) ->
 let dim = int_power n 2
 and bits = ref ( Array.make n false )
 and powers = Array.map F.one ( Array.make n () ) in
  powers.(0) <- twist ;
  let v = Array.map F.one ( Array.make dim () )
  and dd = pred dim in
   for i = 1 to pred n do
    let x = powers.( pred i ) in
     powers.(i) <- F.square x
   done ;
   let f = fun i x -> if x then powers.(i) else F.one () in
    for i = 1 to dd do
     bits := reverse_array ( truncated_bits_of_int n i ) ;
     let factors = Array.mapi f !bits in
      v.(i) <- Array.fold_left F.mult ( F.one () ) factors
    done ;
    M.Full_vector v ;;

(** {v aux_binary_twist_fft_matrix twist_factor degree v} *)
let aux_binary_twist_fft_matrix = fun (twist:F.t) (n:int) ->
 M.vector_to_diag ( binary_twist_fft_vector twist n ) ;;

(** {v binary_twisted_fft_matrix primitive_root_of_unity degree twist_factor v} *)
let binary_twisted_fft_matrix = fun (omega:F.t) (n:int) (twist:F.t) ->
 let m = binary_fft_matrix omega n
 and mm = aux_binary_twist_fft_matrix twist n in
  M.matrix_to_full ( M.matrix_mult m mm ) ;;




(** {C § } *)
(** 
{2 Algorithme primaire de Gauss-Runge-Danielson-Lanczos-Cooley-Tukey}
{2 Primary algorithm of Gauss-Runge-Danielson-Lanczos-Cooley-Tukey}
*)
(** {C  } *)




(** {v element_primary_fft raw_root radix primitive_root_of_unity degree exponent v}

La racine primitive de l'unité [raw_root] doit être d'ordre la base [p]. 
La racine primitive de l'unité [omega] doit être d'ordre p^n. *)
let element_primary_fft = fun (raw_root:F.t) (p:int) (omega:F.t) (n:int) (exponent:int) ->
 if n < 0 then failwith "Bad degree in Fft.element_primary_fft." ;
 let dim = Util.int_power n p
 and pp = pred p
 and x = F.int_pow exponent omega in
  let f = fun i j -> scal_block_fft ( F.mult ( F.int_pow i raw_root ) ( F.int_pow j x ) ) dim in
   let init = scal_block_fft ( F.one () ) dim in
    let m = Array.make_matrix p p init in
     for i = 0 to pp do
      let row = m.(i) in
       for j = 1 to pp do
        row.(j) <- f ( ( i * j ) mod p ) j
       done ;
     done ;
     M.matrix_of_blocks m ;;


(** {v factor_primary_fft raw_root radix primitive_root_of_unity degree step v}

La racine primitive de l'unité [raw_root] doit être d'ordre la base [p]. 
La racine primitive de l'unité [omega] doit être d'ordre p^n. *)
let factor_primary_fft = fun (raw_root:F.t) (p:int) (omega:F.t) (n:int) (step:int) ->
 if n < 2 then failwith "Bad degree in Fft.element_primary_fft." ;
 let nn = pred n in
  if ( step < 0 ) || ( step > nn ) then failwith "Bad step in Fft.element_primary_fft." ;
  let slicing = Util.int_power step p
  and complement = nn - step in
  let small_dim = Util.int_power complement p in
   let z = M.matrix_sparse_null ( Array.make 2 ( p * small_dim ) ) in
    let m = Array.make_matrix slicing slicing z in
     for i = 0 to pred slicing do
      let ii = Util.truncated_digit_reversal p nn i in
       m.(i).(i) <- element_primary_fft raw_root p omega complement ii ;
     done ;
     M.matrix_of_blocks m ;;

(** {v aux_primary_fft_matrix raw_root radix primitive_root_of_unity degree v} *)
let aux_primary_fft_matrix = fun (raw_root:F.t) (p:int) (omega:F.t) (n:int) ->
 let accu = ref ( M.matrix_transpose ( factor_primary_fft raw_root p omega n 0 ) ) in
  for i = 1 to pred n do
   accu := M.matrix_twisted_mult !accu ( factor_primary_fft raw_root p omega n i ) ;
  done ;
  M.matrix_to_full ( M.matrix_transpose !accu ) ;;

(** {v primary_row_mix radix degree matrix v} *)
let primary_row_mix = fun (p:int) (n:int) (m:M.matrix) ->
 let dim = int_power n p in
  for i = 0 to pred dim do
   let ii = truncated_digit_reversal p n i in
    if ( ii > i ) then
     M.matrix_row_exchange i ii m
  done ;;

(** {v primary_fft_matrix raw_root radix primitive_root_of_unity degree v} *)
let primary_fft_matrix = fun (raw_root:F.t) (p:int) (omega:F.t) (n:int) ->
 let m = aux_primary_fft_matrix raw_root p omega n in
  primary_row_mix p n m ;
  m ;;

(** {v inverse_primary_fft_matrix raw_root radix primitive_root_of_unity degree v} *)
let inverse_primary_fft_matrix = fun (raw_root:F.t) (p:int) (omega:F.t) (n:int) ->
 let root = F.inv omega
 and dim = int_power n p in
  let m = primary_fft_matrix raw_root p root n in
   M.matrix_map ( F.int_div dim ) m ;;

(** {v primary_twist_fft_vector radix twist_factor degree v} *)
let primary_twist_fft_vector = fun (p:int) (twist:F.t) (n:int) ->
 let dim = int_power n p
 and pp = pred p
 and p_p = p - 2
 and digits = ref ( Array.make n 0 ) in
  let powers = Array.map ( Array.map F.one ) ( Array.make_matrix n pp () ) in
   let first_row = powers.(0) in
    first_row.(0) <- twist ;
    for j = 1 to p_p do
     first_row.(j) <- F.mult twist first_row. ( pred j )
    done ;
    let v = Array.map F.one ( Array.make dim () )
    and dd = pred dim in
     for i = 1 to pred n do
      let pred_row = powers.( pred i )
      and row = powers.(i) in
       let x = pred_row.(0)
       and y = pred_row.(p_p) in
        let z = F.mult x y in
         row.(0) <- z ;
         for j = 1 to p_p do
          row.(j) <- F.mult row.( pred j ) z
         done ;
     done ;
     let f = fun i x -> if x > 0 then powers.(i).( pred x ) else F.one () in
     for i = 1 to dd do
      digits := reverse_array ( truncated_digits_of_int p n i ) ;
      let factors = Array.mapi f !digits in
       v.(i) <- Array.fold_left F.mult ( F.one () ) factors
      done ;
      M.Full_vector v ;;

(** {v aux_primary_twist_fft_matrix radix twist_factor degree v} *)
let aux_primary_twist_fft_matrix = fun (p:int) (twist:F.t) (n:int) ->
 M.vector_to_diag ( primary_twist_fft_vector p twist n ) ;;

(** {v primary_twisted_fft_matrix raw_root radix primitive_root_of_unity degree twist_factor v} *)
let primary_twisted_fft_matrix = fun (raw_root:F.t) (p:int) (omega:F.t) (n:int) (twist:F.t) ->
 let m = primary_fft_matrix raw_root p omega n
 and mm = aux_primary_twist_fft_matrix p twist n in
  M.matrix_to_full ( M.matrix_mult m mm ) ;;




(** {C § } *)
(** 
{1 Fonctions vectorielles}
{1 Vector functions}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Algorithme binaire de Gauss-Runge-Danielson-Lanczos-Cooley-Tukey}
{2 Binary algorithm of Gauss-Runge-Danielson-Lanczos-Cooley-Tukey}
*)
(** {C  } *)




(** {v naive_vector_binary_mix degree vector v} *)
let naive_vector_binary_mix = fun (n:int) (v:M.vector) ->
 let dim = int_power n 2 in
  for i = 0 to pred dim do
   let ii = truncated_bit_reversal n i in
    if ( ii > i ) then
     M.vector_exchange i ii v
  done ;;

(** {v vector_binary_mix degree vector v} *)
let vector_binary_mix = fun (n:int) (v:M.vector) ->
 let f = function ( i , x ) ->
  begin
   let ii = truncated_bit_reversal n i in
    if ( ii > i ) then
     M.vector_exchange i ii v
  end in
  M.vector_iter f v ;;

(** {v extract_odd vector v} *)
let extract_odd = function (v:M.vector)->
 let ( dd , h ) = M.vector_sparse_demakeup ( M.vector_to_sparse ( M.vector_nihil v ) ) in
  M.V.H.resize ( max 1 ( ( abs ( M.V.H.size h ) ) / 2 ) ) h ;
  let result = M.Sparse_vector ( dd / 2 , h ) in
   let f = function ( i , x ) ->
    begin
     if i land 1 <> 0 then
      M.vector_insert_add x ( i / 2 ) result 
    end in
    M.vector_iter f v ;
    result ;;

(** {v extract_even vector v} *)
let extract_even = function (v:M.vector)->
 let ( dd , h ) = M.vector_sparse_demakeup ( M.vector_to_sparse ( M.vector_nihil v ) ) in
  M.V.H.resize ( max 1 ( ( abs ( M.V.H.size h ) ) / 2 ) ) h ;
  let result = M.Sparse_vector ( dd / 2 , h ) in
   let f = function ( i , x ) ->
    begin
     if i land 1 = 0 then
      M.vector_insert_add x ( i / 2 ) result 
    end in
    M.vector_iter f v ;
    result ;;


(** {v vector_binary_fft threshold primitive_root_of_unity degree powers matrix_array vector v}
This function is not tail recursive.
The dimension of the vector [v] is supposed to be equal to 2^n.

La dimension du vecteur [v] est supposée égale à 2^n.
Cette fonction n'est pas récursive terminale. *)
let rec vector_binary_fft = fun (threshold:int) (omega:F.t) (n:int) (powers:F.t array) (matrices:M.matrix array) (v:M.vector) ->
 match compare n ( max 2 threshold ) with
 | test when test <= 0 ->
  begin
   try
    begin
     let mat = matrices.(n) in
      let dm = M.matrix_dimensions mat
      and dv = M.vector_dimension v in
       assert ( ( dm.(0) = dv ) && ( dm.(1) = dv ) ) ;
       ( M.matrix_vector_prod mat v , powers , mat )
    end
   with _ ->
    begin
     let mat = binary_fft_matrix omega n in
      ( M.matrix_vector_prod mat v , powers , mat )
    end
  end
 | _ ->
  begin
   let om = F.square omega
   and nn = pred n
   and lp = Array.length powers
   and d = M.vector_dimension v
   and result = M.vector_nihil v
   and odd = extract_odd v
   and even = extract_even v in
    let t = Util.primo ( vector_binary_fft threshold om nn ( Util.extract_even powers ) matrices even )
    and dd = d / 2
    and tt = Util.primo ( vector_binary_fft threshold om nn ( Util.extract_even powers ) matrices odd ) in
     let pow = if lp >= dd then powers
      else
       begin
        let tableau = Array.append powers ( Array.map F.one ( Array.make ( dd - lp ) () ) ) in
        for i = lp to pred dd do
         tableau.(i) <- F.mult omega tableau.( pred i )
        done ;
        tableau
      end in
     let f = function ( i , x ) ->
      begin
       M.vector_insert_add x i result ;
       M.vector_insert_add x ( i + dd ) result ;
      end
     and g = function ( i , x ) ->
      begin
       M.vector_insert_add ( F.mult pow.(i) x ) i result ;
       M.vector_insert_sub ( F.mult pow.(i) x ) ( i + dd ) result ;
      end in
      M.vector_iter f t ;
      M.vector_iter g tt ;
      ( result , pow , M.matrix_zero () )
  end ;;


(** {v vector_inverse_binary_fft threshold primitive_root_of_unity degree inverse_powers matrix_array vector v}
This function is not tail recursive.
The dimension of the vector [v] is supposed to be equal to 2^n.

La dimension du vecteur [v] est supposée égale à 2^n.
Cette fonction n'est pas récursive terminale. *)
let rec vector_inverse_binary_fft = fun (threshold:int) (omega:F.t) (n:int) (powers:F.t array) (matrices:M.matrix array) (v:M.vector) ->
 match compare n ( max 2 threshold ) with
 | test when test <= 0 ->
  begin
   try
    begin
     let mat = matrices.(n) in
      let dm = M.matrix_dimensions mat
      and dv = M.vector_dimension v in
       assert ( ( dm.(0) = dv ) && ( dm.(1) = dv ) ) ;
       ( M.matrix_vector_prod mat v , powers , mat )
    end
   with _ ->
    begin
     let mat = inverse_binary_fft_matrix omega n in
      ( M.matrix_vector_prod mat v , powers , mat )
    end
  end
 | _ ->
  begin
   let om = F.square omega
   and inv_omega = F.inv omega
   and half = F.int_div 2 ( F.one () )
   and nn = pred n
   and lp = Array.length powers
   and d = M.vector_dimension v
   and result = M.vector_nihil v
   and odd = extract_odd v
   and even = extract_even v in
    let t = Util.primo ( vector_inverse_binary_fft threshold om nn ( Util.extract_even powers ) matrices even )
    and dd = d / 2
    and tt = Util.primo ( vector_inverse_binary_fft threshold om nn ( Util.extract_even powers ) matrices odd ) in
     let pow = if lp >= dd then powers
      else
       begin
        let tableau = Array.append powers ( Array.map F.one ( Array.make ( dd - lp ) () ) ) in
        for i = lp to pred dd do
         tableau.(i) <- F.mult inv_omega tableau.( pred i )
        done ;
        tableau
      end in
     let f = function ( i , x ) ->
      begin
       M.vector_insert_add x i result ;
       M.vector_insert_add x ( i + dd ) result ;
      end
     and g = function ( i , x ) ->
      begin
       M.vector_insert_add ( F.mult pow.(i) x ) i result ;
       M.vector_insert_sub ( F.mult pow.(i) x ) ( i + dd ) result ;
      end in
      M.vector_iter f t ;
      M.vector_iter g tt ;
      ( M.vector_scal_mult ( half ) result , pow , M.matrix_zero () )
  end ;;













(** {C § § § } *)


end ;;










(** {C § § § } *)


end ;;





