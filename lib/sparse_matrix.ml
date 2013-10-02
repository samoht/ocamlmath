




module Sparse_matrix = struct



(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)



(**
The mathematician will find in this module functors to handle
sparse matrices with coefficients in a commutative rng or a field.



{2 Conventions}



Dimensions are often unreachable in a sparse tensor as soon as it is null (excepted sometimes for the first variable). 
Since sparse matrices are modelled on sparse tensors, the problem is present too for null sparse amtrices, 
and a null sparse matrix is sometimes supposed to be square.



{2 Comments}



A function is {e sealed} if there is no sharing between the input variables and the output value.
This is the expected behavior of usual mathematical functions.
The recursive programming of polymorphic functions is easier in a {e non sealed} way.
Some copy functions are provided for every type of data.
They are sealed provided that they receive as argument elementary copy functions for coefficients and indices.
By composition, they permit to seal all functions necessary.


This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module des focnteurs pour traiter
les matrices creuses à coefficients dans un annau commutatif ou un corps.



{2 Conventions}



Les dimensions d'un tenseur creux sont souvent inaccessibles (sauf parfois pour la première variable) dès qu'il est nul.
Puisque les matrices creuses sont modelées sur les tenseurs creux, le problème se pose aussi pour les matrices creuses nulles, 
et une matrice creuse nulle est parfois supposée carrée.



{2 Commentaires}



Une fonction est {e étanche} quand il n'y a aucun partage entre les variables fournies en entrée et la valeur obtenue en sortie.
C'est le comportement attendu des fonctions mathématiques habituelles. 
La programmation récursive des fonctions polymorphes est plus facile de manière {e non étanche}.
Des fonctions de recopie sont fournies pour tous les types de données. 
Elles sont étanches à condition de leur fournir en argument des fonctions élémentaires de recopie des coefficients et des indices. 
Par composition, elle permettent d'étanchéifier toutes les fonctions voulues.


Ce module est distribué selon la même licence qu'Ocaml.


{C Copyright Stéphane Grognet}
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS}
{C Fédération de recherche mathématique des Pays de la Loire}
{C Centre Henri Lebesgue}
{C IREM des Pays de la Loire - Université de Nantes}
{C version 0.1}
*)
(**
@version 0.1
*)
(**
@author Stéphane Grognet
*)
(**
@since 2013
*)




(** {C § } *)
(** 
{2 Matrices creuses à coefficients dans un annau commutatif}
{2 Sparse matrices with coefficients in a commiutative Rng}
*)
(** {C  } *)




open Util ;;
open Data ;;
open Hash ;;
open Sparse_vector ;;
open Sparse_tensor ;;



module Rng (Index:Data.Index_type) (Hasher:Hash.Hash_type with type t = Index.t) (Coeff:Data.Rng_coeff_type) = struct



type index = Index.t ;;

type coeff = Coeff.t ;;

type elt = index * coeff ;;



module V = Sparse_vector.Rng (Index) (Hasher) (Coeff) ;;



module T = Sparse_tensor.Rng (Index) (Hasher) (Coeff) ;;



(** A very sparse matrix is profitably represented as a sparse tensor.
The matrices who are little different from a scalar or multi-scalar matrix are also included in the type.
In all the preceding cases, the indices may be naturally polymorphic.
In all the following cases, the indices are generally of type [int].
The invertible sparse matrices are handled preferably as full vectors whose coefficients are sparse vectors.
The matrices who differ a little from a diagonal or multi-diagonal matrix are also included in the type.
The non square matrices of type Diff* are supposed to have more columns than rows.
The lengths of the lines of coefficients on their own in Diff_to_diag_matrix and Diff_to_multi_diag_matrix 
may be greater than the dimensions of the associated sparse tensor.

BEWARE : In the [Diff_to_multi_diag] type, somme marginal coefficients of the multidiagonal are not taken into account : the second index of the multidiagonal is always (up to [Index.from_int] translation) the row number of the matrix.

Une matrice très creuse sera représentée avantageusement comme un tenseur creux.
Les matrices qui diffèrent peu d'un matrice scalaire ou multi-scalaire sont aussi incluses dans le type.
Dans tous les cas précédents, les indices peuvent être naturellement polymorphes.
Dans les cas qui suivent, les indices sont généralement de type [int].
Les matrices creuses inversibles sont manipulées de préférence comme des vecteurs pleins dont les coefficients sont des vecteurs creux.
Les matrices qui diffèrent peu d'une matrice diagonale ou multidiagonale sont aussi incluses dans le type.
Les matrics non carrées de type Diff* sont censées avoir plus de colonnes que de lignes.
La longueur des lignes de coefficients mis à part dans Diff_to_diag_matrix et Diff_to_multi_diag_matrix 
peuvent être supérieures aux dimensions du tenseur creux associé.

ATTENTION : Dans le type [Diff_to_multi_diag], certains coefficients marginaux de la multidiagonale ne sont pas pris en compte : le second indice de la multidiagonale est toujours le numéro de ligne de la matrice (à traduction [Index.from_int] près). *)
type t = 
 | Sparse_tensor_matrix of T.t
 | Diff_to_scal_matrix of coeff * T.t
 | Diff_to_multi_scal_matrix of coeff array * T.t
 | Half_full_matrix of V.t array
 | Diff_to_diag_matrix of coeff array * T.t
 | Diff_to_multi_diag_matrix of coeff array array * T.t ;;

(** {v copy matrix v} *)
let copy = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( T.copy w )
 | Diff_to_scal_matrix ( x , w ) -> Diff_to_scal_matrix ( Coeff.copy x , T.copy w )
 | Diff_to_multi_scal_matrix ( a , w ) -> Diff_to_multi_scal_matrix ( Array.map Coeff.copy a , T.copy w )
 | Half_full_matrix w -> Half_full_matrix ( Array.map ( V.copy ) w )
 | Diff_to_diag_matrix ( a , w ) -> Diff_to_diag_matrix ( Array.map Coeff.copy a , T.copy w )
 | Diff_to_multi_diag_matrix ( a , w ) -> Diff_to_multi_diag_matrix ( Array.map ( Array.map Coeff.copy ) a , T.copy w ) ;;




(** {C § } *)
(** 
{3 Déstructurations}
*)
(** {C  } *)




(** {v sparse_tensor_matrix_demakeup matrix v} *)
let sparse_tensor_matrix_demakeup = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> w
 | _ -> failwith "Not a sparse tensor matrix in Sparse_matrix.Rng.sparse_tensor_matrix_demakeup." ;;

(** {v diff_to_scal_matrix_demakeup matrix v} *)
let diff_to_scal_matrix_demakeup = function (m:t) ->
 match m with
 | Diff_to_scal_matrix ( x , w ) -> ( x , w )
 | _ -> failwith "Not a difference to scalar matrix in Sparse_matrix.Rng.diff_to_scal_matrix_demakeup." ;;

(** {v diff_to_multi_scal_matrix_demakeup matrix v} *)
let diff_to_multi_scal_matrix_demakeup = function (m:t) ->
 match m with
 | Diff_to_multi_scal_matrix ( a , w ) -> ( a , w )
 | _ -> failwith "Not a difference to multi-scalar matrix in Sparse_matrix.Rng.diff_to_multi_scal_matrix_demakeup." ;;

(** {v half_full_matrix_demakeup matrix v} *)
let half_full_matrix_demakeup = function (m:t) ->
 match m with
 | Half_full_matrix w -> w
 | _ -> failwith "Not a half full matrix in Sparse_matrix.Rng.half_full_matrix_demakeup." ;;

(** {v diff_to_diag_matrix_demakeup matrix v} *)
let diff_to_diag_matrix_demakeup = function (m:t) ->
 match m with
 | Diff_to_diag_matrix ( a , w ) -> ( a , w )
 | _ -> failwith "Not a difference to diagonal matrix in Sparse_matrix.Rng.diff_to_diag_matrix_demakeup." ;;

(** {v diff_to_multi_diag_matrix_demakeup matrix v} *)
let diff_to_multi_diag_matrix_demakeup = function (m:t) ->
 match m with
 | Diff_to_multi_diag_matrix ( a , w ) -> ( a , w )
 | _ -> failwith "Not a difference to multi-diagonal matrix in Sparse_matrix.Rng.diff_to_multi_diag_matrix_demakeup." ;;




(** {C § } *)
(** 
{3 Opérations élémentaires}
{3 Elementary operations}
*)
(** {C  } *)




(** {v null dimensions v} *)
let null = fun (dimensions:'b array)  ->
 if Array.length dimensions <> 2 then failwith "Bad number of dimensions in Sparse_matrix.Rng.null." ;
 Sparse_tensor_matrix ( T.null dimensions ) ;;

(** {v zero unit v} *)
let zero = fun () ->
 Sparse_tensor_matrix ( T.zero () ) ;;

(** {v filling matrix v} *)
let filling = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> T.filling w
 | Diff_to_scal_matrix ( x , w ) -> T.filling w
 | Diff_to_multi_scal_matrix ( a , w ) -> T.filling w
 | Half_full_matrix w ->
  begin
   let a = Array.map V.filling w in
    Array.fold_left ( + ) 0 a
  end
 | Diff_to_diag_matrix ( a , w ) -> T.filling w
 | Diff_to_multi_diag_matrix ( a , w ) -> T.filling w ;;

(** {v detailed_filling matrix v} *)
let detailed_filling = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> [| 0 ; T.filling w |]
 | Diff_to_scal_matrix ( x , w ) -> [| 1 ; T.filling w |]
 | Diff_to_multi_scal_matrix ( a , w ) -> [| Array.length a ; T.filling w |]
 | Half_full_matrix w -> [| 0 ; filling m |]
 | Diff_to_diag_matrix ( a , w ) -> [| Array.length a ; T.filling w |]
 | Diff_to_multi_diag_matrix ( a , w ) -> [| ( Array.length a ) * ( Array.length a.(0) ) ; T.filling w |] ;;

(** {v sizes matrix v} *)
let sizes = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> T.sizes w
 | Diff_to_scal_matrix ( x , w ) -> T.sizes w
 | Diff_to_multi_scal_matrix ( a , w ) -> T.sizes w
 | Half_full_matrix w -> ( max_int , Array.map V.size w )
 | Diff_to_diag_matrix ( a , w ) -> T.sizes w
 | Diff_to_multi_diag_matrix ( a , w ) -> T.sizes w ;;

(** {v size matrix v} *)
let size = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> T.size w
 | Diff_to_scal_matrix ( x , w ) -> T.size w
 | Diff_to_multi_scal_matrix ( a , w ) -> T.size w
 | Half_full_matrix w -> Array.fold_left ( + ) 0 ( Array.map V.size w )
 | Diff_to_diag_matrix ( a , w ) -> T.size w
 | Diff_to_multi_diag_matrix ( a , w ) -> T.size w ;;

(** {v dimensions matrix v} *)
let dimensions = function (m:t) ->
 let d = 
  begin
   match m with
   | Sparse_tensor_matrix w -> T.dimensions w
   | Diff_to_scal_matrix ( x , w ) -> T.dimensions w
   | Diff_to_multi_scal_matrix ( a , w ) -> T.dimensions w
   | Half_full_matrix w ->
    begin
     let c = V.dimension w.(0)
     and r = Array.length w in
      [| Index.from_int r ; c |]
    end
   | Diff_to_diag_matrix ( a , w ) -> T.dimensions w
   | Diff_to_multi_diag_matrix ( a , w ) -> T.dimensions w 
  end in
  if ( Array.length d ) = 1 then Array.make 2 d.(0) else d ;;

(** {v cleanup matrix v} *)
let cleanup = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> T.cleanup w
 | Diff_to_scal_matrix ( x , w ) -> T.cleanup w
 | Diff_to_multi_scal_matrix ( a , w ) -> T.cleanup w
 | Half_full_matrix w -> ignore ( Array.map V.cleanup w )
 | Diff_to_diag_matrix ( a , w ) -> T.cleanup w
 | Diff_to_multi_diag_matrix ( a , w ) -> T.cleanup w ;;

(** {v resize size matrix v} *)
let resize = fun (n:int) (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> T.resize n w
 | Diff_to_scal_matrix ( x , w ) -> T.resize n w
 | Diff_to_multi_scal_matrix ( a , w ) -> T.resize n w
 | Half_full_matrix w -> ignore ( Array.map ( V.resize n ) w )
 | Diff_to_diag_matrix ( a , w ) -> T.resize n w
 | Diff_to_multi_diag_matrix ( a , w ) -> T.resize n w ;;

(** {v to_string matrix v} *)
let to_string = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> "Sparse_tensor_matrix " ^ ( T.to_string w )
 | Diff_to_scal_matrix ( x , w ) -> "Diff_to_scal_matrix " ^ ( Coeff.to_string x ) ^ " $ " ^ ( T.to_string w )
 | Diff_to_multi_scal_matrix ( a , w ) -> "Diff_to_multi_scal_matrix " ^ ( Util.bare_vector_to_string Coeff.to_string a ) ^ " $ " ^ ( T.to_string w )
 | Half_full_matrix w -> "Half_full_matrix " ^ ( Util.vector_to_string ( V.to_string ) "{|" "~" "|}" w )
 | Diff_to_diag_matrix ( a , w ) -> "Diff_to_diag_matrix " ^ ( Util.bare_vector_to_string Coeff.to_string a ) ^ " $ " ^ ( T.to_string w )
 | Diff_to_multi_diag_matrix ( a , w ) -> "Diff_to_multi_diag_matrix " ^ ( Util.vector_to_string ( Util.bare_vector_to_string Coeff.to_string ) "{|" "~" "|}" a ) ^ " $ " ^ ( T.to_string w ) ;;

(** {v sparse_tensor_matrix_of_string string v} *)
let sparse_tensor_matrix_of_string = function (s:string) ->
 let b = String.sub s 0 21 in
  if String.compare b "Sparse_tensor_matrix " = 0 then
   begin
    let t = String.sub s 21 ( ( String.length s ) - 21 ) in
     Sparse_tensor_matrix ( T.of_string t )
   end
  else
   failwith "Not a sparse tensor matrix in Sparse_matrix.Rng.sparse_tensor_matrix_of_string." ;;

(** {v half_full_matrix_of_string string v} *)
let half_full_matrix_of_string = function (s:string) ->
 let b = String.sub s 0 17 in
  if String.compare b "Half_full_matrix " = 0 then
   begin
    let t = String.sub s 17 ( ( String.length s ) - 17 ) in
     Half_full_matrix ( Util.vector_of_string ( V.of_string ) "{|" "~" "|}" t )
   end
  else
   failwith "Not a half full matrix in Sparse_matrix.Rng.half_full_matrix_of_string." ;;

(** {v diff_to_scal_matrix_of_string string v} *)
let diff_to_scal_matrix_of_string = function (s:string) ->
 let b = String.sub s 0 20 in
  if String.compare b "Diff_to_scal_matrix " = 0 then
   begin
    let t = String.sub s 20 ( ( String.length s ) - 20 ) in
     let listing = Str.split ( Str.regexp_string " $ " ) t in
      let u = List.hd listing
      and v = List.hd ( List.tl listing ) in
       Diff_to_scal_matrix ( Coeff.of_string u , T.of_string v )
   end
  else
   failwith "Not a difference-to-scalar matrix in Sparse_matrix.Rng.diff_to_scal_matrix_of_string." ;;

(** {v diff_to_multi_scal_matrix_of_string string v} *)
let diff_to_multi_scal_matrix_of_string = function (s:string) ->
 let b = String.sub s 0 26 in
  if String.compare b "Diff_to_multi_scal_matrix " = 0 then
   begin
    let t = String.sub s 26 ( ( String.length s ) - 26 ) in
     let listing = Str.split ( Str.regexp_string " $ " ) t in
      let u = List.hd listing
      and v = List.hd ( List.tl listing ) in
       Diff_to_multi_scal_matrix ( Util.bare_vector_of_string Coeff.of_string u , T.of_string v )
   end
  else
   failwith "Not a difference-to-multi-scalar matrix in Sparse_matrix.Rng.diff_to_multi_scal_matrix_of_string." ;;

(** {v diff_to_diag_matrix_of_string string v} *)
let diff_to_diag_matrix_of_string = function (s:string) ->
 let b = String.sub s 0 20 in
  if String.compare b "Diff_to_diag_matrix " = 0 then
   begin
    let t = String.sub s 20 ( ( String.length s ) - 20 ) in
     let listing = Str.split ( Str.regexp_string " $ " ) t in
      let u = List.hd listing
      and v = List.hd ( List.tl listing ) in
       Diff_to_diag_matrix ( Util.bare_vector_of_string Coeff.of_string u , T.of_string v )
   end
  else
   failwith "Not a difference-to-diagonal matrix in Sparse_matrix.Rng.diff_to_diag_matrix_of_string." ;;

(** {v diff_to_multi_diag_matrix_of_string string v} *)
let diff_to_multi_diag_matrix_of_string = function (s:string) ->
 let b = String.sub s 0 26 in
  if String.compare b "Diff_to_multi_diag_matrix " = 0 then
   begin
    let t = String.sub s 26 ( ( String.length s ) - 26 ) in
     let listing = Str.split ( Str.regexp_string " $ " ) t in
      let u = List.hd listing
      and v = List.hd ( List.tl listing ) in
       let uu = Util.vector_of_string ( Util.bare_vector_of_string Coeff.of_string ) "{|" "~" "|}" u in
        Diff_to_multi_diag_matrix ( uu , T.of_string v )
   end
  else
   failwith "Not a difference-to-multi-diagonal matrix in Sparse_matrix.Rng.diff_to_multi_diag_matrix_of_string." ;;

(** {v of_string string v} *)
let of_string = function (s:string) ->
 try
  sparse_tensor_matrix_of_string s
 with _ ->
  begin
   try
    half_full_matrix_of_string s
   with _ ->
    begin
     try
      diff_to_scal_matrix_of_string s
     with _ ->
      begin
       try
        diff_to_multi_scal_matrix_of_string s
       with _ ->
        begin
         try
          diff_to_diag_matrix_of_string s
         with _ ->
          begin
           try
            diff_to_multi_diag_matrix_of_string s
           with _ ->
            failwith "Not a valid string in Sparse_matrix.Rng.of_string."
          end
        end
      end
    end
  end ;;

(** {v print matrix v} *)
let print = function (m:t) ->
 print_string ( to_string m ) ;
 print_newline () ;;


(** {v description_eq_zero matrix v} This verification of nullity is superficial.

Cette vérification de nullité est superficielle. *)
let description_eq_zero = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w  -> T.eq_zero w
 | Diff_to_scal_matrix ( x , w ) -> ( Coeff.eq_zero x ) && ( T.eq_zero w )
 | Diff_to_multi_scal_matrix ( a , w ) -> ( Util.array_eq_zero Coeff.eq_zero a ) && ( T.eq_zero w )
 | Half_full_matrix w -> Util.array_eq_zero V.eq_zero w
 | Diff_to_diag_matrix ( a , w ) -> ( Util.array_eq_zero Coeff.eq_zero a ) && ( T.eq_zero w )
 | Diff_to_multi_diag_matrix ( a , w ) -> ( Util.array_eq_zero ( Util.array_eq_zero Coeff.eq_zero ) a ) && ( T.eq_zero w ) ;;


(** {v description_eq matrix1 matrix2 v} This verification of equality is superficial.

Cette vérification de nullité est superficielle. *)
let rec description_eq = fun (m:t) (n:t) ->
 match ( m , n ) with
 | ( Sparse_tensor_matrix w , Sparse_tensor_matrix ww ) -> T.eq w ww
 | ( Diff_to_scal_matrix ( x , w ) , Diff_to_scal_matrix ( xx , ww ) ) -> ( Coeff.eq x xx ) && ( T.eq w ww )
 | ( Diff_to_multi_scal_matrix ( a , w ) , Diff_to_multi_scal_matrix ( aa , ww ) ) ->
  begin
   let accu = ref ( T.eq w ww )
   and r = Array.length a
   and rr = Array.length aa in
    if !accu then
     begin
      if r <> rr then
       false
      else
       begin
        let i = ref 0 in
         while !i < r do
          accu := Coeff.eq a.(!i) aa.(!i) ;
          if !accu then
           incr i
          else
           i := r ;
         done ;
         !accu
       end
     end
    else
     !accu
  end
 | ( Half_full_matrix w , Half_full_matrix ww ) ->
  begin
   let r = Array.length w
   and rr = Array.length ww in
    let accu = ref ( r = rr ) in
     if !accu then
      begin
       let i = ref 0 in
        while !i < r do
         accu := V.eq w.(!i) ww.(!i) ;
         if !accu then
          incr i
         else i := r
        done ;
        !accu
      end
     else
      false
  end
 | ( Diff_to_diag_matrix ( a , w ) , Diff_to_diag_matrix ( aa , ww ) ) -> description_eq ( Diff_to_multi_scal_matrix ( a , w ) ) ( Diff_to_multi_scal_matrix ( aa , ww ) )
 | ( Diff_to_multi_diag_matrix ( a , w ) , Diff_to_multi_diag_matrix ( aa , ww ) )->
  begin
   let accu = ref ( T.eq w ww )
   and r = Array.length a
   and rr = Array.length aa
   and c = Array.length a.(0)
   and cc = Array.length aa.(0) in
    if !accu then
     begin
      if ( r <> rr ) || ( c <> cc )then
       false
      else
       begin
        let i = ref 0 in
         while !i < r do
          accu := Util.array_eq Coeff.eq a.(!i) aa.(!i) ;
          if !accu then
           incr i
          else
           i := r ;
         done ;
         !accu
       end
     end
    else
     !accu
  end
 | _ -> failwith "Incompatible formats in Sparse_matrix.Rng.sparse_matrix_eq." ;;

(** {v tensor_row_extract index tensor v} *)
let tensor_row_extract = fun (i:index) (w:T.t) ->
 T.vector_demakeup ( T.tensor_to_vector ( T.sub_tensor_extract 0 i w ) ) ;;

(** {v tensor_column_extract index tensor v} *)
let tensor_column_extract = fun (i:index) (w:T.t) ->
  T.vector_demakeup ( T.tensor_to_vector ( T.sub_tensor_extract 1 i w ) ) ;;

(** {v row_extract index matrix v} *)
let row_extract = fun (i:index) (m:t) ->
 match m with
 | Half_full_matrix w -> w.( Index.to_int i )
 | Sparse_tensor_matrix w  -> tensor_row_extract i w
 | Diff_to_scal_matrix ( x , w ) ->
  begin
   let v = tensor_row_extract i w in
    V.insert_add x i v ;
    v
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let v = tensor_row_extract i w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Index.to_int ( V.dimension v ) )
    and shift = ii - h_a_l in
     for j = max 0 shift to min cc ( ii + h_a_l ) do
      V.insert_add a.( j - shift ) ( Index.from_int j ) v ;
     done ;
    v
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let v = tensor_row_extract i w in
    V.insert_add a.( Index.to_int i ) i v ;
    v
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let v = tensor_row_extract i w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Index.to_int ( V.dimension v ) )
    and shift = ii - h_a_l in
     for j = max 0 shift to min cc ( ii + h_a_l ) do
      V.insert_add a.( j - shift ).( Index.to_int i ) ( Index.from_int j ) v ;
     done ;
    v
  end ;;

(** {v column_extract index matrix v} *)
let column_extract = fun (i:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let r = Array.length w in
    let d = Index.from_int r in
     let v = V.null d in
      for j = 0 to pred r do
       try
        begin
         let ( ii , x ) = V.unsafe_extract i w.(j) in
          V.insert_add x ( Index.from_int j ) v
        end
       with _ ->
        ()
      done ;
      v
  end
 | Sparse_tensor_matrix w  -> tensor_column_extract i w
 | Diff_to_scal_matrix ( x , w ) ->
  begin
   let v = tensor_column_extract i w in
    V.insert_add x i v ;
    v
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let v = tensor_column_extract i w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let rr = pred ( Index.to_int ( V.dimension v ) )
    and shift = ii - h_a_l
    and bound = ii + h_a_l in
     for j = max 0 shift to min rr bound do
      V.insert_add a.( bound - j ) ( Index.from_int j ) v ;
     done ;
    v
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let v = tensor_column_extract i w in
    V.insert_add a.( Index.to_int i ) i v ;
    v
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let v = tensor_column_extract i w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let rr = pred ( Index.to_int ( V.dimension v ) )
    and shift = ii - h_a_l
    and bound = ii + h_a_l in
     for j = max 0 shift to min rr bound do
      V.insert_add a.( bound - j ).(j) ( Index.from_int j ) v ;
     done ;
    v
  end ;;

(** {v extract row column matrix v} *)
let extract = fun (i:index) (j:index) (m:t) ->
 match m with
 | Half_full_matrix w -> snd ( V.extract j w.( Index.to_int i ) )
 | Sparse_tensor_matrix w -> snd ( T.extract [| i ; j |] w )
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   let x = snd ( T.extract [| i ; j |] w ) in
    if Index.eq i j then
     Coeff.add y x
    else
     x
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let x = snd ( T.extract [| i ; j |] w )
   and h_a_l = ( Array.length a ) / 2
   and diff = Index.to_int ( Index.sub i j ) in
    if abs diff <= h_a_l then
     Coeff.add a.( h_a_l + diff ) x
    else
     x
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let x = snd ( T.extract [| i ; j |] w ) in
    if Index.eq i j then
     Coeff.add a.( Index.to_int i ) x
    else
     x
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let x = snd ( T.extract [| i ; j |] w )
   and h_a_l = ( Array.length a ) / 2
   and diff = Index.to_int ( Index.sub i j ) in
    if abs diff <= h_a_l then
     Coeff.add a.( h_a_l + diff ).( Index.to_int i ) x
    else
     x
  end ;;

(** {v tensor_row_remove index tensor v} *)
let tensor_row_remove = fun (i:index) (w:T.t) ->
 T.sub_tensor_remove 0 i w ;;

(** {v tensor_column_remove index tensor v} *)
let tensor_column_remove = fun (i:index) (w:T.t) ->
 T.sub_tensor_remove 1 i w ;;

(** {v row_remove index matrix v} *)
let row_remove = fun (i:index) (m:t) ->
 match m with
 | Half_full_matrix w -> w.( Index.to_int i ) <- V.zero ()
 | Sparse_tensor_matrix w -> tensor_row_remove i w
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   tensor_row_remove i w ;
   try
    T.insert_add ( Coeff.opp y ) [| i ; i |] w
   with _ ->
    ()
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   tensor_row_remove i w ;
   let d = T.dimensions w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Index.to_int d.(1) )
    and shift = ii - h_a_l in
     for j = max 0 shift to min ( ii + h_a_l ) cc do
      let jj = Index.from_int j in
       T.replace ( Coeff.opp a.( j - shift ) ) [| i ; jj |] w ;
     done ;
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   tensor_row_remove i w ;
   try
    a.( Index.to_int i ) <- Coeff.zero ()
   with _ ->
    ()
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   tensor_row_remove i w ;
   let d = T.dimensions w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Index.to_int d.(1) )
    and shift = ii - h_a_l in
     for j = max 0 shift to min ( ii + h_a_l ) cc do
      let jj = Index.from_int j in
       T.replace ( Coeff.opp a.( j - shift ).(ii) ) [| i ; jj |] w ;
     done ;
  end ;;

(** {v column_remove index matrix v} *)
let column_remove = fun (i:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   for j = 0 to pred ( Array.length w ) do
    V.remove i w.(j)
   done
  end
 | Sparse_tensor_matrix w -> tensor_column_remove i w
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   tensor_column_remove i w ;
   try
    T.insert_add ( Coeff.opp y ) [| i ; i |] w
   with _ ->
    ()
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   tensor_column_remove i w ;
   let h_a_l = ( Array.length a ) / 2
   and d = T.dimensions w
   and ii = Index.to_int i in
    let cc = pred ( Index.to_int d.(1) )
    and shift = ii - h_a_l
    and bound = ii + h_a_l in
     for j = max 0 shift to min bound cc do
      let jj = Index.from_int j in
       T.replace ( Coeff.opp a.( bound - j ) ) [| i ; jj |] w ;
     done ;
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   tensor_column_remove i w ;
   try
    a.( Index.to_int i ) <- Coeff.zero () ;
   with _ ->
    ()
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   tensor_column_remove i w ;
   let h_a_l = ( Array.length a ) / 2
   and d = T.dimensions w
   and ii = Index.to_int i in
    let cc = pred ( Index.to_int d.(1) )
    and shift = ii - h_a_l
    and bound = ii + h_a_l in
     for j = max 0 shift to min bound cc do
      let jj = Index.from_int j
      and row_index = bound - j in
       T.replace ( Coeff.opp a.(row_index).(j) ) [| jj ; i |] w ;
     done ;
  end ;;

(** {v remove row column matrix v} *)
let remove = fun (i:index) (j:index) (m:t) ->
 match m with
 | Half_full_matrix w -> V.remove j w.( Index.to_int i )
 | Sparse_tensor_matrix w -> T.remove [| i ; j |] w
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   if Index.eq i j then
    T.replace ( Coeff.opp y ) ( Array.make 2 i ) w
   else
    T.remove [| i ; j |] w ;
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let h_a_l = ( Array.length a ) / 2
   and diff = Index.to_int ( Index.sub i j ) in
    if abs diff <= h_a_l then
     T.replace ( Coeff.opp a.( h_a_l + diff ) ) [| i ; j |] w
    else
     T.remove [| i ; j |] w ;
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   T.remove [| i ; j |] w ;
   if Index.eq i j then
    a.( Index.to_int i ) <- Coeff.zero () ;
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   T.remove [| i ; j |] w ;
   let h_a_l = ( Array.length a ) / 2
   and diff = Index.to_int ( Index.sub i j ) in
    if abs diff <= h_a_l then
     try
      a.( h_a_l + diff ).( Index.to_int i ) <- Coeff.zero () ;
     with _ ->
      ()
  end ;;

(** {v tensor_row_replace vector index matrix v} *)
let tensor_row_replace = fun (x:V.t) (i:index) (w:T.t) ->
 T.sub_tensor_replace ( T.Vector x ) 0 i w ;;

(** {v tensor_column_replace vector index matrix v} *)
let tensor_column_replace = fun (x:V.t) (i:index) (w:T.t) ->
 T.sub_tensor_replace ( T.Vector x ) 1 i w ;;


(** {v raw_row_replace vector index matrix v} For the types storing diagonal values on their own,
these ones are not taken into account in the operation.
This function is applied in place.

Cette fonction est exécutée en place.
Dans les types stockant des valeurs diagonales à part, celles-ci n'entrent pas en compte dans l'opération. *)
let raw_row_replace = fun (x:V.t) (i:index) (m:t) ->
 match m with
 | Half_full_matrix w -> w.( Index.to_int i ) <- x
 | Sparse_tensor_matrix w -> tensor_row_replace x i w
 | Diff_to_scal_matrix ( y , w ) -> tensor_row_replace x i w
 | Diff_to_multi_scal_matrix ( y , w ) -> tensor_row_replace x i w
 | Diff_to_diag_matrix ( a , w ) -> tensor_row_replace x i w
 | Diff_to_multi_diag_matrix ( a , w ) -> tensor_row_replace x i w ;;


(** {v raw_column_replace vector index matrix v} For the types storing diagonal values on their own,
these ones are not taken into account in the operation.
This function is applied in place.

Cette fonction est exécutée en place.
Dans les types stockant des valeurs diagonales à part, celles-ci n'entrent pas en compte dans l'opération. *)
let raw_column_replace = fun (x:V.t) (i:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let f = function ( j , y ) -> V.replace y i w.( Index.to_int j ) in
    V.iter f x
  end
 | Sparse_tensor_matrix w -> tensor_column_replace x i w
 | Diff_to_scal_matrix ( y , w ) -> tensor_column_replace x i w
 | Diff_to_multi_scal_matrix ( y , w ) -> tensor_column_replace x i w
 | Diff_to_diag_matrix ( a , w ) -> tensor_column_replace x i w
 | Diff_to_multi_diag_matrix ( a , w ) -> tensor_column_replace x i w ;;


(** {v row_replace vector index matrix v} This function is applied in place.

Cette fonction est exécutée en place. *)
let row_replace = fun (x:V.t) (i:index) (m:t) ->
 match m with
 | Half_full_matrix w -> w.( Index.to_int i ) <- x
 | Sparse_tensor_matrix w -> tensor_row_replace x i w
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   let xx = V.copy x in
    let xi = V.raw_extract i xx in
     V.replace ( Coeff.sub xi y ) i xx ;
     tensor_row_replace xx i w
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let xx = V.copy x
   and d = T.dimensions w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Index.to_int d.(1) )
    and shift = ii - h_a_l in
     for j = max 0 shift to min ( ii + h_a_l ) cc do
      let jj = Index.from_int j in
       let xj = V.raw_extract jj xx in
        V.replace ( Coeff.sub xj a.( j - shift ) ) jj xx ;
     done ;
     tensor_row_replace xx i w
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let xx = V.copy x in
    let xi = V.raw_extract i xx in
     V.replace ( Coeff.sub xi a.( Index.to_int i ) ) i xx ;
     tensor_row_replace xx i w
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let xx = V.copy x
   and d = T.dimensions w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Index.to_int d.(1) )
    and shift = ii - h_a_l in
     for j = max 0 shift to min ( ii + h_a_l ) cc do
      let jj = Index.from_int j in
       let xj = V.raw_extract jj xx in
        V.replace ( Coeff.sub xj a.( j - shift ).(ii) ) jj xx ;
     done ;
     tensor_row_replace xx i w
  end ;;


(** {v column_replace vector index matrix v} This function is applied in place.

Cette fonction est exécutée en place. *)
let column_replace = fun (x:V.t) (i:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let f = function ( j , y ) -> V.replace y i w.( Index.to_int j ) in
    V.iter f x
  end
 | Sparse_tensor_matrix w -> tensor_column_replace x i w
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   let xx = V.copy x in
    let xi = V.raw_extract i xx in
     V.replace ( Coeff.sub xi y ) i xx ;
     tensor_column_replace xx i w
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let xx = V.copy x
   and d = T.dimensions w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Index.to_int d.(1) )
    and shift = ii - h_a_l
    and bound = ii + h_a_l in
     for j = max 0 shift to min bound cc do
      let jj = Index.from_int j in
       let xj = V.raw_extract jj xx in
        V.replace ( Coeff.sub xj a.( bound - j ) ) jj xx ;
     done ;
     tensor_column_replace xx i w
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let xx = V.copy x in
    let xi = V.raw_extract i xx in
     V.replace ( Coeff.sub xi a.( Index.to_int i ) ) i xx ;
     tensor_column_replace xx i w
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let xx = V.copy x
   and d = T.dimensions w
   and ii = Index.to_int i
   and h_a_l = ( Array.length a ) / 2 in
    let rr = pred ( Index.to_int d.(0) )
    and shift = ii - h_a_l
    and bound = ii + h_a_l in
     for j = max 0 shift to min bound rr do
      let jj = Index.from_int j in
       let xj = V.raw_extract jj xx in
        V.replace ( Coeff.sub xj a.( bound - j ).(j) ) jj xx ;
     done ;
     tensor_column_replace xx i w
  end ;;

(** {v replace coefficient row column matrix v} *)
let replace = fun (x:coeff) (i:index) (j:index) (m:t) ->
 match m with
 | Half_full_matrix w -> V.replace x j w.( Index.to_int i )
 | Sparse_tensor_matrix w -> T.replace x [| i ; j |] w
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   if Index.eq i j then
    T.replace ( Coeff.sub x y ) ( Array.make 2 i ) w
   else
    T.replace x [| i ; j |] w ;
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let h_a_l = ( Array.length a ) / 2
   and diff = Index.to_int ( Index.sub i j ) in
    if abs diff <= h_a_l then
     T.replace ( Coeff.sub x a.( h_a_l + diff ) ) [| i ; j |] w
    else
     T.replace x [| i ; j |] w ;
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   if Index.eq i j then
    begin
     a.( Index.to_int i ) <- x ;
     T.remove ( Array.make 2 i ) w ;
    end
   else
    T.replace x [| i ; j |] w ;
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let h_a_l = ( Array.length a ) / 2
   and diff = Index.to_int ( Index.sub i j ) in
    if abs diff <= h_a_l then
     begin
      try
       begin
        a.( h_a_l + diff ).( Index.to_int i ) <- x ;
        T.remove [| i ; j |] w
       end
      with _ ->
       T.replace x [| i ; j |] w
     end
    else
     T.replace x [| i ; j |] w ;
  end ;;

(** {v tensor_row_insert_add vector index matrix v} *)
let tensor_row_insert_add = fun (x:V.t) (i:index) (w:T.t) ->
 let row = tensor_row_extract i w in
  let y = V.add x row in
   tensor_row_replace y i w ;;

(** {v tensor_column_insert_add vector index matrix v} *)
let tensor_column_insert_add = fun (x:V.t) (i:index) (w:T.t) ->
 let column = tensor_column_extract i w in
  let y = V.add x column in
   tensor_column_replace y i w ;;

(** {v row_insert_add vector index matrix v} *)
let row_insert_add = fun (x:V.t) (i:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let ii = Index.to_int i in
    w.(ii) <- V.add x w.(ii)
  end
 | Sparse_tensor_matrix w -> tensor_row_insert_add x i w
 | Diff_to_scal_matrix ( y , w ) -> tensor_row_insert_add x i w
 | Diff_to_multi_scal_matrix ( y , w ) -> tensor_row_insert_add x i w
 | Diff_to_diag_matrix ( a , w ) -> tensor_row_insert_add x i w
 | Diff_to_multi_diag_matrix ( a , w ) -> tensor_row_insert_add x i w ;;

(** {v column_insert_add vector index matrix v} *)
let column_insert_add = fun (x:V.t) (i:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let f = function ( j , y ) -> V.insert_add y i w.( Index.to_int j ) in
    V.iter f x
  end
 | Sparse_tensor_matrix w -> tensor_column_insert_add x i w
 | Diff_to_scal_matrix ( y , w ) -> tensor_column_insert_add x i w
 | Diff_to_multi_scal_matrix ( y , w ) -> tensor_column_insert_add x i w
 | Diff_to_diag_matrix ( a , w ) -> tensor_column_insert_add x i w
 | Diff_to_multi_diag_matrix ( a , w ) -> tensor_column_insert_add x i w ;;

(** {v insert_add coefficient row column matrix v} *)
let insert_add = fun (x:coeff) (i:index) (j:index) (m:t) ->
 match m with
 | Half_full_matrix w -> V.insert_add x j w.( Index.to_int i )
 | Sparse_tensor_matrix w -> T.insert_add x [| i ; j |] w
 | Diff_to_scal_matrix ( y , w ) -> T.insert_add x [| i ; j |] w
 | Diff_to_multi_scal_matrix ( a , w ) -> T.insert_add x [| i ; j |] w
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   if Index.eq i j then
    begin
     let ii = Index.to_int i in
      a.(ii) <- Coeff.add x a.(ii)
    end
   else
    T.insert_add x [| i ; j |] w
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let h_a_l = ( Array.length a ) / 2
   and diff = Index.to_int ( Index.sub i j ) in
    if abs diff <= h_a_l then
     begin
      try
       begin
        let ii = Index.to_int i
        and jj = h_a_l + diff in
         a.(jj).(ii) <- Coeff.add x a.(jj).(ii)
       end
      with _ ->
       T.insert_add x [| i ; j |] w
     end
    else
     T.insert_add x [| i ; j |] w
  end ;;

(** {v tensor_row_insert_sub vector index matrix v} *)
let tensor_row_insert_sub = fun (x:V.t) (i:index) (w:T.t) ->
 let row = tensor_row_extract i w in
  let y = V.sub row x in
   tensor_row_replace y i w ;;

(** {v tensor_column_insert_sub vector index matrix v} *)
let tensor_column_insert_sub = fun (x:V.t) (i:index) (w:T.t) ->
 let column = tensor_column_extract i w in
  let y = V.sub column x in
   tensor_column_replace y i w ;;

(** {v row_insert_sub vector index matrix v} *)
let row_insert_sub = fun (x:V.t) (i:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let ii = Index.to_int i in
    w.(ii) <- V.sub w.(ii) x
  end
 | Sparse_tensor_matrix w -> tensor_row_insert_sub x i w
 | Diff_to_scal_matrix ( y , w ) -> tensor_row_insert_sub x i w
 | Diff_to_multi_scal_matrix ( y , w ) -> tensor_row_insert_sub x i w
 | Diff_to_diag_matrix ( a , w ) -> tensor_row_insert_sub x i w
 | Diff_to_multi_diag_matrix ( a , w ) -> tensor_row_insert_sub x i w ;;

(** {v column_insert_sub vector index matrix v} *)
let column_insert_sub = fun (x:V.t) (i:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let f = function ( j , y ) -> V.insert_sub y i w.( Index.to_int j ) in
    V.iter f x
  end
 | Sparse_tensor_matrix w -> tensor_column_insert_sub x i w
 | Diff_to_scal_matrix ( y , w ) -> tensor_column_insert_sub x i w
 | Diff_to_multi_scal_matrix ( y , w ) -> tensor_column_insert_sub x i w
 | Diff_to_diag_matrix ( a , w ) -> tensor_column_insert_sub x i w
 | Diff_to_multi_diag_matrix ( a , w ) -> tensor_column_insert_sub x i w ;;

(** {v insert_sub coefficient row column matrix v} *)
let insert_sub = fun (x:coeff) (i:index) (j:index) (m:t) ->
 match m with
 | Half_full_matrix w -> V.insert_sub x j w.( Index.to_int i )
 | Sparse_tensor_matrix w -> T.insert_sub x [| i ; j |] w
 | Diff_to_scal_matrix ( y , w ) -> T.insert_sub x [| i ; j |] w
 | Diff_to_multi_scal_matrix ( a , w ) -> T.insert_sub x [| i ; j |] w
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   if Index.eq i j then
    begin
     let ii = Index.to_int i in
      a.(ii) <- Coeff.sub a.(ii) x
    end
   else
    T.insert_sub x [| i ; j |] w
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let h_a_l = ( Array.length a ) / 2
   and diff = Index.to_int ( Index.sub i j ) in
    if abs diff <= h_a_l then
     begin
      try
       begin
        let ii = Index.to_int i
        and jj = h_a_l + diff in
         a.(jj).(ii) <- Coeff.sub a.(jj).(ii) x
       end
      with _ ->
       T.insert_sub x [| i ; j |] w
     end
    else
     T.insert_sub x [| i ; j |] w
  end ;;

(** {v tensor_diag_extract tensor v} *)
let tensor_diag_extract = function (w:T.t) ->
 let ( e , t , v ) = T.flat_tensor_demakeup w
 and ensemble = ref ( T.M.H.B.empty () ) in
  let dim = T.M.dimension v
  and table = ( snd v ).T.M.H.data
  and rows = t.(0) in
   let d = Index.min dim.(0) dim.(1)
   and s = Array.length table in
    let diag = V.null d
    and f = function ( index , coefficient ) -> ensemble := T.M.H.B.union !ensemble table.( ( T.Multi_hash.raw_extract ( Array.make 2 index ) coefficient ) mod s ) in
     T.Info.iter f rows ;
     let g = function ( i , x ) ->
      begin
       let ii = i.(0) in
        if Index.eq ii i.(1) then
         V.insert_add x ii diag
      end in
      T.M.H.B.iter g !ensemble ;
      diag ;;

(** {v sparse_diag_extract tensor v} *)
let sparse_diag_extract = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let dim = dimensions m in
    let d = Index.min dim.(0) dim.(1) in
     let diag = V.null d in
      for i = 0 to pred ( Array.length w ) do
       let ii = Index.from_int i in
        V.insert_add ( snd ( V.extract ii w.(i) ) ) ii diag
      done ;
      diag
  end
 | Sparse_tensor_matrix w -> tensor_diag_extract w
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   if Coeff.eq_zero y then
    tensor_diag_extract w
   else
    failwith "Unadapted format in Sparse_matrix.Rng.sparse_diag_extract."
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   if Coeff.eq_zero a.( ( Array.length a ) / 2 ) then
    tensor_diag_extract w
   else
    failwith "Unadapted format in Sparse_matrix.Rng.sparse_diag_extract."
  end
 | _ -> failwith "Unadapted format in Sparse_matrix.Rng.sparse_diag_extract."

(** {v full_diag_extract tensor v} *)
let full_diag_extract = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let dim = dimensions m in
    let d = Index.min dim.(0) dim.(1) in
     let dd = Index.to_int d in
      let diag = Array.map Coeff.zero ( Array.make dd () ) in
       for i = 0 to pred dd do
        let ii = Index.from_int i in
         diag.(i) <- snd ( V.extract ii w.(i) )
       done ;
       diag
  end
 | Sparse_tensor_matrix w -> V.to_full ( tensor_diag_extract w )
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   let diag = V.to_full ( tensor_diag_extract w ) in
    Array.map ( Coeff.add y ) diag
  end
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let diag = V.to_full ( tensor_diag_extract w ) in
    Array.map ( Coeff.add a.( ( Array.length a ) / 2 ) ) diag
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let diag = V.to_full ( tensor_diag_extract w )
   and f = fun i x -> Coeff.add a.(i) x in
    Array.mapi f diag
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let diag = V.to_full ( tensor_diag_extract w )
   and f = fun i x -> Coeff.add a.( ( Array.length a ) / 2 ).(i) x in
    Array.mapi f diag
  end ;;

(** {v tensor_diag_isolate tensor v} *)
let tensor_diag_isolate = function (w:T.t) ->
 let ( e , t , v ) = T.flat_tensor_demakeup w
 and ensemble = ref ( T.M.H.B.empty () ) in
  let dim = T.M.dimension v
  and table = ( snd v ).T.M.H.data
  and rows = t.(0) in
   let result = T.null dim
   and s = Array.length table in
    let f = function ( index , coefficient ) -> ensemble := T.M.H.B.union !ensemble table.( ( T.Multi_hash.raw_extract ( Array.make 2 index ) coefficient ) mod s ) in
     T.Info.iter f rows ;
     let g = function ( i , x ) -> if Index.eq i.(0) i.(1) then T.insert_add x i result in
      T.M.H.B.iter g !ensemble ;
      result ;;

(** {v diag_isolate matrix v} *)
let diag_isolate = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let r = Array.length w
   and d = V.dimension w.(0) in
    let dd = Array.make r d in
     let accu = Array.map V.null dd in
      for i = 0 to pred ( min r ( Index.to_int d ) ) do
       let ii = Index.from_int i in
        try
         V.insert_add ( snd ( V.unsafe_extract ii w.(i) ) ) ii accu.(i)
        with _ ->
         ()
      done ;
      Half_full_matrix accu
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_diag_isolate w )
 | Diff_to_scal_matrix ( y , w ) -> Diff_to_scal_matrix ( y , tensor_diag_isolate w )
 | Diff_to_multi_scal_matrix ( a , w ) -> Diff_to_scal_matrix ( a.( ( Array.length a ) / 2 ) , tensor_diag_isolate w )
 | Diff_to_diag_matrix ( a , w ) -> Diff_to_diag_matrix ( a , tensor_diag_isolate w )
 | Diff_to_multi_diag_matrix ( a , w ) -> Diff_to_diag_matrix ( a.( ( Array.length a ) / 2 ) , tensor_diag_isolate w ) ;;

(** {v tensor_out_diag_isolate tensor v} *)
let tensor_out_diag_isolate = function (w:T.t) ->
 let result = T.null ( T.dimensions w ) in
  let f = function ( i , x ) ->
   begin
    if not ( Index.eq i.(0) i.(1) ) then T.insert_add x i result
   end in
   T.iter f w ;
   result ;;

(** {v tensor_upper_diag_isolate tensor v} *)
let tensor_upper_diag_isolate = function (w:T.t) ->
 let result = T.null ( T.dimensions w ) in
  let f = function ( i , x ) ->
   begin
    if Index.compare i.(0) i.(1) < 0 then T.insert_add x i result
   end in
   T.iter f w ;
   result ;;

(** {v tensor_lower_diag_isolate tensor v} *)
let tensor_lower_diag_isolate = function (w:T.t) ->
 let result = T.null ( T.dimensions w ) in
  let f = function ( i , x ) ->
   begin
    if Index.compare i.(0) i.(1) > 0 then T.insert_add x i result
   end in
   T.iter f w ;
   result ;;

(** {v out_diag_isolate matrix v} *)
let out_diag_isolate = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let r = Array.length w
   and d = V.dimension w.(0) in
    let accu = Array.map V.copy w in
     for i = 0 to pred ( min r ( Index.to_int d ) ) do
      let ii = Index.from_int i in
       V.remove ii accu.(i)
     done ;
     Half_full_matrix accu
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_out_diag_isolate w )
 | Diff_to_scal_matrix ( y , w ) -> Sparse_tensor_matrix ( tensor_out_diag_isolate w )
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   if Array.length a = 1 then
    Sparse_tensor_matrix ( tensor_out_diag_isolate w )
   else
    let b = Array.map Coeff.copy a in
     b.( ( Array.length a ) / 2 ) <- Coeff.zero () ;
     Diff_to_multi_scal_matrix ( b , tensor_out_diag_isolate w )
  end
 | Diff_to_diag_matrix ( a , w ) -> Sparse_tensor_matrix ( tensor_out_diag_isolate w )
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   if Array.length a = 1 then
    Sparse_tensor_matrix ( tensor_out_diag_isolate w )
   else
    let b = Array.map ( Array.map Coeff.copy ) a in
     b.( ( Array.length a ) / 2 ) <- Array.map Coeff.zero ( Array.make ( Array.length a.(0) ) () ) ;
     Diff_to_multi_diag_matrix ( b , tensor_out_diag_isolate w )
  end ;;

(** {v upper_diag_isolate matrix v} *)
let upper_diag_isolate = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let r = Array.length w
   and d = V.dimension w.(0) in
    let accu = Array.map V.copy w in
     for i = 0 to pred ( min r ( Index.to_int d ) ) do
      let ii = Index.from_int ( succ i ) in
       accu.(i) <- V.ending ii w.(i)
     done ;
     Half_full_matrix accu
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_upper_diag_isolate w )
 | Diff_to_scal_matrix ( y , w ) -> Sparse_tensor_matrix ( tensor_upper_diag_isolate w )
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   if Array.length a = 1 then
    Sparse_tensor_matrix ( tensor_upper_diag_isolate w )
   else
    let h_a_l = ( Array.length a ) / 2 in
     let s_h_a_l = succ h_a_l in
      let aa = Array.map Coeff.zero ( Array.make s_h_a_l () )
      and aaa = Array.map Coeff.copy ( Array.sub a s_h_a_l h_a_l ) in
       let b = Array.append aa aaa in
        Diff_to_multi_scal_matrix ( b , tensor_upper_diag_isolate w )
  end
 | Diff_to_diag_matrix ( a , w ) -> Sparse_tensor_matrix ( tensor_upper_diag_isolate w )
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   if Array.length a = 1 then
    Sparse_tensor_matrix ( tensor_upper_diag_isolate w )
   else
    let h_a_l = ( Array.length a ) / 2 in
     let s_h_a_l = succ h_a_l in
      let aa = Array.map ( Array.map Coeff.zero ) ( Array.make_matrix s_h_a_l ( Array.length a.(0) ) () )
      and aaa = Array.map ( Array.map Coeff.copy ) ( Array.sub a s_h_a_l h_a_l ) in
       let b = Array.append aa aaa in
        Diff_to_multi_diag_matrix ( b , tensor_upper_diag_isolate w )
  end ;;

(** {v lower_diag_isolate matrix v} *)
let lower_diag_isolate = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let r = Array.length w
   and d = V.dimension w.(0) in
    let accu = Array.map V.copy w in
     for i = 0 to pred ( min r ( Index.to_int d ) ) do
      let ii = Index.from_int ( pred i ) in
       accu.(i) <- V.beginning ii w.(i)
     done ;
     Half_full_matrix accu
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_lower_diag_isolate w )
 | Diff_to_scal_matrix ( y , w ) -> Sparse_tensor_matrix ( tensor_lower_diag_isolate w )
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   if Array.length a = 1 then
    Sparse_tensor_matrix ( tensor_lower_diag_isolate w )
   else
    let h_a_l = ( Array.length a ) / 2 in
     let s_h_a_l = succ h_a_l in
      let aa = Array.map Coeff.copy ( Array.sub a 0 h_a_l )
      and aaa = Array.map Coeff.zero ( Array.make s_h_a_l () ) in
       let b = Array.append aa aaa in
        Diff_to_multi_scal_matrix ( b , tensor_lower_diag_isolate w )
  end
 | Diff_to_diag_matrix ( a , w ) -> Sparse_tensor_matrix ( tensor_lower_diag_isolate w )
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   if Array.length a = 1 then
    Sparse_tensor_matrix ( tensor_lower_diag_isolate w )
   else
    let h_a_l = ( Array.length a ) / 2 in
     let s_h_a_l = succ h_a_l in
      let aa = Array.map ( Array.map Coeff.copy ) ( Array.sub a 0 h_a_l )
      and aaa = Array.map ( Array.map Coeff.zero ) ( Array.make_matrix s_h_a_l ( Array.length a.(0) ) () ) in
       let b = Array.append aa aaa in
        Diff_to_multi_diag_matrix ( b , tensor_lower_diag_isolate w )
  end ;;




(** {C § } *)
(** 
{3 Coercitions}
*)
(** {C  } *)




(** These functions are not sealed.

Ces fonctions ne sont pas étanches. *)


(** {v to_half_full matrix v} *)
let to_half_full = function (m:t) ->
 let d = dimensions m
 in
  try
   begin
    match m with
    | Half_full_matrix w -> m
    | Sparse_tensor_matrix w ->
     begin
      let r = Index.to_int d.(0) in
       let dd = Array.make r d.(1) in
        let accu = Array.map V.null dd in
         let f = function ( i , y ) -> V.insert_add y i.(1) accu.( Index.to_int i.(0) ) in
          T.iter f w ;
          Half_full_matrix accu
     end
    | Diff_to_scal_matrix ( x , w ) ->
     begin
      let r = Index.to_int d.(0) in
       let dd = Array.make r d.(1) in
        let accu = Array.map V.null dd in
         let f = function ( i , y ) -> V.insert_add y i.(1) accu.( Index.to_int i.(0) ) in
          T.iter f w ;
          for i = 0 to pred r do
           V.insert_add x ( Index.from_int i ) accu.(i)
          done ;
          Half_full_matrix accu
     end
    | Diff_to_multi_scal_matrix ( a , w ) ->
     begin
      let r = Index.to_int d.(0)
      and c = Index.to_int d.(1)
      and alength = Array.length a in
       if alength mod 2 = 0 then failwith "Bad multi-scalar dimension in Sparse_matrix.Rng.to_half_full." ;
       let dd = Array.make r d.(1)
       and cc = pred c
       and h_a_l = alength / 2 in
        let accu = Array.map V.null dd in
         let f = function ( i , x ) -> V.insert_add x i.(1) accu.( Index.to_int i.(0) ) in
          T.iter f w ;
          for i = 0 to pred r do
           let row = accu.(i)
           and i_i = i - h_a_l
           and ii = i + h_a_l in
            for j = max 0 i_i to min ii cc do
             let jj = j - i_i in
              V.insert_add a.(jj) ( Index.from_int j ) row
            done
          done ;
          Half_full_matrix accu
     end
    | Diff_to_diag_matrix ( a , w ) ->
     begin
      let r = Index.to_int d.(0) in
       let dd = Array.make r d.(1) in
        let accu = Array.map V.null dd in
         let f = function ( i , y ) -> V.insert_add y i.(1) accu.( Index.to_int i.(0) ) in
          T.iter f w ;
          for i = 0 to pred ( min ( Array.length a ) r ) do
           V.insert_add a.(i) ( Index.from_int i ) accu.(i)
          done ;
          Half_full_matrix accu
     end
    | Diff_to_multi_diag_matrix ( a , w ) ->
     begin
      let r = Index.to_int d.(0)
      and c = Index.to_int d.(1)
      and alength = Array.length a in
       if alength mod 2 = 0 then failwith "Bad multi-diagonal dimension in Sparse_matrix.Rng.to_half_full." ;
       let dd = Array.make r d.(1)
       and cc = pred c
       and h_a_l = alength / 2 in
        let accu = Array.map V.null dd in
         let f = function ( i , y ) -> V.insert_add y i.(1) accu.( Index.to_int i.(0) ) in
          T.iter f w ;
          for i = 0 to pred r do
           let row = accu.(i)
           and i_i = i - h_a_l
           and ii = i + h_a_l in
            for j = max 0 i_i to min ii cc do
             let jj = j - i_i in
              V.insert_add a.(jj).(i) ( Index.from_int j ) row
            done
          done ;
          Half_full_matrix accu
     end
   end
  with _ ->
   let dd = Index.to_int d.(0) in
    Half_full_matrix ( Array.map V.null ( Array.make dd d.(1) ) ) ;;

(** {v to_sparse_tensor matrix v} *)
let to_sparse_tensor = function (mm:t) -> 
 let m = copy mm in
  match m with
  | Sparse_tensor_matrix w -> m
  | Diff_to_scal_matrix ( x , w ) ->
   begin
    let d = T.dimensions w in
     for i = 0 to pred ( min ( Index.to_int d.(0) ) ( Index.to_int d.(1) ) ) do
      let j = Index.from_int i in
       T.insert_add x ( Array.make 2 j ) w
     done ;
     Sparse_tensor_matrix w
   end
  | Diff_to_multi_scal_matrix ( a , w ) ->
   begin
    let d = T.dimensions w
    and alength = Array.length a in
     if alength mod 2 = 0 then failwith "Bad multi-scalar dimension in Sparse_matrix.Rng.to_half_full." ;
     let r = Index.to_int d.(0)
     and c = Index.to_int d.(1)
     and h_a_l = alength / 2 in
      let cc = pred c in
       for i = 0 to pred r do
        let i_i = i - h_a_l
        and ii = i + h_a_l in
         for j = max 0 i_i to min ii cc do
          let jj = j - i_i in
           T.insert_add a.(jj) [| Index.from_int i ; Index.from_int j |] w
         done ;
       done ;
       Sparse_tensor_matrix w
   end
  | Half_full_matrix w ->
   begin
    let u = T.null ( dimensions m )
    and r = Array.length w in
     let f = fun k ( j , y ) -> T.insert_add y [| Index.from_int k ; j |] u in
      for i = 0 to pred r do
       V.iter ( f i ) w.(i)
      done ;
      Sparse_tensor_matrix u
   end
  | Diff_to_diag_matrix ( a , w ) ->
   begin
    let d = T.dimensions w in
     for i = 0 to pred ( min ( Index.to_int d.(0) ) ( Index.to_int d.(1) ) ) do
      let j = Index.from_int i in
       T.insert_add a.(i) ( Array.make 2 j ) w
     done ;
     Sparse_tensor_matrix w
   end
  | Diff_to_multi_diag_matrix ( a , w ) ->
   begin
    let d = T.dimensions w
    and alength = Array.length a in
     if alength mod 2 = 0 then failwith "Bad multi-scalar dimension in Sparse_matrix.Rng.to_half_full." ;
     let r = Index.to_int d.(0)
     and c = Index.to_int d.(1)
     and h_a_l = alength / 2 in
      let cc = pred c in
       for i = 0 to pred r do
        let i_i = i - h_a_l
        and ii = i + h_a_l in
         for j = max 0 i_i to min ii cc do
          let jj = j - i_i in
           T.insert_add a.(jj).(i) [| Index.from_int i ; Index.from_int j |] w
         done ;
       done ;
       Sparse_tensor_matrix w
   end ;;

(** {v to_diff_to_scal matrix v} *)
let rec to_diff_to_scal = function (mmm:t) ->
 let m = copy mmm in
  match m with
  | Sparse_tensor_matrix w -> Diff_to_scal_matrix ( Coeff.zero () , w )
  | Diff_to_scal_matrix ( x , w ) -> m
  | Diff_to_multi_scal_matrix ( a , w ) ->
   begin
    let r = Array.length a in
     let rr = r / 2 in
      let x = a.(rr) in
       a.(rr) <- Coeff.zero () ;
       let mm = to_sparse_tensor ( Diff_to_multi_scal_matrix ( a , w ) ) in
        let ww = sparse_tensor_matrix_demakeup mm in
         Diff_to_scal_matrix ( x , ww ) 
   end
  | Half_full_matrix w -> to_diff_to_scal ( to_sparse_tensor m )
  | Diff_to_diag_matrix ( a , w ) -> to_diff_to_scal ( to_sparse_tensor m )
  | Diff_to_multi_diag_matrix ( a , w ) -> to_diff_to_scal ( to_sparse_tensor m ) ;;

(** {v to_diff_to_multi_scal matrix v} *)
let rec to_diff_to_multi_scal = function (mm:t) ->
 let m = copy mm in
  match m with
  | Sparse_tensor_matrix w -> Diff_to_multi_scal_matrix ( [| Coeff.zero () |], w )
  | Diff_to_scal_matrix ( x , w ) -> Diff_to_multi_scal_matrix ( [| x |], w )
  | Diff_to_multi_scal_matrix ( a , w ) -> m
  | Half_full_matrix w -> to_diff_to_multi_scal ( to_diff_to_scal m )
  | Diff_to_diag_matrix ( a , w ) -> to_diff_to_multi_scal ( to_diff_to_scal m )
  | Diff_to_multi_diag_matrix ( a , w ) -> to_diff_to_multi_scal ( to_diff_to_scal m ) ;;

(** {v to_diff_to_diag matrix v} *)
let rec to_diff_to_diag = function (mm:t) ->
 let m = copy mm in
  match m with
  | Sparse_tensor_matrix w ->
   begin
    let r = Index.to_int ( T.dimensions w ).(0) in
     let a = Array.map Coeff.zero ( Array.make r () ) in
      Diff_to_diag_matrix ( a , w )
   end
  | Diff_to_scal_matrix ( x , w ) ->
   begin
    let r = Index.to_int ( T.dimensions w ).(0) in
     let a = Array.map Coeff.copy ( Array.make r x ) in
      Diff_to_diag_matrix ( a , w )
   end
  | Diff_to_multi_scal_matrix ( a , w ) ->
   begin
    let r = Array.length a
    and d = Index.to_int ( T.dimensions w ).(0) in
     let rr = r / 2 in
      let x = a.(rr) in
       a.(rr) <- Coeff.zero () ;
       let mm = to_sparse_tensor ( Diff_to_multi_scal_matrix ( a , w ) )
       and b = Array.map Coeff.copy ( Array.make d x ) in
        let ww = sparse_tensor_matrix_demakeup mm in
         Diff_to_diag_matrix ( b , ww ) 
   end
  | Half_full_matrix w -> to_diff_to_diag ( to_sparse_tensor m )
  | Diff_to_diag_matrix ( a , w ) -> m
  | Diff_to_multi_diag_matrix ( a , w ) ->
   begin
    let r = Array.length a
    and d = Index.to_int ( T.dimensions w ).(0) in
     let rr = r / 2 in
      let b = a.(rr) in
       a.(rr) <- Array.map Coeff.zero ( Array.make d () ) ;
       let mm = to_sparse_tensor ( Diff_to_multi_diag_matrix ( a , w ) ) in
        let ww = sparse_tensor_matrix_demakeup mm in
         Diff_to_diag_matrix ( b , ww ) 
   end ;;

(** {v to_diff_to_multi_diag matrix v} *)
let rec to_diff_to_multi_diag = function (mm:t) ->
 let m = copy mm in
  match m with
  | Sparse_tensor_matrix w ->
   begin
    let r = Index.to_int ( T.dimensions w ).(0) in
     let a = [| Array.map Coeff.zero ( Array.make r () ) |] in
      Diff_to_multi_diag_matrix ( a , w )
   end
  | Diff_to_scal_matrix ( x , w ) ->
   begin
    let r = Index.to_int ( T.dimensions w ).(0) in
     let a = [| Array.map Coeff.copy ( Array.make r x ) |] in
      Diff_to_multi_diag_matrix ( a , w )
   end
  | Diff_to_multi_scal_matrix ( a , w ) ->
   begin
    let d = Index.to_int ( T.dimensions w ).(0) in
     let b = Array.map ( Array.make d ) a in
      Diff_to_multi_diag_matrix ( b , w ) 
   end
  | Half_full_matrix w -> to_diff_to_multi_diag ( to_sparse_tensor m )
  | Diff_to_diag_matrix ( a , w ) -> Diff_to_multi_diag_matrix ( [| a |] , w ) 
  | Diff_to_multi_diag_matrix ( a , w ) -> m ;;

(** {v half_full_matrix_to_full matrix v} *)
let half_full_matrix_to_full = function (m:t) ->
 match m with
 | Half_full_matrix w -> Array.map ( V.to_full ) w
 | _ -> failwith "Not a half full matrix in Sparse_matrix.Rng.half_full_matrix_to_full." ;;

(** {v sparse_tensor_to_full_matrix tensor v} *)
let sparse_tensor_to_full_matrix = function (w:T.t) ->
 let d = T.dimensions w in
  let m = Array.map ( Array.map Coeff.zero ) ( Array.make_matrix ( Index.to_int d.(0) ) ( Index.to_int d.(1) ) () ) in
   let f = function ( i , x ) -> ( m.( Index.to_int i.(0) ).( Index.to_int i.(1) ) <- x ) in
    T.iter f w ;
    m ;;

(** {v sparse_tensor_matrix_to_full matrix v} *)
let sparse_tensor_matrix_to_full = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> sparse_tensor_to_full_matrix w
 | _ -> failwith "Not a sparse tensor matrix in Sparse_matrix.Rng.sparse_tensor_matrix_to_full." ;;

(** {v diff_to_scal_matrix_to_full matrix v} *)
let diff_to_scal_matrix_to_full = function (m:t) ->
 match m with
 | Diff_to_scal_matrix ( x , w ) ->
  begin
   let m = sparse_tensor_to_full_matrix w in
    for i = 0 to pred ( min ( Array.length m ) ( Array.length m.(0) ) ) do
     m.(i).(i) <- Coeff.add m.(i).(i) x ;
    done ;
    m
  end
 | _ -> failwith "Not a difference-to-scalar matrix in Sparse_matrix.Rng.diff_to_scal_matrix_to_full." ;;

(** {v diff_to_multi_scal_matrix_to_full matrix v} *)
let diff_to_multi_scal_matrix_to_full = function (m:t) ->
 match m with
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   let m = sparse_tensor_to_full_matrix w
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Array.length m.(0) ) in
     for i = 0 to pred ( Array.length m ) do
      let row = m.(i)
      and ii = i - h_a_l in
       for j = max 0 ii to min cc ( i + h_a_l ) do
        row.(j) <- Coeff.add row.(j) a.( j - ii ) ;
       done ;
     done ;
     m
  end
 | _ -> failwith "Not a difference-to-multi-scalar matrix in Sparse_matrix.Rng.diff_to_multi_scal_matrix_to_full." ;;

(** {v diff_to_diag_matrix_to_full matrix v} *)
let diff_to_diag_matrix_to_full = function (m:t) ->
 match m with
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let m = sparse_tensor_to_full_matrix w in
    for i = 0 to pred ( min ( Array.length m ) ( Array.length m.(0) ) ) do
     m.(i).(i) <- Coeff.add m.(i).(i) a.(i) ;
    done ;
    m
  end
 | _ -> failwith "Not a difference-to-diagonal matrix in Sparse_matrix.Rng.diff_to_diag_matrix_to_full." ;;

(** {v diff_to_multi_diag_matrix_to_full matrix v} *)
let diff_to_multi_diag_matrix_to_full = function (m:t) ->
 match m with
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let m = sparse_tensor_to_full_matrix w
   and h_a_l = ( Array.length a ) / 2 in
    let cc = pred ( Array.length m.(0) ) in
     for i = 0 to pred ( Array.length m ) do
      let row = m.(i)
      and ii = i - h_a_l in
       for j = max 0 ii to min cc ( i + h_a_l ) do
        row.(j) <- Coeff.add row.(j) a.( j - ii ).(i) ;
       done ;
     done ;
     m
  end
 | _ -> failwith "Not a difference-to-multi-diagonal matrix in Sparse_matrix.Rng.diff_to_multi_diag_matrix_to_full." ;;

(** {v to_full matrix v} *)
let to_full = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> sparse_tensor_matrix_to_full m
 | Half_full_matrix w -> half_full_matrix_to_full m
 | Diff_to_scal_matrix ( x , w ) -> diff_to_scal_matrix_to_full m
 | Diff_to_multi_scal_matrix ( a , w ) -> diff_to_multi_scal_matrix_to_full m
 | Diff_to_diag_matrix ( a , w ) -> diff_to_diag_matrix_to_full m
 | Diff_to_multi_diag_matrix ( a , w ) -> diff_to_multi_diag_matrix_to_full m ;;

(** {v to_sparse size threshold matrix v} *)
let to_sparse = fun (size:int) (threshold:float) (m:coeff array array) ->
 let r = Array.length m
 and c = Array.length m.(0)
 and filling_diag = ref 0
 and filling_rows = ref 0 in
  let cc = Index.from_int c
  and tt = threshold *. ( float r )
  and wide = r <= c
  and rr = pred r
  and result = ref ( T.null [| Index.from_int r ; Index.from_int c |] )
  and diag = Array.map Coeff.zero ( Array.make r () ) in
   let with_diag = Array.map V.null ( Array.make r cc )
   and marked_rows = ref [] in
    for i = 0 to rr do
     let row_input = m.(i) in
      let row_output = V.to_sparse size row_input in
       if V.filling row_output > 0 then
        marked_rows := i :: !marked_rows ;
       with_diag.(i) <- row_output ;
       if not ( V.eq_zero row_output ) then
        incr filling_rows ;
       if wide then
        begin
         let x = row_input.(i) in
          if not ( Coeff.eq_zero x ) then
           begin
            diag.(i) <- x ;
            incr filling_diag ;
           end ;
        end
    done ;
    let diag_is_small = ( not wide ) || ( tt > ( float !filling_diag ) ) in
     if diag_is_small then
      begin
       let few_rows = tt > ( float !filling_rows ) in
        if few_rows then
         begin
          let f = fun i ( j , y ) -> T.insert_add y [| Index.from_int i ; j |] !result in
           while Util.list_non_empty !marked_rows do
            let i = List.hd !marked_rows in
             V.iter ( f i ) with_diag.(i) ;
             marked_rows := List.tl !marked_rows ;
           done ;
           Sparse_tensor_matrix !result
         end
        else
         Half_full_matrix with_diag
      end
     else
      begin
       let f = fun i ( j , y ) -> T.insert_add y [| Index.from_int i ; j |] !result in
        while Util.list_non_empty !marked_rows do
         let i = List.hd !marked_rows in
          V.remove ( Index.from_int i ) with_diag.(i) ;
          V.iter ( f i ) with_diag.(i) ;
          marked_rows := List.tl !marked_rows ;
        done ;
        Diff_to_diag_matrix ( diag , !result )
      end ;;

(** {v auto_to_sparse threshold matrix v} *)
let auto_to_sparse = fun (threshold:float) (m:coeff array array) ->
 to_sparse (-1) threshold m ;;

(** {v sparse_vector_to_line_matrix vector v} *)
let sparse_vector_to_line_matrix = function (v:V.t) ->
 Half_full_matrix [| v |] ;;


(** {v sparse_vector_to_square_matrix vector v} The vector is placed in the first row.

Le vecteur est placé dans la première ligne. *)
let sparse_vector_to_square_matrix = function (v:V.t) ->
 let result = T.null ( Array.make 2 ( V.dimension v ) ) in
  let f = function ( i , x ) -> T.insert_add x [| Index.zero () ; i |] result in
   V.iter f v ;
   Sparse_tensor_matrix result ;;



(** {v full_vector_to_square_matrix vector v} The vector is placed in the first row.

Le vecteur est placé dans la première ligne. *)
let full_vector_to_square_matrix = function (v:coeff array) ->
 let r = Array.length v in
  let rr = Index.from_int r in
   let result = T.null ( Array.make 2 rr ) in
    for i = 0 to pred r do
     let ii = Index.from_int i in
      T.insert_add v.(i) [| Index.zero () ; ii |] result ;
    done ;
    Sparse_tensor_matrix result ;;




(** {C § } *)
(** 
{3 Autres opérations}
{3 Other operations}
*)
(** {C  } *)




(** {v eq_zero matrix v} This verification of nullity is correct, provided that the dimensions be not too big.

Cette vérification de nullité est correcte, à condition que les dimensions ne soient pas trop grandes.  *)
let rec eq_zero = function (m:t) ->
 match m with
 | Sparse_tensor_matrix w  -> T.eq_zero w
 | Half_full_matrix w ->
  begin
   let r = Array.length w
   and i = ref 0
   and accu = ref true in
    if !accu then
     begin
      while !i < r do
       accu := V.eq_zero w.(!i) ;
       if !accu then
        incr i
       else
        i := r ;
      done ;
      !accu
     end
    else
     !accu
  end
 | _ -> eq_zero ( to_half_full m ) ;;

(** {v in_place_transpose matrix v} *)
let rec in_place_transpose = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = dimensions m
   and mm = to_sparse_tensor m in
    if not ( Index.eq d.(0) d.(1) ) then
     failwith "Not a square half_full matrix in Sparse_matrix.Rng.in_place_transpose." ;
    for i = 0 to pred ( Array.length w ) do
     w.(i) <- column_extract ( Index.from_int i ) mm
    done ;
  end
 | Sparse_tensor_matrix w -> T.level_exchange 0 1 w
 | Diff_to_scal_matrix ( y , w ) -> T.level_exchange 0 1 w
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   T.level_exchange 0 1 w ;
   let accu = ref a.(0)
   and r = pred ( Array.length a ) in
    for i = 0 to pred ( r / 2 ) do
     let ii = r - i in
      accu := a.(i) ;
      a.(i) <- a.(ii) ;
      a.(ii) <- !accu ;
    done ;
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   T.level_exchange 0 1 w ;
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   T.level_exchange 0 1 w ;
   let accu = ref a.(0)
   and r = pred ( Array.length a ) in
    let r2 = r / 2 in
     for i = 0 to pred r2 do
      let ii = r - i
      and shift = r2 - i in
       accu := a.(i) ;
       a.(i) <- Array.append ( Array.map Coeff.zero ( Array.make shift () ) ) a.(ii) ;
       a.(ii) <- Util.array_end shift !accu ;
     done ;
  end ;;

(** {v transpose matrix v} *)
let transpose = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = dimensions m
   and mm = to_sparse_tensor m in
    let r = Index.to_int d.(1) in
     let ww = Array.map V.null ( Array.make r d.(0) ) in
      for i = 0 to pred r do
       ww.(i) <- column_extract ( Index.from_int i ) mm
      done ;
      Half_full_matrix ww
  end
 | _ ->
  begin
   let mm = copy m in
    in_place_transpose mm ;
    mm
  end ;;

(** {v sparse_vector_to_column_matrix vector v} *)
let sparse_vector_to_column_matrix = function (v:V.t) ->
 transpose ( sparse_vector_to_line_matrix v ) ;;


(** {v raw_find coefficient matrix v} If the coefficient [x] is null, the search fails.

Si le coefficient [x] est nul, la recherche échoue. *)
let raw_find = fun (x:coeff) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let r = Array.length w
   and row_index = ref ( Index.witness () )
   and column_index = ref ( Index.witness () )
   and i = ref 0 in
    while !i < r do
     column_index := V.find x w.(!i) ;
     if not ( Index.eq ( Index.witness () ) !column_index ) then
      begin
       row_index := Index.from_int !i ;
       i := r
      end
     else
      incr i
    done ;
    [| !row_index ; !column_index |]
  end
 | Sparse_tensor_matrix w -> T.find x w
 | Diff_to_scal_matrix ( y , w ) -> T.find x w
 | Diff_to_multi_scal_matrix ( y , w ) -> T.find x w
 | Diff_to_diag_matrix ( a , w ) -> T.find x w
 | Diff_to_multi_diag_matrix ( a , w ) -> T.find x w ;;


(** {v find coefficient matrix v} If the coefficient [x] is null, the search fails.

Si le coefficient [x] est nul, la recherche échoue. *)
let find = fun (x:coeff) (m:t) ->
 match m with
 | Half_full_matrix w -> raw_find x m
 | Sparse_tensor_matrix w -> T.find x w
 | _ -> raw_find x ( to_half_full m ) ;;

(** {v tensor_sub_row_iter function index beginning ending tensor v} *)
let tensor_sub_row_iter = fun f (i:index) (beginning:index) (ending:index) (w:T.t) ->
 let ( e , t , v ) = T.flat_tensor_demakeup w in
  let candidates = T.Multi_hash.mask_vector [| i ; beginning |] [| i ; ending |] ( T.Info.raw_extract i t.(0) )
  and ( dim , table ) = v in
   let ensemble = Array.fold_left T.Multi_hash.H.B.union ( T.Multi_hash.H.B.empty () ) ( snd candidates ).T.Multi_hash.H.data
   and tab = table.T.M.H.data in
    let s = Array.length tab in
     let g = function ( j , x ) ->
      begin
       let j1 = j.(1) in
        if ( Index.eq i j.(0) ) && ( Index.compare beginning j1 <= 0 ) && ( Index.compare j1 ending <= 0 ) then
         f ( j , x )
      end in
      let h = function ( j , x ) -> T.M.H.B.iter g tab.( x mod s ) in
       T.Multi_hash.H.B.iter h ensemble ;;

(** {v tensor_sub_row_extract index beginning ending tensor v} *)
let tensor_sub_row_extract = fun (i:index) (beginning:index) (ending:index) (w:T.t) ->
 let result = V.null ( T.dimensions w ).(1) in
  let f = function ( j , x ) -> V.insert_add x j.(1) result in
   tensor_sub_row_iter f i beginning ending w ;
   result ;;

(** {v tensor_sub_column_iter function index beginning ending tensor v} *)
let tensor_sub_column_iter = fun f (i:index) (beginning:index) (ending:index) (w:T.t) ->
 let ( e , t , v ) = T.flat_tensor_demakeup w in
  let candidates = T.Multi_hash.mask_vector [| beginning ; i |] [| ending ; i |] ( T.Info.raw_extract i t.(1) )
  and ( dim , table ) = v in
   let ensemble = Array.fold_left T.Multi_hash.H.B.union ( T.Multi_hash.H.B.empty () ) ( snd candidates ).T.Multi_hash.H.data
   and tab = table.T.M.H.data in
    let s = Array.length tab in
     let g = function ( j , x ) ->
      begin
       let j0 = j.(0) in
        if ( Index.eq i j.(1) ) && ( Index.compare beginning j0 <= 0 ) && ( Index.compare j0 ending <= 0 ) then
         f ( j , x )
      end in
      let h = function ( j , x ) -> T.M.H.B.iter g tab.( x mod s ) in
       T.Multi_hash.H.B.iter h ensemble ;;

(** {v tensor_sub_column_extract index beginning ending tensor v} *)
let tensor_sub_column_extract = fun (i:index) (beginning:index) (ending:index) (w:T.t) ->
 let result = V.null ( T.dimensions w ).(0) in
  let f = function ( j , x ) -> V.insert_add x j.(0) result in
   tensor_sub_column_iter f i beginning ending w ;
   result ;;

(** {v tensor_hor_band_iter function beginning ending tensor v} *)
let tensor_hor_band_iter = fun f (beginning:index) (ending:index) (w:T.t) ->
 let g = function ( i , x ) ->
  begin
   let ii = i.(0) in
    if ( Index.compare ii beginning >= 0 ) && ( Index.compare ii ending <= 0 ) then
     f ( i , x )
  end in
  T.iter g w ;;

(** {v tensor_masked_hor_band beginning ending tensor v} *)
let tensor_masked_hor_band = fun (beginning:index) (ending:index) (w:T.t) ->
 let result = T.null ( T.dimensions w ) in
  let f = function ( i , x ) -> T.insert_add x i result in
   tensor_hor_band_iter f beginning ending w ;
   result ;;

(** {v tensor_hor_band beginning ending tensor v} *)
let tensor_hor_band = fun (beginning:index) (ending:index) (w:T.t) ->
 let d = T.dimensions w in
  let result = T.null [| Index.succ ( Index.sub ending beginning ) ; d.(1) |] in
   let f = function ( i , x ) -> T.insert_add x [| Index.sub i.(0) beginning ; i.(1) |] result in
    tensor_hor_band_iter f beginning ending w ;
    result ;;

(** {v tensor_vert_band_iter function beginning ending tensor v} *)
let tensor_vert_band_iter = fun f (beginning:index) (ending:index) (w:T.t) ->
 let g = function ( i , x ) ->
  begin
   let ii = i.(1) in
    if ( Index.compare ii beginning >= 0 ) && ( Index.compare ii ending <= 0 ) then
     f ( i , x )
  end in
  T.iter g w ;;

(** {v tensor_masked_vert_band beginning ending tensor v} *)
let tensor_masked_vert_band = fun (beginning:index) (ending:index) (w:T.t) ->
 let result = T.null ( T.dimensions w ) in
  let f = function ( i , x ) -> T.insert_add x i result in
   tensor_vert_band_iter f beginning ending w ;
   result ;;

(** {v tensor_vert_band beginning ending tensor v} *)
let tensor_vert_band = fun (beginning:index) (ending:index) (w:T.t) ->
 let d = T.dimensions w in
  let result = T.null [| d.(0) ; Index.succ ( Index.sub ending beginning ) |] in
   let f = function ( i , x ) -> T.insert_add x [| i.(0) ; Index.sub i.(1) beginning |] result in
    tensor_vert_band_iter f beginning ending w ;
    result ;;

(** {v tensor_head_iter function vert_ending hor_ending tensor v} *)
let tensor_head_iter = fun f (vert_ending:index) (hor_ending:index) (w:T.t) ->
 let g = function ( i , x ) ->
  begin
   if ( Index.compare i.(0) vert_ending <= 0 ) && ( Index.compare i.(1) hor_ending <= 0 ) then
    f ( i , x )
  end in
  T.iter g w ;;

(** {v tensor_masked_head vert_ending hor_ending tensor v} *)
let tensor_masked_head = fun (vert_ending:index) (hor_ending:index) (w:T.t) ->
 let result = T.null ( T.dimensions w ) in
  let f = function ( i , x ) -> T.insert_add x i result in
   tensor_head_iter f vert_ending hor_ending w ;
   result ;;

(** {v tensor_head vert_ending hor_ending tensor v} *)
let tensor_head = fun (vert_ending:index) (hor_ending:index) (w:T.t) ->
 let result = T.null [| Index.succ vert_ending ; Index.succ hor_ending |] in
  let f = function ( i , x ) -> T.insert_add x i result in
   tensor_head_iter f vert_ending hor_ending w ;
   result ;;

(** {v tensor_tail_iter function vert_beginning hor_beginning tensor v} *)
let tensor_tail_iter = fun f (vert_beginning:index) (hor_beginning:index) (w:T.t) ->
 let g = function ( i , x ) ->
  begin
   if ( Index.compare i.(0) vert_beginning >= 0 ) && ( Index.compare i.(1) hor_beginning >= 0 ) then
    f ( i , x )
  end in
  T.iter g w ;;

(** {v tensor_masked_tail vert_beginning hor_beginning tensor v} *)
let tensor_masked_tail = fun (vert_beginning:index) (hor_beginning:index) (w:T.t) ->
 let result = T.null ( T.dimensions w ) in
  let f = function ( i , x ) -> T.insert_add x i result in
   tensor_tail_iter f vert_beginning hor_beginning w ;
   result ;;

(** {v tensor_tail vert_beginning hor_beginning tensor v} *)
let tensor_tail = fun (vert_beginning:index) (hor_beginning:index) (w:T.t) ->
 let d = T.dimensions w in
  let result = T.null [| Index.sub d.(0) vert_beginning ; Index.sub d.(1) hor_beginning |] in
   let f = function ( i , x ) -> T.insert_add x [| Index.sub i.(0) vert_beginning ; Index.sub i.(1) hor_beginning |] result in
   tensor_tail_iter f vert_beginning hor_beginning w ;
    result ;;

(** {v tensor_sample_iter function vert_beginning vert_ending hor_beginning hor_ending tensor v} *)
let tensor_sample_iter = fun f (vert_beginning:index) (vert_ending:index) (hor_beginning:index) (hor_ending:index) (w:T.t) ->
 let g = function ( i , x ) ->
  begin
   let row = i.(0)
   and column = i.(1) in
    if ( Index.compare row vert_beginning >= 0 ) && ( Index.compare row vert_ending <= 0 ) && ( Index.compare column hor_beginning >= 0 ) && ( Index.compare column hor_ending <= 0 ) then
     f ( i , x )
  end in
  T.iter g w ;;

(** {v tensor_masked_sample vert_beginning vert_ending hor_beginning hor_ending tensor v} *)
let tensor_masked_sample = fun (vert_beginning:index) (vert_ending:index) (hor_beginning:index) (hor_ending:index) (w:T.t) ->
 let result = T.null ( T.dimensions w ) in
  let f = function ( i , x ) -> T.insert_add x i result in
   tensor_sample_iter f vert_beginning vert_ending hor_beginning hor_ending w ;
   result ;;

(** {v tensor_sample vert_beginning vert_ending hor_beginning hor_ending tensor v} *)
let tensor_sample = fun (vert_beginning:index) (vert_ending:index) (hor_beginning:index) (hor_ending:index) (w:T.t) ->
 let r = Index.succ ( Index.sub vert_ending vert_beginning )
 and c = Index.succ ( Index.sub hor_ending hor_beginning ) in
  let result = T.null [| r ; c |] in
   let f = function ( i , x ) -> T.insert_add x [| Index.sub i.(0) vert_beginning ; Index.sub i.(1) hor_beginning |] result in
    tensor_sample_iter f vert_beginning vert_ending hor_beginning hor_ending w ;
    result ;;

(** {v masked_hor_band beginning ending matrix v} *)
let rec masked_hor_band = fun (beginning:index) (ending:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = V.dimension w.(0)
   and b = Index.to_int beginning
   and e = Index.to_int ending
   and r = Array.length w in
    let result = Array.map V.null ( Array.make r d ) in
     for i = max 0 b to min ( pred r ) e do
      result.(i) <- w.(i)
     done ;
     Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_masked_hor_band beginning ending w )
 | Diff_to_scal_matrix ( y , w ) -> masked_hor_band beginning ending ( to_diff_to_diag m )
 | Diff_to_multi_scal_matrix ( y , w ) -> masked_hor_band beginning ending ( to_diff_to_multi_diag m )
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let b = Index.to_int beginning
   and e = Index.to_int ending
   and r = Array.length a in
    let aa = Array.map Coeff.zero ( Array.make r () ) in
     for i = max 0 b to min ( pred r ) e do
      aa.(i) <- a.(i)
     done ;
     Diff_to_diag_matrix ( aa , tensor_masked_hor_band beginning ending w )
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let b = Index.to_int beginning
   and e = Index.to_int ending
   and r = Array.length a
   and c = Array.length a.(0) in
    let aa = Array.map ( Array.map Coeff.zero ) ( Array.make_matrix r c () ) in
     for i = 0 to pred r do
      let output = aa.(i)
      and input = a.(i) in
       for j = b to e do
        output.(j) <- input.(j)
       done ;
     done ;
     Diff_to_multi_diag_matrix ( aa , tensor_masked_hor_band beginning ending w )
  end ;;

(** {v masked_vert_band beginning ending matrix v} *)
let rec masked_vert_band = fun (beginning:index) (ending:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = V.dimension w.(0)
   and r = Array.length w in
    let result = Array.map V.null ( Array.make r d ) in
     for i = 0 to pred r do
      result.(i) <- V.mask_vector beginning ending w.(i)
     done ;
     Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_masked_vert_band beginning ending w )
 | Diff_to_scal_matrix ( y , w ) -> masked_vert_band beginning ending ( to_diff_to_diag m )
 | Diff_to_multi_scal_matrix ( y , w ) -> masked_vert_band beginning ending ( to_diff_to_multi_diag m )
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let b = Index.to_int beginning
   and e = Index.to_int ending
   and r = Array.length a in
    let aa = Array.map Coeff.zero ( Array.make r () ) in
     for i = max 0 b to min ( pred r ) e do
      aa.(i) <- a.(i)
     done ;
     Diff_to_diag_matrix ( aa , tensor_masked_vert_band beginning ending w )
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let b = Index.to_int beginning
   and e = Index.to_int ending
   and r = Array.length a
   and c = Array.length a.(0) in
    let aa = Array.map ( Array.map Coeff.zero ) ( Array.make_matrix r c () )
    and hr = r / 2
    and cc = pred c in
     for i = 0 to pred r do
      let output = aa.(i)
      and ii = i - hr
      and input = a.(i) in
       for j = max 0 ( b - ii ) to min cc ( e - ii ) do
        output.(j) <- input.(j)
       done ;
     done ;
     Diff_to_multi_diag_matrix ( aa , tensor_masked_vert_band beginning ending w )
  end ;;

(** {v masked_head vert_ending hor_ending matrix v} *)
let rec masked_head = fun (vert_ending:index) (hor_ending:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = V.dimension w.(0)
   and r = Array.length w in
    let result = Array.map V.null ( Array.make r d ) in
     for i = 0 to min ( pred r ) ( Index.to_int vert_ending ) do
      result.(i) <- V.mask_vector ( Index.zero () ) hor_ending w.(i)
     done ;
     Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_masked_head vert_ending hor_ending w )
 | Diff_to_scal_matrix ( y , w ) -> masked_head vert_ending hor_ending ( to_diff_to_diag m )
 | Diff_to_multi_scal_matrix ( y , w ) -> masked_head vert_ending hor_ending ( to_diff_to_multi_diag m )
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let v = Index.to_int vert_ending
   and h = Index.to_int hor_ending
   and r = Array.length a in
    let aa = Array.map Coeff.zero ( Array.make r () ) in
     for i = 0 to min ( pred r ) ( min v h ) do
      aa.(i) <- a.(i)
     done ;
     Diff_to_diag_matrix ( aa , tensor_masked_head vert_ending hor_ending w )
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let v = Index.to_int vert_ending
   and h = Index.to_int hor_ending
   and r = Array.length a
   and c = Array.length a.(0) in
    let aa = Array.map ( Array.map Coeff.zero ) ( Array.make_matrix r c () )
    and hr = r / 2
    and cc = pred c in
     let bound = min v cc in
      for i = 0 to pred r do
       let output = aa.(i)
       and ii = i - hr
       and input = a.(i) in
        for j = 0 to min bound ( h - ii ) do
         output.(j) <- input.(j)
        done ;
      done ;
      Diff_to_multi_diag_matrix ( aa , tensor_masked_head vert_ending hor_ending w )
  end ;;

(** {v masked_tail vert_beginning hor_beginning matrix v} *)
let rec masked_tail = fun (vert_beginning:index) (hor_beginning:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = V.dimension w.(0)
   and r = Array.length w in
    let result = Array.map V.null ( Array.make r d ) in
     for i = max 0 ( Index.to_int vert_beginning ) to pred r do
      result.(i) <- V.mask_vector hor_beginning d w.(i)
     done ;
     Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_masked_tail vert_beginning hor_beginning w )
 | Diff_to_scal_matrix ( y , w ) -> masked_tail vert_beginning hor_beginning ( to_diff_to_diag m )
 | Diff_to_multi_scal_matrix ( y , w ) -> masked_tail vert_beginning hor_beginning ( to_diff_to_multi_diag m )
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let v = Index.to_int vert_beginning
   and h = Index.to_int hor_beginning
   and r = Array.length a in
    let aa = Array.map Coeff.zero ( Array.make r () ) in
     for i = max v h to pred r do
      aa.(i) <- a.(i)
     done ;
     Diff_to_diag_matrix ( aa , tensor_masked_tail vert_beginning hor_beginning w )
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let v = Index.to_int vert_beginning
   and h = Index.to_int hor_beginning
   and r = Array.length a
   and c = Array.length a.(0) in
    let aa = Array.map ( Array.map Coeff.zero ) ( Array.make_matrix r c () )
    and hr = r / 2
    and cc = pred c in
     let bound = max 0 v in
      for i = 0 to pred r do
       let output = aa.(i)
       and ii = i - hr
       and input = a.(i) in
        for j = max bound ( h - ii ) to cc do
         output.(j) <- input.(j)
        done ;
      done ;
      Diff_to_multi_diag_matrix ( aa , tensor_masked_tail vert_beginning hor_beginning w )
  end ;;

(** {v masked_sample vert_beginning vert_ending hor_beginning hor_ending matrix v} *)
let rec masked_sample = fun (vert_beginning:index) (vert_ending:index) (hor_beginning:index) (hor_ending:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = V.dimension w.(0)
   and r = Array.length w in
    let result = Array.map V.null ( Array.make r d ) in
     for i = max 0 ( Index.to_int vert_beginning ) to min ( pred r ) ( Index.to_int vert_ending ) do
      result.(i) <- V.mask_vector hor_beginning hor_ending w.(i)
     done ;
     Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_masked_sample vert_beginning vert_ending hor_beginning hor_ending w )
 | Diff_to_scal_matrix ( y , w ) -> masked_sample vert_beginning vert_ending hor_beginning hor_ending ( to_diff_to_diag m )
 | Diff_to_multi_scal_matrix ( y , w ) -> masked_sample vert_beginning vert_ending hor_beginning hor_ending ( to_diff_to_multi_diag m )
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let vb = Index.to_int vert_beginning
   and hb = Index.to_int hor_beginning
   and ve = Index.to_int vert_ending
   and he = Index.to_int hor_ending
   and r = Array.length a in
    let aa = Array.map Coeff.zero ( Array.make r () ) in
     for i = max 0 ( max vb hb ) to min ( pred r ) ( min ve he ) do
      aa.(i) <- a.(i)
     done ;
     Diff_to_diag_matrix ( aa , tensor_masked_sample vert_beginning vert_ending hor_beginning hor_ending w )
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let vb = Index.to_int vert_beginning
   and hb = Index.to_int hor_beginning
   and ve = Index.to_int vert_ending
   and he = Index.to_int hor_ending
   and r = Array.length a
   and c = Array.length a.(0) in
    let aa = Array.map ( Array.map Coeff.zero ) ( Array.make_matrix r c () )
    and hr = r / 2
    and cc = pred c in
     let beginning = max 0 vb
     and ending = min ve cc in
      for i = 0 to pred r do
       let output = aa.(i)
       and ii = i - hr
       and input = a.(i) in
        for j = max beginning ( hb - ii ) to min ending ( he - ii ) do
         output.(j) <- input.(j)
        done ;
      done ;
      Diff_to_multi_diag_matrix ( aa , tensor_masked_sample vert_beginning vert_ending hor_beginning hor_ending w )
  end ;;

(** {v hor_band beginning ending matrix v} *)
let rec hor_band = fun (beginning:index) (ending:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = V.dimension w.(0)
   and b = Index.to_int beginning
   and e = Index.to_int ending in
    let r = max 0 ( e - b ) in
     let result = Array.map V.null ( Array.make ( succ r ) d ) in
      for i = 0 to r do
       result.(i) <- w.( b + i)
      done ;
      Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_hor_band beginning ending w )
 | Diff_to_scal_matrix ( y , w ) -> hor_band beginning ending ( to_half_full m )
 | Diff_to_multi_scal_matrix ( y , w ) -> hor_band beginning ending ( to_half_full m )
 | Diff_to_diag_matrix ( a , w ) -> hor_band beginning ending ( to_half_full m )
 | Diff_to_multi_diag_matrix ( a , w ) -> hor_band beginning ending ( to_half_full m ) ;;

(** {v vert_band beginning ending matrix v} *)
let rec vert_band = fun (beginning:index) (ending:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = V.dimension w.(0)
   and r = Array.length w in
    let result = Array.map V.null ( Array.make r d ) in
     for i = 0 to pred r do
      result.(i) <- V.sub_vector beginning ending w.(i)
     done ;
     Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_vert_band beginning ending w )
 | Diff_to_scal_matrix ( y , w ) -> vert_band beginning ending ( to_half_full m )
 | Diff_to_multi_scal_matrix ( y , w ) -> vert_band beginning ending ( to_half_full m )
 | Diff_to_diag_matrix ( a , w ) -> vert_band beginning ending ( to_half_full m )
 | Diff_to_multi_diag_matrix ( a , w ) -> vert_band beginning ending ( to_half_full m ) ;;

(** {v head vert_ending hor_ending matrix v} *)
let rec head = fun (vert_ending:index) (hor_ending:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let ve = Index.to_int vert_ending in
    let result = Array.map V.null ( Array.make ( succ ve ) ( Index.succ hor_ending ) ) in
     for i = 0 to max 0 ve do
      result.(i) <- V.sub_vector ( Index.zero () ) hor_ending w.(i)
     done ;
     Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_head vert_ending hor_ending w )
 | Diff_to_scal_matrix ( y , w ) -> Diff_to_scal_matrix ( y , tensor_head vert_ending hor_ending w )
 | Diff_to_multi_scal_matrix ( y , w ) -> Diff_to_multi_scal_matrix ( y , tensor_head vert_ending hor_ending w )
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   let v = Index.to_int vert_ending
   and h = Index.to_int hor_ending in
    let aa = Array.sub a 0 ( succ ( min v h ) ) in
      Diff_to_diag_matrix ( aa , tensor_head vert_ending hor_ending w )
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   let v = Index.to_int vert_ending in
    let aa = Array.map ( function b -> Array.sub b 0 ( succ v ) ) a in
     Diff_to_multi_diag_matrix ( aa , tensor_head vert_ending hor_ending w )
  end ;;

(** {v tail vert_beginning hor_beginning matrix v} *)
let rec tail = fun (vert_beginning:index) (hor_beginning:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let vb = Index.to_int vert_beginning
   and d = V.dimension w.(0)
   and r = Array.length w in
    let rr = max 0 ( r - vb )
    and dd = Index.pred d in
     let result = Array.map V.null ( Array.make rr ( Index.sub d hor_beginning ) ) in
      for i = 0 to pred rr do
       result.(i) <- V.sub_vector hor_beginning dd w.( vb + i )
      done ;
      Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_tail vert_beginning hor_beginning w )
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   if Index.eq vert_beginning hor_beginning then
    Diff_to_scal_matrix ( y , tensor_tail vert_beginning hor_beginning w )
   else
    tail vert_beginning hor_beginning ( to_half_full m )
  end
 | Diff_to_multi_scal_matrix ( y , w ) ->
  begin
   if Index.eq vert_beginning hor_beginning then
    Diff_to_multi_scal_matrix ( y , tensor_tail vert_beginning hor_beginning w )
   else
    tail vert_beginning hor_beginning ( to_half_full m )
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   if Index.eq vert_beginning hor_beginning then
    begin
     let v = Index.to_int vert_beginning
     and h = Index.to_int hor_beginning in
      let aa = Util.array_end ( min v h ) a in
        Diff_to_diag_matrix ( aa , tensor_tail vert_beginning hor_beginning w )
    end
   else
    tail vert_beginning hor_beginning ( to_half_full m )
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   if Index.eq vert_beginning hor_beginning then
    begin
     let v = Index.to_int vert_beginning in
      let aa = Array.map ( Util.array_end ( max 0 v ) ) a in
       Diff_to_multi_diag_matrix ( aa , tensor_head vert_beginning hor_beginning w )
    end
   else
    tail vert_beginning hor_beginning ( to_half_full m )
  end ;;

(** {v sample vert_beginning vert_ending hor_beginning hor_ending matrix v} *)
let rec sample = fun (vert_beginning:index) (vert_ending:index) (hor_beginning:index) (hor_ending:index) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let d = Index.succ ( Index.sub hor_ending hor_beginning )
   and b = Index.to_int vert_beginning
   and rr = Index.to_int ( Index.sub vert_ending vert_beginning ) in
    let result = Array.map V.null ( Array.make ( succ rr ) d ) in
     for i = 0 to rr do
      result.(i) <- V.sub_vector hor_beginning hor_ending w.( b + i )
     done ;
     Half_full_matrix result
  end
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( tensor_sample vert_beginning vert_ending hor_beginning hor_ending w )
 | Diff_to_scal_matrix ( y , w ) ->
  begin
   if Index.eq vert_beginning hor_beginning then
    Diff_to_scal_matrix ( y , tensor_sample vert_beginning vert_ending hor_beginning hor_ending w )
   else
    sample vert_beginning vert_ending hor_beginning hor_ending ( to_half_full m )
  end
 | Diff_to_multi_scal_matrix ( y , w ) ->
  begin
   if Index.eq vert_beginning hor_beginning then
    Diff_to_multi_scal_matrix ( y , tensor_sample vert_beginning vert_ending hor_beginning hor_ending w )
   else
    sample vert_beginning vert_ending hor_beginning hor_ending ( to_half_full m )
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   if Index.eq vert_beginning hor_beginning then
    begin
     let vb = Index.to_int vert_beginning
     and ve = Index.to_int vert_ending
     and he = Index.to_int hor_ending in
      let aa = Array.sub a vb ( ( min ve he ) - vb + 1 ) in
       Diff_to_diag_matrix ( aa , tensor_sample vert_beginning vert_ending hor_beginning hor_ending w )
    end
   else
    sample vert_beginning vert_ending hor_beginning hor_ending ( to_half_full m )
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   if Index.eq vert_beginning hor_beginning then
    begin
     let vb = Index.to_int vert_beginning
     and ve = Index.to_int vert_ending in
      let cc = ve - vb in
       let c = succ cc in
        let aa = Array.map ( function table -> Array.sub table vb c ) a in
         Diff_to_multi_diag_matrix ( aa , tensor_sample vert_beginning vert_ending hor_beginning hor_ending w )
    end
   else
    sample vert_beginning vert_ending hor_beginning hor_ending ( to_half_full m )
  end ;;

(** {v iter function matrix v} *)
let rec iter = fun f (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let r = Array.length w in
    for i = 0 to pred r do
     V.iter ( function ( j , x ) -> f ( [| ( Index.from_int i ) ; j |] , x ) ) w.(i)
    done ;
  end
 | Sparse_tensor_matrix w -> T.iter f w
 |  _ -> iter f ( to_sparse_tensor m ) ;;


(** {v row_exchange index1 index2 matrix v} This function is applied in place.

Cette fonction est exécutée en place.*)
let row_exchange = fun (i:index) (j:index) (m:t) ->
 if not ( Index.eq i j ) then
  begin
   match m with
   | Half_full_matrix w ->
    begin
     let ii = Index.to_int i
     and jj = Index.to_int j in
      let accu = w.(ii) in
       w.(ii) <- w.(jj) ;
       w.(jj) <- accu
    end
   | Sparse_tensor_matrix w -> T.exchange 0 i j w
   | Diff_to_scal_matrix ( y , w ) ->
    begin
     let ii = Array.make 2 i
     and jj = Array.make 2 j in
      T.exchange 0 i j w ;
      T.insert_sub y ii w ;
      T.insert_sub y jj w ;
      T.insert_add y [| i ; j |] w ;
      T.insert_add y [| j ; i |] w ;
    end
   | Diff_to_multi_scal_matrix ( a , w ) ->
    begin
     let alength = Array.length a
     and d = T.dimensions w
     and ii = Index.to_int i
     and jj = Index.to_int j in
      T.exchange 0 i j w ;
      let h_a_l = alength / 2
      and c = Index.to_int d.(1) in
       let shift_i = ii - h_a_l
       and shift_j = jj - h_a_l in
        for k = 0 to pred alength do
         let aa = a.(k)
         and ki = shift_i + k
         and kj = shift_j + k in
          let k_i = Index.from_int ki
          and k_j = Index.from_int kj in
           if ( ki >= 0 ) && ( ki < c ) then
            begin
             T.insert_add aa [| j ; k_i |] w ;
             T.insert_sub aa [| i ; k_i |] w
            end ;
           if ( kj >= 0 ) && ( kj < c ) then
            begin
             T.insert_add aa [| i ; k_j |] w ;
             T.insert_sub aa [| j ; k_j |] w
            end ;
        done ;
    end
   | Diff_to_diag_matrix ( a , w ) ->
    begin
     let ii = Array.make 2 i
     and jj = Array.make 2 j
     and i_i = Index.to_int i
     and j_j = Index.to_int j in
      T.exchange 0 i j w ;
      let ai = a.(i_i)
      and aj = a.(j_j) in
       T.insert_sub ai ii w ;
       T.insert_sub aj jj w ;
       T.insert_add aj [| i ; j |] w ;
       T.insert_add ai [| j ; i |] w ;
    end
   | Diff_to_multi_diag_matrix ( a , w ) ->
    begin
     let alength = Array.length a
     and d = T.dimensions w
     and ii = Index.to_int i
     and jj = Index.to_int j in
      T.exchange 0 i j w ;
      let h_a_l = alength / 2
      and c = Index.to_int d.(1) in
       let shift_i = ii - h_a_l
       and shift_j = jj - h_a_l in
        for k = 0 to pred alength do
         let aa = a.(k)
         and ki = shift_i + k
         and kj = shift_j + k in
          let k_i = Index.from_int ki
          and k_j = Index.from_int kj
          and ai = aa.(ii)
          and aj = aa.(jj) in
           if ( ki >= 0 ) && ( ki < c ) then
            begin
             T.insert_add ai [| j ; k_i |] w ;
             T.insert_sub ai [| i ; k_i |] w
            end ;
           if ( kj >= 0 ) && ( kj < c ) then
            begin
             T.insert_add aj [| i ; k_j |] w ;
             T.insert_sub aj [| j ; k_j |] w
            end ;
        done ;
    end
  end ;;


(** {v column_exchange index1 index2 matrix v} This function is applied in place.

Cette fonction est exécutée en place. *)
let column_exchange = fun (i:index) (j:index) (m:t) ->
 if not ( Index.eq i j ) then
  begin
   match m with
   | Half_full_matrix w ->
    begin
     for k = 0 to pred ( Array.length w ) do
      V.exchange i j w.(k)
     done ;
    end
   | Sparse_tensor_matrix w -> T.exchange 1 i j w
   | Diff_to_scal_matrix ( y , w ) ->
    begin
     let ii = Array.make 2 i
     and jj = Array.make 2 j in
      T.exchange 1 i j w ;
      T.insert_sub y ii w ;
      T.insert_sub y jj w ;
      T.insert_add y [| i ; j |] w ;
      T.insert_add y [| j ; i |] w ;
    end
   | Diff_to_diag_matrix ( a , w ) ->
    begin
     let ii = Array.make 2 i
     and jj = Array.make 2 j
     and i_i = Index.to_int i
     and j_j = Index.to_int j in
      T.exchange 1 i j w ;
      let ai = a.(i_i)
      and aj = a.(j_j) in
       T.insert_sub ai ii w ;
       T.insert_sub aj jj w ;
       T.insert_add ai [| i ; j |] w ;
       T.insert_add aj [| j ; i |] w ;
    end
   | _ ->
    begin
     let ci = column_extract i m
     and cj = column_extract j m in
      column_replace ci j m ;
      column_replace cj i m ;
    end
  end ;;

(** {v in_place_map function matrix v} *)
let in_place_map = fun f (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> T.in_place_map f w
 | Diff_to_scal_matrix ( x , w ) -> failwith "Diff_to_scal matrix in Sparse_matrix.Rng.in_place_map."
 | Diff_to_multi_scal_matrix ( a , w ) ->
  begin
   for i = 0 to pred ( Array.length a ) do
    a.(i) <- f a.(i)
   done ;
   T.in_place_map f w
  end
 | Half_full_matrix w ->
  begin
   for i = 0 to pred ( Array.length w ) do
    V.in_place_map f w.(i)
   done ;
  end
 | Diff_to_diag_matrix ( a , w ) ->
  begin
   for i = 0 to pred ( Array.length a ) do
    a.(i) <- f a.(i)
   done ;
   T.in_place_map f w
  end
 | Diff_to_multi_diag_matrix ( a , w ) ->
  begin
   for i = 0 to pred ( Array.length a ) do
    a.(i) <- Array.map f a.(i)
   done ;
   T.in_place_map f w
  end ;;

(** {v map function matrix v} *)
let map = fun f (m:t) ->
 match m with
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( T.map f w )
 | Diff_to_scal_matrix ( x , w ) -> Diff_to_scal_matrix ( f x , T.map f w )
 | Diff_to_multi_scal_matrix ( a , w ) -> Diff_to_multi_scal_matrix ( Array.map f a , T.map f w )
 | Half_full_matrix w -> Half_full_matrix ( Array.map ( V.map f ) w )
 | Diff_to_diag_matrix ( a , w ) -> Diff_to_diag_matrix ( Array.map f a , T.map f w )
 | Diff_to_multi_diag_matrix ( a , w ) -> Diff_to_multi_diag_matrix ( Array.map ( Array.map f ) a , T.map f w ) ;;

(** {v in_place_opp matrix v} *)
let in_place_opp = function (m:t) ->
 in_place_map Coeff.opp m ;;

(** {v opp matrix v} *)
let opp = function (m:t) ->
 map Coeff.opp m ;;

(** {v embed dimensions shifts matrix v} *)
let rec embed = fun (dim:index array) (shifts:index array) (m:t) ->
 let d = dimensions m
 and s0 = shifts.(0)
 and s1 = shifts.(1)
 and r = dim.(0)
 and c = dim.(1) in
  let e0 = Index.add s0 d.(0)
  and e1 = Index.add s1 d.(1) in
  assert ( ( Index.compare r e0 >= 0 ) && ( Index.compare c e1 >= 0 ) ) ;
  match m with
  | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( T.embed dim shifts w )
  | Diff_to_scal_matrix ( x , w ) -> embed dim shifts ( to_half_full m )
  | Diff_to_multi_scal_matrix ( a , w ) -> embed dim shifts ( to_half_full m )
  | Half_full_matrix w ->
   begin
    let c = dim.(1)
    and s_0 = Index.to_int s0 in
     let ww = Array.map V.null ( Array.make ( Index.to_int r ) c ) in
      for i = 0 to pred ( Array.length w ) do
       ww.( s_0 + i ) <- V.embed c s1 w.(i)
      done ;
      Half_full_matrix ( ww )
   end
  | Diff_to_diag_matrix ( a , w ) ->
   begin
    if Index.eq s0 s1 then
     begin
      let rest = Index.min ( Index.sub r e0 ) ( Index.sub c e1 ) in
       let aa = Array.concat [ Array.map Coeff.zero ( Array.make ( Index.to_int s0 ) () ) ; a ; Array.map Coeff.zero ( Array.make ( Index.to_int rest ) () ) ] in
        Diff_to_diag_matrix ( aa , T.embed dim shifts w )
     end
    else
     embed dim shifts ( to_half_full m )
   end
  | Diff_to_multi_diag_matrix ( a , w ) ->
   begin
    if Index.eq s0 s1 then
     begin
      let rest = Index.min ( Index.sub r e0 ) ( Index.sub c e1 ) in
       let f = function b -> Array.concat [ Array.map Coeff.zero ( Array.make ( Index.to_int s0 ) () ) ; b ; Array.map Coeff.zero ( Array.make ( Index.to_int rest ) () ) ] in
        Diff_to_multi_diag_matrix ( Array.map f a , T.embed dim shifts w )
     end
    else
     embed dim shifts ( to_half_full m )
   end ;;

(** {v sparse_tensor_dirty_rows_list tensor v} *)
let sparse_tensor_dirty_rows_list = function (w:T.t) ->
 let ( e , t , v ) = T.flat_tensor_demakeup w
 and result = ref [] in
  let rows = t.(0) in
   let f = function ( index , coefficient ) -> result := index :: !result in
    T.Info.iter f rows ;
    !result ;;

(** {v sparse_tensor_dirty_columns_list tensor v} *)
let sparse_tensor_dirty_columns_list = function (w:T.t) ->
 let ( e , t , v ) = T.flat_tensor_demakeup w
 and result = ref [] in
  let columns = t.(1) in
   let f = function ( index , coefficient ) -> result := index :: !result in
    T.Info.iter f columns ;
    !result ;;

(** {v sparse_tensor_dirty_rows_array tensor v} *)
let sparse_tensor_dirty_rows_array = function (w:T.t) ->
 Array.of_list ( sparse_tensor_dirty_rows_list w ) ;;
 
(** {v sparse_tensor_dirty_columns_array tensor v} *)
let sparse_tensor_dirty_columns_array = function (w:T.t) ->
 Array.of_list ( sparse_tensor_dirty_columns_list w ) ;;




(** {C § } *)
(** 
{3 Opérations de calcul}
{3 Calculative operations}
*)
(** {C  } *)




(** {v sparse_row_fold function init matrix v} *)
let rec sparse_row_fold = fun (f:coeff -> V.t -> coeff) (init:V.t) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let ff = fun i x -> f ( V.raw_extract ( Index.from_int i ) init ) x in
    V.to_sparse ( V.size init ) ( Array.mapi ff w )
  end
 | Sparse_tensor_matrix w ->
  begin
   let accu = ref ( V.H.B.E.empty )
   and result = V.null ( V.dimension init ) in
    let ff = function ( i , x ) -> accu := V.H.B.E.add ( i.(0) , Coeff.zero () ) !accu
    and g = function ( i , x ) ->
     begin
      let y = V.raw_extract i init
      and row = tensor_row_extract i w in
       V.insert_add ( f y row ) i result ;
     end in
     T.iter ff w ;
     V.H.B.E.iter g !accu ;
     result
  end
 | _ -> sparse_row_fold f init ( to_half_full m ) ;;

(** {v sparse_column_fold function init matrix v} *)
let rec sparse_column_fold = fun (f:coeff -> V.t -> coeff) (init:V.t) (m:t) ->
 match m with
 | Sparse_tensor_matrix w ->
  begin
   let accu = ref ( V.H.B.E.empty )
   and result = V.null ( V.dimension init ) in
    let ff = function ( i , x ) -> accu := V.H.B.E.add ( i.(1) , Coeff.zero () ) !accu
    and g = function ( i , x ) ->
     begin
      let y = V.raw_extract i init
      and column = tensor_column_extract i w in
       V.insert_add ( f y column ) i result ;
     end in
     T.iter ff w ;
     V.H.B.E.iter g !accu ;
     result
  end
 | _ -> sparse_column_fold f init ( to_sparse_tensor m ) ;;

(** {v full_row_fold function matrix init v} *)
let rec full_row_fold = fun (f:coeff -> V.t -> coeff) (init:coeff array) (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   let ff = fun i x -> f init.(i) x in
    Array.mapi ff w
  end
 | Sparse_tensor_matrix w ->
  begin
   let accu = ref ( V.H.B.E.empty )
   and r = Array.length init in
    let result = Array.map Coeff.zero ( Array.make r () )
    and ff = function ( i , x ) -> accu := V.H.B.E.add ( i.(0) , Coeff.zero () ) !accu in
     let g = function ( i , x ) ->
      begin
       let ii = Index.to_int i
       and row = tensor_row_extract i w in
        result.(ii) <- f init.(ii) row ;
      end in
      T.iter ff w ;
      V.H.B.E.iter g !accu ;
      result
  end
 | _ -> full_row_fold f init ( to_half_full m ) ;;

(** {v full_column_fold function init matrix v} *)
let rec full_column_fold = fun (f:coeff -> V.t -> coeff) (init:coeff array) (m:t) ->
 match m with
 | Sparse_tensor_matrix w ->
  begin
   let accu = ref ( V.H.B.E.empty )
   and r = Array.length init in
    let result = Array.map Coeff.zero ( Array.make r () )
    and ff = function ( i , x ) -> accu := V.H.B.E.add ( i.(1) , Coeff.zero () ) !accu in
     let g = function ( i , x ) ->
      begin
       let ii = Index.to_int i
       and column = tensor_column_extract i w in
        result.(ii) <- f init.(ii) column ;
      end in
      T.iter ff w ;
      V.H.B.E.iter g !accu ;
      result
  end
 | _ -> full_column_fold f init ( to_sparse_tensor m ) ;;

(** {v sparse_row_sum matrix v} *)
let sparse_row_sum = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.sum v in
  sparse_row_fold f ( V.null d.(0) ) m ;;

(** {v sparse_column_sum matrix v} *)
let sparse_column_sum = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.sum v in
  sparse_column_fold f ( V.null d.(1) ) m ;;

(** {v full_row_sum matrix v} *)
let full_row_sum = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.sum v in
  let z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(0) ) () ) in
   full_row_fold f z m ;;

(** {v full_column_sum matrix v} *)
let full_column_sum = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.sum v in
  let z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(1) ) () ) in
   full_column_fold f z m ;;

(** {v sparse_matrix_sum matrix v} *)
let sparse_matrix_sum = function (m:t) ->
 let v = sparse_row_sum m in
  V.sum v ;;

(** {v full_matrix_sum matrix v} *)
let full_matrix_sum = function (m:t) ->
 let v = full_row_sum m in
  Array.fold_left Coeff.add ( Coeff.zero () ) v ;;

(** {v matrix_sparse_vector_sparse_prod matrix vector v} *)
let matrix_sparse_vector_sparse_prod = fun (m:t) (v:V.t) ->
 let d = dimensions m
 and f = fun x w -> V.scal_prod v w in
  let z = V.null d.(0) in
   sparse_row_fold f z m ;;

(** {v matrix_full_vector_sparse_prod matrix vector v} *)
let matrix_full_vector_sparse_prod = fun (m:t) (v:coeff array) ->
 let d = dimensions m in
  let f = fun x w -> V.sparse_full_scal_prod w v
  and z = V.null d.(0) in
   sparse_row_fold f z m ;;

(** {v matrix_sparse_vector_full_prod matrix vector v} *)
let matrix_sparse_vector_full_prod = fun (m:t) (v:V.t) ->
 let d = dimensions m
 and f = fun x w -> V.scal_prod v w in
  let z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(0) ) () ) in
   full_row_fold f z m ;;

(** {v matrix_full_vector_full_prod matrix vector v} *)
let matrix_full_vector_full_prod = fun (m:t) (v:coeff array) ->
 let d = dimensions m in
  let f = fun x w -> V.sparse_full_scal_prod w v
  and z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(0) ) () ) in
   full_row_fold f z m ;;

(** {v sparse_vector_matrix_sparse_prod vector matrix v} *)
let sparse_vector_matrix_sparse_prod = fun (v:V.t) (m:t) ->
 let d = dimensions m
 and f = fun x w -> V.scal_prod v w in
  let z = V.null d.(1) in
   sparse_column_fold f z m ;;

(** {v full_vector_matrix_sparse_prod vector matrix v} *)
let full_vector_matrix_sparse_prod = fun (v:coeff array) (m:t) ->
 let d = dimensions m in
  let f = fun x w -> V.sparse_full_scal_prod w v
  and z = V.null d.(1) in
   sparse_column_fold f z m ;;

(** {v sparse_vector_matrix_full_prod vector matrix v} *)
let sparse_vector_matrix_full_prod = fun (v:V.t) (m:t) ->
 let d = dimensions m
 and f = fun x w -> V.scal_prod v w in
  let z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(1) ) () ) in
   full_column_fold f z m ;;

(** {v full_vector_matrix_full_prod vector matrix v} *)
let full_vector_matrix_full_prod = fun (v:coeff array) (m:t) ->
 let d = dimensions m in
  let f = fun x w -> V.sparse_full_scal_prod w v
  and z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(1) ) () ) in
   full_column_fold f z m ;;

(** {v sparse_row_max matrix v} *)
let sparse_row_max = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.max v in
  sparse_row_fold f ( V.null d.(0) ) m ;;

(** {v sparse_column_max matrix v} *)
let sparse_column_max = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.max v in
  sparse_column_fold f ( V.null d.(1) ) m ;;

(** {v full_row_max matrix v} *)
let full_row_max = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.max v in
  let z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(0) ) () ) in
   full_row_fold f z m ;;

(** {v full_column_max matrix v} *)
let full_column_max = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.max v in
  let z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(1) ) () ) in
   full_column_fold f z m ;;

(** {v sparse_matrix_max matrix v} *)
let sparse_matrix_max = function (m:t) ->
 let v = sparse_row_max m in
  V.max v ;;

(** {v full_matrix_max matrix v} *)
let full_matrix_max = function (m:t) ->
 let v = full_row_max m in
  Array.fold_left Coeff.add ( Coeff.zero () ) v ;;

(** {v sparse_row_min matrix v} *)
let sparse_row_min = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.min v in
  sparse_row_fold f ( V.null d.(0) ) m ;;

(** {v sparse_column_min matrix v} *)
let sparse_column_min = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.min v in
  sparse_column_fold f ( V.null d.(1) ) m ;;

(** {v full_row_min matrix v} *)
let full_row_min = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.min v in
  let z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(0) ) () ) in
   full_row_fold f z m ;;

(** {v full_column_min matrix v} *)
let full_column_min = function (m:t) ->
 let d = dimensions m
 and f = fun x v -> V.min v in
  let z = Array.map Coeff.zero ( Array.make ( Index.to_int d.(1) ) () ) in
   full_column_fold f z m ;;

(** {v sparse_matrix_min matrix v} *)
let sparse_matrix_min = function (m:t) ->
 let v = sparse_row_min m in
  V.min v ;;

(** {v full_matrix_min matrix v} *)
let full_matrix_min = function (m:t) ->
 let v = full_row_min m in
  Array.fold_left Coeff.add ( Coeff.zero () ) v ;;

(** {v norm_inf matrix v} *)
let rec norm_inf = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   Util.array_maximum Coeff.norm_compare ( Array.map V.norm_1 w )
  end
 | Sparse_tensor_matrix w ->
  begin
   let accu = ref ( V.H.B.E.empty )
   and result = ref ( Coeff.norm_zero () )
   and accumulator = ref [] in
    let ff = function ( i , x ) -> accu := V.H.B.E.add ( i.(0) , Coeff.zero () ) !accu
    and g = function ( i , x ) ->
     begin
      let row = tensor_row_extract i w in
       accumulator := ( V.norm_1 row ) :: !accumulator ;
     end in
     T.iter ff w ;
     V.H.B.E.iter g !accu ;
     while Util.list_non_empty !accumulator do
      let test = List.hd !accumulator in
       if Coeff.norm_compare test !result > 0 then
        result := test ;
       accumulator := List.tl !accumulator ;
     done ;
     !result
  end
 | _ -> norm_inf ( to_half_full m ) ;;

(** {v norm_1 matrix v} *)
let rec norm_1 = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   Array.fold_left Coeff.norm_add ( Coeff.norm_zero () ) ( Array.map V.norm_inf w )
  end
 | Sparse_tensor_matrix w ->
  begin
   let accu = ref ( V.H.B.E.empty )
   and result = ref ( Coeff.norm_zero () )
   and accumulator = ref [] in
    let ff = function ( i , x ) -> accu := V.H.B.E.add ( i.(0) , Coeff.zero () ) !accu
    and g = function ( i , x ) ->
     begin
      let row = tensor_row_extract i w in
       accumulator := ( V.norm_inf row ) :: !accumulator ;
     end in
     T.iter ff w ;
     V.H.B.E.iter g !accu ;
     while Util.list_non_empty !accumulator do
      let test = List.hd !accumulator in
       result := Coeff.norm_add test !result ;
       accumulator := List.tl !accumulator ;
     done ;
     !result
  end
 | _ -> norm_1 ( to_half_full m ) ;;

(** {v square_norm_frobenius matrix v} *)
let rec square_norm_frobenius = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   Array.fold_left Coeff.norm_add ( Coeff.norm_zero () ) ( Array.map V.square_norm_2 w )
  end
 | Sparse_tensor_matrix w ->
  begin
   let result = ref ( Coeff.norm_zero () ) in
    let f = function ( i , x ) ->
     begin
      let y = Coeff.norm x in
       result := Coeff.norm_add !result ( Coeff.norm_mult y y )
     end in
     T.iter f w ;
     !result
  end
 | _ -> square_norm_frobenius ( to_half_full m ) ;;

(** {v norm_sum matrix v} *)
let rec norm_sum = function (m:t) ->
 match m with
 | Half_full_matrix w ->
  begin
   Array.fold_left Coeff.norm_add ( Coeff.norm_zero () ) ( Array.map V.norm_1 w )
  end
 | Sparse_tensor_matrix w ->
  begin
   let result = ref ( Coeff.norm_zero () ) in
    let f = function ( i , x ) ->
     begin
      let y = Coeff.norm x in
       result := Coeff.norm_add !result y
     end in
     T.iter f w ;
     !result
  end
 | _ -> norm_sum ( to_half_full m ) ;;

(** {v sparse_trace matrix v} *)
let sparse_trace = function (m:t) ->
 let d = sparse_diag_extract m in
  V.sum d ;;

(** {v full_trace matrix v} *)
let full_trace = function (m:t) ->
 let d = full_diag_extract m in
  Array.fold_left Coeff.add ( Coeff.zero () ) d ;;

(** {v trace matrix v} *)
let trace = function (m:t) ->
 try
  sparse_trace m
 with _ ->
  full_trace m ;;

(** {v scal_mult coefficient matrix v} *)
let scal_mult = fun (x:coeff) (m:t) ->
 match m with
 | Half_full_matrix w -> Half_full_matrix ( Array.map ( V.scal_mult x ) w )
 | Sparse_tensor_matrix w -> Sparse_tensor_matrix ( T.scal_mult x w )
 | Diff_to_scal_matrix ( y , w ) -> Diff_to_scal_matrix ( Coeff.mult x y , T.scal_mult x w ) 
 | Diff_to_multi_scal_matrix ( y , w ) -> Diff_to_multi_scal_matrix ( Array.map ( Coeff.mult x ) y , T.scal_mult x w )
 | Diff_to_diag_matrix ( a , w ) -> Diff_to_diag_matrix ( Array.map ( Coeff.mult x ) a , T.scal_mult x w ) 
 | Diff_to_multi_diag_matrix ( a , w ) -> Diff_to_multi_diag_matrix ( Array.map ( Array.map ( Coeff.mult x ) ) a , T.scal_mult x w ) ;;

(** {v add matrix1 matrix2 v} *)
let rec add = fun (m:t) (n:t) ->
 let test = ( description_eq_zero m , description_eq_zero n )
 and d = dimensions m in
  match test with
  | ( true , _ ) -> copy n
  | ( false , true ) -> copy m
  | _ ->
   begin
    match m with
    | Half_full_matrix w ->
     begin
      match n with
      | Half_full_matrix ww ->
       begin
        let r = Array.length w in
         let result = Array.map V.null ( Array.make r d.(1) ) in
          for i = 0 to pred r do
           result.(i) <- V.add w.(i) ww.(i)
          done ;
          Half_full_matrix result
       end
      | _ -> add m ( to_half_full n )
     end
    | Sparse_tensor_matrix w ->
     begin
      match n with
      | Half_full_matrix ww -> add n m
      | Sparse_tensor_matrix ww -> Sparse_tensor_matrix ( T.add w ww )
      | Diff_to_scal_matrix ( yy , ww ) -> Diff_to_scal_matrix ( Coeff.copy yy , T.add w ww )
      | Diff_to_multi_scal_matrix ( aa , ww ) -> Diff_to_multi_scal_matrix ( Array.map Coeff.copy aa , T.add w ww )
      | Diff_to_diag_matrix ( aa , ww ) -> Diff_to_diag_matrix ( Array.map Coeff.copy aa , T.add w ww )
      | Diff_to_multi_diag_matrix ( aa , ww ) -> Diff_to_multi_diag_matrix ( Array.map ( Array.map Coeff.copy ) aa , T.add w ww )
     end
    | Diff_to_scal_matrix ( y , w ) ->
     begin
      match n with
      | Half_full_matrix ww -> add n m
      | Sparse_tensor_matrix ww -> add n m
      | Diff_to_scal_matrix ( yy , ww ) -> Diff_to_scal_matrix ( Coeff.add y yy , T.add w ww )
      | Diff_to_multi_scal_matrix ( aa , ww ) ->
       begin
        let b = Array.map Coeff.copy aa
        and aalength = Array.length aa in
         b.( aalength / 2 ) <- Coeff.add b.( aalength / 2 ) y ;
         Diff_to_multi_scal_matrix ( b , T.add w ww )
       end
      | Diff_to_diag_matrix ( aa , ww ) ->
       begin
        let b = Array.map ( Coeff.add y ) aa in
         Diff_to_diag_matrix ( b , T.add w ww )
       end
      | Diff_to_multi_diag_matrix ( aa , ww ) ->
       begin
        let b = Array.map ( Array.map Coeff.copy ) aa
        and aalength = Array.length aa in
         b.( aalength / 2 ) <- Array.map ( Coeff.add y ) b.( aalength / 2 ) ;
         Diff_to_multi_diag_matrix ( b , T.add w ww )
       end
     end
    | Diff_to_multi_scal_matrix ( a , w ) ->
     begin
      match n with
      | Half_full_matrix ww -> add n m
      | Sparse_tensor_matrix ww -> add n m
      | Diff_to_scal_matrix ( yy , ww ) -> add n m
      | Diff_to_multi_scal_matrix ( aa , ww ) ->
       begin
        let b = Util.array_center_add Coeff.copy Coeff.add a aa in
         Diff_to_multi_scal_matrix ( b , T.add w ww )
       end
      | _ -> add ( to_half_full m ) ( to_half_full n )
     end
    | Diff_to_diag_matrix ( a , w ) ->
     begin
      match n with
      | Half_full_matrix ww -> add n m
      | Sparse_tensor_matrix ww -> add n m
      | Diff_to_scal_matrix ( yy , ww ) -> add n m
      | Diff_to_multi_scal_matrix ( aa , ww ) -> add n m
      | Diff_to_diag_matrix ( aa , ww ) ->
       begin
        let b = Util.array_map2 Coeff.add a aa in
         Diff_to_diag_matrix ( b , T.add w ww )
       end
      | Diff_to_multi_diag_matrix ( aa , ww ) ->
       begin
        let b = Util.array_center_add ( Array.map Coeff.copy ) ( Util.array_map2 Coeff.add ) [| a |] aa in
         Diff_to_multi_diag_matrix ( b , T.add w ww )
       end
     end
    | Diff_to_multi_diag_matrix ( a , w ) ->
     begin
      match n with
      | Half_full_matrix ww -> add n m
      | Sparse_tensor_matrix ww -> add n m
      | Diff_to_scal_matrix ( yy , ww ) -> add n m
      | Diff_to_multi_scal_matrix ( aa , ww ) -> add n m
      | Diff_to_diag_matrix ( aa , ww ) -> add n m
      | Diff_to_multi_diag_matrix ( aa , ww ) ->
       begin
        let b = Util.array_center_add ( Array.map Coeff.copy ) ( Util.array_map2 Coeff.add ) a aa in
         Diff_to_multi_diag_matrix ( b , T.add w ww )
       end
     end
   end ;;

(** {v sub matrix1 matrix2 v} *)
let sub = fun (m:t) (n:t) ->
 add m ( opp n ) ;;

(** {v eq matrix1 matrix2 v} *)
let eq = fun (m:t) (n:t) ->
 eq_zero ( sub m n ) ;;

(** {v twisted_mult matrix1 matrix2 v} *)
let rec twisted_mult = fun (m:t) (n:t) ->
 let dm = dimensions m
 and dn = dimensions n
 and test = ( description_eq_zero m , description_eq_zero n ) in
  let dm0 = dm.(0)
  and dm1 = dm.(1)
  and dn0 = dn.(0)
  and dn1 = dn.(1) in
   if not ( Index.eq dm1 dn1 ) then
    failwith "Bad dimensions in Sparse_matrix.Rng.twisted_mult." ;
   match test with
   | ( true , _ ) | ( false , true ) -> null [| dm0 ; dn0 |]
   | _ ->
    begin
     match m with
     | Half_full_matrix w  ->
      begin
       match n with
       | Half_full_matrix ww  ->
        begin
         let r = Index.to_int dm0 in
          let result = Array.map V.null ( Array.make r dn0 )
          and rr = pred r in
           for i = 0 to rr do
            let row_output = result.(i)
            and row_input_left = w.(i) in
             for j = 0 to rr do
              V.insert_add ( V.scal_prod row_input_left ww.(j) ) ( Index.from_int j ) row_output
             done ;
           done ;
           Half_full_matrix result
        end
       | Sparse_tensor_matrix ww ->
        begin
         let r = Index.to_int dm0 in
          let result = Array.map V.null ( Array.make r dn0 ) in
           let f = fun row_input_left row_output ( a , y ) ->
            begin
             let j = a.(0)
             and k = a.(1) in
              V.insert_add ( Coeff.mult ( V.raw_extract k row_input_left ) y ) j row_output
            end in
            for i = 0 to pred r do
             T.iter ( f w.(i) result.(i) ) ww ;
            done ;
            Half_full_matrix result
        end
       | Diff_to_scal_matrix ( xx , ww ) ->
        begin
         if Index.eq dn0 dn1 then
          add ( scal_mult xx m ) ( twisted_mult m ( Sparse_tensor_matrix ww ) )
         else
          twisted_mult m ( to_half_full n )
        end
       | _ -> twisted_mult m ( to_half_full n )
      end
     | Sparse_tensor_matrix w ->
      begin
       match n with
       | Half_full_matrix ww  ->
        begin
         let r = Index.to_int dn0 in
          let result = T.null [| dm0 ; dn0 |] in
           let f = fun j row_input_right ( a , y ) ->
            begin
             let i = a.(0)
             and k = a.(1) in
              T.insert_add ( Coeff.mult ( V.raw_extract k row_input_right ) y ) [| i ; j |] result
            end in
            for j = 0 to pred r do
             T.iter ( f ( Index.from_int j ) ww.(j) ) w ;
            done ;
            Sparse_tensor_matrix result
        end
       | Sparse_tensor_matrix ww ->
        begin
         let result = T.null [| dm0 ; dn0 |]
         and rows = sparse_tensor_dirty_rows_array w
         and columns = sparse_tensor_dirty_rows_array ww in
          let r = Array.length rows
          and c = Array.length columns in
           let dirty_right = Array.make c ( V.null dn1 )
           and cc = pred c in
            for i = 0 to cc do
             dirty_right.(i) <- tensor_row_extract columns.(i) ww
            done ;
            for i = 0 to pred r do
             let first_index = rows.(i) in
              let row_input_left = tensor_row_extract first_index w in
               for j = 0 to cc do
                let coefficient = V.scal_prod row_input_left dirty_right.(j) in
                 T.insert_add coefficient [| first_index ; columns.(j) |] result
               done ;
            done ;
            Sparse_tensor_matrix result
        end
       | Diff_to_scal_matrix ( xx , ww ) ->
        begin
         if Index.eq dn0 dn1 then
          add ( scal_mult xx m ) ( twisted_mult m ( Sparse_tensor_matrix ww ) )
         else
          twisted_mult m ( to_half_full n )
        end
       | _ -> twisted_mult m ( to_half_full n )
      end
     | Diff_to_scal_matrix ( x , w ) ->
      begin
       if Index.eq dm0 dm1 then
        add ( scal_mult x n ) ( twisted_mult ( Sparse_tensor_matrix w ) n )
       else
        twisted_mult ( to_half_full m ) n
      end
     | Diff_to_multi_scal_matrix ( a , w ) ->
      begin
       if Array.length a = 1 then
        twisted_mult ( Diff_to_scal_matrix ( a.(0) , w ) ) n
       else
        twisted_mult ( to_half_full m ) n
      end
     | _ -> twisted_mult ( to_half_full m ) n
    end ;;

(** {v mult matrix1 matrix2 v} *)
let rec mult = fun (m:t) (n:t) ->
 let dm = dimensions m
 and dn = dimensions n
 and test = ( description_eq_zero m , description_eq_zero n ) in
  let dm0 = dm.(0)
  and dm1 = dm.(1)
  and dn0 = dn.(0)
  and dn1 = dn.(1) in
   if not ( Index.eq dm1 dn0 ) then
    failwith "Bad dimensions in Sparse_matrix.Rng.mult." ;
   match test with
   | ( true , _ ) | ( false , true ) -> null [| dm0 ; dn1 |]
   | _ ->
    begin
     match m with
     | Half_full_matrix w  ->
      begin
       match n with
       | Half_full_matrix ww  -> twisted_mult m ( transpose ( to_sparse_tensor n ) )
       | Sparse_tensor_matrix ww ->
        begin
         let r = Index.to_int dm0 in
          let result = Array.map V.null ( Array.make r dn1 ) in
           let f = fun row_input_left row_output ( a , y ) ->
            begin
             let k = a.(0)
             and j = a.(1) in
              V.insert_add ( Coeff.mult ( V.raw_extract k row_input_left ) y ) j row_output
            end in
            for i = 0 to pred r do
             T.iter ( f w.(i) result.(i) ) ww ;
            done ;
            Half_full_matrix result
        end
       | Diff_to_scal_matrix ( xx , ww ) ->
        begin
         if Index.eq dn0 dn1 then
          add ( scal_mult xx m ) ( mult m ( Sparse_tensor_matrix ww ) )
         else
          mult m ( to_half_full n )
        end
       | _ -> mult m ( to_sparse_tensor n )
      end
     | Sparse_tensor_matrix w ->
      begin
       match n with
       | Sparse_tensor_matrix ww ->
        begin
         let result = T.null [| dm0 ; dn1 |]
         and rows = sparse_tensor_dirty_rows_array w
         and columns = sparse_tensor_dirty_columns_array ww in
          let r = Array.length rows
          and c = Array.length columns in
           let dirty_right = Array.map V.null ( Array.make c dn0 )
           and cc = pred c in
            for i = 0 to cc do
             dirty_right.(i) <- tensor_column_extract columns.(i) ww
            done ;
            for i = 0 to pred r do
             let first_index = rows.(i) in
              let row_input_left = tensor_row_extract first_index w in
               for j = 0 to cc do
                let coefficient = V.scal_prod row_input_left dirty_right.(j) in
                 T.insert_add coefficient [| first_index ; columns.(j) |] result
               done ;
            done ;
            Sparse_tensor_matrix result
        end
       | Diff_to_scal_matrix ( xx , ww ) ->
        begin
         if Index.eq dn0 dn1 then
          add ( Sparse_tensor_matrix ( T.scal_mult xx w ) ) ( mult m ( Sparse_tensor_matrix ww ) )
         else
          mult m ( to_half_full n )
        end
       | _ -> mult m ( to_sparse_tensor n )
      end
     | Diff_to_scal_matrix ( x , w ) ->
      begin
       if Index.eq dm0 dm1 then
        add ( scal_mult x n ) ( mult ( Sparse_tensor_matrix w ) n )
       else
        mult ( to_half_full m ) n
      end
     | Diff_to_multi_scal_matrix ( a , w ) ->
      begin
       if Array.length a = 1 then
        mult ( Diff_to_scal_matrix ( a.(0) , w ) ) n
       else
        mult ( to_half_full m ) n
      end
     | _ -> mult ( to_half_full m ) n
    end ;;

(** {v sparse_full_twisted_mult matrix1 matrix2 v} *)
let rec sparse_full_twisted_mult = fun (m:t) (n:coeff array array) ->
 let dm = dimensions m
 and rn = Array.length n
 and test = description_eq_zero m in
  let dm0 = dm.(0)
  and dm1 = dm.(1)
  and dn0 = Index.from_int rn in
   if ( Index.to_int dm1 ) <> rn then
    failwith "Bad dimensions in Sparse_matrix.Rng.sparse_full_twisted_mult." ;
   if test = true then
    null [| dm0 ; dn0 |]
   else
    begin
     match m with
     | Half_full_matrix w  ->
      begin
       let r = Index.to_int dm0 in
        let result = Array.map V.null ( Array.make r dn0 )
          and rr = pred r in
           for i = 0 to rr do
            let row_output = result.(i)
            and row_input_left = w.(i) in
             for j = 0 to rr do
              V.insert_add ( V.sparse_full_scal_prod row_input_left n.(j) ) ( Index.from_int j ) row_output
             done ;
           done ;
           Half_full_matrix result
      end
     | Sparse_tensor_matrix w ->
      begin
       let r = Index.to_int dn0 in
        let result = T.null [| dm0 ; dn0 |] in
         let f = fun j row_input_right ( a , y ) ->
          begin
           let i = a.(0)
           and k = a.(1) in
            T.insert_add ( Coeff.mult row_input_right.( Index.to_int k ) y ) [| i ; j |] result
          end in
          for j = 0 to pred r do
           T.iter ( f ( Index.from_int j ) n.(j) ) w ;
          done ;
          Sparse_tensor_matrix result
      end
     | _ -> sparse_full_twisted_mult ( to_half_full m ) n
    end ;;

(** {v sparse_full_mult matrix1 matrix2 v} *)
let sparse_full_mult = fun (m:t) (n:coeff array array) ->
 let dm = dimensions m
 and rn = Array.length n
 and cn = Array.length n.(0)
 and test = description_eq_zero m in
  let dm0 = dm.(0)
  and dm1 = dm.(1)
  and dn1 = Index.from_int cn in
   if ( Index.to_int dm1 ) <> rn then
    failwith "Bad dimensions in Sparse_matrix.Rng.sparse_full_mult." ;
   if test = true then
    null [| dm0 ; dn1 |]
   else
    sparse_full_twisted_mult m ( Util.transpose n ) ;;

(** {v full_sparse_twisted_mult matrix1 matrix2 v} *)
let rec full_sparse_twisted_mult = fun (m:coeff array array) (n:t) ->
 let dn = dimensions n
 and rm = Array.length m
 and cm = Array.length m.(0)
 and test = description_eq_zero n in
  let dn0 = dn.(0)
  and dn1 = dn.(1)
  and dm0 = Index.from_int rm in
   if cm <> ( Index.to_int dn1 ) then
    failwith "Bad dimensions in Sparse_matrix.Rng.full_sparse_twisted_mult." ;
   if test = true then
    null [| dm0 ; dn0 |]
   else
    begin
     match n with
     | Half_full_matrix w ->
      begin
       let result = Array.map V.null ( Array.make rm dn0 )
       and rr = pred rm
       and cc = pred cm in
        for i = 0 to rr do
         let row_output = result.(i)
         and row_input_left = m.(i) in
          for j = 0 to cc do
           V.insert_add ( V.sparse_full_scal_prod w.(j) row_input_left ) ( Index.from_int j ) row_output
          done ;
        done ;
        Half_full_matrix result
      end
     | Sparse_tensor_matrix w ->
      begin
       let result = Array.map V.null ( Array.make rm dn0 ) in
        let f = fun row_in row_out ( a , x ) -> V.insert_add ( Coeff.mult x row_in.( Index.to_int a.(1) ) ) a.(0) row_out in
         for i = 0 to pred rm do
          let row_output = result.(i)
          and row_input_left = m.(i) in
           T.iter ( f row_input_left row_output ) w
         done ;
         Half_full_matrix result
      end
     | _ -> full_sparse_twisted_mult m ( to_half_full n )
    end ;;

(** {v full_sparse_mult matrix1 matrix2 v} *)
let rec full_sparse_mult = fun (m:coeff array array) (n:t) ->
 let dn = dimensions n
 and rm = Array.length m
 and cm = Array.length m.(0)
 and test = description_eq_zero n in
  let dn0 = dn.(0)
  and dn1 = dn.(1)
  and dm0 = Index.from_int rm in
   if cm <> ( Index.to_int dn0 ) then
    failwith "Bad dimensions in Sparse_matrix.Rng.full_sparse_mult." ;
   if test = true then
    null [| dm0 ; dn1 |]
   else
    transpose ( sparse_full_twisted_mult ( transpose n ) m ) ;;

(** {v triple_mult matrix1 matrix2 matrix3 v} *)
let triple_mult = fun (m:t) (n:t) (p:t) ->
 twisted_mult m ( twisted_mult ( transpose p ) n ) ;;

(** {v commut matrix1 matrix2 v} *)
let commut = fun (m:t) (n:t) ->
 sub ( mult m n ) ( mult n m ) ;;





(** {C § § § } *)




end ;;







module Field (Index:Data.Index_type) (Hasher:Hash.Hash_type with type t = Index.t) (Coeff:Data.Field_coeff_type) = struct


include Rng (Index) (Hasher) (Coeff) ;;



module U = Sparse_vector.Field (Index) (Hasher) (Coeff) ;;



module S = Sparse_tensor.Field (Index) (Hasher) (Coeff) ;;



(** The coefficient two is used at different places for matrix calculations.

Le coefficient deux est utilisé à différents endroits pour les calculs matriciels. *)
let coeff_two = function () ->
 Coeff.add ( Coeff.one () ) ( Coeff.one () );;


(** The coefficient one half is used at different places for matrix calculations.
The characteristic of the field is supposed to be different from two.

Le coefficient un demi est utilisé à différents endroits pour les calculs matriciels.
On suppose que la caractéristique du corps est différente de deux. *)
let coeff_half = function () ->
 Coeff.inv ( coeff_two () ) ;;

(** {v scal_left_div scalar matrix v} *)
let scal_left_div = fun (x:coeff) (m:t) ->
 scal_mult ( Coeff.inv x ) m ;;

(** {v reciprocal matrix v} *)
let reciprocal = function (m:t) ->
 scal_left_div ( Coeff.norm_inject ( square_norm_frobenius m ) ) m ;;

(** {v sym matrix v} *)
let sym = function (m:t) ->
 scal_mult ( coeff_half () ) ( add m ( transpose m ) ) ;;

(** {v antisym matrix v} *)
let antisym = function (m:t) ->
 scal_mult ( coeff_half () ) ( sub m ( transpose m ) ) ;;

(** {v in_place_pivot_downward matrix1 matrix2 v} *)
let in_place_pivot_downward = fun (m:t) (p:t) ->
 let coeff = ref ( Coeff.zero () )
 and error_message = "Non invertible left matrix in Sparse_matrix.Field.in_place_pivot_downward." ;
 and dim = dimensions m
 and dip = dimensions p
 and i = ref 0 in
  let dim0 = dim.(0)
  and dim1 = dim.(1)
  and dip0 = dip.(0)
  and dip1 = dip.(1)
  and norm_coeff = ref ( Coeff.norm !coeff )
  and ii = ref ( Index.from_int !i ) in
   assert ( Index.eq dim0 dim1 ) ;
   assert ( Index.eq dim0 dip0 ) ;
   let row_left = ref ( U.null dim1 )
   and row_right = ref ( U.null dip1 )
   and substraction_row_left = ref ( U.null dim1 )
   and substraction_row_right = ref ( U.null dip1 )
   and row_output_left = ref ( U.null dim1 )
   and row_output_right = ref ( U.null dip1 )
   and col = ref ( U.null dim0 )
   and index = ref !ii
   and r = Index.to_int dim0 in
    let rr = pred r in
     while !i <= rr do
      ii := Index.from_int !i ;
      index := !ii ;
      coeff := extract !ii !ii m ;
      norm_coeff := Coeff.norm !coeff ;
      col := column_extract !ii m ;
      let f = function ( j , x ) ->
       begin
        if Index.compare j !ii > 0 then
         begin
          let nx = Coeff.norm x in
           if Coeff.norm_compare nx !norm_coeff > 0 then
            begin
             coeff := x ;
             norm_coeff := nx ;
             index := j ;
            end
         end
       end in
       U.iter f !col ;
       if Coeff.eq_zero !coeff then
        failwith error_message ;
       if not ( Index.eq !index !ii ) then
        begin
         row_exchange !index !ii m ;
         row_exchange !index !ii p ;
        end ;
       col := column_extract !ii m ;
       row_left := row_extract !ii m ;
       substraction_row_left := U.scal_left_div !coeff !row_left ;
       U.replace ( Coeff.one () ) !ii !substraction_row_left ;
       row_replace !substraction_row_left !ii m ;
       row_right := row_extract !ii p ;
       substraction_row_right := U.scal_left_div !coeff !row_right ;
       row_replace !substraction_row_right !ii p ;
       let g = function ( j , x ) ->
        begin
         if Index.compare j !ii > 0 then
          begin
           row_output_left := row_extract j m ;
           row_output_left := V.sub !row_output_left ( V.scal_mult x !substraction_row_left ) ;
           U.remove !ii !row_output_left ;
           row_replace !row_output_left j m ;
           row_output_right := row_extract j p ;
           row_output_right := V.sub !row_output_right ( V.scal_mult x !substraction_row_right ) ;
           row_replace !row_output_right j p ;
          end
        end in
        U.iter g !col ;
        incr i ;
     done ;;

(** {v pivot_downward matrix1 matrix2 v} *)
let pivot_downward = fun (m:t) (p:t) ->
 let mm = copy m
 and pp = copy p in
  in_place_pivot_downward mm pp ;
  [| mm ; pp |] ;;

(** {v invertibility matrix v} *)
let invertibility = function (m:t) ->
 let mm = copy m
 and coefficient = ref ( Coeff.zero () )
 and coeff = ref ( Coeff.zero () )
 and dim = dimensions m
 and hh = ref ( Index.zero () )
 and i = ref 0 in
  let dim0 = dim.(0)
  and dim1 = dim.(1)
  and norm_coeff = ref ( Coeff.norm !coeff )
  and ii = ref ( Index.from_int !i ) in
   assert ( Index.eq dim0 dim1 ) ;
   let row_left = ref ( U.null dim1 )
   and substraction_row_left = ref ( U.null dim1 )
   and row_output_left = ref ( U.null dim1 )
   and col = ref ( U.null dim0 )
   and index = ref !ii
   and r = Index.to_int dim0 in
    let rr = pred r in
     try
      begin
       while !i <= rr do
        ii := Index.from_int !i ;
        index := !ii ;
        coeff := extract !ii !ii mm ;
        norm_coeff := Coeff.norm !coeff ;
        col := column_extract !ii mm ;
        let f = function ( j , x ) ->
         begin
          if Index.compare j !ii > 0 then
           begin
            let nx = Coeff.norm x in
             if Coeff.norm_compare nx !norm_coeff > 0 then
              begin
               coeff := x ;
               norm_coeff := nx ;
               index := j ;
              end
           end
         end in
         U.iter f !col ;
         if Coeff.eq_zero !coeff then
          failwith "The end." ;
         if not ( Index.eq !index !ii ) then
          row_exchange !index !ii mm ;
         row_left := row_extract !ii mm ;
         substraction_row_left := U.scal_left_div !coeff !row_left ;
         U.replace ( Coeff.one () ) !ii !substraction_row_left ;
         row_replace !substraction_row_left !ii mm ;
         for h = !i + 1 to rr do
          hh := Index.from_int h ;
          row_output_left := row_extract !hh mm ;
          coefficient := U.raw_extract !ii !row_output_left ;
          row_output_left := V.sub !row_output_left ( V.scal_mult !coefficient !substraction_row_left ) ;
          U.remove !ii !row_output_left ;
          row_replace !row_output_left !hh mm ;
         done ;
         incr i ;
       done ;
       true
      end
     with _ ->
      false ;;

(** {v det matrix v} *)
let det = function (m:t) ->
 let mm = copy m
 and result = ref ( Coeff.one () )
 and coefficient = ref ( Coeff.zero () )
 and coeff = ref ( Coeff.zero () )
 and dim = dimensions m
 and hh = ref ( Index.zero () )
 and i = ref 0 in
  let dim0 = dim.(0)
  and dim1 = dim.(1)
  and norm_coeff = ref ( Coeff.norm !coeff )
  and ii = ref ( Index.from_int !i ) in
   assert ( Index.eq dim0 dim1 ) ;
   let row_left = ref ( U.null dim1 )
   and substraction_row_left = ref ( U.null dim1 )
   and row_output_left = ref ( U.null dim1 )
   and col = ref ( U.null dim0 )
   and index = ref !ii
   and r = Index.to_int dim0 in
    let rr = pred r in
     try
      begin
       while !i <= rr do
        ii := Index.from_int !i ;
        index := !ii ;
        coeff := extract !ii !ii mm ;
        norm_coeff := Coeff.norm !coeff ;
        col := column_extract !ii mm ;
        let f = function ( j , x ) ->
         begin
          if Index.compare j !ii > 0 then
           begin
            let nx = Coeff.norm x in
             if Coeff.norm_compare nx !norm_coeff > 0 then
              begin
               coeff := x ;
               norm_coeff := nx ;
               index := j ;
              end
           end
         end in
         U.iter f !col ;
         if Coeff.eq_zero !coeff then
          failwith "The end." ;
         if not ( Index.eq !index !ii ) then
          row_exchange !index !ii mm ;
         row_left := row_extract !ii mm ;
         result := Coeff.mult !result !coeff ;
         substraction_row_left := U.scal_left_div !coeff !row_left ;
         U.replace ( Coeff.one () ) !ii !substraction_row_left ;
         row_replace !substraction_row_left !ii mm ;
         for h = !i + 1 to rr do
          hh := Index.from_int h ;
          row_output_left := row_extract !hh mm ;
          coefficient := U.raw_extract !ii !row_output_left ;
          row_output_left := V.sub !row_output_left ( V.scal_mult !coefficient !substraction_row_left ) ;
          U.remove !ii !row_output_left ;
          row_replace !row_output_left !hh mm ;
         done ;
         incr i ;
       done ;
       !result
      end
     with _ ->
      Coeff.zero () ;;


(** {v in_place_pivot_upward matrix1 matrix2 v} The left matrix is supposed to be upper triangular with 
ones on the diagonal.

La matrice de gauche est supposée triangulaire supérieure avec des uns sur la diagonale. *)
let in_place_pivot_upward = fun (m:t) (p:t) ->
 let dim = dimensions m
 and dip = dimensions p
 and hh = ref ( Index.zero () ) in
  let dim0 = dim.(0)
  and dim1 = dim.(1)
  and dip0 = dip.(0)
  and dip1 = dip.(1) in
   assert ( Index.eq dim0 dim1 ) ;
   assert ( Index.eq dim0 dip0 ) ;
   let row_left = ref ( U.null dim1 )
   and row_right = ref ( U.null dip1 )
   and row_output_left = ref ( U.null dim1 )
   and row_output_right = ref ( U.null dip1 )
   and col = ref ( U.null dim0 )
   and r = Index.to_int dim0 in
    let rr = pred r in
     let i = ref rr in
      let ii = ref ( Index.from_int !i ) in
       while !i > 0 do
        ii := Index.from_int !i ;
        row_left := row_extract !ii m ;
        row_right := row_extract !ii p ;
        col := column_extract !ii m ;
        let g = function ( j , x ) ->
         begin
          if Index.compare j !ii < 0 then
           begin
            row_output_left := row_extract !hh m ;
            row_output_left := V.sub !row_output_left ( V.scal_mult x !row_left ) ;
            U.remove !ii !row_output_left ;
            row_replace !row_output_left !hh m ;
            row_output_right := row_extract !hh p ;
            row_output_right := V.sub !row_output_right ( V.scal_mult x !row_right ) ;
            row_replace !row_output_right !hh p ;
           end
         end in
         U.iter g !col ;
         decr i ;
       done ;;

(** {v pivot_upward matrix1 matrix2 v} *)
let pivot_upward = fun (m:t) (p:t) ->
 let mm = copy m
 and pp = copy p in
  in_place_pivot_upward mm pp ;
  [| mm ; pp |] ;;

(** {v inv matrix v} *)
let inv = function (m:t) ->
 let p = Diff_to_scal_matrix ( Coeff.one () , T.null ( Array.make 2 ( dimensions m ).(0) ) )
 and mm = copy m in
  in_place_pivot_downward mm p ;
  in_place_pivot_upward mm p ;
  p ;;

(** {v left_quotient matrix1 matrix2 v} *)
let left_quotient = fun (m:t) (p:t) ->
 let mm = copy m
 and pp = copy p in
  in_place_pivot_downward mm pp ;
  in_place_pivot_upward mm pp ;
  pp ;;

(** {v right_quotient matrix1 matrix2 v} *)
let right_quotient = fun (m:t) (p:t) ->
 let mm = transpose m
 and pp = transpose p in
  in_place_pivot_downward pp mm ;
  in_place_pivot_upward pp mm ;
  transpose mm ;;

(** {v cond norm invertor matrix v} *)
let cond = fun norm invertor (m:t) ->
 Coeff.norm_mult ( norm m ) ( norm ( invertor m ) ) ;;

(** {v naive_solve matrix vector v} *)
let naive_solve = fun (m:t) (v:U.t) ->
 let mm = inv m in
  matrix_sparse_vector_sparse_prod mm v ;;

(** {v solve matrix vector v} *)
let solve = fun (m:t) (v:U.t) ->
 let p = sparse_vector_to_column_matrix v in
  column_extract ( Index.zero () ) ( left_quotient m p ) ;;

(** {v full_solve matrix vector v} *)
let full_solve = fun (m:t) (v:coeff array) ->
 let p = to_sparse ( -1 ) 1. ( Array.map ( Array.make 1 ) v ) in
  Array.map ( function x -> x.(0) ) ( to_full ( left_quotient m p ) ) ;;


(** {v tune_inv matrix inverse_candidate v} *)
let tune_inv = fun (x:t) (y:t) ->
 let two = Diff_to_scal_matrix ( coeff_two () , S.null ( Array.make 2 ( dimensions x ).(0) ) )
 and right_product = mult x y in
  let difference = sub two right_product in
   mult y difference ;;

(** {v approx_inv distance invertor matrix v} *)
let approx_inv = fun distance invertor (x:t) ->
 let y = invertor x in
  let product = mult x y
  and identity = Diff_to_scal_matrix ( Coeff.one () , T.null ( Array.make 2 ( dimensions x ).(0) ) )
  and result = tune_inv x y in
   let error0 = distance ( sub product identity )
   and new_product = mult x result in
    let error1 = distance ( sub new_product identity ) in
     if Coeff.norm_compare error1 error0 >=0 then
      ( y , error0 ) 
     else
      ( result , error1 ) ;;

(** {v extrap_inv parameter matrix v} *)
let extrap_inv = fun (parameter:Coeff.t) (x:t) ->
 let y = inv x in
  let yy = tune_inv x y in
   add yy ( scal_mult parameter ( sub yy y ) ) ;;

(** {v approx_solve distance matrix vector v} *)
let approx_solve = fun distance (m:t) (v:U.t) ->
 let mm = approx_inv distance inv m in
  matrix_sparse_vector_sparse_prod ( fst mm ) v ;;

(** {v approx_full_solve distance matrix vector v} *)
let approx_full_solve = fun distance (m:t) (v:coeff array) ->
 let mm = approx_inv distance inv m in
  matrix_full_vector_sparse_prod ( fst mm ) v ;;


(** {v iterate exponent matrix vector v} *)
let iterate = fun (s:int) (x:t) (v:U.t) ->
 let y = ref v in
  if s >= 0 then
   begin
    for i = 1 to s do
     y := matrix_sparse_vector_sparse_prod x !y ;
    done ;
   end
  else
   begin
    let xx = fst ( approx_inv norm_inf inv x ) in
     for i = 1 to ( abs s ) do
      y := matrix_sparse_vector_sparse_prod xx !y ;
     done ;
   end ;
  !y ;;

(** {v int_pow exponent matrix v} *)
let rec int_pow = fun (s:int) (x:t) ->
 if s >= 0 then
  begin
   if s = 0 then
    Diff_to_scal_matrix ( Coeff.one () , T.null ( dimensions x ) )
   else
    begin
     let n = s / 2 in
      let factor = int_pow n x in
       let prod = mult factor factor in
        if s mod 2 = 0 then
         prod
        else
         mult prod x
    end
  end
 else
  int_pow ( abs s ) ( fst ( approx_inv norm_inf inv x ) ) ;;

(** {v find_first_pivot_downward tensor v} *)
let find_first_pivot_downward = function (w:S.t) ->
 let coeff = ref ( Coeff.zero () )
 and index = ref ( Array.make 2 ( Index.witness () ) ) in
  let f = function ( i , x ) ->
   begin
    let i0 = i.(0)
    and i1 = i.(1)
    and index0 = !index.(0)
    and index1 = !index.(1) in
     if ( Index.compare i0 i1 > 0 ) && ( ( Index.eq index1 ( Index.witness () ) ) || ( Index.compare i1 index1 < 0 ) || ( ( Index.eq i1 index1 ) && ( Index.compare i0 index0 < 0 ) ) ) then
      begin
       coeff := x ;
       index := i ;
      end
   end in
   S.iter f w ;
   ( !index , !coeff ) ;;

(** {v find_first_pivot_upward tensor v} *)
let find_first_pivot_upward = function (w:S.t) ->
 let coeff = ref ( Coeff.zero () )
 and index = ref ( Array.make 2 ( Index.witness () ) ) in
  let f = function ( i , x ) ->
   begin
    let i0 = i.(0)
    and i1 = i.(1)
    and index0 = !index.(0)
    and index1 = !index.(1) in
     if ( Index.compare i0 i1 < 0 ) && ( ( Index.eq index1 ( Index.witness () ) ) || ( Index.compare i1 index1 > 0 ) || ( ( Index.eq i1 index1 ) && ( Index.compare i0 index0 > 0 ) ) ) then
      begin
       coeff := x ;
       index := i ;
      end
   end in
   S.iter f w ;
   ( !index , !coeff ) ;;

(** {v in_place_diff_to_id_pivot_downward matrix1 matrix2 v} *)
let in_place_diff_to_id_pivot_downward = fun (m:t) (p:t) ->
 let dim = dimensions m
 and error_string = "The first argument must be Diff_to_scal_matrix ( Coeff.one , sparse_tensor ) in Sparse_matrix.Field.diff_to_id_pivot_downward."
 and error_message = "Non invertible left matrix in Sparse_matrix.Field.in_place_diff_to_id_pivot_downward" in
  match m with
  | Diff_to_scal_matrix ( x , w ) ->
   begin
    if Coeff.eq_one x then
     begin
      let coefficient = ref ( Coeff.zero () )
      and coeff = ref ( Coeff.zero () )
      and pivot = ref ( find_first_pivot_downward w )
      and dip = dimensions p in
       let i = ref ( fst !pivot ).(1)
       and dim0 = dim.(0)
       and dim1 = dim.(1)
       and dip1 = dip.(1)
       and norm_coeff = ref ( Coeff.norm !coeff ) in
        assert ( Index.eq dim0 dim1 ) ;
        assert ( Index.eq dim0 dip.(0) ) ;
        let row_left = ref ( U.null dim1 )
        and row_right = ref ( U.null dip1 )
        and si = ref ( Index.succ !i )
        and substraction_row_left = ref ( U.null dim1 )
        and substraction_row_right = ref ( U.null dip1 )
        and row_output_left = ref ( U.null dim1 )
        and row_output_right = ref ( U.null dip1 )
        and rr = Index.pred dim0
        and cc = Index.pred dim1
        and index = ref !i in
         while not ( Index.eq !i ( Index.witness () ) ) do
          index := !i ;
          coeff := extract !i !i m ;
          norm_coeff := Coeff.norm !coeff ;
          let f = function ( j , x ) ->
           begin
            let nx = Coeff.norm x in
             if Coeff.norm_compare nx !norm_coeff > 0 then
              begin
               coeff := x ;
               norm_coeff := nx ;
               index := j.(0) ;
              end
           end in
           tensor_sub_column_iter f !i !si rr w ;
           if Coeff.eq_zero !coeff then
            failwith error_message ;
           if not ( Index.eq !index !i ) then
            begin
             row_exchange !index !i m ;
             row_exchange !index !i p ;
            end ;
           row_left := row_extract !i m ;
           substraction_row_left := U.scal_left_div !coeff !row_left ;
           U.replace ( Coeff.one () ) !i !substraction_row_left ;
           row_replace !substraction_row_left !i m ;
           row_right := row_extract !i p ;
           substraction_row_right := U.scal_left_div !coeff !row_right ;
           row_replace !substraction_row_right !i p ;
           let g = function ( j , x ) ->
            begin
             let j0 = j.(0) in
              row_output_left := row_extract j0 m ;
              coefficient := U.raw_extract !i !row_output_left ;
              row_output_left := U.sub !row_output_left ( V.scal_mult !coefficient !substraction_row_left ) ;
              U.remove !i !row_output_left ;
              row_replace !row_output_left j0 m ;
              row_output_right := row_extract j0 p ;
              row_output_right := U.sub !row_output_right ( V.scal_mult !coefficient !substraction_row_right ) ;
              row_replace !row_output_right j0 p ;
            end in
            tensor_sub_column_iter g !i !si rr w ;
            pivot := find_first_pivot_downward w ;
            i := ( fst !pivot ).(1) ;
            si := Index.succ !i ;
         done ;
         let ff = function ( j , x ) ->
          begin
           let j0 = j.(0) in
            if Index.eq j0 j.(1) then
             begin
              coefficient := Coeff.add x ( Coeff.one () ) ;
              if Coeff.eq_zero !coefficient then
               failwith error_message ;
              coeff := Coeff.inv !coefficient ;
              row_output_left := tensor_sub_row_extract j0 ( Index.succ j0 ) cc w ;
              row_output_left := U.scal_mult !coeff !row_output_left ;
              tensor_row_replace !row_output_left j0 w ;
              row_output_right := row_extract j0 p ;
              row_output_right := U.scal_mult !coeff !row_output_right ;
              row_replace !row_output_right j0 p ;
             end
          end in
          S.iter ff w ;
     end
    else
     failwith error_string
   end
  | _ -> failwith error_string ;;

(** {v diff_to_id_pivot_downward matrix1 matrix2 v} *)
let diff_to_id_pivot_downward = fun (m:t) (p:t) ->
 let mm = copy m
 and pp = copy p in
  in_place_diff_to_id_pivot_downward mm pp ;
  [| mm ; pp |] ;;

(** {v diff_to_id_invertibility matrix v} *)
let diff_to_id_invertibility = function (mm:t) ->
 let m = copy mm
 and dim = dimensions mm
 and error_string = "The argument must be Diff_to_scal_matrix ( Coeff.one , sparse_tensor ) in Sparse_matrix.Field.diff_to_id_invertibility."
 and error_message = "Not invertible." in
  match m with
  | Diff_to_scal_matrix ( x , w ) ->
   begin
    if Coeff.eq_one x then
     begin
      let coefficient = ref ( Coeff.zero () )
      and result = ref true
      and coeff = ref ( Coeff.zero () )
      and pivot = ref ( find_first_pivot_downward w ) in
       let i = ref ( fst !pivot ).(1)
       and dim0 = dim.(0)
       and dim1 = dim.(1)
       and norm_coeff = ref ( Coeff.norm !coeff ) in
        assert ( Index.eq dim0 dim1 ) ;
        let row_left = ref ( U.null dim1 )
        and si = ref ( Index.succ !i )
        and substraction_row_left = ref ( U.null dim1 )
        and row_output_left = ref ( U.null dim1 )
        and rr = Index.pred dim0
        and index = ref !i in
         while not ( Index.eq !i ( Index.witness () ) ) do
          index := !i ;
          coeff := extract !i !i m ;
          norm_coeff := Coeff.norm !coeff ;
          let f = function ( j , x ) ->
           begin
            let nx = Coeff.norm x in
             if Coeff.norm_compare nx !norm_coeff > 0 then
              begin
               coeff := x ;
               norm_coeff := nx ;
               index := j.(0) ;
              end
           end in
           tensor_sub_column_iter f !i !si rr w ;
           if Coeff.eq_zero !coeff then
            begin
             result := false ;
             i := Index.witness () ;
            end
           else
            begin
             if not ( Index.eq !index !i ) then
              begin
               row_exchange !index !i m ;
              end ;
             row_left := row_extract !i m ;
             substraction_row_left := U.scal_left_div !coeff !row_left ;
             U.replace ( Coeff.one () ) !i !substraction_row_left ;
             row_replace !substraction_row_left !i m ;
             let g = function ( j , x ) ->
              begin
               let j0 = j.(0) in
                row_output_left := row_extract j0 m ;
                coefficient := U.raw_extract !i !row_output_left ;
                row_output_left := U.sub !row_output_left ( V.scal_mult !coefficient !substraction_row_left ) ;
                U.remove !i !row_output_left ;
                row_replace !row_output_left j0 m ;
              end in
              tensor_sub_column_iter g !i !si rr w ;
              pivot := find_first_pivot_downward w ;
              i := ( fst !pivot ).(1) ;
              si := Index.succ !i ;
            end
         done ;
         let ff = function ( j , x ) ->
          begin
           let j0 = j.(0) in
            if Index.eq j0 j.(1) then
             begin
              coefficient := Coeff.add x ( Coeff.one () ) ;
              if Coeff.eq_zero !coefficient then
               failwith error_message ;
             end
          end in
          begin
           try
            S.iter ff w
           with _ ->
            result := false
          end ;
          !result ;
     end
    else
     failwith error_string
   end
  | _ -> failwith error_string ;;

(** {v diff_to_id_det matrix v} *)
let diff_to_id_det = function (mm:t) ->
 let m = copy mm
 and dim = dimensions mm
 and error_string = "The argument must be Diff_to_scal_matrix ( Coeff.one , sparse_tensor ) in Sparse_matrix.Field.diff_to_id_det."
 and error_message = "Not invertible." in
  match m with
  | Diff_to_scal_matrix ( x , w ) ->
   begin
    if Coeff.eq_one x then
     begin
      let coefficient = ref ( Coeff.zero () )
      and result = ref ( Coeff.one () )
      and coeff = ref ( Coeff.zero () )
      and pivot = ref ( find_first_pivot_downward w ) in
       let i = ref ( fst !pivot ).(1)
       and dim0 = dim.(0)
       and dim1 = dim.(1)
       and norm_coeff = ref ( Coeff.norm !coeff ) in
        assert ( Index.eq dim0 dim1 ) ;
        let row_left = ref ( U.null dim1 )
        and si = ref ( Index.succ !i )
        and substraction_row_left = ref ( U.null dim1 )
        and row_output_left = ref ( U.null dim1 )
        and rr = Index.pred dim0
        and index = ref !i in
         while not ( Index.eq !i ( Index.witness () ) ) do
          index := !i ;
          coeff := extract !i !i m ;
          norm_coeff := Coeff.norm !coeff ;
          let f = function ( j , x ) ->
           begin
            let nx = Coeff.norm x in
             if Coeff.norm_compare nx !norm_coeff > 0 then
              begin
               coeff := x ;
               norm_coeff := nx ;
               index := j.(0) ;
              end
           end in
           tensor_sub_column_iter f !i !si rr w ;
           if Coeff.eq_zero !coeff then
            begin
             result := Coeff.zero () ;
             i := Index.witness () ;
            end
           else
            begin
             result := Coeff.mult !result !coeff ;
             if not ( Index.eq !index !i ) then
              begin
               row_exchange !index !i m ;
              end ;
             row_left := row_extract !i m ;
             substraction_row_left := U.scal_left_div !coeff !row_left ;
             U.replace ( Coeff.one () ) !i !substraction_row_left ;
             row_replace !substraction_row_left !i m ;
             let g = function ( j , x ) ->
              begin
               let j0 = j.(0) in
                row_output_left := row_extract j0 m ;
                coefficient := U.raw_extract !i !row_output_left ;
                row_output_left := U.sub !row_output_left ( V.scal_mult !coefficient !substraction_row_left ) ;
                U.remove !i !row_output_left ;
                row_replace !row_output_left j0 m ;
              end in
              tensor_sub_column_iter g !i !si rr w ;
              pivot := find_first_pivot_downward w ;
              i := ( fst !pivot ).(1) ;
              si := Index.succ !i ;
            end
         done ;
         let ff = function ( j , x ) ->
          begin
           let j0 = j.(0) in
            if Index.eq j0 j.(1) then
             begin
              coefficient := Coeff.add x ( Coeff.one () ) ;
              if Coeff.eq_zero !coefficient then
               failwith error_message
              else
               result := Coeff.mult !result !coefficient ;
             end
          end in
          begin
           try
            S.iter ff w
           with _ ->
            result := Coeff.zero ()
          end ;
         !result ;
     end
    else
     failwith error_string
   end
  | _ -> failwith error_string ;;


(** {v in_place_diff_to_id_pivot_upward matrix1 matrix2 v} The left matrix is supposed to be 
upper triangular with ones on the diagonal.

La matrice de gauche est supposée triangulaire supérieure avec des uns sur la diagonale. *)
let in_place_diff_to_id_pivot_upward = fun (m:t) (p:t) ->
 let dim = dimensions m
 and error_string = "The first argument must be Diff_to_scal_matrix ( Coeff.one , sparse_tensor ) in Sparse_matrix.Field.diff_to_id_pivot_upward." in
  match m with
  | Diff_to_scal_matrix ( x , w ) ->
   begin
    if Coeff.eq_one x then
     begin
      let coefficient = ref ( Coeff.zero () )
      and pivot = ref ( find_first_pivot_upward w )
      and z = Index.zero ()
      and dip = dimensions p in
       let i = ref ( fst !pivot ).(1)
       and dim0 = dim.(0)
       and dim1 = dim.(1)
       and dip0 = dip.(0)
       and dip1 = dip.(1) in
        assert ( Index.eq dim0 dim1 ) ;
        assert ( Index.eq dim0 dip0 ) ;
        let row_left = ref ( U.null dim1 )
        and row_right = ref ( U.null dip1 )
        and row_output_left = ref ( U.null dim1 )
        and row_output_right = ref ( U.null dip1 )
        and pi = ref ( Index.pred !i ) in
         while not ( Index.eq !i ( Index.witness () ) ) do
          row_left := row_extract !i m ;
          row_right := row_extract !i p ;
          let g = function ( j , x ) ->
           begin
            let j0 = j.(0) in
             row_output_left := row_extract j0 m ;
             coefficient := U.raw_extract !i !row_output_left ;
             row_output_left := V.sub !row_output_left ( V.scal_mult !coefficient !row_left ) ;
             U.remove !i !row_output_left ;
             row_replace !row_output_left j0 m ;
             row_output_right := row_extract j0 p ;
             row_output_right := V.sub !row_output_right ( V.scal_mult !coefficient !row_right ) ;
             row_replace !row_output_right j0 p ;
           end in
           tensor_sub_column_iter g !i z !pi w ;
           pivot := find_first_pivot_downward w ;
           i := ( fst !pivot ).(1) ;
           pi := Index.pred !i ;
         done ;
     end
    else
     failwith error_string
   end
  | _ -> failwith error_string ;;


(** {v diff_to_id_pivot_upward matrix1 matrix2 v} The left matrix is supposed to be 
upper triangular with ones on the diagonal.

La matrice de gauche est supposée triangulaire supérieure avec des uns sur la diagonale. *)
let diff_to_id_pivot_upward = fun (m:t) (p:t) ->
 let mm = copy m
 and pp = copy p in
  in_place_diff_to_id_pivot_upward mm pp ;
  [| mm ; pp |] ;;

(** {v diff_to_id_inv matrix v} *)
let diff_to_id_inv = function (m:t) ->
 let p = Diff_to_scal_matrix ( Coeff.one () , T.null ( Array.make 2 ( dimensions m ).(0) ) )
 and mm = copy m in
  in_place_diff_to_id_pivot_downward mm p ;
  in_place_diff_to_id_pivot_upward mm p ;
  p ;;


(** {v diff_to_id_left_quotient matrix1 matrix2 v} *)
let diff_to_id_left_quotient = fun (m:t) (p:t) ->
 let mm = copy m
 and pp = copy p in
  in_place_diff_to_id_pivot_downward mm pp ;
  in_place_diff_to_id_pivot_upward mm pp ;
  pp ;;

(** {v diff_to_id_right_quotient matrix1 matrix2 v} *)
let diff_to_id_right_quotient = fun (m:t) (p:t) ->
 let mm = transpose m
 and pp = transpose p in
  in_place_diff_to_id_pivot_downward pp mm ;
  in_place_diff_to_id_pivot_upward pp mm ;
  transpose mm ;;

(** {v diff_to_id_solve matrix vector v} *)
let diff_to_id_solve = fun (m:t) (v:U.t) ->
 let p = sparse_vector_to_column_matrix v in
  column_extract ( Index.zero () ) ( diff_to_id_left_quotient m p ) ;;

(** {v diff_to_id_full_solve matrix vector v} *)
let diff_to_id_full_solve = fun (m:t) (v:coeff array) ->
 let p = to_sparse ( -1 ) 1. ( Array.map ( Array.make 1 ) v ) in
  Array.map ( function x -> x.(0) ) ( to_full ( diff_to_id_left_quotient m p ) ) ;;

(** {v diff_to_id_tune_inv matrix v} *)
let diff_to_id_tune_inv = function (m:t) ->
 tune_inv m ( diff_to_id_inv m ) ;;

(** {v diff_to_id_tune_solve matrix vector v} *)
let diff_to_id_tune_solve = fun (m:t) (v:U.t) ->
 let mm = tune_inv m ( diff_to_id_inv m ) in
  matrix_sparse_vector_sparse_prod mm v ;;

(** {v diff_to_id_tune_full_solve matrix vector v} *)
let diff_to_id_tune_full_solve = fun (m:t) (v:coeff array) ->
 let mm = diff_to_id_tune_inv m in
  matrix_full_vector_sparse_prod mm v ;;


(** {v diff_to_id_iterate exponent matrix vector v} *)
let diff_to_id_iterate = fun (s:int) (x:t) (v:U.t) ->
 let y = ref v in
  if s >= 0 then
   begin
    for i = 1 to s do
     y := matrix_sparse_vector_sparse_prod x !y ;
    done ;
   end
  else
   begin
    let xx = diff_to_id_tune_inv x in
     for i = 1 to ( abs s ) do
      y := matrix_sparse_vector_sparse_prod xx !y ;
     done ;
   end ;
  !y ;;

(** {v diff_to_id_int_pow exponent matrix v} *)
let rec diff_to_id_int_pow = fun (s:int) (x:t) ->
 if s >= 0 then
  begin
   if s = 0 then
    Diff_to_scal_matrix ( Coeff.one () , T.null ( dimensions x ) )
   else
    begin
     let n = s / 2 in
      let factor = int_pow n x in
       let prod = mult factor factor in
        if s mod 2 = 0 then
         prod
        else
         mult prod x
    end
  end
 else
  int_pow ( abs s ) ( diff_to_id_tune_inv x ) ;;










(** {C § § § } *)






end ;;





(** {C § § § } *)





end ;;



