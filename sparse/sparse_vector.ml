




module Sparse_vector = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)



(**
The mathematician will find in this module functors to handle sparse vectors 
with coefficients in a commutative rng or a field.



{2 Conventions}



The indices are polymorphic and must be structured in a module.
The indices that immediately come to mind are in the types [int], [int array], [Big_int.big_int], [Big_int.big_int array].
For the multi-indices, the order may be lexicographic or anything else.



{2 Comments}



This module is inspired by the module [Hashtbl] of the standard library of OCaml.


A function is {e sealed} if there is no sharing between the input variables and the output value.
This is the expected behavior of usual mathematical functions.
Copy functions are provided.
They are sealed provided that they receive as argument elementary copy functions for coefficients and indices.
By composition, they permit to seal all functions necessary.



This module is distributed under the same licence as Ocaml.



{C § }



La mathématicienne ou le mathématicien trouvera dans ce module des foncteurs pour traiter 
les vecteurs creux à coefficients dans un annau commutatif ou bien dans un corps commutatif.



{2 Conventions}



Les indices sont polymorphes et doivent être structurés dans un module.
Les indices qui viennent immédiatement à l'esprit sont dans les types [int], [int array], [Big_int.big_int], [Big_int.big_int array].
Pour les multi-indices, l'ordre peut être lexicographique ou autre.



{2 Commentaires}



Une fonction est {e étanche} quand il n'y a aucun partage entre les variables fournies en entrée et la valeur obtenue en sortie.
C'est le comportement attendu des fonctions mathématiques habituelles. 
Des fonctions de recopie sont fournies.
Elles sont étanches à condition de leur fournir en argument des fonctions élémentaires de recopie des coefficients et des indices. 
Par composition, elle permettent d'étanchéifier toutes les fonctions voulues.


Ce module est inspiré du module [Hashtbl] de la bibliothèque standard d'OCaml.

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
{2 Vecteurs creux à coefficients dans un annau commutatif}
{2 Sparse vectors with coefficients in a commutative rng}
*)
(** {C  } *)




open Util ;;
open Data ;;
open Hash ;;



module Rng (Index:Data.Index_type) (Hasher:Hash.Hash_type with type t = Index.t) (Coeff:Data.Rng_coeff_type) = struct


(** The type [elt] consists in couples (index, weight) and is identified with the type [elt] of the following module [H].

Le type [elt] est formé par couple (indice, poids) et est identifié avec le type [elt] du module [H] suivant. *)

type index = Index.t ;;

type coeff = Coeff.t ;;

type elt = index * coeff ;;


(** The module [H] provides the hash tables.

Le module [H] fournit les tables de hachage. *)

module H = ( Hash.Make (Index) (Hasher) (Coeff)
: sig
 include module type of Hash.Make (Index) (Hasher) (Coeff)
end
 with type elt := elt with type index = index with type weight = coeff ) ;;


(** The type [t] contains the hash table together with the dimension of the vector space.

Le type [t] contient la table de hachage avec la dimension de l'espace vectoriel. *)
type t = index * H.t ;;


(** The type [t] is that of the norm.

Le type [u] est celui de la norme. *)
type u = Coeff.u ;;


(** {v to_sparse hash_size vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let to_sparse = fun (size:int) (v:coeff array) ->
 let r = Array.length v
 and h = H.create size in
  for i = 0 to pred r do
   H.add h ( Index.from_int i , v.(i) ) ;
  done ;
  ( Index.from_int r , h ) ;;

(** {v auto_to_sparse vector v} *)
let auto_to_sparse = function (v:coeff array) ->
 to_sparse ( -1 ) v ;;

(** {v first_non_zero vector v} *)
let first_non_zero = function (( d , v ):t) ->
 let result = ref d in
  let f = function ( i , x ) -> if Index.compare i !result > 0 then result := i in
   H.iter f v ;
   !result ;;

(** {v last_non_zero vector v} *)
let last_non_zero = function (( d , v ):t) ->
 let result = ref ( Index.witness () ) in
  let f = function ( i , x ) -> if Index.compare i !result > 0 then result := i in
   H.iter f v ;
   !result ;;

(** {v to_full vector v} This function is not sealed.

Cette fonction n'est pas étanche. *)
let to_full = function (( d , v ) as x:t) ->
 let r = max ( Index.to_int d ) ( succ ( Index.to_int ( last_non_zero x ) ) ) in
  let y = Array.make r ( Coeff.zero () ) in
   let f = function ( i , z ) -> ( y.( Index.to_int i ) <- z ) in
    H.iter f v ; 
    y ;;

(** {v filling vector v} *)
let filling = function (( d , v ):t) ->
 H.filling v ;;

(** {v dimension vector v} *)
let dimension = function (( d , v ):t) ->
 Index.copy d ;;

(** {v info vector v} *)
let info = function (( d , v ):t) ->
 ( Index.copy d , H.filling v ) ;;

(** {v size vector v} *)
let size = function (( d , v ):t) ->
 H.size v ;;

(** {v null dimension v} *)
let null = function (d:index) ->
 ( Index.copy d , H.create ( -1 ) ) ;;

(** {v zero unit v} *)
let zero = function () ->
 ( Index.from_int 1 , H.create ( -1 ) )

(** {v to_string vector v}*)
let to_string = function (( d , v ):t) ->
 let a = Index.to_string d
 and b = ref "" in
  let f = function ( i , z ) -> b := !b ^ "(" ^ ( Index.to_string i ) ^ "," ^ ( Coeff.to_string z ) ^ ");" in
 H.iter f v ;
 a ^ ":" ^ ( String.sub !b 0 ( max 0 ( pred ( String.length !b ) ) ) ) ;;

(** {v special_to_string dimension_separator beginning separator ending vector v}*)
let special_to_string = fun (dim_sep:char) (beginning:string) (separator:string) (ending:string) (( d , v ):t) ->
 assert ( ( String.length beginning ) + 1 = String.length ending ) ;
 let a = Index.to_string d
 and b = ref "" in
  let f = function ( i , z ) -> b := !b ^ beginning ^ ( Index.to_string i ) ^ separator ^ ( Coeff.to_string z ) ^ ending in
 H.iter f v ;
 a ^ ( String.make 1 dim_sep ) ^ ( String.sub !b 0 ( max 0 ( pred ( String.length !b ) ) ) ) ;;

(** {v print vector v}*)
let print = function (x:t) ->
 print_string ( to_string x ) ;
 print_newline () ;;

(** {v from_string size string v} *)
let from_string = fun (size:int) (s:string) ->
 let f = fun boundary separator st ->
  begin
   let lst = String.length st in
    let str = String.sub st boundary ( max 0 ( lst - 2 * boundary ) ) in
     Str.split ( Str.regexp_string separator ) str
  end in
  let separation = String.index s ':' in
   let dimension = String.sub s 0 separation
   and sep = succ separation in
    try
     begin
      let content = String.sub s sep ( max 0 ( ( String.length s ) - sep ) ) in
       let listing = f 1 ");(" content
       and g = function st ->
        begin
         match f 0 "," st with
         | head :: tail -> ( Index.of_string head , Coeff.of_string ( List.hd tail ) )
         | _ -> failwith "Bad string in Sparse.Rng_vector.of_string."
        end in
        let h = H.of_list size ( List.rev_map g listing ) in
         ( Index.of_string dimension , h )
     end
    with _ ->
     null ( Index.of_string dimension ) ;;

(** {v special_from_string dimension_separator beginning separator ending size string v} *)
let special_from_string = fun (dim_sep:char) (beginning:string) (separator:string) (ending:string) (size:int) (s:string) ->
 assert ( ( String.length beginning ) + 1 = String.length ending ) ;
 let f = fun boundary separator st ->
  begin
   let lst = String.length st in
    let str = String.sub st boundary ( max 0 ( lst - 2 * boundary ) ) in
     Str.split ( Str.regexp_string separator ) str
  end in
  let separation = String.index s dim_sep in
   let dimension = String.sub s 0 separation
   and sep = succ separation in
    try
     begin
      let content = String.sub s sep ( max 0 ( ( String.length s ) - sep ) ) in
       let listing = f ( String.length beginning ) ( ending ^ beginning ) content
       and g = function st ->
        begin
         match f 0 separator st with
         | head :: tail -> ( Index.of_string head , Coeff.of_string ( List.hd tail ) )
         | _ -> failwith "Bad string in Sparse.Rng_vector.of_string."
        end in
        let h = H.of_list size ( List.rev_map g listing ) in
         ( Index.of_string dimension , h )
     end
    with _ ->
     null ( Index.of_string dimension ) ;;

(** {v of_string string v} *)
let of_string = fun (s:string) ->
 from_string ( -1 ) s ;;

(** {v copy vector v} *)
let copy = function (( d , v ):t) ->
 let f = function ( i , x ) -> ( Index.copy i , Coeff.copy x ) in
  let vv = H.copy f v in
   ( Index.copy d , vv ) ;;


(** {v cleanup vector v} This function is needed in case the indices are modified in place.

Cette fonction est nécessaire si l'on modifie en place les indices. *)
let cleanup = function (( d , v ):t) ->
 let s = H.size v in
  let t = if s >= 0 then max 1 s else s in
   H.resize t v ;;

(** {v resize size vector v} *)
let resize = fun (n:int) (( d , v ):t) ->
 let t = if n >= 0 then max 1 n else n in
  H.resize t v ;;

(** {v elements vector v} *)
let elements = function (( d , v ):t) ->
 H.elements v ;;


(** {v iter function vector v} This function is applied in place.

Cette fonction est appliquée en place. *)
let iter = fun f (( d , v ):t) ->
 H.iter f v ;;

(** {v fold function vector init v} *)
let fold = fun f (( d , v ):t) init ->
 H.fold f v init ;;

(** {v in_place_map function vector v} *)
let in_place_map = fun f (( d , v ):t) ->
 H.in_place_map f v ;;

(** {v in_place_mapi function vector v} *)
let in_place_mapi = fun f (( d , v ):t) ->
 H.in_place_mapi f v ;;

(** {v map function vector v} *)
let map = fun f (( d , v ):t) ->
 ( Index.copy d , H.map f v ) ;;

(** {v mapi function vector v} *)
let mapi = fun f (( d , v ):t) ->
 ( Index.copy d , H.mapi f v ) ;;

(** {v min vector v} *)
let min = function (( d , v ):t) ->
 H.min v ;;

(** {v max vector v} *)
let max = function (( d , v ):t) ->
 H.max v ;;

(** {v unsafe_extract index vector v} *)
let unsafe_extract = fun (i:index) (( d , v ):t) ->
 H.extract i v ;;

(** {v extract index vector v} *)
let extract = fun (i:index) (( d , v ):t) ->
 try
  H.extract i v
 with _ -> ( Index.zero () , Coeff.zero () ) ;;

(** {v raw_extract index vector v} *)
let raw_extract = fun (i:index) (x:t) ->
 snd ( extract i x ) ;;

(** {v filter predicate vector v} *)
let filter = fun (p:index -> bool) (( d , v ):t) ->
 let result = ref [] in
  let f = function ( i , x ) -> if p(i) then result := ( i , x ) :: !result in
   H.iter f v ;
   !result ;;


(** {v insert_add coefficient index vector v} This function is applied in place.

Cette fonction est appliquée en place. *)
let insert_add = fun (x:coeff) (i:index) (( d , v ):t) ->
 H.add v ( i , x ) ;;


(** {v insert_sub coefficient index vector v} This function is applied in place.

Cette fonction est appliquée en place. *)
let insert_sub = fun (x:coeff) (i:index) (( d , v ):t) ->
 H.sub v ( i , x ) ;;


(** {v replace coefficient index vector v} This function is applied in place.

Cette fonction est appliquée en place. *)
let replace = fun (x:coeff) (i:index) (( d , v ):t) ->
 H.replace v ( i , x ) ;;


(** {v remove index vector v} This function is applied in place.

Cette fonction est appliquée en place. *)
let remove = fun (i:index) (( d , v ) as x:t) ->
 let x = raw_extract i x in
  H.remove v ( i , x ) ;;

(** {v beginning index vector v} *)
let beginning = fun (i:index) (( d , v ) as x :t) ->
 let w = null d in
  let f = function ( j , y ) -> if ( Index.compare j i <= 0 ) && ( Index.compare j ( Index.zero () ) >= 0 ) then insert_add y j w in
   iter f x ;
   w ;;

(** {v ending index vector v} *)
let ending = fun (i:index) (( d , v ) as x :t) ->
 let w = null d in
  let f = function ( j , y ) -> if ( Index.compare j i >= 0 ) && ( Index.compare j d < 0 ) then insert_add y j w in
   iter f x ;
   w ;;


(** {v in_place_add vector1 vector2 v} The first vector stores the result.

Le premier vecteur accueille le résultat. *)
let in_place_add = fun (( d , v ):t) (( e , w ):t) ->
 let f = function x -> H.add v x in
  H.iter f w ;;

(** {v add vector1 vector2 v} *)
let add = fun (( d , v ) as x:t) (( e , w ) as y:t) ->
 let xx = copy x in
  in_place_add xx y ;
  xx ;;


(** {v in_place_sub vector1 vector2 v} The first vector stores the result.

Le premier vecteur accueille le résultat. *)
let in_place_sub = fun (( d , v ):t) (( e , w ):t) ->
 let f = function x -> H.sub v x in
  H.iter f w ;;

(** {v sub vector1 vector2 v} *)
let sub = fun (( d , v ) as x:t) (( e , w ) as y:t) ->
 let xx = copy x in
  in_place_sub xx y ;
  xx ;;

(** {v eq_zero vector v} *)
let eq_zero = function (( d , v ):t) ->
 ( H.filling v ) = 0 ;;

(** {v eq vector1 vector2 v} *)
let eq = fun (x:t) (y:t) ->
 eq_zero ( sub x y ) ;;

(** {v in_place_opp vector v} *)
let in_place_opp = function (( d , v ):t) ->
 H.in_place_opp v ;;

(** {v opp vector v} *)
let opp = function (( d , v ):t) ->
 ( Index.copy d , H.opp v ) ;;

(** {v sub_vector beginning ending vector v} *)
let sub_vector = fun (beginning:index) (ending:index) (( d , v ) as x:t) ->
 let h = H.create ( size x ) in
  let f = function ( i , y ) ->
   if ( Index.compare i beginning >= 0 ) && ( Index.compare i ending <= 0 ) then
    H.add h ( Index.sub i beginning , y ) in
   H.iter f v ;
   ( Index.succ ( Index.sub ending beginning ) , h ) ;;

(** {v mask_vector beginning ending vector v} *)
let mask_vector = fun (beginning:index) (ending:index) (( d , v ) as x:t) ->
 let h = H.create ( size x ) in
  let f = function ( i , y ) ->
   if ( Index.compare i beginning >= 0 ) && ( Index.compare i ending <= 0 ) then
    H.add h ( i , y ) in
   H.iter f v ;
   ( d , h ) ;;

(** {v embed dimension shift vector v} *)
let embed = fun (dimension:index) (shift:index) (( d , v ) as x:t) ->
 assert ( Index.compare dimension ( Index.add shift d ) >= 0 ) ;
 let h = H.create ( size x ) in
  let f = function ( i , y ) -> H.add h ( Index.add shift i , y ) in
   H.iter f v ;
   ( dimension , h ) ;;

(** {v find coefficient vector v} *)
let find = fun (c:coeff) (( d , v ) :t) ->
 try
  fst ( H.load_find v c )
 with _ ->
  Index.witness () ;;

(** {v find_all coefficient vector v} *)
let find_all = fun (c:coeff) (( d , v ) :t) ->
 H.load_find_all v c ;;

(** {v list_find_all coefficient vector v} *)
let list_find_all = fun (c:coeff) (( d , v ) :t) ->
 H.B.E.elements ( H.load_find_all v c ) ;;

(** {v index_list_find_all coefficient vector v} *)
let index_list_find_all = fun (c:coeff) (( d , v ) :t) ->
 List.rev_map fst ( H.B.E.elements ( H.load_find_all v c ) ) ;;

(** {v sum vector v} *)
let sum = function (x:t) ->
 let accu = ref ( Coeff.zero () ) in
  let f = function ( i , y ) -> accu := Coeff.add !accu y in
   ignore ( iter f x ) ;
   !accu ;;

(** {v contraction init vector v} *)
let contraction = fun (init:coeff) (x:t) ->
 let accu = ref init in
  let f = function ( i , y ) -> accu := Coeff.mult !accu y in
   ignore ( iter f x ) ;
   !accu ;;

(** {v in_place_scal_add scalar vector v} *)
let in_place_scal_add = fun (y:coeff) (( d , v ):t) ->
 H.in_place_map ( Coeff.add y ) v ;;

(** {v scal_add scalar vector v} *)
let scal_add = fun (y:coeff) (( d , v ):t) ->
 let h = H.map ( Coeff.add y ) v in
  ( Index.copy d , h ) ;;

(** {v in_place_scal_mult scalar vector v} *)
let in_place_scal_mult = fun (y:coeff) (( d , v ):t) ->
 H.in_place_map ( Coeff.mult y ) v ;;

(** {v scal_mult scalar vector v} *)
let scal_mult = fun (y:coeff) (( d , v ):t) ->
 let h = H.map ( Coeff.mult y ) v in
  ( Index.copy d , h ) ;;

(** {v in_place_scal_right_sub scalar vector v} *)
let in_place_scal_right_sub = fun (y:coeff) (( d , v ):t) ->
 H.in_place_map ( Coeff.sub y ) v ;;

(** {v scal_right_sub scalar vector v} *)
let scal_right_sub = fun (y:coeff) (( d , v ):t) ->
 let h = H.map ( Coeff.sub y ) v in
  ( Index.copy d , h ) ;;

(** {v in_place_scal_left_sub scalar vector v} *)
let in_place_scal_left_sub = fun (y:coeff) (( d , v ):t) ->
 let f = function z -> Coeff.sub z y in
  H.in_place_map f v ;;

(** {v scal_left_sub scalar vector v} *)
let scal_left_sub = fun (y:coeff) (( d , v ):t) ->
 let f = function z -> Coeff.sub z y in
  let h = H.map f v in
  ( Index.copy d , h ) ;;

(** {v coeff_prod vector1 vector2 v} *)
let coeff_prod = fun (x:t) (y:t) ->
 let result = null ( dimension x ) in
  let f = function ( i , z ) ->
   begin
    let zz = raw_extract i x in
     insert_add ( Coeff.mult zz z ) i result
   end in
   iter f y ;
   result ;;

(** {v scal_prod vector1 vector2 v} *)
let scal_prod = fun (( d , v ):t) (( e , w ):t) ->
 let f = fun ( i , x ) previous ->
  begin
   try
    begin
     let zz = H.extract i w in
      Coeff.add previous ( Coeff.mult x ( snd zz ) )
    end
   with _ ->
    previous
  end in
  H.fold f v ( Coeff.zero () ) ;;

(** {v sparse_full_scal_prod vector1 vector2 v} *)
let sparse_full_scal_prod = fun (( d , v ):t) (w:coeff array) ->
 let f = fun ( i , x ) previous ->
  begin
   try
    begin
     let zz = w.( Index.to_int i ) in
      Coeff.add previous ( Coeff.mult x zz )
    end
   with _ ->
    previous
  end in
  H.fold f v ( Coeff.zero () ) ;;

(** {v norm_1 vector v} *)
let norm_1 = function (x:t) ->
 let accu = ref ( Coeff.norm_zero () ) in
  let f = function ( i , y ) -> accu := Coeff.norm_add !accu ( Coeff.norm y ) in
   ignore ( iter f x ) ;
   !accu ;;

(** {v norm_inf vector v} *)
let norm_inf = function (x:t) ->
 let accu = ref ( Coeff.norm_zero () ) in
  let f = function ( i , y ) ->
   begin
    let new_norm = Coeff.norm y in
     if Coeff.norm_compare new_norm !accu > 0 then
      accu := new_norm
   end in
   ignore ( iter f x ) ;
   !accu ;;

(** {v square_sum vector v} *)
let square_sum = function (x:t) ->
 let accu = ref ( Coeff.zero () ) in
  let f = function ( i , y ) -> accu := Coeff.add !accu ( Coeff.mult y y ) in
   ignore ( iter f x ) ;
   !accu ;;

(** {v square_norm_2 vector v} *)
let square_norm_2 = function (x:t) ->
 let accu = ref ( Coeff.norm_zero () ) in
  let f = function ( i , y ) -> accu := Coeff.norm_add !accu ( Coeff.norm ( Coeff.mult y y ) ) in
   ignore ( iter f x ) ;
   !accu ;;

(** {v compare_norm norm vector1 vector2 v} *)
let compare_norm = fun n (x:t) (y:t) ->
 Coeff.norm_compare ( n x ) ( n y ) ;;

(** {v exchange index_1 index_2 vector v} *) 
let exchange = fun (i:index) (j:index) (u:t) ->
 let ( ii , x ) = extract i u
 and ( jj , y ) = extract j u in
  replace x j u ;
  replace y i u ;;

(** {v compare vector1 vector2 v} *)
let compare = compare_norm norm_1 ;;

(** {v mult vector1 vector2 v} *)
let mult = coeff_prod ;;

(** {v square vector1 vector2 v} *)
let square = function x ->
 mult x x ;;

(** {v int_mult integer vector v} *)
let int_mult = fun (n:int) (( d , v ):t) ->
 let h = H.map ( Coeff.int_mult n ) v in
  ( Index.copy d , h ) ;;

(** {v int_pow integer vector v} *)
let int_pow = fun (n:int) (( d , v ):t) ->
 let h = H.map ( Coeff.int_pow n ) v in
  ( Index.copy d , h ) ;;

(** {v norm vector v} *)
let norm = norm_1 ;;

(** {v norm_inject number v} *)
let norm_inject = function (x:u) -> auto_to_sparse [| Coeff.norm_inject x |] ;;

(** {v norm_zero unit v} *)
let norm_zero = function () ->
 Coeff.norm_zero () ;;

(** {v norm_of_string string v} *)
let norm_of_string = Coeff.norm_of_string ;;

(** {v norm_to_string number v} *)
let norm_to_string = Coeff.norm_to_string ;;

(** {v norm_print number v} *)
let norm_print = Coeff.norm_print ;;

(** {v norm_eq number1 number2 v} *)
let norm_eq = Coeff.norm_eq ;;

(** {v norm_eq_zero number v} *)
let norm_eq_zero = Coeff.norm_eq_zero ;;

(** {v norm_compare number1 number2 v} *)
let norm_compare = Coeff.norm_compare ;;

(** {v norm_add number1 number2 v} *)
let norm_add = Coeff.norm_add ;;

(** {v norm_int_mult integer number v} *)
let norm_int_mult = Coeff.norm_int_mult ;;

(** {v norm_mult number1 number2 v} *)
let norm_mult = Coeff.norm_mult ;;

(** {v norm_square number1 number2 v} *)
let norm_square = Coeff.norm_square ;;




(** {C § § § } *)




end ;;




(** {C § } *)
(** 
{2 Vecteurs creux à coefficients dans un corps commutatif}
{2 Sparse vectors with coefficients in a field}
*)
(** {C  } *)




module Field (Index:Data.Index_type) (Hasher:Hash.Hash_type with type t = Index.t) (Coeff:Data.Field_coeff_type) = struct


include Rng (Index) (Hasher) (Coeff) ;;


(** {v in_place_inv vector v} *)
let in_place_inv = function (( d , v ):t) ->
 H.in_place_map Coeff.inv v ;;

(** {v inv vector v} *)
let inv = function (( d , v ):t) ->
 ( Index.copy d , H.map Coeff.inv v ) ;;

(** {v in_place_scal_right_div scalar vector v} *)
let in_place_scal_right_div = fun (y:coeff) (( d , v ):t) ->
 H.in_place_map ( Coeff.div y ) v ;;

(** {v scal_right_div scalar vector v} *)
let scal_right_div = fun (y:coeff) (( d , v ):t) ->
 let h = H.map ( Coeff.div y ) v in
  ( Index.copy d , h ) ;;

(** {v in_place_scal_left_div scalar vector v} *)
let in_place_scal_left_div = fun (y:coeff) (( d , v ):t) ->
 let f = function z -> Coeff.div z y in
  H.in_place_map f v ;;

(** {v scal_left_div scalar vector v} *)
let scal_left_div = fun (y:coeff) (( d , v ):t) ->
 let f = function z -> Coeff.div z y in
  let h = H.map f v in
  ( Index.copy d , h ) ;;

(** {v reciprocal vector v} *)
let reciprocal = function (v:t) ->
 scal_left_div ( square_sum v ) v ;;

(** {v norm_reciprocal vector v} *)
let norm_reciprocal = function (v:t) ->
 scal_left_div ( Coeff.norm_inject ( square_norm_2 v ) ) v ;;





(** {C § § § } *)




end ;;








(** {C § § § } *)




end ;;



