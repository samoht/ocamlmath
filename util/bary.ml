

module Bary = struct





(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module tools to treat weighted sets, 
inspired from those of the module [Set] of the OCaml standard library,
with the difference that the sets are preferably modified in place.


{2 Conventions}


An element disappears when its weight is zero.

The functions of this module are not sealed.

The sets are modifiable in place.

The cardinal is recorded and modified with the set.


{2 Comments}


This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module des outils pour traiter les ensembles à poids,
inspirés de ceux du module [Set] de la bibliothèque standard OCaml,
à la différence près que les ensembles sont de préférence modifiés en place.


{2 Conventions}


Un élément disparaît quand son poids est nul.

Les fonctions de ce module ne sont pas étanches.

Les ensembles sont modifiables en place.

Le cardinal est enregistré et modifié avec l'ensemble.


{2 Commentaires}


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




open Util ;;


module type Index_type = sig

type t
val zero: unit -> t
val compare: t -> t -> int
val copy: t -> t

end ;;


module type Weight_type = sig

type t
val zero: unit -> t
val eq_zero: t -> bool
val compare: t -> t -> int
val copy: t -> t
val add: t -> t -> t
val sub: t -> t -> t
val opp: t -> t

end ;;


module Make (Index: Index_type) (Weight: Weight_type) = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(** This functor deals with weighted sets.

Ce foncteur traite des ensembles à poids. *)




(** {C § } *)
(** 
{1 Modules auxiliaires}
{1 Auxiliary modules}
*)
(** {C  } *)




module Pairs = struct

type t = Index.t * Weight.t ;;
let compare = fun (( x , xx ):t) (( y , yy ):t) ->
 Index.compare x y ;;

end ;;


module StrongPairs = struct

type t = Index.t * Weight.t ;;
let compare = fun (( x , xx ):t) (( y , yy ):t) ->
 let c = Index.compare x y in
  if c = 0 then
   Weight.compare xx yy
  else c ;;

end ;;


(** The type [elt] consists in couples (index, weight) and is identified with the type [elt] of the following modules [E] and [S].

Le type [elt] est formé par couple (indice, poids) et est identifié avec le type [elt] des modules [E] et [S] suivants. *)

type index = Index.t ;;

type weight = Weight.t ;;

type elt = index * weight ;;


(** The module [E] provides basic tools from the module Set of the standard OCaml library.

Le module [E] fournit les outils de base à partir du module Set de la bibliothèque standard OCaml. *)

module E = ( Set.Make (Pairs)
: sig
 include module type of Set.Make (Pairs)
end
 with type elt := elt ) ;;


(** In contrast to the module [E], the module [S] takes the weights into account in order to build some functions.

À la différence du module [E], le module [S] tient compte des poids pour construire quelques fonctions. *)

module S = ( Set.Make (StrongPairs)
: sig
 include module type of Set.Make (StrongPairs)
end
 with type elt := elt ) ;;




(** {C § } *)
(** 
{1 Construction of the functor}
{1 Construction du foncteur}
*)
(** {C  } *)




(** The type [t] records a set modifiable in place and its cardinal.

Le type [t] enregistre un ensemble modifiable en place et son cardinal.*)
type t = { mutable cardinal: int ; mutable content: E.t } ;;




(** {C § } *)
(** 
{2 Importations}
*)
(** {C  } *)




(** {v empty unit v} *)
let empty = function () ->
 { cardinal = 0 ; content = E.empty } ;;

(** {v is_empty set v} *)
let is_empty = function (s:t) ->
 E.is_empty s.content ;;

(** {v quick_is_empty set v} *)
let quick_is_empty = function (s:t) ->
 s.cardinal = 0 ;;

(** {v mem element set v} *)
let mem = fun (x:elt) (s:t) ->
 E.mem x s.content ;;

(** {v element_copy element v} *)
let element_copy = function ((x , y):elt) ->
 ( Index.copy x , Weight.copy y ) ;;

(** {v singleton element v} *)
let singleton = function (x:elt) ->
 { cardinal = 1 ; content = E.singleton x } ;;

(** {v remove element set v} *)
let remove = fun (x:elt) (s:t) ->
 if E.mem x s.content then
  begin
   s.cardinal <- pred s.cardinal ;
   s.content <- E.remove x s.content
  end ;;

(** {v iter function set v} *)
let iter = fun f (s:t) ->
 E.iter f s.content ;
 s.cardinal <- E.cardinal s.content ;;

(** {v fold function set init v} *)
let fold = fun f (s:t) init ->
 E.fold f s.content init ;;

(** {v for_all predicate set v} *)
let for_all = fun (p:elt -> bool) (s:t) ->
 E.for_all p s.content ;;

(** {v exists predicate set v} *)
let exists = fun (p:elt -> bool) (s:t) ->
 E.exists p s.content ;;

(** {v filter predicate set v} *)
let filter = fun (p:elt -> bool) (s:t) ->
 let t = E.filter p s.content in
  { cardinal = E.cardinal t ; content = t } ;;

(** {v partition predicate set v} *)
let partition = fun (p:elt -> bool) (s:t) ->
 let ( yes , no ) = E.partition p s.content in
  let c = E.cardinal yes
  and cc = E.cardinal no in
   ( { cardinal = c ; content = yes } , { cardinal = cc ; content = no } )

(** {v cardinal set v} *)
let cardinal = function (s:t) ->
 s.cardinal ;;

(** {v elements set v} *)
let elements = function (s:t) ->
 ( s.cardinal , E.elements s.content ) ;;

(** {v to_list set v} *)
let to_list = function (s:t) ->
 List.fast_sort Pairs.compare ( E.elements s.content ) ;;

(** {v min_elt set v} *)
let min_elt = function (s:t) ->
 E.min_elt s.content ;;

(** {v max_elt set v} *)
let max_elt = function (s:t) ->
 E.max_elt s.content ;;

(** {v choose set v} *)
let choose = function (s:t) ->
 E.choose s.content ;;

(** {v split element set v} *)
let split = fun (x:elt) (s:t) ->
 E.split x s.content ;;


(** {v translate set v} This function helps to use the module [S].

Cette fonction sert à utiliser le module [S]. *)
let translate = function (s:t) ->
 List.fold_right S.add ( E.elements s.content ) S.empty ;;

(** {v compare set1 set2 v} *)
let compare = fun (s1:t) (s2:t) ->
 S.compare ( translate s1 ) ( translate s2 ) ;;

(** {v equal set1 set2 v} *)
let equal = fun (s1:t) (s2:t) ->
 S.equal ( translate s1 ) ( translate s2 ) ;;

(** {v subset set1 set2 v} *)
let subset = fun (s1:t) (s2:t) ->
 S.subset ( translate s1 ) ( translate s2 ) ;;

(** {v strong_mem element set v} *)
let strong_mem = fun (x:elt) (s:t) ->
 S.mem x ( translate s ) ;;




(** {C § } *)
(** 
{2 Constructions propres aux ensembles à poids}
{2 Constructions proper to weighted sets}
*)
(** {C  } *)




(** {v add element set v} *)
let add = fun (( x , xx ) as z:elt) (s:t) ->
 let c = s.cardinal
 and d = s.content in
  if E.mem z s.content then
   begin
    let f = E.filter ( function w -> Pairs.compare z w = 0 ) d
    and t = E.remove z d in
     let liste = E.elements f in
      assert ( List.length ( List.tl liste ) = 0 ) ;
      let ( y , yy ) = List.hd liste in
       let w = Weight.add xx yy in
        if Weight.eq_zero w then
         begin
          s.cardinal <- pred c ;
          s.content <- t ;
         end
        else
         s.content <- E.add ( x , w ) t ;
   end
  else
   begin
    s.cardinal <- succ c ;
    s.content <- E.add z d ;
   end ;;

(** {v sub element set v} *)
let sub = fun (( x , xx ) as z:elt) (s:t) ->
 let e = s.content
 and c = s.cardinal in
  if E.mem z e then
   begin
    let f = E.filter ( function w -> Pairs.compare z w = 0 ) e
    and t = E.remove z e in
     let liste = E.elements f in
      assert ( List.length ( List.tl liste ) = 0 ) ;
      let ( y , yy ) = List.hd liste in
       let w = Weight.sub yy xx in
        if Weight.eq_zero w then
         begin
          s.cardinal <- pred c ;
          s.content <- t ;
         end
        else
         s.content <- E.add ( x , w ) t ;
   end
  else
   begin
    s.cardinal <- succ c ;
    s.content <- E.add ( x , Weight.opp xx ) e ;
   end ;;


(** {v inter set1 set2 v} The intersection chooses the minimal weight.

L'intersection choisit le poids minimal. *)
let rec inter = fun (s1:t) (s2:t) ->
 let c1 = s1.cardinal
 and d1 = s1.content
 and d = ref E.empty
 and c = ref 0
 and c2 = s2.cardinal
 and d2 = s2.content in
  if c1 <= c2 then
   begin
    let liste = ref ( E.elements d1 ) in
     let count = ref c1 in
      while !count > 0 do
       let z = List.hd !liste in
        if E.mem z d2 then
         begin
          let e = E.elements ( E.filter ( function w -> Pairs.compare z w = 0 ) d2 )
          and x = fst z
          and xx = snd z in
           assert ( List.length ( List.tl e ) = 0 ) ;
           let ( y , yy ) = List.hd e in
            let zz = if Weight.compare xx yy <= 0 then xx else yy in
             d := E.add ( x , zz ) !d ;
             incr c ;
         end ;
         liste := List.tl !liste ;
         decr count ;
      done ;
      { cardinal = !c ; content = !d }
   end
  else
   inter s2 s1 ;;

(** {v of_list element_list v} *)
let of_list = function (x:elt list) ->
 let s = empty () in
  let f = function y -> add y s in
   List.iter f x ;
   s ;;

(** {v map function set v} *)
let map = fun (f:elt -> elt) (s:t) ->
 let g = fun x u -> E.add ( f x ) u in
  let d = E.fold g s.content E.empty in
   { cardinal = E.cardinal d ; content = d } ;;

(** {v copy set v} *)
let copy = function (s:t) ->
 let g = fun x u -> E.add ( element_copy x ) u in
  let d = E.fold g s.content E.empty in
   { cardinal = s.cardinal ; content = d } ;;


(** {v opp set v} *)
let opp = function (s:t) ->
 let f = function ( x , y ) -> ( x , Weight.opp y ) in
  let g = fun x u -> E.add ( f x ) u in
   let d = E.fold g s.content E.empty in
    { cardinal = s.cardinal ; content = d } ;;

(** {v union set1 set2 v} *)
let union = fun (s1:t) (s2:t) ->
 let s = empty () in
  let f = function y -> add y s in
   E.iter f s1.content ;
   E.iter f s2.content ;
   s ;;

(** {v diff set1 set2 v} *)
let diff = fun (s1:t) (s2:t) ->
 let s = empty () in
  let f = function y -> add y s
  and g = function y -> sub y s in
   E.iter f s1.content ;
   E.iter g s2.content ;
   s ;;

(** {v extract index set v} *)
let extract = fun (i:Index.t) (s:t) ->
 let test = mem ( i , Weight.zero () ) s in
  if test then
   begin
    let p = function ( j , y ) -> Index.compare i j = 0 in
     let ( with_i , without_i ) = partition p s in
      E.choose with_i.content
   end
  else
   failwith "Index not present in Bary.Make.extract." ;;





(** {C § § § } *)



end ;;



module Z = struct

type t = int ;;
let zero = function () -> 0 ;;
let compare = Pervasives.compare ;;
let copy = Util.int_identity ;;
let eq_zero = function (x:t) -> x = 0 ;;
let add = ( + ) ;;
let sub = ( - ) ;;
let opp = ( ~- ) ;;

end ;;



module Zset = struct


include Make (Z) (Z) ;;
let insert_add = add ;;
let insert_sub = sub ;;
type u = t ;;
type v = t ;;
let norm = map ( function ( x , y ) -> ( x , abs y ) ) ;;
let norm_inject = map ( function ( x , y ) -> ( x , y ) ) ;;
let zero = empty ;;
let to_string = function (x:t) ->
 begin
  let f = function ( x , y ) -> ( string_of_int x ) ^ "^" ^ ( string_of_int y ) in
   String.concat "~" ( List.rev ( List.rev_map f ( snd ( elements x ) ) ) )
 end ;;
let of_string = function (s:string) ->
 begin
  let listing = Str.split ( Str.regexp_string "~" ) s
  and result = empty () in
   let f = function (st:string) ->
    begin
     let listing = Str.split ( Str.regexp_string "^" ) st in
      add ( int_of_string ( List.hd listing ) , int_of_string ( List.hd ( List.tl listing ) ) ) result
    end in
    ignore ( List.rev_map f listing ) ;
    result
 end ;;
let print = function (x:t) -> print_string ( to_string x ) ;;
let eq = equal ;;
let eq_zero = function (x:t) -> x.cardinal = 0 ;;
let int_mult = fun (n:int) (x:t) ->
 begin
  let f = function ( a , b ) -> ( a , n * b ) in
   map f x
 end ;;
let int_pow = fun (n:int) (x:t) ->
 begin
  let f = function ( a , b ) -> ( a , Util.int_power n b ) in
   map f x
 end ;;
let add = union ;;
let sub = diff ;;
let mult = add ;;

end ;;



(** {C § § § } *)



end ;;
