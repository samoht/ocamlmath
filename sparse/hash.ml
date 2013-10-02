

module Hash = struct



open Util ;;
open Bary ;;
open Data ;;



module type Hash_type = sig

type t
val high:int
val low:int
val hash:t -> int

end ;;



module Z = struct

type t = int ;;

(** {v high v} In case of equirepartition, the depth of the binary trees does not exceed four.

En cas d'équirépartition, la profondeur des arbres binaires équilibrés ne va pas au-delà de quatre. *)
let high = 32 ;;
let low = 32 ;;
let hash = fun x -> Hashtbl.hash x ;;

end ;;



module Big = struct

type t = Big_int.big_int ;;

(** {v high v} In case of equirepartition, the depth of the binary trees does not exceed four.

En cas d'équirépartition, la profondeur des arbres binaires équilibrés ne va pas au-delà de quatre. *)
let high = 32 ;;
let low = 2 ;;
let hash = fun x -> Hashtbl.hash x ;;

end ;;



module Number = struct

type t = Num.num ;;

(** {v high v} In case of equirepartition, the depth of the binary trees does not exceed four.

En cas d'équirépartition, la profondeur des arbres binaires équilibrés ne va pas au-delà de quatre. *)
let high = 32 ;;
let low = 2 ;;
let hash = fun x -> Hashtbl.hash x ;;

end ;;



module Multi_hasher (Index:Bary.Index_type) (Hasher:Hash_type with type t = Index.t) = struct

type t = Index.t array ;;
let high = Hasher.high ;;
let low = Hasher.low ;;
let multi_hash = Array.map Hasher.hash ;;

(** A null hash value must be avoided when it is considered as a coefficient in a sparse vector.

Il faut éviter une valeur de hachage nulle quand elle est considérée comme coefficient dans un vecteur creux. *)
let hash = function (x:t) ->
 begin
 let y = Hashtbl.hash ( multi_hash x ) in
  if y = 0 then
   max_int
  else
   y
 end ;;

end ;;



module Make (Index:Bary.Index_type) (Hasher:Hash_type with type t = Index.t) (Weight:Bary.Weight_type) = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module tools to treat couples (index, weight) in hash tables.

If the index is an integer and the weight an element of a ring or a field,
sparse vectors may be treated.

If the index is an element of a totally ordered set and the weight an integer,
then (totally ordered) sets may be treated with multiplicity.


{2 Conventions}


The hash tables are arrays of weighted sets.

The functions of this module are not sealed.


{2 Comments}


This module is inspired by the module [Hashtbl] of the standard library of OCaml.

This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module des outils pour traiter 
des couples (indice, poids) dans des tables de hachage.

Si l'indice est un entier et le poids un élément d'un anneau ou d'un corps, 
on peut traiter des vecteurs creux.

Si l'indice est un élément d'un ensemble totalement ordonnné et le poids un entier, 
on peut traiter des ensembles (totalement ordonnnés) avec multiplicité.


{2 Conventions}


Les tables de hachage sont des tableaux d'ensembles à poids.

Les fonctions de ce module ne sont pas étanches.


{2 Commentaires}


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
{1 Ensembles à poids}
{1 Weighted sets}
*)
(** {C  } *)




(** The type [elt] consists in couples (index, weight) and is identified with the type [elt] of the following module [B].

Le type [elt] est formé par couple (indice, poids) et est identifié avec le type [elt] du module [B] suivant. *)

type index = Index.t ;;

type weight = Weight.t ;;

type elt = index * weight ;;


(** The module [B] provides the weighted sets.

Le module [B] fournit les ensembles à poids. *)

module B = ( Bary.Make (Index) (Weight)
: sig
 include module type of Bary.Make (Index) (Weight)
end
 with type elt := elt with type index = index with type weight = weight ) ;;


(** {v element_copy element v} *)
let element_copy = function (x:elt) ->
 B.element_copy x ;;

(** {v set_copy element v} *)
let set_copy = function (s:B.t) ->
 B.copy s ;;




(** 
{1 Tables de hachage}
{1 Hash tables}
*)
(** {C  } *)




(** The type [t] records the hash table together with its filling value.
A switch shows if the length of the array must be automatically tuned in some operations.

Le type [t] enregistre la table de hachage avec la valeur de son remplissage. 
Un commutateur indique si la longueur du tableau doit être ajustée automatiquement dans certaines opérations. *)
type t =
 { mutable filling: int ; mutable auto: bool ; mutable data: B.t array } ;;


(** {v create initial_size v} A negative initial size activates the switch of the automatic tuning of the length.

Une taille initiale négative active le commutateur d'ajustage automatique de la longueur. *)
let create = function (initial_size:int) ->
 let s = min ( max 1 ( abs initial_size ) ) Sys.max_array_length in
  let table = Array.map B.empty ( Array.make s () ) in
   for i = 0 to pred s do
    table.(i) <- B.empty () ;
   done ;
   if initial_size < 0 then
    { filling = 0 ; auto = true ; data = table }
   else
    { filling = 0 ; auto = false ; data = table }


(** {v make element initial_size v} A negative initial size activates the switch of the automatic tuning of the length.

Une taille initiale négative active le commutateur d'ajustage automatique de la longueur. *)
let make = fun (x:elt) (initial_size:int) ->
 let s = min ( max 1 ( abs initial_size ) ) Sys.max_array_length in
  let table = Array.map B.empty ( Array.make s () ) in
   for i = 0 to pred s do
    table.(i) <- B.singleton x ;
   done ;
   if initial_size < 0 then
    { filling = s ; auto = true ; data = table }
   else
    { filling = s ; auto = false ; data = table }

(** {v clear table v} *)
let clear = function (h:t) ->
 let r = Array.length h.data in
  for i = 0 to pred r do
   h.data.(i) <- B.empty () ;
  done ;
  h.filling <- 0 ;;

(** {v copy table v} *)
let copy = function (h:t) ->
 let r = Array.length h.data in
  let out = create r in
   for i = 0 to pred r do
    out.data.(i) <- set_copy h.data.(i) ;
   done ;
   out.filling <- h.filling ;
   out ;;

(** {v filling table v} *)
let filling = function (h:t) ->
 h.filling ;;

(** {v length table v} *)
let length = function (h:t) ->
 Array.length h.data ;;

(** {v size table v} *)
let size = function (h:t) ->
 if h.auto then
  - ( Array.length h.data )
 else
  Array.length h.data ;;

(** {v expand table v} *)
let expand = function (h:t) ->
 let r = Array.length h.data in
  let new_length = min ( 2 * r ) Sys.max_array_length in
   let table = Array.map B.empty ( Array.make new_length () ) in
    let f = function ((key,load) as x:elt) -> B.add x ( table.( ( Hasher.hash key ) mod new_length ) ) in
     for i = 0 to pred r do
      B.E.iter f h.data.(i).B.content ;
     done ;
     h.data <- table ;;

(** {v retract table v} *)
let retract = function (h:t) ->
 let r = Array.length h.data in
  let new_length = max 1 ( r / 2 ) in
   let table = Array.map B.empty ( Array.make new_length () ) in
    let f = function ((key,load) as x:elt) -> B.add x ( table.( ( Hasher.hash key ) mod new_length ) ) in
     for i = 0 to pred r do
      B.E.iter f h.data.(i).B.content ;
     done ;
     h.data <- table ;;

(** {v resize new_length table v} *)
let resize = fun (new_length:int) (h:t) ->
 let nl = abs new_length in
  let table = Array.map B.empty ( Array.make nl () ) in
   let f = function ((key,load) as x:elt) -> B.add x ( table.( ( Hasher.hash key ) mod nl ) ) in
    for i = 0 to pred ( Array.length h.data ) do
     B.E.iter f h.data.(i).B.content ;
    done ;
    if new_length < 0 then
     h.auto <- true
    else
     h.auto <- false ;
    h.data <- table ;;


(** {v auto_resize table v} If the filling exceeds [Hasher.high] times the length of the table, then the size is doubled. 
If the size exceeds [Hasher.low] times the filling, then the size is divided by two.

Si le remplissage dépasse [Hasher.high] fois la taille de la table, la taille est doublée.
Si la taille dépasse [Hasher.low] fois le remplissage, la taille est divisée par deux. *)
let auto_resize = fun (h:t) ->
 let r = Array.length h.data in
  if h.filling > Hasher.high * r then
   resize ( 2 * r ) h
  else if r > Hasher.low * h.filling then
   resize ( max 1 ( r / 2 ) ) h ;;

(** {v elements table v} *)
let elements = function (h:t) ->
 let f = function s -> snd ( B.elements s ) in
  let t = Array.map f h.data in
   List.concat ( Array.to_list t ) ;;

(** {v apply table function element v} *)
let apply = fun (h:t) (f:elt -> B.t -> unit) ((key,load) as x:elt) ->
 if not ( Weight.eq_zero load ) then
  begin
   let i = Hasher.hash key mod (Array.length h.data) in
    let previous = B.cardinal h.data.(i) in
     f x h.data.(i) ;
     h.filling <- h.filling + ( B.cardinal h.data.(i) ) - previous
  end ;
 if h.auto then
  auto_resize h ;;

(** {v add table element v} *)
let add = fun (h:t) (x:elt) ->
 apply h B.add x ;;

(** {v sub table element v} *)
let sub = fun (h:t) (x:elt) ->
 apply h B.sub x ;;

(** {v raw_apply table function element v} *)
let raw_apply = fun (h:t) (f:elt -> B.t -> unit) ((key,load) as x:elt) ->
 let i = Hasher.hash key mod (Array.length h.data) in
  let previous = B.cardinal h.data.(i) in
   f x h.data.(i) ;
   h.filling <- h.filling + ( B.cardinal h.data.(i) ) - previous ;
   if h.auto then
    auto_resize h ;;

(** {v remove table element v} *)
let remove = fun (h:t) ((key,load) as x:elt) ->
 raw_apply h B.remove x ;;

(** {v index_mem table key v} *)
let index_mem = fun (h:t) (key:index) ->
 let i = Hasher.hash key mod (Array.length h.data) in
  B.E.mem ( key , Weight.zero () ) h.data.(i).B.content ;;

(** {v index_find_all table key v} *)
let index_find_all = fun (h:t) (key:index) ->
 let i = Hasher.hash key mod (Array.length h.data)
 and p = function z -> Index.compare key ( fst z ) = 0 in
  let ( with_key , without_key ) = B.E.partition p h.data.(i).B.content in
   ( i , with_key ) ;;

(** {v index_find table key v} *)
let index_find = fun (h:t) (key:index) ->
 let ( i , with_key ) = index_find_all h key in
  try
   B.E.choose with_key
  with _ ->
   ( key , Weight.zero () ) ;;

(** {v load_find_all table load v} *)
let load_find_all = fun (h:t) (load:weight) ->
 let p = function z -> Weight.compare load ( snd z ) = 0
 and accu = ref B.E.empty in
  for i = 0 to pred ( Array.length h.data ) do
   let ( with_load , without_load ) = B.E.partition p h.data.(i).B.content in
    accu := B.E.union with_load !accu ;
  done ;
  !accu ;;

(** {v load_find table load v} *)
let load_find = fun (h:t) (load:weight) ->
 try
  B.E.choose ( load_find_all h load )
 with _ ->
  failwith "Not found in Hash.Make.load_find." ;;

(** {v element_mem table key v} *)
let element_mem = fun (h:t) ((key,load) as x:elt) ->
 let i = Hasher.hash key mod (Array.length h.data) in
  B.strong_mem x h.data.(i) ;;

(** {v element_find_all v} *)
let element_find_all = fun (h:t) ((key,load):elt) ->
 let i = Hasher.hash key mod (Array.length h.data)
 and p = fun ( y , z ) -> ( Index.compare key y = 0 ) && ( Weight.compare load z = 0 ) in
  let ( with_pair , without_pair ) = B.E.partition p h.data.(i).B.content in
   ( i , with_pair ) ;;

(** {v element_find v} *)
let element_find = fun (h:t) (x:elt) ->
 try
  B.E.choose ( snd ( element_find_all h x ) )
 with _ ->
  ( fst x , Weight.zero () ) ;;

(** {v replace table element v} *)
let replace = fun (h:t) ( ( key , load ) as x: elt ) ->
 if Weight.eq_zero load then
  remove h x
 else
  begin
   let i = Hasher.hash key mod (Array.length h.data)
   and p = function z -> Index.compare key ( fst z ) = 0 in
    let ( with_key , without_key ) = B.E.partition p h.data.(i).B.content
    and previous = h.data.(i).B.cardinal in
     h.data.(i).B.content <- B.E.add x without_key ;
     let difference = 1 - ( B.E.cardinal with_key ) in
      h.data.(i).B.cardinal <- previous + difference ;
      h.filling <- h.filling + difference ;
  end ;;

(** {v in_place_map weight_function table v} *)
let in_place_map = fun f (h:t) ->
 let r = Array.length h.data
 and ff = function ( i , x ) -> ( i , f x ) in
  for i = 0 to pred r do
   h.data.(i) <- B.map ff h.data.(i) ;
  done ;;

(** {v in_place_mapi function table v} *)
let in_place_mapi = fun f (h:t) ->
 let r = Array.length h.data
 and ff = function ( i , x ) -> ( i , f i x ) in
  for i = 0 to pred r do
   h.data.(i) <- B.map ff h.data.(i) ;
  done ;;

(** {v in_place_opp table v} *)
let in_place_opp = function (h:t) ->
 let r = Array.length h.data in
  for i = 0 to pred r do
   h.data.(i) <- B.opp h.data.(i) ;
  done ;;

(** {v iter function table v} *)
let iter = fun f (h:t) ->
 let r = Array.length h.data
 and accu = ref 0 in
  for i = 0 to pred r do
   B.E.iter f h.data.(i).B.content ;
   let contribution = B.E.cardinal h.data.(i).B.content in
    h.data.(i).B.cardinal <- contribution ;
    accu := !accu + contribution ;
  done ;
  h.filling <- !accu ;
  if h.auto then
   auto_resize h ;;


(** {v copy copy_function table v} The copy function is supposed not to change the hash value of the first factor.

La fonction de recopie est censée ne pas changer la valeur de hachage du premier facteur. *)
let copy = fun f (h:t) ->
 let r = Array.length h.data in
  let hh = create r in
   for i = 0 to pred r do
    hh.data.(i) <- B.map f h.data.(i) ;
   done ;
   hh.filling <- h.filling ;
   hh.auto <- h.auto ;
   hh ;;

(** {v map weight_function table v} *)
let map = fun (f:weight -> weight) (h:t) ->
 let r = Array.length h.data
 and accu = ref 0
 and ff = function ( a , b ) -> ( a , f b ) in
  let hh = create r in
   for i = 0 to pred r do
    hh.data.(i) <- B.map ff h.data.(i) ;
    accu := !accu + B.cardinal hh.data.(i) ;
   done ;
   hh.filling <- !accu ;
   hh.auto <- h.auto ;
   if hh.auto then
    auto_resize hh ;
   hh ;;

(** {v mapi function table v} *)
let mapi = fun f (h:t) ->
 let r = Array.length h.data
 and accu = ref 0
 and ff = function ( a , b ) -> ( a , f a b ) in
  let hh = create r in
   for i = 0 to pred r do
    hh.data.(i) <- B.map ff h.data.(i) ;
    accu := !accu + B.cardinal hh.data.(i) ;
   done ;
   hh.filling <- !accu ;
   hh.auto <- h.auto ;
   if hh.auto then
    auto_resize hh ;
   hh ;;

(** {v opp table v} *)
let opp = function (h:t) ->
 let r = Array.length h.data in
  let hh = create r in
   for i = 0 to pred r do
    hh.data.(i) <- B.opp h.data.(i) ;
   done ;
   hh.filling <- h.filling ;
   hh.auto <- h.auto ;
   hh ;;

(** {v fold function table init v} *)
let fold = fun (f:elt -> 'a -> 'a) (h:t) (init:'a) ->
 let r = Array.length h.data
 and accu = ref init in
  for i = 0 to pred r do
   accu := B.fold f h.data.(i) !accu ;
  done ;
  !accu ;;

(** {v dump table v} *)
let dump = function (h:t) ->
 let r = Array.length h.data
 and cmp = fun x y -> Index.compare ( fst x ) ( fst y )
 and accu = ref [] in
  for i = 0 to pred r do
   accu := List.merge cmp ( B.to_list h.data.(i) ) !accu ;
  done ;
  !accu ;;

(** {v flush table v} *)
let flush = function (h:t) ->
 let r = Array.length h.data
 and cmp = fun x y -> Index.compare ( fst x ) ( fst y )
 and accu = ref [] in
  for i = 0 to pred r do
   accu := List.merge cmp ( B.to_list h.data.(i) ) !accu ;
   h.data.(i) <- B.empty () ;
  done ;
  h.filling <- 0 ;
  !accu ;;

(** {v of_list size element_list v} *)
let of_list = fun (size:int) (x:elt list) ->
 let h = create size in
  ignore ( List.rev_map ( add h ) x ) ;
  h ;;

(** {v min table v} *)
let min = function (h:t) ->
 let accu = ref ( Weight.zero () ) in
  let f = function ( i , x ) ->
   if compare x !accu < 0 then
    accu := x in
   iter f h ;
   !accu ;;

(** {v max table v} *)
let max = fun (h:t) ->
 let accu = ref ( Weight.zero () )in
  let f = function ( i , x ) ->
   if compare x !accu > 0 then
    accu := x in
   iter f h ;
   !accu ;;

(** {v extract index table v} *)
let extract = fun (i:index) (h:t) ->
 let j = Hasher.hash i mod (Array.length h.data) in
  try
   B.extract i h.data.(j)
  with _ ->
   failwith "Bad index in Hash.Make.extract." ;;
 




(** {C § § § } *)



end ;;


end ;;


