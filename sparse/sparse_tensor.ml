




module Sparse_tensor = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)



(**
The mathematician will find in this module functors to handle
sparse tensors with coefficients in a commutative rng or a field.


{2 Conventions}


Dimensions are often unreachable in a sparse tensor as soon as it is null (excepted sometimes for the first variable). 


{2 Comments}



A function is {e sealed} if there is no sharing between the input variables and the output value.
This is the expected behavior of usual mathematical functions.
The recursive programming of polymorphic functions is easier in a {e non sealed} way.
Some copy functions are provided for every type of data.
They are sealed provided that they receive as argument elementary copy functions for coefficients and indices.
By composition, they permit to seal all functions necessary.


This module is distributed under the same licence as Ocaml.


{C § }



La mathématicienne ou le mathématicien trouvera dans ce module des foncteurs pour traiter
les tenseurs creux à coefficients dans un annau commutatif ou un corps commutatif.


{2 Conventions}



Les dimensions d'un tenseur creux sont souvent inaccessibles (sauf parfois pour la première variable) dès qu'il est nul.



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
{2 Tenseurs creux à coefficients dans un annau commutatif}
{2 Sparse tensors with coefficients in a commutative rng}
*)
(** {C  } *)




open Util ;;
open Data ;;
open Hash ;;
open Sparse_vector ;;



module Rng (Index:Data.Index_type) (Hasher:Hash.Hash_type with type t = Index.t) (Coeff:Data.Rng_coeff_type) = struct



type index = Index.t ;;

type coeff = Coeff.t ;;

type elt = index * coeff ;;


module V = Sparse_vector.Rng (Index) (Hasher) (Coeff) ;;


(** Sparse tensors are built in a flat manner as sparse vectors with multi-indices.

Les tenseurs creux sont construits à plat comme des vecteurs creux avec des multi-indices. *)

module Multi_index = Data.Multi_index (Index) ;;


module Multi_hasher = Hash.Multi_hasher (Index) (Hasher) ;;


module M = Sparse_vector.Rng (Multi_index) (Multi_hasher) (Coeff) ;;


(** In order to have access to the different levels of a tensor, the coordinates of a multi-index are necessary.

Pour accéder aux différents niveaux d'un tenseur, les coordonnées d'un multi-indice sont nécessaires. *)


module Multi_hash = Sparse_vector.Rng (Multi_index) (Multi_hasher) (Data.Zcoeff) ;;


module Info = Sparse_vector.Rng (Index) (Hasher) (Multi_hash) ;;





(** The type [t] is either a sparse vector or a flat tensor which contains the thickness, the information tree and the multi-indices vector.

Le type [t] est soit un vecteur creux soit un tenseur à plat qui contient l'épaisseur, l'arbre d'information et le vecteur à multi-indices. *)
type t =
 | Vector of V.t
 | Flat_tensor of int * Info.t array * M.t ;;


(**

Pour trouver les éléments du tenseur dont le kème indice est i, on extrait le kème élément du tableau qui est de type [Info.t].
Dans ce vecteur, on extrait l'élément de coordonnée [Hasher.hash i] (la dimension est toujours [1]) .
Cet élément est un vecteur creux dont les indices sont des multi-entiers et les coefficients sont les valeurs de hachage des multi-indices du tenseur (la dimension est toujours [Array.make (e+1) 1]).
L'indice est la valeur de hachage du kème indice.
Les réponses sont dedans, il faut continuer avec des tests sur le tenseur lui-même, mais seulement en extrayant les éléments dont la valeur de hachage est lue dans les coefficients des éléments de ce vecteur.
Il est possible de sélectionner en deuxième étape des contraintes supplémentaires sur l'ensemble des indices en testant les multi_indices de ce vecteur (attention à la permutation circulaire).

Attention ! les vecteurs Info.t peuvent donner des coefficients nuls dans la lecture du tenseur parce qu'il est difficile d'effacer dans le tableau d'information lors d'une modification en place. *)



(** {v copy tensor v} *)
let copy = function (w:t) ->
 match w with
 | Vector v -> Vector ( V.copy v )
 | Flat_tensor ( e , t , v ) -> Flat_tensor ( e , Array.map Info.copy t , M.copy v ) ;;

(** {v info_augment tensor v} *)
let info_augment = function (w:t) ->
 match w with
 | Vector v -> ()
 | Flat_tensor ( e , t , v ) ->
  begin
   let f = function ( index , coefficient ) ->
    begin
     let h = Multi_hasher.hash index in
      for j = 0 to e do
       let information = t.(j)
       and i = index.(j) in
        let x = Info.raw_extract i information in
         Multi_hash.replace h index x ;
         Info.replace x i information ;
      done ;
    end in
    M.iter f v
  end ;;

(** {v info_cleanup tensor v} *)
let info_cleanup = function (w:t) ->
 match w with
 | Vector v -> ()
 | Flat_tensor ( e , t , v ) ->
  begin
   let ( dim , table ) = v in
    let s = Array.length table.M.H.data in
     let f = fun x ( index , coefficient ) ->
      begin
       let ensemble = table.M.H.data.( coefficient mod s )
       and p = function ( i , z ) -> Multi_index.eq i index in
        if not ( M.H.B.exists p ensemble ) then
         Multi_hash.remove index x
      end in
      let g = function ( index , coefficient ) -> Multi_hash.iter ( f coefficient ) coefficient in
      for i = 0 to e do
       let information = t.(i) in
        Info.iter g information ;
        Info.cleanup information ;
      done ;
  end ;;

(** {v info_update tensor v} *)
let info_update = function (w:t) ->
 info_augment w ;
 info_cleanup w ;;


(** {v cleanup tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let cleanup = function (w:t) ->
 match w with
 | Vector v -> V.cleanup v
 | Flat_tensor ( e , t , v ) ->
  begin
   M.cleanup v ;
   info_update w ;
  end ;;

(** {v resize size tensor v} *)
let resize = fun (n:int) (w:t) ->
 match w with
 | Vector v -> V.resize n v
 | Flat_tensor ( e , t , v ) -> M.resize n v ;;

(** {v thickness tensor v} *)
let thickness = function (w:t) ->
 match w with
 | Vector v -> 0
 | Flat_tensor ( e , t , v ) -> e ;;

(** {v vector_demakeup tensor v} *)
let vector_demakeup = function (w:t) ->
 match w with
 | Vector v -> v
 | Flat_tensor ( e , t , v ) -> failwith "Not a Vector in Sparse.Rng_tensor.vector_demakeup." ;;

(** {v flat_tensor_demakeup tensor v} *)
let flat_tensor_demakeup = function (w:t) ->
 match w with
 | Vector v -> failwith "Not a Flat_tensor in Sparse.Rng_tensor.flat_tensor_demakeup."
 | Flat_tensor ( e , t , v ) -> ( e , t , v ) ;;

(** {v flat_tensor_info tensor v} *)
let flat_tensor_info = function (w:t) ->
 match w with
 | Vector v -> failwith "Not a Flat_tensor in Sparse.Rng_tensor.flat_tensor_info."
 | Flat_tensor ( e , t , v ) -> v ;;

(** {v flat_tensor_bare_demakeup tensor v} *)
let flat_tensor_bare_demakeup = function (w:t) ->
 match w with
 | Vector v -> failwith "Not a Flat_tensor in Sparse.Rng_tensor.flat_tensor_bare_demakeup."
 | Flat_tensor ( e , t , v ) -> v ;;

(** {v null multi_dimension v} *)
let null = function (i:index array) ->
 let ee = Array.length i in
  Flat_tensor ( pred ee , Array.map Info.null ( Array.map Index.zero ( Array.make ee () ) ) , M.null i ) ;;

(** {v zero unit v} *)
let zero = function () ->
 Vector ( V.zero () ) ;;

(** {v dimensions tensor v} *)
let dimensions = function (w:t) ->
 match w with
 | Vector v -> [| V.dimension v |]
 | Flat_tensor ( e , t , v ) -> M.dimension v ;;

(** {v filling tensor v} *)
let filling = function (w:t) ->
 match w with
 | Vector v -> V.filling v
 | Flat_tensor ( e , t , v ) -> M.filling v ;;

(** {v sizes tensor v} *)
let sizes = function (w:t) ->
 match w with
 | Vector v -> ( V.size v , [| |] )
 | Flat_tensor ( e , t , v ) -> ( M.size v , Array.map Info.size t ) ;;

(** {v size tensor v} *)
let size = function (w:t) ->
 match w with
 | Vector v -> V.size v
 | Flat_tensor ( e , t , v ) -> M.size v ;;

(** {v eq_zero tensor v} *)
let eq_zero = function (w:t) ->
 match w with
 | Vector v -> V.eq_zero v
 | Flat_tensor ( e , t , v ) -> M.eq_zero v ;;

(** {v in_place_opp tensor v} *)
let in_place_opp = function (w:t) ->
 match w with
 | Vector v -> V.in_place_opp v
 | Flat_tensor ( e , t , v ) -> M.in_place_opp v ;;

(** {v opp tensor v} *)
let opp = function (w:t) ->
 match w with
 | Vector v -> Vector ( V.opp v )
 | Flat_tensor ( e , t , v ) -> Flat_tensor ( e , Array.map Info.copy t , M.opp v ) ;;

(** {v flatten tensor v} *)
let flatten = function (w:t) ->
 match w with
 | Vector v ->
  begin
   let d = V.dimension v
   and t = [| Info.zero () |]
   and f = V.filling v
   and accu = ref ( M.H.B.empty () )
   and table = snd v in
    let source = ( snd v ).V.H.data
    and h = function ( i , x ) ->
     begin
      let ii = [| i |] in
       let x = Multi_hash.null [| Index.zero () |] in
        Multi_hash.insert_add ( Multi_hasher.hash ii ) ii x ;
        Info.insert_add x i t.(0)
     end
    and g = fun ( i , x )-> M.H.B.add ( [| i |] , x ) !accu in
     V.iter h v ;
     let r = Array.length source in
      let goal = ( M.H.create r ).M.H.data in
       for i = 0 to pred r do
        V.H.B.iter g source.(i) ;
        goal.(i) <- !accu ;
        accu := M.H.B.empty () ;
       done ;
       let vv = ( [| d |] , { M.H.filling = f ; M.H.auto = table.V.H.auto ; M.H.data = goal } ) in
       Flat_tensor ( 0 , t , vv )
  end
 | Flat_tensor ( e , t , v ) -> w ;;

(** {v tensor_to_vector tensor v} *)
let tensor_to_vector = function (w:t) ->
 match w with
 | Vector v -> w
 | Flat_tensor ( e , t , v ) ->
  begin
   if e <> 0 then failwith "Bad thickness in Sparse.Rng_tensor.tensor_to_vector." ;
   let d = M.dimension v
   and f = M.filling v
   and accu = ref ( V.H.B.empty () )
   and source = ( snd v ).M.H.data in
    let r = Array.length source
    and g = fun ( i , x )-> V.H.B.add ( i.(0) , x ) !accu in
     let goal = ( V.H.create r ).V.H.data in
      for i = 0 to pred r do
       M.H.B.iter g source.(i) ;
       goal.(i) <- !accu ;
       accu := V.H.B.empty () ;
      done ;
      Vector ( d.(0) , { V.H.filling = f ; V.H.auto = ( snd v ).M.H.auto ; V.H.data = goal } )
  end ;;

(** {v to_string tensor v} *)
let rec to_string = function (w:t) ->
 match w with
 | Vector v -> to_string ( flatten w )
 | Flat_tensor ( e , t , v ) ->
  begin
   let ee = string_of_int e
   and tt = Util.vector_to_string ( Info.special_to_string '!' "<" "@" ">;" ) "{" "_" "}" t
   and vv = M.to_string v in
    "(" ^ ee ^ "#" ^ tt ^ "#" ^ vv ^ ")"
  end ;;

(** {v bare_to_string tensor v} *)
let rec bare_to_string = function (w:t) ->
 match w with
 | Vector v -> to_string ( flatten w )
 | Flat_tensor ( e , t , v ) ->
  begin
   let ee = string_of_int e
   and vv = M.to_string v in
    "(" ^ ee ^ "#" ^ vv ^ ")"
  end ;;

(** {v of_string string v} *)
let rec of_string = function (s:string) ->
 let a = ref ( String.index s '#' ) in
  let e = int_of_string ( String.sub s 1 ( max 0 ( pred !a ) ) )
  and rest = ref ( String.sub s ( !a + 1 ) ( max 0 ( String.length s - 2 - !a ) ) ) in
   a := String.index !rest '#' ;
   let t = ref ( Array.map Info.null ( Array.map Index.zero ( Array.make ( succ e ) () ) ) ) in
    let chaine = String.sub !rest 0 ( max 0 ( !a ) ) in
     t := Util.vector_of_string ( Info.special_from_string '!' "<" "@" ">;" (-1) ) "{" "_" "}" chaine ;
     rest := String.sub !rest ( !a + 1 ) ( max 0 ( String.length !rest - 1 - !a ) ) ;
     let v = M.of_string !rest in
      Flat_tensor ( e , !t , v ) ;;

(** {v print tensor v} *)
let print = function (w:t) ->
 print_string ( to_string w ) ;;

(** {v eq tensor1 tensor2 v} *)
let rec eq = fun (w:t) (x:t) ->
 match w with
 | Vector v ->
  begin
   match x with
   | Vector y -> V.eq v y
   | Flat_tensor ( ee , tt , y ) -> ( ee = 0 ) && ( eq w ( tensor_to_vector x ) )
  end
 | Flat_tensor ( e , t , v ) ->
  begin
   match x with
   | Vector y -> eq ( tensor_to_vector w ) x
   | Flat_tensor ( ee , tt , y ) -> ( e = ee ) && ( Util.array_eq Info.eq t tt ) && ( M.eq v y )
  end ;;

(** {v unsafe_extract index_array tensor v} *)
let unsafe_extract = fun (i:index array) (w:t) ->
 let e = Array.length i in
  match w with
  | Vector v ->
   begin
    if e <> 0 then failwith "Bad thickness in Sparse.Rng_tensor.unsafe_extract." ;
    let ( j , x ) = V.unsafe_extract i.(0) v in
     ( [| j |] , x )
   end
  | Flat_tensor ( ee , t , v ) ->
   begin
    if e <> ee then failwith "Bad thickness in Sparse.Rng_tensor.unsafe_extract." ;
    M.unsafe_extract i v
   end ;;

(** {v extract index_array tensor v} *)
let extract = fun (i:index array) (w:t) ->
 let e = pred ( Array.length i ) in
  match w with
  | Vector v ->
   begin
    if e <> 0 then failwith "Bad thickness in Sparse.Rng_tensor.extract." ;
    let ( j , x ) = V.extract i.(0) v in
     ( [| j |] , x )
   end
  | Flat_tensor ( ee , t , v ) ->
   begin
    if e <> ee then failwith "Bad thickness in Sparse.Rng_tensor.extract." ;
    M.extract i v
   end ;;

(** {v raw_extract index_array tensor v} *)
let raw_extract = fun (i:index array) (w:t) ->
 snd ( extract i w ) ;;

(** {v is_present index_array tensor v} *)
let is_present = fun (i:index array) (w:t) ->
 let e = pred ( Array.length i )
 and ee = thickness w in
  if e <> ee then failwith "Bad thickness in Sparse.Rng_tensor.is_present." ;
  try
   begin
    ignore ( unsafe_extract i w ) ;
    true
   end
  with _ -> false ;;

(** {v find coefficient tensor v} *)
let find = fun (c:coeff) (w:t) ->
 match w with
 | Vector v -> [| V.find c v |]
 | Flat_tensor ( e , t , v ) ->
  begin
   let index = M.find c v
   and ee = succ e in
    if Array.length index < ee then
     Array.map Index.witness ( Array.make ee () ) 
    else
     index
  end ;;

(** {v list_find_all coefficient tensor v} *)
let list_find_all = fun (c:coeff) (w:t) ->
 match w with
 | Vector v ->
  begin
   let f = function ( i , x ) -> ( [| i |] , x ) in
    List.rev_map f ( V.list_find_all c v )
  end
 | Flat_tensor ( e , t , v ) -> M.list_find_all c v ;;

(** {v index_list_find_all coefficient tensor v} *)
let index_list_find_all = fun (c:coeff) (w:t) ->
 match w with
 | Vector v -> List.rev_map ( Array.make 1 ) ( V.index_list_find_all c v )
 | Flat_tensor ( e , t , v ) -> M.index_list_find_all c v ;;

(** {v filter predicate tensor v} *)
let filter = fun (p:index array -> bool) (w:t) ->
 match w with
 | Vector v ->
  begin
   let f = function ( i , x ) -> ( [| i |] , x ) in
    List.rev_map f ( V.filter ( function i -> p [| i |] ) v )
  end
 | Flat_tensor ( e , t , v ) -> M.filter p v ;;

(** {v sub_tensor_extract level index tensor v} *)
let rec sub_tensor_extract = fun (k:int) (i:index) (w:t) ->
 match w with
 | Vector v ->
  begin
   assert ( k = 0 ) ;
   let z = V.null ( Index.zero () ) in
    V.insert_add ( V.raw_extract i v ) ( Index.witness () ) z ;
    Vector z
  end
 | Flat_tensor ( e , t , v ) ->
  begin
   if e = 0 then
    sub_tensor_extract k i ( tensor_to_vector w )
   else
    begin
     try
      begin
       let ( dim , table ) = v
       and ee = pred e
       and tt = Array.map ( Info.null ) ( Array.map Index.zero ( Array.make ( succ e ) () ) )
       and hints = t.(k) in
        let d = Util.array_forget k dim
        and range = Info.raw_extract i hints in
         let result = M.null d
         and candidates = Multi_hash.elements range in
          let ( indices , hash_values ) = List.split candidates
          and f = function ( index , coefficient ) ->
           begin
            if Index.eq i index.(k) then
             begin
              let new_index = Util.array_forget k index in
               let h = Multi_hasher.hash new_index in
                for level = 0 to ee do
                 let indice = new_index.(level)
                 and ttt = tt.(level) in
                  let element = Info.raw_extract indice ttt in
                   Multi_hash.replace h new_index element ;
                   Info.replace element indice ttt ;
                done
             end
           end
          and g = function ( index , coefficient ) ->
           begin
            if Index.eq i index.(k) then
             begin
              let new_index = Util.array_forget k index in
               M.replace coefficient new_index result ;
             end
           end in
           let hv = Array.of_list hash_values
           and taille = Array.length table.M.H.data in
            let u = table.M.H.data.( hv.(0) mod taille ) in
             M.H.B.iter f u ;
             M.H.B.iter g table.M.H.data.( hv.(0) mod taille ) ;
             for j = 1 to pred ( Array.length hv ) do
              let s = table.M.H.data.( hv.(j) mod taille ) in
               M.H.B.iter f s ;
             done ;
             Flat_tensor ( ee , tt , result )
      end
     with _ ->
      begin
       let d = Util.array_forget k ( dimensions w ) in
        Flat_tensor ( pred e , Array.map Info.null ( Array.map Index.zero ( Array.make e () ) ) , M.null d )
      end
    end
  end ;;


(** {v insert_add coefficient index_array tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let insert_add = fun (x:coeff) (i:index array) (w:t) ->
 if ( Array.length i ) <> ( succ ( thickness w ) ) then failwith "Bad thickness in Sparse.Rng_tensor.insert." ;
 match w with
 | Vector v -> V.insert_add x i.(0) v
 | Flat_tensor ( e , t , v ) ->
  begin
   M.insert_add x i v ;
   let test = M.raw_extract i v in
    if not ( Coeff.eq_zero test ) then
     begin
      let h = Multi_hasher.hash i in
       for level = 0 to e do
        let indice = i.(level) in
         let element = Info.raw_extract indice t.(level) in
          Multi_hash.replace h i element ;
          Info.replace element indice t.(level) ;
       done ;
     end
  end ;;


(** {v insert_sub coefficient index_array tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let insert_sub = fun (x:coeff) (i:index array) (w:t) ->
 if ( Array.length i ) <> ( succ ( thickness w ) ) then failwith "Bad thickness in Sparse.Rng_tensor.insert." ;
 match w with
 | Vector v -> V.insert_sub x i.(0) v
 | Flat_tensor ( e , t , v ) ->
  begin
   M.insert_sub x i v ;
   let test = M.raw_extract i v in
    if not ( Coeff.eq_zero test ) then
     begin
      let h = Multi_hasher.hash i in
       for level = 0 to e do
        let indice = i.(level) in
         let element = Info.raw_extract indice t.(level) in
          Multi_hash.replace h i element ;
          Info.replace element indice t.(level) ;
       done ;
     end
  end ;;


(** {v replace coefficient index_array tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let replace = fun (x:coeff) (i:index array) (w:t) ->
 if ( Array.length i ) <> ( succ ( thickness w ) ) then failwith "Bad thickness in Sparse.Rng_tensor.replace." ;
 match w with
 | Vector v -> V.replace x i.(0) v
 | Flat_tensor ( e , t , v ) ->
  begin
   M.replace x i v ;
   let test = M.raw_extract i v in
    if not ( Coeff.eq_zero test ) then
     begin
      let h = Multi_hasher.hash i in
       for level = 0 to e do
        let indice = i.(level) in
         let element = Info.raw_extract indice t.(level) in
          Multi_hash.replace h i element ;
          Info.replace element indice t.(level) ;
       done ;
     end
  end ;;


(** {v remove index_array tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let remove = fun (i:index array) (w:t) ->
 if ( Array.length i ) <> ( succ ( thickness w ) ) then failwith "Bad thickness in Sparse.Rng_tensor.remove." ;
 match w with
 | Vector v -> V.remove i.(0) v
 | Flat_tensor ( e , t , v ) ->
  begin
   let test = M.raw_extract i v in
    if not ( Coeff.eq_zero test ) then
     begin
      for level = 0 to e do
       let indice = i.(level) in
        let element = Info.raw_extract indice t.(level) in
         Multi_hash.remove i element ;
         Info.replace element indice t.(level) ;
       done ;
     end ;
    M.remove i v ;
  end ;;

(** {v sub_tensor_remove level index tensor v} *)
let sub_tensor_remove = fun (k:int) (i:index) (w:t) ->
 match w with
 | Vector v ->
  begin
   assert ( k = 0 ) ;
    V.remove i v
  end
 | Flat_tensor ( e , t , v ) ->
  begin
   let tk = t.(k)
   and f = fun y (( index , coefficient ):index array * coeff) ->
    begin
     if Index.eq i index.(k) then
      begin
       M.H.B.remove ( index , Coeff.zero () ) y ;
       for j = 0 to pred k do
        let jj = index.(j)
        and tj = t.(j) in
         let x = Info.raw_extract jj tj in
          Multi_hash.remove index x ;
          Info.replace x jj tj ;
       done ;
       for j = succ k to e do
        let jj = index.(j)
        and tj = t.(j) in
         let x = Info.raw_extract jj tj in
          Multi_hash.remove index x ;
          Info.replace x jj tj ;
       done ;
      end
    end
   and ( dim , table ) = v in
    let tki = Info.raw_extract i tk
    and s = Array.length table.M.H.data in
     let g = function (( index , coefficient ):index array * int) ->
      begin
       let u = table.M.H.data.( coefficient mod s ) in
        M.H.B.iter ( f u ) u ;
      end in
      Multi_hash.iter g tki ;
      Info.remove i tk ;
  end ;;

(** {v sub_tensor_replace sub_tensor level index tensor v} *)
let sub_tensor_replace = fun (x:t) (k:int) (i:index) (w:t) ->
 match w with
 | Vector v ->
  begin
   assert ( k = 0 ) ;
   let e = V.elements ( vector_demakeup x ) in
    assert ( List.length e = 1 ) ;
    let y = snd ( List.hd e ) in
     V.replace y i v
  end
 | Flat_tensor ( e , t , v ) ->
  begin
   sub_tensor_remove k i w ;
   let f = function ( index , coefficient ) ->
    begin
     let new_index = Util.array_insert k i index in
      insert_add coefficient new_index w ;
      for j = 0 to e do
       let tj = t.(j)
       and indj = new_index.(j) in
        let xx = Info.raw_extract indj tj in
         Multi_hash.replace ( Multi_hasher.hash new_index ) new_index xx ;
         Info.replace xx indj tj ;
      done ;
    end in
    M.iter f ( flat_tensor_bare_demakeup ( flatten x ) )
  end ;;

(** {v iter function tensor v} *)
let iter = fun (f:M.elt -> unit) (w:t) ->
 match w with
 | Vector v ->
  begin
   let ff = function ( i , x ) -> f ( [| i |] , x ) in
    V.iter ff v
  end
 | Flat_tensor ( e , t , v ) -> M.iter f v ;;

(** {v in_place_map function tensor v} *)
let in_place_map = fun (f:coeff -> coeff) (w:t) ->
 match w with
 | Vector v -> V.in_place_map f v
 | Flat_tensor ( e , t , v ) -> M.in_place_map f v ;;

(** {v in_place_mapi function tensor v} *)
let in_place_mapi = fun f (w:t) ->
 match w with
 | Vector v ->
  begin
   let g = fun i x -> f [| i |] x in
    V.in_place_mapi g v
  end
 | Flat_tensor ( e , t , v ) -> M.in_place_mapi f v ;;

(** {v raw_map function tensor v} *)
let raw_map = fun (f:coeff -> coeff) (w:t) ->
 match w with
 | Vector v -> Vector ( V.map f v )
 | Flat_tensor ( e , t , v ) -> Flat_tensor ( e , Array.map Info.copy t , M.map f v ) ;;

(** {v map function tensor v} *)
let map = fun (f:coeff -> coeff) (w:t) ->
 match w with
 | Vector v -> Vector ( V.map f v )
 | Flat_tensor ( e , t , v ) ->
  begin
   let result = Flat_tensor ( e , Array.map Info.copy t , M.map f v ) in
    info_update result ;
    result
  end ;;

(** {v mapi function tensor v} *)
let mapi = fun f (w:t) ->
 match w with
 | Vector v ->
  begin
   let g = fun i x -> f [| i |] x in
    Vector ( V.mapi g v )
  end
 | Flat_tensor ( e , t , v ) ->
  begin
   let result = Flat_tensor ( e , Array.map Info.copy t , M.mapi f v ) in
    info_update result ;
    result
  end ;;

(** {v fold function tensor init v} *)
let fold = fun (f:M.elt -> 'a -> 'a) (w:t) (init:'a) ->
 match w with
 | Vector v ->
  begin
   let ff = function ( i , x ) -> f ( [| i |] , x ) in
    V.fold ff v init
  end
 | Flat_tensor ( e , t , v ) -> M.fold f v init ;;

(** {v sub_flat_tensor index_array tensor v} *)
let sub_flat_tensor = fun (i:index array) (w:t) ->
 match w with
 | Vector v -> failwith "Not a flat tensor in Sparse.Rng_tensor.sub_tensor."
 | Flat_tensor ( e , t , v ) ->
  begin
   let d = M.dimension v
   and shift = Array.length i in
    let complement = e - shift + 1
    and p = function j -> Util.array_eq Index.eq i ( Array.sub j 0 shift ) in
     if shift > e then
      failwith "Not a valid index array in Sparse.Rng_tensor.sub_tensor." ;
     let dd = Array.sub d shift complement in
      let result = null dd in
       let f = fun ( i , x ) -> if p (i) then insert_add x ( Array.sub i shift complement ) result in
        iter f w ;
        info_update result ;
        result
  end ;;

(** {v suffix_sub_flat_tensor index_array tensor v} *)
let suffix_sub_flat_tensor = fun (i:index array) (w:t) ->
 match w with
 | Vector v -> failwith "Not a flat tensor in Sparse.Rng_tensor.suffix_sub_tensor."
 | Flat_tensor ( e , t , v ) ->
  begin
   let d = M.dimension v
   and complement = Array.length i in
    if complement > e then
      failwith "Not a valid index array in Sparse.Rng_tensor.suffix_sub_tensor." ;
    let shift = e - complement + 1 in
     let dd = Array.sub d 0 shift
     and p = function j -> Util.array_eq Index.eq i ( Array.sub j shift complement ) in
      let result = null dd in
       let f = fun ( i , x ) -> if p(i) then insert_add x ( Array.sub i 0 shift ) result in
        iter f w ;
        info_update result ;
        result
  end ;;

(** {v sub_vector_tensor index_array tensor v} *)
let sub_vector_tensor = fun (i:index array) (w:t) ->
 match w with
 | Vector v -> failwith "Not a flat tensor in Sparse.Rng_tensor.sub_vector."
 | Flat_tensor ( e , t , v ) ->
  begin
   let d = M.dimension v
   and shift = Array.length i in
    if shift <> e then
     failwith "Not a valid index array in Sparse.Rng_tensor.sub_vector." ;
    let p = function j -> Util.array_eq Index.eq i ( Array.sub j 0 shift )
    and dd = Util.array_last d in
     let result = V.null dd in
      let f = fun ( i , x ) -> if p i then V.insert_add x ( Util.array_last i ) result in
       iter f w ;
       result
  end ;;

(** {v suffix_sub_vector_tensor index_array tensor v} *)
let suffix_sub_vector_tensor = fun (i:index array) (w:t) ->
 match w with
 | Vector v -> failwith "Not a flat tensor in Sparse.Rng_tensor.suffix_sub_vector."
 | Flat_tensor ( e , t , v ) ->
  begin
   let d = M.dimension v
   and complement = Array.length i in
    if complement <> e then
     failwith "Not a valid index array in Sparse.Rng_tensor.suffix_sub_vector." ;
    let p = function j -> Util.array_eq Index.eq i ( Util.array_tail j )
    and dd = d.(0) in
     let result = V.null dd in
      let f = fun ( i , x ) -> if p i then V.insert_add x i.(0) result in
       iter f w ;
       result
  end ;;

(** {v embed dimensions shift tensor v} *)
let embed = fun (dimensions:index array) (shift:index array) (w:t) ->
 match w with
 | Vector v ->
  begin
   assert ( ( Array.length dimensions = 1 ) && ( Array.length shift = 1 ) ) ;
   Vector ( V.embed dimensions.(0) shift.(0) v )
  end
 | Flat_tensor ( e , t , v ) ->
  begin
   let ee = succ e
   and tt = Array.map Info.zero ( Array.make ( succ e ) () ) in
    assert ( ( Array.length dimensions = ee ) && ( Array.length shift = ee ) ) ;
    let w = Flat_tensor ( e , tt , M.embed dimensions shift v ) in
     info_augment w ;
     w
  end ;;

(** {v min tensor v} *)
let min = function (w:t) ->
 match w with
 | Vector v -> V.min v
 | Flat_tensor ( e , t , v ) -> M.min v ;;

(** {v max tensor v} *)
let max = function (w:t) ->
 match w with
 | Vector v -> V.max v
 | Flat_tensor ( e , t , v ) -> M.max v ;;

(** {v in_place_opp tensor v} *)
let in_place_opp = function (w:t) ->
 match w with
 | Vector v -> V.in_place_opp v
 | Flat_tensor ( e , t , v ) -> M.in_place_opp v ;;

(** {v opp tensor v} *)
let opp = function (w:t) ->
 match w with
 | Vector v -> Vector ( V.opp v )
 | Flat_tensor ( e , t , v ) -> Flat_tensor ( e , Array.map Info.copy t , M.opp v ) ;;


(** {v in_place_add tensor1 tensor2 v} The first tensor stores the result.

Le premier tenseur accueille le résultat. *)
let in_place_add = fun (w:t) (x:t) ->
 let error_message = "Bad thickness in Sparse.Rng_tensor.in_place_add." in
  match w with
  | Vector u ->
   begin
    match x with
    | Vector y -> V.in_place_add u y
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if ee <> 0 then
       failwith error_message ;
      V.in_place_add u ( vector_demakeup ( tensor_to_vector x ) )
     end
   end
  | Flat_tensor ( e , t , v ) ->
   begin
    match x with
    | Vector y ->
     begin
      if e <> 0 then
       failwith error_message ;
      M.in_place_add v ( flat_tensor_bare_demakeup ( flatten x ) )
     end
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      M.in_place_add v vv ;
      info_update w ;
     end
   end ;;

(** {v add tensor1 tensor2 v} *)
let rec add = fun (w:t) (x:t) ->
 let error_message = "Bad thickness in Sparse.Rng_tensor.add." in
  match w with
  | Vector u ->
   begin
    match x with
    | Vector y -> Vector ( V.add u y )
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if ee <> 0 then
       failwith error_message ;
      Vector ( V.add u ( vector_demakeup ( tensor_to_vector x ) ) )
     end
   end
  | Flat_tensor ( e , t , v ) ->
   begin
    match x with
    | Vector y -> add x w
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if e <> ee then
       failwith error_message ;
      let result = Flat_tensor ( e , Array.map Info.zero ( Array.make ( succ e ) () ) , M.add v vv ) in
       info_augment result ;
       result
     end
   end ;;

(** {v in_place_sub tensor1 tensor2 v} *)
let in_place_sub = fun (w:t) (x:t) ->
 let error_message = "Bad thickness in Sparse.Rng_tensor.in_place_sub." in
  match w with
  | Vector u ->
   begin
    match x with
    | Vector y -> V.in_place_sub u y
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if ee <> 0 then failwith
       error_message ;
      V.in_place_sub u ( vector_demakeup ( tensor_to_vector x ) )
     end
   end
  | Flat_tensor ( e , t , v ) ->
   begin
    match x with
    | Vector y ->
     begin
      if e <> 0 then failwith
       error_message ;
      M.in_place_sub v ( flat_tensor_bare_demakeup ( flatten x ) ) ;
      info_update w ;
     end
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      M.in_place_sub v vv ;
      info_update w ;
     end
   end ;;

(** {v sub tensor1 tensor2 v} *)
let sub = fun (w:t) (x:t) ->
 let error_message = "Bad thickness in Sparse.Rng_tensor.sub." in
  match w with
  | Vector u ->
   begin
    match x with
    | Vector y -> Vector ( V.sub u y )
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if ee <> 0 then
       failwith error_message ;
      Vector ( V.sub u ( vector_demakeup ( tensor_to_vector x ) ) )
     end
   end
  | Flat_tensor ( e , t , v ) ->
   begin
    match x with
    | Vector y ->
     begin
      if e <> 0 then
       failwith error_message ;
      Vector ( V.sub ( vector_demakeup ( tensor_to_vector w ) ) y )
     end
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if e <> ee then
       failwith error_message ;
      let result = Flat_tensor ( e , Array.map Info.zero ( Array.make ( succ e ) () ) , M.sub v vv ) in
       info_augment result ;
       result 
     end
   end ;;


(** {v tensor_sample beginning ending tensor v} All the multi-indices between the two ends 
in the lexicographic order are selected.

Tous les multi-indices entre les deux bornes dans l'ordre lexicographique sont sélectionnés. *)
let tensor_sample = fun (beginning:index array) (ending:index array) (w:t) ->
 match w with
 | Vector v -> Vector ( V.sub_vector beginning.(0) ending.(0) v )
 | Flat_tensor ( e , t , v ) ->
  begin
   let result = Flat_tensor ( e , Array.map Info.zero ( Array.make ( succ e ) () ) , M.sub_vector beginning ending v ) in
    info_augment result ;
    result 
  end ;;

(** {v sum tensor v} *)
let sum = function (w:t) ->
 match w with
 | Vector v -> V.sum v
 | Flat_tensor ( e , t , v ) -> M.sum v ;;

(** {v contraction init tensor v} *)
let contraction = fun (init:coeff) (w:t) ->
 match w with
 | Vector v -> V.contraction init v
 | Flat_tensor ( e , t , v ) -> M.contraction init v ;;

(** {v in_place_scal_add scalar tensor v} *)
let in_place_scal_add = fun (scal:coeff) (w:t) ->
 match w with
 | Vector v -> V.in_place_scal_add scal v
 | Flat_tensor ( e , t , v ) ->
  begin
   M.in_place_scal_add scal v ;
   info_update w
  end ;;

(** {v scal_add scalar tensor v} *)
let scal_add = fun (scal:coeff) (w:t) ->
 match w with
 | Vector v -> Vector ( V.scal_add scal v )
 | Flat_tensor ( e , t , v ) ->
  begin
   let result = Flat_tensor ( e , Array.map Info.zero ( Array.make ( succ e ) () ) , M.scal_add scal v ) in
    info_augment result ;
    result
  end ;;

(** {v in_place_scal_mult scalar tensor v} *)
let in_place_scal_mult = fun (scal:coeff) (w:t) ->
 match w with
 | Vector v -> V.in_place_scal_mult scal v
 | Flat_tensor ( e , t , v ) ->
  begin
   M.in_place_scal_mult scal v ;
   info_update w
  end ;;

(** {v scal_mult scalar tensor v} *)
let scal_mult = fun (scal:coeff) (w:t) ->
 match w with
 | Vector v -> Vector ( V.scal_mult scal v )
 | Flat_tensor ( e , t , v ) ->
  begin
   let result = Flat_tensor ( e , Array.map Info.zero ( Array.make ( succ e ) () ) , M.scal_mult scal v ) in
    info_augment result ;
    result
  end ;;

(** {v in_place_scal_right_sub scalar tensor v} *)
let in_place_scal_right_sub = fun (scal:coeff) (w:t) ->
 match w with
 | Vector v -> V.in_place_scal_right_sub scal v
 | Flat_tensor ( e , t , v ) ->
  begin
   M.in_place_scal_right_sub scal v ;
   info_update w
  end ;;

(** {v scal_right_sub scalar tensor v} *)
let scal_right_sub = fun (scal:coeff) (w:t) ->
 match w with
 | Vector v -> Vector ( V.scal_right_sub scal v )
 | Flat_tensor ( e , t , v ) ->
  begin
   let result = Flat_tensor ( e , Array.map Info.zero ( Array.make ( succ e ) () ) , M.scal_right_sub scal v ) in
    info_augment result ;
    result
  end ;;

(** {v in_place_scal_left_sub scalar tensor v} *)
let in_place_scal_left_sub = fun (scal:coeff) (w:t) ->
 match w with
 | Vector v -> V.in_place_scal_left_sub scal v
 | Flat_tensor ( e , t , v ) ->
  begin
   M.in_place_scal_left_sub scal v ;
   info_update w
  end ;;

(** {v scal_left_sub scalar tensor v} *)
let scal_left_sub = fun (scal:coeff) (w:t) ->
 match w with
 | Vector v -> Vector ( V.scal_left_sub scal v )
 | Flat_tensor ( e , t , v ) ->
  begin
   let result = Flat_tensor ( e , Array.map Info.zero ( Array.make ( succ e ) () ) , M.scal_left_sub scal v ) in
    info_augment result ;
    result
  end ;;

(** {v coeff_prod tensor1 tensor2 v} *)
let rec coeff_prod = fun (w:t) (x:t) ->
 let error_message = "Bad thickness in Sparse.Rng_tensor.coeff_prod." in
  match w with
  | Vector u ->
   begin
    match x with
    | Vector y -> Vector ( V.coeff_prod u y )
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if ee <> 0 then
       failwith error_message ;
      Vector ( V.coeff_prod u ( vector_demakeup ( tensor_to_vector x ) ) )
     end
   end
  | Flat_tensor ( e , t , v ) ->
   begin
    match x with
    | Vector y -> coeff_prod x w
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if e <> ee then failwith error_message ;
      let result = Flat_tensor ( e , Array.map Info.zero ( Array.make ( succ e ) () ) , M.coeff_prod v vv ) in
       info_augment result ;
       result
     end
   end ;;

(** {v scal_prod tensor1 tensor2 v} *)
let rec scal_prod = fun (w:t) (x:t) ->
 let error_message = "Bad thickness in Sparse.Rng_tensor.scal_prod." in
  match w with
  | Vector u ->
   begin
    match x with
    | Vector y -> V.scal_prod u y
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if ee <> 0 then
       failwith error_message ;
      V.scal_prod u ( vector_demakeup ( tensor_to_vector x ) )
     end
   end
  | Flat_tensor ( e , t , v ) ->
   begin
    match x with
    | Vector y -> scal_prod x w
    | Flat_tensor ( ee , tt , vv ) ->
     begin
      if e <> ee then
       failwith error_message ;
      M.scal_prod v vv
     end
   end ;;

(** {v norm_1 tensor v} *)
let norm_1 = function (w:t) ->
 match w with
 | Vector v -> V.norm_1 v
 | Flat_tensor ( e , t , v ) -> M.norm_1 v ;;

(** {v norm_inf tensor v} *)
let norm_inf = function (w:t) ->
 match w with
 | Vector v -> V.norm_inf v
 | Flat_tensor ( e , t , v ) -> M.norm_inf v ;;

(** {v square_sum tensor v} *)
let square_sum = function (w:t) ->
 match w with
 | Vector v -> V.square_sum v
 | Flat_tensor ( e , t , v ) -> M.square_sum v ;;

(** {v square_norm_2 tensor v} *)
let square_norm_2 = function (w:t) ->
 match w with
 | Vector v -> V.square_norm_2 v
 | Flat_tensor ( e , t , v ) -> M.square_norm_2 v ;;

(** {v norm_compare norm tensor1 tensor2 v} *)
let norm_compare = fun n (w:t) (x:t) ->
 Coeff.norm_compare ( n w ) ( n x ) ;;

(** {v compare tensor1 tensor2 v} *)
let compare = fun (w:t) (x:t) ->
 norm_compare norm_1 w x ;;


(** {v exchange level index1 index2 tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let exchange = fun (level:int) (i:index) (j:index) (w:t) ->
 let error_message = "Bad level in Sparse.Rng_tensor.exchange." in
  match w with
  | Vector v ->
   begin
    if level <> 0 then
     failwith error_message ;
    if not ( Index.eq i j ) then
     V.exchange i j v ;
     V.cleanup v
   end
  | Flat_tensor ( e , t , v ) ->
   begin
    if level > e then
     failwith error_message ;
    if not ( Index.eq i j ) then
     begin
      let old_i = sub_tensor_extract level i w
      and old_j = sub_tensor_extract level j w in
       sub_tensor_replace old_i level j w ;
       sub_tensor_replace old_j level i w ;
     end
   end ;;


(** {v level_exchange level1 level2 tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let level_exchange = fun (i:int) (j:int) (w:t) ->
 let error_message = "Bad levels in Sparse.Rng_tensor.level_exchange." in
  match w with
  | Vector v ->
   begin
    if ( i <> 0 ) || ( j <> 0 ) then
     failwith error_message ;
   end
  | Flat_tensor ( e , t , v ) ->
   begin
    if ( i > e ) || ( j > e ) then
     failwith error_message ;
    if i <> j then
     begin
      let f = function ( k , x ) ->
       begin
        let accu = k.(i) in
         k.(i) <- k.(j) ;
         k.(j) <- accu ;
       end in
       M.iter f v ;
       let d = fst v in
        let accu = d.(i) in
         d.(i) <- d.(j) ;
         d.(j) <- accu ;
         cleanup w ;
     end
   end ;;

(** {v mult tensor1 tensor2 v} *)
let mult = fun (w:t) (ww:t) ->
 match w with
 | Vector v ->
  begin
   match ww with
   | Vector vv ->
    begin
     let d = [| V.dimension v ; V.dimension vv |] in
      let x = null d in
       let f = function ( index , coefficient ) -> sub_tensor_replace ( scal_mult coefficient ww ) 0 index x in
        V.iter f v ;
        x
    end
   | Flat_tensor ( ee , tt , vv ) ->
    begin
     let d = Array.append [| V.dimension v |] ( M.dimension vv ) in
      let x = null d in
       let f = function ( index , coefficient ) -> sub_tensor_replace ( scal_mult coefficient ww ) 0 index x in
        V.iter f v ;
        x
    end
  end
 | Flat_tensor ( e , t , v ) ->
  begin
   match ww with
   | Vector vv ->
    begin
     let d = Array.append ( M.dimension v ) [| V.dimension vv |] in
      let x = null d in
       let f = function ( index , coefficient ) -> sub_tensor_replace ( scal_mult coefficient w ) ( succ e ) index x in
        V.iter f vv ;
        x
    end
   | Flat_tensor ( ee , tt , vv ) ->
    begin
     let d = Array.append ( M.dimension v ) ( M.dimension vv ) in
      let x = null d in
       let f = function ( index , coefficient ) ->
        begin
         let g = function ( i , y ) -> insert_add ( Coeff.mult coefficient y ) ( Array.append index i ) x in
          M.iter g vv
        end in
        M.iter f v ;
        x
    end
  end ;;

(** {v safe_mult tensor1 tensor2 v} *)
let safe_mult = fun (w:t) (ww:t) ->
 mult ( copy w ) ( copy ww ) ;;








(** {C § § § } *)




end ;;




(** {C § } *)
(** 
{2 Tenseurs creux à coefficients dans un corps commutatif}
{2 Sparse tensors with coefficients in a field}
*)
(** {C  } *)




module Field (Index:Data.Index_type) (Hasher:Hash.Hash_type with type t = Index.t) (Coeff:Data.Field_coeff_type) = struct


include Rng (Index) (Hasher) (Coeff) ;;


module W = Sparse_vector.Field (Index) (Hasher) (Coeff) ;;


module N = Sparse_vector.Field (Multi_index) (Multi_hasher) (Coeff) ;;


(** {v in_place_inv tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let in_place_inv = function (w:t) ->
 match w with
 | Vector v -> W.in_place_inv v
 | Flat_tensor ( e , t , v ) -> N.in_place_inv v ;;

(** {v inv tensor v} *)
let inv = function (w:t) ->
 match w with
 | Vector v -> Vector ( W.inv v )
 | Flat_tensor ( e , t , v ) -> Flat_tensor ( e , Array.map Info.copy t , N.inv v ) ;;

(** {v in_place_scal_right_div scalar tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let in_place_scal_right_div = fun (x:coeff) (w:t) ->
 match w with
 | Vector v -> W.in_place_scal_right_div x v
 | Flat_tensor ( e , t , v ) -> N.in_place_scal_right_div x v ;;

(** {v scal_right_div scalar tensor v} *)
let scal_right_div = fun (x:coeff) (w:t) ->
 match w with
 | Vector v -> Vector ( W.scal_right_div x v )
 | Flat_tensor ( e , t , v ) -> Flat_tensor ( e , Array.map Info.copy t , N.scal_right_div x v ) ;;


(** {v in_place_scal_left_div scalar tensor v} This function is applied in place.

Cette fonction est appliquée en place. *)
let in_place_scal_left_div = fun (x:coeff) (w:t) ->
 match w with
 | Vector v -> W.in_place_scal_left_div x v
 | Flat_tensor ( e , t , v ) -> N.in_place_scal_left_div x v ;;

(** {v scal_left_div scalar tensor v} *)
let scal_left_div = fun (x:coeff) (w:t) ->
 match w with
 | Vector v -> Vector ( W.scal_left_div x v )
 | Flat_tensor ( e , t , v ) -> Flat_tensor ( e , Array.map Info.copy t , N.scal_left_div x v ) ;;









(** {C § § § } *)




end ;;








(** {C § § § } *)




end ;;




