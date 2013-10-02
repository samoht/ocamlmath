



module Util = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module several elementary tools
used in the other modules of the [ocamlmath] library.



{2 Comments}



This module is distributed under the same licence as Ocaml.



{C § }



La mathématicienne ou le mathématicien trouvera dans ce module différents outils
élémentaires utilisés dans les autres modules de la bibliothèque [ocamlmath].



{2 Commentaires}



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




(** {C § } *)
(** 
{1 Constructions minimales sur les entiers et réels}
{1 Minimal constructions on integer and real numbers}
*)
(** {C  } *)




(** {v int_copy integer v} *)
let int_copy = function (x:int) -> x

(** {v big_int_copy big_integer v} *)
let big_int_copy = function (x:Big_int.big_int) ->
 Big_int.add_big_int Big_int.zero_big_int x ;;

(** {v big integer integer v} *)
let big = Big_int.big_int_of_int ;;

(** {v small big_integer v} *)
let small = Big_int.int_of_big_int ;;

(** {v round_num number v} *)
let round_num = function (x:Num.num) ->
 let xx = Num.integer_num x in
  let y = Num.mult_num ( Num.num_of_int 2 ) ( Num.sub_num x xx ) in
   let yy = Num.integer_num y in
    Num.add_num xx yy ;;

(** {v adhoc_division integer1 integer2 v} *)
let adhoc_division = fun (x:int) (y:int) ->
 if y <> 0 then x / y
 else 0 ;;


(** {v int_sign integer v} *)
let int_sign = function (x:int) ->
 compare x 0 ;;

(** {v int_truncate base number v} *)
let int_truncate = fun (b:int) (x:int) ->
 if x > b then b
 else
  begin
   if x < - b then - b
   else
    x
  end ;;

(** {v int_gcd integer1 integer2 v} *)
let int_gcd = fun (p:int) (q:int) ->
 Big_int.int_of_big_int  ( Big_int.gcd_big_int ( Big_int.big_int_of_int p ) ( Big_int.big_int_of_int q ) ) ;;

(** {v int_identity integer v} *)
let int_identity = function (x:int) -> x ;;

(** {v int_sqrt integer v} *)
let int_sqrt = function (x:int) ->
 small ( Big_int.sqrt_big_int ( big x ) ) ;;

(** {v int_power exponent integer v} *)
let rec int_power = fun (n:int) (x:int) ->
 match compare n 0 with
 | 0 -> 1
 | negative when negative < 0 -> failwith "Negative exponent in Util.int_power."
 | _ ->
  begin
   match n with
   | 1 -> x
   | 2 -> x * x
   | _ ->
    begin
     match n mod 2 with
     | 0 ->
      begin
       let y = int_power ( n / 2 ) x in
        y * y
      end
     | _ -> x * ( int_power ( pred n ) x )
    end
  end ;;
 
(** {v ilog integer v} *)
let ilog = function (x:int) ->
 let result = ref 0
 and y = ref x in
  while !y > 0 do
   incr result ;
   y := !y lsr 1 ;
  done ;
  !result ;;



(** {v float_zero float v} *)
let float_zero = function (x:float) -> 0. ;;

(** {v float_one float v} *)
let float_one = function (x:float) -> 1. ;;


(** {v ulp real v} Unit in the last place. This function is described by William Kahan in [http://www.cs.berkeley.edu/~wkahan/LOG10HAF.TXT].

Unité dans la position la moins significative. Cette fonction est décrite par William Kahan dans [http://www.cs.berkeley.edu/~wkahan/LOG10HAF.TXT]. *)
let ulp = function (x:float) ->
 let u = abs_float x in
  match u with
  | v when v < 5.0e-308 -> 5.0e-324
(** 1024. -. 53. = 971. *)
  | v when v > 9.0e307 -> 2.** 971.
  | _ ->
   begin
    let v = ( 0.7 *. epsilon_float ) *. u in
     min ( ( v +. u ) -. u ) ( ( v -. u ) +. u )
   end ;;

(** {v float_sign float v} *)
let float_sign = function (x:float) ->
 float ( compare x 0. ) ;;

(** {v float_identity float v} *)
let float_identity = function (x:float) -> x ;;

(** {v round real v} *)
let round = function (x:float) ->
 let n = int_of_float ( x +. x ) in
  n / 2 + n mod 2 ;;

(** {v frac real v} *)
let frac = function (x:float) ->
 fst ( modf x ) ;;

(** {v float_pos_part float v} *)
let float_pos_part = function (x:float) -> ( x +. ( abs_float x ) ) /. 2. ;;

(** {v int_float_power integer float v} *)
let rec int_float_power = fun (n:int) (x:float) ->
 match compare n 0 with
 | 0 -> 1.
 | negative when negative < 0 -> int_float_power ( abs n ) ( 1. /. x )
 | _ ->
  begin
   match n with
   | 1 -> x
   | 2 -> x *. x
   | _ ->
    begin
     let y = int_float_power ( n / 2 ) x in
      match n mod 2 with
      | 0 -> y *. y
      | _ -> x *. y *. y
    end
  end ;;
 

(** The factorial function is defined in a tail recursive way.

La factorielle est définie de manière récursive terminale. *)

(** {v fact_aux stack init v} *)
let rec fact_aux = fun (x:int) (y:int) -> match y with
 | 0 -> x
 | 1 -> x
 | _ -> fact_aux ( x * y ) ( pred y ) ;;

(** {v fact integer v} *)
let fact = function (x:int) -> fact_aux 1 ( abs x ) ;;


(** {v float_max real1 real2 v} *)
let float_max = fun (x:float) (y:float) -> if x > y then x else y ;;

(** {v float_min real1 real2 v} *)
let float_min = fun (x:float) (y:float) -> if y > x then x else y ;;


(** {v int_max int1 int2 v} *)
let int_max = fun (x:int) (y:int) -> if x > y then x else y ;;

(** {v int_min int1 int2 v} *)
let int_min = fun (x:int) (y:int) -> if y > x then x else y ;;




(** {C § } *)
(** 
{1 Constructions minimales sur les vecteurs réels}
{1 Minimal constructions on real vectors}
*)
(** {C  } *)




(** {v vector_zero dimension v} *)
let vector_zero = fun n (x:float) -> Array.make n 0. ;;

(** {v vector_one dimension float v} *)
let vector_one = fun n (x:float) -> Array.make n 1. ;;

(** {v vector_float_prod_3 vector1 vector2 v} *)
let vector_float_prod_3 = fun (v1:float array) (v2:float array) ->
 [| v1.(1) *. v2.(2) -. v1.(2) *. v2.(1) ;
    v1.(2) *. v2.(0) -. v1.(0) *. v2.(2) ;
    v1.(0) *. v2.(1) -. v1.(1) *. v2.(0) |] ;;




(** {C § } *)
(** 
{1 Constructions diverses}
{1 Miscellaneous constructions}
*)
(** {C  } *)




(** {v print_bool boolean v} *)
let print_bool = function (x:bool) ->
 print_string ( string_of_bool x ) ;;

(** {v append_char string character v} *)
let append_char = fun (s:string) (c:char) ->
 s ^ ( String.make 1 c ) ;;

(** {v string_eq string1 string2 v} *)
let string_eq = fun x y ->
 ( String.compare x y ) = 0 ;;

(** {v string_tail string v} *)
let string_tail = function (s:string) ->
 String.sub s 1 ( pred ( String.length s ) ) ;;

(** {v deverminage () v} L'erreur [Exception: Invalid_argument "index out of bounds".] arrive fréquemment
quand on fait de mauvais dimensionnenments de matrices ou de vecteurs. Placer cette instruction 
à différents endroits du code peut permettre de localiser le problème. *)
let deverminage = function () ->
 print_string "Jusqu'ici, tout va bien !" ;
 print_newline () ;;

(** {v degugging () v} The error [Exception: Invalid_argument "index out of bounds".]
is frequently seen when matrices or vectors are ill-dimensioned. Placing this instruction 
at different places in the code may help to situate the issue. *)
let debugging = function () ->
 print_string "So far so good !" ;
 print_newline () ;;




(** {C § } *)
(** 
{1 Constructions polymorphes sur les structures élémentaires d'Ocaml}
{1 Polymorphic constructions on elementary structures of Ocaml}
*)
(** {C  } *)




(** Polymorphic functions are reputed to be slow.

Les fonctions polymorphes sont réputés lentes.*)
(** {C § } *)


(** primo triple *)
let primo = function ( x , y , z ) -> x ;;

(** secundo triple *)
let secundo = function ( x , y , z ) -> y ;;

(** tertio triple *)
let tertio = function ( x , y , z ) -> z ;;


(** {v vector_print coeff_print vector v} *)
let vector_print = fun (coeff_print:'a -> unit) (v:'a array) ->
 let rr = Array.length v - 1 in
  print_string "[| " ;
  for i = 0 to ( rr - 1 ) do
   coeff_print v.(i) ; print_string " ; " 
  done ;
  coeff_print v.(rr) ;
  print_string " |]" ;
  print_newline () ;;

(** {v bare_vector_print coeff_print vector v} *)
let bare_vector_print = fun (coeff_print:'a -> unit) (v:'a array) ->
 let rr = Array.length v - 1 in
  print_string "[|" ;
  for i = 0 to ( rr - 1 ) do
   coeff_print v.(i) ; print_string " " 
  done ;
  coeff_print v.(rr) ;
  print_string "|]" ;;

(** {v vector_to_string coeff_to_string beginning separator ending vector v} *)
let vector_to_string = fun (coeff_to_string:'a -> string) (beginning:string) (separator:string) (ending:string) (v:'a array) ->
 try
  begin
   let rr = Array.length v - 1
   and s = ref beginning in
    for i = 0 to ( rr - 1 ) do
     s := !s ^ ( coeff_to_string v.(i) ) ^ separator
    done ;
    s := !s ^ ( coeff_to_string v.(rr) ) ^ ending ;
    !s
  end
 with _ ->
  beginning ^ ending ;;

(** {v bare_vector_to_string coeff_to_string vector v} *)
let bare_vector_to_string = fun (coeff_to_string:'a -> string) (v:'a array) ->
 vector_to_string coeff_to_string "[|" " " "|]" v ;;

(** {v vector_of_string coeff_of_string beginning separator ending string v} *)
let vector_of_string = fun (coeff_of_string:string -> 'a) (beginning:string) (separator:string) (ending:string) (s:string) ->
 let ls = String.length s
 and lb = String.length beginning
 and le = String.length ending in
  let st = String.sub s lb ( ls - lb - le ) in
   let listing = Str.split ( Str.regexp_string separator ) st in
    let a = Array.of_list listing in
     Array.map coeff_of_string a ;;

(** {v bare_vector_of_string coeff_of_string string v} *)
let bare_vector_of_string = fun (coeff_of_string:string -> 'a) (s:string) ->
 vector_of_string coeff_of_string "[|" " " "|]" s ;;


(** {v extract_even table v} *)
let extract_even = function (table:'a array) ->
 let r = ( Array.length table ) / 2 in
  let result = Array.make r table.(0) in
   for i = 0 to pred r do
    result.(i) <- table.( 2 * i )
   done ;
   result ;;

(** {v extract_odd table v} *)
let extract_odd = function (table:'a array) ->
 let r = ( 1 + ( Array.length table ) ) / 2 in
  let result = Array.make r table.(0) in
   for i = 0 to pred r do
    result.(i) <- table.( 2 * i + 1 )
   done ;
   result ;;

(** {v extract_every_other number table v} *)
let extract_every_other = fun (n:int) (table:'a array) ->
 let r = ( Array.length table ) / n in
  let result = Array.make r table.(0) in
   for i = 0 to pred r do
    result.(i) <- table.( n * i )
   done ;
   result ;;

(** {v array_combine array1 array2 v} *)
let array_combine = fun a b ->
 let r = Array.length a
 and s = Array.length b in
  if r != s then failwith "Different array lengths in Util.array_combine." ;
  if r = 0 then
   [| |]
  else
   begin
    let v = Array.make r ( a.(0) , b.(0) ) in
     for i = 1 to pred r do
      v.(i) <- ( a.(i) , b.(i) )
     done ;
     v
   end ;;

(** {v array_split array v} *)
let array_split = function a ->
 ( Array.map fst a , Array.map snd a ) ;;


(** {v list_non_empty list v} *)
let list_non_empty = function (l:'a list) ->
 match l with
 | [] -> false
 | _ -> true ;;

(** {v list_is_empty list v} *)
let list_is_empty = function (l:'a list) ->
 match l with
 | [] -> true
 | _ -> false ;;

(** {v array_is_empty array v} *)
let array_is_empty = function (a:'a array) ->
 Array.length a = 0 ;;


(** {v reverse_array array v} *)
let reverse_array = function a ->
 let r = Array.length a in
  let pr = pred r in
   Array.init r ( function i -> a.( pr - i ) ) ;;


(** {v lexico_compare comparison array1 array2 v} *)
let lexico_compare = fun (cmp:'a -> 'a -> int) (x:'a array) (y:'a array) ->
 let lx = Array.length x
 and ly = Array.length y
 and result = ref 0 in
  let ll = min lx ly
  and i = ref 0 in
   while ( !result = 0 ) && ( !i < ll ) do
    result := cmp x.(!i) y.(!i) ;
    incr i
   done ;
   if ( !result = 0 ) && ( lx < ly ) then
    result := -1 ;
   if ( !result = 0 ) && ( lx > ly ) then
    result := 1 ;
   !result ;;

(** {v array_eq coeff_equality array1 array2 v} *)
let array_eq = fun (eq:'a -> 'a -> bool) (v:'a array) (w:'a array) ->
 let f = fun i x -> eq v.(i) x in
  let test = Array.mapi f w in
   Array.fold_left ( && ) true test ;;

(** {v array_eq_zero coeff_nullity array v} *)
let array_eq_zero = fun (eq_zero:'a -> bool) (v:'a array) ->
 let test = Array.map eq_zero v in
  Array.fold_left ( && ) true test ;;


(** {v vector_max vector v} *)
let vector_max = function v -> Array.fold_left max v.(0) v ;;

(** {v vector_min vector v} *)
let vector_min = function v -> Array.fold_left min v.(0) v ;;


(** {v maximum comparison x y v} *)
let maximum = fun (cmp:'a -> 'a -> int) (x:'a) (y:'a) ->
 if cmp x y < 0 then
  y
 else
  x ;;

(** {v minimum comparison x y v} *)
let minimum = fun (cmp:'a -> 'a -> int) (x:'a) (y:'a) ->
 if cmp x y > 0 then
  y
 else
  x ;;


(** {v array_maximum comparison array v} *)
let array_maximum = fun (cmp:'a -> 'a -> int) (v:'a array) ->
 if Array.length v = 0 then failwith "Empty array in Util.array_maximum." ;
 let x = ref v.(0)
 and r = Array.length v in
  for i = 1 to pred r do
   let y = v.(i) in
    if cmp y !x > 0 then
     x := y
  done ;
  !x ;;

(** {v array_minimum comparison array v} *)
let array_minimum = fun (cmp:'a -> 'a -> int) (v:'a array) ->
 if Array.length v = 0 then failwith "Empty array in Util.array_minimum." ;
 let x = ref v.(0)
 and r = Array.length v in
  for i = 1 to pred r do
   let y = v.(i) in
    if cmp y !x < 0 then
     x := y
  done ;
  !x ;;

(** {v list_maximum comparison list v} *)
let list_maximum = fun (cmp:'a -> 'a -> int) (l:'a list) ->
 let rec aux = fun cmp accu m ->
  match m with
  | [] -> accu
  | x :: n -> aux cmp ( maximum cmp x accu ) n in
  match l with
  | [] -> failwith "Empty list in Util.list_maximum."
  | x :: m -> aux cmp x m ;;


(** {v list_minimum comparison list v} *)
let list_minimum = fun (cmp:'a -> 'a -> int) (l:'a list) ->
 let rec aux = fun cmp accu m ->
  match m with
  | [] -> accu
  | x :: n -> aux cmp ( minimum cmp x accu ) n in
  match l with
  | [] -> failwith "Empty list in Util.list_minimum."
  | x :: m -> aux cmp x m ;;


(** {v array_find_first predicate vector v} This function returns [-1] if it does not find:

Cette fonction retourne [-1] s'il ne trouve pas. *)
let array_find_first = fun (p:'a -> bool) (v:'a array) ->
 let r = Array.length v and index = ref (-1) and i = ref 0 in
  while  !i < r do
   if p v.(!i) then (index := !i ; i := r) else i := !i + 1 ; 
  done ;
  !index ;;


(** {v array_find_last predicate vector v} This function returns [-1] if it does not find:

Cette fonction retourne [-1] s'il ne trouve pas. *)
let array_find_last = fun (p:'a -> bool) (v:'a array) ->
 let r = Array.length v and index = ref (-1) in
  let i = ref ( pred r ) in
   while  !i >= 0 do
    if p v.(!i) then (index := !i ; i := -1) else i := !i - 1 ; 
   done ;
   !index ;;


(** {v vector_find_last equality element vector v} vector_find_last returns [-1] if it does not find:

vector_find_last retourne [-1] s'il ne trouve pas. *)
let vector_find_last = fun eq x v -> 
 let r = Array.length v and index = ref (-1) in
  let i = ref (r - 1) in
   while  !i >= 0 do
    if eq x v.(!i) then (index := !i ; i := -1) else i := !i - 1 ; 
   done ;
   !index ;;

(** {v vector_find_first equality element vector v} vector_find_first returns [-1] if it does not find:

vector_find_first retourne [-1] s'il ne trouve pas. *)
let vector_find_first = fun eq x v ->
 let r = Array.length v and index = ref (-1) and i = ref 0 in
  while  !i < r do
   if eq x v.(!i) then (index := !i ; i := r) else i := !i + 1 ; 
  done ;
 !index ;;

(** {v vector_find_twin equality element vector v} vector_find_first returns [-1] if it does not find:

vector_find_twin retourne [-1] s'il ne trouve pas. *)
let vector_find_twin = fun eq x v ->
 let r = Array.length v and index = ref (-1) and i = ref 0 in
  while  !i <= int_min (r - 1) ( int_of_float ( ceil ( (float r) /. 2. ) ) )  do
   if x = v.(!i) then (index := !i ; i := 1 + r)
   else
    begin
     let j = r - 1 - !i in
     if eq x v.(j) then (index := j ; i := 1 + r) else i := !i + 1 ; 
    end
  done ;
 !index ;;

(** {v vector_find_all equality element vector v} vector_find_all returns [[||]]  if it does not find:

vector_find_all retourne [[||]] s'il ne trouve pas.*)
let vector_find_all = fun eq x v -> 
 let r = Array.length v and index = ref [||] in
  for i = 0 to r - 1 do
   if eq x v.(i) then (index := Array.append !index [|i|] ; ())
  done ;
 !index ;;

(** {v vector_filter predicate vector v} *)
let vector_filter = fun (p:int -> bool) v ->
 let result = ref [] in
  for i = 0 to pred ( Array.length v ) do
   if p i then
    result := ( i , v.(i) ) :: !result
  done ;
  !result ;;


(** {v list_accumulate first_factor_comparison second_factor_addition combined_element ordered_combined_list v} The list [lis] is supposed
to be sorted according to [cmp] with respect to the first factors.

La liste [lis] est supposée triée selon [cmp] par rapport aux premiers facteurs. *)
let list_accumulate = fun (cmp:'a -> 'a -> int) (add:'b -> 'b -> 'b) (( i , x ) as y:'a * 'b) (lis:('a * 'b) list) ->
 let p = function z -> cmp i ( fst z ) = 0
 and comparison = fun z zz -> cmp ( fst z ) ( fst zz )
 and add_ad_hoc = fun a b -> ( fst a , add ( snd a ) ( snd b ) ) in
  let ( with_i , without_i ) = List.partition p lis in
   let z = List.fold_left add_ad_hoc y with_i in
    List.merge comparison [ z ] without_i ;;


(** The collision functions take two ordered families Y and Z into argument and provide four such ones.
The first value is Y \ Z and the second is Z \ Y and the third is Y inter Z as seen from Y, the last is Y inter Z as seen from Z.
The order is either globally reversed or globally respected. A total preorder is enough.
These functions are tail-recursive.

Les fonctions de collisions prennent deux familles ordonnées Y et Z en argument et en renvoient quatre.
La première valeur est Y \ Z, la deuxième est Z \ Y, la troisième est Y inter Z vu par Y et la dernière est Y inter Z vu par Z.
L'ordre est soit globalement renversé soit globalement respecté. Un préordre total suffit.
Ces fonctions sont récursives terminales. *)

(** {v list_collision_aux comparison accumulator list1 list2 v} *)
let rec list_collision_aux = fun (cmp:'a -> 'a -> int) (accu:'a list array) (y:'a list) (z:'a list) ->
 if ( list_is_empty y ) || ( list_is_empty z ) then [| List.append y accu.(0) ; List.rev_append z accu.(1) ; accu.(2) ; accu.(3) |]
 else
  begin
   let xy = List.hd y
   and xz = List.hd z in
    match cmp xy xz with
    | 0 -> list_collision_aux cmp [| accu.(0) ; accu.(1) ; xy :: accu.(2) ; xz :: accu.(3)|] ( List.tl y ) ( List.tl z )
    | 1 -> list_collision_aux cmp [| accu.(0) ; xz :: accu.(1) ; accu.(2) ; accu.(3) |] y ( List.tl z )
    | _ -> list_collision_aux cmp [| xy :: accu.(0) ; accu.(1) ; accu.(2) ; accu.(3) |] ( List.tl y ) z
  end ;;

(** {v reverse_list_collision comparison list1 list2 v} *)
let reverse_list_collision = fun (cmp:'a -> 'a -> int) (y:'a list) (z:'a list) ->
 let c = fun x y -> cmp y x in
  let f = function x -> List.fast_sort c x in
   Array.map f ( list_collision_aux cmp ( Array.make 4 [] ) y z ) ;;

(** {v list_collision comparison list1 list2 v} *)
let list_collision = fun (cmp:'a -> 'a -> int) (y:'a list) (z:'a list) ->
 Array.map List.rev ( reverse_list_collision cmp y z ) ;;

(** {v reverse_array_collision comparison array1 array2 v} *)
let reverse_array_collision = fun (cmp:'a -> 'a -> int) (y:'a array) (z:'a array) ->
 Array.map Array.of_list ( reverse_list_collision cmp ( Array.to_list y ) ( Array.to_list z ) ) ;;

(** {v array_collision comparison array1 array2 v} *)
let array_collision = fun (cmp:'a -> 'a -> int) (y:'a array) (z:'a array) ->
 Array.map ( function x -> Array.of_list ( List.rev x ) ) ( reverse_list_collision cmp ( Array.to_list y ) ( Array.to_list z ) ) ;;


(** {v transpose matrix v} *)
let transpose = function m ->
 let r = Array.length m
 and c = Array.length m.(0) in
  if ( r = 0 ) || ( c = 0 ) then [| [| |] |]
  else
   begin
    let mm = Array.make_matrix c r m.(0).(0)
    and rr = pred r in
     for i = 0 to pred c do
      let row_output = mm.(i) in
       for j = 0 to rr do
        row_output.(j) <- m.(j).(i)
       done ;
     done ;
     mm
   end ;;


(** {v array_first v} *)
let array_first = function a ->
 a.(0) ;;

(** {v array_last array v} *)
let array_last = function a ->
 if Array.length a = 0 then failwith "Empty array in Util.array_last." ;
 a.( pred ( Array.length a ) ) ;;

(** {v array_tail array v} *)
let array_tail = function (a:'a array) ->
 Array.sub a 1 ( pred ( Array.length a ) ) ;;

(** {v array_end index array v} *)
let array_end = fun (i:int) (a:'a array) ->
 try
  let r = ( Array.length a ) - i in
   Array.sub a i r
 with _ -> [| |] ;;

(** {v array_forget index array v} *)
let array_forget = fun (k:int) (a:'a array) ->
 if ( k < 0 ) || ( k >= Array.length a ) then
  failwith "Bad index in Util.array_forget." ;
 let b = Array.sub a 0 k
 and c = array_end ( succ k ) a in
  Array.append b c ;;

(** {v array_insert index value array v} *)
let array_insert = fun (k:int) (x:'a) (a:'a array) ->
 let b = Array.sub a 0 k
 and c = array_end k a in
   Array.concat [ b ; [| x |] ; c ] ;;


(** {v array_cut table array v} *)
let array_cut = fun (table:int array) (a:'a array) ->
 let rr = Array.fold_left ( + ) 0 table in
  if Array.length a < rr then failwith "Array of bad length in Util.array_cut." ;
  let r = Array.length table
  and accu = ref 0 in
   let b = Array.make_matrix r 0 a.(0) in
    for i = 0 to pred r do
     let shift = table.(i) in
      b.(i) <- Array.sub a !accu shift ;
      accu := !accu + shift ;
    done ;
    b ;;


(** {v array_guarded_cut thickness table array v} *)
let array_guarded_cut = fun (e:int) (table:int array) (a:'a array) ->
 let r = Array.length table
 and i = ref 0
 and accu = ref 0 in
  if e = 0 then Array.map ( Array.make 1 ) a
  else
   begin
    let b = Array.make_matrix r 0 a.(0) in
     while !i < r do
      let shift = table.(!i) in
       begin
        try
         b.(!i) <- Array.sub a !accu shift
        with _ ->
         begin
          try
           b.(!i) <- array_end !accu a
          with _ -> ()
         end
       end ;
       accu := !accu + shift ;
      incr i ;
     done ;
     b
   end ;;

(** {v array_cut table array v} *)
let array_cut = fun (table:int array) (a:'a array) ->
 let rr = Array.fold_left ( + ) 0 table in
  if Array.length a < rr then failwith "Array of bad length in Util.array_cut." ;
  let r = Array.length table
  and accu = ref 0 in
   let b = Array.make_matrix r 0 a.(0) in
    for i = 0 to pred r do
     let shift = table.(i) in
      b.(i) <- Array.sub a !accu shift ;
      accu := !accu + shift ;
    done ;
    b ;;

(** {v array_over_cut table array v} *)
let array_over_cut = fun (table:int array) (a:'a array) ->
 let t = Array.map succ table in
  array_cut t a ;;


(** {v array_guarded_over_cut thickness table array v} *)
let array_guarded_over_cut = fun (e:int) (table:int array) (a:'a array) ->
 let t = Array.map succ table in
  array_guarded_cut e t a ;;


(** {v array_map2 function array1 array2 v} *)
let array_map2 = fun f (a:'a array) (b:'b array) ->
 let r = Array.length a in
  assert ( r = Array.length b ) ;
  let c = Array.make r ( f a.(0) b.(0) ) in
   for i = 1 to pred r do
    c.(i) <- f a.(i) b.(i)
   done ;
   c ;;


(** {v array_center_add copy addition array1 array2 v} The lengths of the arrays are supposed to be odd.
The coefficients are added whith alignment of the center of the arrays.

Les tailles des tableaux sont supposées impaires. Les coefficients sont additionnés en alignant les centres des tableaux.*)
let array_center_add = fun copy add (a:'a array) (b:'a array) ->
 let alength = Array.length a
 and blength = Array.length b in
  let clength = max alength blength
  and ( short , long ) = if alength < blength then ( a , b ) else ( b , a )
  and shift = ( abs ( blength - alength ) ) / 2 in
   let c = Array.make clength a.(0)
   and p_c_l = pred clength in
    for i = 0 to pred shift do
     c.(i) <- copy long.(i)
    done ;
    for i = shift to p_c_l - shift do
     c.(i) <- add long.(i) short.( i - shift )
    done ;
    for i = clength - shift to p_c_l do
     c.(i) <- copy long.(i)
    done ;
    c ;;


(** {v vector_spray integer vector v} *)
let vector_spray = fun (n:int) a ->
 let l = Array.length a in
  if ( l mod n != 0 ) || ( l < n ) then failwith "Problem of length in Readwrite.vector_spray." ;
  let ll = l / n in
   let lll = pred ll
   and m = Array.make_matrix n ll a.(0) in
    for i = 0 to pred n do
     let row_output = m.(i) in
      for j = 0 to lll do
       row_output.(j) <- a.( i + j * n )
      done 
    done ;
    m ;;

(** {v vector_interlace vector_array v} *)
let vector_interlace = function a ->
 let n = Array.length a
 and l = Array.length a.(0) in
  let ll = n * l in
   let v = Array.make ll a.(0).(0) in
    for i = 0 to pred ll do
     v.(i) <- a.( i mod n ).( i / n )
    done ;
    v ;;

(** {v vector_to_matrix rows columns vector v} *)
let vector_to_matrix = fun (r:int) (c:int) a ->
 let l = Array.length a in
  if l != r * c then failwith "Bad dimensions in Readwrite.vector_to_matrix." ; 
  let m = Array.make_matrix r c a.(0)
  and cc = pred c in
   for i = 0 to pred r do
    let row_output = m.(i)
    and ii = c * i in
     for j = 0 to cc do
      row_output.(j) <- a.( ii + j )
     done
   done ;
   m ;;











(** {C § } *)
(** 
{1 Numération}
*)
(** {C  } *)




(** {v bits_of_byte int_byte v} *)
let bits_of_byte = function (x:int) ->
 let f = fun x y -> ( x land y ) <> 0 in
  Array.map ( f x ) [| 128 ; 64 ; 32 ; 16 ; 8 ; 4 ; 2 ; 1 |] ;;

(** {v int_bits_of_byte int_byte v} *)
let int_bits_of_byte = function (x:int) ->
 Array.map ( function x -> if x then 1 else 0 ) ( bits_of_byte x ) ;;

(** {v reverse_int_sequence integer v} *)
let reverse_int_sequence = function (n:int) ->
 let a = Array.make ( succ n ) 0 in
  for i = 0 to pred n do
   a.(i) <- n - i
  done ;
  a ;;

(** {v standard_reverse_int_sequence v} *)
let standard_reverse_int_sequence = reverse_int_sequence 62 ;;

(** {v standard_reverse_binary_powers_sequence v} *)
let standard_reverse_binary_powers_sequence = Array.map ( function x -> int_power x 2 ) standard_reverse_int_sequence ;;

(** {v int_sequence integer v} *)
let int_sequence = function (n:int) ->
 let a = Array.make ( succ n ) 0 in
  for i = 1 to n do
   a.(i) <- i
  done ;
  a ;;

(** {v standard_int_sequence v} *)
let standard_int_sequence = int_sequence 62 ;;

(** {v binary_powers_sequence integer v} *)
let binary_powers_sequence = function (n:int) ->
 Array.map ( function x -> int_power x 2 ) ( int_sequence n ) ;;

(** {v standard_binary_powers_sequence v} *)
let standard_binary_powers_sequence = Array.map ( function x -> int_power x 2 ) standard_int_sequence ;;

(** {v bits_of_int integer v} *)
let bits_of_int = function (x:int) ->
 let f = fun x y -> ( x land y ) <> 0 in
  Array.map ( f x ) standard_reverse_binary_powers_sequence ;;


(** {v next_binary_exponent integer v} This functions shifts from one unit if the integer is a power of two.

Cette fonction décale d'un cran si l'entier est une puissance de deux. *)
let next_binary_exponent = function (x:int) ->
 let b = bits_of_int ( abs x ) in
  let n = Array.length b
  and result = ref 0 in
   let i = ref 0 in
    while !i < n do
     if b.(!i) <> false then
      begin
       result := n - !i ;
       i := n ;
      end
     else
      begin
       incr i ;
      end ;
    done ;
    !result ;;

(** {v truncated_bits_of_int exponent integer v} *)
let truncated_bits_of_int = fun (n:int) (x:int) ->
 let f = fun x y -> ( x land y ) <> 0
 and sequence = array_end ( 63 - n ) standard_reverse_binary_powers_sequence in
  Array.map ( f x ) sequence ;;

(** {v int_bits_of_int integer v} *)
let int_bits_of_int = function (x:int) ->
 Array.map ( function x -> if x then 1 else 0 ) ( bits_of_int x ) ;;

(** {v truncated_int_bits_of_int exponent integer v} *)
let truncated_int_bits_of_int = fun (n:int) (x:int) ->
 Array.map ( function x -> if x then 1 else 0 ) ( truncated_bits_of_int n x ) ;;

(** {v digits_of_int radix integer v} *)
let digits_of_int = fun (p:int) (x:int) ->
 assert ( x > 0 ) ;
 let y = ref x
 and accu = ref [] in
  while !y > 0 do
   let remain = !y mod p in
    y := ( !y - remain ) / p ;
    accu := remain :: !accu ;
  done ;
  Array.of_list !accu ;;

(** {v truncated_digits_of_int radix exponent integer v} *)
let truncated_digits_of_int = fun (p:int) (n:int) (x:int) ->
 assert ( x >= 0 ) ;
 let y = ref x
 and i = ref ( pred n )
 and accu = Array.make n 0 in
  begin
   try
    while !y > 0 do
     let remain = !y mod p in
      y := ( !y - remain ) / p ;
      accu.(!i) <- remain ;
      decr i ;
    done
   with _ ->
    ()
  end ;
  accu ;;


(** {v naive_reverse_int_of_bits bool_bits v} *)
let naive_reverse_int_of_bits = function (a:bool array) ->
 let b = Array.mapi ( fun i x -> if x then int_power i 2 else 0 ) a in
  Array.fold_left ( + ) 0 b ;;

(** {v reverse_int_of_bits bool_bits v} *)
let reverse_int_of_bits = function (a:bool array) ->
 naive_reverse_int_of_bits a ;;


(** {v naive_int_of_bits bool_bits v} *)
let naive_int_of_bits = function (a:bool array) ->
 let b = reverse_array a in
  reverse_int_of_bits b ;;

(** {v other_int_of_bits bool_bits v} *)
let other_int_of_bits = function (a:bool array) ->
 let accu = ref 0
 and n = pred ( Array.length a ) in
  let sequence = array_end ( 62 - n ) standard_reverse_binary_powers_sequence in
   for i = 0 to n do
    if a.(i) then
     accu := !accu + sequence.(i) ;
   done ;
   !accu ;;

(** {v int_of_bits bool_bits v} *)
let int_of_bits = function (a:bool array) ->
 let accu = ref 0
 and n = pred ( Array.length a ) in
  for i = 0 to pred n do
   if a.(i) then
    incr accu ;
   accu := 2 * !accu ;
  done ;
  if a.(n) then
   incr accu ;
  !accu ;;


(** {v int_of_digits radix integer v} *)
let int_of_digits = fun (p:int) (a:int array) ->
 let n = pred ( Array.length a )
 and accu = ref 0 in
  for i = 0 to pred n do
   let digit = a.(i) in
    if digit > 0 then
     accu := !accu + digit ;
    accu := p * !accu ;
  done ;
  let digit = a.(n) in
   if digit > 0 then
    accu := !accu + digit ;
   !accu ;;


(** {v bit_reversal integer v} *)
let bit_reversal = function (x:int) ->
 let a = bits_of_int x in
  let b = reverse_array a in 
   int_of_bits b ;;

(** {v truncated_bit_reversal exponent integer v} *)
let truncated_bit_reversal = fun (n:int) (x:int) ->
 let a = truncated_bits_of_int n x in
  let b = reverse_array a in
   int_of_bits b ;;

(** {v digit_reversal radix integer v} *)
let digit_reversal = fun (p:int) (x:int) ->
 let a = digits_of_int p x in
  let b = reverse_array a in 
   int_of_digits p b ;;

(** {v truncated_digit_reversal radix exponent integer v} *)
let truncated_digit_reversal = fun (p:int) (n:int) (x:int) ->
 let a = truncated_digits_of_int p n x in
  let b = reverse_array a in 
   int_of_digits p b ;;












(** {C § § § } *)

end ;;


