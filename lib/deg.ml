




module Deg = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module utility functions and types to treat big degrees and valuations
of polynomials and polynomial fractions. The infinite values are provided by the real numbers.



{2 Conventions}



When of type [float], the absolute value of degrees must not be greater than or equal to a value
which is the minimum of the inverse of [epsilon_float] and of [max_int].


The addition and substraction and multiplication pass quickly enough into big integers in order to avoid casual overflows.


This module is distributed under the same licence as Ocaml.



{C § }



La mathématicienne ou le mathématicien trouvera dans ce module des fonctions utilitaires et types pour traiter
les grands degrés et valuations des polynômes et fractions ratonnelles. Les infinis sont fournis par les réels.



{2 Conventions}



Pour les degrés de type [float], la valeur absolue ne doit pas atteindre ou dépasser une valeur
qui est le minimum de l'inverse de [epsilon_float] et de [max_int].


Les addition, soustraction et multiplication passent assez rapidement dans les grands entiers pour éviter d'éventuels dépassements de capacité.


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
@since février 2012
*)



(** {v bad_float real v} *)
let bad_float = function (x:float) ->
 let xx = abs_float x
 and result = ref false in
  if ( xx >= 1. /. epsilon_float ) || ( int_of_float xx < 0 ) || ( int_of_float xx >= max_int ) || ( xx >= float max_int ) then
   result := true ;
  if ( Pervasives.compare x infinity = 0 ) || ( Pervasives.compare x neg_infinity = 0 ) then
   result := false ;
  !result ;;


(** The type collects integer and big integer and real values.
The real numbers provide the infinities.

Le type collationne les valeurs entières, grandes entières, réelles.
Les réels fournissent les infinis. *)
type t = 
 | Witness
 | Float of float
 | Int of int
 | Big of Big_int.big_int ;;

let zero = Int 0 ;;

let witness = Witness ;;

(** {v float_demakeup degree v} *)
let float_demakeup = function (x:t) ->
 match x with
 | Float u ->
  begin
   if bad_float u then failwith "Bad real in Deg.float_demakeup." ;
   u
  end
 | _ -> failwith "Not a Float in Deg.float_demakeup." ;;

(** {v int_demakeup degree v} *)
let int_demakeup = function (x:t) ->
 match x with
 | Int u -> u
 | _ -> failwith "Not an Int in Deg.int_demakeup." ;;

(** {v big_demakeup degree v} *)
let big_demakeup = function (x:t) ->
 match x with
 | Big u -> u
 | _ -> failwith "Not a Big in Deg.float_demakeup." ;;

(** {v print degree v} *)
let print = function (x:t) ->
 match x with
 | Witness -> print_string "Deg.Witness"
 | Float u -> print_float u
 | Int u -> print_int u
 | Big u -> print_string ( Big_int.string_of_big_int u ) ;;

(** {v eq_zero degree v} *)
let eq_zero = function (x:t) ->
 match x with
 | Witness -> false
 | Float u -> ( Pervasives.compare u 0. ) = 0
 | Int u -> ( Pervasives.compare u 0 ) = 0
 | Big u -> ( Big_int.compare_big_int u Big_int.zero_big_int ) = 0 ;;

(** {v copy degree v} *)
let copy = function (x:t) ->
 match x with
 | Witness -> Witness
 | Float u -> Float ( u +. 0. )
 | Int u -> Int u
 | Big u -> Big ( Big_int.add_big_int u Big_int.zero_big_int ) ;;

(** {v opp degree v} *)
let opp = function (x:t) ->
 match x with
 | Witness -> Witness
 | Float u ->
  begin
   if bad_float u then failwith "Bad real in Deg.opp." ;
   Float ( -. u )
  end
 | Int u -> Int ( - u )
 | Big u -> Big ( Big_int.minus_big_int u ) ;;

(** {v to_int degree v} *)
let to_int = function (x:t) ->
 match x with
 | Witness -> failwith "Witness in Deg.to_int."
 | Float u -> int_of_float u
 | Int u -> u
 | Big u -> Big_int.int_of_big_int u ;;

(** {v from_int integer v} *)
let from_int = function (u:int) ->
 Int u ;;

(** {v pred degree v} *)
let pred = function (x:t) ->
 match x with
 | Witness -> Witness
 | Float u -> Float ( u -. 1. )
 | Int u -> Int ( pred u )
 | Big u -> Big ( Big_int.pred_big_int u ) ;;

(** {v succ degree v} *)
let succ = function (x:t) ->
 match x with
 | Witness -> Witness
 | Float u -> Float ( u +. 1. )
 | Int u -> Int ( succ u )
 | Big u -> Big ( Big_int.succ_big_int u ) ;;

(** {v maximum degree1 degree2 v} *)
let rec maximum = fun (x:t) (y:t) ->
 match x with
 | Witness -> y
 | Float u ->
  begin
   if bad_float u then failwith "Bad real in Deg.maximum." ;
   match ( Pervasives.compare u infinity , Pervasives.compare u neg_infinity ) with
   | ( 0 , _ ) -> x
   | ( _ , 0 )  -> y
   | _ ->
    begin
     match y with
     | Witness -> x
     | Float v ->
      begin
       if bad_float v then failwith "Bad real in Deg.maximum." ;
       Float ( max u v )
      end
     | _ -> maximum y ( Int ( int_of_float u ) )
    end
  end
 | Int u ->
  begin
   match y with
   | Witness -> x
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.maximum." ;
     match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
     | ( 0 , _ ) -> y
     | ( _ , 0 )  -> x
     | _ -> Int ( max u ( int_of_float v ) )
    end
   | Int v -> Int ( max u v )
   | Big v -> Big ( Big_int.max_big_int v ( Big_int.big_int_of_int u ) )
  end
 | Big u ->
  begin
   match y with
   | Witness -> x
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.maximum." ;
     match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
     | ( 0 , _ ) -> y
     | ( _ , 0 )  -> x
     | _ -> maximum x ( Int ( int_of_float v ) )
    end
   | Int v -> maximum y x
   | Big v -> Big ( Big_int.max_big_int u v )
  end ;;

(** {v minimum degree1 degree2 v} *)
let minimum = fun (x:t) (y:t) ->
 opp ( maximum ( opp x ) ( opp y ) ) ;;

(** {v add degree1 degree2 v} *)
let rec add = fun (x:t) (y:t) ->
 match x with
 | Witness -> failwith "Witness adding in Deg.add."
 | Float u ->
  begin
   if bad_float u then failwith "Bad real in Deg.add." ;
   match ( Pervasives.compare u infinity , Pervasives.compare u neg_infinity ) with
   | ( 0 , _ ) ->
    begin
     match y with
     | Witness -> failwith "Witness adding in Deg.add."
     | Float v ->
      begin
       if bad_float v then failwith "Bad real in Deg.add." ;
       if Pervasives.compare v neg_infinity = 0 then failwith "Undetermined form in Deg.add."
       else x
      end
     | _ -> x
    end
   | ( _ , 0 )  ->
    begin
     match y with
     | Witness -> failwith "Witness adding in Deg.add."
     | Float v ->
      begin
       if bad_float v then failwith "Bad real in Deg.add." ;
       if Pervasives.compare v infinity = 0 then failwith "Undetermined form in Deg.add."
       else x
      end
     | _ -> x
    end
   | _ ->
    begin
     match y with
     | Witness -> failwith "Witness adding in Deg.add."
     | Float v ->
      begin
       if bad_float v then failwith "Bad real in Deg.add." ;
       Int ( ( int_of_float u ) + ( int_of_float v ) )
      end
     | _ -> add y ( Int ( int_of_float u ) )
    end
  end
 | Int u ->
  begin
   match y with
   | Witness -> failwith "Witness adding in Deg.add."
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.add." ;
     match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
     | ( 0 , _ ) -> y
     | ( _ , 0 )  -> y
     | _ -> add x ( Int ( int_of_float v ) )
    end
   | Int v -> Big ( Big_int.add_big_int ( Big_int.big_int_of_int u ) ( Big_int.big_int_of_int v ) )
   | Big v -> Big ( Big_int.add_big_int v ( Big_int.big_int_of_int u ) )
  end
 | Big u ->
  begin
   match y with
   | Witness -> failwith "Witness adding in Deg.add."
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.add." ;
     match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
     | ( 0 , _ ) -> y
     | ( _ , 0 )  -> y
     | _ -> add x ( Int ( int_of_float v ) )
    end
   | Int v -> add y x
   | Big v -> Big ( Big_int.add_big_int u v )
  end ;;

(** {v sub degree1 degree2 v} *)
let sub = fun (x:t) (y:t) ->
 match x with
 | Witness -> failwith "Witness substracting in Deg.sub."
 | Float u ->
  begin
   if bad_float u then failwith "Bad real in Deg.sub." ;
   match y with
   | Witness -> failwith "Witness substracting in Deg.sub."
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.sub." ;
     add x ( opp y )
    end
   | _ -> add x ( opp y )
  end
 | _ ->
  begin
   match y with
   | Witness -> failwith "Witness substracting in Deg.sub."
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.sub." ;
     add x ( opp y )
    end
   | _ -> add x ( opp y )
  end ;;

(** {v shift integer degree v} *)
let shift = fun (i:int) (x:t) ->
 add ( Int i ) x ;;

(** {v mult degree1 degree2 v} *)
let rec mult = fun (x:t) (y:t) ->
 match x with
 | Witness -> failwith "Witness multiplying in Deg.sub."
 | Float u ->
  begin
   if bad_float u then failwith "Bad real in Deg.mult." ;
   match ( Pervasives.compare u infinity , Pervasives.compare u neg_infinity ) with
   | ( 0 , _ ) ->
    begin
     match y with
     | Witness -> failwith "Witness multiplying in Deg.sub."
     | Float v ->
      begin
       if bad_float v then failwith "Bad real in Deg.mult." ;
       if v = 0. then failwith "Undetermined form in Deg.mult." ;
       match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
       | ( 0 , _ ) -> x
       | ( _ , 0 )  -> y
       | _ ->
        begin
         if v > 0. then x
         else opp x
        end
      end
     | Int v ->
      begin
       match Pervasives.compare v 0 with
       | 0 -> failwith "Undetermined form in Deg.mult." ;
       | 1 -> x
       | _ -> opp x
      end
     | Big v ->
      begin
       match Pervasives.compare v Big_int.zero_big_int with
       | 0 -> failwith "Undetermined form in Deg.mult." ;
       | 1 -> x
       | _ -> opp x
      end
    end
   | ( _ , 0 )  -> opp ( mult ( opp x ) y )
   | _ -> mult ( Big ( Big_int.big_int_of_int ( int_of_float u ) ) ) y
  end
 | Int u ->
  begin
   match y with
   | Witness -> failwith "Witness multiplying in Deg.sub."
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.mult." ;
     match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
     | ( 0 , _ ) ->
      begin
       match Pervasives.compare u 0 with
       | 0 -> failwith "Undetermined form in Deg.mult." ;
       | 1 -> y
       | _ -> opp y
      end
     | ( _ , 0 )  -> opp ( mult x ( opp y ) )
     | _ -> Big ( Big_int.mult_big_int ( Big_int.big_int_of_int u ) ( Big_int.big_int_of_int ( int_of_float v ) ) )
    end
   | Int v -> Big ( Big_int.mult_big_int ( Big_int.big_int_of_int u ) ( Big_int.big_int_of_int v ) )
   | Big v -> Big ( Big_int.mult_big_int v ( Big_int.big_int_of_int u ) )
  end
 | Big u ->
  begin
   match y with
   | Witness -> failwith "Witness multiplying in Deg.sub."
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.mult." ;
     match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
     | ( 0 , _ ) ->
      begin
       match Pervasives.compare u Big_int.zero_big_int with
       | 0 -> failwith "Undetermined form in Deg.mult." ;
       | 1 -> y
       | _ -> opp y
      end
     | ( _ , 0 )  -> opp ( mult x ( opp y ) )
     | _ -> Big ( Big_int.mult_big_int u ( Big_int.big_int_of_int ( int_of_float v ) ) )
    end
   | Int v -> mult y x
   | Big v -> Big ( Big_int.mult_big_int u v )
  end ;;


(** {v compare degree1 degree2 v} *)
let rec compare = fun (x:t) (y:t) ->
 match x with
 | Witness ->
  begin
   match y with
   | Witness -> 0
   | _ -> - 1
  end
 | Float u ->
  begin
   if bad_float u then failwith "Bad real in Deg.compare." ;
   match ( Pervasives.compare u infinity , Pervasives.compare u neg_infinity ) with
   | ( 0 , _ ) ->
    begin
     try ( let v = float_demakeup y in ignore ( bad_float v ) ; Pervasives.compare u v )
     with Failure "Not a Float in Deg.float_demakeup." -> 1
    end
   | ( _ , 0 )  ->
    begin
     try ( let v = float_demakeup y in ignore ( bad_float v ) ; Pervasives.compare u v )
     with Failure "Not a Float in Deg.float_demakeup." -> -1
    end
   | _ ->
    begin
     match y with
     | Witness -> 1
     | Float v ->
      begin
       if bad_float v then failwith "Bad real in Deg.compare." ;
       Pervasives.compare u v
      end
     | _ -> compare ( Int ( int_of_float u ) ) y
    end
  end
 | Int u ->
  begin
   match y with
   | Witness -> 1
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.compare." ;
     match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
     | ( 0 , _ ) -> -1
     | ( _ , 0 )  -> 1
     | _ -> Pervasives.compare u ( int_of_float v )
    end
   | Int v -> Pervasives.compare u v
   | Big v -> Big_int.compare_big_int ( Big_int.big_int_of_int u ) v
  end
 | Big u ->
  begin
   match y with
   | Witness -> 1
   | Float v ->
    begin
     if bad_float v then failwith "Bad real in Deg.compare." ;
     match ( Pervasives.compare v infinity , Pervasives.compare v neg_infinity ) with
     | ( 0 , _ ) -> -1
     | ( _ , 0 )  -> 1
     | _ -> - ( compare ( Int ( int_of_float v ) ) x )
    end
   | Int v -> - ( compare y x )
   | Big v -> Big_int.compare_big_int u v
  end ;;





(** {C § § § } *)




end







