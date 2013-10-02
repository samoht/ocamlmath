




module Draw = struct


(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module methods in order to draw:

- function graphs,

- curves and surfaces,

- vector fields,

- frame fields

in dimension two and three.


{2 Comments}


The book [ocaml-ora-book.pdf] available on th web site of Ocaml yields background constructions.

The site [http://www.antigrain.com] gives some explanations about the oversampling method for anti-aliasing.

This module is distributed under the same licence as Ocaml.



{C § }



La mathématicienne ou le mathématicien trouvera dans ce module des méthodes pour dessiner :

- graphes de fonctions,

- courbes et surfaces,

- champs de vecteurs,

- champs de repères

en dimensions deux et trois.


{2 Commentaires}


Le livre [ocaml-ora-book.pdf] disponible sur le site web d'Ocaml fournit des constructions de base.

Le site [http://www.antigrain.com] donne des explications sur le procédé de suréchantillonnnage pour l'antialiassage.

Ce module est distribué selon la même licence qu'Ocaml.

{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.4}
*)
(**
@version 0.4
*)
(**
@author Stéphane Grognet
*)
(**
@since 2011, 2012, 2013
*)



(** {C § } *)
(** 
{1 Outils}
*)
(** {C  } *)




open Util ;;
open Matrix ;;
open Infinitesimal ;;
open Readwrite ;;
open Data ;;
Graphics.open_graph "" ;;




(** 
{2 Constantes}
{2 Constants}
*)
(** {C  } *)





(** {v (text_size_h,text_size_v) v} Size of the characters.

Taille des caractères. *)
let (text_size_h,text_size_v) = Graphics.text_size "W" ;;


(** {v tsh v} Width of the characters.

Largeur des caractères. *)
let tsh = text_size_h ;;


(** {v tsv v} Height of the characters.

Hauteur des caractères.  *)
let tsv = text_size_v ;;


(** {v color_list v} List of the pre-defined colors.

Liste des couleurs prédéfinies. *)
let color_list = [| Graphics.green ; Graphics.magenta ; Graphics.cyan ; Graphics.blue ; Graphics.red ; Graphics.yellow ; Graphics.white ; Graphics.black |] ;;




(** 
{2 Fonctions}
{2 Functions}
*)
(** {C  } *)




(** {v color_to_rgb color v} color == R * 256 * 256 + G * 256 + B *)
let color_to_rgb = function (c:Graphics.color) ->
 let r = c / 65536 and g = c / 256 mod 256 and b = c mod 256 in
  [| r ; g ; b |] ;;

(** {v inv_color color v} *)
let inv_color = function (c:Graphics.color) ->
 let z = color_to_rgb c in
  Graphics.rgb ( 255 - z.(0) ) ( 255 - z.(1) ) ( 255 - z.(2) ) ;;

(** {v mix_color coefficient color1 color2 v} *)
let mix_color = fun (x:float) (c1:Graphics.color) (c2:Graphics.color) ->
 let z = color_to_rgb c1
 and zz = color_to_rgb c2 in
  let zzz = Matrix.vector_float_scal_mult ( 1. -. x ) ( Matrix.float_of_vector z )
  and zzzz = Matrix.vector_float_scal_mult x ( Matrix.float_of_vector zz ) in
   let z_z = Matrix.int_of_vector ( Matrix.vector_float_plus zzz zzzz ) in
    Graphics.rgb z_z.(0) z_z.(1) z_z.(2) ;;

(** {v bias_color color_bias color v} *)
let bias_color = fun (c1:Graphics.color) (c2:Graphics.color) ->
 let z = color_to_rgb c1
 and zz = color_to_rgb c2 in
  Graphics.rgb ( ( z.(0) + 11 * zz.(0) ) / 12 ) ( ( z.(1) + 11 * zz.(1) ) / 12 ) ( ( z.(2) + 11 * z.(2) ) / 12 ) ;;

(** {v soft_color color v} *)
let soft_color = function (c:Graphics.color) ->
 let z = color_to_rgb c in
  Graphics.rgb ( 6 * z.(0) /10 ) ( 6 * z.(1) / 10 ) ( 6 * z.(2) / 10 ) ;;

(** {v inv_color_vector v} *)
let inv_color_vector = Array.map inv_color ;;

(** {v inv_color_matrix v} *)
let inv_color_matrix = Array.map inv_color_vector ;;

(** {v arrow paint x y u v v} An arrow is formed of four segments. The function [paint] is supposed to draw a segment.

Une flèche est constituée de quatre segments. La fonction [paint] est censée dessiner un segment. *)
let arrow = fun paint (x:int) (y:int) (u:int) (v:int) ->
 let xx = x + u
 and yy = y + v
 and su = Infinitesimal.int_sign u
 and sv = Infinitesimal.int_sign v in
  let uu = - ( sv + v / 20 )
  and vv = su + u / 20
  and uuu = su + u / 10
  and vvv = sv + v / 10 in
   let x_x = x + uu
   and y_y = y + vv
   and x_xx = x - uu
   and y_yy = y - vv
   and xxx = xx + uu - uuu
   and yyy = yy + vv  - vvv
   and xxxx = xx - uu - uuu
   and yyyy = yy - vv - vvv in
    let a = [| (x,y,xx,yy) ; (x_x,y_y,x_xx,y_yy) ; (xx,yy,xxx,yyy) ; (xx,yy,xxxx,yyyy) |] in
     for i = 0 to 3 do
      let ( old_x , old_y , new_x , new_y ) = a.(i) in
       paint old_x old_y new_x new_y ;
     done ;;

(** {v box3 frame_coordinates v} *)
let box_3 = function (coordinates:float array) ->
 let c0 = [| [| coordinates.(0) ; 0. ; 0. |] ; [| coordinates.(1) ; 0. ; 0. |] ; [| 0. ; coordinates.(2) ; 0. |] ;
  [| 0. ; coordinates.(3) ; 0. |] ; [| 0. ; 0. ; coordinates.(4) |] ; [| 0. ; 0. ; coordinates.(5) |] |]
 and c1 = [| [| 0. ; coordinates.(2) ; coordinates.(4) |] ; [| 0. ; coordinates.(3) ; coordinates.(4) |] ;
  [| 0. ; coordinates.(2) ; coordinates.(5) |] ;  [| 0. ; coordinates.(3) ; coordinates.(5) |] |]
 and c2 = [| [| coordinates.(0) ; 0. ; coordinates.(4) |] ; [| coordinates.(0) ; 0. ; coordinates.(5) |] ;
  [| coordinates.(1) ; 0. ; coordinates.(4) |] ; [| coordinates.(1) ; 0. ; coordinates.(5) |] |]
 and c3 = [| [| coordinates.(0) ; coordinates.(2) ; 0. |] ; [| coordinates.(0) ; coordinates.(3) ; 0. |] ;
  [| coordinates.(1) ; coordinates.(2) ; 0. |] ; [| coordinates.(1) ; 0. ; coordinates.(3) ; 0. |] |]
 and c4 = [| [| coordinates.(0) ; coordinates.(2) ; coordinates.(4) |] ; [| coordinates.(0) ; coordinates.(2) ; coordinates.(5) |] ;
  [| coordinates.(0) ; coordinates.(3) ; coordinates.(4) |] ; [| coordinates.(0) ; coordinates.(3) ; coordinates.(5) |] ;
  [| coordinates.(1) ; coordinates.(2) ; coordinates.(4) |] ; [| coordinates.(1) ; coordinates.(2) ; coordinates.(5) |] ;
  [| coordinates.(1) ; coordinates.(3) ; coordinates.(4) |] ; [| coordinates.(1) ; coordinates.(3) ; coordinates.(5) |] |] in
  let c5 = Array.append c0 c1
  and c6 = Array.append c2 c3 in
   let c7 = Array.append c5 c6 in
    Array.append c7 c4 ;;

(** {v box_proj_3_2 vector frame_coordinates v} *)
let box_proj_3_2 = fun (u:float array) (coordinates:float array) ->
 let m = Matrix.vector_float_chose_3 u
 and eee = Matrix.vector_float_norm_2 u
 and coord = box_3 coordinates in
  let v = m.(1)
  and w = m.(2) in
   let coord0 = Array.map ( Matrix.vector_float_scal_prod v ) coord
   and coord1 = Array.map ( Matrix.vector_float_scal_prod w ) coord in
    [| eee *. Matrix.vector_float_min coord0 ; eee *. Matrix.vector_float_max coord0 ;
     eee *. Matrix.vector_float_min coord1 ; eee *. Matrix.vector_float_max coord1 |] ;;

(** {v frame frame_coordinates window_size v} *)
let frame = fun (coordinates:float array) (size:int array) ->
 let a = Array.make 11 (0,0,0,0)
 and b = Array.make 11 (0,0,0,0)
 and c = Array.make 11 (0,0,0,0)
 and d = Array.make 11 (0,0,0,0)
 and h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let h_scale = h_real /. ( float ( size.(0) - 2 * h_margin ) )
   and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real
   and hh = h_margin - 1
   and vv = v_margin - 1
   and max_stringsize = h_margin / ( 5 + tsh ) in
    let coupe = function s -> String.sub s 0 (min max_stringsize ( String.length s ) )
    and v_scale = 1. /. v_inv_scale
    and hhh = size.(0) - 2 * hh
    and h_h = size.(0) - hh
    and vvv = size.(1) - 2 * vv
    and v_v = size.(1) - vv
    and x_function = ( function z -> coordinates.(0) +. h_scale *. z ) in
     let v_function = ( function z -> ( coordinates.(2) +. v_scale *. z ) ) in
      Graphics.set_color Graphics.yellow ;
      Graphics.draw_rect hh vv hhh vvv ;
      for i = 0 to 10 do
       let j = i * ( hhh / 10 )
       and jj = i * ( vvv / 10 ) in
        let k = 1 + hh + j
        and kk = 1 + vv + jj in
         a.(i) <- ( k , vv - 4 , k , v_v + 4 ) ;
         b.(i) <- ( k , v_v , k , v_v + 4 ) ;
         c.(i) <- ( hh - 4 , kk , h_h + 4 , kk ) ;
         d.(i) <- ( h_h , kk , h_h + 4 , kk ) ;
         let chaine = string_of_float ( x_function ( float j ) ) in
          Graphics.moveto k ( vv / 3 ) ;
          Graphics.draw_string ( coupe chaine ) ;
          Graphics.moveto k ( v_v + vv / 3 ) ;
          Graphics.draw_string ( coupe chaine ) ;
         let chaine = string_of_float ( v_function ( float jj ) ) in
          Graphics.moveto ( hh / 7 ) kk ;
          Graphics.draw_string ( coupe chaine ) ;
          Graphics.moveto ( h_h + hh / 7 ) kk ;
          Graphics.draw_string ( coupe chaine ) ;
      done ;
      ( a , b , c , d ) ;;

(** {v restricted_frame frame_coordinates window_size v} *)
let restricted_frame = fun (coordinates:float array) (size:int array) ->
 let a = Array.make 11 (0,0,0,0)
 and c = Array.make 11 (0,0,0,0)
 and h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let h_scale = h_real /. ( float ( size.(0) - 2 * h_margin ) )
   and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real
   and hh = h_margin - 1
   and vv = v_margin - 1
   and max_stringsize = h_margin / ( 5 + tsh ) in
    let coupe = function s -> String.sub s 0 (min max_stringsize ( String.length s ) )
    and v_scale = 1. /. v_inv_scale
    and hhh = size.(0) - 2 * hh
    and h_h = size.(0) - hh
    and vvv = size.(1) - 2 * vv
    and v_v = size.(1) - vv
    and x_function = ( function z -> coordinates.(0) +. h_scale *. z ) in
     let v_function = ( function z -> ( coordinates.(2) +. v_scale *. z ) ) in
      Graphics.set_color Graphics.yellow ;
      Graphics.draw_rect hh vv hhh vvv ;
      for i = 0 to 10 do
       let j = i * ( hhh / 10 )
       and jj = i * ( vvv / 10 ) in
        let k = 1 + hh + j
        and kk = 1 + vv + jj in
         a.(i) <- ( k , vv - 4 , k , v_v + 4 ) ;
         c.(i) <- ( hh - 4 , kk , h_h + 4 , kk ) ;
         let chaine = string_of_float ( x_function ( float j ) ) in
          Graphics.moveto k ( vv / 3 ) ;
          Graphics.draw_string ( coupe chaine ) ;
          Graphics.moveto k ( v_v + vv / 3 ) ;
          Graphics.draw_string ( coupe chaine ) ;
         let chaine = string_of_float ( v_function ( float jj ) ) in
          Graphics.moveto ( hh / 7 ) kk ;
          Graphics.draw_string ( coupe chaine ) ;
          Graphics.moveto ( h_h + hh / 7 ) kk ;
          Graphics.draw_string ( coupe chaine ) ;
      done ;;

(** {v grid coordinates size hh vv v} *)
let grid = fun (coordinates:float array) (size:int array) (hh:int) (vv:int) ->
 let ( a , b , c , d ) = frame coordinates size in
  Graphics.set_color Graphics.red ;
  Graphics.draw_segments a ;
  Graphics.draw_segments c ;
  Graphics.set_color Graphics.white ;
  let chaine = string_of_float coordinates.(0) in
   Graphics.moveto ( hh / 3 ) 3 ;
   Graphics.draw_string chaine ;
  let chaine = string_of_float coordinates.(1) in
   Graphics.moveto ( size.(0) - ( tsh * ( String.length chaine ) + 3 ) ) 3 ;
   Graphics.draw_string chaine ;
  let chaine = string_of_float coordinates.(2) in
   Graphics.moveto 3 ( 2 * vv / 3 ) ;
   Graphics.draw_string chaine ;
  let chaine = string_of_float coordinates.(3) in
   Graphics.moveto 3 ( size.(1) - vv / 3 ) ;
   Graphics.draw_string chaine ;;

(** {v paint_coordinates coordinates size hh vv v} *)
let paint_coordinates = fun (coordinates:float array) (size:int array) (hh:int) (vv:int) ->
 restricted_frame coordinates size ;
 Graphics.set_color Graphics.white ;
 let chaine = string_of_float coordinates.(0) in
  Graphics.moveto ( hh / 3 ) 3 ;
  Graphics.draw_string chaine ;
 let chaine = string_of_float coordinates.(1) in
  Graphics.moveto ( size.(0) - ( tsh * ( String.length chaine ) + 3 ) ) 3 ;
  Graphics.draw_string chaine ;
 let chaine = string_of_float coordinates.(2) in
  Graphics.moveto 3 ( 2 * vv / 3 ) ;
  Graphics.draw_string chaine ;
 let chaine = string_of_float coordinates.(3) in
  Graphics.moveto 3 ( size.(1) - vv / 3 ) ;
  Graphics.draw_string chaine ;;

(** {v prepare_1_1 coordinates size v} *)
let prepare_1_1 = fun (coordinates:float array) (size:int array) ->
 let h_size = string_of_int size.(0)
 and v_size = string_of_int size.(1)
 and h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let h_scale = h_real /. ( float ( size.(0) - 2 * h_margin ) )
   and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real
   and hh = h_margin - 1
   and vv = v_margin - 1 in
    let x_function = ( function z -> coordinates.(0) +. h_scale *. z )
    and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale ) in
     Graphics.close_graph () ;
     Graphics.open_graph ( " " ^ h_size ^ "x" ^ v_size ) ;
     Graphics.set_color Graphics.black ;
     Graphics.fill_rect 0 0 size.(0) size.(1) ;
     ( hh , vv , h_margin , v_margin , x_function , y_function ) ;;

(** {v prep_1_1 coordinates size v} *)
let prep_1_1 = fun (coordinates:float array) (size:int array) ->
 let h_size = string_of_int size.(0)
 and v_size = string_of_int size.(1) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let hh = h_margin - 1
   and vv = v_margin - 1 in
    Graphics.close_graph () ;
    Graphics.open_graph ( " " ^ h_size ^ "x" ^ v_size ) ;
    Graphics.set_color Graphics.black ;
    Graphics.fill_rect 0 0 size.(0) size.(1) ;
    ( hh , vv ) ;;

(** {v prepare_1_2 nsteps coordinates size interval v} *)
let prepare_1_2 = fun (nsteps:int) (coordinates:float array) (size:int array) (interval:float array) ->
 let h_size = string_of_int size.(0)
 and v_size = string_of_int size.(1)
 and h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2)
 and h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
 and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
  let h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
  and t_scale = ( interval.(1) -. interval.(0) ) /. ( float nsteps )
  and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real
  and hh = h_margin - 1
  and vv = v_margin - 1 in
   let t_function = ( function z -> interval.(0) +. t_scale *. z )
   and x_function = ( function z -> ( z -. coordinates.(0) ) *. h_inv_scale ) 
   and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale ) in
    Graphics.close_graph () ;
    Graphics.open_graph ( " " ^ h_size ^ "x" ^ v_size ) ;
    Graphics.set_color Graphics.black ;
    Graphics.fill_rect 0 0 size.(0) size.(1) ;
    ( hh , vv , h_margin , v_margin , t_function , x_function , y_function ) ;;

(** {v prepare_field_1_2 nsteps coordinates size interval v} *)
let prepare_field_1_2 = fun (nsteps:int) (coordinates:float array) (size:int array) (interval:float array) ->
 let h_size = string_of_int size.(0)
 and v_size = string_of_int size.(1)
 and h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
   and t_scale = ( interval.(1) -. interval.(0) ) /. ( float nsteps )
   and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real
   and hh = h_margin - 1
   and vv = v_margin - 1 in
    let t_function = ( function z -> interval.(0) +. t_scale *. z )
    and x_function = ( function z -> ( z -. coordinates.(0) ) *. h_inv_scale ) 
    and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale )
    and vx_function = ( function z -> z *. h_inv_scale ) 
    and vy_function = ( function z -> z *. v_inv_scale ) in
     Graphics.close_graph () ;
     Graphics.open_graph ( " " ^ h_size ^ "x" ^ v_size ) ;
     Graphics.set_color Graphics.black ;
     Graphics.fill_rect 0 0 size.(0) size.(1) ;
    ( hh , vv , h_margin , v_margin , t_function , x_function , y_function , vx_function , vy_function ) ;;

(** {v prepare_section_2_2 section half_ngrid coordinates size v} *)
let prepare_section_2_2 = fun (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ngrid = 2 * half_ngrid
 and r = Array.length ( f [| coordinates.(0) ; coordinates.(2) |] )
 and h_size = string_of_int size.(0)
 and v_size = string_of_int size.(1)
 and h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let h_factor = ( size.(0) - 2 * h_margin ) / ngrid
   and v_factor = ( size.(1) - 2 * v_margin ) / ngrid
   and h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
   and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real
   and hh = h_margin - 1
   and vv = v_margin - 1 in
    let h_scale = h_real /. ( float ( size.(0) - 2 * h_margin ) )
    and v_scale = v_real /. ( float ( size.(1) - 2 * v_margin ) )
    and hhh = size.(0) - 2 * hh
    and h_h = size.(0) - hh
    and vvv = size.(1) - 2 * vv
    and v_v = size.(1) - vv in
     let x_function = function i -> ( float i ) *. h_real /. ( float ngrid ) +. coordinates.(0)
     and y_function = function i -> ( float i ) *. v_real /. ( float ngrid ) +. coordinates.(2)
     and vx_function = ( function z -> z *. h_inv_scale ) 
     and vy_function = ( function z -> z *. v_inv_scale )
     and h_function = ( function z -> ( coordinates.(0) +. h_scale *. z ) )
     and v_function = ( function z -> ( coordinates.(2) +. v_scale *. z ) ) in
      ( r , ngrid , hh , vv , hhh , vvv , h_h , v_v , h_size , v_size , h_margin , v_margin , h_factor , v_factor , h_function , v_function , x_function , y_function , vx_function , vy_function ) ;;

(** {v init_section_2_2 hh vv hhh vvv h_size v_size size v} *)
let init_section_2_2 = fun (hh:int) (vv:int) (hhh:int) (vvv:int) h_size v_size (size:int array) ->
 Graphics.close_graph () ;
 Graphics.open_graph ( " " ^ h_size ^ "x" ^ v_size ) ;
 Graphics.set_color Graphics.black ;
 Graphics.fill_rect 0 0 size.(0) size.(1) ;
 Graphics.set_color Graphics.yellow ;
 Graphics.draw_rect hh vv hhh vvv ;;

(** {v terminate_section_2_2 hh vv hhh vvv h_h v_v h_margin v_margin h_function v_function coordinates size v} *)
let terminate_section_2_2 = fun (hh:int) (vv:int) (hhh:int) (vvv:int) (h_h:int) (v_v:int) (h_margin:int) (v_margin:int) h_function v_function (coordinates:float array) (size:int array) ->
 let aa = Array.make 11 (0,0,0,0)
 and bb = Array.make 11 (0,0,0,0)
 and cc = Array.make 11 (0,0,0,0)
 and dd = Array.make 11 (0,0,0,0)
 and max_stringsize = h_margin / ( 5 + tsh ) in
  let coupe = function s -> String.sub s 0 (min max_stringsize ( String.length s ) ) in
   for i = 0 to 10 do
    let j = i * ( hhh / 10 )
    and jj = i * ( vvv / 10 ) in
     let k = 1 + hh + j
     and kk = 1 + vv + jj in
      aa.(i) <- ( k , vv , k , vv - 4 ) ;
      bb.(i) <- ( k , v_v , k , v_v + 4 ) ;
      cc.(i) <- ( hh - 4 , kk , hh , kk ) ;
      dd.(i) <- ( h_h , kk , h_h + 4 , kk ) ;
      let chaine = string_of_float ( h_function ( float j ) ) in
       Graphics.moveto k ( vv / 3 ) ;
       Graphics.draw_string ( coupe chaine ) ;
       Graphics.moveto k ( v_v + vv / 3 ) ;
       Graphics.draw_string ( coupe chaine ) ;
      let chaine = string_of_float ( v_function ( float jj ) ) in
       Graphics.moveto ( hh / 7 ) kk ;
       Graphics.draw_string ( coupe chaine ) ;
       Graphics.moveto ( h_h + hh / 7 ) kk ;
       Graphics.draw_string ( coupe chaine ) ;
   done ;
   Graphics.set_color Graphics.yellow ;
   Graphics.draw_segments aa ;
   Graphics.draw_segments bb ;
   Graphics.draw_segments cc ;
   Graphics.draw_segments dd ;
   Graphics.set_color Graphics.white ;
   let chaine = string_of_float coordinates.(0) in
    Graphics.moveto ( hh / 3 ) 3 ;
    Graphics.draw_string chaine ;
   let chaine = string_of_float coordinates.(1) in
    Graphics.moveto ( size.(0) - ( tsh * ( String.length chaine ) + 3 ) ) 3 ;
    Graphics.draw_string chaine ;
   let chaine = string_of_float coordinates.(2) in
    Graphics.moveto 3 ( 2 * vv / 3 ) ;
    Graphics.draw_string chaine ;
   let chaine = string_of_float coordinates.(3) in
    Graphics.moveto 3 ( size.(1) - vv / 3 ) ;
    Graphics.draw_string chaine ;;

(** {v prepare_1_3 u interval nsteps coordinates size v} *)
let prepare_1_3 = fun (u:float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let h_size = string_of_int size.(0)
 and v_size = string_of_int size.(1)
 and accu = Array.make 6 0.
 and b = interval.(0)
 and a = ( interval.(1) -. interval.(0) ) /. ( float ( nsteps + 1 ) ) in
  Graphics.close_graph () ;
  Graphics.open_graph ( " " ^ h_size ^ "x" ^ v_size ) ;
  Graphics.set_color Graphics.black ;
  Graphics.fill_rect 0 0 size.(0) size.(1) ;
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and m = Matrix.vector_float_chose_3 u in
   let hh = h_margin - 1
   and v = m.(1)
   and w = m.(2)
   and coo = box_proj_3_2 u coordinates in
   ( a , b , accu , hh , v , w , coo ) ;;

(** {v project_1_3 accu a b u v w nsteps coordinates coo v} *)
let project_1_3 = fun (accu:float array) (a:float) (b:float) (u:float array) (v:float array) (w:float array) (nsteps:int) (coordinates:float array) (coo:float array) (f:float -> float array) ->
 let accumul = ref b in
  let g = function t -> [| Matrix.vector_float_scal_prod ( f t ) v ; Matrix.vector_float_scal_prod ( f t ) w |] in
   for i = 0 to nsteps do
    let value = f ( !accumul ) in
     accu.(0) <- min accu.(0) value.(0) ;
     accu.(1) <- max accu.(1) value.(0) ;
     accu.(2) <- min accu.(2) value.(1) ;
     accu.(3) <- max accu.(3) value.(1) ;
     accu.(4) <- min accu.(4) value.(2) ;
     accu.(5) <- max accu.(5) value.(2) ;
     accumul := !accumul +. a ;
   done ;
   g ;;

(** {v multi_project_1_3 accu a b u v w nsteps coordinates coo v} *)
let multi_project_1_3 = fun (accu:float array) (a:float) (b:float) (u:float array) (v:float array) (w:float array) (nsteps:int) (coordinates:float array) (coo:float array) (f:float -> float array) ->
 let accumul = ref b
 and r = Array.length ( f a ) in
  assert ( r mod 3 == 0 ) ;
  let rrr = ( r / 3 ) in
   let rr = 2 * rrr
   and r_r = pred rrr in
    let g = function t ->
     begin
      let result = Array.make rr 0.
      and px = Matrix.vector_float_scal_prod v
      and py = Matrix.vector_float_scal_prod w
      and value = f t in
       for i = 0 to r_r do
        let ii = 2 * i
        and i_i = 3 * i in
         let slice = Array.sub value i_i 3 in
          result.(ii) <- px slice ;
          result.( succ ii ) <- py slice ;
       done ;
       result
     end in
     for i = 0 to nsteps do
      let value = f ( !accumul ) in
       accu.(0) <- min accu.(0) value.(0) ;
       accu.(1) <- max accu.(1) value.(0) ;
       accu.(2) <- min accu.(2) value.(1) ;
       accu.(3) <- max accu.(3) value.(1) ;
       accu.(4) <- min accu.(4) value.(2) ;
       accu.(5) <- max accu.(5) value.(2) ;
       accumul := !accumul +. a ;
     done ;
     g ;;

(** {v terminate_1_3 accu hh v} *)
let terminate_1_3 = fun (accu:float array) (hh:int) ->
 Graphics.set_color Graphics.yellow ;
 let chaine = Matrix.string_of_vector_float accu in
  Graphics.moveto ( hh / 3 ) 3 ;
  Graphics.draw_string chaine ;;

(** {v prepare_2_3 u interval nsteps coordinates size v} *)
let prepare_2_3 = fun (u:float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let h_size = string_of_int size.(0)
 and v_size = string_of_int size.(1)
 and xx = Matrix.float_closed_equal_subdivision a.(0) ngrid b.(0)
 and yy = Matrix.float_closed_equal_subdivision a.(1) ngrid b.(1)
 and bb = a.(0)
 and bbb = a.(1)
 and aa = ( b.(0) -. a.(0) ) /. ( float ( nsteps + 1 ) )
 and aaa = ( b.(1) -. a.(1) ) /. ( float ( nsteps + 1 ) ) in
  Graphics.close_graph () ;
  Graphics.open_graph ( " " ^ h_size ^ "x" ^ v_size ) ;
  Graphics.set_color Graphics.black ;
  Graphics.fill_rect 0 0 size.(0) size.(1) ;
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh ) in
   let hh = h_margin - 1 in
    ( aa , aaa , bb , bbb , hh , xx , yy ) ;;

(** {v prepare_section_3_3 vector section half_ngrid coordinates size v} *)
let prepare_section_3_3 = fun (u:float array) (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let h_size = string_of_int size.(0)
 and v_size = string_of_int size.(1)
 and h_middle = size.(0) / 2
 and v_middle = size.(1) / 2
 and ng = 2 * half_ngrid
 and shift = Array.make 2 0.
 and m = Matrix.vector_float_chose_3 u
 and eee = 1. /. ( Matrix.vector_float_norm_2 u )
 and coord = box_proj_3_2 u coordinates in
  let ngrid = ng + 1
  and vv = m.(1)
  and ww = m.(2)
  and h_real = coord.(1) -. coord.(0)
  and v_real = coord.(3) -. coord.(2) in
   let xx = Matrix.float_closed_equal_subdivision coordinates.(0) ngrid coordinates.(1)
   and yy = Matrix.float_closed_equal_subdivision coordinates.(2) ngrid coordinates.(3)
   and zz = Matrix.float_closed_equal_subdivision coordinates.(4) ngrid coordinates.(5)
   and h_inv_scale = ( float size.(0) ) /. h_real
   and v_inv_scale = ( float size.(1) ) /. v_real
   and v = Matrix.vector_float_scal_mult eee vv
   and w = Matrix.vector_float_scal_mult eee ww in
    let vx_function = function z -> int_of_float ( z *. h_inv_scale )
    and vy_function = function z -> int_of_float ( z *. v_inv_scale )
    and x_function = function z -> int_of_float ( z *. h_inv_scale ) + h_middle
    and y_function = function z -> int_of_float ( z *. v_inv_scale ) + v_middle in
    ( ng , shift , h_size , v_size , v , w , xx , yy , zz , x_function , y_function , vx_function , vy_function ) ;;

(** {v init_section_3_3 h_size v_size size v} *)
let init_section_3_3 = fun h_size v_size (size:int array) ->
 Graphics.close_graph () ;
 Graphics.open_graph ( " " ^ h_size ^ "x" ^ v_size ) ;
 Graphics.set_color Graphics.black ;
 Graphics.fill_rect 0 0 size.(0) size.(1) ;;


(** {v light_rectangle up width height v} This function is supposed to draw a segment with integer ends with anti-aliasing.
The boolean [up] indicates if the segment is oriented SW-NE or in the other case NW-SE.

Cette fonction sert à dessiner un segment à extrémités entières avec antialiassage. Le booléen [up] indique si le segment est orienté SO-NE, sinon NO-SE. *)
let light_rectangle = fun (up:bool) (width:int) (height:int) ->
 let info = [| height ; width ; 0 |]
 and ww = float ( width - 1 )
 and hh = float ( height - 1 )
 and w = float ( width )
 and h = float ( height )
 and eps = 1e-6
 and p = ( float height ) /. ( float width )
 and rr = pred height in
  let pinv = 1. /. p in
   let coeff = ref 0.
   and x_beginning = ref 0.
   and x_end = ref pinv
   and x_left = ref 0.
   and x_right = ref ( min 1. pinv )
   and y_left = ref 0.
   and y_right = ref ( min p 1. ) in
    let luminosity = ref [] in
     begin
      match ( width , height ) with
      | ( 0 , _ ) | ( 1 , _ ) ->
       begin
        for i = 0 to rr do
         luminosity := ( i , 0 , 1. ) :: !luminosity ;
         info.(2) <- height ;
        done
       end
      | ( _ , 0 ) | ( _ , 1 ) ->
       begin
        for i = 0 to pred width do
         luminosity := ( 0 , i , 1. ) :: !luminosity ;
         info.(2) <- width ;
        done
       end
      | ( _ , _ ) ->
       begin
        let count = ref 0 in
         if up then
          begin
           for i = 0 to rr do
            for j = int_of_float ( floor !x_beginning ) to int_of_float ( floor ( !x_end -. eps ) ) do
             coeff := min 1. ( Matrix.vector_float_norm_2 [| !x_right -. !x_left ; !y_right -. !y_left |] ) ;
             luminosity := ( i , j , !coeff ) :: !luminosity ;
             incr count ;
             x_left := min ww !x_right ;
             y_left := min hh !y_right ;
             x_right := Matrix.vector_float_min [| ceil ( !x_left +. eps ) ; !x_left +. pinv ; w |] ;
             y_right := Matrix.vector_float_min [| ceil ( !y_left +. eps ) ; !y_left +. p ; h |] ;
             x_right := min !x_right ( !y_right *. pinv ) ;
             y_right := min !y_right ( !x_right *. p ) ;
            done ;
            x_beginning := min ww !x_end ;
            x_end := min w ( !x_beginning +. pinv ) ;
           done ;
          end
         else
          begin
           for i = rr downto 0 do
            for j = int_of_float ( floor !x_beginning ) to int_of_float ( floor ( !x_end -. eps ) ) do
             coeff := min 1. ( Matrix.vector_float_norm_2 [| !x_right -. !x_left ; !y_right -. !y_left |] ) ;
             luminosity := ( i , j , !coeff ) :: !luminosity ;
             incr count ;
             x_left := min ww !x_right ;
             y_left := min hh !y_right ;
             x_right := Matrix.vector_float_min [| ceil ( !x_left +. eps ) ; !x_left +. pinv ; w |] ;
             y_right := Matrix.vector_float_min [| ceil ( !y_left +. eps ) ; !y_left +. p ; h |] ;
             x_right := min !x_right ( !y_right *. pinv ) ;
             y_right := min !y_right ( !x_right *. p ) ;
            done ;
            x_beginning := min ww !x_end ;
            x_end := min w ( !x_beginning +. pinv ) ;
           done ;
          end ;
         info.(2) <- !count ;
       end
     end ;
     ( info , !luminosity ) ;;

(** {v enlighten luminosity color v} *)
let enlighten = fun (x:float) (color:Graphics.color) ->
 let colors = Array.map float ( Readwrite.color_to_int_rgb color )
 and f = function z -> min 255 ( int_of_float ( ceil ( x *. z ) ) ) in
  let result = Array.map f colors in
   Graphics.rgb result.(0) result.(1) result.(2) ;;

(** {v power_gamma exponent luminosity v} *)
let power_gamma = fun (exponent:float) (x:float) ->
 let xx = min 1. ( max 0. x ) in
  ( xx ** exponent ) *. ( 1. -. ( ( 1. -. xx ) ** exponent ) );;


(** {v draw_smooth_segment gamma color abscissa ordinate abscissa ordinate v} 
The segment with integer ends is drawn with anti-aliasing.

Le segment à extrémités entières est dessiné avec antialiassage.*)
let draw_smooth_segment = fun (gamma:float -> float) (color:Graphics.color) (old_x:int) (old_y:int) (x:int) (y:int) ->
 let ( x_left , x_right ) = if old_x <= x then ( old_x , x ) else ( x , old_x )
 and ( y_left , y_right ) = if old_y <= y then ( old_y , y ) else ( y , old_y ) in
  let width = x_right - x_left
  and height = y_right - y_left
  and up = ( x - old_x ) * ( y - old_y ) >= 0 in
   let ( info , luminosity ) = light_rectangle up width height in
    let remaining = ref luminosity in
     for k = 0 to pred info.(2) do
      let ( i , j , coeff ) = List.hd !remaining in
       Graphics.set_color ( enlighten ( gamma coeff ) color ) ;
       Graphics.plot ( x_left + j ) ( y_left + i ) ;
       remaining := List.tl !remaining ;
     done ;
      Graphics.set_color color ;
      Graphics.plot old_x old_y ;
      Graphics.plot x y ;;


(** {v oversample_paint gamma color h_margin v_margin luminosity_rectangle v} 
This function displays pixels of the color [color] according to the luminosity recorded in the sparse matrix [luminosity_rectangle].

Cette fonction affiche des pixels de couleur [color] selon la luminosité enregistrée dans la matrice creuse [luminosity_rectangle]. *)
let oversample_paint = fun (gamma:float -> float) (color:Graphics.color) (h_margin:int) (v_margin:int) (luminosity_rectangle:int array * (int * int * float) list) ->
 let ( info , luminosity ) = luminosity_rectangle in
  let remaining = ref luminosity in
   for k = 0 to pred info.(2) do
    let ( i , j , coeff ) = List.hd !remaining in
     Graphics.set_color ( enlighten ( gamma coeff ) color ) ;
     Graphics.plot ( h_margin + j ) ( v_margin + i ) ;
     remaining := List.tl !remaining ;
   done ;;


(** {v paint_axes paint_segments color v w extrema v} The function [paint_segments] draws the segments representing the axes.

La fonction [paint_segments] dessine les segments représentant les axes. *)
let paint_axes = fun paint_segments (color:Graphics.color) (v:float array) (w:float array) (axes:float array) (coo:float array) (size:int array) ->
 assert ( ( Array.length axes ) >= 6 ) ;
 let h_real = coo.(1) -. coo.(0)
 and v_real = coo.(3) -. coo.(2)
 and h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
 and v_margin = max ( size.(1) / 10 ) ( 4 * tsv )
 and f = function x -> [| Matrix.vector_float_scal_prod v x ; Matrix.vector_float_scal_prod w x |] in
  let ax = f [| axes.(0) ; 0. ; 0. |]
  and bx = f [| axes.(1) ; 0. ; 0. |]
  and ay = f [| 0. ; axes.(2) ; 0. |]
  and by = f [| 0. ; axes.(3) ; 0. |]
  and az = f [| 0. ; 0. ; axes.(4) |]
  and bz = f [| 0. ; 0. ; axes.(5) |]
  and h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
  and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real in
   let x_function = ( function z -> ( z ) *. h_inv_scale ) 
   and y_function = ( function z -> ( z  ) *. v_inv_scale )
   and h_center = Util.round ( -. coo.(0) *. h_inv_scale ) + h_margin
   and v_center = Util.round ( -. coo.(2) *. v_inv_scale ) + v_margin in
    let g = function z -> [| h_center + Util.round ( x_function z.(0) ) ; v_center + Util.round ( y_function z.(1) ) |] in
     let cx = g ax
     and dx = g bx
     and cy = g ay
     and dy = g by
     and cz = g az
     and dz = g bz in
      paint_segments color [| ( cx , dx ) ; ( cy , dy ) ; ( cz , dz ) |] ;;

(** {v draw_segments color table v} *)
let draw_segments = fun (color:Graphics.color) (table:((int array) * (int array)) array) ->
 let r = Array.length table in
  let tableau = Array.make r ( 0 , 0 , 0 , 0 ) in
   for i = 0 to pred r do
    let a = fst table.(i)
    and b = snd table.(i) in
     tableau.(i) <- ( a.(0) , a.(1) , b.(0) , b.(1) )
   done ;
   Graphics.set_color color ;
   Graphics.draw_segments tableau ;;

(** {v smooth_segments gamma color table v} *)
let smooth_segments = fun (gamma:float -> float) (color:Graphics.color) (table:((int array) * (int array)) array) ->
 let r = Array.length table in
  for i = 0 to pred r do
   let a = fst table.(i)
   and b = snd table.(i) in
    draw_smooth_segment gamma color a.(0) a.(1) b.(0) b.(1) ;
  done ;;

(** {v draw_axes color v w axes coo size v} *)
let draw_axes = fun (color:Graphics.color) (v:float array) (w:float array) (axes:float array) (coo:float array) (size:int array) ->
 paint_axes draw_segments color v w axes coo size ;;

(** {v smooth_axes gamma color v w axes coo size v} *)
let smooth_axes = fun (gamma:float -> float) (color:Graphics.color) (v:float array) (w:float array) (axes:float array) (coo:float array) (size:int array) ->
 paint_axes ( smooth_segments gamma ) color v w axes coo size ;;


(** {v func_plot size h_margin v_margin x_function y_function function v} *)
let func_plot = fun (size:int array) (h_margin:int) (v_margin:int) (x_function:float -> float) (y_function:float -> float) (f:float -> float) ->
 for i = 0 to size.(0) - 2 * h_margin do
  let x = x_function ( float i ) in
   let y = y_function ( f x ) in
    Graphics.plot ( i + h_margin ) ( ( int_of_float y ) + v_margin ) ;
 done ;;

(** {v func_segment size h_margin v_margin x_function y_function function v} *)
let func_segment = fun (size:int array) (h_margin:int) (v_margin:int) (x_function:float -> float) (y_function:float -> float) (f:float -> float) ->
 Graphics.moveto ( h_margin ) ( ( int_of_float ( y_function ( f ( x_function 0. ) ) ) ) + v_margin ) ;
 for i = 1 to size.(0) - 2 * h_margin do
  let x = x_function ( float i ) in
   let y = y_function ( f x ) in
    Graphics.lineto ( i + h_margin ) ( ( int_of_float y ) + v_margin ) ;
 done ;;

(** {v func_smooth gamma color size h_margin v_margin x_function y_function function v} *)
let func_smooth = fun (gamma:float -> float) (color:Graphics.color) (size:int array) (h_margin:int) (v_margin:int) (x_function:float -> float) (y_function:float -> float) (f:float -> float) ->
 let old_x = ref h_margin
 and old_y = ref ( int_of_float ( y_function ( f ( x_function 0. ) ) ) + v_margin ) in
  Graphics.moveto ( h_margin ) ( ( int_of_float ( y_function ( f ( x_function 0. ) ) ) ) + v_margin ) ;
  for i = 1 to size.(0) - 2 * h_margin do
   let x = x_function ( float i ) in
    let y = y_function ( f x ) in
     let xx = i + h_margin
     and yy = ( int_of_float y ) + v_margin in
      draw_smooth_segment gamma color !old_x !old_y xx yy ;
      old_x := xx ;
      old_y := yy ;
  done ;;


(** {v func_oversample over_sampling_factor gamma color size h_margin v_margin x_function y_function function v}
This function records the data corresponding to a display zone of resolution dilated by the factor [n]
in a sparse matrix, by counting the multiplicity of the points included in the same real pixel.

Cette fonction enregistre les données correspondant à une zone d'affichage de définition dilatée par le facteur [n] 
dans une matrice creuse, en comptant la multiplicité des points inclus dans le même pixel réel. *)
let func_oversample = fun (n:int) (gamma:float -> float) (color:Graphics.color) (size:int array) (h_margin:int) (v_margin:int) (x_function:float -> float) (y_function:float -> float) (f:float -> float) ->
 let dims = [| size.(1) -2 * v_margin ; size.(0) -2 * h_margin |]
 and coeff = ref 0.
 and pn = pred n
 and filling = ref 0
 and column = ref []
 and intercal = fun index ( a , b ) -> ( a , index , b )
 and nn = 1. /. ( float n ) in
  let h_function = function z -> x_function ( z *. nn )
  and v_function = function z -> ( float n ) *. ( y_function z )
  and luminosity = ref [] in
   for jj = 0 to pred dims.(1) do
    for k = 0 to pn do
     let j = n * jj + k in
      let x = h_function ( float j ) in
       let y = v_function ( f x ) in
        let i = Util.round y in
         let ii = i / n in
          column := Util.list_accumulate compare ( +. ) ( ii , nn ) !column ;
    done ;
    luminosity := List.rev_append ( List.rev_map ( intercal jj ) !column ) !luminosity ;
    filling := !filling + ( List.length !column ) ;
    column := [] ;
    coeff := 0. ;
   done ;
   oversample_paint gamma color h_margin v_margin ( Array.append dims [| !filling |] , !luminosity ) ;;

(** {v curve_plot color nsteps t_function h_margin v_margin x_function y_function function v} *)
let curve_plot = fun (color:Graphics.color) (nsteps:int) (t_function:float -> float) (h_margin:int) (v_margin:int) (x_function:float -> float) (y_function:float -> float) (f:float -> float array) ->
 Graphics.set_color color ;
 for i = 0 to nsteps do
  let t = t_function ( float i ) in
   let z = f t in
    let x = x_function ( z.(0) )
    and y = y_function ( z.(1) ) in
     Graphics.plot ( ( Util.round x ) + h_margin ) ( ( Util.round y ) + v_margin ) ;
 done ;;

(** {v curve_segment color nsteps t_function h_margin v_margin x_function y_function function v} *)
let curve_segment = fun (color:Graphics.color) (nsteps:int) (t_function:float -> float) (h_margin:int) (v_margin:int) (x_function:float -> float) (y_function:float -> float) (f:float -> float array) ->
 Graphics.set_color color ;
 let z0 = f ( t_function 0. ) in
  Graphics.moveto ( h_margin + ( int_of_float ( x_function z0.(0) ) ) ) ( v_margin + ( int_of_float ( y_function z0.(1) ) ) ) ;
  for i = 1 to nsteps do
   let t = t_function ( float i ) in
    let z = f t in
     let x = x_function ( z.(0) )
     and y = y_function ( z.(1) ) in
      Graphics.lineto ( ( int_of_float x ) + h_margin ) ( ( int_of_float y ) + v_margin ) ;
  done ;;

(** {v curve_smooth gamma color nsteps t_function h_margin v_margin x_function y_function function v} *)
let curve_smooth = fun (gamma:float -> float) (color:Graphics.color) (nsteps:int) (t_function:float -> float) (h_margin:int) (v_margin:int) (x_function:float -> float) (y_function:float -> float) (f:float -> float array) ->
 let z0 = f ( t_function 0. ) in
  let old_x = ref ( ( int_of_float ( x_function z0.(0) ) ) + h_margin )
  and old_y = ref ( ( int_of_float ( y_function z0.(1) ) ) + v_margin ) in
   Graphics.moveto ( h_margin + ( int_of_float ( x_function z0.(0) ) ) ) ( v_margin + ( int_of_float ( y_function z0.(1) ) ) ) ;
   for i = 1 to nsteps do
    let t = t_function ( float i ) in
     let z = f t in
      let x = x_function z.(0)
      and y = y_function z.(1) in
       let xx = ( int_of_float x ) + h_margin
       and yy = ( int_of_float y ) + v_margin in
        draw_smooth_segment gamma color !old_x !old_y xx yy ;
        old_x := xx ;
        old_y := yy ;
   done ;;


(** {v curve_oversample over_sampling_factor gamma size color nsteps t_function h_margin v_margin x_function y_function function v}
This function records the data corresponding to a display zone of resolution dilated by the factor [n]
in a sparse matrix, by counting the multiplicity of the points included in the same real pixel.

Cette fonction enregistre les données correspondant à une zone d'affichage de définition dilatée par le facteur [n] 
dans une matrice creuse, en comptant la multiplicité des points inclus dans le même pixel réel. *)
let curve_oversample = fun (n:int) (gamma:float -> float) (size:int array) (color:Graphics.color) (nsteps:int) (t_function:float -> float) (h_margin:int) (v_margin:int) (x_function:float -> float) (y_function:float -> float) (f:float -> float array) ->
 let dims = [| size.(1) -2 * v_margin ; size.(0) -2 * h_margin |]
 and format = function ( a , b ) -> ( a.(0) , a.(1) , b )
 and cmp = Util.lexico_compare ( fun a b -> compare b a )
 and nn = 1. /. ( float n ) in
  let luminosity = ref [] in
   for i = 0 to nsteps do
    let t = t_function ( float i ) in
     let z = f t in
      let x = x_function z.(0)
      and y = y_function z.(1) in
       let xx = Util.round x
       and yy = Util.round y in
        luminosity := Util.list_accumulate cmp ( +. ) ( [| yy ; xx |] , nn ) !luminosity ;
   done ;
   oversample_paint gamma color h_margin v_margin ( Array.append dims [| List.length !luminosity |] , List.rev_map format !luminosity ) ;;




(** {C § } *)
(** 
{1 Dessins de R dans R}
{1 Drawings from R to R}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v function_grid_paint_1_1 paint function frame_coordinates window_size v} *)
let function_grid_paint_1_1 = fun paint (f:float -> float) (coordinates:float array) (size:int array) ->
 let ( hh , vv , h_margin , v_margin , x_function , y_function ) = prepare_1_1 coordinates size in
  grid coordinates size hh vv ;
  paint size h_margin v_margin x_function y_function f ;;

(** {v function_grid_draw_1_1 function frame_coordinates window_size v} *)
let function_grid_draw_1_1 = fun (f:float -> float) (coordinates:float array) (size:int array) ->
 function_grid_paint_1_1 func_plot f coordinates size ;;

(** {v function_grid_segment_1_1 function frame_coordinates window_size v} *)
let function_grid_segment_1_1 = fun (f:float -> float) (coordinates:float array) (size:int array) ->
 function_grid_paint_1_1 func_segment f coordinates size ;;

(** {v function_grid_smooth_1_1 gamma function frame_coordinates window_size v} *)
let function_grid_smooth_1_1 = fun (gamma:float -> float) (f:float -> float) (coordinates:float array) (size:int array) ->
 function_grid_paint_1_1 ( func_smooth gamma Graphics.white ) f coordinates size ;;

(** {v function_grid_oversample_1_1 over_sampling_factor gamma function frame_coordinates window_size v} *)
let function_grid_oversample_1_1 = fun (n:int) (gamma:float -> float) (f:float -> float) (coordinates:float array) (size:int array) ->
 function_grid_paint_1_1 ( func_oversample n gamma Graphics.white ) f coordinates size ;;


(** {v function_paint_1_1 paint function frame_coordinates window_size v} *)
let function_paint_1_1 = fun paint (f:float -> float) (coordinates:float array) (size:int array) ->
 let ( hh , vv , h_margin , v_margin , x_function , y_function ) = prepare_1_1 coordinates size in
  Graphics.set_color Graphics.white ;
  paint size h_margin v_margin x_function y_function f ;
  paint_coordinates coordinates size hh vv ;;

(** {v function_draw_1_1 function frame_coordinates window_size v} *)
let function_draw_1_1 = fun (f:float -> float) (coordinates:float array) (size:int array) ->
 function_paint_1_1 func_plot f coordinates size ;;

(** {v function_segment_1_1 function frame_coordinates window_size v} *)
let function_segment_1_1 = fun (f:float -> float) (coordinates:float array) (size:int array) ->
 function_paint_1_1 func_segment f coordinates size ;;

(** {v function_smooth_1_1 gamma function frame_coordinates window_size v} *)
let function_smooth_1_1 = fun (gamma:float -> float) (f:float -> float) (coordinates:float array) (size:int array) ->
 function_paint_1_1 ( func_smooth gamma Graphics.white ) f coordinates size ;;

(** {v function_oversample_1_1 over_sampling_factor gamma function frame_coordinates window_size v} *)
let function_oversample_1_1 = fun (n:int) (gamma:float -> float) (f:float -> float) (coordinates:float array) (size:int array) ->
 function_paint_1_1 ( func_oversample n gamma Graphics.white ) f coordinates size ;;


(** {v function_partial_paint_1_1 paint function frame_coordinates window_size v} *)
let function_partial_paint_1_1 = fun paint (f:float -> float) (coordinates:float array) (size:int array) ->
 let h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let h_scale = h_real /. ( float ( size.(0) - 2 * h_margin ) )
   and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real in
    let x_function = ( function z -> coordinates.(0) +. h_scale *. z )
    and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale ) in
     paint size h_margin v_margin x_function y_function f ;;

(** {v function_partial_draw_1_1 function frame_coordinates window_size v} *)
let function_partial_draw_1_1 = fun (f:float -> float) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_1 func_plot f coordinates size ;;

(** {v function_partial_segment_1_1 function frame_coordinates window_size v} *)
let function_partial_segment_1_1 = fun (f:float -> float) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_1 func_segment f coordinates size ;;

(** {v function_partial_smooth_1_1 gamma color function frame_coordinates window_size v} *)
let function_partial_smooth_1_1 = fun (gamma:float -> float) (color:Graphics.color) (f:float -> float) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_1 ( func_smooth gamma color ) f coordinates size ;;

(** {v function_partial_oversample_1_1 over_sampling_factor gamma color function frame_coordinates window_size v} *)
let function_partial_oversample_1_1 = fun (n:int) (gamma:float -> float) (color:Graphics.color) (f:float -> float) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_1 ( func_oversample n gamma color ) f coordinates size ;;


(** {v function_multi_paint_1_1 paint function frame_coordinates window_size v} *)
let function_multi_paint_1_1 = fun paint (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  Graphics.set_color Graphics.white ;
  for i = 0 to r - 1 do
   paint f.(i) coordinates size ;
  done ;
  paint_coordinates coordinates size hh vv ;;

(** {v function_multi_draw_1_1 function frame_coordinates window_size v} *)
let function_multi_draw_1_1 = fun (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_1 function_partial_draw_1_1 f coordinates size ;;

(** {v function_multi_segment_1_1 function frame_coordinates window_size v} *)
let function_multi_segment_1_1 = fun (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_1 function_partial_segment_1_1 f coordinates size ;;

(** {v function_multi_smooth_1_1 gamma function frame_coordinates window_size v} *)
let function_multi_smooth_1_1 = fun (gamma:float -> float) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_1 ( function_partial_smooth_1_1 gamma Graphics.white ) f coordinates size ;;

(** {v function_multi_oversample_1_1 over_sampling_factor gamma function frame_coordinates window_size v} *)
let function_multi_oversample_1_1 = fun (n:int) (gamma:float -> float) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_1 ( function_partial_oversample_1_1 n gamma Graphics.white ) f coordinates size ;;


(** {v function_multicolor_paint_1_1 paint colors function frame_coordinates window_size v} *)
let function_multicolor_paint_1_1 = fun paint (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
 for i = 0 to r - 1 do
  paint colors.(i) f.(i) coordinates size ;
 done ;
 paint_coordinates coordinates size hh vv ;;

(** {v function_multicolor_draw_1_1 colors function frame_coordinates window_size v} *)
let function_multicolor_draw_1_1 = fun (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 let paint = fun color g c s ->
  begin
   Graphics.set_color color ;
   function_partial_draw_1_1 g c s ;
  end in
 function_multicolor_paint_1_1 paint colors f coordinates size ;;

(** {v function_multicolor_segment_1_1 colors function frame_coordinates window_size v} *)
let function_multicolor_segment_1_1 = fun (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 let paint = fun color g c s ->
  begin
   Graphics.set_color color ;
   function_partial_segment_1_1 g c s ;
  end in
 function_multicolor_paint_1_1 paint colors f coordinates size ;;

(** {v function_multicolor_smooth_1_1 gamma colors function frame_coordinates window_size v} *)
let function_multicolor_smooth_1_1 = fun (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_1 ( function_partial_smooth_1_1 gamma ) colors f coordinates size ;;

(** {v function_multicolor_oversample_1_1 over_sampling_factor gamma colors function frame_coordinates window_size v} *)
let function_multicolor_oversample_1_1 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_1 ( function_partial_oversample_1_1 n gamma ) colors f coordinates size ;;


(** {v function_grid_multi_paint_1_1 paint function frame_coordinates window_size v} *)
let function_grid_multi_paint_1_1 = fun paint (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
 grid coordinates size hh vv ;
 for i = 0 to r - 1 do
  paint f.(i) coordinates size ;
 done ;;

(** {v function_grid_multi_draw_1_1 function frame_coordinates window_size v} *)
let function_grid_multi_draw_1_1 = fun (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_grid_multi_paint_1_1 function_partial_draw_1_1 f coordinates size ;;

(** {v function_grid_multi_segment_1_1 function frame_coordinates window_size v} *)
let function_grid_multi_segment_1_1 = fun (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_grid_multi_paint_1_1 function_partial_segment_1_1 f coordinates size ;;

(** {v function_grid_multi_smooth_1_1 gamma function frame_coordinates window_size v} *)
let function_grid_multi_smooth_1_1 = fun (gamma:float -> float) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_grid_multi_paint_1_1 ( function_partial_smooth_1_1 gamma Graphics.white ) f coordinates size ;;

(** {v function_grid_multi_oversample_1_1 over_sampling_factor gamma function frame_coordinates window_size v} *)
let function_grid_multi_oversample_1_1 = fun (n:int) (gamma:float -> float) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_grid_multi_paint_1_1 ( function_partial_oversample_1_1 n gamma Graphics.white ) f coordinates size ;;


(** {v function_grid_multicolor_paint_1_1 paint colors function frame_coordinates window_size v} *)
let function_grid_multicolor_paint_1_1 = fun paint (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
 grid coordinates size hh vv ;
 for i = 0 to r - 1 do
  paint colors.(i) f.(i) coordinates size ;
 done ;;

(** {v function_grid_multicolor_draw_1_1 colors function frame_coordinates window_size v} *)
let function_grid_multicolor_draw_1_1 = fun (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 let paint = fun color g c s ->
  begin
   Graphics.set_color color ;
   function_partial_draw_1_1 g c s ;
  end in
  function_grid_multicolor_paint_1_1 paint colors f coordinates size ;;

(** {v function_grid_multicolor_segment_1_1 colors function frame_coordinates window_size v} *)
let function_grid_multicolor_segment_1_1 = fun (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 let paint = fun color g c s ->
  begin
   Graphics.set_color color ;
   function_partial_segment_1_1 g c s ;
  end in
  function_grid_multicolor_paint_1_1 paint colors f coordinates size ;;

(** {v function_grid_multicolor_smooth_1_1 gamma colors function frame_coordinates window_size v} *)
let function_grid_multicolor_smooth_1_1 = fun (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_grid_multicolor_paint_1_1 ( function_partial_smooth_1_1 gamma ) colors f coordinates size ;;

(** {v function_grid_multicolor_oversample_1_1 over_sampling_factor gamma colors function frame_coordinates window_size v} *)
let function_grid_multicolor_oversample_1_1 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float) array) (coordinates:float array) (size:int array) ->
 function_grid_multicolor_paint_1_1 ( function_partial_oversample_1_1 n gamma ) colors f coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_paint_1_1 paint vector frame_coordinates window_size v} *)
let discrete_paint_1_1 = fun paint (v:float array) (coordinates:float array) (size:int array) ->
 let scale = ( float ( Array.length v ) ) /. ( coordinates.(1) -. coordinates.(0) )
 and f = Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 v in
  let g = function x -> f ( ( x -. coordinates.(0) ) *. scale ) in
   paint g coordinates size ;;

(** {v discrete_draw_1_1 vector frame_coordinates window_size v} *)
let discrete_draw_1_1 = fun (v:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_1 function_draw_1_1 v coordinates size ;;

(** {v discrete_grid_draw_1_1 vector frame_coordinates window_size v} *)
let discrete_grid_draw_1_1 = fun (v:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_1 function_grid_draw_1_1 v coordinates size ;;

(** {v discrete_segment_1_1 vector frame_coordinates window_size v} *)
let discrete_segment_1_1 = fun (v:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_1 function_segment_1_1 v coordinates size ;;

(** {v discrete_grid_segment_1_1 vector frame_coordinates window_size v} *)
let discrete_grid_segment_1_1 = fun (v:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_1 function_grid_segment_1_1 v coordinates size ;;

(** {v discrete_smooth_1_1 gamma vector frame_coordinates window_size v} *)
let discrete_smooth_1_1 = fun (gamma:float -> float) (v:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_1 ( function_smooth_1_1 gamma ) v coordinates size ;;

(** {v discrete_grid_smooth_1_1 gamma vector frame_coordinates window_size v} *)
let discrete_grid_smooth_1_1 = fun (gamma:float -> float) (v:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_1 ( function_grid_smooth_1_1 gamma ) v coordinates size ;;

(** {v discrete_multi_paint_1_1 paint matrix frame_coordinates window_size v} *)
let discrete_multi_paint_1_1 = fun paint (v:float array array) (coordinates:float array) (size:int array) ->
 let scale = ( float ( Array.length v.(0) ) ) /. ( coordinates.(1) -. coordinates.(0) )
 and r = Array.length v in
  let g = Array.make r Util.float_zero in
   for i = 0 to r - 1 do
    let f = Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 v.(i) in
     g.(i) <- ( function x -> f ( ( x -. coordinates.(0) ) *. scale ) ) ;
   done ;
   paint g coordinates size ;;

(** {v discrete_multi_draw_1_1 matrix frame_coordinates window_size v} *)
let discrete_multi_draw_1_1 = fun (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_1 function_multi_draw_1_1 v coordinates size ;;

(** {v discrete_grid_multi_draw_1_1 matrix frame_coordinates window_size v} *)
let discrete_grid_multi_draw_1_1 = fun (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_1 function_grid_multi_draw_1_1 v coordinates size ;;

(** {v discrete_multi_segment_1_1 matrix frame_coordinates window_size v} *)
let discrete_multi_segment_1_1 = fun (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_1 function_multi_segment_1_1 v coordinates size ;;

(** {v discrete_grid_multi_segment_1_1 matrix frame_coordinates window_size v} *)
let discrete_grid_multi_segment_1_1 = fun (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_1 function_grid_multi_segment_1_1 v coordinates size ;;

(** {v discrete_multi_smooth_1_1 gamma matrix frame_coordinates window_size v} *)
let discrete_multi_smooth_1_1 = fun (gamma:float -> float) (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_1 ( function_multi_smooth_1_1 gamma ) v coordinates size ;;

(** {v discrete_grid_multi_smooth_1_1 gamma matrix frame_coordinates window_size v} *)
let discrete_grid_multi_smooth_1_1 = fun (gamma:float -> float) (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_1 ( function_grid_multi_smooth_1_1 gamma ) v coordinates size ;;


(** {v discrete_multicolor_paint_1_1 paint colors matrix frame_coordinates window_size v} *)
let discrete_multicolor_paint_1_1 = fun paint (colors:Graphics.color array) (v:float array array) (coordinates:float array) (size:int array) ->
 let scale = ( float ( Array.length v.(0) ) ) /. ( coordinates.(1) -. coordinates.(0) )
 and r = Array.length v in
  let g = Array.make r Util.float_zero in
   for i = 0 to r - 1 do
    let f = Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 v.(i) in
     g.(i) <- ( function x -> f ( ( x -. coordinates.(0) ) *. scale ) ) ;
   done ;
   paint colors g coordinates size ;;

(** {v discrete_multicolor_draw_1_1 colors matrix frame_coordinates window_size v} *)
let discrete_multicolor_draw_1_1 = fun (colors:Graphics.color array) (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_1 function_multicolor_draw_1_1 colors v coordinates size ;;

(** {v discrete_grid_multicolor_draw_1_1 colors matrix frame_coordinates window_size v} *)
let discrete_grid_multicolor_draw_1_1 = fun (colors:Graphics.color array) (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_1 function_grid_multicolor_draw_1_1 colors v coordinates size ;;

(** {v discrete_multicolor_segment_1_1 colors matrix frame_coordinates window_size v} *)
let discrete_multicolor_segment_1_1 = fun (colors:Graphics.color array) (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_1 function_multicolor_segment_1_1 colors v coordinates size ;;

(** {v discrete_grid_multicolor_segment_1_1 colors matrix frame_coordinates window_size v} *)
let discrete_grid_multicolor_segment_1_1 = fun (colors:Graphics.color array) (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_1 function_grid_multicolor_segment_1_1 colors v coordinates size ;;

(** {v discrete_multicolor_smooth_1_1 gamma colors matrix frame_coordinates window_size v} *)
let discrete_multicolor_smooth_1_1 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_1 ( function_multicolor_smooth_1_1 gamma ) colors v coordinates size ;;

(** {v discrete_grid_multicolor_smooth_1_1 gamma colors matrix frame_coordinates window_size v} *)
let discrete_grid_multicolor_smooth_1_1 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_1 (function_grid_multicolor_smooth_1_1 gamma ) colors v coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R dans R^2}
{1 Drawings from R to R^2}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v function_grid_paint_1_2 paint function interval nsteps frame_coordinates window_size v} *)
let function_grid_paint_1_2 = fun paint (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let ( hh , vv , h_margin , v_margin , t_function , x_function , y_function ) = prepare_1_2 nsteps coordinates size interval in
  grid coordinates size hh vv ;
  paint nsteps t_function h_margin v_margin x_function y_function f ;;

(** {v function_grid_draw_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_grid_draw_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_paint_1_2 ( curve_plot Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_grid_segment_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_grid_segment_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_paint_1_2 ( curve_segment Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_grid_smooth_1_2 gamma function interval nsteps frame_coordinates window_size v} *)
let function_grid_smooth_1_2 = fun (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_paint_1_2 ( curve_smooth gamma Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_grid_oversample_1_2 over_sampling_factor gamma function interval nsteps frame_coordinates window_size v} *)
let function_grid_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_paint_1_2 ( curve_oversample n gamma size Graphics.white ) f interval nsteps coordinates size ;;


(** {v function_paint_1_2 paint function interval nsteps frame_coordinates window_size v} *)
let function_paint_1_2 = fun paint (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let ( hh , vv , h_margin , v_margin , t_function , x_function , y_function ) = prepare_1_2 nsteps coordinates size interval in
  Graphics.set_color Graphics.white ;
  paint nsteps t_function h_margin v_margin x_function y_function f ;
  paint_coordinates coordinates size hh vv ;;

(** {v function_draw_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_draw_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_paint_1_2 ( curve_plot Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_segment_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_segment_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_paint_1_2 ( curve_segment Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_smooth_1_2 gamma function interval nsteps frame_coordinates window_size v} *)
let function_smooth_1_2 = fun (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
  function_paint_1_2 ( curve_smooth gamma Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_oversample_1_2 over_sampling_factor gamma function interval nsteps frame_coordinates window_size v} *)
let function_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
  function_paint_1_2 ( curve_oversample n gamma size Graphics.white ) f interval nsteps coordinates size ;;


(** {v function_partial_paint_1_2 paint function interval nsteps frame_coordinates window_size v} *)
let function_partial_paint_1_2 = fun paint (g:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2)
 and h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
 and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
  let h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
  and t_scale = ( interval.(1) -. interval.(0) ) /. ( float nsteps )
  and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real in
   let t_function = ( function z -> interval.(0) +. t_scale *. z )
   and x_function = ( function z -> ( z -. coordinates.(0) ) *. h_inv_scale ) 
   and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale ) in
    paint nsteps t_function h_margin v_margin x_function y_function g ;;

(** {v function_partial_draw_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_partial_draw_1_2 = fun (color:Graphics.color) (g:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_2 ( curve_plot color ) g interval nsteps coordinates size ;;

(** {v function_partial_segment_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_partial_segment_1_2 = fun (color:Graphics.color) (g:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_2 ( curve_segment color ) g interval nsteps coordinates size ;;

(** {v function_partial_smooth_1_2 gamma color function interval nsteps frame_coordinates window_size v} *)
let function_partial_smooth_1_2 = fun (gamma:float -> float) (color:Graphics.color) (g:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_2 ( curve_smooth gamma color ) g interval nsteps coordinates size ;;

(** {v function_partial_oversample_1_2 over_sampling_factor gamma color function interval nsteps frame_coordinates window_size v} *)
let function_partial_oversample_1_2 = fun (n:int) (gamma:float -> float) (color:Graphics.color) (g:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_2 ( curve_oversample n gamma size color ) g interval nsteps coordinates size ;;


(** {v function_color_draw_1_2 function color_function interval nsteps frame_coordinates window_size v} *)
let function_color_draw_1_2 = fun (g:float -> float array) (fc:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2)
 and h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
 and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
  let h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
  and t_scale = ( interval.(1) -. interval.(0) ) /. ( float nsteps )
  and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real in
   let t_function = ( function z -> interval.(0) +. t_scale *. z )
   and x_function = ( function z -> ( z -. coordinates.(0) ) *. h_inv_scale ) 
   and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale ) in
    for i = 0 to nsteps do
     let t = t_function ( float i ) in
      let zz = g t
      and real_color = fc t in
       let x = x_function ( zz.(0) )
       and y = y_function ( zz.(1) )
       and color = Matrix.int_of_vector real_color in
         Graphics.set_color ( Graphics.rgb color.(0) color.(1) color.(2) ) ;
         Graphics.plot ( ( int_of_float x ) + h_margin ) ( ( int_of_float y ) + v_margin ) ;
    done ;;

(** {v function_color_segment_1_2 function color_function interval nsteps frame_coordinates window_size v} *)
let function_color_segment_1_2 = fun (g:float -> float array) (fc:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2)
 and h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
 and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
  let h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
  and t_scale = ( interval.(1) -. interval.(0) ) /. ( float nsteps )
  and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real in
   let t_function = ( function z -> interval.(0) +. t_scale *. z )
   and x_function = ( function z -> ( z -. coordinates.(0) ) *. h_inv_scale ) 
   and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale ) in
    let z0 = g ( t_function 0. ) in
     Graphics.moveto ( h_margin + ( int_of_float ( x_function z0.(0) ) ) ) ( v_margin + ( int_of_float ( y_function z0.(1) ) ) ) ;
    for i = 1 to nsteps do
     let t = t_function ( float i ) in
      let zz = g t
      and real_color = fc t in
       let x = x_function ( zz.(0) )
       and y = y_function ( zz.(1) )
       and color = Matrix.int_of_vector real_color in
         Graphics.set_color ( Graphics.rgb color.(0) color.(1) color.(2) ) ;
         Graphics.lineto ( ( int_of_float x ) + h_margin ) ( ( int_of_float y ) + v_margin ) ;
    done ;;


(** {v function_multi_paint_1_2 paint function interval nsteps frame_coordinates window_size v} *)
let function_multi_paint_1_2 = fun paint (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  Graphics.set_color Graphics.white ;
  for i = 0 to r - 1 do
   paint f.(i) interval nsteps coordinates size ;
  done ;
  paint_coordinates coordinates size hh vv ;;

(** {v function_multi_draw_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_multi_draw_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_2 ( function_partial_draw_1_2 Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_multi_segment_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_multi_segment_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_2 ( function_partial_segment_1_2 Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_multi_smooth_1_2 gamma function interval nsteps frame_coordinates window_size v} *)
let function_multi_smooth_1_2 = fun (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_2 ( function_partial_smooth_1_2 gamma Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_multi_oversample_1_2 over_sampling_factor gamma function interval nsteps frame_coordinates window_size v} *)
let function_multi_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_2 ( function_partial_oversample_1_2 n gamma Graphics.white ) f interval nsteps coordinates size ;;


(** {v function_grid_multi_paint_1_2 paint function interval nsteps frame_coordinates window_size v} *)
let function_grid_multi_paint_1_2 = fun paint (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  grid coordinates size hh vv ;
  for i = 0 to r - 1 do
   paint f.(i) interval nsteps coordinates size ;
  done ;;

(** {v function_grid_multi_draw_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_grid_multi_draw_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_multi_paint_1_2 ( function_partial_draw_1_2 Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_grid_multi_segment_1_2 function interval nsteps frame_coordinates window_size v} *)
let function_grid_multi_segment_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_multi_paint_1_2 ( function_partial_segment_1_2 Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_grid_multi_smooth_1_2 gamma function interval nsteps frame_coordinates window_size v} *)
let function_grid_multi_smooth_1_2 = fun (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_multi_paint_1_2 ( function_partial_smooth_1_2 gamma Graphics.white ) f interval nsteps coordinates size ;;

(** {v function_grid_multi_oversample_1_2 over_sampling_factor gamma function interval nsteps frame_coordinates window_size v} *)
let function_grid_multi_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_multi_paint_1_2 ( function_partial_oversample_1_2 n gamma Graphics.white ) f interval nsteps coordinates size ;;


(** {v function_multicolor_paint_1_2 paint colors function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_paint_1_2 = fun paint (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  for i = 0 to r - 1 do
   paint colors.(i) f.(i) interval nsteps coordinates size ;
  done ;
  paint_coordinates coordinates size hh vv ;;

(** {v function_multicolor_draw_1_2 colors function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_draw_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_2 function_partial_draw_1_2 colors f interval nsteps coordinates size ;;

(** {v function_multicolor_segment_1_2 colors function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_segment_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_2 function_partial_segment_1_2 colors f interval nsteps coordinates size ;;

(** {v function_multicolor_smooth_1_2 gamma colors function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_2 ( function_partial_smooth_1_2 gamma ) colors f interval nsteps coordinates size ;;

(** {v function_multicolor_oversample_1_2 over_sampling_factor gamma colors function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_oversample_1_2 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_2 ( function_partial_oversample_1_2 n gamma ) colors f interval nsteps coordinates size ;;


(** {v function_grid_multicolor_paint_1_2 paint colors function interval nsteps frame_coordinates window_size v} *)
let function_grid_multicolor_paint_1_2 = fun paint (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  grid coordinates size hh vv ;
  for i = 0 to r - 1 do
   paint colors.(i) f.(i) interval nsteps coordinates size ;
  done ;;

(** {v function_grid_multicolor_draw_1_2 colors function interval nsteps frame_coordinates window_size v} *)
let function_grid_multicolor_draw_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_multicolor_paint_1_2 function_partial_draw_1_2 colors f interval nsteps coordinates size ;;

(** {v function_grid_multicolor_segment_1_2 colors function interval nsteps frame_coordinates window_size v} *)
let function_grid_multicolor_segment_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_multicolor_paint_1_2 function_partial_segment_1_2 colors f interval nsteps coordinates size ;;

(** {v function_grid_multicolor_smooth_1_2 gamma colors function interval nsteps frame_coordinates window_size v} *)
let function_grid_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_multicolor_paint_1_2 ( function_partial_smooth_1_2 gamma ) colors f interval nsteps coordinates size ;;

(** {v function_grid_multicolor_oversample_1_2 over_sampling_factor gamma colors function interval nsteps frame_coordinates window_size v} *)
let function_grid_multicolor_oversample_1_2 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_grid_multicolor_paint_1_2 ( function_partial_oversample_1_2 n gamma ) colors f interval nsteps coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_trans_paint_1_2 paint matrix interval frame_coordinates window_size v} *)
let discrete_trans_paint_1_2 = fun paint (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 let r = Array.length m.(0) in
  let scale = ( float r ) /. ( interval.(1) -. interval.(0) )
  and f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function x -> f ( ( x -. interval.(0) ) *. scale ) in
    paint g interval r coordinates size ;;

(** {v discrete_paint_1_2 paint matrix interval frame_coordinates window_size v} *)
let discrete_paint_1_2 = fun paint (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_2 paint ( Matrix.float_transpose m ) interval coordinates size ;;


(** {v discrete_trans_draw_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_trans_draw_1_2 = fun (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_2 function_draw_1_2 m interval coordinates size ;;

(** {v discrete_draw_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_draw_1_2 = fun (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_2 function_draw_1_2 m interval coordinates size ;;

(** {v discrete_trans_grid_draw_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_draw_1_2 = fun (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_2 function_grid_draw_1_2 m interval coordinates size ;;

(** {v discrete_grid_draw_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_grid_draw_1_2 = fun (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_2 function_grid_draw_1_2 m interval coordinates size ;;


(** {v discrete_trans_segment_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_trans_segment_1_2 = fun (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_2 function_segment_1_2 m interval coordinates size ;;

(** {v discrete_segment_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_segment_1_2 = fun (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_2 function_segment_1_2 m interval coordinates size ;;

(** {v discrete_trans_grid_segment_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_segment_1_2 = fun (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_2 function_grid_segment_1_2 m interval coordinates size ;;

(** {v discrete_grid_segment_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_grid_segment_1_2 = fun (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_2 function_grid_segment_1_2 m interval coordinates size ;;


(** {v discrete_trans_smooth_1_2 gamma matrix interval frame_coordinates window_size v} *)
let discrete_trans_smooth_1_2 = fun (gamma:float -> float) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_2 ( function_smooth_1_2 gamma ) m interval coordinates size ;;

(** {v discrete_smooth_1_2 gamma matrix interval frame_coordinates window_size v} *)
let discrete_smooth_1_2 = fun (gamma:float -> float) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_2 ( function_smooth_1_2 gamma ) m interval coordinates size ;;

(** {v discrete_trans_grid_smooth_1_2 gamma matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_smooth_1_2 = fun (gamma:float -> float) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_2 ( function_grid_smooth_1_2 gamma ) m interval coordinates size ;;

(** {v discrete_grid_smooth_1_2 gamma matrix interval frame_coordinates window_size v} *)
let discrete_grid_smooth_1_2 = fun (gamma:float -> float) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_2 ( function_grid_smooth_1_2 gamma ) m interval coordinates size ;;


(** {v discrete_multi_paint_1_2 paint matrix interval frame_coordinates window_size v} *)
let discrete_multi_paint_1_2 = fun paint (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 2 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint g interval nsteps coordinates size ;;

(** {v discrete_trans_multi_paint_1_2 paint matrix interval frame_coordinates window_size v} *)
let discrete_trans_multi_paint_1_2 = fun paint (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 2 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint g interval nsteps coordinates size ;;


(** {v discrete_multi_draw_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 function_multi_draw_1_2 v interval coordinates size ;;

(** {v discrete_trans_multi_draw_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_trans_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 function_multi_draw_1_2 v interval coordinates size ;;

(** {v discrete_grid_multi_draw_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_grid_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 function_grid_multi_draw_1_2 v interval coordinates size ;;

(** {v discrete_trans_grid_multi_draw_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 function_grid_multi_draw_1_2 v interval coordinates size ;;

(** {v discrete_multicolor_draw_1_2 colors matrix interval frame_coordinates window_size v} *)
let discrete_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 ( function_multicolor_draw_1_2 colors ) v interval coordinates size ;;

(** {v discrete_trans_multicolor_draw_1_2 colors matrix interval frame_coordinates window_size v} *)
let discrete_trans_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 ( function_multicolor_draw_1_2 colors ) v interval coordinates size ;;

(** {v discrete_grid_multicolor_draw_1_2 colors matrix interval frame_coordinates window_size v} *)
let discrete_grid_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 ( function_grid_multicolor_draw_1_2 colors ) v interval coordinates size ;;

(** {v discrete_trans_grid_multicolor_draw_1_2 colors matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 ( function_grid_multicolor_draw_1_2 colors ) v interval coordinates size ;;


(** {v discrete_multi_segment_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 function_multi_segment_1_2 v interval coordinates size ;;

(** {v discrete_trans_multi_segment_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_trans_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 function_multi_segment_1_2 v interval coordinates size ;;

(** {v discrete_grid_multi_segment_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_grid_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 function_grid_multi_segment_1_2 v interval coordinates size ;;

(** {v discrete_trans_grid_multi_segment_1_2 matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 function_grid_multi_segment_1_2 v interval coordinates size ;;

(** {v discrete_multicolor_segment_1_2 colors matrix interval frame_coordinates window_size v} *)
let discrete_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 ( function_multicolor_segment_1_2 colors ) v interval coordinates size ;;

(** {v discrete_trans_multicolor_segment_1_2 colors matrix interval frame_coordinates window_size v} *)
let discrete_trans_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 ( function_multicolor_segment_1_2 colors ) v interval coordinates size ;;

(** {v discrete_grid_multicolor_segment_1_2 colors matrix interval frame_coordinates window_size v} *)
let discrete_grid_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 ( function_grid_multicolor_segment_1_2 colors ) v interval coordinates size ;;

(** {v discrete_trans_grid_multicolor_segment_1_2 colors matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 ( function_grid_multicolor_segment_1_2 colors ) v interval coordinates size ;;


(** {v discrete_multi_smooth_1_2 gamma matrix interval frame_coordinates window_size v} *)
let discrete_multi_smooth_1_2 = fun (gamma:float -> float) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 ( function_multi_smooth_1_2 gamma ) v interval coordinates size ;;

(** {v discrete_trans_multi_smooth_1_2 gamma matrix interval frame_coordinates window_size v} *)
let discrete_trans_multi_smooth_1_2 = fun (gamma:float -> float) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 ( function_multi_smooth_1_2 gamma ) v interval coordinates size ;;

(** {v discrete_grid_multi_smooth_1_2 gamma matrix interval frame_coordinates window_size v} *)
let discrete_grid_multi_smooth_1_2 = fun (gamma:float -> float) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 ( function_grid_multi_smooth_1_2 gamma ) v interval coordinates size ;;

(** {v discrete_trans_grid_multi_smooth_1_2 gamma matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_multi_smooth_1_2 = fun (gamma:float -> float) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 ( function_grid_multi_smooth_1_2 gamma ) v interval coordinates size ;;

(** {v discrete_multicolor_smooth_1_2 gamma colors matrix interval frame_coordinates window_size v} *)
let discrete_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 ( function_multicolor_smooth_1_2 gamma colors ) v interval coordinates size ;;

(** {v discrete_trans_multicolor_smooth_1_2 gamma colors matrix interval frame_coordinates window_size v} *)
let discrete_trans_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 ( function_multicolor_smooth_1_2 gamma colors ) v interval coordinates size ;;

(** {v discrete_grid_multicolor_smooth_1_2 gamma colors matrix interval frame_coordinates window_size v} *)
let discrete_grid_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_2 ( function_grid_multicolor_smooth_1_2 gamma colors ) v interval coordinates size ;;

(** {v discrete_trans_grid_multicolor_smooth_1_2 gamma colors matrix interval frame_coordinates window_size v} *)
let discrete_trans_grid_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_2 ( function_grid_multicolor_smooth_1_2 gamma colors ) v interval coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R dans TR^2}
{1 Drawings from R to TR^2}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v paint_field arrow_paint nsteps nfeet t_function h_margin v_margin x_function y_function vx_function vy_function function v} *)
let paint_field = fun arrow_paint (nsteps:int) (nfeet:int) t_function (h_margin:int) (v_margin:int) x_function y_function vx_function vy_function (f:float -> float array) ->
 let ratio = float ( nsteps / nfeet ) in
  for i = 0 to nfeet do
   let t = t_function ( ratio *. ( float i ) ) in
    let z = f t in
     let x = x_function ( z.(0) )
     and y = y_function ( z.(1) )
     and xx = vx_function ( z.(2) )
     and yy = vy_function ( z.(3) ) in
      arrow arrow_paint ( ( Util.round x ) + h_margin ) ( ( Util.round y ) + v_margin ) ( Util.round xx ) ( Util.round yy ) 
  done ;;


(** {v field_grid_paint_1_2 paint arrow_paint function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_paint_1_2 = fun paint arrow_paint (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ( hh , vv , h_margin , v_margin , t_function , x_function , y_function , vx_function , vy_function ) = prepare_field_1_2 nsteps coordinates size interval in
  grid coordinates size hh vv ;
  paint_field arrow_paint nsteps nfeet t_function h_margin v_margin x_function y_function vx_function vy_function f ;
  paint nsteps t_function h_margin v_margin x_function y_function f ;;

(** {v field_grid_draw_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_draw_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.green ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_grid_paint_1_2 ( curve_plot Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_grid_segment_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_segment_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.green ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_grid_paint_1_2 ( curve_segment Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_grid_smooth_1_2 gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_smooth_1_2 = fun (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_grid_paint_1_2 ( curve_smooth gamma Graphics.white ) ( draw_smooth_segment gamma Graphics.green ) f interval nsteps nfeet coordinates size ;;

(** {v field_grid_oversample_1_2 over_sampling_factor gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_grid_paint_1_2 ( curve_oversample n gamma size Graphics.white ) ( draw_smooth_segment gamma Graphics.green ) f interval nsteps nfeet coordinates size ;;


(** {v field_paint_1_2 paint arrow_paint function interval nsteps nfeet frame_coordinates window_size v} *)
let field_paint_1_2 = fun paint arrow_paint (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ( hh , vv , h_margin , v_margin , t_function , x_function , y_function , vx_function , vy_function ) = prepare_field_1_2 nsteps coordinates size interval in
  paint_coordinates coordinates size hh vv ;
  let ratio = float ( nsteps / nfeet ) in
   for i = 0 to nfeet do
    let t = t_function ( ratio *. ( float i ) ) in
     let z = f t in
      let x = x_function ( z.(0) )
      and y = y_function ( z.(1) )
      and xx = vx_function ( z.(2) )
      and yy = vy_function ( z.(3) ) in
       arrow arrow_paint ( ( Util.round x ) + h_margin ) ( ( Util.round y ) + v_margin ) ( Util.round xx ) ( Util.round yy ) 
   done ;
   paint nsteps t_function h_margin v_margin x_function y_function f ;;

(** {v field_draw_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let field_draw_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.green ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_paint_1_2 ( curve_plot Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_segment_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let field_segment_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.green ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_paint_1_2 ( curve_segment Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_smooth_1_2 gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let field_smooth_1_2 = fun (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_paint_1_2 ( curve_smooth gamma Graphics.white ) ( draw_smooth_segment gamma Graphics.green ) f interval nsteps nfeet coordinates size ;;

(** {v field_oversample_1_2 over_sampling_factor gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let field_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_paint_1_2 ( curve_oversample n gamma size Graphics.white ) ( draw_smooth_segment gamma Graphics.green ) f interval nsteps nfeet coordinates size ;;


(** {v field_partial_paint_1_2 paint arrow_paint function interval nsteps frame_coordinates window_size v} *)
let field_partial_paint_1_2 = fun paint arrow_paint (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
   and t_scale = ( interval.(1) -. interval.(0) ) /. ( float nsteps )
   and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real in
    let t_function = ( function z -> interval.(0) +. t_scale *. z )
    and x_function = ( function z -> ( z -. coordinates.(0) ) *. h_inv_scale ) 
    and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale )
    and vx_function = ( function z -> z *. h_inv_scale ) 
    and vy_function = ( function z -> z *. v_inv_scale ) in
     let ratio = float ( nsteps / nfeet ) in
      for i = 0 to nfeet do
       let t = t_function ( ratio *. ( float i ) ) in
        let z = f t in
         let x = x_function ( z.(0) )
         and y = y_function ( z.(1) )
         and xx = vx_function ( z.(2) )
         and yy = vy_function ( z.(3) ) in
          arrow arrow_paint ( ( Util.round x ) + h_margin ) ( ( Util.round y ) + v_margin ) ( Util.round xx ) ( Util.round yy ) 
       done ;
      paint nsteps t_function h_margin v_margin x_function y_function f ;;


(** {v field_partial_draw_1_2 color1 color2 function interval nsteps frame_coordinates window_size v} *)
let field_partial_draw_1_2 = fun (color1:Graphics.color) (color2:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color color2 ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_partial_paint_1_2 ( curve_plot color1 ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_partial_segment_1_2 color1 color2 function interval nsteps frame_coordinates window_size v} *)
let field_partial_segment_1_2 = fun (color1:Graphics.color) (color2:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color color2 ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_partial_paint_1_2 ( curve_segment color1 ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_partial_smooth_1_2 gamma color1 color2 function interval nsteps frame_coordinates window_size v} *)
let field_partial_smooth_1_2 = fun (gamma:float -> float) (color1:Graphics.color) (color2:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_partial_paint_1_2 ( curve_smooth gamma color1 ) ( draw_smooth_segment gamma color2 ) f interval nsteps nfeet coordinates size ;;

(** {v field_partial_oversample_1_2 over_sampling_factor gamma color1 color2 function interval nsteps frame_coordinates window_size v} *)
let field_partial_oversample_1_2 = fun (n:int) (gamma:float -> float) (color1:Graphics.color) (color2:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_partial_paint_1_2 ( curve_oversample n gamma size color1 ) ( draw_smooth_segment gamma color2 ) f interval nsteps nfeet coordinates size ;;


(** {v field_grid_multi_paint_1_2 paint arrow_paint function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multi_paint_1_2 = fun paint arrow_paint (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  grid coordinates size hh vv ;
  for i = 0 to r - 1 do
   field_partial_paint_1_2 paint arrow_paint f.(i) interval nsteps nfeet coordinates size ;
  done ;;

(** {v field_grid_multi_draw_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multi_draw_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.green ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
 field_grid_multi_paint_1_2 ( curve_plot Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_grid_multi_segment_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multi_segment_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.green ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
 field_grid_multi_paint_1_2 ( curve_segment Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_grid_multi_smooth_1_2 gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multi_smooth_1_2 = fun (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_grid_multi_paint_1_2 ( curve_smooth gamma Graphics.white ) ( draw_smooth_segment gamma Graphics.green ) f interval nsteps nfeet coordinates size ;;

(** {v field_grid_multi_oversample_1_2 over_sampling_factor gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multi_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_grid_multi_paint_1_2 ( curve_oversample n gamma size Graphics.white ) ( draw_smooth_segment gamma Graphics.green ) f interval nsteps nfeet coordinates size ;;


(** {v field_grid_multicolor_paint_1_2 paint arrow_paint colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multicolor_paint_1_2 = fun paint arrow_paint (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  grid coordinates size hh vv ;
  for i = 0 to r - 1 do
   field_partial_paint_1_2 ( paint colors.(i) ) ( arrow_paint ( soft_color colors.(i) ) ) f.(i) interval nsteps nfeet coordinates size ;
  done ;;

(** {v field_grid_multicolor_draw_1_2 colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multicolor_draw_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_grid_multicolor_paint_1_2 curve_plot ap colors f interval nsteps nfeet coordinates size ;;

(** {v field_grid_multicolor_segment_1_2 colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multicolor_segment_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_grid_multicolor_paint_1_2 curve_segment ap colors f interval nsteps nfeet coordinates size ;;

(** {v field_grid_multicolor_smooth_1_2 gamma colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_grid_multicolor_paint_1_2 ( curve_smooth gamma ) ( draw_smooth_segment gamma ) colors f interval nsteps nfeet coordinates size ;;

(** {v field_grid_multicolor_oversample_1_2 over_sampling_factor gamma colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_grid_multicolor_oversample_1_2 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_grid_multicolor_paint_1_2 ( curve_oversample n gamma size ) ( draw_smooth_segment gamma ) colors f interval nsteps nfeet coordinates size ;;


(** {v field_multi_paint_1_2 paint arrow_paint function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_paint_1_2 = fun paint arrow_paint (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  paint_coordinates coordinates size hh vv ;
  for i = 0 to r - 1 do
   field_partial_paint_1_2 paint arrow_paint f.(i) interval nsteps nfeet coordinates size ;
  done ;;

(** {v field_multi_draw_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_draw_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.green ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
 field_multi_paint_1_2 ( curve_plot Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_multi_segment_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_segment_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.green ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
 field_multi_paint_1_2 ( curve_segment Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v field_multi_smooth_1_2 gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_smooth_1_2 = fun (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multi_paint_1_2 ( curve_smooth gamma Graphics.white ) ( draw_smooth_segment gamma Graphics.green ) f interval nsteps nfeet coordinates size ;;

(** {v field_multi_oversample_1_2 over_sampling_factor gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multi_paint_1_2 ( curve_oversample n gamma size Graphics.white ) ( draw_smooth_segment gamma Graphics.green ) f interval nsteps nfeet coordinates size ;;


(** {v field_multicolor_paint_1_2 paint arrow_paint colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_paint_1_2 = fun paint arrow_paint (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  paint_coordinates coordinates size hh vv ;
  for i = 0 to r - 1 do
   field_partial_paint_1_2 ( paint colors.(i) ) ( arrow_paint ( soft_color colors.(i) ) ) f.(i) interval nsteps nfeet coordinates size ;
  done ;;

(** {v field_multicolor_draw_1_2 colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_draw_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_multicolor_paint_1_2 curve_plot ap colors f interval nsteps nfeet coordinates size ;;

(** {v field_multicolor_segment_1_2 colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_segment_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
 field_multicolor_paint_1_2 curve_segment ap colors f interval nsteps nfeet coordinates size ;;

(** {v field_multicolor_smooth_1_2 gamma colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multicolor_paint_1_2 ( curve_smooth gamma ) ( draw_smooth_segment gamma ) colors f interval nsteps nfeet coordinates size ;;

(** {v field_multicolor_oversample_1_2 over_sampling_factor gamma colors function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_oversample_1_2 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multicolor_paint_1_2 ( curve_oversample n gamma size ) ( draw_smooth_segment gamma ) colors f interval nsteps nfeet coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_field_trans_paint_1_2 paint matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_paint_1_2 = fun paint (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length m.(0) in
  let scale = ( float r ) /. ( interval.(1) -. interval.(0) )
  and f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function x -> f ( ( x -. interval.(0) ) *. scale ) in
    paint g interval r nfeet coordinates size ;;

(** {v discrete_field_paint_1_2 paint matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_paint_1_2 = fun paint (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_2 paint ( Matrix.float_transpose m ) interval nfeet coordinates size ;;


(** {v discrete_field_trans_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_draw_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_2 field_draw_1_2 m interval nfeet coordinates size ;;

(** {v discrete_field_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_draw_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_2 field_draw_1_2 m interval nfeet coordinates size ;;

(** {v discrete_field_trans_grid_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_draw_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_2 field_grid_draw_1_2 m interval nfeet coordinates size ;;

(** {v discrete_field_grid_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_draw_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_2 field_grid_draw_1_2 m interval nfeet coordinates size ;;


(** {v discrete_field_trans_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_segment_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_2 field_segment_1_2 m interval nfeet coordinates size ;;

(** {v discrete_field_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_segment_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_2 field_segment_1_2 m interval nfeet coordinates size ;;

(** {v discrete_field_trans_grid_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_segment_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_2 field_grid_segment_1_2 m interval nfeet coordinates size ;;

(** {v discrete_field_grid_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_segment_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_2 field_grid_segment_1_2 m interval nfeet coordinates size ;;


(** {v discrete_field_trans_smooth_1_2 gamma matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_smooth_1_2 = fun (gamma:float -> float) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_2 ( field_smooth_1_2 gamma ) m interval nfeet coordinates size ;;

(** {v discrete_field_smooth_1_2 gamma matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_smooth_1_2 = fun (gamma:float -> float) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_2 ( field_smooth_1_2 gamma ) m interval nfeet coordinates size ;;

(** {v discrete_field_trans_grid_smooth_1_2 gamma matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_smooth_1_2 = fun (gamma:float -> float) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_2 ( field_grid_smooth_1_2 gamma ) m interval nfeet coordinates size ;;

(** {v discrete_field_grid_smooth_1_2 gamma matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_smooth_1_2 = fun (gamma:float -> float) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_2 ( field_grid_smooth_1_2 gamma ) m interval nfeet coordinates size ;;


(** {v discrete_field_trans_multi_paint_1_2 paint matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multi_paint_1_2 = fun paint (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 4 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint g interval nsteps nfeet coordinates size ;;

(** {v discrete_field_multi_paint_1_2 paint matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multi_paint_1_2 = fun paint (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let m = Array.map Matrix.float_transpose v in
  discrete_field_trans_multi_paint_1_2 paint m interval nfeet coordinates size ;;


(** {v discrete_field_trans_grid_multi_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 field_grid_multi_draw_1_2 v interval nfeet coordinates size ;;

(** {v discrete_field_grid_multi_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 field_grid_multi_draw_1_2 v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multi_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 field_multi_draw_1_2 v interval nfeet coordinates size ;;

(** {v discrete_field_multi_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 field_multi_draw_1_2 v interval nfeet coordinates size ;;


(** {v discrete_field_trans_grid_multi_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 field_grid_multi_segment_1_2 v interval nfeet coordinates size ;;

(** {v discrete_field_grid_multi_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 field_grid_multi_segment_1_2 v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multi_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 field_multi_segment_1_2 v interval nfeet coordinates size ;;

(** {v discrete_field_multi_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 field_multi_segment_1_2 v interval nfeet coordinates size ;;


(** {v discrete_field_trans_grid_multi_smooth_1_2 gamma matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_multi_smooth_1_2 = fun (gamma:float -> float) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 ( field_grid_multi_smooth_1_2 gamma )  v interval nfeet coordinates size ;;

(** {v discrete_field_grid_multi_smooth_1_2 gamma matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_multi_smooth_1_2 = fun (gamma:float -> float) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 ( field_grid_multi_smooth_1_2 gamma ) v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multi_smooth_1_2 gamma matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multi_smooth_1_2 = fun (gamma:float -> float) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 ( field_multi_smooth_1_2 gamma ) v interval nfeet coordinates size ;;

(** {v discrete_field_multi_smooth_1_2 gamma matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multi_smooth_1_2 = fun (gamma:float -> float) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 ( field_multi_smooth_1_2 gamma ) v interval nfeet coordinates size ;;


(** {v discrete_field_trans_grid_multicolor_draw_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 ( field_grid_multicolor_draw_1_2 colors ) v interval nfeet coordinates size ;;

(** {v discrete_field_grid_multicolor_draw_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 ( field_grid_multicolor_draw_1_2 colors ) v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multicolor_draw_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 ( field_multicolor_draw_1_2 colors ) v interval nfeet coordinates size ;;

(** {v discrete_field_multicolor_draw_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 ( field_multicolor_draw_1_2 colors ) v interval nfeet coordinates size ;;


(** {v discrete_field_trans_grid_multicolor_segment_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 ( field_grid_multicolor_segment_1_2 colors ) v interval nfeet coordinates size ;;

(** {v discrete_field_grid_multicolor_segment_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 ( field_grid_multicolor_segment_1_2 colors ) v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multicolor_segment_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 ( field_multicolor_segment_1_2 colors ) v interval nfeet coordinates size ;;

(** {v discrete_field_multicolor_segment_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 ( field_multicolor_segment_1_2 colors ) v interval nfeet coordinates size ;;


(** {v discrete_field_trans_grid_multicolor_smooth_1_2 gamma colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_grid_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 ( field_grid_multicolor_smooth_1_2 gamma colors )  v interval nfeet coordinates size ;;

(** {v discrete_field_grid_multicolor_smooth_1_2 gamma colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_grid_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 ( field_grid_multicolor_smooth_1_2 gamma colors ) v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multicolor_smooth_1_2 gamma colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_2 ( field_multicolor_smooth_1_2 gamma colors ) v interval nfeet coordinates size ;;

(** {v discrete_field_multicolor_smooth_1_2 gamma colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_2 ( field_multicolor_smooth_1_2 gamma colors ) v interval nfeet coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R dans R^n ¤ TR^2}
{1 Drawings from R to R^n ¤ TR^2}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v paint_frame arrow_paint nsteps nfeet t_function h_margin v_margin x_function y_function vx_function vy_function function interval v} *)
let paint_frame = fun arrow_paint (nsteps:int) (nfeet:int) t_function (h_margin:int) (v_margin:int) x_function y_function vx_function vy_function (f:float -> float array) (interval:float array) ->
 let r = Array.length ( f interval.(0) )
 and ratio = float ( nsteps / nfeet ) in
  for i = 0 to nfeet do
   let t = t_function ( ratio *. ( float i ) ) in
    let z = f t in
     let x = x_function ( z.(0) )
     and y = y_function ( z.(1) ) in
      for j = 1 to ( r - 1 ) / 2 do
       let jj = 2 * j in
        let xx = vx_function ( z.(jj) )
        and yy = vy_function ( z.( jj + 1 ) ) in
         arrow ( arrow_paint color_list.( pred j mod 5 ) ) ( ( Util.round x ) + h_margin ) ( ( Util.round y ) + v_margin ) ( Util.round xx ) ( Util.round yy ) 
      done ;
  done ;;


(** {v frame_field_partial_paint_1_2 paint arrow_paint color function interval nsteps frame_coordinates window_size v} *)
let frame_field_partial_paint_1_2 = fun paint arrow_paint (color:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let h_real = coordinates.(1) -. coordinates.(0)
 and v_real = coordinates.(3) -. coordinates.(2) in
  let h_margin = max ( size.(0) / 10 ) ( 10 * tsh )
  and v_margin = max ( size.(1) / 10 ) ( 4 * tsv ) in
   let h_inv_scale = ( float ( size.(0) - 2 * h_margin ) ) /. h_real
   and t_scale = ( interval.(1) -. interval.(0) ) /. ( float nsteps )
   and v_inv_scale = ( float ( size.(1) - 2 * v_margin ) ) /. v_real in
    let t_function = ( function z -> interval.(0) +. t_scale *. z )
    and x_function = ( function z -> ( z -. coordinates.(0) ) *. h_inv_scale ) 
    and y_function = ( function z -> ( z -. coordinates.(2) ) *. v_inv_scale )
    and vx_function = ( function z -> z *. h_inv_scale ) 
    and vy_function = ( function z -> z *. v_inv_scale ) in
      paint_frame arrow_paint nsteps nfeet t_function h_margin v_margin x_function y_function vx_function vy_function f interval ;
      paint color nsteps t_function h_margin v_margin x_function y_function f ;;

(** {v frame_field_partial_draw_1_2 color function interval nsteps frame_coordinates window_size v} *)
let frame_field_partial_draw_1_2 = fun (color:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun couleur a b c d ->
  begin
   Graphics.set_color couleur ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_partial_paint_1_2 curve_plot ap color f interval nsteps nfeet coordinates size ;;

(** {v frame_field_partial_segment_1_2 color function interval nsteps frame_coordinates window_size v} *)
let frame_field_partial_segment_1_2 = fun (color:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun couleur a b c d ->
  begin
   Graphics.set_color couleur ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_partial_paint_1_2 curve_segment ap color f interval nsteps nfeet coordinates size ;;

(** {v frame_field_partial_smooth_1_2 gamma color function interval nsteps frame_coordinates window_size v} *)
let frame_field_partial_smooth_1_2 = fun (gamma:float -> float) (color:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
  frame_field_partial_paint_1_2 ( curve_smooth gamma ) ( draw_smooth_segment gamma ) color f interval nsteps nfeet coordinates size ;;

(** {v frame_field_partial_oversample_1_2 over_sampling_factor gamma color function interval nsteps frame_coordinates window_size v} *)
let frame_field_partial_oversample_1_2 = fun (n:int) (gamma:float -> float) (color:Graphics.color) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
  frame_field_partial_paint_1_2 ( curve_oversample n gamma size ) ( draw_smooth_segment gamma ) color f interval nsteps nfeet coordinates size ;;


(** {v frame_field_grid_paint_1_2 paint arrow_paint function interval nsteps frame_coordinates window_size v} *)
let frame_field_grid_paint_1_2 = fun paint arrow_paint (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ( hh , vv , h_margin , v_margin , t_function , x_function , y_function , vx_function , vy_function ) = prepare_field_1_2 nsteps coordinates size interval in
  grid coordinates size hh vv ;
  paint_frame arrow_paint nsteps nfeet t_function h_margin v_margin x_function y_function vx_function vy_function f interval ;
  paint nsteps t_function h_margin v_margin x_function y_function f ;;

(** {v frame_field_grid_draw_1_2 function interval nsteps frame_coordinates window_size v} *)
let frame_field_grid_draw_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_grid_paint_1_2 ( curve_plot Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_segment_1_2 function interval nsteps frame_coordinates window_size v} *)
let frame_field_grid_segment_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_grid_paint_1_2 ( curve_segment Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_smooth_1_2 gamma function interval nsteps frame_coordinates window_size v} *)
let frame_field_grid_smooth_1_2 = fun (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_grid_paint_1_2 ( curve_smooth gamma Graphics.white ) ( draw_smooth_segment gamma ) f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_oversample_1_2 over_sampling_factor gamma function interval nsteps frame_coordinates window_size v} *)
let frame_field_grid_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_grid_paint_1_2 ( curve_oversample n gamma size Graphics.white ) ( draw_smooth_segment gamma ) f interval nsteps nfeet coordinates size ;;


(** {v frame_field_paint_1_2 paint arrow_paint function interval nsteps frame_coordinates window_size v} *)
let frame_field_paint_1_2 = fun paint arrow_paint (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ( hh , vv , h_margin , v_margin , t_function , x_function , y_function , vx_function , vy_function ) = prepare_field_1_2 nsteps coordinates size interval in
  paint_coordinates coordinates size hh vv ;
  paint_frame arrow_paint nsteps nfeet t_function h_margin v_margin x_function y_function vx_function vy_function f interval ;
  paint nsteps t_function h_margin v_margin x_function y_function f ;;

(** {v frame_field_draw_1_2 function interval nsteps frame_coordinates window_size v} *)
let frame_field_draw_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_paint_1_2 ( curve_plot Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v frame_field_segment_1_2 function interval nsteps frame_coordinates window_size v} *)
let frame_field_segment_1_2 = fun (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_paint_1_2 ( curve_segment Graphics.white ) ap f interval nsteps nfeet coordinates size ;;

(** {v frame_field_smooth_1_2 gamma function interval nsteps frame_coordinates window_size v} *)
let frame_field_smooth_1_2 = fun (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_1_2 ( curve_smooth gamma Graphics.white ) ( draw_smooth_segment gamma ) f interval nsteps nfeet coordinates size ;;

(** {v frame_field_oversample_1_2 over_sampling_factor gamma function interval nsteps frame_coordinates window_size v} *)
let frame_field_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_1_2 ( curve_oversample n gamma size Graphics.white ) ( draw_smooth_segment gamma ) f interval nsteps nfeet coordinates size ;;


(** {v frame_field_grid_multi_paint_1_2 paint arrow_paint function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multi_paint_1_2 = fun paint arrow_paint (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  grid coordinates size hh vv ;
  for i = 0 to r - 1 do
   frame_field_partial_paint_1_2 paint arrow_paint Graphics.white f.(i) interval nsteps nfeet coordinates size ;
  done ;;

(** {v frame_field_grid_multi_draw_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multi_draw_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun couleur a b c d ->
  begin
   Graphics.set_color couleur ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_grid_multi_paint_1_2 curve_plot ap f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_multi_segment_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multi_segment_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun couleur a b c d ->
  begin
   Graphics.set_color couleur ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_grid_multi_paint_1_2 curve_segment ap f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_multi_smooth_1_2 gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multi_smooth_1_2 = fun (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_grid_multi_paint_1_2 ( curve_smooth gamma ) ( draw_smooth_segment gamma ) f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_multi_oversample_1_2 over_sampling_factor gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multi_oversample_1_2 = fun (n:int) (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_grid_multi_paint_1_2 ( curve_oversample n gamma size ) ( draw_smooth_segment gamma ) f interval nsteps nfeet coordinates size ;;


(** {v frame_field_multi_paint_1_2 paint arrow_paint function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_paint_1_2 = fun paint arrow_paint (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  paint_coordinates coordinates size hh vv ;
  for i = 0 to r - 1 do
   frame_field_partial_paint_1_2 paint arrow_paint Graphics.white f.(i) interval nsteps nfeet coordinates size ;
  done ;;

(** {v frame_field_multi_draw_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_draw_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_multi_paint_1_2 curve_plot ap f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multi_segment_1_2 function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_segment_1_2 = fun (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_multi_paint_1_2 curve_segment ap f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multi_smooth_1_2 gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_smooth_1_2 = fun (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multi_paint_1_2 ( curve_smooth gamma ) ( draw_smooth_segment gamma ) f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multi_oversample_1_2 over_sampling_factor gamma function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_overample_1_2 = fun (n:int) (gamma:float -> float) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multi_paint_1_2 ( curve_oversample n gamma size ) ( draw_smooth_segment gamma ) f interval nsteps nfeet coordinates size ;;


(** {v frame_field_grid_multicolor_paint_1_2 paint arrow_paint colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multicolor_paint_1_2 = fun paint arrow_paint (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv ) = prep_1_1 coordinates size in
  grid coordinates size hh vv ;
  for i = 0 to r - 1 do
   frame_field_partial_paint_1_2 paint arrow_paint colors.(i) f.(i) interval nsteps nfeet coordinates size ;
  done ;;

(** {v frame_field_grid_multicolor_draw_1_2 colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multicolor_draw_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_grid_multicolor_paint_1_2 curve_plot ap colors f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_multicolor_segment_1_2 colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multicolor_segment_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_grid_multicolor_paint_1_2 curve_segment ap colors f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_multicolor_smooth_1_2 gamma colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_grid_multicolor_paint_1_2 ( curve_smooth gamma ) ( draw_smooth_segment gamma ) colors f interval nsteps nfeet coordinates size ;;

(** {v frame_field_grid_multicolor_oversample_1_2 over_sampling_factor gamma colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_grid_multicolor_oversample_1_2 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_grid_multicolor_paint_1_2 ( curve_oversample n gamma size ) ( draw_smooth_segment gamma ) colors f interval nsteps nfeet coordinates size ;;


(** {v frame_field_multicolor_paint_1_2 paint arrow_paint colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_paint_1_2 = fun paint arrow_paint (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( hh , vv , h_margin , v_margin , x_function , y_function ) = prepare_1_1 coordinates size in
  paint_coordinates coordinates size hh vv ;
  for i = 0 to r - 1 do
   frame_field_partial_paint_1_2 paint arrow_paint colors.(i) f.(i) interval nsteps nfeet coordinates size ;
  done ;;

(** {v frame_field_multicolor_draw_1_2 colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_draw_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun couleur a b c d ->
  begin
   Graphics.set_color couleur ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_multicolor_paint_1_2 curve_plot ap colors f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multicolor_segment_1_2 colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_segment_1_2 = fun (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ap = fun couleur a b c d ->
  begin
   Graphics.set_color couleur ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_multicolor_paint_1_2 curve_segment ap colors f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multicolor_smooth_1_2 gamma colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_smooth_1_2 = fun (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multicolor_paint_1_2 ( curve_smooth gamma ) ( draw_smooth_segment gamma ) colors f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multicolor_oversample_1_2 over_sampling_factor gamma colors function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_oversample_1_2 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multicolor_paint_1_2 ( curve_oversample n gamma size ) ( draw_smooth_segment gamma ) colors f interval nsteps nfeet coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)











(** {v discrete_frame_field_trans_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_draw_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length m.(0) in
  let scale = ( float r ) /. ( interval.(1) -. interval.(0) )
  and f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function x -> f ( ( x -. interval.(0) ) *. scale ) in
    frame_field_draw_1_2 g interval r nfeet coordinates size ;;

(** {v discrete_frame_field_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_draw_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_draw_1_2 ( Matrix.float_transpose m ) interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multi_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multi_draw_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 4 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    frame_field_multi_draw_1_2 g interval nsteps nfeet coordinates size ;;

(** {v discrete_frame_field_multi_draw_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multi_draw_1_2 = fun (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multi_draw_1_2 ( Array.map Matrix.float_transpose m ) interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multicolor_draw_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multicolor_draw_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 4 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    frame_field_multicolor_draw_1_2 colors g interval nsteps nfeet coordinates size ;;

(** {v discrete_frame_field_multicolor_draw_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multicolor_draw_1_2 = fun (colors:Graphics.color array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multicolor_draw_1_2 colors ( Array.map Matrix.float_transpose m ) interval nfeet coordinates size ;;


(** {v discrete_frame_field_trans_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_segment_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length m.(0) in
  let scale = ( float r ) /. ( interval.(1) -. interval.(0) )
  and f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function x -> f ( ( x -. interval.(0) ) *. scale ) in
    frame_field_segment_1_2 g interval r nfeet coordinates size ;;

(** {v discrete_frame_field_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_segment_1_2 = fun (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_segment_1_2 ( Matrix.float_transpose m ) interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multi_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multi_segment_1_2 = fun (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 4 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    frame_field_multi_segment_1_2 g interval nsteps nfeet coordinates size ;;

(** {v discrete_frame_field_multi_segment_1_2 matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multi_segment_1_2 = fun (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multi_segment_1_2 ( Array.map Matrix.float_transpose m ) interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multicolor_segment_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multicolor_segment_1_2 = fun (colors:Graphics.color array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 4 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    frame_field_multicolor_segment_1_2 colors g interval nsteps nfeet coordinates size ;;

(** {v discrete_frame_field_multicolor_segment_1_2 colors matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multicolor_segment_1_2 = fun (colors:Graphics.color array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multicolor_segment_1_2 colors ( Array.map Matrix.float_transpose m ) interval nfeet coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R dans R^3}
{1 Drawings from R to R^3}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v function_paint_1_3 paint axes_paint vector function interval nsteps frame_coordinates window_size v} *)
let function_paint_1_3 = fun paint axes_paint (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
  let g = project_1_3 accu a b u v w nsteps coordinates coo f in
   axes_paint v w accu coo size ;
   paint Graphics.white g interval nsteps coo size ;
   terminate_1_3 accu hh ;;

(** {v function_draw_1_3 vector function interval nsteps frame_coordinates window_size v} *)
let function_draw_1_3 = fun (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_paint_1_3 function_partial_draw_1_2 ( draw_axes Graphics.yellow ) u f interval nsteps coordinates size ;;

(** {v function_segment_1_3 vector function interval nsteps frame_coordinates window_size v} *)
let function_segment_1_3 = fun (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_paint_1_3 function_partial_segment_1_2 ( draw_axes Graphics.yellow ) u f interval nsteps coordinates size ;;

(** {v function_segment_1_3 vector function interval nsteps frame_coordinates window_size v} *)
let function_segment_1_3 = fun (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_paint_1_3 function_partial_segment_1_2 ( draw_axes Graphics.yellow ) u f interval nsteps coordinates size ;;

(** {v function_smooth_1_3 gamma vector function interval nsteps frame_coordinates window_size v} *)
let function_smooth_1_3 = fun (gamma:float -> float) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_paint_1_3 ( function_partial_smooth_1_2 gamma ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps coordinates size ;;

(** {v function_oversample _1_3 over_sampling_factor gamma vector function interval nsteps frame_coordinates window_size v} *)
let function_oversample_1_3 = fun (n:int) (gamma:float -> float) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_paint_1_3 ( function_partial_oversample_1_2 n gamma ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps coordinates size ;;


(** {v function_partial_paint_1_3 paint vector function interval nsteps frame_coordinates window_size v} *)
let function_partial_paint_1_3 = fun paint (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let m = Matrix.vector_float_chose_3 u in
  let v = m.(1)
  and w = m.(2) in
   let g = function t -> [| Matrix.vector_float_scal_prod ( f t ) v ; Matrix.vector_float_scal_prod ( f t ) w |]
   and coo = box_proj_3_2 u coordinates in
    paint g interval nsteps coo size ;;

(** {v function_partial_draw_1_3 color vector function interval nsteps frame_coordinates window_size v} *)
let function_partial_draw_1_3 = fun (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_3 ( function_partial_draw_1_2 color ) u f interval nsteps coordinates size ;;

(** {v function_partial_segment_1_3 color vector function interval nsteps frame_coordinates window_size v} *)
let function_partial_segment_1_3 = fun (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_3 ( function_partial_segment_1_2 color ) u f interval nsteps coordinates size ;;

(** {v function_partial_smooth_1_3 gamma color vector function interval nsteps frame_coordinates window_size v} *)
let function_partial_smooth_1_3 = fun (gamma:float -> float) (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_3 ( function_partial_smooth_1_2 gamma color ) u f interval nsteps coordinates size ;;

(** {v function_partial_oversample_1_3 over_sampling_factor gamma color vector function interval nsteps frame_coordinates window_size v} *)
let function_partial_oversample_1_3 = fun (n:int) (gamma:float -> float) (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_partial_paint_1_3 ( function_partial_oversample_1_2 n gamma color ) u f interval nsteps coordinates size ;;


(** {v function_color_paint_1_3 paint vector function color_function interval nsteps frame_coordinates window_size v} *)
let function_color_paint_1_3 = fun paint (u:float array) (f: float -> float array) (fc:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let m = Matrix.vector_float_chose_3 u in
  let v = m.(1)
  and w = m.(2) in
   let g = function t -> [| Matrix.vector_float_scal_prod ( f t ) v ; Matrix.vector_float_scal_prod ( f t ) w |]
   and coo = box_proj_3_2 u coordinates in
    paint g fc interval nsteps coo size ;;

(** {v function_color_draw_1_3 vector function color_function interval nsteps frame_coordinates window_size v} *)
let function_color_draw_1_3 = fun (u:float array) (f: float -> float array) (fc:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_color_paint_1_3 function_color_draw_1_2 u f fc interval nsteps coordinates size ;;

(** {v function_color_segment_1_3 vector function color_function interval nsteps frame_coordinates window_size v} *)
let function_color_segment_1_3 = fun (u:float array) (f: float -> float array) (fc:float -> float array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_color_paint_1_3 function_color_segment_1_2 u f fc interval nsteps coordinates size ;;


(** {v function_multi_paint_1_3 paint axes_paint vector function interval nsteps frame_coordinates window_size v} *)
let function_multi_paint_1_3 = fun paint axes_paint (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
(** The following instruction puts the extremal coordinates in the [accu] array.

L'instruction suivante place les coordonnées extrémales dans le tableau [accu].*)
  ignore ( Array.map ( project_1_3 accu a b u v w nsteps coordinates coo ) f ) ;
  axes_paint v w accu coo size ;
  for i = 0 to r - 1 do
   paint u f.(i) interval nsteps coordinates size ;
  done ;
  terminate_1_3 accu hh ;;

(** {v function_multi_draw_1_3 vector function interval nsteps frame_coordinates window_size v} *)
let function_multi_draw_1_3 = fun (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_3 ( function_partial_draw_1_3 Graphics.white ) ( draw_axes Graphics.yellow ) u f interval nsteps coordinates size ;;

(** {v function_multi_segment_1_3 vector function interval nsteps frame_coordinates window_size v} *)
let function_multi_segment_1_3 = fun (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_3 ( function_partial_segment_1_3 Graphics.white ) ( draw_axes Graphics.yellow ) u f interval nsteps coordinates size ;;

(** {v function_multi_smooth_1_3 gamma vector function interval nsteps frame_coordinates window_size v} *)
let function_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_3 ( function_partial_smooth_1_3 gamma Graphics.white ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps coordinates size ;;

(** {v function_multi_oversample_1_3 over_sampling_factor gamma vector function interval nsteps frame_coordinates window_size v} *)
let function_multi_oversample_1_3 = fun (n:int) (gamma:float -> float) (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multi_paint_1_3 ( function_partial_oversample_1_3 n gamma Graphics.white ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps coordinates size ;;


(** {v function_multicolor_paint_1_3 paint axes_paint colors vector function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_paint_1_3 = fun paint axes_paint (colors:Graphics.color array) (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
(** The following instruction puts the extremal coordinates in the [accu] array.

L'instruction suivante place les coordonnées extrémales dans le tableau [accu].*)
  ignore ( Array.map ( project_1_3 accu a b u v w nsteps coordinates coo ) f ) ;
  axes_paint v w accu coo size ;
  for i = 0 to r - 1 do
   paint colors.(i) u f.(i) interval nsteps coordinates size ;
  done ;
  terminate_1_3 accu hh ;;

(** {v function_multicolor_draw_1_3 colors vector function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_3 function_partial_draw_1_3 ( draw_axes Graphics.yellow ) colors u f interval nsteps coordinates size ;;

(** {v function_multicolor_segment_1_3 colors vector function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_3 function_partial_segment_1_3 ( draw_axes Graphics.yellow ) colors u f interval nsteps coordinates size ;;

(** {v function_multicolor_smooth_1_3 gamma colors vector function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_3 ( function_partial_smooth_1_3 gamma ) ( smooth_axes gamma Graphics.yellow ) colors u f interval nsteps coordinates size ;;

(** {v function_multicolor_oversample_1_3 over_sampling_factor gamma colors vector function interval nsteps frame_coordinates window_size v} *)
let function_multicolor_oversample_1_3 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (u:float array) (f:(float -> float array) array) (interval:float array) (nsteps:int) (coordinates:float array) (size:int array) ->
 function_multicolor_paint_1_3 ( function_partial_oversample_1_3 n gamma ) ( smooth_axes gamma Graphics.yellow ) colors u f interval nsteps coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_trans_paint_1_3 paint vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_paint_1_3 = fun paint (u:float array) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 let r = Array.length m.(0) in
  let scale = ( float r ) /. ( interval.(1) -. interval.(0) )
  and f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function x -> f ( ( x -. interval.(0) ) *. scale ) in
    paint u g interval r coordinates size ;;

(** {v discrete_paint_1_3 paint vector matrix interval frame_coordinates window_size v} *)
let discrete_paint_1_3 = fun paint (u:float array) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_3 paint u ( Matrix.float_transpose m ) interval coordinates size ;;

(** {v discrete_trans_draw_1_3 vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_draw_1_3 = fun (u:float array) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_3 function_draw_1_3 u m interval coordinates size ;;

(** {v discrete_draw_1_3 vector matrix interval frame_coordinates window_size v} *)
let discrete_draw_1_3 = fun (u:float array) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_3 function_draw_1_3 u m interval coordinates size ;;

(** {v discrete_trans_segment_1_3 vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_segment_1_3 = fun (u:float array) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_3 function_segment_1_3 u m interval coordinates size ;;

(** {v discrete_segment_1_3 vector matrix interval frame_coordinates window_size v} *)
let discrete_segment_1_3 = fun (u:float array) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_3 function_segment_1_3 u m interval coordinates size ;;

(** {v discrete_trans_smooth_1_3 gamma vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_smooth_1_3 = fun (gamma:float -> float) (u:float array) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_paint_1_3 ( function_smooth_1_3 gamma ) u m interval coordinates size ;;

(** {v discrete_smooth_1_3 gamma vector matrix interval frame_coordinates window_size v} *)
let discrete_smooth_1_3 = fun (gamma:float -> float) (u:float array) (m:float array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_1_3 ( function_smooth_1_3 gamma ) u m interval coordinates size ;;


(** {v discrete_trans_multi_paint_1_3 paint vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_multi_paint_1_3 = fun paint (u:float array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 2 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint u g interval nsteps coordinates size ;;

(** {v discrete_multi_paint_1_3 paint vector matrix interval frame_coordinates window_size v} *)
let discrete_multi_paint_1_3 = fun paint (u:float array) (m:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_3 paint u ( Array.map Matrix.float_transpose m ) interval coordinates size ;;

(** {v discrete_trans_multi_draw_1_3 vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_multi_draw_1_3 = fun (u:float array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_3 function_multi_draw_1_3 u v interval coordinates size ;;

(** {v discrete_multi_draw_1_3 vector matrix interval frame_coordinates window_size v} *)
let discrete_multi_draw_1_3 = fun (u:float array) (m:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_3 function_multi_draw_1_3 u m interval coordinates size ;;

(** {v discrete_trans_multi_segment_1_3 vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_multi_segment_1_3 = fun (u:float array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_3 function_multi_segment_1_3 u v interval coordinates size ;;

(** {v discrete_multi_segment_1_3 vector matrix interval frame_coordinates window_size v} *)
let discrete_multi_segment_1_3 = fun (u:float array) (m:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_3 function_multi_segment_1_3 u m interval coordinates size ;;

(** {v discrete_trans_multi_smooth_1_3 gamma vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multi_paint_1_3 ( function_multi_smooth_1_3 gamma ) u v interval coordinates size ;;

(** {v discrete_multi_smooth_1_3 gamma vector matrix interval frame_coordinates window_size v} *)
let discrete_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (m:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multi_paint_1_3 ( function_multi_smooth_1_3 gamma ) u m interval coordinates size ;;


(** {v discrete_trans_multicolor_paint_1_3 paint colors vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_multicolor_paint_1_3 = fun paint (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 2 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint colors u g interval nsteps coordinates size ;;

(** {v discrete_multicolor_paint_1_3 paint colors vector matrix interval frame_coordinates window_size v} *)
let discrete_multicolor_paint_1_3 = fun paint (colors:Graphics.color array) (u:float array) (m:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multicolor_paint_1_3 paint colors u ( Array.map Matrix.float_transpose m ) interval coordinates size ;;

(** {v discrete_trans_multicolor_draw_1_3 colors vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multicolor_paint_1_3 function_multicolor_draw_1_3 colors u v interval coordinates size ;;

(** {v discrete_multicolor_draw_1_3 colors vector matrix interval frame_coordinates window_size v} *)
let discrete_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (m:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_3 function_multicolor_draw_1_3 colors u m interval coordinates size ;;

(** {v discrete_trans_multicolor_segment_1_3 colors vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multicolor_paint_1_3 function_multicolor_segment_1_3 colors u v interval coordinates size ;;

(** {v discrete_multicolor_segment_1_3 colors vector matrix interval frame_coordinates window_size v} *)
let discrete_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (m:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_3 function_multicolor_segment_1_3 colors u m interval coordinates size ;;

(** {v discrete_trans_multicolor_smooth_1_3 gamma colors vector matrix interval frame_coordinates window_size v} *)
let discrete_trans_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_trans_multicolor_paint_1_3 ( function_multicolor_smooth_1_3 gamma ) colors u v interval coordinates size ;;

(** {v discrete_multicolor_smooth_1_3 gamma colors vector matrix interval frame_coordinates window_size v} *)
let discrete_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (m:float array array array) (interval:float array) (coordinates:float array) (size:int array) ->
 discrete_multicolor_paint_1_3 ( function_multicolor_smooth_1_3 gamma ) colors u m interval coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R dans TR^3}
{1 Drawings from R to TR^3}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v field_paint_1_3 paint axes_paint vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_paint_1_3 = fun paint axes_paint (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
(** The following instruction puts the extremal coordinates in the [accu] array.

L'instruction suivante place les coordonnées extrémales dans le tableau [accu].*)
  let g = multi_project_1_3 accu a b u v w nsteps coordinates coo f in
   axes_paint v w accu coo size ;
   paint Graphics.white Graphics.green g interval nsteps nfeet coo size ;
   terminate_1_3 accu hh ;;

(** {v field_draw_1_3 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_draw_1_3 = fun (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_paint_1_3 ( field_partial_draw_1_2 ) ( draw_axes Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v field_segment_1_3 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_segment_1_3 = fun (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_paint_1_3 ( field_partial_segment_1_2 ) ( draw_axes Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v field_smooth_1_3 gamma vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_smooth_1_3 = fun (gamma:float -> float) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_paint_1_3 ( field_partial_smooth_1_2 gamma ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v field_oversample_1_3 over_sampling_factor gamma vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_oversample_1_3 = fun (n:int) (gamma:float -> float) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_paint_1_3 ( field_partial_oversample_1_2 n gamma ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;


(** {v field_partial_paint_1_3 paint color1 color2 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_partial_paint_1_3 = fun paint (color1:Graphics.color) (color2:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let m = Matrix.vector_float_chose_3 u
 and accu = Array.make 6 0.
 and b = interval.(0)
 and a = ( interval.(1) -. interval.(0) ) /. ( float ( nsteps + 1 ) ) in
  let v = m.(1)
  and w = m.(2)
  and coo = box_proj_3_2 u coordinates in
   let g = multi_project_1_3 accu a b u v w nsteps coordinates coo f in
    paint color1 color2 g interval nsteps nfeet coo size ;;

(** {v field_partial_draw_1_3 color1 color2 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_partial_draw_1_3 = fun (color1:Graphics.color) (color2:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_partial_paint_1_3 field_partial_draw_1_2 color1 color2 u f interval nsteps nfeet coordinates size ;;

(** {v field_partial_segment_1_3 color1 color2 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_partial_segment_1_3 = fun (color1:Graphics.color) (color2:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_partial_paint_1_3 field_partial_segment_1_2 color1 color2 u f interval nsteps nfeet coordinates size ;;

(** {v field_partial_smooth_1_3 gamma color1 color2 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_partial_smooth_1_3 = fun (gamma:float -> float) (color1:Graphics.color) (color2:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_partial_paint_1_3 ( field_partial_smooth_1_2 gamma ) color1 color2 u f interval nsteps nfeet coordinates size ;;

(** {v field_partial_oversample_1_3 over_sampling_factor gamma color1 color2 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_partial_oversample_1_3 = fun (n:int) (gamma:float -> float) (color1:Graphics.color) (color2:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_partial_paint_1_3 ( field_partial_oversample_1_2 n gamma ) color1 color2 u f interval nsteps nfeet coordinates size ;;


(** {v field_multi_paint_1_3 paint axes_paint vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_paint_1_3 = fun paint axes_paint (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
  for i = 0 to r - 1 do
   let g = multi_project_1_3 accu a b u v w nsteps coordinates coo f.(i) in
    paint Graphics.white Graphics.green g interval nsteps nfeet coo size ;
  done ;
  axes_paint v w accu coo size ;
  terminate_1_3 accu hh ;;

(** {v field_multi_draw_1_3 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_draw_1_3 = fun (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multi_paint_1_3 field_partial_draw_1_2 ( draw_axes Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v field_multi_segment_1_3 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_segment_1_3 = fun (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multi_paint_1_3 field_partial_segment_1_2 ( draw_axes Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v field_multi_smooth_1_3 gamma vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multi_paint_1_3 ( field_partial_smooth_1_2 gamma ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v field_multi_oversample_1_3 over_sampling_factor gamma vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multi_oversample_1_3 = fun (n:int) (gamma:float -> float) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multi_paint_1_3 ( field_partial_oversample_1_2 n gamma ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;


(** {v field_multicolor_paint_1_3 paint axes_paint colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_paint_1_3 = fun paint axes_paint (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
  for i = 0 to r - 1 do
   let g = multi_project_1_3 accu a b u v w nsteps coordinates coo f.(i) in
    paint colors.(i) ( soft_color colors.(i) ) g interval nsteps nfeet coo size ;
  done ;
  axes_paint v w accu coo size ;
  terminate_1_3 accu hh ;;

(** {v field_multicolor_draw_1_3 colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multicolor_paint_1_3 field_partial_draw_1_2 ( draw_axes Graphics.yellow ) colors u f interval nsteps nfeet coordinates size ;;

(** {v field_multicolor_segment_1_3 colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multicolor_paint_1_3 field_partial_segment_1_2 ( draw_axes Graphics.yellow ) colors u f interval nsteps nfeet coordinates size ;;

(** {v field_multicolor_smooth_1_3 gamma colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multicolor_paint_1_3 ( field_partial_smooth_1_2 gamma ) ( smooth_axes gamma Graphics.yellow ) colors u f interval nsteps nfeet coordinates size ;;

(** {v field_multicolor_oversample_1_3 over_sampling_factor gamma colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let field_multicolor_oversample_1_3 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 field_multicolor_paint_1_3 ( field_partial_oversample_1_2 n gamma ) ( smooth_axes gamma Graphics.yellow ) colors u f interval nsteps nfeet coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_field_trans_paint_1_3 paint vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_paint_1_3 = fun paint (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length m.(0) in
  let scale = ( float r ) /. ( interval.(1) -. interval.(0) )
  and f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function x -> f ( ( x -. interval.(0) ) *. scale ) in
    paint u g interval r nfeet coordinates size ;;

(** {v discrete_field_paint_1_3 paint vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_paint_1_3 = fun paint (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_3 paint u ( Matrix.float_transpose m ) interval nfeet coordinates size ;;

(** {v discrete_field_trans_draw_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_draw_1_3 = fun (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_3 field_draw_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_field_draw_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_draw_1_3 = fun (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_3 field_draw_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_field_trans_segment_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_segment_1_3 = fun (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_3 field_segment_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_field_segment_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_segment_1_3 = fun (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_3 field_segment_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_field_trans_smooth_1_3 gamma vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_smooth_1_3 = fun (gamma:float -> float) (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_paint_1_3 ( field_smooth_1_3 gamma ) u m interval nfeet coordinates size ;;

(** {v discrete_field_smooth_1_3 gamma vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_smooth_1_3 = fun (gamma:float -> float) (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_paint_1_3 ( field_smooth_1_3 gamma ) u m interval nfeet coordinates size ;;


(** {v discrete_field_trans_multi_paint_1_3 paint vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multi_paint_1_3 = fun paint (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 6 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint u g interval nsteps nfeet coordinates size ;;

(** {v discrete_field_multi_paint_1_3 paint vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multi_paint_1_3 = fun paint (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_3 paint u ( Array.map Matrix.float_transpose v ) interval nfeet coordinates size ;;

(** {v discrete_field_trans_multi_draw_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multi_draw_1_3 = fun (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_3 field_multi_draw_1_3 u v interval nfeet coordinates size ;;

(** {v discrete_field_multi_draw_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multi_draw_1_3 = fun (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_3 field_multi_draw_1_3 u v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multi_segment_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multi_segment_1_3 = fun (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_3 field_multi_segment_1_3 u v interval nfeet coordinates size ;;

(** {v discrete_field_multi_segment_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multi_segment_1_3 = fun (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_3 field_multi_segment_1_3 u v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multi_smooth_1_3 gamma vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multi_paint_1_3 ( field_multi_smooth_1_3 gamma ) u v interval nfeet coordinates size ;;

(** {v discrete_field_multi_smooth_1_3 gamma vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multi_paint_1_3 ( field_multi_smooth_1_3 gamma ) u v interval nfeet coordinates size ;;


(** {v discrete_field_trans_multicolor_paint_1_3 paint colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multicolor_paint_1_3 = fun paint (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 6 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint colors u g interval nsteps nfeet coordinates size ;;

(** {v discrete_field_multicolor_paint_1_3 paint colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multicolor_paint_1_3 = fun paint (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multicolor_paint_1_3 paint colors u ( Array.map Matrix.float_transpose v ) interval nfeet coordinates size ;;

(** {v discrete_field_trans_multicolor_draw_1_3 colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multicolor_paint_1_3 field_multicolor_draw_1_3 colors u v interval nfeet coordinates size ;;

(** {v discrete_field_multicolor_draw_1_3 colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multicolor_paint_1_3 field_multicolor_draw_1_3 colors u v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multicolor_segment_1_3 colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multicolor_paint_1_3 field_multicolor_segment_1_3 colors u v interval nfeet coordinates size ;;

(** {v discrete_field_multicolor_segment_1_3 colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multicolor_paint_1_3 field_multicolor_segment_1_3 colors u v interval nfeet coordinates size ;;

(** {v discrete_field_trans_multicolor_smooth_1_3 gamma colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_trans_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_trans_multicolor_paint_1_3 ( field_multicolor_smooth_1_3 gamma ) colors u v interval nfeet coordinates size ;;

(** {v discrete_field_multicolor_smooth_1_3 gamma colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_field_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_field_multicolor_paint_1_3 ( field_multicolor_smooth_1_3 gamma ) colors u v interval nfeet coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R dans R^n ¤ TR^3}
{1 Drawings from R to R^n ¤ TR^3}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v frame_field_paint_1_3 paint axes_paint vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_paint_1_3 = fun paint axes_paint (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
  let g = multi_project_1_3 accu a b u v w nsteps coordinates coo f in
   axes_paint v w accu coo size ;
   paint g interval nsteps nfeet coo size ;
   terminate_1_3 accu hh ;;

(** {v frame_field_draw_1_3 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_draw_1_3 = fun (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_1_3 frame_field_draw_1_2 ( draw_axes Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_segment_1_3 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_segment_1_3 = fun (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_1_3 frame_field_segment_1_2 ( draw_axes Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_smooth_1_3 gamma vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_smooth_1_3 = fun (gamma:float -> float) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_1_3 ( frame_field_smooth_1_2 gamma ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_oversample_1_3 over_sampling_factor gamma vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_oversample_1_3 = fun (n:int) (gamma:float -> float) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_1_3 ( frame_field_oversample_1_2 n gamma ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;


(** {v frame_field_partial_paint_1_3 paint color vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_partial_paint_1_3 = fun paint (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let m = Matrix.vector_float_chose_3 u
 and accu = Array.make 6 0.
 and b = interval.(0)
 and a = ( interval.(1) -. interval.(0) ) /. ( float ( nsteps + 1 ) ) in
  let v = m.(1)
  and w = m.(2)
  and coo = box_proj_3_2 u coordinates in
   let g = multi_project_1_3 accu a b u v w nsteps coordinates coo f in
    paint color g interval nsteps nfeet coo size ;;

(** {v frame_field_partial_draw_1_3 color vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_partial_draw_1_3 = fun (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_partial_paint_1_3 frame_field_partial_draw_1_2 color u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_partial_segment_1_3 color vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_partial_segment_1_3 = fun (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_partial_paint_1_3 frame_field_partial_segment_1_2 color u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_partial_smooth_1_3 gamma color vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_partial_smooth_1_3 = fun (gamma:float -> float) (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_partial_paint_1_3 ( frame_field_partial_smooth_1_2 gamma ) color u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_partial_oversample_1_3 over_sampling_factor gamma color vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_partial_oversample_1_3 = fun (n:int) (gamma:float -> float) (color:Graphics.color) (u:float array) (f: float -> float array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_partial_paint_1_3 ( frame_field_partial_oversample_1_2 n gamma ) color u f interval nsteps nfeet coordinates size ;;


(** {v frame_field_multi_paint_1_3 paint axes_paint vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_paint_1_3 = fun paint axes_paint (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
(** The following instruction puts the extremal coordinates in the [accu] array.

L'instruction suivante place les coordonnées extrémales dans le tableau [accu].*)
  let g = Array.map ( multi_project_1_3 accu a b u v w nsteps coordinates coo ) f in
   axes_paint v w accu coo size ;
   for i = 0 to pred r do
    paint g.(i) interval nsteps nfeet coo size ;
   done ;
   terminate_1_3 accu hh ;;

(** {v frame_field_multi_draw_1_3 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_draw_1_3 = fun (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multi_paint_1_3 ( frame_field_partial_draw_1_2 Graphics.white ) ( draw_axes Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multi_segment_1_3 vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_segment_1_3 = fun (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multi_paint_1_3 ( frame_field_partial_segment_1_2 Graphics.white ) ( draw_axes Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multi_smooth_1_3 gamma vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multi_paint_1_3 ( frame_field_partial_smooth_1_2 gamma Graphics.white ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multi_oversample_1_3 over_sampling_factor gamma vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multi_oversample_1_3 = fun (n:int) (gamma:float -> float) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multi_paint_1_3 ( frame_field_partial_oversample_1_2 n gamma Graphics.white ) ( smooth_axes gamma Graphics.yellow ) u f interval nsteps nfeet coordinates size ;;


(** {v frame_field_multicolor_paint_1_3 paint axes_paint vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_paint_1_3 = fun paint axes_paint (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length f
 and ( a , b , accu , hh , v , w , coo ) = prepare_1_3 u interval nsteps coordinates size in
(** The following instruction puts the extremal coordinates in the [accu] array.

L'instruction suivante place les coordonnées extrémales dans le tableau [accu].*)
  let g = Array.map ( multi_project_1_3 accu a b u v w nsteps coordinates coo ) f in
   axes_paint v w accu coo size ;
   for i = 0 to pred r do
    paint colors.(i) g.(i) interval nsteps nfeet coo size ;
   done ;
   terminate_1_3 accu hh ;;

(** {v frame_field_multicolor_draw_1_3 colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multicolor_paint_1_3 frame_field_partial_draw_1_2 ( draw_axes Graphics.yellow ) colors u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multicolor_segment_1_3 colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multicolor_paint_1_3 frame_field_partial_segment_1_2 ( draw_axes Graphics.yellow ) colors u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multicolor_smooth_1_3 gamma colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multicolor_paint_1_3 ( frame_field_partial_smooth_1_2 gamma ) ( smooth_axes gamma Graphics.yellow ) colors u f interval nsteps nfeet coordinates size ;;

(** {v frame_field_multicolor_oversample_1_3 over_sampling_factor gamma colors vector function interval nsteps nfeet frame_coordinates window_size v} *)
let frame_field_multicolor_oversample_1_3 = fun (n:int) (gamma:float -> float) (colors:Graphics.color array) (u:float array) (f: (float -> float array) array) (interval:float array) (nsteps:int) (nfeet:int) (coordinates:float array) (size:int array) ->
 frame_field_multicolor_paint_1_3 ( frame_field_partial_oversample_1_2 n gamma ) ( smooth_axes gamma Graphics.yellow ) colors u f interval nsteps nfeet coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_frame_field_trans_paint_1_3 paint vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_paint_1_3 = fun paint (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let r = Array.length m.(0) in
  let scale = ( float r ) /. ( interval.(1) -. interval.(0) )
  and f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function x -> f ( ( x -. interval.(0) ) *. scale ) in
    paint u g interval r nfeet coordinates size ;;

(** {v discrete_frame_field_paint_1_3 paint vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_paint_1_3 = fun paint (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_paint_1_3 paint u ( Matrix.float_transpose m ) interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_draw_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_draw_1_3 = fun (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_paint_1_3 frame_field_draw_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_draw_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_draw_1_3 = fun (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_1_3 frame_field_draw_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_segment_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_segment_1_3 = fun (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_paint_1_3 frame_field_segment_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_segment_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_segment_1_3 = fun (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_1_3 frame_field_segment_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_smooth_1_3 gamma vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_smooth_1_3 = fun (gamma:float -> float) (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_paint_1_3 ( frame_field_smooth_1_3 gamma ) u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_smooth_1_3 gamma vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_smooth_1_3 = fun (gamma:float -> float) (u:float array) (m:float array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_1_3 ( frame_field_smooth_1_3 gamma ) u m interval nfeet coordinates size ;;


(** {v discrete_frame_field_trans_multi_paint_1_3 paint vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multi_paint_1_3 = fun paint (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 4 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint u g interval nsteps nfeet coordinates size ;;

(** {v discrete_frame_field_multi_paint_1_3 paint vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multi_paint_1_3 = fun paint (u:float array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multi_paint_1_3 paint u ( Array.map Matrix.float_transpose m ) interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multi_draw_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multi_draw_1_3 = fun (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multi_paint_1_3 frame_field_multi_draw_1_3 u v interval nfeet coordinates size ;;

(** {v discrete_frame_field_multi_draw_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multi_draw_1_3 = fun (u:float array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_multi_paint_1_3 frame_field_multi_draw_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multi_segment_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multi_segment_1_3 = fun (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multi_paint_1_3 frame_field_multi_segment_1_3 u v interval nfeet coordinates size ;;

(** {v discrete_frame_field_multi_segment_1_3 vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multi_segment_1_3 = fun (u:float array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_multi_paint_1_3 frame_field_multi_segment_1_3 u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multi_smooth_1_3 gamma vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multi_paint_1_3 ( frame_field_multi_smooth_1_3 gamma ) u v interval nfeet coordinates size ;;

(** {v discrete_frame_field_multi_smooth_1_3 gamma vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multi_smooth_1_3 = fun (gamma:float -> float) (u:float array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_multi_paint_1_3 ( frame_field_multi_smooth_1_3 gamma ) u m interval nfeet coordinates size ;;


(** {v discrete_frame_field_trans_multicolor_paint_1_3 paint colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multicolor_paint_1_3 = fun paint (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 let nsteps = Array.length v.(0).(0) in
  let scale = ( float nsteps ) /. ( interval.(1) -. interval.(0) )
  and r = Array.length v in
   let g = Array.make r ( Util.vector_zero 4 ) in
    for i = 0 to r - 1 do
     let f = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) v.(i) in
      g.(i) <- ( function x -> f ( ( x -. interval.(0) ) *. scale ) ) ;
    done ;
    paint colors u g interval nsteps nfeet coordinates size ;;

(** {v discrete_frame_field_multicolor_paint_1_3 paint colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multicolor_paint_1_3 = fun paint (colors:Graphics.color array) (u:float array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multicolor_paint_1_3 paint colors u ( Array.map Matrix.float_transpose m ) interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multicolor_draw_1_3 colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multicolor_paint_1_3 frame_field_multicolor_draw_1_3 colors u v interval nfeet coordinates size ;;

(** {v discrete_frame_field_multicolor_draw_1_3 colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multicolor_draw_1_3 = fun (colors:Graphics.color array) (u:float array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_multicolor_paint_1_3 frame_field_multicolor_draw_1_3 colors u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multicolor_segment_1_3 colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multicolor_paint_1_3 frame_field_multicolor_segment_1_3 colors u v interval nfeet coordinates size ;;

(** {v discrete_frame_field_multicolor_segment_1_3 colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multicolor_segment_1_3 = fun (colors:Graphics.color array) (u:float array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_multicolor_paint_1_3 frame_field_multicolor_segment_1_3 colors u m interval nfeet coordinates size ;;

(** {v discrete_frame_field_trans_multicolor_smooth_1_3 gamma colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_trans_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (v:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_trans_multicolor_paint_1_3 ( frame_field_multicolor_smooth_1_3 gamma ) colors u v interval nfeet coordinates size ;;

(** {v discrete_frame_field_multicolor_smooth_1_3 gamma colors vector matrix interval nfeet frame_coordinates window_size v} *)
let discrete_frame_field_multicolor_smooth_1_3 = fun (gamma:float -> float) (colors:Graphics.color array) (u:float array) (m:float array array array) (interval:float array) (nfeet:int) (coordinates:float array) (size:int array) ->
 discrete_frame_field_multicolor_paint_1_3 ( frame_field_multicolor_smooth_1_3 gamma ) colors u m interval nfeet coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R^2 dans R^3}
{1 Drawings from R^2 to R^3}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v function_paint_2_3 paint vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_paint_2_3 = fun paint (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let ( aa , aaa , bb , bbb , hh , xx , yy ) = prepare_2_3 u a b nsteps ngrid coordinates size in
  let accumul = ref bb
  and accu = Array.make 6 0.
  and accumulator = ref bbb in
   for i = 0 to ngrid  - 1 do
    accumul := bb ;
    accumulator := bbb ;
    let g = function x -> f [| x ; yy.(i) |]
    and h = function x -> f [| xx.(i) ; x |] in
     for j = 0 to nsteps do
      let value = g ( !accumul )
      and valeur = h ( !accumulator) in
       accu.(0) <- min accu.(0) ( min value.(0) valeur.(0) ) ;
       accu.(1) <- max accu.(1) ( max value.(0) valeur.(0) ) ;
       accu.(2) <- min accu.(2) ( min value.(1) valeur.(1) ) ;
       accu.(3) <- max accu.(3) ( max value.(1) valeur.(1) ) ;
       accu.(4) <- min accu.(4) ( min value.(2) valeur.(2) ) ;
       accu.(5) <- max accu.(5) ( max value.(2) valeur.(2) ) ;
       accumul := !accumul +. aa ;
       accumulator := !accumulator +. aaa ;
     done ;
     paint Graphics.white u g [| a.(0) ; b.(0) |] nsteps coordinates size ;
     paint Graphics.white u h [| a.(1) ; b.(1) |] nsteps coordinates size ;
   done ;
   terminate_1_3 accu hh ;;

(** {v function_draw_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_draw_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
function_paint_2_3 function_partial_draw_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v function_segment_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_segment_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
function_paint_2_3 function_partial_segment_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v function_smooth_2_3 gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_smooth_2_3 = fun (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
function_paint_2_3 ( function_partial_smooth_1_3 gamma ) u f a b nsteps ngrid coordinates size ;;

(** {v function_oversample_2_3 over_sampling_factor gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_oversample_2_3 = fun (n:int) (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
function_paint_2_3 ( function_partial_oversample_1_3 n gamma ) u f a b nsteps ngrid coordinates size ;;


(** {v function_color_paint_2_3 paint vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let function_color_paint_2_3 = fun paint (u:float array) (f:float array -> float array) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let ( aa , aaa , bb , bbb , hh , xx , yy ) = prepare_2_3 u a b nsteps ngrid coordinates size in
  let accumul = ref bb
  and accu = Array.make 6 0.
  and accumulator = ref bbb in
   for i = 0 to ngrid  - 1 do
    accumul := bb ;
    accumulator := bbb ;
    let g = function x -> f [| x ; yy.(i) |]
    and gc = function x -> fc [| x ; yy.(i) |]
    and h = function x -> f [| xx.(i) ; x |]
    and hc = function x -> fc [| xx.(i) ; x |] in
     for j = 0 to nsteps do
      let value = g ( !accumul )
      and valeur = h ( !accumulator) in
       accu.(0) <- min accu.(0) ( min value.(0) valeur.(0) ) ;
       accu.(1) <- max accu.(1) ( max value.(0) valeur.(0) ) ;
       accu.(2) <- min accu.(2) ( min value.(1) valeur.(1) ) ;
       accu.(3) <- max accu.(3) ( max value.(1) valeur.(1) ) ;
       accu.(4) <- min accu.(4) ( min value.(2) valeur.(2) ) ;
       accu.(5) <- max accu.(5) ( max value.(2) valeur.(2) ) ;
       accumul := !accumul +. aa ;
       accumulator := !accumulator +. aaa ;
     done ;
     paint u g gc [| a.(0) ; b.(0) |] nsteps coordinates size ;
     paint u h hc [| a.(1) ; b.(1) |] nsteps coordinates size ;
   done ;
   terminate_1_3 accu hh ;;

(** {v function_color_draw_2_3 vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let function_color_draw_2_3 = fun (u:float array) (f:float array -> float array) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 function_color_paint_2_3 function_color_draw_1_3 u f fc a b nsteps ngrid coordinates size ;;

(** {v function_color_segment_2_3 vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let function_color_segment_2_3 = fun (u:float array) (f:float array -> float array) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 function_color_paint_2_3 function_color_segment_1_3 u f fc a b nsteps ngrid coordinates size ;;


(** {v graph_paint_2_3 paint vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_paint_2_3 = fun paint (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let f = function x -> [| x.(0) ; x.(1) ; ff x |] in
  function_paint_2_3 paint u f a b nsteps ngrid coordinates size ;;

(** {v graph_draw_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_draw_2_3 = fun (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_paint_2_3 function_partial_draw_1_3 u ff a b nsteps ngrid coordinates size ;;

(** {v graph_segment_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_segment_2_3 = fun (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_paint_2_3 function_partial_segment_1_3 u ff a b nsteps ngrid coordinates size ;;

(** {v graph_smooth_2_3 gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_smooth_2_3 = fun (gamma:float -> float) (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_paint_2_3 ( function_partial_smooth_1_3 gamma ) u ff a b nsteps ngrid coordinates size ;;

(** {v graph_oversample_2_3 over_sampling_factor gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_oversample_2_3 = fun (n:int) (gamma:float -> float) (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_paint_2_3 ( function_partial_oversample_1_3 n gamma ) u ff a b nsteps ngrid coordinates size ;;


(** {v graph_color_paint_2_3 paint vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_color_paint_2_3 = fun paint (u:float array) (ff:float array -> float) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let f = function x -> [| x.(0) ; x.(1) ; ff x |] in
  function_color_paint_2_3 paint u f fc a b nsteps ngrid coordinates size ;;

(** {v graph_color_draw_2_3 vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_color_draw_2_3 = fun (u:float array) (ff:float array -> float) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_color_paint_2_3 function_color_draw_1_3 u ff fc a b nsteps ngrid coordinates size ;;

(** {v graph_color_segment_2_3 vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_color_segment_2_3 = fun (u:float array) (ff:float array -> float) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_color_paint_2_3 function_color_segment_1_3 u ff fc a b nsteps ngrid coordinates size ;;


(** {v function_reverse_paint_2_3 paint vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_reverse_paint_2_3 = fun paint (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let ( aa , aaa , bb , bbb , hh , xx , yy ) = prepare_2_3 u a b nsteps ngrid coordinates size in
  let accumul = ref bb
  and accu = Array.make 6 0.
  and accumulator = ref bbb in
    for i = ngrid - 1 downto 0 do
     accumul := bb ;
     accumulator := bbb ;
     let g = function x -> f [| x ; yy.(i) |]
     and h = function x -> f [| xx.(i) ; x |] in
      for j = 0 to nsteps do
       let value = g ( !accumul )
       and valeur = h ( !accumulator) in
        accu.(0) <- min accu.(0) ( min value.(0) valeur.(0) ) ;
        accu.(1) <- max accu.(1) ( max value.(0) valeur.(0) ) ;
        accu.(2) <- min accu.(2) ( min value.(1) valeur.(1) ) ;
        accu.(3) <- max accu.(3) ( max value.(1) valeur.(1) ) ;
        accu.(4) <- min accu.(4) ( min value.(2) valeur.(2) ) ;
        accu.(5) <- max accu.(5) ( max value.(2) valeur.(2) ) ;
        accumul := !accumul +. aa ;
        accumulator := !accumulator +. aaa ;
      done ;
      paint Graphics.white u g [| a.(0) ; b.(0) |] nsteps coordinates size ;
      paint Graphics.white u h [| a.(1) ; b.(1) |] nsteps coordinates size ;
     done ;
   terminate_1_3 accu hh ;;

(** {v function_reverse_draw_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_reverse_draw_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 function_reverse_paint_2_3 function_partial_draw_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v function_reverse_segment_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_reverse_segment_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 function_reverse_paint_2_3 function_partial_segment_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v function_reverse_smooth_2_3 gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_reverse_smooth_2_3 = fun (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 function_reverse_paint_2_3 ( function_partial_smooth_1_3 gamma ) u f a b nsteps ngrid coordinates size ;;

(** {v function_reverse_oversample_2_3 over_sampling_factor gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let function_reverse_oversample_2_3 = fun (n:int) (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 function_reverse_paint_2_3 ( function_partial_oversample_1_3 n gamma ) u f a b nsteps ngrid coordinates size ;;


(** {v function_reverse_color_paint_2_3 paint vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let function_reverse_color_paint_2_3 = fun paint (u:float array) (f:float array -> float array) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let ( aa , aaa , bb , bbb , hh , xx , yy ) = prepare_2_3 u a b nsteps ngrid coordinates size in
  let accumul = ref bb
  and accu = Array.make 6 0.
  and accumulator = ref bbb in
   for i = ngrid - 1 downto 0 do
    accumul := bb ;
    accumulator := bbb ;
    let g = function x -> f [| x ; yy.(i) |]
    and gc = function x -> fc [| x ; yy.(i) |]
    and h = function x -> f [| xx.(i) ; x |]
    and hc = function x -> fc [| xx.(i) ; x |] in
     for j = 0 to nsteps do
      let value = g ( !accumul )
      and valeur = h ( !accumulator) in
       accu.(0) <- min accu.(0) ( min value.(0) valeur.(0) ) ;
       accu.(1) <- max accu.(1) ( max value.(0) valeur.(0) ) ;
       accu.(2) <- min accu.(2) ( min value.(1) valeur.(1) ) ;
       accu.(3) <- max accu.(3) ( max value.(1) valeur.(1) ) ;
       accu.(4) <- min accu.(4) ( min value.(2) valeur.(2) ) ;
       accu.(5) <- max accu.(5) ( max value.(2) valeur.(2) ) ;
       accumul := !accumul +. aa ;
       accumulator := !accumulator +. aaa ;
     done ;
     paint u g gc [| a.(0) ; b.(0) |] nsteps coordinates size ;
     paint u h hc [| a.(1) ; b.(1) |] nsteps coordinates size ;
   done ;
   terminate_1_3 accu hh ;;

(** {v function_reverse_color_draw_2_3 vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let function_reverse_color_draw_2_3 = fun (u:float array) (f:float array -> float array) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 function_reverse_color_paint_2_3 function_color_draw_1_3 u f fc a b nsteps ngrid coordinates size ;;

(** {v function_reverse_color_segment_2_3 vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let function_reverse_color_segment_2_3 = fun (u:float array) (f:float array -> float array) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 function_reverse_color_paint_2_3 function_color_segment_1_3 u f fc a b nsteps ngrid coordinates size ;;


(** {v graph_reverse_paint_2_3 paint vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_reverse_paint_2_3 = fun paint (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let f = function x -> [| x.(0) ; x.(1) ; ff x |] in
  function_reverse_paint_2_3 paint u f a b nsteps ngrid coordinates size ;;


(** {v graph_reverse_draw_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_reverse_draw_2_3 = fun (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_reverse_paint_2_3 function_partial_draw_1_3 u ff a b nsteps ngrid coordinates size ;;

(** {v graph_reverse_segment_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_reverse_segment_2_3 = fun (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_reverse_paint_2_3 function_partial_segment_1_3 u ff a b nsteps ngrid coordinates size ;;

(** {v graph_reverse_smooth_2_3 gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_reverse_smooth_2_3 = fun (gamma:float -> float) (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_reverse_paint_2_3 ( function_partial_smooth_1_3 gamma ) u ff a b nsteps ngrid coordinates size ;;

(** {v graph_reverse_oversample_2_3 over_sampling_factor gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_reverse_oversample_2_3 = fun (n:int) (gamma:float -> float) (u:float array) (ff:float array -> float) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_reverse_paint_2_3 ( function_partial_oversample_1_3 n gamma ) u ff a b nsteps ngrid coordinates size ;;


(** {v graph_reverse_color_paint_2_3 paint vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_reverse_color_paint_2_3 = fun paint (u:float array) (ff:float array -> float) (fc:float array -> float array)  (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let f = function x -> [| x.(0) ; x.(1) ; ff x |] in
  function_reverse_color_paint_2_3 paint u f fc a b nsteps ngrid coordinates size ;;

(** {v graph_reverse_color_draw_2_3 vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_reverse_color_draw_2_3 = fun (u:float array) (ff:float array -> float) (fc:float array -> float array)  (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_reverse_color_paint_2_3 function_color_draw_1_3 u ff fc a b nsteps ngrid coordinates size ;;

(** {v graph_reverse_color_segment_2_3 vector function color_function a b nsteps ngrid frame_coordinates window_size v} *)
let graph_reverse_color_segment_2_3 = fun (u:float array) (ff:float array -> float) (fc:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 graph_reverse_color_paint_2_3 function_color_segment_1_3 u ff fc a b nsteps ngrid coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_graph_paint_2_3 paint vector matrix a b frame_coordinates window_size v} *)
let discrete_graph_paint_2_3 = fun paint (u:float array) (m:float array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let scale_x = ( float c ) /. ( b.(0) -. a.(0) )
  and scale_y = ( float r ) /. ( b.(1) -. a.(1) )
  and f = Infinitesimal.interpol_2 ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function z -> f ( ( z.(0) -. a.(0) ) *. scale_x ) ( ( z.(1) -. a.(1) ) *. scale_y ) in
    paint u g a b ( max r c ) ( min r c ) coordinates size ;;

(** {v discrete_graph_draw_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_graph_draw_2_3 = fun (u:float array) (m:float array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_graph_paint_2_3 graph_draw_2_3 u m a b coordinates size ;;

(** {v discrete_graph_segment_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_graph_segment_2_3 = fun (u:float array) (m:float array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_graph_paint_2_3 graph_segment_2_3 u m a b coordinates size ;;

(** {v discrete_graph_smooth_2_3 gamma vector matrix a b frame_coordinates window_size v} *)
let discrete_graph_smooth_2_3 = fun (gamma:float -> float) (u:float array) (m:float array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_graph_paint_2_3 ( graph_smooth_2_3 gamma ) u m a b coordinates size ;;


(** {v discrete_graph_color_paint_2_3 paint vector matrix color_function a b frame_coordinates window_size v} *)
let discrete_graph_color_paint_2_3 = fun paint (u:float array) (m:float array array) (fc:float array -> float array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let scale_x = ( float c ) /. ( b.(0) -. a.(0) )
  and scale_y = ( float r ) /. ( b.(1) -. a.(1) )
  and f = Infinitesimal.interpol_2 ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) m in
   let g = function z -> f ( ( z.(0) -. a.(0) ) *. scale_x ) ( ( z.(1) -. a.(1) ) *. scale_y ) in
    paint u g fc a b ( max r c ) ( min r c ) coordinates size ;;

(** {v discrete_graph_color_draw_2_3 vector matrix color_function a b frame_coordinates window_size v} *)
let discrete_graph_color_draw_2_3 = fun (u:float array) (m:float array array) (fc:float array -> float array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_graph_color_paint_2_3 graph_color_draw_2_3 u m fc a b coordinates size ;;

(** {v discrete_graph_color_segment_2_3 vector matrix color_function a b frame_coordinates window_size v} *)
let discrete_graph_color_segment_2_3 = fun (u:float array) (m:float array array) (fc:float array -> float array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_graph_color_paint_2_3 graph_color_segment_2_3 u m fc a b coordinates size ;;


(** {v discrete_paint_2_3 paint vector tensor a b frame_coordinates window_size v} *)
let discrete_paint_2_3 = fun paint (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let scale_x = ( float c ) /. ( b.(0) -. a.(0) )
  and scale_y = ( float r ) /. ( b.(1) -. a.(1) )
  and f = Infinitesimal.interpol_2 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) m in
   let g = function z -> f ( ( z.(0) -. a.(0) ) *. scale_x ) ( ( z.(1) -. a.(1) ) *. scale_y ) in
    paint u g a b ( max r c ) ( min r c ) coordinates size ;;

(** {v discrete_draw_2_3 vector tensor a b frame_coordinates window_size v} *)
let discrete_draw_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_2_3 function_draw_2_3 u m a b coordinates size ;;

(** {v discrete_segment_2_3 vector tensor a b frame_coordinates window_size v} *)
let discrete_segment_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_2_3 function_segment_2_3 u m a b coordinates size ;;

(** {v discrete_smooth_2_3 gamma vector tensor a b frame_coordinates window_size v} *)
let discrete_smooth_2_3 = fun (gamma:float -> float) (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_paint_2_3 ( function_smooth_2_3 gamma ) u m a b coordinates size ;;


(** {v discrete_color_paint_2_3 paint vector tensor color_function  a b frame_coordinates window_size v} *)
let discrete_color_paint_2_3 = fun paint (u:float array) (m:float array array array) (fc:float array -> float array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let scale_x = ( float c ) /. ( b.(0) -. a.(0) )
  and scale_y = ( float r ) /. ( b.(1) -. a.(1) )
  and f = Infinitesimal.interpol_2 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) m in
   let g = function z -> f ( ( z.(0) -. a.(0) ) *. scale_x ) ( ( z.(1) -. a.(1) ) *. scale_y ) in
    paint u g fc a b ( max r c ) ( min r c ) coordinates size ;;

(** {v discrete_color_draw_2_3 vector tensor color_function  a b frame_coordinates window_size v} *)
let discrete_color_draw_2_3 = fun (u:float array) (m:float array array array) (fc:float array -> float array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_color_paint_2_3 function_color_draw_2_3 u m fc a b coordinates size ;;

(** {v discrete_color_segment_2_3 vector tensor color_function  a b frame_coordinates window_size v} *)
let discrete_color_segment_2_3 = fun (u:float array) (m:float array array array) (fc:float array -> float array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_color_paint_2_3 function_color_segment_2_3 u m fc a b coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R^2 dans TR^3}
{1 Drawings from R^2 to TR^3}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v field_paint_2_3 paint vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_paint_2_3 = fun paint (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let ( aa , aaa , bb , bbb , hh , xx , yy ) = prepare_2_3 u a b nsteps ngrid coordinates size in
  for i = 0 to ngrid  - 1 do
   let g = function x -> f [| x ; yy.(i) |]
   and h = function x -> f [| xx.(i) ; x |] in
    paint Graphics.white Graphics.green u g [| a.(0) ; b.(0) |] nsteps ngrid coordinates size ;
    paint Graphics.white Graphics.green u h [| a.(1) ; b.(1) |] nsteps ngrid coordinates size ;
  done ;;

(** {v field_draw_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_draw_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 field_paint_2_3 field_partial_draw_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v field_segment_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_segment_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 field_paint_2_3 field_partial_segment_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v field_smooth_2_3 gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_smooth_2_3 = fun (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 field_paint_2_3 ( field_partial_smooth_1_3 gamma ) u f a b nsteps ngrid coordinates size ;;

(** {v field_oversample_2_3 over_sampling_factor gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_oversample_2_3 = fun (n:int) (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 field_paint_2_3 ( field_partial_oversample_1_3 n gamma ) u f a b nsteps ngrid coordinates size ;;


(** {v field_reverse_paint_2_3 paint vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_reverse_paint_2_3 = fun paint (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let ( aa , aaa , bb , bbb , hh , xx , yy ) = prepare_2_3 u a b nsteps ngrid coordinates size in
  for i = ngrid  - 1 downto 0 do
   let g = function x -> f [| x ; yy.(i) |]
   and h = function x -> f [| xx.(i) ; x |] in
    paint Graphics.white Graphics.green u g [| a.(0) ; b.(0) |] nsteps ngrid coordinates size ;
    paint Graphics.white Graphics.green u h [| a.(1) ; b.(1) |] nsteps ngrid coordinates size ;
  done ;;

(** {v field_reverse_draw_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_reverse_draw_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 field_reverse_paint_2_3 field_partial_draw_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v field_reverse_segment_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_reverse_segment_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 field_reverse_paint_2_3 field_partial_segment_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v field_reverse_smooth_2_3 gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_reverse_smooth_2_3 = fun (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 field_reverse_paint_2_3 ( field_partial_smooth_1_3 gamma ) u f a b nsteps ngrid coordinates size ;;

(** {v field_reverse_oversample_2_3 over_sampling_factor gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let field_reverse_oversample_2_3 = fun (n:int) (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 field_reverse_paint_2_3 ( field_partial_oversample_1_3 n gamma ) u f a b nsteps ngrid coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_field_paint_2_3 paint vector matrix a b frame_coordinates window_size v} *)
let discrete_field_paint_2_3 = fun paint (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let scale_x = ( float c ) /. ( b.(0) -. a.(0) )
  and scale_y = ( float r ) /. ( b.(1) -. a.(1) )
  and f = Infinitesimal.interpol_2 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) m in
   let g = function z -> f ( ( z.(0) -. a.(0) ) *. scale_x ) ( ( z.(1) -. a.(1) ) *. scale_y ) in
    paint u g a b ( max r c ) ( min r c ) coordinates size ;;

(** {v discrete_field_draw_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_field_draw_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_2_3 field_draw_2_3 u m a b coordinates size ;;

(** {v discrete_field_segment_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_field_segment_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_2_3 field_segment_2_3 u m a b coordinates size ;;

(** {v discrete_field_smooth_2_3 gamma vector matrix a b frame_coordinates window_size v} *)
let discrete_field_smooth_2_3 = fun (gamma:float -> float) (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_2_3 ( field_smooth_2_3 gamma ) u m a b coordinates size ;;

(** {v discrete_reverse_field_draw_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_field_reverse_draw_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_2_3 field_reverse_draw_2_3 u m a b coordinates size ;;

(** {v discrete_reverse_field_segment_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_field_reverse_segment_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_2_3 field_reverse_segment_2_3 u m a b coordinates size ;;

(** {v discrete_reverse_field_smooth_2_3 gamma vector matrix a b frame_coordinates window_size v} *)
let discrete_field_reverse_smooth_2_3 = fun (gamma:float -> float) (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_2_3 ( field_reverse_smooth_2_3 gamma ) u m a b coordinates size ;;




(** {C § } *)
(** 
{1 Dessins de R^2 dans R^n ¤ TR^3}
{1 Drawings from R^2 to R^n ¤ TR^3}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)





(** {v frame_field_paint_2_3 paint vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_paint_2_3 = fun paint (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let ( aa , aaa , bb , bbb , hh , xx , yy ) = prepare_2_3 u a b nsteps ngrid coordinates size in
  for i = 0 to ngrid  - 1 do
   let g = function x -> f [| x ; yy.(i) |]
   and h = function x -> f [| xx.(i) ; x |] in
    paint Graphics.white u g [| a.(0) ; b.(0) |] nsteps ngrid coordinates size ;
    paint Graphics.white u h [| a.(1) ; b.(1) |] nsteps ngrid coordinates size ;
  done ;;

(** {v frame_field_draw_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_draw_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_2_3 frame_field_partial_draw_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v frame_field_segment_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_segment_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_2_3 frame_field_partial_segment_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v frame_field_smooth_2_3 gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_smooth_2_3 = fun (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_2_3 ( frame_field_partial_smooth_1_3 gamma ) u f a b nsteps ngrid coordinates size ;;

(** {v frame_field_oversample_2_3 over_sampling_factor gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_oversample_2_3 = fun (n:int) (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_2_3 ( frame_field_partial_oversample_1_3 n gamma ) u f a b nsteps ngrid coordinates size ;;


(** {v frame_field_reverse_paint_2_3 paint vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_reverse_paint_2_3 = fun paint (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 let ( aa , aaa , bb , bbb , hh , xx , yy ) = prepare_2_3 u a b nsteps ngrid coordinates size in
  for i = ngrid  - 1 downto 0 do
   let g = function x -> f [| x ; yy.(i) |]
   and h = function x -> f [| xx.(i) ; x |] in
    paint Graphics.white u g [| a.(0) ; b.(0) |] nsteps ngrid coordinates size ;
    paint Graphics.white u h [| a.(1) ; b.(1) |] nsteps ngrid coordinates size ;
  done ;;

(** {v frame_field_reverse_draw_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_reverse_draw_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_reverse_paint_2_3 frame_field_partial_draw_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v frame_field_reverse_segment_2_3 vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_reverse_segment_2_3 = fun (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_reverse_paint_2_3 frame_field_partial_segment_1_3 u f a b nsteps ngrid coordinates size ;;

(** {v frame_field_reverse_smooth_2_3 gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_reverse_smooth_2_3 = fun (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_reverse_paint_2_3 ( frame_field_partial_smooth_1_3 gamma ) u f a b nsteps ngrid coordinates size ;;

(** {v frame_field_reverse_oversample_2_3 over_sampling_factor gamma vector function a b nsteps ngrid frame_coordinates window_size v} *)
let frame_field_reverse_oversample_2_3 = fun (n:int) (gamma:float -> float) (u:float array) (f:float array -> float array) (a:float array) (b:float array) (nsteps:int) (ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_reverse_paint_2_3 ( frame_field_partial_oversample_1_3 n gamma ) u f a b nsteps ngrid coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_frame_field_paint_2_3 paint vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_paint_2_3 = fun paint (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let scale_x = ( float c ) /. ( b.(0) -. a.(0) )
  and scale_y = ( float r ) /. ( b.(1) -. a.(1) )
  and f = Infinitesimal.interpol_2 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) m in
   let g = function z -> f ( ( z.(0) -. a.(0) ) *. scale_x ) ( ( z.(1) -. a.(1) ) *. scale_y ) in
    paint u g a b ( max r c ) ( min r c ) coordinates size ;;

(** {v discrete_frame_field_draw_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_draw_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_2_3 frame_field_draw_2_3 u m a b coordinates size ;;

(** {v discrete_frame_field_segment_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_segment_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_2_3 frame_field_segment_2_3 u m a b coordinates size ;;

(** {v discrete_frame_field_smooth_2_3 gamma vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_smooth_2_3 = fun (gamma:float -> float) (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_2_3 ( frame_field_smooth_2_3 gamma ) u m a b coordinates size ;;

(** {v discrete_frame_field_reverse_draw_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_reverse_draw_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_2_3 frame_field_reverse_draw_2_3 u m a b coordinates size ;;

(** {v discrete_frame_field_reverse_segment_2_3 vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_reverse_segment_2_3 = fun (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_2_3 frame_field_reverse_segment_2_3 u m a b coordinates size ;;

(** {v discrete_frame_field_reverse_smooth_2_3 gamma vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_reverse_smooth_2_3 = fun (gamma:float -> float) (u:float array) (m:float array array array) (a:float array) (b:float array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_2_3 ( frame_field_reverse_smooth_2_3 gamma ) u m a b coordinates size ;;




(** {C § } *)
(** 
{1 Sections de TR^2}
{1 Sections of TR^2}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v field_paint_2_2 arrow_paint function half_ngrid frame_coordinates window_size v} *)
let field_paint_2_2 = fun arrow_paint (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ( r , ngrid , hh , vv , hhh , vvv , h_h , v_v , h_size , v_size , h_margin , v_margin , h_factor , v_factor , h_function , v_function , x_function , y_function , vx_function , vy_function ) = prepare_section_2_2 f half_ngrid coordinates size in
  init_section_2_2 hh vv hhh vvv h_size v_size size ;
  for i = 0 to ngrid do
   let x = x_function i
   and ii = h_factor * i in
    for j = 0 to ngrid do
     let y = y_function j
     and jj = v_factor * j in
      let z = f [| x ; y |] in
       let xx = vx_function ( z.(0) )
       and yy = vy_function ( z.(1) ) in
        arrow arrow_paint ( ii + h_margin ) ( jj + v_margin ) ( int_of_float xx ) ( int_of_float yy ) ;
    done ;
  done ;
  terminate_section_2_2 hh vv hhh vvv h_h v_v h_margin v_margin h_function v_function coordinates size ;;

(** {v field_draw_2_2 function half_ngrid frame_coordinates window_size v} *)
let field_draw_2_2 = fun (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ap = fun a b c d ->
  begin
   Graphics.set_color Graphics.white ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_paint_2_2 ap f half_ngrid coordinates size ;;

(** {v field_smooth_2_2 gamma function half_ngrid frame_coordinates window_size v} *)
let field_smooth_2_2 = fun (gamma:float -> float) (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 field_paint_2_2 ( draw_smooth_segment gamma Graphics.white ) f half_ngrid coordinates size ;;

(** {v field_partial_paint_2_2 ngrid h_margin v_margin h_factor v_factor x_function y_function vx_function vy_function arrow_paint color function half_ngrid frame_coordinates window_size v} *)
let field_partial_paint_2_2 = fun ngrid h_margin v_margin h_factor v_factor x_function y_function vx_function vy_function arrow_paint (color:Graphics.color) (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 for i = 0 to ngrid do
  let x = x_function i
  and ii = h_factor * i in
   for j = 0 to ngrid do
    let y = y_function j
    and jj = v_factor * j in
     let z = f [| x ; y |] in
      let xx = vx_function ( z.(0) )
      and yy = vy_function ( z.(1) ) in
       arrow ( arrow_paint color ) ( ii + h_margin ) ( jj + v_margin ) ( int_of_float xx ) ( int_of_float yy ) ;
   done ;
 done ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_field_paint_2_2 paint vector matrix a b frame_coordinates window_size v} *)
let discrete_field_paint_2_2 = fun paint (m:float array array array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let scale_x = ( float c ) /. ( coordinates.(1) -. coordinates.(0) )
  and scale_y = ( float r ) /. ( coordinates.(3) -. coordinates.(2) )
  and f = Infinitesimal.interpol_2 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) m in
   let g = function z -> f ( ( z.(0) -. coordinates.(0) ) *. scale_x ) ( ( z.(1) -. coordinates.(2) ) *. scale_y ) in
    paint g ( ( min r c ) / 2 ) coordinates size ;;

(** {v discrete_field_draw_2_2 vector matrix a b frame_coordinates window_size v} *)
let discrete_field_draw_2_2 = fun (m:float array array array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_2_2 field_draw_2_2 m coordinates size ;;

(** {v discrete_field_smooth_2_2 gamma vector matrix a b frame_coordinates window_size v} *)
let discrete_field_smooth_2_2 = fun (gamma:float -> float) (m:float array array array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_2_2 ( field_smooth_2_2 gamma ) m coordinates size ;;




(** {C § } *)
(** 
{1 Sections de TR^2 ¤ R^n}
{1 Sections of TR^2 ¤ R^n}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v frame_field_paint_2_2 arrow_paint function half_ngrid frame_coordinates window_size v} *)
let frame_field_paint_2_2 = fun arrow_paint (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ( r , ngrid , hh , vv , hhh , vvv , h_h , v_v , h_size , v_size , h_margin , v_margin , h_factor , v_factor , h_function , v_function , x_function , y_function , vx_function , vy_function ) = prepare_section_2_2 f half_ngrid coordinates size in
  init_section_2_2 hh vv hhh vvv h_size v_size size ;
  for k = 0 to r / 2 - 1 do
   let kk = 2 * k in
    let g = function z -> let zz = f z in [| zz.(kk) ; zz.( kk + 1 ) |] in
     field_partial_paint_2_2 ngrid h_margin v_margin h_factor v_factor x_function y_function vx_function vy_function arrow_paint ( color_list.( k mod 5 ) ) g half_ngrid coordinates size ;
  done ;
  terminate_section_2_2 hh vv hhh vvv h_h v_v h_margin v_margin h_function v_function coordinates size ;;

(** {v frame_field_draw_2_2 function half_ngrid frame_coordinates window_size v} *)
let frame_field_draw_2_2 = fun (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_paint_2_2 ap f half_ngrid coordinates size ;;

(** {v frame_field_smooth_2_2 gamma function half_ngrid frame_coordinates window_size v} *)
let frame_field_smooth_2_2 = fun (gamma:float -> float) (f:float array-> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_2_2 ( draw_smooth_segment gamma ) f half_ngrid coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_frame_field_paint_2_2 paint vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_paint_2_2 = fun paint (m:float array array array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let scale_x = ( float c ) /. ( coordinates.(1) -. coordinates.(0) )
  and scale_y = ( float r ) /. ( coordinates.(3) -. coordinates.(2) )
  and f = Infinitesimal.interpol_2 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) m in
   let g = function z -> f ( ( z.(0) -. coordinates.(0) ) *. scale_x ) ( ( z.(1) -. coordinates.(2) ) *. scale_y ) in
    paint g ( ( min r c ) / 2 - 1 ) coordinates size ;;


(** {v discrete_frame_field_draw_2_2 vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_draw_2_2 = fun (m:float array array array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_2_2 frame_field_draw_2_2 m coordinates size ;;

(** {v discrete_frame_field_smooth_2_2 gamma vector matrix a b frame_coordinates window_size v} *)
let discrete_frame_field_smooth_2_2 = fun (gamma:float -> float) (m:float array array array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_2_2 ( frame_field_smooth_2_2 gamma ) m coordinates size ;;




(** {C § } *)
(** 
{1 Sections de TR^3}
{1 Sections of TR^3}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v field_paint_3_3 arrow_paint vector color1 color2 function ngrid frame_coordinates window_size v} *)
let field_paint_3_3 = fun arrow_paint (u:float array) (color1:Graphics.color) (color2:Graphics.color) (f:float array -> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ( ng , shift , h_size , v_size , v , w , xx , yy , zz , x_function , y_function , vx_function , vy_function ) = prepare_section_3_3 u f half_ngrid coordinates size in
  init_section_3_3 h_size v_size size ;
  for i = 0 to ng do
   let couleur = mix_color ( ( float i ) /. ( float ng ) ) color1 color2
   and x = xx.(i) in
    for j = 0 to ng do
     let y = yy.(j) in
      for k = 0 to ng do
       let z = zz.(k) in
        let pos = [| x ; y ; z |] in
         let xyz = f pos in
          shift.(0) <- Matrix.vector_float_scal_prod v pos ;
          shift.(1) <- Matrix.vector_float_scal_prod w pos ;
          let x_x = Matrix.vector_float_scal_prod xyz v
          and y_y = Matrix.vector_float_scal_prod xyz w in
           arrow ( arrow_paint couleur ) ( x_function shift.(0) ) ( y_function shift.(1) ) ( vx_function x_x ) ( vy_function y_y ) ;
      done ;
    done ;
  done ;;

(** {v field_draw_3_3 vector color1 color2 function ngrid frame_coordinates window_size v} *)
let field_draw_3_3 = fun (u:float array) (color1:Graphics.color) (color2:Graphics.color) (f:float array -> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  field_paint_3_3 ap u color1 color2 f half_ngrid coordinates size ;;

(** {v field_smooth_3_3 gamma vector color1 color2 function ngrid frame_coordinates window_size v} *)
let field_smooth_3_3 = fun (gamma:float -> float) (u:float array) (color1:Graphics.color) (color2:Graphics.color) (f:float array -> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 field_paint_3_3 ( draw_smooth_segment gamma ) u color1 color2 f half_ngrid coordinates size ;;

(** {v field_partial_paint_3_3 ng xx yy zz shift v w x_function y_function vx_function vy_function arrow_paint vector color1 color2 function ngrid frame_coordinates window_size v} *)
let field_partial_paint_3_3 = fun ng xx yy zz shift v w x_function y_function vx_function vy_function arrow_paint (u:float array) (color1:Graphics.color) (color2:Graphics.color) (f:float array -> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 for i = 0 to ng do
  let couleur = mix_color ( ( float i ) /. ( float ng ) ) color1 color2
  and x = xx.(i) in
   for j = 0 to ng do
    let y = yy.(j) in
     for k = 0 to ng do
      let z = zz.(k) in
       let pos = [| x ; y ; z |] in
        let xyz = f pos in
         shift.(0) <- Matrix.vector_float_scal_prod v pos ;
         shift.(1) <- Matrix.vector_float_scal_prod w pos ;
         let x_x = Matrix.vector_float_scal_prod xyz v
         and y_y = Matrix.vector_float_scal_prod xyz w in
          arrow ( arrow_paint couleur ) ( x_function shift.(0) ) ( y_function shift.(1) ) ( vx_function x_x ) ( vy_function y_y ) ;
     done ;
   done ;
 done ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_field_paint_3_3 paint vector color1 color2 matrix frame_coordinates window_size v} *)
let discrete_field_paint_3_3 = fun paint (u:float array) (color1:Graphics.color) (color2:Graphics.color) (m:float array array array array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0)
 and s = Array.length m.(0).(0) in
  let scale_x = ( float s ) /. ( coordinates.(1) -. coordinates.(0) )
  and scale_y = ( float c ) /. ( coordinates.(3) -. coordinates.(2) )
  and scale_z = ( float r ) /. ( coordinates.(5) -. coordinates.(4) )
  and f = Infinitesimal.interpol_3 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) m in
   let g = function z -> f ( ( z.(0) -. coordinates.(0) ) *. scale_x ) ( ( z.(1) -. coordinates.(2) ) *. scale_y ) ( ( z.(2) -. coordinates.(4) ) *. scale_z ) in
    paint u color1 color2 g ( ( min r c ) / 2 ) coordinates size ;;

(** {v discrete_field_draw_3_3 vector color1 color2 matrix frame_coordinates window_size v} *)
let discrete_field_draw_3_3 = fun (u:float array) (color1:Graphics.color) (color2:Graphics.color) (m:float array array array array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_3_3 field_draw_3_3 u color1 color2 m coordinates size ;;

(** {v discrete_field_smooth_3_3 gamma vector color1 color2 matrix frame_coordinates window_size v} *)
let discrete_field_smooth_3_3 = fun (gamma:float -> float) (u:float array) (color1:Graphics.color) (color2:Graphics.color) (m:float array array array array) (coordinates:float array) (size:int array) ->
 discrete_field_paint_3_3 ( field_smooth_3_3 gamma ) u color1 color2 m coordinates size ;;




(** {C § } *)
(** 
{1 Sections de TR^3 ¤ R^n}
{1 Sections of TR^3 ¤ R^n}
*)
(** {C  } *)




(** {C § } *)
(** 
{2 Fonctions dessinées}
{2 Drawn functions}
*)
(** {C  } *)




(** {v frame_field_paint_3_3 paint vector color1 color2 function ngrid frame_coordinates window_size v} *)
let frame_field_paint_3_3 = fun paint (u:float array) (colors1:Graphics.color array) (colors2:Graphics.color array) (f:float array -> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ( ng , shift , h_size , v_size , v , w , xx , yy , zz , x_function , y_function , vx_function , vy_function ) = prepare_section_3_3 u f half_ngrid coordinates size
 and r = Array.length  ( f [| coordinates.(0) ; coordinates.(2) ; coordinates.(4) |] ) in
  init_section_3_3 h_size v_size size ;
  for k = 0 to r / 3 - 1 do
   let kk = 3 * k
   and k_k = k mod 5 in
    let g = function z -> let zz = f z in [| zz.(kk) ; zz.( kk + 1 ) ; zz.( kk + 2 ) |] in
     field_partial_paint_3_3 ng xx yy zz shift v w x_function y_function vx_function vy_function paint u colors1.(k_k) colors2.(k_k) g half_ngrid coordinates size ;
  done ;;

(** {v frame_field_draw_3_3 vector color1 color2 function ngrid frame_coordinates window_size v} *)
let frame_field_draw_3_3 = fun (u:float array) (colors1:Graphics.color array) (colors2:Graphics.color array) (f:float array -> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 let ap = fun color a b c d ->
  begin
   Graphics.set_color color ;
   Graphics.draw_segments [| (a,b,c,d) |]
  end in
  frame_field_paint_3_3 ap u colors1 colors2 f half_ngrid coordinates size ;;

(** {v frame_field_smooth_3_3 gamma vector color1 color2 function ngrid frame_coordinates window_size v} *)
let frame_field_smooth_3_3 = fun (gamma:float -> float) (u:float array) (colors1:Graphics.color array) (colors2:Graphics.color array) (f:float array -> float array) (half_ngrid:int) (coordinates:float array) (size:int array) ->
 frame_field_paint_3_3 ( draw_smooth_segment gamma ) u colors1 colors2 f half_ngrid coordinates size ;;




(** {C § } *)
(** 
{2 Dessins discrets}
{2 Discrete drawings}
*)
(** {C  } *)




(** {v discrete_frame_field_paint_3_3 paint vector color1 color2 matrix frame_coordinates window_size v} *)
let discrete_frame_field_paint_3_3 = fun paint (u:float array) (colors1:Graphics.color array) (colors2:Graphics.color array) (m:float array array array array) (coordinates:float array) (size:int array) ->
 let r = Array.length m
 and c = Array.length m.(0)
 and s = Array.length m.(0).(0) in
  let scale_x = ( float s ) /. ( coordinates.(1) -. coordinates.(0) )
  and scale_y = ( float c ) /. ( coordinates.(3) -. coordinates.(2) )
  and scale_z = ( float r ) /. ( coordinates.(5) -. coordinates.(4) )
  and f = Infinitesimal.interpol_3 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) m in
   let g = function z -> f ( ( z.(0) -. coordinates.(0) ) *. scale_x ) ( ( z.(1) -. coordinates.(2) ) *. scale_y ) ( ( z.(2) -. coordinates.(4) ) *. scale_z ) in
    paint u colors1 colors2 g ( ( min r c ) / 2 ) coordinates size ;;

(** {v discrete_frame_field_draw_3_3 vector color1 color2 matrix frame_coordinates window_size v} *)
let discrete_frame_field_draw_3_3 = fun (u:float array) (colors1:Graphics.color array) (colors2:Graphics.color array) (m:float array array array array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_3_3 frame_field_draw_3_3 u colors1 colors2 m coordinates size ;;

(** {v discrete_frame_field_smooth_3_3 gamma vector color1 color2 matrix frame_coordinates window_size v} *)
let discrete_frame_field_smooth_3_3 = fun (gamma:float -> float) (u:float array) (colors1:Graphics.color array) (colors2:Graphics.color array) (m:float array array array array) (coordinates:float array) (size:int array) ->
 discrete_frame_field_paint_3_3 ( frame_field_smooth_3_3 gamma ) u colors1 colors2 m coordinates size ;;





(** {C § § § } *)

Graphics.close_graph () ;;

end ;;


