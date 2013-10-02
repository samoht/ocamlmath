



module Widget = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module:

- utility functions in order to make interactive graphical windows,

- some dialog boxes.



{2 Conventions}



In order to activate a graphic element, one has to click in the area that it determines (with the left button of the pointing device),
casually twice.

The use is rather aimed at analogic metrology.



{2 Comments}



When rewriting a button, it useful to use messages with the same number of lines,
lines with fixed lengths, and a character font with fixed width.


For character string edition, controls similar to common controls are available,
given that not all of them are reachable, depending on the system :

- move to left end with [Ctl-A],

- delete left part with [Ctl-U],

- delete toward left with [Ctl-Space], [Ctl-H], [Ctl-R], [Ctl-X],

- move left with [Ctl-B], [Ctl-P],

- move right with [Ctl-F], [Ctl-N],

- delete toward right with [Ctl-S], [Ctl-Z],

- delete right part with [Ctl-K],

- move to right end with [Ctl-E].


The position of the edited character is at the change of the color of the text.



This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module :

- des fonctions utilitaires pour construire des fenêtres graphiques interactives,

- quelques boîtes de dialogues.



{2 Conventions}



Pour activer un élément graphique, il faut cliquer dans zone qu'il délimite (avec le bouton gauche du pointeur),
éventuellement deux fois.


L'utilisation visée est plutôt la métrologie analogique.



{2 Commentaires}



Quand on réécrit un bouton, il est utile que les messages utilisent le même nombre de lignes, 
des lignes de longueurs invariables et une police de caractères à chasse fixe.


Pour le modelage de chaînes de caractères, des commandes proches des commandes habituelles sont disponibles, 
sachant qu'elles ne sont pas toutes accessibles d'un système à l'autre :

- déplacement vers l'extrémité gauche avec [Ctl-A],

- effaçage de la partie gauche avec [Ctl-U],

- effaçage vers la gauche avec [Ctl-Space], [Ctl-H], [Ctl-R], [Ctl-X],

- déplacement vers la gauche avec [Ctl-B], [Ctl-P],

- déplacement vers la droite avec [Ctl-F], [Ctl-N],

- effaçage vers la droite avec [Ctl-S], [Ctl-Z],

- effaçage de la partie droite avec [Ctl-K],

- déplacement vers l'extrémité droite avec [Ctl-E].


La position du caractère courant est au changement de couleur du texte.



Ce module est distribué selon la même licence qu'Ocaml.


{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.3}
*)
(**
@version 0.3
*)
(**
@author Stéphane Grognet
*)
(**
@since 2012, 2013
*)




(** {C § } *)
(** 
{1 Utilitaires}
{1 Utilities}
*)
(** {C  } *)




open Util ;;
open Matrix ;;
open Readwrite ;;
open Infinitesimal ;;


(** {v dark_grey v} *)
let dark_grey = 65793 * 70 ;;

(** {v bee_v integer v} *)
let bee_v = function (x:int) ->
 433 * x / 250 ;;

(** {v half_bee_v integer v} *)
let half_bee_v = function (x:int) ->
 433 * x / 500 ;;

(** {v bee_h integer v} *)
let bee_h = function (x:int) ->
 250 * x / 433 ;;

(** {v double_bee_h integer v} *)
let double_bee_h = function (x:int) ->
 500 * x / 433 ;;

(** {v tour_split integer center_abscissa center_ordinate radius v} *)
let tour_split = fun (n:int) (x:float) (y:float) (r:float) ->
 let f = function x -> Infinitesimal.halfpi *. ( ( 2. +. 4. *. x ) /. ( float n ) -. 1. )
 and a = Array.make_matrix n 2 0. in
  for i = 0 to pred n do
   let row = a.(i) in
    row.(0) <- x +. r *. cos ( f ( float i ) ) ;
    row.(1) <- y +. r *. sin ( f ( float i ) ) ;
  done ;
  a ;;

(** {v ring_split integer center_abscissa center_ordinate inner_radius exterior_radius v} *)
let ring_split = fun (n:int) (x:int) (y:int) (r_in:int) (r_ext:int) -> 
 let xx = float x
 and yy = float y
 and ri = float r_in
 and re = float r_ext
 and f = function z -> ( int_of_float z.(0) , int_of_float z.(1) ) in
  let t_in = Array.map f ( tour_split n xx yy ri )
  and t_ext = Array.map f ( tour_split n xx yy re ) in
   Array.append t_in ( Util.reverse_array t_ext ) ;;
 
(** {v triangle_isobarycentre x y z v} *)
let triangle_isobarycentre = fun (x:int * int) (y:int * int) (z:int * int) ->
 [| ( fst x + fst y + fst z ) / 3 ; ( snd x + snd y + snd z ) / 3 |] ;;

(** {v triangle_barycentre weight x y z v} *)
let triangle_barycentre = fun (weight:float) (x:int * int) (y:int * int) (z:int * int) ->
 [| int_of_float ( weight *. ( float ( fst x ) ) +. ( 0.5 *. ( 1. -. weight ) ) *. ( float ( fst y + fst z ) ) ) ;
  int_of_float ( weight *. ( float ( snd x ) ) +. ( 0.5 *. ( 1. -. weight ) ) *. ( float ( snd y + snd z ) ) ) |] ;;

(** {v positive_argument x y v} *)
let positive_argument = fun (x:float) (y:float) ->
 if y > 0. then atan2 y x
 else
  if y < 0. then
   ( atan2 y x ) +. Infinitesimal.doublepi
  else
   if x > 0. then 0.
   else Infinitesimal.pi ;;




(** {C § } *)
(** 
{1 Interrupteurs}
{1 Switches}
*)
(** {C  } *)




(** {v rect_switch center_abscissa center_ordinate button_color text_color text v} The function returns the coordinates
of the bottom left corner, the coordinates of the upper right corner.

La fonction retourne les coordonnées du coin en bas à gauche, les coordonnées du point en haut à droite. *)
let rect_switch = fun (abscissa:int) (ordinate:int) (color:int) (text_color:int) (text:string array) ->
 Graphics.set_color Graphics.foreground ;
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let w = Matrix.vector_int_max widths
   and h = Matrix.vector_int_sum heights in
    let bw = min ( ( 7 * w ) / 5 ) ( w + 20 )
    and bh = min ( ( 8 * h ) / 5 ) ( h + 10 )
    and shift = ref 5 in
     let x = abscissa - bw / 2
     and y = ordinate - bh / 2 in
      Graphics.draw_rect x y bw bh ;
      Graphics.set_color color ;
      Graphics.fill_rect ( x + 1 ) ( y + 1 ) ( bw - 2 ) ( bh - 2 ) ;
      Graphics.set_color text_color ;
      for i = pred n downto 0 do
       let left = x + ( bw - widths.(i) - 2 ) / 2 in
        Graphics.moveto left ( y + !shift ) ;
        Graphics.draw_string text.(i) ;
        shift := !shift + heights.(i) ;
      done ;
      [| abscissa ; ordinate ; x ; y ; x + bw ; y + bh |] ;;

(** {v is_over_rect_button button_position abscissa ordinate v} *)
let is_over_rect_button = fun (button:int array) (x:int) (y:int) ->
 let table = [| x >= button.(2) ; x <= button.(4) ; y >= button.(3) ; y <= button.(5) |] in
  Array.fold_left ( && ) true table ;;


(** {v hat_switch center_abscissa center_ordinate button_color text_color text v} The function returns 
the position of the circonscript rectangle of the triangle.

La fonction retourne la position du rectangle circonscrit au triangle. *)
let hat_switch = fun (abscissa:int) (ordinate:int) (color:int) (text_color:int) (text:string array) ->
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let h = Matrix.vector_int_max heights
   and w = Matrix.vector_int_max widths in
    let coeff = double_bee_h h in
     let f = fun j z -> z + ( n - j ) * coeff in
      let width_measures = Array.mapi f widths in
       let wh = Matrix.vector_int_max width_measures  in
        let size = 5 + ( ( 11 * wh ) / 20 ) in
         let bw = 2 * size
         and x = abscissa - size
         and bh = bee_v size
         and shift = ref ( 1 + size / 5 ) in
          let xx = x + bw
          and y = ordinate - bh / 3 in
           let yy = y + bh in
            let sequence = [| ( x , y ) ; ( xx , y ) ; ( x + size , yy ) |] in 
             Graphics.set_color color ;
             Graphics.fill_poly sequence ;
             Graphics.set_color Graphics.foreground ;
             Graphics.draw_poly sequence ;
             Graphics.set_color text_color ;
             for i = pred n downto 0 do
              let left = x + ( 7 * w - 4 * widths.(i) ) / 8 in
               Graphics.moveto left ( y + !shift ) ;
               Graphics.draw_string text.(i) ;
               shift := !shift + heights.(i) ;
             done ;
             [| abscissa ; ordinate ; x ; y ; xx ; yy |] ;;

(** {v is_over_hat_button button_position abscissa ordinate v} *)
let is_over_hat_button = fun (button:int array) (x:int) (y:int) ->
 let yy = button.(3) in
  let dy = y - yy in
   let table = [| x >= button.(2) + ( bee_h dy ) ; x <= button.(4) - ( bee_h dy ) ; y >= yy ; y <= button.(5) |] in
    Array.fold_left ( && ) true table ;;


(** {v cup_switch center_abscissa center_ordinate button_color text_color text v} The function returns 
the position of the circonscript rectangle of the triangle.

La fonction retourne la position du rectangle circonscrit au triangle. *)
let cup_switch = fun (abscissa:int) (ordinate:int) (color:int) (text_color:int) (text:string array) ->
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let w = Matrix.vector_int_max widths
   and h = Matrix.vector_int_max heights in
    let coeff = double_bee_h h in
     let f = fun j z -> z + ( succ j ) * coeff in
      let width_measures = Array.mapi f widths in
       let wh = Matrix.vector_int_max width_measures  in
        let size = 5 + ( ( 11 * wh ) / 20 ) in
         let bw = 2 * size
         and x = abscissa - size
         and bh = bee_v size
         and shift = ref ( - size / 10 ) in
          let xx = x + bw
          and y = ordinate + bh / 3 in
           let yy = y - bh in
            let sequence = [| ( x , y ) ; ( xx , y ) ; ( x + size , yy ) |] in 
             Graphics.set_color color ;
             Graphics.fill_poly sequence ;
             Graphics.set_color Graphics.foreground ;
             Graphics.draw_poly sequence ;
             Graphics.set_color text_color ;
             for i = 0 to pred n do
              shift := !shift - heights.(i) ;
              let left = x + ( 7 * w - 4 * widths.(i) ) / 8 in
                Graphics.moveto left ( y + !shift ) ;
                Graphics.draw_string text.(i) ;
             done ;
             [| abscissa ; ordinate ; x ; y ; xx ; yy |] ;;

(** {v is_over_cup_button button_position abscissa ordinate v} *)
let is_over_cup_button = fun (button:int array) (x:int) (y:int) ->
 let yy = button.(3) in
  let dy = y - yy in
   let table = [| x >= button.(2) - ( bee_h dy ) ; x <= button.(4) + ( bee_h dy ) ; y >= button.(5) ; y <= yy |] in
    Array.fold_left ( && ) true table ;;


(** {v right_switch center_abscissa center_ordinate button_color text_color text v} The function returns 
the position of the circonscript rectangle of the triangle.

La fonction retourne la position du rectangle circonscrit au triangle. *)
let right_switch = fun (abscissa:int) (ordinate:int) (color:int) (text_color:int) (text:string array) ->
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let h = Matrix.vector_int_max heights in
    let coeff = double_bee_h h in
     let f = fun j z -> z + ( abs ( n / 2 - j ) ) * coeff in
      let width_measures = Array.mapi f widths in
       let wh = Matrix.vector_int_max width_measures  in
        let size = 5 + ( ( 19 * wh ) / 20 ) in
         let bw = 2 * size
         and y = ordinate - size
         and bh = bee_v size
         and shift = ref ( 1 + 11 * size / 20 ) in
          let x = abscissa - bh / 3
          and yy = y + bh in
           let xx = x + bw in
            let sequence = [| ( x , y ) ; ( xx , y + size ) ; ( x , yy ) |] in 
             Graphics.set_color color ;
             Graphics.fill_poly sequence ;
             Graphics.set_color Graphics.foreground ;
             Graphics.draw_poly sequence ;
             Graphics.set_color text_color ;
             for i = pred n downto 0 do
              let left = x + size / 5 in
               Graphics.moveto left ( y + !shift ) ;
               Graphics.draw_string text.(i) ;
               shift := !shift + heights.(i) ;
             done ;
             [| abscissa ; ordinate ; x ; y ; xx ; yy |] ;;

(** {v is_over_right_button button_position abscissa ordinate v} *)
let is_over_right_button = fun (button:int array) (x:int) (y:int) ->
 let xx = button.(2) in
  let dx = x - xx in
   let table = [| x >= xx ; x <= button.(4) ; y >= button.(3) + ( bee_h dx ) ; y <= button.(5) - ( bee_h dx ) |] in
    Array.fold_left ( && ) true table ;;


(** {v left_switch center_abscissa center_ordinate button_color text_color text v} The function returns 
the position of the circonscript rectangle of the triangle.

La fonction retourne la position du rectangle circonscrit au triangle. *)
let left_switch = fun (abscissa:int) (ordinate:int) (color:int) (text_color:int) (text:string array) ->
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let h = Matrix.vector_int_max heights
   and w = Matrix.vector_int_max widths in
    let coeff = double_bee_h h in
     let f = fun j z -> z + ( abs ( n / 2 - j ) ) * coeff in
      let width_measures = Array.mapi f widths in
       let wh = Matrix.vector_int_max width_measures  in
        let size = 5 + ( ( 19 * wh ) / 20 ) in
         let bw = 2 * size
         and bh = bee_v size
         and y = ordinate - size
         and shift = ref ( 1 + 11 * size / 20 ) in
          let x = abscissa + bw / 3
          and yy = y + bh in
           let xx = x - bw in
            let sequence = [| ( x , y ) ; ( xx , y + size ) ; ( x , yy ) |] in 
             Graphics.set_color color ;
             Graphics.fill_poly sequence ;
             Graphics.set_color Graphics.foreground ;
             Graphics.draw_poly sequence ;
             Graphics.set_color text_color ;
             for i = pred n downto 0 do
              let left = x - ( 18 * w + 3 * size ) / 20 in
               Graphics.moveto left ( y + !shift ) ;
               Graphics.draw_string text.(i) ;
               shift := !shift + heights.(i) ;
             done ;
             [| abscissa ; ordinate ; x ; y ; xx ; yy |] ;;

(** {v is_over_left_button button_position abscissa ordinate v} *)
let is_over_left_button = fun (button:int array) (x:int) (y:int) ->
 let xx = button.(2) in
  let dx = x - xx in
   let table = [| x <= xx ; x >= button.(4) ; y >= button.(3) - ( bee_h dx ) ; y <= button.(5) + ( bee_h dx ) |] in
    Array.fold_left ( && ) true table ;;


(** {v ll_switch bottom_left_corner_abscissa bottom_left_corner_ordinate button_color text_color text v} The function returns
the position of the circonscript rectangle of the triangle.

La fonction retourne la position du rectangle circonscrit au triangle. *)
let ll_switch = fun (x:int) (y:int) (color:int) (text_color:int) (text:string array) ->
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let h = Matrix.vector_int_max heights in
    let f = fun j z -> z + ( n - j ) * h in
     let width_measures = Array.mapi f widths in
      let wh = Matrix.vector_int_max width_measures  in
       let size = 5 + ( ( 12 * wh ) / 20 ) in
        let bw = 2 * size
        and shift = ref ( 1 + size / 10 ) in
         let xx = x + bw
         and yy = y + bw in
          let sequence = [| ( x , y ) ; ( xx , y ) ; ( x , yy ) |] in 
           Graphics.set_color color ;
           Graphics.fill_poly sequence ;
           Graphics.set_color Graphics.foreground ;
           Graphics.draw_poly sequence ;
           Graphics.set_color text_color ;
           for i = pred n downto 0 do
            let left = x + size / 10 in
             Graphics.moveto left ( y + !shift ) ;
             Graphics.draw_string text.(i) ;
             shift := !shift + heights.(i) ;
           done ;
           [| x ; y ; xx ; yy |] ;;

(** {v is_over_ll_button button_position abscissa ordinate v} *)
let is_over_ll_button = fun (button:int array) (x:int) (y:int) ->
 let xx = button.(0) in
  let dx = x - xx in
   let table = [| x >= xx ; x <= button.(2) ; y >= button.(1) ; y <= button.(3) - dx |] in
    Array.fold_left ( && ) true table ;;


(** {v lr_switch bottom_right_corner_abscissa bottom_right_corner_ordinate button_color text_color text v} The function returns
the position of the circonscript rectangle of the triangle.

La fonction retourne la position du rectangle circonscrit au triangle. *)
let lr_switch = fun (x:int) (y:int) (color:int) (text_color:int) (text:string array) ->
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let h = Matrix.vector_int_max heights in
    let f = fun j z -> z + ( n - j ) * h in
     let width_measures = Array.mapi f widths in
      let wh = Matrix.vector_int_max width_measures  in
       let size = 5 + ( ( 12 * wh ) / 20 ) in
        let bw = 2 * size
        and shift = ref ( 1 + size / 10 ) in
         let xx = x - bw
         and yy = y + bw in
          let sequence = [| ( x , y ) ; ( xx , y ) ; ( x , yy ) |] in 
           Graphics.set_color color ;
           Graphics.fill_poly sequence ;
           Graphics.set_color Graphics.foreground ;
           Graphics.draw_poly sequence ;
           Graphics.set_color text_color ;
           for i = pred n downto 0 do
            let left = x - ( 29 * size ) / 20 in
             Graphics.moveto left ( y + !shift ) ;
             Graphics.draw_string text.(i) ;
             shift := !shift + heights.(i) ;
           done ;
           [| x ; y ; xx ; yy |] ;;

(** {v is_over_lr_button button_position abscissa ordinate v} *)
let is_over_lr_button = fun (button:int array) (x:int) (y:int) ->
 let xx = button.(0) in
  let dx = x - xx in
   let table = [| x <= xx ; x >= button.(2) ; y >= button.(1) ; y <= button.(3) + dx |] in
    Array.fold_left ( && ) true table ;;


(** {v ul_switch upper_left_corner_abscissa upper_left_corner_ordinate button_color text_color text v} The function returns
the position of the circonscript rectangle of the triangle.

La fonction retourne la position du rectangle circonscrit au triangle. *)
let ul_switch = fun (x:int) (y:int) (color:int) (text_color:int) (text:string array) ->
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let h = Matrix.vector_int_max heights in
    let f = fun j z -> z + j * h in
     let width_measures = Array.mapi f widths in
      let wh = Matrix.vector_int_max width_measures  in
       let size = 5 + ( ( 12 * wh ) / 20 ) in
        let bw = 2 * size
        and shift = ref ( 1 + size / 10 ) in
         let xx = x + bw
         and yy = y - bw in
          let sequence = [| ( x , y ) ; ( xx , y ) ; ( x , yy ) |] in 
           Graphics.set_color color ;
           Graphics.fill_poly sequence ;
           Graphics.set_color Graphics.foreground ;
           Graphics.draw_poly sequence ;
           Graphics.set_color text_color ;
           for i = 0 to pred n do
            let left = x + size / 10 in
             shift := !shift + heights.(i) ;
             Graphics.moveto left ( y - !shift ) ;
             Graphics.draw_string text.(i) ;
           done ;
           [| x ; y ; xx ; yy |] ;;

(** {v is_over_ul_button button_position abscissa ordinate v} *)
let is_over_ul_button = fun (button:int array) (x:int) (y:int) ->
 let xx = button.(0) in
  let dx = x - xx in
   let table = [| x >= xx ; x <= button.(2) ; y <= button.(1) ; y >= button.(3) + dx |] in
    Array.fold_left ( && ) true table ;;


(** {v ur_switch upper_right_corner_abscissa upper_right_corner_ordinate button_color text_color text v} The function returns
the position of the circonscript rectangle of the triangle.

La fonction retourne la position du rectangle circonscrit au triangle. *)
let ur_switch = fun (x:int) (y:int) (color:int) (text_color:int) (text:string array) ->
 let n = Array.length text
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts in
   let h = Matrix.vector_int_max heights in
    let f = fun j z -> z + ( n - j ) * h in
     let width_measures = Array.mapi f widths in
      let wh = Matrix.vector_int_max width_measures  in
       let size = 5 + ( ( 13 * wh ) / 20 ) in
        let bw = 2 * size
        and shift = ref ( 1 + size / 5 ) in
         let xx = x - bw
         and yy = y - bw in
          let sequence = [| ( x , y ) ; ( xx , y ) ; ( x , yy ) |] in 
           Graphics.set_color color ;
           Graphics.fill_poly sequence ;
           Graphics.set_color Graphics.foreground ;
           Graphics.draw_poly sequence ;
           Graphics.set_color text_color ;
           for i = 0 to pred n do
            let left = x - ( 27 * size ) / 20 in
             Graphics.moveto left ( y - !shift ) ;
             Graphics.draw_string text.(i) ;
             shift := !shift + heights.(i) ;
           done ;
           [| x ; y ; xx ; yy |] ;;

(** {v is_over_ur_button button_position abscissa ordinate v} *)
let is_over_ur_button = fun (button:int array) (x:int) (y:int) ->
 let xx = button.(2)
 and yy = button.(1) in
  let dx = x - xx in
   let table = [| x <= button.(0) ; x >= xx ; y <= yy ; y >= yy - dx |] in
    Array.fold_left ( && ) true table ;;




(** {C § } *)
(** 
{1 Sélecteurs}
{1 Selectors}
*)
(** {C  } *)




(** {v polygon_selector center_abscissa center_ordinate small_radius button_color color text_color selection_color text selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the size of the choice, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, la taille du choix, la sélection. *)
let polygon_selector = fun (x:int) (y:int) (small_radius:int) (max_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (text:string array) (selection:int) ->
 let n = Array.length text
 and center = ( x , y )
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts
  and nn = succ n in
   let w = Matrix.vector_int_max widths
   and h = Matrix.vector_int_max heights
   and n_n = n + nn in
    let wh = ( ( max w h ) * ( 30 + 5 * n ) ) / ( 30 + n ) in
     let radius = min ( small_radius + wh ) max_radius in
      let p = ring_split nn x y small_radius radius in
       Graphics.set_color color ;
       Graphics.fill_poly p ;
       Graphics.set_color button_color ;
       Graphics.fill_circle x y small_radius ;
       for i = 0 to pred n do
        if i = selection then
         Graphics.set_color selection_color
        else
         Graphics.set_color text_color ;
        let position = p.( n + i + 2 mod n_n )
        and follower = p.( n + i + 1 mod n_n ) in
         let place = triangle_barycentre ( ( float ( 99 * n + wh ) ) ** (-0.25) ) center position follower in
          Graphics.moveto ( place.(0) - ( 2 * widths.(i) / 5 ) ) ( place.(1) - ( 3 * heights.(i) ) / 5 ) ;
          Graphics.draw_string text.(i) ;
       done ;
       [| x ; y ; small_radius ; radius ; n ; selection |] ;; 


(** {v cursor_polygon_selector center_abscissa center_ordinate small_radius button_color color text_color selection_color text selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the size of the choice, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, la taille du choix, la sélection. *)
let cursor_polygon_selector = fun (x:int) (y:int) (small_radius:int) (max_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (cursor_color:int) (text:string array) (selection:int) ->
 let n = Array.length text
 and center = ( x , y )
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts
  and nn = succ n in
   let w = Matrix.vector_int_max widths
   and h = Matrix.vector_int_max heights
   and n_n = n + nn in
    let wh = ( ( max w h ) * ( 30 + 5 * n ) ) / ( 30 + n ) in
     let radius = min ( small_radius + wh ) max_radius in
      let p = ring_split nn x y small_radius radius in
       Graphics.set_color color ;
       Graphics.fill_poly p ;
       Graphics.set_color button_color ;
       Graphics.fill_circle x y small_radius ;
       for i = 0 to pred n do
        let position = p.( n + i + 2 mod n_n )
        and follower = p.( n + i + 1 mod n_n ) in
         if i = selection then
          begin
           Graphics.set_color cursor_color ;
           Graphics.fill_poly [| center ; p.( n - i ) ; p.( n - i - 1 mod n ) |] ;
           Graphics.set_color selection_color ;
           Graphics.draw_segments [| ( x , y , fst position, snd position ) ; ( x , y , fst follower, snd follower ) |] ;
          end
         else
          Graphics.set_color text_color ;
          let place = triangle_barycentre ( ( float ( 99 * n + wh ) ) ** (-0.25) ) center position follower in
           Graphics.moveto ( place.(0) - ( 2 * widths.(i) / 5 ) ) ( place.(1) - ( 3 * heights.(i) ) / 5 ) ;
           Graphics.draw_string text.(i)
       done ;
       [| x ; y ; small_radius ; radius ; n ; selection |] ;; 


(** {v round_selector center_abscissa center_ordinate small_radius button_color color text_color selection_color text selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the size of the choice, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, la taille du choix, la sélection. *)
let round_selector = fun (x:int) (y:int) (small_radius:int) (max_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (text:string array) (selection:int) ->
 let n = Array.length text
 and center = ( x , y )
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts
  and nn = succ n in
   let w = Matrix.vector_int_max widths
   and h = Matrix.vector_int_max heights
   and n_n = n + nn in
    let wh = ( ( max w h ) * ( 30 + 5 * n ) ) / ( 30 + n ) in
     let radius = min ( small_radius + wh ) max_radius in
      let p = ring_split nn x y small_radius radius in
       Graphics.set_color color ;
       Graphics.fill_circle x y radius ;
       Graphics.set_color button_color ;
       Graphics.fill_circle x y small_radius ;
       for i = 0 to pred n do
        if i = selection then
         Graphics.set_color selection_color
        else
         Graphics.set_color text_color ;
         let position = p.( n + i + 2 mod n_n )
         and follower = p.( n + i + 1 mod n_n ) in
          let place = triangle_barycentre ( ( float ( 99 * n + wh ) ) ** (-0.25) ) center position follower in
           Graphics.moveto ( place.(0) - ( 2 * widths.(i) / 5 ) ) ( place.(1) - ( 3 * heights.(i) ) / 5 ) ;
           Graphics.draw_string text.(i) ;
       done ;
       [| x ; y ; small_radius ; radius ; n ; selection |] ;; 


(** {v cursor_round_selector center_abscissa center_ordinate small_radius button_color color text_color selection_color text selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the size of the choice, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, la taille du choix, la sélection. *)
let cursor_round_selector = fun (x:int) (y:int) (small_radius:int) (max_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (cursor_color:int) (text:string array) (selection:int) ->
 let n = Array.length text
 and center = ( x , y )
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts
  and nn = succ n in
   let w = Matrix.vector_int_max widths
   and h = Matrix.vector_int_max heights
   and n_n = n + nn in
    let wh = ( ( max w h ) * ( 30 + 5 * n ) ) / ( 30 + n ) in
     let radius = min ( small_radius + wh ) max_radius in
      let p = ring_split nn x y small_radius radius in
       Graphics.set_color color ;
       Graphics.fill_circle x y radius ;
       Graphics.set_color button_color ;
       Graphics.fill_circle x y small_radius ;
       for i = 0 to pred n do
        let position = p.( n + i + 2 mod n_n )
        and follower = p.( n + i + 1 mod n_n ) in
         if i = selection then
          begin
           Graphics.set_color cursor_color ;
           Graphics.fill_poly [| center ; p.( n - i ) ; p.( n - i - 1 mod n ) |] ;
           Graphics.set_color selection_color ;
           Graphics.draw_segments [| ( x , y , fst position, snd position ) ; ( x , y , fst follower, snd follower ) |] ;
          end
         else
          Graphics.set_color text_color ;
          let place = triangle_barycentre ( ( float ( 99 * n + wh ) ) ** (-0.25) ) center position follower in
           Graphics.moveto ( place.(0) - ( 2 * widths.(i) / 5 ) ) ( place.(1) - ( 3 * heights.(i) ) / 5 ) ;
           Graphics.draw_string text.(i)
       done ;
       [| x ; y ; small_radius ; radius ; n ; selection |] ;; 


(** {v over_selector button_position x y v} *)
let over_selector = fun (button:int array) (x:int) (y:int) ->
 let dx = x - button.(0)
 and dy = y - button.(1)
 and small_radius = button.(2)
 and radius = button.(3)
 and n = button.(4)
 and selection = button.(5) in
  let ddx = float dx
  and ddy = float dy
  and nn = pred n in
   let distance = dx * dx + dy * dy in
    if distance <= small_radius * small_radius then
     begin
      if dx > 0 then 
       min ( succ selection ) nn
      else
       max 0 ( pred selection )
     end
    else
     if distance <= radius * radius then
      let angle = positive_argument ( -. ddy ) ( -. ddx )
      and sector = Infinitesimal.pi /. ( 1. +. ( float n ) ) in
       let position = floor ( angle /. sector ) in
        min nn ( ( ( int_of_float position ) - 1 ) / 2 )
     else selection ;;


(** {v polygon_folded_selector center_abscissa center_ordinate small_radius button_color color text_color selection_color text selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the size of the choice, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, la taille du choix, la sélection. *)
let polygon_folded_selector = fun (x:int) (y:int) (small_radius:int) (max_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (cursor_color:int) (text:string array) (max_number:int) (selection:int) ->
 let tt = Array.length text 
 and center = ( x , y )
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts
  and select = if selection < 0 then ( pred ( - selection ) ) else ( if selection >= tt then ( selection - tt ) else selection ) in
   let beginning = max_number * ( select / max_number ) in
    let n = min max_number ( tt - beginning ) in
     let nn = succ n
     and w = Matrix.vector_int_max widths
     and h = Matrix.vector_int_max heights in
      let n_n = n + nn
      and wh = ( ( max w h ) * ( 30 + 5 * n ) ) / ( 30 + n ) in
       let radius = min ( small_radius + wh ) max_radius in
        let p = ring_split nn x y small_radius radius in
         Graphics.set_color color ;
         Graphics.fill_poly p ;
         Graphics.set_color button_color ;
         Graphics.fill_circle x y small_radius ;
         for i = 0 to pred n do
          if i = select mod max_number then
           Graphics.set_color selection_color
          else
           Graphics.set_color text_color ;
          let position = p.( n + i + 2 mod n_n )
          and follower = p.( n + i + 1 mod n_n ) in
           let place = triangle_barycentre ( ( float ( 99 * n + wh ) ) ** (-0.25) ) center position follower in
            Graphics.moveto ( place.(0) - ( 2 * widths.(i) / 5 ) ) ( place.(1) - ( 3 * heights.(i) ) / 5 ) ;
            Graphics.draw_string text.( i + beginning )
         done ;
         [| x ; y ; small_radius ; radius ; n ; select ; tt ; max_number ; beginning |] ;; 


(** {v cursor_polygon_folded_selector center_abscissa center_ordinate small_radius button_color color text_color selection_color text selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the size of the choice, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, la taille du choix, la sélection. *)
let cursor_polygon_folded_selector = fun (x:int) (y:int) (small_radius:int) (max_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (cursor_color:int) (text:string array) (max_number:int) (selection:int) ->
 let tt = Array.length text 
 and center = ( x , y )
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts
  and select = if selection < 0 then ( pred ( - selection ) ) else ( if selection >= tt then ( selection - tt ) else selection ) in
   let beginning = max_number * ( select / max_number ) in
    let n = min max_number ( tt - beginning ) in
     let nn = succ n
     and w = Matrix.vector_int_max widths
     and h = Matrix.vector_int_max heights in
      let n_n = n + nn
      and wh = ( ( max w h ) * ( 30 + 5 * n ) ) / ( 30 + n ) in
       let radius = min ( small_radius + wh ) max_radius in
        let p = ring_split nn x y small_radius radius in
         Graphics.set_color color ;
         Graphics.fill_poly p ;
         Graphics.set_color button_color ;
         Graphics.fill_circle x y small_radius ;
         for i = 0 to pred n do
          let position = p.( n + i + 2 mod n_n )
          and follower = p.( n + i + 1 mod n_n ) in
           if i = select mod max_number then
            begin
             Graphics.set_color cursor_color ;
             Graphics.fill_poly [| center ; p.( n - i ) ; p.( n - i - 1 mod n ) |] ;
             Graphics.set_color selection_color ;
             Graphics.draw_segments [| ( x , y , fst position, snd position ) ; ( x , y , fst follower, snd follower ) |] ;
            end
           else
            Graphics.set_color text_color ;
           let place = triangle_barycentre ( ( float ( 99 * n + wh ) ) ** (-0.25) ) center position follower in
            Graphics.moveto ( place.(0) - ( 2 * widths.(i) / 5 ) ) ( place.(1) - ( 3 * heights.(i) ) / 5 ) ;
            Graphics.draw_string text.( i + beginning )
         done ;
         [| x ; y ; small_radius ; radius ; n ; select ; tt ; max_number ; beginning |] ;; 


(** {v round_folded_selector center_abscissa center_ordinate small_radius button_color color text_color selection_color text selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the size of the choice, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, la taille du choix, la sélection. *)
let round_folded_selector = fun (x:int) (y:int) (small_radius:int) (max_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (cursor_color:int) (text:string array) (max_number:int) (selection:int) ->
 let tt = Array.length text 
 and center = ( x , y )
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts
  and select = if selection < 0 then ( pred ( - selection ) ) else ( if selection >= tt then ( selection - tt ) else selection ) in
   let beginning = max_number * ( select / max_number ) in
    let n = min max_number ( tt - beginning ) in
     let nn = succ n
     and w = Matrix.vector_int_max widths
     and h = Matrix.vector_int_max heights in
      let n_n = n + nn
      and wh = ( ( max w h ) * ( 30 + 5 * n ) ) / ( 30 + n ) in
       let radius = min ( small_radius + wh ) max_radius in
        let p = ring_split nn x y small_radius radius in
         Graphics.set_color color ;
         Graphics.fill_circle x y radius ;
         Graphics.set_color button_color ;
         Graphics.fill_circle x y small_radius ;
         for i = 0 to pred n do
          if i = select mod max_number then
           Graphics.set_color selection_color
          else
           Graphics.set_color text_color ;
          let position = p.( n + i + 2 mod n_n )
          and follower = p.( n + i + 1 mod n_n ) in
           let place = triangle_barycentre ( ( float ( 99 * n + wh ) ) ** (-0.25) ) center position follower in
            Graphics.moveto ( place.(0) - ( 2 * widths.(i) / 5 ) ) ( place.(1) - ( 3 * heights.(i) ) / 5 ) ;
            Graphics.draw_string text.( i + beginning )
         done ;
         [| x ; y ; small_radius ; radius ; n ; select ; tt ; max_number ; beginning |] ;; 


(** {v cursor_round_folded_selector center_abscissa center_ordinate small_radius button_color color text_color selection_color text selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the size of the choice, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, la taille du choix, la sélection. *)
let cursor_round_folded_selector = fun (x:int) (y:int) (small_radius:int) (max_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (cursor_color:int) (text:string array) (max_number:int) (selection:int) ->
 let tt = Array.length text 
 and center = ( x , y )
 and ts = Array.map Graphics.text_size text in
  let widths = Array.map fst ts
  and heights = Array.map snd ts
  and select = if selection < 0 then ( pred ( - selection ) ) else ( if selection >= tt then ( selection - tt ) else selection ) in
   let beginning = max_number * ( select / max_number ) in
    let n = min max_number ( tt - beginning ) in
     let nn = succ n
     and w = Matrix.vector_int_max widths
     and h = Matrix.vector_int_max heights in
      let n_n = n + nn
      and wh = ( ( max w h ) * ( 30 + 5 * n ) ) / ( 30 + n ) in
       let radius = min ( small_radius + wh ) max_radius in
        let p = ring_split nn x y small_radius radius in
         Graphics.set_color color ;
         Graphics.fill_circle x y radius ;
         Graphics.set_color button_color ;
         Graphics.fill_circle x y small_radius ;
         for i = 0 to pred n do
          let position = p.( n + i + 2 mod n_n )
          and follower = p.( n + i + 1 mod n_n ) in
           if i = select mod max_number then
            begin
             Graphics.set_color cursor_color ;
             Graphics.fill_poly [| center ; p.( n - i ) ; p.( n - i - 1 mod n ) |] ;
             Graphics.set_color selection_color ;
             Graphics.draw_segments [| ( x , y , fst position, snd position ) ; ( x , y , fst follower, snd follower ) |] ;
            end
           else
            Graphics.set_color text_color ;
           let place = triangle_barycentre ( ( float ( 99 * n + wh ) ) ** (-0.25) ) center position follower in
            Graphics.moveto ( place.(0) - ( 2 * widths.(i) / 5 ) ) ( place.(1) - ( 3 * heights.(i) ) / 5 ) ;
            Graphics.draw_string text.( i + beginning )
         done ;
         [| x ; y ; small_radius ; radius ; n ; select ; tt ; max_number ; beginning |] ;; 


(** {v over_folded_selector button_position x y v} *)
let over_folded_selector = fun (button:int array) (x:int) (y:int) ->
 let dx = x - button.(0)
 and dy = y - button.(1)
 and small_radius = button.(2)
 and radius = button.(3)
 and n = button.(4)
 and selection = button.(5)
 and tt = button.(6)
 and max_number = button.(7)
 and beginning = button.(8) in
  let ddx = float dx
  and ddy = float dy
  and nn = pred n in
   let distance = dx * dx + dy * dy in
    if distance <= small_radius * small_radius then
     begin
      if dx > 0 then 
       min ( selection + max_number + tt ) ( 2 * tt - 1 )
      else
       ( - ( max 1 ( selection - max_number ) ) )
     end
    else
     if distance <= radius * radius then
      let angle = positive_argument ( -. ddy ) ( -. ddx )
      and sector = Infinitesimal.pi /. ( 1. +. ( float n ) ) in
       let position = floor ( angle /. sector ) in
        beginning + ( min nn ( ( ( int_of_float position ) - 1 ) / 2 ) )
     else selection ;;




(** {C § } *)
(** 
{1 Verniers}
*)
(** {C  } *)




(** {v float_vernier center_abscissa center_ordinate small_radius button_color color text_color selection_color cursor_color steps range selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the number of steps, the range, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, le nombre de pas, l'ambitus, la sélection. *)
let float_vernier = fun (x:int) (y:int) (small_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (cursor_color:int) (n:int) (range:float array) (selection:float) ->
 let beginning = range.(0)
 and ending = range.(1)
 and nn = succ n
 and n_n = 2 * n in
  let marks = Matrix.float_closed_equal_subdivision beginning nn ending
  and sector = Infinitesimal.doublepi /. ( 1. +. ( float nn ) ) in
   let angle = sector *. ( 0.5 +. ( float nn ) *. ( min 1. ( ( max 0. ( selection -. beginning ) ) /. ( ending -. beginning ) ) ) )
   and text = Array.map string_of_float marks
   and nnn = n_n - 2 in
    let ts = Array.map Graphics.text_size text in
     let widths = Array.map fst ts
     and heights = Array.map snd ts in
      let w = Matrix.vector_int_max widths
      and h = Matrix.vector_int_max heights in
       let wh = ( ( max w h ) * 707 ) / 500 in
        let radius = small_radius + wh in
         let r = float radius
         and p = ring_split nn x y small_radius radius
         and pp = ring_split nn x y small_radius ( radius / 2 ) in
          Graphics.set_color color ;
          Graphics.fill_circle x y radius ;
          for i = 0 to n do
           let position = p.( n + i + 1 mod nnn )
           and point = pp.( n + i + 1 mod nnn ) in
            Graphics.set_color selection_color ;
            Graphics.draw_segments [| ( x , y , fst point, snd point ) |] ;
            let place = [| ( ( fst point ) + ( fst position ) ) / 2 - widths.(i) / 3 ; ( ( snd point ) + ( snd position ) - heights.(i) ) / 2 |] in
             Graphics.moveto place.(0) place.(1) ;
             Graphics.set_color text_color ;
             Graphics.draw_string text.(i)
          done ;
          let xx = x - ( int_of_float ( r *. ( sin angle ) ) )
          and yy = y - ( int_of_float ( r *. ( cos angle ) ) ) in
           Graphics.set_color cursor_color ;
           Graphics.draw_segments [| ( x , y , xx , yy ) |] ;
           Graphics.set_color button_color ;
           Graphics.fill_circle x y small_radius ;
           [| float x ; float y ; float small_radius ; float radius ; float n ; beginning ; ending ; selection |] ;; 


(** {v over_float_vernier button_position x y v} *)
let over_float_vernier = fun (button:float array) (x:int) (y:int) ->
 let xx = float x
 and yy = float y
 and small_radius = button.(2)
 and radius = button.(3)
 and n = button.(4)
 and beginning = button.(5)
 and ending = button.(6)
 and selection = button.(7) in
  let dx = xx -. button.(0)
  and dy = yy -. button.(1)
  and sector = Infinitesimal.doublepi /. ( n +. 2. ) in
   let half_sector = 0.5 *. sector
   and course = Infinitesimal.doublepi -. sector
   and distance = dx *. dx +. dy *. dy in
    if ( distance >= small_radius *. small_radius ) && ( distance <= radius *. radius ) then
     let angle = positive_argument ( -. dy ) ( -. dx ) in
      let alpha = max 0. ( angle -. half_sector ) in
       let beta = min alpha ( Infinitesimal.doublepi -. sector ) in
        beginning +. beta *. ( ending -. beginning ) /. course
    else selection ;;


(** {v int_vernier center_abscissa center_ordinate small_radius button_color color text_color selection_color cursor_color steps range selection v} The function returns 
the coordinates of the center, the radius of the button, the big radius, the number of steps, the range, the selection.

La fonction retourne les coordonnées du centre, le rayon du bouton, le grand rayon, le nombre de pas, l'ambitus, la sélection. *)
let int_vernier = fun (x:int) (y:int) (small_radius:int) (button_color:int) (color:int) (text_color:int) (selection_color:int) (cursor_color:int) (n:int) (range:int array) (selection:int) ->
 let beginning = range.(0)
 and ending = range.(1)
 and nn = succ n
 and n_n = 2 * n in
  let marks = Matrix.int_equal_range beginning ( 1 + ( ending - beginning ) / n ) nn
  and sector = Infinitesimal.doublepi /. ( 1. +. ( float nn ) ) in
   let angle = sector *. ( 0.5 +. ( float nn ) *. ( float ( selection - beginning ) ) /. ( float ( ending - beginning ) ) )
   and text = Array.map string_of_int marks
   and nnn = n_n - 2 in
    let ts = Array.map Graphics.text_size text in
     let widths = Array.map fst ts
     and heights = Array.map snd ts in
      let w = Matrix.vector_int_max widths
      and h = Matrix.vector_int_max heights in
       let wh = ( ( max w h ) * 707 ) / 500 in
        let radius = small_radius + wh in
         let r = float radius
         and p = ring_split nn x y small_radius radius
         and pp = ring_split nn x y small_radius ( radius / 2 ) in
          Graphics.set_color color ;
          Graphics.fill_circle x y radius ;
          for i = 0 to n do
           let position = p.( n + i + 1 mod nnn )
           and point = pp.( n + i + 1 mod nnn ) in
            Graphics.set_color selection_color ;
            Graphics.draw_segments [| ( x , y , fst point, snd point ) |] ;
            let place = [| ( ( fst point ) + ( fst position ) ) / 2 - widths.(i) / 3 ; ( ( snd point ) + ( snd position ) - heights.(i) ) / 2 |] in
             Graphics.moveto place.(0) place.(1) ;
             Graphics.set_color text_color ;
             Graphics.draw_string text.(i)
          done ;
          let xx = x - ( int_of_float ( r *. ( sin angle ) ) )
          and yy = y - ( int_of_float ( r *. ( cos angle ) ) ) in
           Graphics.set_color cursor_color ;
           Graphics.draw_segments [| ( x , y , xx , yy ) |] ;
           Graphics.set_color button_color ;
           Graphics.fill_circle x y small_radius ;
           [| x ; y ; small_radius ; radius ; n ; beginning ; ending ; selection |] ;; 


(** {v over_int_vernier button_position x y v} *)
let over_int_vernier = fun (button:int array) (x:int) (y:int) ->
 let small_radius = button.(2)
 and radius = button.(3)
 and n = button.(4)
 and beginning = float button.(5)
 and ending = float button.(6)
 and selection = button.(7) in
  let dx = x - button.(0)
  and dy = y - button.(1)
  and sector = Infinitesimal.doublepi /. ( ( float n ) +. 2. ) in
   let half_sector = 0.5 *. sector
   and course = Infinitesimal.doublepi -. sector
   and distance = dx * dx + dy * dy in
    if ( distance >= small_radius * small_radius ) && ( distance <= radius * radius ) then
     let angle = positive_argument ( -. ( float dy ) ) ( -. ( float dx ) ) in
      let alpha = max 0. ( angle -. half_sector ) in
       let beta = min alpha ( Infinitesimal.doublepi -. sector ) in
        int_of_float ( beginning +. beta *. ( ending -. beginning ) /. course )
    else selection ;;




(** {C § } *)
(** 
{1 Modelage de chaînes de caractères}
{1 Character string edition}
*)
(** {C  } *)




(** {v string_treatment left_string right_string escaped_character v} *)
let string_treatment = fun (l:string) (r:string) (c:string) ->
 match String.length c with
 | 1 -> [| l ^ c ; r |]
 | _ ->
  begin
   match c with
   | "\\000" -> [| String.sub l 0 ( max 0 ( pred ( String.length l ) ) ) ; r |] (** Ctl-Space *)
   | "\\001" -> [| "" ; l ^ r |] (** Ctl-A *)
   | "\\002" -> (** Ctl-B *)
    begin
     let pos = pred ( String.length l ) in
      if pos < 0 then [| "" ; l ^ r |]
      else [| String.sub l 0 pos ; ( Char.escaped l.[pos] ) ^ r |]
    end
   | "\\005" -> [| l ^ r ; "" |] (** Ctl-E *)
   | "\\006" -> (** Ctl-F *)
    begin
     let length = pred ( String.length r ) in
      if length < 0 then [| l ^ r ; "" |]
      else [| l ^ ( Char.escaped r.[0] ) ; String.sub r 1 length |]
    end
   | "\\008" -> [| String.sub l 0 ( max 0 ( pred ( String.length l ) ) ) ; r |] (** Ctl-H *)
   | "\\011" -> [| l ; "" |] (** Ctl-K *)
   | "\\014" -> (** Ctl-N *)
    begin
     let length = pred ( String.length r ) in
      if length < 0 then [| l ^ r ; "" |]
      else [| l ^ ( Char.escaped r.[0] ) ; String.sub r 1 length |]
    end
   | "\\016" -> (** Ctl-P *)
    begin
     let pos = pred ( String.length l ) in
      if pos < 0 then [| "" ; l ^ r |]
      else [| String.sub l 0 pos ; ( Char.escaped l.[pos] ) ^ r |]
    end
   | "\\018" -> [| String.sub l 0 ( max 0 ( pred ( String.length l ) ) ) ; r |] (** Ctl-R *)
   | "\\019" -> [| l ; String.sub r 1 ( max 0 ( pred ( String.length r ) ) ) |] (** Ctl-S *)
   | "\\021" -> [| "" ; r |] (** Ctl-U *)
   | "\\024" -> [| String.sub l 0 ( max 0 ( pred ( String.length l ) ) ) ; r |] (** Ctl-X *)
   | "\\026" -> [| l ; String.sub r 1 ( max 0 ( pred ( String.length r ) ) ) |] (** Ctl-Z *)
   | _ -> [| l ; r |]
  end ;;


(** {v string_edit bottom_left_corner_abscissa bottom_left_corner_ordinate width background_color left_text_color right_text_color left_string right_string v} *)
let string_edit = fun (abscissa:int) (ordinate:int) (width:int) (color:int) (left_text_color:int) (right_text_color:int) (initial:string) (final:string) ->
 let offset = String.length initial
 and setoff = String.length final
 and text = " " ^ initial ^ final ^ " "
 and x = abscissa - width / 2 in
  let size = Graphics.text_size text
  and length = 2 + offset + setoff in
   let n = ( length * width ) / ( fst size )
   and h = snd size in
    let nn = n / 2
    and y = ordinate - h / 2 in
     let beginning = max 0 ( offset - nn ) in
      let left_length = offset - beginning in
       let left = String.sub initial beginning left_length
       and right_length = min ( n - left_length ) setoff in
        let right = String.sub final 0 right_length in
         Graphics.set_color color ;
         Graphics.fill_rect x y width h ;
         Graphics.moveto x ( y + h / 10 ) ;
         Graphics.set_color left_text_color ;
         Graphics.draw_string left ;
         Graphics.set_color right_text_color ;
         Graphics.draw_string right ;
         [| string_of_int abscissa ; string_of_int ordinate ; string_of_int width ; string_of_int x ; string_of_int y ; initial ; final |] ;;


(** {v is_over_string_edit bottom_left_corner_abscissa bottom_left_corner_ordinate width abscissa ordinate v} *)
let is_over_string_edit = fun (zone:string array) (abscissa:int) (ordinate:int) ->
 let x = int_of_string zone.(3)
 and y = int_of_string zone.(4)
 and width = int_of_string zone.(2)
 and height = snd ( Graphics.text_size "W" ) in
  let v = [| abscissa >= x ; ordinate >= y ; abscissa <= x + width ; ordinate <= y + height |] in
   Array.fold_left ( && ) true v ;;




(** {C § } *)
(** 
{1 Divers}
{1 Miscellaneous}
*)
(** {C  } *)




(** sand_glass *)
let sand_glass = fun (hori_size:int) (verti_size:int)  ->
 Graphics.open_graph  ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
 let center = [| hori_size / 2 ; verti_size / 2 |] in
  let t = hat_switch center.(0) center.(1) Graphics.white Graphics.black [| "  Wait  " |] in
   let verti_shift = ( t.(5) - t.(3) ) * 2 in
    ignore ( cup_switch center.(0) ( center.(1) + verti_shift ) Graphics.black Graphics.white [| "Attendre" |] ) ;;






(** {C § } *)
(** 
{1 Interactions}
*)
(** {C  } *)




(** The interaction loop comes from E. Chailloux, P. Manoury, B. Pagano : Developing Applications With Objective Caml, O'Reilly & associates.

La boucle d'interaction provient de E. Chailloux, P. Manoury, B. Pagano : Developing Applications With Objective Caml, O'Reilly & associates. *)

(** {v End v} *)
exception End ;;

(** {v skeleton f_init f_end f_key f_mouse f_except  v} *)
let skeleton = fun f_init f_end f_key f_mouse f_except ->
 f_init () ;
 try
  while true do
   try
    let s = Graphics.wait_next_event [Graphics.Button_down; Graphics.Key_pressed] in
     if s.Graphics.keypressed then
      f_key s.Graphics.key
     else
      begin
       if s.Graphics.button then
        f_mouse s.Graphics.mouse_x s.Graphics.mouse_y
      end
   with
   | End -> raise End
   | e -> f_except e
  done
 with
 | End -> f_end ()
 | Failure unknown -> f_end () ;;



(** {v choose_directory approx_hori_size approx_verti_size max_number_display address v} *)
let choose_directory = fun (approx_hori_size:int) (approx_verti_size:int) (max_number:int) (dname:string) ->
 let beginning = Sys.getcwd () in
  Sys.chdir dname ;
  let wd = ref ( Sys.getcwd () ) in
   try
    begin
     Graphics.close_graph () ;
     let obsolete = ref [ !wd ]
     and choice = ref 0
     and sub_directories_list = ref ( Readwrite.sub_directories_with_parent dname )
     and hori_size = approx_hori_size
     and verti_size = approx_verti_size
     and first_time = ref true
     and refresh_message = [| "Rafraîchir" ; "Refresh" |] and current_character = ref "0"
     and sd = ref [| 0 |] and sed = ref [| "" |]
     and address_initial = ref dname and address_final = ref ""
     and ok = ref [| 0 |] and cancel = ref [| 0 |] and back = ref [| 0 |] and seek_out = ref [| 0 |] and refresh = ref [| 0 |] in
      let hori_shift = hori_size / 20
      and verti_shift = verti_size / 20
      and l = ref ( Array.length !sub_directories_list )
      and small_radius = max 10 ( approx_verti_size / 40 ) in
       let f_init = function () ->
        begin (** f_init *)
         Gc.compact () ;
         if !first_time then
          begin
           Graphics.open_graph ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
           Graphics.set_window_title "Analogic.choose_directory"
          end ;
         address_initial := !wd ;
         address_final := "" ;
         sub_directories_list := Readwrite.sub_directories_with_parent !wd ;
         l := Array.length !sub_directories_list ;
         Graphics.set_color Graphics.background ;
         Graphics.fill_rect 0 0 hori_size verti_size ;
         sd := cursor_round_folded_selector ( 10 * hori_shift ) ( 10 * verti_shift ) small_radius ( 7 * ( min hori_shift verti_shift ) ) Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white !sub_directories_list max_number !choice ;
         sed := string_edit ( 10 * hori_shift ) verti_shift ( 16 * hori_shift ) dark_grey Graphics.white Graphics.yellow !address_initial !address_final ;
         ok := rect_switch ( hori_size - hori_shift ) verti_shift Graphics.green Graphics.black [| " OK " |] ;
         cancel := rect_switch hori_shift verti_shift Graphics.red Graphics.black [| " Annuler " ; " Cancel " |] ;
         refresh := rect_switch hori_shift ( 10 * verti_shift ) Graphics.yellow Graphics.black refresh_message ;
         seek_out := rect_switch hori_shift ( verti_size - verti_shift ) Graphics.cyan Graphics.black [| "Remonter" ; "Seek out" |] ;
         back := rect_switch ( hori_size - hori_shift ) ( verti_size - verti_shift ) Graphics.cyan Graphics.black [| "Précédent" ; "Backward" |] ;
         Graphics.set_color Graphics.foreground ;
         Graphics.moveto ( 18 * hori_shift ) ( 10 * verti_shift ) ;
         Graphics.draw_string ( ( string_of_int !sd.(4) ) ^ " / " ^ ( string_of_int !l ) ^ " : " ^ ( string_of_int !sd.(5) ) ) ;
        end (** f_init *)
       and f_end = function () -> ( Sys.chdir beginning ; Graphics.close_graph () ; raise End )
       and f_key = function c -> ( current_character := Char.escaped c ) in
        let f_mouse = fun x y ->
         begin (** f_mouse *)
          first_time := false ;
          choice := over_folded_selector !sd x y ;
          if is_over_rect_button !cancel x y then
           begin
            obsolete := !wd :: !obsolete ;
            wd := dname ;
            f_end ()
           end
          else
          if is_over_rect_button !ok x y then
           f_end () 
          else
          if is_over_rect_button !refresh x y then
           f_init ()
          else
          if ( !choice != 0 ) && ( !choice >= 0 ) && ( !choice < !l ) then
           begin
            Sys.chdir ( !sub_directories_list ).(!choice) ;
            obsolete := !wd :: !obsolete ;
            wd := Sys.getcwd () ;
            choice := 0 ;
            f_init ()
           end
          else
          if ( !choice >= !l ) || ( !choice < 0 ) then
           f_init ()
          else
          if ( !choice = 0 ) || ( is_over_rect_button !seek_out x y ) then
           begin
            obsolete := !wd :: !obsolete ;
            wd := Filename.dirname ( Sys.getcwd () ) ;
            Sys.chdir !wd ;
            f_init ()
           end
          else
          if is_over_rect_button !back x y then
           begin
            wd := List.hd !obsolete ;
            obsolete := List.tl !obsolete ;
            if List.length !obsolete = 0 then
             obsolete := [ beginning ] ;
            Sys.chdir !wd ;
            f_init ()
           end
         end (** f_mouse *)
       and f_except = function unknown -> ( Graphics.close_graph () ; raise End ) in
        skeleton f_init f_end f_key f_mouse f_except ;
        !wd
   end
  with End -> Gc.compact () ;
  Sys.chdir beginning ;
  !wd ;;



(** {v choose_regular_file approx_hori_size approx_verti_size max_number_display address v}  *)
let choose_regular_file = fun (approx_hori_size:int) (approx_verti_size:int) (max_number:int) (dname:string) ->
 let beginning = Sys.getcwd () in
  Sys.chdir dname ;
  let address = ref "" in
   try
    begin
     Graphics.close_graph () ;
     let choice = ref 0
     and regular_files_list = Readwrite.regular_files dname
     and hori_size = approx_hori_size
     and verti_size = approx_verti_size
     and first_time = ref true
     and refresh_message = [| "Rafraîchir" ; "Refresh" |] and current_character = ref "0"
     and sf = ref [| 0 |] and sef = ref [| "" |]
     and address_initial = ref "" and address_final = ref ""
     and ok = ref [| 0 |] and cancel = ref [| 0 |] and refresh = ref [| 0 |] in
      let hori_shift = hori_size / 20
      and verti_shift = verti_size / 20
      and l = Array.length regular_files_list
      and small_radius = max 10 ( approx_verti_size / 30 ) in
       let f_init = function () ->
        begin (** f_init *)
         Gc.compact () ;
         if !first_time then
          begin
           Graphics.open_graph ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
           Graphics.set_window_title "Analogic.choose_regular_file"
          end ;
         address_initial := !address ;
         address_final := "" ;
         Graphics.set_color Graphics.background ;
         Graphics.fill_rect 0 0 hori_size verti_size ;
         sf := cursor_round_folded_selector ( 10 * hori_shift ) ( 10 * verti_shift ) small_radius ( 7 * ( min hori_shift verti_shift ) ) Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white regular_files_list max_number !choice ;
         sef := string_edit ( 10 * hori_shift ) verti_shift ( 16 * hori_shift ) Graphics.black Graphics.white Graphics.yellow !address_initial !address_final ;
         ok := rect_switch ( hori_size - hori_shift ) verti_shift Graphics.green Graphics.black [| " OK " |] ;
         cancel := rect_switch hori_shift verti_shift Graphics.red Graphics.black [| " Annuler " ; " Cancel " |] ;
         refresh := rect_switch hori_shift ( 10 * verti_shift ) Graphics.yellow Graphics.black refresh_message ;
         Graphics.set_color Graphics.foreground ;
         Graphics.moveto ( 18 * hori_shift ) ( 10 * verti_shift ) ;
         Graphics.draw_string ( ( string_of_int !sf.(4) ) ^ " / " ^ ( string_of_int l ) ^ " : " ^ ( string_of_int !sf.(5) ) ) ;
        end (** f_init *)
       and f_end = function () -> ( Sys.chdir beginning ; Graphics.close_graph () ; raise End )
       and f_key = function c -> ( current_character := Char.escaped c ) in
        let f_mouse = fun x y ->
         begin (** f_mouse *)
          first_time := false ;
          choice := over_folded_selector !sf x y ;
          if is_over_rect_button !cancel x y then
           begin
            address := "" ;
            f_end ()
           end
          else
          if is_over_rect_button !ok x y then
           f_end () 
          else
          if is_over_rect_button !refresh x y then
           f_init ()
          else
          if is_over_string_edit !sef x y then
           let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] ) in
            while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
             f_key ( !ev.Graphics.key ) ;
             let res = string_treatment !address_initial !address_final !current_character in
              address_initial := res.(0) ;
              address_final := res.(1) ;
              address := !address_initial ^ !address_final ;
              sef := string_edit ( int_of_string !sef.(0) ) ( int_of_string !sef.(1) ) ( int_of_string !sef.(2) ) Graphics.black Graphics.white Graphics.green !address_initial !address_final ;
              ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
            done
          else
          if ( !choice >= 0 ) && ( !choice < l ) then
           begin
            address := ( regular_files_list ).(!choice) ;
            f_init ()
           end
          else
          if ( !choice >= l ) || ( !choice < 0 ) then
           f_init ()
         end (** f_mouse *)
       and f_except = function unknown -> ( Graphics.close_graph () ; raise End ) in
        skeleton f_init f_end f_key f_mouse f_except ;
        !address
   end
  with End -> Gc.compact () ;
  Sys.chdir beginning ;
  !address ;;



(** {v choose_real hori_size verti_size range value v} The float vector [value] is modified in place.

Le vecteur réel [value] est modifié en place. *)
let choose_real = fun (hori_size:int) (verti_size:int) (range:float array) (value:float array) ->
 assert ( Array.length value >= 1 ) ;
 assert ( Array.length range >= 2 ) ;
 try
  begin
   let obsolete = ref value.(0)
   and first_time = ref true
   and f_string = ( function s -> String.sub s 0 ( min 16 ( String.length s ) ) )
   and cancel_message = [| "Annuler" ; "Cancel" |] and ok_message = [| " OK " |]
   and refresh_message = [| "Rafraîchir" ; "Refresh" |] and current_character = ref "0"
   and sr = ref [| 0. |] and ser = ref [| "" |]
   and ok = ref [| 0 |] and cancel = ref [| 0 |] and refresh = ref [| 0 |] in
    let hori_shift = hori_size / 20
    and verti_shift = verti_size / 20
    and coefficient_initial = ref ( f_string ( string_of_float value.(0) ) ) and coefficient_final = ref ""
    and small_radius = max 10 ( verti_size / 50 ) in
     Graphics.close_graph () ;
     let f_init = function () ->
      begin (** f_init *)
       Gc.compact () ;
       if !first_time then
        begin
         Graphics.open_graph ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
         Graphics.set_window_title "Analogic.choose_real"
        end ;
       ok := rect_switch ( 15 * hori_shift ) ( 5 * verti_shift ) Graphics.green Graphics.black ok_message ;
       cancel := rect_switch ( 5 * hori_shift ) ( 5 * verti_shift ) Graphics.red Graphics.black cancel_message ;
       refresh := rect_switch ( 5 * hori_shift ) ( 10 * verti_shift ) Graphics.yellow Graphics.black refresh_message ;
       sr := float_vernier ( 10 * hori_shift ) ( 8 * verti_shift ) small_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white 10 range value.(0) ;
       ser := string_edit ( 10 * hori_shift ) ( 15 * verti_shift ) ( 10 * hori_shift ) Graphics.black Graphics.white Graphics.yellow !coefficient_initial !coefficient_final ;
      end (** f_init *)
     and f_end = function () -> ( Graphics.close_graph () ; raise End )
     and f_key = function c -> ( current_character := Char.escaped c ) in
      let f_mouse = fun x y ->
       begin (** f_mouse *)
        first_time := false ;
        value.(0) <- over_float_vernier !sr x y ;
        if is_over_rect_button !cancel x y then
         begin
          value.(0) <- !obsolete ;
          f_end ()
         end
        else
        if is_over_rect_button !ok x y then
         f_end () 
        else
        if is_over_rect_button !refresh x y then
         begin
          coefficient_initial := !coefficient_initial ^ !coefficient_final ;
          coefficient_final := "" ;
          f_init ()
         end
        else
        if is_over_string_edit !ser x y then
         let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
         and old_real = f_string ( string_of_float value.(0) )
         and old_initial = !coefficient_initial
         and old_final = !coefficient_final in
          while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
           f_key ( !ev.Graphics.key ) ;
           let res = string_treatment !coefficient_initial !coefficient_final !current_character in
            coefficient_initial := res.(0) ;
            coefficient_final := res.(1) ;
            begin
             try
              begin
               value.(0) <- float_of_string ( !coefficient_initial ^ !coefficient_final )
              end
             with Failure unknown -> ( value.(0) <- float_of_string old_real ; coefficient_initial := old_initial ; coefficient_final := old_final ) ;
            end ;
            ser := string_edit ( int_of_string !ser.(0) ) ( int_of_string !ser.(1) ) ( int_of_string !ser.(2) ) Graphics.black Graphics.white Graphics.green !coefficient_initial !coefficient_final ;
            sr := float_vernier ( 10 * hori_shift ) ( 8 * verti_shift ) small_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white 10 range value.(0) ;
            ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
          done
        else
        if ( value.(0) != float_of_string ( !coefficient_initial ^ !coefficient_final ) ) then
         begin
          sr := float_vernier ( 10 * hori_shift ) ( 8 * verti_shift ) small_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white 10 range value.(0) ;
          coefficient_initial := f_string ( string_of_float value.(0) ) ;
          coefficient_final := "" ;
          f_init ()
         end
       end (** f_mouse *)
      and f_except = function unknown -> ( Graphics.close_graph () ; raise End ) in
       skeleton f_init f_end f_key f_mouse f_except ;
       value
  end
 with End -> Gc.compact () ;
 value ;;







(** {C § § § } *)




end ;;



