



module Analogic = struct



(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module: 

- function to digitize oscillograms and photographic pictures of graphic recorders,

- utilities for the intermediate steps of picture treatment or digitization.

Going with it, a script {{:../analogic/compile.sh} compile.sh} compiles
{{:../analogic/ferdinand.ml} ferdinand.ml}, yielding a native executable [ferdinand] for digitizing oscillograms;
a script {{:../analogic/bytecode.sh} bytecode.sh} compiles
{{:../analogic/ferdinand-bytecode.ml} ferdinand-bytecode.ml}, 
yielding a bytecode executable [ferdinand.o] for digitizing oscillograms.


{2 Conventions}


In digitizing a curve from its picture, the picture has to be cropped in order that only the pixels containing either the screen background or the curve are treated.
The horizontal cropping will preferably match the horizontal extension of the curve, without exceeding it.

The treatment of an oscillogram uses the luminosity of the picture.
The median value of the luminosity is roughly evaluated as the median of the medians of the luminosities of the lines.

The signal is supposed to be in the values of luminosity greater than the product of the threshold with the median.
The graticule is supposed to be in the lower values, up to full black.

For the pictures of oscilloscopes with graticule lit up by incandescence light or for pictures of graphic recorders 
(for which the variety of colors is unpredictable),
it is necessary to treat the colors in order to be reduced to the preceding conventions for oscillograms.
The [phosphor] switch permits to choose between the functions [oscillo] and [recorder].

The mathematician must supply the value of the threshold to separate the traces from the rest of the picture.
The detection of the number of traces is automatically done a priori (with a convenient threshold value),
but it is possible to stretch the work on a unique trace by assigning the value [true] to the argument [monotrace].

The algorithm does not accept the trace crossing.

As the luminosity of the trace is higher inside the bends than outside, 
the position of the curve is evaluated recursively by taking into account the curvature radius from the preceding step,
until the variation from a step to another is low enough.

If the digitization gives strange results, try again changing the threshold value or the cropping.
If for a multitrace picture the number of traces remains inadequate, crop again the traces one-by-one and stretch the monotrace mode.


{2 Comments}


Here follow the maximal quantities of information contained inside a picture :

+ of size 1333 * 2000 with 8 bits per color : 2000 samples of 20 bits or one measurement of 31 bits,
+ of size 3000 * 4000 with 12 bits per color : 4000 samples of 25 bits or one measurement of 37 bits,
+ of size 8000 * 12000 with 16 bits per color : 12000 samples of 30,5 bits or one measurement of 44 bits.


The digitization of an oscillogram receives interference from different phenomenons, the main one being the presence of the graticule.

The noise plays a part in the measurement with the probe, the input attenuator, the amplifying chain, 
the cathode ray tube and the unevenness of the phosphore layer.

The noise plays a part in the view capture with the optical irregularities, the thermal noise,
the Bayer outmatrixing, the lossy compression.

A screen without graticule will give more pure results.


For monochromatic photographic archives of oscillograms in presence of lit up graticule or of graphic records with graticule,
it is necessary to touch up the picture once digitized by erasing the graticule before applying the present treatment.


The graphical interfaces are greedy in resources; 
a generous use of [Gc.compact ()] is realized by calling it at each window update.


This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module :

- des fonctions pour numériser des oscillogrammes ou des images photographiques de tables traçantes,

- des utilitaires pour les étapes intermédiaires dans le traitement ou la numérisation des images.

L'accompagnant, un script {{:../analogic/compile.sh} compile.sh} compile
{{:../analogic/ferdinand.ml} ferdinand.ml}, produisant un exécutable natif [ferdinand] de numérisation d'oscillogrammes ;
un script {{:../analogic/bytecode.sh} bytecode.sh} compile
{{:../analogic/ferdinand-bytecode.ml} ferdinand-bytecode.ml}, 
produisant un exécutable virtuel [ferdinand.o] de numérisation d'oscillogrammes.


{2 Conventions}


Dans la numérisation d'une courbe par son image, l'image doit d'abord être recadrée de façon que ne soient traités que des pixels qui contiennent
soit le fond d'écran soit la courbe. Le cadrage horizontal collera de préférence à l'extension horizontale de la courbe, sans la dépasser.

Le traitement d'un oscillogramme utilise la luminosité de l'image.
La valeur médiane de luminosité est évaluée grossièrement comme la médiane des médianes des luminosités de chaque ligne.

Le signal est censé être dans les valeurs de luminosité supérieures au produit du seuil par la médiane.
Le réticule est censé être dans les valeurs inférieures de luminosité, jusqu'au noir complet.

Pour les images d'oscilloscope avec réticule éclairé par des lampes à incandescence ou pour les images de tables traçantes 
(pour lesquelles la variété des couleurs est imprévisible),
il est nécessaire de traiter les couleurs pour se ramener aux conventions précédentes pour l'oscillogramme.
Le commutateur [phosphor] permet de choisir entre les fonctions [oscillo] et [recorder].

La mathématicienne ou le mathématicien doit fournir la valeur de seuil pour séparer les traces du reste de l'image.
La détection du nombre de traces se fait a priori automatiquement (avec une valeur de seuil convenable), 
mais il est possible de forcer le travail sur une trace unique en attribuant la valeur [true] à l'argument [monotrace].

L'algorithme n'accepte pas le croisement des traces.

Comme la luminosité de la trace est plus élevée à l'intérieur des virages qu'à l'extérieur, 
la position de la courbe est évaluée récursivement en tenant compte du rayon de courbure à l'étape précédente,
jusqu'à ce que la variation d'une étape à l'autre soit assez faible.

Si la numérisation donne des résultats bizarres, recommencer en changeant la valeur de seuil ou le recadrage.
Si pour une image multitrace le nombre de traces détectées reste inadéquat, recadrer trace par trace et forcer le mode monotrace.



{2 Commentaires}


Suivent les quantités maximales d'information contenues dans une image :

+ de taille 1333 * 2000 à 8 bits par couleur : 2000 échantillons de 20 bits ou une mesure de 31 bits,
+ de taille 3000 * 4000 à 12 bits par couleur : 4000 échantillons de 25 bits ou une mesure de 37 bits,
+ de taille 8000 * 12000 à 16 bits par couleur : 12000 échantillons de 30,5 bits ou une mesure de 44 bits.


La numérisation d'un oscillogramme est parasitée par différents phénomènes, le principal étant la présence du réticule.

Le bruit de souffle intervient dans la mesure avec la sonde, l'atténuateur d'entrée, la chaîne d'amplification, 
le tube cathodique et l'irrégularité de la couche de phosphore.

Le bruit de souffle intervient dans la prise de vue avec les irrégularités optiques, le bruit thermique, 
le dématriçage de Bayer, la compression avec perte.

Un écran sans réticule donnera des résultats plus purs.


Pour des archives photographiques monochromes d'oscillogrammes avec présence du réticule éclairé ou de table traçante avec réticule,, 
il est nécessaire de retoucher l'image une fois numérisée en effaçant le réticule avant d'appliquer le présent traitement.


Les interfaces graphiques sont gourmandes en ressouces ; 
une utilisation généreuse de [Gc.compact ()] est effectuée par son appel à chaque rafraîchissement de fenêtre.


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
open Draw ;;
open Widget ;;


Graphics.open_graph " " ;;


(** {v max_color v} *)
let max_color = Graphics.red + Graphics.green + Graphics.blue ;;

(** {v red_for_color real v} *)
let red_for_color = fun (x:float) ->
 min Graphics.red ( 65536 * ( Util.round x ) ) ;;

(** {v green_for_color real v} *)
let green_for_color = fun (x:float) ->
 min Graphics.green ( 256 * ( Util.round x ) ) ;;

(** {v blue_for_color real v} *)
let blue_for_color = fun (x:float) ->
 min Graphics.blue ( Util.round x ) ;;



(** {v display_float_unicolor h_offset v_offset matrix v} The function Graphics.draw_image is too unstable to be useful.

La fonction Graphics.draw_image est trop instable pour être utilisable. *)
let display_float_unicolor = fun (h_offset:int) (v_offset:int) (m:float array array) ->
 let height = Array.length m
 and width = Array.length m.(0) in
  let ww = pred width
  and hh = pred height in
   let abscissa = ref h_offset
   and ordinate = ref ( v_offset + hh ) in
    for i = 0 to hh do
     let row = m.(i) in
      for j = 0 to ww do
       let color = 65793 * ( int_of_float row.(j) ) in
        Graphics.set_color color ;
        Graphics.plot !abscissa !ordinate ;
        abscissa := succ !abscissa ;
      done ;
      ordinate := pred !ordinate ;
      abscissa := h_offset
    done ;;


(** {v display_float_rgb h_offset v_offset rgb_matrix v} The function Graphics.draw_image is too unstable to be useful.

La fonction Graphics.draw_image est trop instable pour être utilisable. *)
let display_float_rgb = fun (h_offset:int) (v_offset:int) (m:float array array array) ->
 let red = m.(0)
 and green = m.(1)
 and blue = m.(2) in
  let height = Array.length red
  and width = Array.length red.(0) in
   let ww = pred width
   and hh = pred height in
    let abscissa = ref h_offset
    and ordinate = ref ( v_offset + hh ) in
     for i = 0 to hh do
      let row_red = red.(i)
      and row_green = green.(i)
      and row_blue = blue.(i) in
       for j = 0 to ww do
        let color = ( red_for_color row_red.(j) ) + ( green_for_color row_green.(j) ) + ( blue_for_color row_blue.(j) ) in
         Graphics.set_color color ;
         Graphics.plot !abscissa !ordinate ;
         abscissa := succ !abscissa ;
       done ;
       ordinate := pred !ordinate ;
       abscissa := h_offset
     done ;;

(** {v relative_draw_line_float_rgb x_offset y_offset rgb_matrix row v} *)
let relative_draw_line_float_rgb = fun (x_offset:int) (y_offset:int) (m:float array array array) (row:int) ->
 let red = m.(0)
 and green = m.(1)
 and blue = m.(2) in
  let height = Array.length red
  and width = Array.length red.(0) in
   let guarded_row = max 0 ( min row ( pred height ) )
   and x = ref x_offset in
    let row_red = red.(guarded_row)
    and row_green = green.(guarded_row)
    and row_blue = blue.(guarded_row)
    and y = y_offset + height - guarded_row in
     for j = 0 to pred width do
      Graphics.set_color ( ( red_for_color row_red.(j) ) + ( green_for_color row_green.(j) ) + ( blue_for_color row_blue.(j) ) ) ;
      Graphics.plot !x y ;
      x := succ !x ;
     done ;;

(** {v relative_draw_line_magnify_float_rgb factor x_offset y_offset rgb_matrix row v} *)
let relative_draw_line_magnify_float_rgb = fun (factor:int) (x_offset:int) (y_offset:int) (m:float array array array) (row:int) ->
 let red = m.(0)
 and green = m.(1)
 and blue = m.(2)
 and ff = pred factor in
  let height = Array.length red
  and width = Array.length red.(0) in
   let guarded_row = max 0 ( min row ( pred height ) )
   and x = ref ( x_offset ) in
    let row_red = red.(guarded_row)
    and row_green = green.(guarded_row)
    and row_blue = blue.(guarded_row)
    and y = y_offset + factor * ( height - guarded_row - 1 ) in
     for j = 0 to pred width do
      Graphics.set_color ( ( red_for_color row_red.(j) ) + ( green_for_color row_green.(j) ) + ( blue_for_color row_blue.(j) ) ) ;
       for h = 0 to ff do
        for k = 0 to ff do
         Graphics.plot ( !x + h ) ( y + k )
        done ;
       done ;
      x := !x + factor ;
     done ;;

(** {v relative_draw_column_float_rgb x_offset y_offset rgb_matrix row v} *)
let relative_draw_column_float_rgb = fun (x_offset:int) (y_offset:int) (m:float array array array) (column:int) ->
 let red = m.(0)
 and green = m.(1)
 and blue = m.(2) in
  let height = Array.length red
  and width = Array.length red.(0) in
   let guarded_column = max 0 ( min column ( pred width ) )
   and y = ref ( y_offset + height - 1 ) in
    let x = x_offset + guarded_column in
     for i = 0 to pred height do
      let row_red = red.(i)
      and row_green = green.(i)
      and row_blue = blue.(i) in
       Graphics.set_color ( ( red_for_color row_red.(guarded_column) ) + ( green_for_color row_green.(guarded_column) ) + ( blue_for_color row_blue.(guarded_column) ) ) ;
       Graphics.plot x !y ;
       y := pred !y ;
     done ;;


(** {v relative_draw_column_magnify_float_rgb factor x_offset y_offset rgb_matrix row v} *)
let relative_draw_column_magnify_float_rgb = fun (factor:int) (x_offset:int) (y_offset:int) (m:float array array array) (column:int) ->
 let red = m.(0)
 and green = m.(1)
 and blue = m.(2)
 and ff = pred factor in
  let height = Array.length red
  and width = Array.length red.(0) in
   let guarded_column = max 0 ( min column ( pred width ) )
   and y = ref ( y_offset + factor * ( height - 1 ) ) in
    let x = x_offset + factor * guarded_column in
     for i = 0 to pred height do
      let row_red = red.(i)
      and row_green = green.(i)
      and row_blue = blue.(i) in
       for h = 0 to ff do
        for k = 0 to ff do
         Graphics.set_color ( ( red_for_color row_red.(guarded_column) ) + ( green_for_color row_green.(guarded_column) ) + ( blue_for_color row_blue.(guarded_column) ) ) ;
         Graphics.plot ( x + h ) ( !y + k ) ;
        done ;
       done ;
       y := !y - factor ;
     done ;;

(** {v cross_float_rgb float_color row column rgb_matrix v} *)
let cross_float_rgb = fun (color:float array) (row:int) (column:int) (m:float array array array) ->
 let red = Matrix.matrix_float_copy m.(0)
 and green = Matrix.matrix_float_copy m.(1)
 and blue = Matrix.matrix_float_copy m.(2) in
  let height = Array.length red
  and width = Array.length red.(0) in
   let guarded_row = max 0 ( min row ( pred height ) )
   and guarded_column = max 0 ( min column ( pred width ) ) in
   red.(guarded_row) <- Array.make width color.(0) ;
   green.(guarded_row) <- Array.make width color.(1) ;
   blue.(guarded_row) <- Array.make width color.(2) ;
    for i = 0 to pred height do
     red.(i).(guarded_column) <- color.(0) ;
     green.(i).(guarded_column) <- color.(1) ;
     blue.(i).(guarded_column) <- color.(2)
    done ;
    [| red ; green ; blue |] ;;




(** {C § } *)
(** 
{1 Boîtes de dialogues}
{1 Dialog boxes}
*)
(** {C  } *)




(** {v float_rgb_crop_image approx_hori_size approx_verti_size rgb_matrix reduced_matrix crop_values v} The integer vector [crop_values] is modified in place.

Le vecteur entier [crop_values] est modifié en place.  *)
let float_rgb_crop_image = fun (approx_hori_size:int) (approx_verti_size:int) (m:float array array array) (reduc:float array array array) (crop_values:int array) ->
 assert ( Array.length crop_values >= 6 ) ;
 try
  begin
   let height = Array.length m.(0)
   and width = Array.length m.(0).(0)
   and h_pic = Array.length reduc.(0)
   and w_pic = Array.length reduc.(0).(0)
   and obsolete = Array.make 4 0
   and trace_left = ref 0
   and trace_right = ref 0
   and trace_top = ref 0
   and trace_bottom = ref 0
   and first_time = ref true
   and date = ref ""
   and steps = 8 in
    obsolete.(0) <- crop_values.(0) ;
    obsolete.(1) <- crop_values.(1) ;
    obsolete.(2) <- crop_values.(2) ;
    obsolete.(3) <- crop_values.(3) ;
    let hh = height - 2
    and ww = width - 2
    and hori_range = [| 0 ; pred width |]
    and verti_range = [| 0 ; pred height |]
    and small_radius = max 10 ( approx_verti_size / 50 )
    and factor = height / h_pic
    and h_border = ( 23 * h_pic ) / 100
    and w_border = ( 23 * w_pic ) / 100 in
     let hori_shift = w_border / 2
     and verti_shift = h_border / 2
     and hori_mid = ( 5 * w_border ) / 2
     and verti_mid = ( 5 * h_border ) / 2
     and hori_size = 2 * w_border + w_pic
     and verti_size = 2 * h_border + h_pic in
      let ls0 = ref [| 0 |] and ls1 = ref [| 0 |] and ls2 = ref [| 0 |] and ls3 = ref [| 0 |] 
      and rs0 = ref [| 0 |] and rs1 = ref [| 0 |] and rs2 = ref [| 0 |] and rs3 = ref [| 0 |]
      and vl = ref [| 0 |] and vr = ref [| 0 |] and sel = ref [| "0" |] and ser = ref [| "0" |]
      and cs0 = ref [| 0 |] and cs1 = ref [| 0 |] and cs2 = ref [| 0 |] and cs3 = ref [| 0 |]
      and hs0 = ref [| 0 |] and hs1 = ref [| 0 |] and hs2 = ref [| 0 |] and hs3 = ref [| 0 |]
      and vt = ref [| 0 |] and vb = ref [| 0 |] and set = ref [| "0" |] and seb = ref [| "0" |]
      and left_initial = ref ( string_of_int crop_values.(0) ) and left_final = ref ""
      and right_initial = ref ( string_of_int crop_values.(1) ) and right_final = ref ""
      and top_initial = ref ( string_of_int crop_values.(2) ) and top_final = ref ""
      and bottom_initial = ref ( string_of_int crop_values.(3) ) and bottom_final = ref ""
      and refresh_left = ref [| 0 |] and refresh_right = ref [| 0 |] and refresh_top = ref [| 0 |] and refresh_bottom = ref [| 0 |] 
      and cancel_message = [| "Annuler" ; "Cancel" |] and ok_message = [| " OK " |]
      and refresh_message = [| "Rafraîchir" ; "Refresh" |] and grab_message = ref "" and save_name = ref "" and current_character = ref "0" 
      and ok = ref [| 0 |] and cancel = ref [| 0 |] and grab = ref [| 0 |] in
       Graphics.close_graph () ;
       let f_init = function () ->
        begin (** f_init *)
         Gc.compact () ;
         if !first_time then
          begin
           Graphics.open_graph ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
           Graphics.set_window_title "Analogic.float_rgb_crop_image"
          end ;
         left_initial := string_of_int crop_values.(0) ;
         left_final := "" ;
         right_initial := string_of_int crop_values.(1) ;
         right_final := "" ;
         top_initial := string_of_int crop_values.(2) ;
         top_final := "" ;
         bottom_initial := string_of_int crop_values.(3) ;
         bottom_final := "" ;
         Graphics.set_color  Graphics.background ;
         Graphics.fill_rect ( hori_size / 2 ) 0 ( hori_size / 2 ) verti_shift ;
         Graphics.moveto ( hori_size / 2 ) 0 ;
         Graphics.set_color  Graphics.foreground ;
         Graphics.draw_string !grab_message ;
         ok := Widget.rect_switch ( hori_size / 2 ) verti_shift Graphics.green Graphics.black ok_message ;
         cancel := Widget.rect_switch ( hori_size / 2 ) ( verti_size - verti_shift ) Graphics.red Graphics.black cancel_message ;
         grab :=Widget.rect_switch hori_shift verti_shift Graphics.cyan Graphics.black [| "Saisir" ; "Grab" |] ;
         ls0 := Widget.left_switch w_border verti_shift Graphics.cyan Graphics.black [| " " |] ;
         ls1 := Widget.left_switch ( hori_size - hori_mid ) verti_shift Graphics.cyan Graphics.black [| " " |] ;
         ls2 := Widget.left_switch ( hori_size - hori_mid ) ( verti_size - verti_shift ) Graphics.cyan Graphics.black [| " " |] ;
         ls3 := Widget.left_switch w_border ( verti_size - verti_shift ) Graphics.cyan Graphics.black [| " " |] ;
         rs0 := Widget.right_switch hori_mid verti_shift Graphics.blue Graphics.black [| " " |] ;
         rs1 := Widget.right_switch ( hori_size - w_border ) verti_shift Graphics.blue Graphics.black [| " " |] ;
         rs2 := Widget.right_switch ( hori_size - w_border ) ( verti_size - verti_shift ) Graphics.blue Graphics.black [| " " |] ;
         rs3 := Widget.right_switch hori_mid ( verti_size - verti_shift ) Graphics.blue Graphics.black [| " " |] ;
         vl := Widget.int_vernier ( w_border + ( 3 * hori_shift / 2 ) ) verti_shift small_radius Graphics.red Graphics.yellow Graphics.red Graphics.white Graphics.black steps hori_range crop_values.(0) ;
         vr := Widget.int_vernier ( hori_size - ( w_border + ( 3 * hori_shift / 2 ) ) ) verti_shift small_radius Graphics.magenta Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps hori_range crop_values.(1) ;
         sel := Widget.string_edit ( 2 * h_border + hori_shift / 2 ) ( verti_size - verti_shift / 2 ) w_border Graphics.black Graphics.white Graphics.yellow !left_initial !left_final ;
         refresh_left := Widget.rect_switch ( 2 * h_border + hori_shift / 2 ) ( verti_size - ( 3 * verti_shift ) / 2 ) Graphics.yellow Graphics.black refresh_message ;
         ser := Widget.string_edit ( hori_size - ( 7 * hori_shift ) / 2 ) ( verti_size - verti_shift / 2 ) w_border Graphics.black Graphics.white Graphics.yellow !right_initial !right_final ;
         refresh_right := Widget.rect_switch ( hori_size - ( 7 * hori_shift ) / 2 ) ( verti_size - ( 3 * verti_shift ) / 2 ) Graphics.yellow Graphics.black refresh_message ;
         cs0 := Widget.cup_switch hori_shift h_border Graphics.blue Graphics.black [| "" |] ;
         cs1 := Widget.cup_switch ( hori_size - hori_shift ) h_border Graphics.blue Graphics.black [| "" |] ;
         cs2 := Widget.cup_switch ( hori_size - hori_shift ) ( verti_size - verti_mid ) Graphics.blue Graphics.black [| "" |] ;
         cs3 := Widget.cup_switch hori_shift ( verti_size - verti_mid ) Graphics.blue Graphics.black [| "" |] ;
         hs0 := Widget.hat_switch hori_shift verti_mid Graphics.cyan Graphics.black [| "" |] ;
         hs1 := Widget.hat_switch ( hori_size - hori_shift ) verti_mid Graphics.cyan Graphics.black [| "" |] ;
         hs2 := Widget.hat_switch ( hori_size - hori_shift ) ( verti_size - h_border ) Graphics.cyan Graphics.black [| "" |] ;
         hs3 := Widget.hat_switch hori_shift ( verti_size - h_border ) Graphics.cyan Graphics.black [| "" |] ;
         vt := Widget.int_vernier hori_shift ( verti_size - ( h_border + ( ( 3 * verti_shift ) / 2 ) ) ) small_radius Graphics.red Graphics.yellow Graphics.red Graphics.white Graphics.black steps verti_range crop_values.(2) ;
         vb := Widget.int_vernier hori_shift ( h_border + ( ( 3 * verti_shift ) / 2 ) ) small_radius Graphics.magenta Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps verti_range crop_values.(3) ;
         set := Widget.string_edit ( hori_size - hori_shift ) ( verti_size - 2 * h_border ) ( 3 * hori_shift / 2 ) Graphics.black Graphics.white Graphics.yellow !top_initial !top_final ;
         refresh_top := Widget.rect_switch ( hori_size - hori_shift ) ( verti_size -  h_border - verti_shift ) Graphics.yellow Graphics.black refresh_message ;
         seb := Widget.string_edit ( hori_size - hori_shift ) ( 2 * h_border ) ( 3 * hori_shift / 2 ) Graphics.black Graphics.white Graphics.yellow !bottom_initial !bottom_final ;
         refresh_bottom := Widget.rect_switch ( hori_size - hori_shift ) (  h_border + verti_shift ) Graphics.yellow Graphics.black refresh_message ;
         trace_left := w_border + crop_values.(0) / factor ;
         trace_right := w_border + crop_values.(1) / factor ;
         trace_top := h_border + h_pic - crop_values.(2) / factor ;
         trace_bottom := h_border + h_pic - crop_values.(3) / factor ;
         if !first_time then
          display_float_rgb w_border h_border reduc
         else
          begin
           relative_draw_column_float_rgb w_border h_border reduc obsolete.(0) ;
           relative_draw_column_float_rgb w_border h_border reduc obsolete.(1) ;
           relative_draw_line_float_rgb w_border h_border reduc obsolete.(2) ;
           relative_draw_line_float_rgb w_border h_border reduc obsolete.(3) ;
           Graphics.set_color Graphics.red ;
           Graphics.draw_segments [| ( !trace_left , h_border + 1 , !trace_left , h_border + h_pic - 2 ) ;
            ( w_border + 1 , !trace_top , w_border + w_pic - 2 , !trace_top ) |] ;
           Graphics.set_color Graphics.magenta ;
           Graphics.draw_segments [| ( !trace_right , h_border + 1 , !trace_right , h_border + h_pic - 2 ) ;
            ( w_border + 1 , !trace_bottom , w_border + w_pic - 2 , !trace_bottom ) |] ;
          end ;
        end (** f_init *)
       and f_end = function () -> ( Graphics.close_graph () ; raise Widget.End )
       and f_key = function c -> ( current_character := Char.escaped c ) in
        let f_mouse = fun x y ->
         begin (** f_mouse *)
          first_time := false ;
          let left_choice = Widget.over_int_vernier !vl x y
          and right_choice = Widget.over_int_vernier !vr x y
          and top_choice = Widget.over_int_vernier !vt x y
          and bottom_choice = Widget.over_int_vernier !vb x y in
           if Widget.is_over_rect_button !cancel x y then
            begin
             crop_values.(0) <- 0 ;
             crop_values.(1) <- succ width ;
             crop_values.(2) <- 0 ;
             crop_values.(3) <- succ height ;
             crop_values.(4) <- width ;
             crop_values.(5) <- height ;
             f_end ()
            end
           else
           if Widget.is_over_rect_button !ok x y then
            f_end () 
           else
           if Widget.is_over_rect_button !grab x y then
            begin
             let d = Unix.localtime ( Unix.time () ) in
              date := ( string_of_int ( 1900 + d.Unix.tm_year ) ) ^ "." ^ ( string_of_int ( succ d.Unix.tm_mon ) ) ^ "." ^ ( string_of_int d.Unix.tm_mday )
               ^ "." ^ ( string_of_int d.Unix.tm_hour ) ^ "." ^ ( string_of_int  d.Unix.tm_min) ^ "." ^ ( string_of_int d.Unix.tm_sec ) ;
              save_name := "Analogic.float_rgb_crop_image-" ^ !date ^ ".ppm" ;
              Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 hori_size verti_size ) ) !save_name ;
              grab_message := " Last grab : " ^ !save_name ;
              f_init ()
            end
           else
           if ( Widget.is_over_left_button !ls0 x y ) || ( Widget.is_over_left_button !ls3 x y ) then 
            begin
             obsolete.(0) <- crop_values.(0) / factor ; 
             crop_values.(0) <- max 0 ( pred crop_values.(0) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_left_button !ls1 x y ) || ( Widget.is_over_left_button !ls2 x y) then 
            begin
             obsolete.(1) <- crop_values.(1) / factor ; 
             crop_values.(1) <- max 0 ( pred crop_values.(1) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_right_button !rs0 x y ) || ( Widget.is_over_right_button !rs3 x y ) then 
            begin
             obsolete.(0) <- crop_values.(0) / factor ; 
             crop_values.(0) <- succ ( min ww crop_values.(0) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_right_button !rs1 x y ) || ( Widget.is_over_right_button !rs2 x y) then 
            begin
             obsolete.(1) <- crop_values.(1) / factor ; 
             crop_values.(1) <- succ ( min ww crop_values.(1) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_hat_button !hs3 x y ) || ( Widget.is_over_hat_button !hs2 x y ) then 
            begin
             obsolete.(2) <- crop_values.(2) / factor ; 
             crop_values.(2) <- max 0 ( pred crop_values.(2) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_hat_button !hs0 x y ) || ( Widget.is_over_hat_button !hs1 x y ) then 
            begin
             obsolete.(3) <- crop_values.(3) / factor ; 
             crop_values.(3) <- max 0 ( pred crop_values.(3) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_cup_button !cs3 x y ) || ( Widget.is_over_cup_button !cs2 x y ) then 
            begin
             obsolete.(2) <- crop_values.(2) / factor ; 
             crop_values.(2) <- succ ( min hh crop_values.(2) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_cup_button !cs0 x y ) || ( Widget.is_over_cup_button !cs1 x y ) then 
            begin
             obsolete.(3) <- crop_values.(3) / factor ; 
             crop_values.(3) <- succ ( min hh crop_values.(3) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_rect_button !refresh_left x y ) || ( Widget.is_over_rect_button !refresh_right x y ) || ( Widget.is_over_rect_button !refresh_top x y ) || ( Widget.is_over_rect_button !refresh_bottom x y )
            then f_init ()
           else
           if left_choice != crop_values.(0) then
            begin
             obsolete.(0) <- crop_values.(0) / factor ; 
             crop_values.(0) <- left_choice ;
             vl := Widget.int_vernier ( w_border + ( 3 * hori_shift / 2 ) ) verti_shift small_radius Graphics.red Graphics.yellow Graphics.red Graphics.black Graphics.white steps hori_range crop_values.(0) ;
             f_init ()
            end
           else
           if right_choice != crop_values.(1) then
            begin
             obsolete.(1) <- crop_values.(1) / factor ; 
             crop_values.(1) <- right_choice ;
             vr := Widget.int_vernier ( hori_size - ( w_border + ( 3 * hori_shift / 2 ) ) ) verti_shift small_radius Graphics.magenta Graphics.yellow Graphics.magenta Graphics.black Graphics.white steps hori_range crop_values.(1) ;
             f_init ()
            end
           else
           if top_choice != crop_values.(2) then
            begin
             obsolete.(2) <- crop_values.(2) / factor ; 
             crop_values.(2) <- top_choice ;
             vt := Widget.int_vernier hori_shift ( h_border + ( ( 3 * verti_shift ) / 2 ) ) small_radius Graphics.red Graphics.yellow Graphics.red Graphics.black Graphics.white steps verti_range crop_values.(2) ;
             f_init ()
            end
           else
           if bottom_choice != crop_values.(3) then
            begin
             obsolete.(3) <- crop_values.(3) / factor ; 
             crop_values.(3) <- bottom_choice ;
             vb := Widget.int_vernier hori_shift ( verti_size - ( h_border + ( ( 3 * verti_shift ) / 2 ) ) ) small_radius Graphics.magenta Graphics.yellow Graphics.magenta Graphics.black Graphics.white steps verti_range crop_values.(3) ;
             f_init ()
            end
           else
           if Widget.is_over_string_edit !sel x y then
            let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
            and old_left = crop_values.(0)
            and old_obs = obsolete.(0)
            and old_initial = !left_initial
            and old_final = !left_final in
             while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
              f_key ( !ev.Graphics.key ) ;
              let res = Widget.string_treatment !left_initial !left_final !current_character in
               left_initial := res.(0) ;
               left_final := res.(1) ;
               begin
                try
                 begin
                  obsolete.(0) <- crop_values.(0) / factor ;
                  crop_values.(0) <- int_of_string ( !left_initial ^ !left_final )
                 end
                with Failure unknown -> ( obsolete.(0) <- old_obs ; crop_values.(0) <- old_left ; left_initial := old_initial ; left_final := old_final ) ;
               end ;
               relative_draw_column_float_rgb w_border h_border reduc obsolete.(0) ;
               relative_draw_column_float_rgb w_border h_border reduc crop_values.(0) ;
               sel := Widget.string_edit ( int_of_string !sel.(0) ) ( int_of_string !sel.(1) ) ( int_of_string !sel.(2) ) Graphics.black Graphics.white Graphics.green !left_initial !left_final ;
               ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
             done
           else
           if Widget.is_over_string_edit !ser x y then
            let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
            and old_right = crop_values.(1)
            and old_obs = obsolete.(1)
            and old_initial = !right_initial
            and old_final = !right_final in
             while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
              f_key ( !ev.Graphics.key ) ;
              let res = Widget.string_treatment !right_initial !right_final !current_character in
               right_initial := res.(0) ;
               right_final := res.(1) ;
               begin
                try
                 begin
                  obsolete.(1) <- crop_values.(1) / factor ;
                  crop_values.(1) <- int_of_string ( !right_initial ^ !right_final )
                 end
                with Failure unknown -> ( obsolete.(1) <- old_obs ; crop_values.(1) <- old_right ; right_initial := old_initial ; right_final := old_final ) ;
               end ;
               relative_draw_column_float_rgb w_border h_border reduc obsolete.(1) ;
               relative_draw_column_float_rgb w_border h_border reduc crop_values.(1) ;
               ser := Widget.string_edit ( int_of_string !ser.(0) ) ( int_of_string !ser.(1) ) ( int_of_string !ser.(2) ) Graphics.black Graphics.white Graphics.green !right_initial !right_final ;
               ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
             done
           else
           if Widget.is_over_string_edit !set x y then
            let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
            and old_top = crop_values.(2)
            and old_obs = obsolete.(2)
            and old_initial = !top_initial
            and old_final = !top_final in
             while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
              f_key ( !ev.Graphics.key ) ;
              let res = Widget.string_treatment !top_initial !top_final !current_character in
               top_initial := res.(0) ;
               top_final := res.(1) ;
               begin
                try
                 begin
                  obsolete.(2) <- crop_values.(2) / factor ;
                  crop_values.(2) <- int_of_string ( !top_initial ^ !top_final )
                 end
                with Failure unknown -> ( obsolete.(2) <- old_obs ; crop_values.(2) <- old_top ; top_initial := old_initial ; top_final := old_final ) ;
               end ;
               relative_draw_line_float_rgb w_border h_border reduc obsolete.(2) ;
               relative_draw_line_float_rgb w_border h_border reduc crop_values.(2) ;
               set := Widget.string_edit ( int_of_string !set.(0) ) ( int_of_string !set.(1) ) ( int_of_string !set.(2) ) Graphics.black Graphics.white Graphics.green !top_initial !top_final ;
               ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
             done
           else
           if Widget.is_over_string_edit !seb x y then
            let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
            and old_bottom = crop_values.(3)
            and old_obs = obsolete.(3)
            and old_initial = !bottom_initial
            and old_final = !bottom_final in
             while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
              f_key ( !ev.Graphics.key ) ;
              let res = Widget.string_treatment !bottom_initial !bottom_final !current_character in
               bottom_initial := res.(0) ;
               bottom_final := res.(1) ;
               begin
                try
                 begin
                  obsolete.(3) <- crop_values.(3) / factor ;
                  crop_values.(3) <- int_of_string ( !bottom_initial ^ !bottom_final )
                 end
                with Failure unknown -> ( obsolete.(3) <- old_obs ; crop_values.(3) <- old_bottom ; bottom_initial := old_initial ; bottom_final := old_final ) ;
               end ;
               relative_draw_line_float_rgb w_border h_border reduc obsolete.(3) ;
               relative_draw_line_float_rgb w_border h_border reduc crop_values.(3) ;
               seb := Widget.string_edit ( int_of_string !seb.(0) ) ( int_of_string !seb.(1) ) ( int_of_string !seb.(2) ) Graphics.black Graphics.white Graphics.green !bottom_initial !bottom_final ;
               ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
             done
         end (** f_mouse *)
        and f_except = function unknown -> ( Graphics.close_graph () ; raise Widget.End ) in
         Widget.skeleton f_init f_end f_key f_mouse f_except ;
         crop_values
  end
 with Widget.End -> Gc.compact () ;
 crop_values ;;



(** {v float_rgb_color_treatment approx_hori_size approx_verti_size reduced_rgb_matrix coefficients v} The integer vector [coefficients] is modified in place.

Le vecteur entier [coefficients] est modifié en place.  *)
let float_rgb_color_treatment = fun (approx_hori_size:int) (approx_verti_size:int) (reduc:float array array array) (coefficients:int array) ->
 assert ( Array.length coefficients >= 3 ) ;
 try
  begin
   let red = ref reduc.(0)
   and green = ref reduc.(1)
   and blue = ref reduc.(2)
   and rouge = ref coefficients.(0)
   and vert = ref coefficients.(1)
   and bleu = ref coefficients.(2)
   and h_pic = Array.length reduc.(0)
   and w_pic = Array.length reduc.(0).(0)
   and triplet = [| "-1" ; "0" ; "1" |]
   and first_time = ref true
   and date = ref "" in
    let h_border = ( 23 * h_pic ) / 100
    and w_border = ( 23 * w_pic ) / 100 in
     let hori_shift = w_border / 2
     and verti_shift = h_border / 2
     and hori_size = 2 * w_border + w_pic
     and verti_size = 2 * h_border + h_pic
     and sr = ref [| 0 |] and sg = ref [| 0 |] and sb = ref [| 0 |]
     and cancel_message = [| "Annuler" ; "Cancel" |] and ok_message = [| " OK " |]
     and grab_message = ref "" and save_name = ref "" and current_character = ref "0" 
     and ok = ref [| 0 |] and cancel = ref [| 0 |] and grab = ref [| 0 |] in
      let max_radius = ( min hori_size verti_size ) / 3
      and small_radius = max 10 ( verti_size / 50 ) in
       Graphics.close_graph () ;
       let f_init = function () ->
        begin (** f_init *)
         Gc.compact () ;
         if !first_time then
          begin
           Graphics.open_graph ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
           Graphics.set_window_title "Analogic.color_treatment" ;
           Graphics.moveto hori_shift ( 6 * verti_shift ) ;
           Graphics.draw_string "blue" ;
           Graphics.moveto hori_shift ( 4 * verti_shift ) ;
           Graphics.draw_string "green" ;
           Graphics.moveto hori_shift ( 2 * verti_shift ) ;
           Graphics.draw_string "red"
          end ;
         Graphics.set_color  Graphics.background ;
         Graphics.fill_rect ( hori_size / 2 ) 0 ( hori_size / 2 ) verti_shift ;
         Graphics.moveto ( hori_size / 2 ) 0 ;
         Graphics.set_color  Graphics.foreground ;
         Graphics.draw_string !grab_message ;
         ok := Widget.rect_switch ( hori_size / 2 ) verti_shift Graphics.green Graphics.black ok_message ;
         cancel := Widget.rect_switch ( hori_size / 2 ) ( verti_size - verti_shift ) Graphics.red Graphics.black cancel_message ;
         grab :=Widget.rect_switch hori_shift verti_shift Graphics.cyan Graphics.black [| "Saisir" ; "Grab" |] ;
         sr := Widget.cursor_round_selector hori_shift ( 3 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white triplet coefficients.(0) ;
         sg := Widget.cursor_round_selector hori_shift ( 5 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white triplet coefficients.(1) ;
         sb := Widget.cursor_round_selector hori_shift ( 7 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white triplet coefficients.(2) ;
         begin
          match !rouge with
          | 0 -> red := Matrix.matrix_float_scal_right_sub 255. reduc.(0)
          | 1 -> red := Matrix.zeros_float reduc.(0)
          | _ -> red := reduc.(0) ;
         end ;
         begin
          match !vert with
          | 0 -> green := Matrix.matrix_float_scal_right_sub 255. reduc.(1)
          | 1 -> green := Matrix.zeros_float reduc.(1)
          | _ -> green := reduc.(1) ;
         end ;
         begin
          match !bleu with
          | 0 -> blue := Matrix.matrix_float_scal_right_sub 255. reduc.(2)
          | 1 -> blue := Matrix.zeros_float reduc.(2)
          | _ -> blue := reduc.(2) ;
         end ;
         display_float_rgb w_border h_border [| !red ; !green ; !blue |] ;
        end (** f_init *)
       and f_end = function () -> ( Graphics.close_graph () ; raise Widget.End )
       and f_key = function c -> ( current_character := Char.escaped c ) in
        let f_mouse = fun x y ->
         begin (** f_mouse *)
          first_time := false ;
          rouge := Widget.over_selector !sr x y ;
          vert := Widget.over_selector !sg x y ;
          bleu := Widget.over_selector !sb x y ;
          if Widget.is_over_rect_button !cancel x y then
           begin
            coefficients.(0) <- 2 ;
            coefficients.(1) <- 2 ;
            coefficients.(2) <- 2 ;
            f_end ()
           end
          else
          if Widget.is_over_rect_button !ok x y then
           f_end () 
          else
          if Widget.is_over_rect_button !grab x y then
           begin
            let d = Unix.localtime ( Unix.time () ) in
             date := ( string_of_int ( 1900 + d.Unix.tm_year ) ) ^ "." ^ ( string_of_int ( succ d.Unix.tm_mon ) ) ^ "." ^ ( string_of_int d.Unix.tm_mday )
              ^ "." ^ ( string_of_int d.Unix.tm_hour ) ^ "." ^ ( string_of_int  d.Unix.tm_min) ^ "." ^ ( string_of_int d.Unix.tm_sec ) ;
             save_name := "Analogic.float_rgb_color_treatment-" ^ !date ^ ".ppm" ;
             Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 hori_size verti_size ) ) !save_name ;
             grab_message := " Last grab : " ^ !save_name ;
             f_init ()
           end
          else
          if !rouge != coefficients.(0) then
           begin
            coefficients.(0) <- !rouge ;
            begin
             match !rouge with
             | 0 -> red := Matrix.matrix_float_scal_right_sub 255. reduc.(0)
             | 1 -> red := Matrix.zeros_float reduc.(0)
             | _ -> red := reduc.(0) ;
            end ;
            f_init () 
           end
          else
          if !vert != coefficients.(1) then
           begin
            coefficients.(1) <- !vert ;
            begin
             match !vert with
             | 0 -> green := Matrix.matrix_float_scal_right_sub 255. reduc.(1)
             | 1 -> green := Matrix.zeros_float reduc.(1)
             | _ -> green := reduc.(1) ;
            end ;
            f_init () 
           end
          else
          if !bleu != coefficients.(2) then
           begin
            coefficients.(2) <- !bleu ;
            begin
             match !bleu with
             | 0 -> blue := Matrix.matrix_float_scal_right_sub 255. reduc.(2)
             | 1 -> blue := Matrix.zeros_float reduc.(2)
             | _ -> blue := reduc.(2) ;
            end ;
            f_init () 
           end
         end (** f_mouse *)
        and f_except = function unknown -> ( Graphics.close_graph () ; raise Widget.End ) in
         Widget.skeleton f_init f_end f_key f_mouse f_except ;
         coefficients
  end
 with Widget.End -> Gc.compact () ;
coefficients ;;




(** {v float_rgb_mark_image approx_hori_size approx_verti_size rgb_matrix reduced_matrix mark v} The real vector [mark] is modified in place.

Le vecteur réel [mark] est modifié en place. *)
let float_rgb_mark_image = fun (approx_hori_size:int) (approx_verti_size:int) (m:float array array array) (reduc:float array array array) (mark:float array) ->
 assert ( Array.length mark >= 4 ) ;
 try
  begin
   let height = Array.length m.(0)
   and width = Array.length m.(0).(0)
   and h_pic = Array.length reduc.(0)
   and w_pic = Array.length reduc.(0).(0)
   and obsolete = Array.make 2 0
   and trace_hori = ref 0
   and trace_verti = ref 0
   and first_time = ref true
   and date = ref ""
   and f_string = ( function s -> String.sub s 0 ( min 6 ( String.length s ) ) )
   and steps = 10 in
    mark.(0) <- float ( width / 2 ) ;
    mark.(1) <- float ( height / 2 ) ;
    mark.(2) <- 0. ;
    mark.(3) <- 0. ;
    let factor = height / h_pic
    and hh = ( float height ) -. 2.
    and ww = ( float width ) -. 2.
    and hori_range = [| 0 ; pred width |]
    and verti_range = [| 0 ; pred height |]
    and hori_grid = [| -10. ; 10. |]
    and verti_grid = [| -10. ; 10. |]
    and small_radius = max 10 ( approx_verti_size / 50 )
    and threshold = ( sqrt epsilon_float ) *. 20.
    and h_border = ( 23 * h_pic ) / 100
    and w_border = ( 23 * w_pic ) / 100 in
     let hori_shift = w_border / 2
     and verti_shift = h_border / 2
     and hori_mid = ( 5 * w_border ) / 2
     and verti_mid = ( 5 * h_border ) / 2
     and hori_size = 2 * w_border + w_pic
     and verti_size = 2 * h_border + h_pic in
      let ls0 = ref [| 0 |] and rs0 = ref [| 0 |] and vl = ref [| 0 |] and sel = ref [| "0" |]
      and cs0 = ref [| 0 |] and hs0 = ref [| 0 |] and vt = ref [| 0 |] and set = ref [| "0" |]
      and left_initial = ref ( string_of_int ( int_of_float mark.(0) ) ) and left_final = ref ""
      and top_initial = ref ( string_of_int ( int_of_float mark.(1) ) ) and top_final = ref ""
      and cancel_message = [| "Annuler" ; "Cancel" |] and ok_message = [| " OK " |]
      and refresh_message = [| "Rafraîchir" ; "Refresh" |] and grab_message = ref "" and save_name = ref "" and current_character = ref "0"
      and right_initial = ref ( string_of_float mark.(2) ) and right_final = ref ""
      and ls3 = ref [| 0 |] and rs3 = ref [| 0 |] and vr = ref [| 0. |] and ser = ref [| "0." |]
      and bottom_initial = ref ( string_of_float mark.(3) ) and bottom_final = ref ""
      and hs1 = ref [| 0 |]  and cs1 = ref [| 0 |] and vb = ref [| 0. |]  and seb = ref [| "0." |]
      and refresh_left = ref [| 0 |] and refresh_top = ref [| 0 |] and refresh_right = ref [| 0 |] and refresh_bottom = ref [| 0 |] 
      and ok = ref [| 0 |] and cancel = ref [| 0 |] and grab = ref [| 0 |]
      and picture = [| hori_size / 2 ; verti_size / 2 ; w_border ; h_border ; w_border + w_pic ; h_border + h_pic |] in
       Graphics.close_graph () ;
       let f_init = function () ->
        begin (** f_init *)
         Gc.compact () ;
         if !first_time then
          begin
           Graphics.open_graph ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
           Graphics.set_window_title "Analogic.float_rgb_mark_image"
          end ;
         left_initial := string_of_int ( int_of_float mark.(0) ) ;
         left_final := "" ;
         top_initial := string_of_int ( int_of_float mark.(1) ) ;
         top_final := "" ;
         right_initial := f_string ( string_of_float mark.(2) ) ;
         right_final := "" ;
         bottom_initial := f_string ( string_of_float mark.(3) ) ;
         bottom_final := "" ;
         Graphics.set_color  Graphics.background ;
         Graphics.fill_rect ( hori_size / 2 ) 0 ( hori_size / 2 ) verti_shift ;
         Graphics.moveto ( hori_size / 2 ) 0 ;
         Graphics.set_color  Graphics.foreground ;
         Graphics.draw_string !grab_message ;
         ok := Widget.rect_switch ( hori_size - hori_shift ) verti_shift Graphics.green Graphics.black ok_message ;
         cancel := Widget.rect_switch hori_shift ( verti_size - verti_shift ) Graphics.red Graphics.black cancel_message ;
         grab :=Widget.rect_switch hori_shift verti_shift Graphics.cyan Graphics.black [| "Saisir" ; "Grab" |] ;
         ls0 := Widget.left_switch ( w_border + hori_shift / 2 ) verti_shift Graphics.cyan Graphics.black [| " " |] ;
         rs0 := Widget.right_switch hori_mid verti_shift Graphics.blue Graphics.black [| " " |] ;
         vl := Widget.int_vernier ( w_border + ( 3 * hori_shift / 2 ) ) verti_shift small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps hori_range ( int_of_float mark.(0) ) ;
         sel := Widget.string_edit ( hori_size / 2 + hori_shift ) verti_shift w_border Graphics.black Graphics.white Graphics.yellow !left_initial !left_final ;
         refresh_left := Widget.rect_switch ( hori_size - w_border - hori_shift ) verti_shift Graphics.yellow Graphics.black refresh_message ;
         ls3 := Widget.left_switch ( w_border + hori_shift / 2 ) ( verti_size - verti_shift ) Graphics.cyan Graphics.black [| " " |] ;
         rs3 := Widget.right_switch hori_mid ( verti_size - verti_shift ) Graphics.blue Graphics.black [| " " |] ;
         vr := Widget.float_vernier ( w_border + ( 3 * hori_shift / 2 ) ) ( verti_size - verti_shift ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps hori_grid mark.(2) ;
         ser := Widget.string_edit ( hori_size / 2 + hori_shift ) ( verti_size - verti_shift ) w_border Graphics.black Graphics.white Graphics.yellow !right_initial !right_final ;
         refresh_right := Widget.rect_switch ( hori_size - w_border ) ( verti_size - verti_shift ) Graphics.yellow Graphics.black refresh_message ;
         cs0 := Widget.cup_switch hori_shift ( h_border + verti_shift / 2 ) Graphics.blue Graphics.black [| "" |] ;
         hs0 := Widget.hat_switch hori_shift verti_mid Graphics.cyan Graphics.black [| "" |] ;
         vt := Widget.int_vernier hori_shift ( 2 * h_border ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps verti_range ( int_of_float mark.(1) ) ;
         set := Widget.string_edit hori_shift ( verti_size - 2 * h_border ) ( 3 * hori_shift / 2 ) Graphics.black Graphics.white Graphics.yellow !top_initial !top_final ;
         refresh_top := Widget.rect_switch hori_shift ( verti_size - h_border - verti_shift ) Graphics.yellow Graphics.black refresh_message ;
         hs1 := Widget.hat_switch ( hori_size - hori_shift ) verti_mid Graphics.cyan Graphics.black [| "" |] ;
         cs1 := Widget.cup_switch ( hori_size - hori_shift ) ( h_border + verti_shift / 2 ) Graphics.blue Graphics.black [| "" |] ;
         vb := Widget.float_vernier ( hori_size - hori_shift ) ( 2 * h_border ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps verti_grid mark.(3) ;
         seb := Widget.string_edit ( 1 + hori_size - hori_shift ) ( verti_size - 2 * h_border ) ( 3 * hori_shift / 2 ) Graphics.black Graphics.white Graphics.yellow !bottom_initial !bottom_final ;
         refresh_bottom := Widget.rect_switch ( hori_size - hori_shift ) ( verti_size - h_border - verti_shift ) Graphics.yellow Graphics.black refresh_message ;
         trace_verti := w_border + ( int_of_float mark.(0) ) / factor ;
         trace_hori := h_border + h_pic - ( int_of_float mark.(1) ) / factor ;
         if !first_time then
          display_float_rgb w_border h_border reduc
         else
          begin
           relative_draw_column_float_rgb w_border h_border reduc obsolete.(0) ;
           relative_draw_line_float_rgb w_border h_border reduc obsolete.(1) ;
          end ;
         Graphics.set_color Graphics.red ;
         Graphics.draw_segments [| ( !trace_verti , h_border + 1 , !trace_verti , h_border + h_pic - 2 ) ;
          ( w_border + 1 , !trace_hori , w_border + w_pic - 2 , !trace_hori ) |] ;
        end (** f_init *)
       and f_end = function () -> ( Graphics.close_graph () ; raise Widget.End )
       and f_key = function c -> ( current_character := Char.escaped c ) in
        let f_mouse = fun x y ->
         begin (** f_mouse *)
          first_time := false ;
          let left_choice = Widget.over_int_vernier !vl x y
          and top_choice = Widget.over_int_vernier !vt x y
          and right_choice = Widget.over_float_vernier !vr x y
          and bottom_choice = Widget.over_float_vernier !vb x y in
           if Widget.is_over_rect_button !cancel x y then
            begin
             mark.(0) <- -1. ;
             mark.(1) <- -1. ;
             mark.(2) <- 0. ;
             mark.(3) <- 0. ;
             f_end ()
            end
           else
           if Widget.is_over_rect_button !ok x y then
            f_end () 
           else
           if Widget.is_over_rect_button !grab x y then
            begin
             let d = Unix.localtime ( Unix.time () ) in
              date := ( string_of_int ( 1900 + d.Unix.tm_year ) ) ^ "." ^ ( string_of_int ( succ d.Unix.tm_mon ) ) ^ "." ^ ( string_of_int d.Unix.tm_mday )
               ^ "." ^ ( string_of_int d.Unix.tm_hour ) ^ "." ^ ( string_of_int  d.Unix.tm_min) ^ "." ^ ( string_of_int d.Unix.tm_sec ) ;
              save_name := "Analogic.float_rgb_mark_image-" ^ !date ^ ".ppm" ;
              Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 hori_size verti_size ) ) !save_name ;
              grab_message := " Last grab : " ^ !save_name ;
              f_init ()
            end
           else
           if Widget.is_over_rect_button picture x y then 
            begin
             obsolete.(0) <- ( int_of_float mark.(0) ) / factor ; 
             obsolete.(1) <- ( int_of_float mark.(1) ) / factor ;
             mark.(0) <- float ( ( x - w_border ) * factor ) ;
             mark.(1) <- float ( ( verti_size - h_border - y ) * factor ) ;
             f_init ()
            end
           else
           if Widget.is_over_left_button !ls0 x y then 
            begin
             obsolete.(0) <- ( int_of_float mark.(0) ) / factor ; 
             mark.(0) <- max 0. ( mark.(0) -. 1. ) ;
             f_init ()
            end
           else
           if Widget.is_over_right_button !rs0 x y then 
            begin
             obsolete.(0) <- ( int_of_float mark.(0) ) / factor ; 
             mark.(0) <- 1. +. ( min ww mark.(0) ) ;
             f_init ()
            end
           else
           if Widget.is_over_hat_button !hs0 x y then 
            begin
             obsolete.(1) <- ( int_of_float mark.(1) ) / factor ;
             mark.(1) <- max 0. ( mark.(1) -. 1. ) ;
             f_init ()
            end
           else
           if Widget.is_over_cup_button !cs0 x y then 
            begin
             obsolete.(1) <- ( int_of_float mark.(1) ) / factor ;
             mark.(1) <- 1. +. ( min hh mark.(1) ) ;
             f_init ()
            end
           else
           if Widget.is_over_left_button !ls3 x y then 
            begin
             mark.(2) <- 0.1 *. ( float ( int_of_float  ( 10. *. mark.(2) -. 1. ) ) ) ;
             f_init ()
            end
           else
           if Widget.is_over_right_button !rs3 x y then 
            begin
             mark.(2) <- 0.1 *. ( float ( int_of_float ( 10. *. mark.(2) +. 1. ) ) ) ;
             f_init ()
            end
           else
           if Widget.is_over_hat_button !hs1 x y then 
            begin
             mark.(3) <- 0.1 *. ( float ( int_of_float ( 10. *. mark.(3) +. 1. ) ) ) ;
             f_init ()
            end
           else
           if Widget.is_over_cup_button !cs1 x y then 
            begin
             mark.(3) <- 0.1 *. ( float ( int_of_float  ( 10. *. mark.(3) -. 1. ) ) ) ;
             f_init ()
            end
           else
           if ( Widget.is_over_rect_button !refresh_left x y ) || ( Widget.is_over_rect_button !refresh_right x y ) || ( Widget.is_over_rect_button !refresh_top x y ) || ( Widget.is_over_rect_button !refresh_bottom x y )
            then f_init ()
           else
           if left_choice != int_of_float mark.(0) then
            begin
             obsolete.(0) <- ( int_of_float mark.(0) ) / factor ; 
             mark.(0) <- float left_choice ;
             vl := Widget.int_vernier ( w_border + ( 3 * hori_shift / 2 ) ) verti_shift small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.black Graphics.white steps hori_range ( int_of_float mark.(0) ) ;
             f_init ()
            end
           else
           if top_choice != int_of_float mark.(1) then
            begin
             obsolete.(1) <- ( int_of_float mark.(1) ) / factor ;
             mark.(1) <- float top_choice ;
             vt := Widget.int_vernier hori_shift ( 2 * h_border ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps verti_range ( int_of_float mark.(1) ) ;
             f_init ()
            end
           else
           if abs_float ( right_choice -. mark.(2) ) > threshold then
            begin
             mark.(2) <- right_choice ;
             vr := Widget.float_vernier ( w_border + ( 3 * hori_shift / 2 ) ) ( verti_size - verti_shift ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps hori_grid mark.(2) ;
             f_init ()
            end
           else
           if abs_float ( bottom_choice -. mark.(3) ) > threshold then
            begin
             mark.(3) <- bottom_choice ;
             vb := Widget.float_vernier ( hori_size - hori_shift ) ( 2 * h_border ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps verti_grid mark.(3) ;
             f_init ()
            end
           else
           if Widget.is_over_string_edit !sel x y then
            let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
            and old_left = string_of_int ( int_of_float mark.(0) )
            and old_obs = obsolete.(0)
            and old_initial = !left_initial
            and old_final = !left_final in
             while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
              f_key ( !ev.Graphics.key ) ;
              let res = Widget.string_treatment !left_initial !left_final !current_character in
               left_initial := res.(0) ;
               left_final := res.(1) ;
               begin
                try
                 begin
                  obsolete.(0) <- ( int_of_float mark.(0) ) / factor ; 
                  mark.(0) <- float ( int_of_string ( !left_initial ^ !left_final ) )
                 end
                with Failure unknown -> ( obsolete.(0) <- old_obs ; mark.(0) <- float ( int_of_string old_left ) ; left_initial := old_initial ; left_final := old_final ) ;
               end ;
               relative_draw_column_float_rgb w_border h_border reduc obsolete.(0) ;
               relative_draw_column_float_rgb w_border h_border reduc ( int_of_float mark.(0) ) ;
               sel := Widget.string_edit ( int_of_string !sel.(0) ) ( int_of_string !sel.(1) ) ( int_of_string !sel.(2) ) Graphics.black Graphics.white Graphics.green !left_initial !left_final ;
               ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
             done
           else
           if Widget.is_over_string_edit !set x y then
            let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
            and old_top = string_of_int ( int_of_float mark.(1) )
            and old_obs = obsolete.(1)
            and old_initial = !top_initial
            and old_final = !top_final in
             while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
              f_key ( !ev.Graphics.key ) ;
              let res = Widget.string_treatment !top_initial !top_final !current_character in
               top_initial := res.(0) ;
               top_final := res.(1) ;
               begin
                try
                 begin
                  obsolete.(1) <- ( int_of_float mark.(1) ) / factor ;
                  mark.(1) <- float ( int_of_string ( !top_initial ^ !top_final ) )
                 end
                with Failure unknown -> ( obsolete.(1) <- old_obs ; mark.(1) <- float ( int_of_string old_top ) ; top_initial := old_initial ; top_final := old_final ) ;
               end ;
               relative_draw_line_float_rgb w_border h_border reduc obsolete.(1) ;
               relative_draw_line_float_rgb w_border h_border reduc ( int_of_float mark.(1) ) ;
               set := Widget.string_edit ( int_of_string !set.(0) ) ( int_of_string !set.(1) ) ( int_of_string !set.(2) ) Graphics.black Graphics.white Graphics.green !top_initial !top_final ;
               ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
             done
           else
           if Widget.is_over_string_edit !ser x y then
            let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
            and old_right = f_string ( string_of_float mark.(2) )
            and old_initial = !right_initial
            and old_final = !right_final in
             while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
              f_key ( !ev.Graphics.key ) ;
              let res = Widget.string_treatment !right_initial !right_final !current_character in
               right_initial := res.(0) ;
               right_final := res.(1) ;
               begin
                try
                 begin
                  mark.(2) <- float_of_string ( !right_initial ^ !right_final )
                 end
                with Failure unknown -> ( mark.(2) <- float_of_string old_right ; right_initial := old_initial ; right_final := old_final ) ;
               end ;
               ser := Widget.string_edit ( int_of_string !ser.(0) ) ( int_of_string !ser.(1) ) ( int_of_string !ser.(2) ) Graphics.black Graphics.white Graphics.green !right_initial !right_final ;
               ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
             done
           else
           if Widget.is_over_string_edit !seb x y then
            let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
            and old_bottom = f_string ( string_of_float mark.(3) )
            and old_initial = !bottom_initial
            and old_final = !bottom_final in
             while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
              f_key ( !ev.Graphics.key ) ;
              let res = Widget.string_treatment !bottom_initial !bottom_final !current_character in
               bottom_initial := res.(0) ;
               bottom_final := res.(1) ;
               begin
                try
                 begin
                  mark.(3) <- float_of_string ( !bottom_initial ^ !bottom_final )
                 end
                with Failure unknown -> ( mark.(3) <- float_of_string old_bottom ; bottom_initial := old_initial ; bottom_final := old_final ) ;
               end ;
               seb := Widget.string_edit ( int_of_string !seb.(0) ) ( int_of_string !seb.(1) ) ( int_of_string !seb.(2) ) Graphics.black Graphics.white Graphics.green !bottom_initial !bottom_final ;
               ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
             done
          end (** f_mouse *)
        and f_except = function unknown -> ( Graphics.close_graph () ; raise Widget.End ) in
         Widget.skeleton f_init f_end f_key f_mouse f_except ;
         mark
  end
 with Widget.End -> Gc.compact () ;
 mark ;;



(** {v float_rgb_refine_mark approx_hori_size approx_verti_size rgb_matrix mark v} The real vector [mark] is modified in place.

Le vecteur réel [mark] est modifié en place. *)
let float_rgb_refine_mark = fun (approx_hori_size:int) (approx_verti_size:int) (m:float array array array) (mark:float array) ->
 assert ( Array.length mark >= 4 ) ;
 try
  begin
   let height = Array.length m.(0)
   and width = Array.length m.(0).(0)
   and archive = [| mark.(0) ; mark.(1) ; mark.(2) ; mark.(3) |]
   and obsolete = Array.make 2 0
   and trace_hori = ref 0
   and trace_verti = ref 0
   and first_time = ref true
   and date = ref ""
   and f_string = ( function s -> String.sub s 0 ( min 6 ( String.length s ) ) )
   and steps = 10 in
    let g = Util.int_gcd height width
    and hh = ( float height ) -. 2.
    and ww = ( float width ) -. 2.
    and hori_focus = width / 40
    and verti_focus = height / 40
    and hori_range = [| 0 ; pred width |]
    and verti_range = [| 0 ; pred height |]
    and hori_grid = [| -10. ; 10. |]
    and verti_grid = [| -10. ; 10. |]
    and small_radius = max 10 ( approx_verti_size / 50 ) in
     let factor = ref ( min g ( max ( height / approx_hori_size ) ( width / approx_verti_size ) ) )
     and hori_beginning = max 0 ( ( int_of_float mark.(0) ) - hori_focus )
     and hori_ending = min ( width - 1 ) ( ( int_of_float mark.(0) ) + hori_focus )
     and verti_beginning = max 0 ( ( int_of_float mark.(1) ) - verti_focus )
     and verti_ending = min ( height - 1 ) ( ( int_of_float mark.(1) ) + verti_focus )
     and threshold = ( sqrt epsilon_float ) *. 20. in
      while g mod !factor != 0 do
       factor := succ !factor
      done ;
      if ( height mod !factor != 0 ) && ( width mod !factor != 0 ) then failwith "The under-sampling factor must divide the height and the width of the picture in Analogic.float_rgb_refine_mark." ;
      let h_pic = height / !factor
      and neighbour = ref ( Readwrite.rgb_crop m verti_beginning verti_ending hori_beginning hori_ending )
      and w_pic = width / !factor in
       let h_border = ( 23 * h_pic ) / 100
       and maximums = Array.map Matrix.matrix_float_max !neighbour
       and w_border = ( 23 * w_pic ) / 100 in
        let hori_shift = w_border / 2
        and verti_shift = h_border / 2
        and mm = Matrix.vector_float_max maximums
        and hori_mid = ( 5 * w_border ) / 2
        and verti_mid = ( 5 * h_border ) / 2
        and hori_size = 2 * w_border + w_pic
        and verti_size = 2 * h_border + h_pic in
         neighbour := Array.map ( Matrix.matrix_float_scal_mult ( 255. /. (1. +. mm ) ) ) !neighbour ;
         let ls0 = ref [| 0 |] and rs0 = ref [| 0 |] and vl = ref [| 0 |] and sel = ref [| "0" |]
         and cs0 = ref [| 0 |] and hs0 = ref [| 0 |] and vt = ref [| 0 |] and set = ref [| "0" |]
         and reduc = Readwrite.float_rgb_magnify ( 20 / !factor ) !neighbour
         and left_initial = ref ( string_of_int ( int_of_float mark.(0) ) ) and left_final = ref ""
         and top_initial = ref ( string_of_int ( int_of_float mark.(1) ) ) and top_final = ref ""
         and cancel_message = [| "Annuler" ; "Cancel" |] and ok_message = [| " OK " |]
         and refresh_message = [| "Rafraîchir" ; "Refresh" |] and grab_message = ref "" and save_name = ref "" and current_character = ref "0"
         and right_initial = ref ( string_of_float mark.(2) ) and right_final = ref ""
         and ls3 = ref [| 0 |] and rs3 = ref [| 0 |] and vr = ref [| 0. |] and ser = ref [| "0." |]
         and bottom_initial = ref ( string_of_float mark.(3) ) and bottom_final = ref ""
         and hs1 = ref [| 0 |]  and cs1 = ref [| 0 |] and vb = ref [| 0. |]  and seb = ref [| "0." |]
         and refresh_left = ref [| 0 |] and refresh_top = ref [| 0 |] and refresh_right = ref [| 0 |] and refresh_bottom = ref [| 0 |] 
         and ok = ref [| 0 |] and cancel = ref [| 0 |] and grab = ref [| 0 |]
         and picture = [| hori_size / 2 ; verti_size / 2 ; w_border ; h_border ; w_border + w_pic ; h_border + h_pic |] in
          Graphics.close_graph () ;
          let f_init = function () ->
           begin (** f_init *)
            Gc.compact () ;
            if !first_time then
             begin
              Graphics.open_graph ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
              Graphics.set_window_title "Analogic.float_rgb_refine_mark"
             end ;
            left_initial := string_of_int ( int_of_float mark.(0) ) ;
            left_final := "" ;
            top_initial := string_of_int ( int_of_float mark.(1) ) ;
            top_final := "" ;
            right_initial := f_string ( string_of_float mark.(2) ) ;
            right_final := "" ;
            bottom_initial := f_string ( string_of_float mark.(3) ) ;
            bottom_final := "" ;
            Graphics.set_color  Graphics.background ;
            Graphics.fill_rect ( hori_size / 2 ) 0 ( hori_size / 2 ) verti_shift ;
            Graphics.moveto ( hori_size / 2 ) 0 ;
            Graphics.set_color  Graphics.foreground ;
            Graphics.draw_string !grab_message ;
            ok := Widget.rect_switch ( hori_size - hori_shift ) verti_shift Graphics.green Graphics.black ok_message ;
            cancel := Widget.rect_switch hori_shift ( verti_size - verti_shift ) Graphics.red Graphics.black cancel_message ;
            grab :=Widget.rect_switch hori_shift verti_shift Graphics.cyan Graphics.black [| "Saisir" ; "Grab" |] ;
            ls0 := Widget.left_switch ( w_border + hori_shift / 2 ) verti_shift Graphics.cyan Graphics.black [| " " |] ;
            rs0 := Widget.right_switch hori_mid verti_shift Graphics.blue Graphics.black [| " " |] ;
            vl := Widget.int_vernier ( w_border + ( 3 * hori_shift / 2 ) ) verti_shift small_radius Widget.dark_grey Widget.dark_grey Graphics.magenta Graphics.yellow Graphics.white steps hori_range ( int_of_float mark.(0) ) ;
            sel := Widget.string_edit ( hori_size / 2 + hori_shift ) verti_shift w_border Widget.dark_grey Graphics.white Graphics.yellow !left_initial !left_final ;
            refresh_left := Widget.rect_switch ( hori_size - w_border - hori_shift ) verti_shift Graphics.yellow Graphics.black refresh_message ;
            ls3 := Widget.left_switch ( w_border + hori_shift / 2 ) ( verti_size - verti_shift ) Graphics.cyan Graphics.black [| " " |] ;
            rs3 := Widget.right_switch hori_mid ( verti_size - verti_shift ) Graphics.blue Graphics.black [| " " |] ;
            vr := Widget.float_vernier ( w_border + ( 3 * hori_shift / 2 ) ) ( verti_size - verti_shift ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps hori_grid mark.(2) ;
            ser := Widget.string_edit ( hori_size / 2 + hori_shift ) ( verti_size - verti_shift ) w_border Graphics.black Graphics.white Graphics.yellow !right_initial !right_final ;
            refresh_right := Widget.rect_switch ( hori_size - w_border ) ( verti_size - verti_shift ) Graphics.yellow Graphics.black refresh_message ;
            cs0 := Widget.cup_switch hori_shift ( h_border + verti_shift / 2 ) Graphics.blue Graphics.black [| "" |] ;
            hs0 := Widget.hat_switch hori_shift verti_mid Graphics.cyan Graphics.black [| "" |] ;
            vt := Widget.int_vernier hori_shift ( 2 * h_border ) small_radius Widget.dark_grey Widget.dark_grey Graphics.magenta Graphics.yellow Graphics.white steps verti_range ( int_of_float mark.(1) ) ;
            set := Widget.string_edit hori_shift ( verti_size - 2 * h_border ) ( 3 * hori_shift / 2 ) Widget.dark_grey Graphics.white Graphics.yellow !top_initial !top_final ;
            refresh_top := Widget.rect_switch hori_shift ( verti_size - h_border - verti_shift ) Graphics.yellow Graphics.black refresh_message ;
            hs1 := Widget.hat_switch ( hori_size - hori_shift ) verti_mid Graphics.cyan Graphics.black [| "" |] ;
            cs1 := Widget.cup_switch ( hori_size - hori_shift ) ( h_border + verti_shift / 2 ) Graphics.blue Graphics.black [| "" |] ;
            vb := Widget.float_vernier ( hori_size - hori_shift ) ( 2 * h_border ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps verti_grid mark.(3) ;
            seb := Widget.string_edit ( 1 + hori_size - hori_shift ) ( verti_size - 2 * h_border ) ( 3 * hori_shift / 2 ) Graphics.black Graphics.white Graphics.yellow !bottom_initial !bottom_final ;
            refresh_bottom := Widget.rect_switch ( hori_size - hori_shift ) ( verti_size - h_border - verti_shift ) Graphics.yellow Graphics.black refresh_message ;
            trace_verti := w_border + ( ( ( int_of_float mark.(0) ) - hori_beginning ) * 20 ) / !factor ;
            trace_hori := h_border + h_pic - ( ( ( int_of_float mark.(1) ) - verti_beginning )  * 20 ) / !factor ;
            if !first_time then
             display_float_rgb w_border h_border reduc
            else
             begin
              relative_draw_column_magnify_float_rgb ( 20 / !factor ) w_border h_border !neighbour obsolete.(0) ;
              relative_draw_line_magnify_float_rgb ( 20 / !factor ) w_border h_border !neighbour obsolete.(1) ;
             end ;
            Graphics.set_color Graphics.red ;
            Graphics.draw_segments [| ( !trace_verti , h_border + 1 , !trace_verti , h_border + h_pic - 2 ) ;
             ( w_border + 1 , !trace_hori , w_border + w_pic - 2 , !trace_hori ) |] ;
           end (** f_init *)
          and f_end = function () -> ( Graphics.close_graph () ; raise Widget.End )
          and f_key = function c -> ( current_character := Char.escaped c ) in
           let f_mouse = fun x y ->
            begin (** f_mouse *)
             first_time := false ;
             let right_choice = Widget.over_float_vernier !vr x y
             and bottom_choice = Widget.over_float_vernier !vb x y in
              if Widget.is_over_rect_button !cancel x y then
               begin
                mark.(0) <- archive.(0) ;
                mark.(1) <- archive.(1) ;
                mark.(2) <- archive.(2) ;
                mark.(3) <- archive.(3) ;
                f_end ()
               end
              else
              if Widget.is_over_rect_button !ok x y then
               f_end () 
              else
              if Widget.is_over_rect_button !grab x y then
               begin
                let d = Unix.localtime ( Unix.time () ) in
                 date := ( string_of_int ( 1900 + d.Unix.tm_year ) ) ^ "." ^ ( string_of_int ( succ d.Unix.tm_mon ) ) ^ "." ^ ( string_of_int d.Unix.tm_mday )
                  ^ "." ^ ( string_of_int d.Unix.tm_hour ) ^ "." ^ ( string_of_int  d.Unix.tm_min) ^ "." ^ ( string_of_int d.Unix.tm_sec ) ;
                 save_name := "Analogic.float_rgb_refine_mark-" ^ !date ^ ".ppm" ;
                 Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 hori_size verti_size ) ) !save_name ;
                 grab_message := " Last grab : " ^ !save_name ;
                 f_init ()
               end
              else
              if Widget.is_over_rect_button picture x y then 
               begin
                obsolete.(0) <- ( int_of_float mark.(0) ) - hori_beginning ;
                obsolete.(1) <- ( int_of_float mark.(1) ) - verti_beginning ;
                mark.(0) <- float ( ( x - w_border ) * !factor / 20 + hori_beginning ) ;
                mark.(1) <- float ( ( verti_size - h_border - y ) * !factor / 20 + verti_beginning ) ;
                f_init ()
               end
              else
              if Widget.is_over_left_button !ls0 x y then 
               begin
                obsolete.(0) <- ( int_of_float mark.(0) ) - hori_beginning ;
                mark.(0) <- max 0. ( mark.(0) -. 1. ) ;
                f_init ()
               end
              else
              if Widget.is_over_right_button !rs0 x y then 
               begin
                obsolete.(0) <- ( int_of_float mark.(0) ) - hori_beginning ;
                mark.(0) <- 1. +. ( min ww mark.(0) ) ;
                f_init ()
               end
              else
              if Widget.is_over_hat_button !hs0 x y then 
               begin
                obsolete.(1) <- ( int_of_float mark.(1) ) - verti_beginning ;
                mark.(1) <- max 0. ( mark.(1) -. 1. ) ;
                f_init ()
               end
              else
              if Widget.is_over_cup_button !cs0 x y then 
               begin
                obsolete.(1) <- ( int_of_float mark.(1) ) - verti_beginning ;
                mark.(1) <- 1. +. ( min hh mark.(1) ) ;
                f_init ()
               end
              else
              if Widget.is_over_left_button !ls3 x y then 
               begin
                mark.(2) <- 0.1 *. ( float ( int_of_float  ( 10. *. mark.(2) -. 1. ) ) ) ;
                f_init ()
               end
              else
              if Widget.is_over_right_button !rs3 x y then 
               begin
                mark.(2) <- 0.1 *. ( float ( int_of_float ( 10. *. mark.(2) +. 1. ) ) ) ;
                f_init ()
               end
              else
              if Widget.is_over_hat_button !hs1 x y then 
               begin
                mark.(3) <- 0.1 *. ( float ( int_of_float ( 10. *. mark.(3) +. 1. ) ) ) ;
                f_init ()
               end
              else
              if Widget.is_over_cup_button !cs1 x y then 
               begin
                mark.(3) <- 0.1 *. ( float ( int_of_float  ( 10. *. mark.(3) -. 1. ) ) ) ;
                f_init ()
               end
              else
              if ( Widget.is_over_rect_button !refresh_left x y ) || ( Widget.is_over_rect_button !refresh_right x y ) || ( Widget.is_over_rect_button !refresh_top x y ) || ( Widget.is_over_rect_button !refresh_bottom x y )
               then f_init ()
              else
              if abs_float ( right_choice -. mark.(2) ) > threshold then
               begin
                mark.(2) <- right_choice ;
                vr := Widget.float_vernier ( w_border + ( 3 * hori_shift / 2 ) ) ( verti_size - verti_shift ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps hori_grid mark.(2) ;
                f_init ()
               end
              else
              if abs_float ( bottom_choice -. mark.(3) ) > threshold then
               begin
                mark.(3) <- bottom_choice ;
                vb := Widget.float_vernier ( hori_size - hori_shift ) ( 2 * h_border ) small_radius Graphics.red Graphics.yellow Graphics.magenta Graphics.white Graphics.black steps verti_grid mark.(3) ;
                f_init ()
               end
              else
              if Widget.is_over_string_edit !ser x y then
               let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
               and old_right = f_string ( string_of_float mark.(2) )
               and old_initial = !right_initial
               and old_final = !right_final in
                while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
                 f_key ( !ev.Graphics.key ) ;
                 let res = Widget.string_treatment !right_initial !right_final !current_character in
                  right_initial := res.(0) ;
                  right_final := res.(1) ;
                  begin
                   try
                    begin
                     mark.(2) <- float_of_string ( !right_initial ^ !right_final )
                    end
                   with Failure unknown -> ( mark.(2) <- float_of_string old_right ; right_initial := old_initial ; right_final := old_final ) ;
                  end ;
                  ser := Widget.string_edit ( int_of_string !ser.(0) ) ( int_of_string !ser.(1) ) ( int_of_string !ser.(2) ) Graphics.black Graphics.white Graphics.green !right_initial !right_final ;
                  ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
                done
              else
              if Widget.is_over_string_edit !seb x y then
               let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
               and old_bottom = f_string ( string_of_float mark.(3) )
               and old_initial = !bottom_initial
               and old_final = !bottom_final in
                while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
                 f_key ( !ev.Graphics.key ) ;
                 let res = Widget.string_treatment !bottom_initial !bottom_final !current_character in
                  bottom_initial := res.(0) ;
                  bottom_final := res.(1) ;
                  begin
                   try
                    begin
                     mark.(3) <- float_of_string ( !bottom_initial ^ !bottom_final )
                    end
                   with Failure unknown -> ( mark.(3) <- float_of_string old_bottom ; bottom_initial := old_initial ; bottom_final := old_final ) ;
                  end ;
                  seb := Widget.string_edit ( int_of_string !seb.(0) ) ( int_of_string !seb.(1) ) ( int_of_string !seb.(2) ) Graphics.black Graphics.white Graphics.green !bottom_initial !bottom_final ;
                  ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
                done
             end (** f_mouse *)
           and f_except = function unknown -> ( Graphics.close_graph () ; raise Widget.End ) in
            Widget.skeleton f_init f_end f_key f_mouse f_except ;
            mark
  end
 with Widget.End -> Gc.compact () ;
 mark ;;



(** {v coefficient_list v} *)
let coefficient_list = [| "***" ; "0.01" ; "0.02" ; "0.05" ; "0.1" ; "0.2" ; "0.5" ; "1." ; "2." ; "5." ; "10." ; "20." ; "50." ; "100." ; "200." ; "500." |] ;;

(** {v other_coefficient_list v} *)
let other_coefficient_list = [| "***" ; "0.01" ; "0.02" ; "0.025" ; "0.04" ; "0.05" ; "0.1" ; "0.2" ; "0.25" ; "0.4" ; "0.5" ; "1." ; "2." ; "2.5" ; "4." ; "5." ; "10." ; "20." ; "25." ; "40." ; "50." ; "100." ; "200." ; "250." ; "400." ; "500." |] ;;

(** {v multiplicator_list v} *)
let multiplicator_list = Array.map float_of_string ( Array.sub coefficient_list 1 ( pred ( Array.length coefficient_list ) ) ) ;;

(** {v prefix_list v} *)
let prefix_list = [| "a" ; "f" ; "p" ; "n" ; "µ" ; "m" ; "c" ; "d" ; "" ; "da" ; "h" ; "k" ; "M" ; "G" ; "T" ; "P" ; "E" |] ;;

(** {v exponent_list v} *)
let exponent_list = [| 1e-18 ; 1e-15 ; 1e-12 ; 1e-9 ; 1e-6 ; 1e-3 ; 0.01 ; 0.1 ; 1. ; 10. ; 100. ; 1e3 ; 1e6 ; 1e9 ; 1e12 ; 1e15 ; 1e18 |] ;;

(** {v unit_list v} *)
let unit_list = [| "***" ; "V" ; "A" ; "h" ; "min" ; "s" ; "B" ; "Bm" ; "BA" ; "Hz" ; "m" ; "ft" ; "%" ; "DIV" ; "pix" |] ;;

(** {v visual_unit_list v} *)
let visual_unit_list = [| "DIV" ; "pix" ; "scr" |] ;;



(** {v choose_unit hori_size verti_size units v} The string vector [unit_strings] is modified in place.

Le vecteur de chaînes de caractères [unit_strings] est modifié en place. *)
let choose_unit = fun (hori_size:int) (verti_size:int) (unit_strings:string array) ->
 assert ( Array.length unit_strings >= 4 ) ;
 try
  begin
   let obsolete = ref ""
   and first_time = ref true
   and date = ref ""
   and cancel_message = [| "Annuler" ; "Cancel" |] and ok_message = [| " OK " |]
   and refresh_message = [| "Rafraîchir" ; "Refresh" |] and grab_message = ref "" and save_name = ref "" and current_character = ref "0"
   and sc = ref [| 0 |] and sp = ref [| 0 |] and su = ref [| 0 |] and sv = ref [| 0 |] 
   and coefficient = ref 7 and prefix = ref 8 and unit = ref 1 and visual_unit = ref 0
   and sec = ref [| "" |] and sep = ref [| "" |] and seu = ref [| "" |] and sev = ref [| "" |]
   and ok = ref [| 0 |] and cancel = ref [| 0 |] and grab = ref [| 0 |] and refresh = ref [| 0 |] in
    let hori_shift = hori_size / 20
    and verti_shift = verti_size / 20
    and coefficient_initial = ref coefficient_list.(!coefficient) and coefficient_final = ref ""
    and prefix_initial = ref prefix_list.(!prefix) and prefix_final = ref ""
    and unit_initial = ref unit_list.(!unit) and unit_final = ref ""
    and visual_unit_initial = ref visual_unit_list.(!visual_unit) and visual_unit_final = ref ""
    and max_radius = ( min hori_size verti_size ) / 3
    and small_radius = max 10 ( verti_size / 50 ) in
     unit_strings.(0) <- !coefficient_initial ;
     unit_strings.(1) <- !prefix_initial ;
     unit_strings.(2) <- !unit_initial ;
     unit_strings.(3) <- !visual_unit_initial ;
     Graphics.close_graph () ;
     let f_init = function () ->
      begin (** f_init *)
       Gc.compact () ;
       if !first_time then
        begin
         Graphics.open_graph ( " " ^ ( string_of_int hori_size ) ^ "x" ^ ( string_of_int verti_size ) ) ;
         Graphics.set_window_title "Analogic.choose_unit"
        end ;
       Graphics.set_color  Graphics.background ;
       Graphics.fill_rect ( 10 * hori_shift ) 0 ( 10 * hori_shift ) verti_shift ;
       Graphics.moveto ( 10 * hori_shift ) 0 ;
       Graphics.set_color  Graphics.foreground ;
       Graphics.draw_string !grab_message ;
       Graphics.moveto ( 16 * hori_shift ) ( 17 * verti_shift - ( snd ( Graphics.text_size "W" ) ) / 2 ) ;
       Graphics.draw_string "/" ;
       coefficient_initial := unit_strings.(0) ;
       coefficient_final := "" ;
       prefix_initial := unit_strings.(1) ;
       prefix_final := "" ;
       unit_initial := unit_strings.(2) ;
       unit_final := "" ;
       visual_unit_initial := unit_strings.(3) ;
       visual_unit_final :="" ;
       obsolete := unit_strings.(0) ;
       ok := Widget.rect_switch ( hori_size - 2 * hori_shift ) ( 3 * verti_shift ) Graphics.green Graphics.black ok_message ;
       cancel := Widget.rect_switch ( 2 * hori_shift ) ( verti_size - 2 * verti_shift ) Graphics.red Graphics.black cancel_message ;
       grab :=Widget.rect_switch ( 2 * hori_shift ) ( 2 * verti_shift ) Graphics.cyan Graphics.black [| "Saisir" ; "Grab" |] ;
       refresh := Widget.rect_switch ( 10 * hori_shift ) ( verti_size - verti_shift ) Graphics.yellow Graphics.black refresh_message ;
       sc := Widget.cursor_round_selector ( 5 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white coefficient_list !coefficient ;
       sp := Widget.cursor_round_selector ( 11 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white prefix_list !prefix ;
       su := Widget.cursor_round_selector ( 15 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white unit_list !unit ;
       sv := Widget.cursor_round_selector ( 18 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white visual_unit_list !visual_unit ;
       sec := Widget.string_edit ( 8 * hori_shift ) ( 17 * verti_shift ) ( 2 * hori_shift ) Graphics.black Graphics.white Graphics.yellow !coefficient_initial !coefficient_final ;
       sep := Widget.string_edit ( 11 * hori_shift ) ( 17 * verti_shift )( 2 *  hori_shift ) Widget.dark_grey Graphics.white Graphics.yellow !prefix_initial !prefix_final ;
       seu := Widget.string_edit ( 14 * hori_shift ) ( 17 * verti_shift ) ( 2 * hori_shift ) Graphics.black Graphics.white Graphics.yellow !unit_initial !unit_final ;
       sev := Widget.string_edit ( 18 * hori_shift ) ( 17 * verti_shift ) ( 2 * hori_shift ) Graphics.black Graphics.white Graphics.yellow !visual_unit_initial !visual_unit_final ;
       Graphics.set_color  Graphics.background ;
       Graphics.fill_rect ( 10 * hori_shift ) ( 15 * verti_shift ) ( 10 * hori_shift ) verti_shift ;
       Graphics.set_color  Graphics.foreground ;
       Graphics.moveto ( 10 * hori_shift ) ( 15 * verti_shift ) ;
       Graphics.draw_string ( unit_strings.(0) ^ " " ^ unit_strings.(1) ^ unit_strings.(2) ^ " / " ^ unit_strings.(3) ) ;
      end (** f_init *)
     and f_end = function () -> ( Graphics.close_graph () ; raise Widget.End )
     and f_key = function c -> ( current_character := Char.escaped c ) in
      let f_mouse = fun x y ->
       begin (** f_mouse *)
        first_time := false ;
        coefficient := Widget.over_selector !sc x y ;
        prefix := Widget.over_selector !sp x y ;
        unit := Widget.over_selector !su x y ;
        visual_unit := Widget.over_selector !sv x y ;
        if Widget.is_over_rect_button !cancel x y then
         begin
          unit_strings.(0) <- "" ;
          unit_strings.(1) <- "" ;
          unit_strings.(2) <- "" ;
          unit_strings.(3) <- "" ;
          f_end ()
         end
        else
        if Widget.is_over_rect_button !ok x y then
         f_end () 
        else
        if Widget.is_over_rect_button !grab x y then
         begin
          let d = Unix.localtime ( Unix.time () ) in
           date := ( string_of_int ( 1900 + d.Unix.tm_year ) ) ^ "." ^ ( string_of_int ( succ d.Unix.tm_mon ) ) ^ "." ^ ( string_of_int d.Unix.tm_mday )
            ^ "." ^ ( string_of_int d.Unix.tm_hour ) ^ "." ^ ( string_of_int  d.Unix.tm_min) ^ "." ^ ( string_of_int d.Unix.tm_sec ) ;
           save_name := "Analogic.choose_unit-" ^ !date ^ ".ppm" ;
           Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 hori_size verti_size ) ) !save_name ;
           grab_message := " Last grab : " ^ !save_name ;
           f_init ()
         end
        else
        if Widget.is_over_rect_button !refresh x y then
         f_init ()
        else
        if ( !coefficient != 0 ) && ( coefficient_list.(!coefficient) != unit_strings.(0) ) then
         begin
          unit_strings.(0) <- coefficient_list.(!coefficient) ;
          sc := Widget.cursor_round_selector ( 5 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white coefficient_list !coefficient ;
          f_init ()
         end
        else
        if prefix_list.(!prefix) != unit_strings.(1) then
         begin
          unit_strings.(1) <- prefix_list.(!prefix) ;
          sp := Widget.cursor_round_selector ( 11 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white prefix_list !prefix ;
          f_init ()
         end
        else
        if ( !unit != 0 ) && ( unit_list.(!unit) != unit_strings.(2) ) then
         begin
          unit_strings.(2) <- unit_list.(!unit) ;
          su := Widget.cursor_round_selector ( 15 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white unit_list !unit ;
          f_init ()
         end
        else
        if visual_unit_list.(!visual_unit) != unit_strings.(3) then
         begin
          unit_strings.(3) <- visual_unit_list.(!visual_unit) ;
          sv := Widget.cursor_round_selector ( 18 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white visual_unit_list !visual_unit ;
          f_init ()
         end
        else
        if Widget.is_over_string_edit !sec x y then
         let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] )
         and old_coefficient = unit_strings.(0)
         and old_obs = !obsolete
         and old_initial = !coefficient_initial
         and old_final = !coefficient_final in
          while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
           f_key ( !ev.Graphics.key ) ;
           let res = Widget.string_treatment !coefficient_initial !coefficient_final !current_character in
            coefficient_initial := res.(0) ;
            coefficient_final := res.(1) ;
            begin
             try
              begin
               let chaine = !coefficient_initial ^ !coefficient_final in
                ignore ( float_of_string chaine ) ;
                obsolete := unit_strings.(0) ;
                unit_strings.(0) <- chaine
              end
             with Failure unknown -> ( obsolete := old_obs ; unit_strings.(0) <- old_coefficient ; coefficient_initial := old_initial ; coefficient_final := old_final ) ;
            end ;
            sec := Widget.string_edit ( int_of_string !sec.(0) ) ( int_of_string !sec.(1) ) ( int_of_string !sec.(2) ) Graphics.black Graphics.white Graphics.green !coefficient_initial !coefficient_final ;
            coefficient := 0 ;
            sc := Widget.cursor_round_selector ( 5 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white coefficient_list !coefficient ;
            ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
          done
        else
        if Widget.is_over_string_edit !seu x y then
         let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] ) in
          while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
           f_key ( !ev.Graphics.key ) ;
           let res = Widget.string_treatment !unit_initial !unit_final !current_character in
            unit_initial := res.(0) ;
            unit_final := res.(1) ;
            unit_strings.(2) <- !unit_initial ^ !unit_final ;
            seu := Widget.string_edit ( int_of_string !seu.(0) ) ( int_of_string !seu.(1) ) ( int_of_string !seu.(2) ) Graphics.black Graphics.white Graphics.green !unit_initial !unit_final ;
            unit := 0 ;
            su := Widget.cursor_round_selector ( 5 * hori_shift ) ( 10 * verti_shift ) small_radius max_radius Graphics.red Graphics.yellow Graphics.black Graphics.green Graphics.white unit_list !unit ;
            ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
          done
       end (** f_mouse *)
      and f_except = function unknown -> ( Graphics.close_graph () ; raise Widget.End ) in
       Widget.skeleton f_init f_end f_key f_mouse f_except ;
       unit_strings
  end
 with Widget.End -> Gc.compact () ;
 unit_strings ;;




(** {C § } *)
(** 
{1 Calculs}
*)
(** {C  } *)




(** {v antigamma luminance_matrix v} *)
let antigamma = function (signal:float array array)->
 let m = 1. +. ( Matrix.matrix_float_max signal ) in
  let f = function x -> ( x *. m ) /. ( m -. x ) in
   Matrix.matrix_float_apply f signal ;;


(** {v prepare_trace threshold luminance_matrix v} This function corresponds to the function [split_traces]
when the monotrace mode is strectched.

Cette fonction correspond à la fonction [split_traces] quand le mode monotrace est forcé. *)
let prepare_trace = fun (threshold:float) (lumi:float array array) ->
 let mediane = Matrix.matrix_float_composed_median lumi in
  let signal = Matrix.float_transpose ( Matrix.matrix_float_crest ( threshold *. mediane ) lumi ) in
   let slopes = Array.map ( function x -> Infinitesimal.mean_float_discrete_diff 1. x ) signal in
    [| [| signal |] ; [| slopes |] |] ;;


(** {v split_traces threshold luminance_matrix v} A good value for the threshold is between around 0.1 and 10.
It depends much on the view capture and must be set by the user.
Output: transposed matrices of the peaks of the luminosities restricted to the zone of each trace: [traces],
partial derivatives (in the vertical direction of the picture) of the preceding ones: [slopes = diff_traces].

Une bonne valeur pour le seuil [threshold] est autour de 0.1 à 10.
Il dépend beaucoup de la prise de vue et doit être fixé par l'utilisateur.
Sortie : transposées des crêtes de luminosités restreintes aux zones de chaque trace : [traces], 
dérivées partielles (dans la direction verticale de l'image) des précédentes : [slopes = diff_traces]. *)
let split_traces = fun (threshold:float) (lumi:float array array) ->
 let ff = ( fun x y -> if ( x != y ) && ( abs x = abs y ) then true else false )
 and f = function x -> compare ( Matrix.vector_float_sum ( Matrix.vector_float_demakeup x ) ) 0. in
  let mediane = Matrix.matrix_float_composed_median lumi in
   let signal = Matrix.float_transpose ( Matrix.matrix_float_crest ( threshold *. mediane ) lumi ) in
    let slopes = Array.map ( function x -> Infinitesimal.mean_float_discrete_diff 1. x ) signal
    and width = Array.length signal
    and height = Array.length signal.(0) in
     let separations = Array.make_matrix width 1 0
     and hh = float height
     and nn = pred width
     and numbers = Array.make width 0 in
      let range = int_of_float ( 2. *. ( sqrt hh ) ) in
       let g = ( function x -> Matrix.vector_foa_demakeup ( Matrix.vector_foa_cut range ( Matrix.Float_vector_cons x ) ) )
       and factor = height / range
       and pr = pred range in
        let h = function x -> Array.map f ( g x )
        and rr = pred pr in
         let auxil = Array.map h slopes in
          let up = Array.map ( function x -> Array.sub x 0 pr ) auxil
          and down = Array.map ( function x -> Array.sub x 1 pr ) auxil in
           for i = 0 to nn do
            let column_up = up.(i)
            and column_down = down.(i)
            and sep = ref separations.(i) in
             for j = 0 to rr do
              if ff column_up.(j) column_down.(j) then
               sep := Array.append !sep [| ( factor * j + !sep.( pred ( Array.length !sep ) ) ) / 2 |] ;
             done ;
             sep := Array.append !sep [| pred height |] ;
             separations.(i) <- !sep ;
             numbers.(i) <- ( Array.length !sep ) - 2
           done ;
           let nombre = int_of_float ( Matrix.vector_float_median ( Matrix.float_of_vector numbers ) ) in
            let valid = Array.map ( ( = ) nombre ) numbers
            and t = Array.make nombre 0. in
             let beginning = Util.vector_find_first ( = ) true valid
             and traces = Array.map ( Array.make_matrix width height ) t
             and diff_traces = Array.map ( Array.make_matrix width height ) t in
              for i = 0 to pred nombre do
               let accu_high = ref ( separations.(beginning).( succ i ) )
               and accu_low = ref ( separations.(beginning).( i + 2 ) )
               and trace = traces.(i)
               and diff_trace = diff_traces.(i) in
                for j = 0 to beginning do
                 Array.blit signal.(j) !accu_high trace.(j) !accu_high ( !accu_low - !accu_high ) ;
                 Array.blit slopes.(j) !accu_high diff_trace.(j) !accu_high ( !accu_low - !accu_high )
                done ;
                for j = succ beginning to pred width do
                 if valid.(j) then
                  begin
                   accu_high := separations.(j).( succ i ) ;
                   accu_low := separations.(j).( 2 + i )
                  end ;
                 Array.blit signal.(j) !accu_high trace.(j) !accu_high ( !accu_low - !accu_high ) ;
                 Array.blit slopes.(j) !accu_high diff_trace.(j) !accu_high ( !accu_low - !accu_high )
                done
              done ;
              [| traces ; diff_traces |] ;;

(** {v bound_trace trace v} *)
let bound_trace = function (trace:float array array) ->
 let r = Array.length trace
 and height = Array.length trace.(0)
 and f = fun i x -> ( i , x )
 and cmp_lum = fun ( i , x ) ( j , y ) -> compare y x
 and cmp_index = fun ( i , x ) ( j , y ) -> compare i j in
  let b = Array.make_matrix r 2 0.
  and rr = pred r
  and first_decile = height / 10 in
   for i = 0 to rr do
    let x = Array.mapi f trace.(i)
    and row = b.(i) in
     Array.sort cmp_lum x ;
     let y = Array.sub x 0 first_decile in
      Array.sort cmp_index y ;
      row.(0) <- float ( fst y.(0) ) ;
      row.(1) <- float ( fst ( Util.array_last y ) ) ;
   done ;
   b ;;

(** {v digitize_step bounds weights trace slope v} *)
let digitize_step = fun (bounds:float array array) (weights:float array) (trace:float array array) (slope:float array array) ->
 let wl = Array.length weights
 and width = Array.length trace in
  if wl != width then failwith "Bad number of weights in Analogic.digitize." ;
  let result = Array.make width 0.
  and f = fun x y -> if y >= 0. then x else 1. -. x
  and g = fun x i y -> if y >= 0. then ( float i ) *. x else ( float i ) *. ( 1. -. x ) in
   for i = 0 to pred width do
    let form = trace.(i)
    and row = bounds.(i)
    and gradient = slope.(i)
    and w = weights.(i) in
     let ww = Array.mapi ( g w ) gradient
     and w_w = Array.map ( f w ) gradient in
      let position = ( Matrix.vector_float_scal_prod ww form ) /. ( Matrix.vector_float_scal_prod w_w form ) in
       result.(i) <- max row.(0) ( min position row.(1) ) ;
   done ;
   result ;;


(** {v thickness matrix v} *)
let thickness = function (trace:float array array) ->
 let width = Array.length trace in
  let thickness = Array.make width 0. in
   for i = 0 to pred width do
    let form = trace.(i) in
     let t = Array.map ( function x -> float ( compare x 0. ) ) form in
      thickness.(i) <- Matrix.vector_float_sum t
   done ;
   thickness ;;


(** {v direct_tame vector v} *)
let direct_tame = function (v:float array) ->
 let r = Array.length v
 and a = 0.5 in
  let w = Array.make r 0.
  and rr = pred r
  and rrr = r - 2 in
   for i = 1 to rrr do
    w.(i) <- a *. ( v.( pred i ) +. v.(i) )
   done ;
   w.(rr) <- v.(rrr) ;
   w.(0) <- w.(1) ;
   w ;;

(** {v tame degree vector v} *)
let tame = fun (n:int) (v:float array) ->
 let w = ref ( Matrix.vector_float_copy v ) in
  for i = 1 to n do
   w := direct_tame !w
  done ;
  !w ;;

(** {v weigh factor position v} *)
let weigh = fun (factor:float) (index:int) (position:float array) ->
  let kappa = Infinitesimal.discrete_graph_curvature Infinitesimal.mean_float_discrete_richardson_binary_diff factor ( tame index position ) in
   let f = function x -> 0.5 *. ( 1. -. 0.99 *. tanh ( x *. factor ) ) in
    tame index ( Array.map f kappa ) ;;


(** {v digitize threshold trace slope v} *)
let digitize = fun (threshold:float) (trace:float array array) (slope:float array array) ->
 let error = ref max_float
 and t = Matrix.vector_float_median ( thickness trace )
 and index = ref 1
 and rate = ref 1
 and bounds = bound_trace trace in
  let position = ref ( digitize_step bounds ( Array.make ( Array.length trace ) 0.5 ) trace slope ) in
   let old_position = ref !position
   and n = int_of_float ( 1. +. ( log t ) /. ( log 2. ) ) in
    while !error >= threshold do
     old_position := !position ;
     position := digitize_step bounds ( weigh t n !position ) trace slope ;
     error := Matrix.vector_float_norm_inf ( Matrix.vector_float_minus ( tame !rate !position ) ( tame !rate !old_position ) ) ;
     incr index ;
     rate := int_of_float ( sqrt ( float !index ) )
    done ;
    !position ;;


(** {v automatic_digitize trace slope v} *)
let automatic_digitize = fun (trace:float array array) (slope:float array array) ->
 let height = Array.length trace.(0) in
  let threshold = ( float ( height * 256 ) ) *. epsilon_float in
   digitize threshold trace slope ;;


(** {v linear_regression departure arrival v} *)
let linear_regression = fun (x:float array array) (y:float array array) ->
 Matrix.linear_regression ( Matrix.sym_float_tune_reduc 2e-4 30 ) x y ;;


(** {v autocrop luminance_matrix v} *)
let autocrop = function (lumi:float array array) ->
 let mediane = Matrix.matrix_float_composed_median lumi in
  let test = function x -> if ( x >= 0.8 *. mediane ) && ( x <= 1.2 *. mediane ) then 1. else 0. in
   let mask = Array.map ( Array.map test ) lumi in
    let hori_scan = Matrix.matrix_float_sum_by_row mask
    and verti_scan = Matrix.matrix_float_sum_by_column mask in
     let hm = Matrix.vector_float_median hori_scan
     and vm = Matrix.vector_float_median verti_scan in
      let hs = Array.map ( ( <= ) ( 0.8 *. hm ) ) hori_scan
      and vs = Array.map ( ( <= ) ( 0.8 *. vm ) ) verti_scan in
       [| Util.vector_find_first ( = ) true vs ; Util.vector_find_last ( = ) true vs ; Util.vector_find_first ( = ) true hs ; Util.vector_find_last ( = ) true hs ; Array.length lumi.(0) ; Array.length lumi |] ;;




(** {C § } *)
(** 
{1 Numérisation interactive}
{1 Interactive digitization}
*)
(** {C  } *)




(** The following functions save automatically some window captures in [ppm] files.
The numerical data are automatically saved in text files or [au] files.
The treatment data are saved in a file whose name ends with "metadata".
A good value for the threshold is between around 0.1 and 10.
It depends much on the view capture and must be set by the user.
The [monotrace] switch permits to stretch the work on a unique trace.
The integer [max_number] limits the display of selectors: go all over the lists with the central button.

Les fonctions suivantes sauvegardent automatiquement certaines saisies de fenêtres dans des fichiers [ppm].
Les données numériques sont sauvegardées automatiquement dans des fichiers textes ou des fichiers [au].
Les données de traitement sont sauvegardées dans un fichier dont le nom se termine par "metadata".
Une bonne valeur pour le seuil est autour de 0.1 à 10.
Il dépend beaucoup de la prise de vue et doit être fixé par l'utilisateur.
Le commutateur [monotrace] permet de forcer le travail sur une trace unique.
L'entier [max_number] limite l'affichage des sélecteurs : parcourir les listes avec le bouton central. *)



(** {v oscillo_recorder phosphor_switch monotrace_switch hori_size verti_size max_number v} This function acts only by side effect, writing data into files.
The [phosphor] switch permits using this function for oscilloscopes and graphic recorders.

Cette fonction opère uniquement par effet de bord, en écrivant les données dans des fichiers.
Le commutateur [phosphor] permet une utilisation pour les oscilloscopes, les tables traçantes, les enregistreurs graphiques. *)
let oscillo_recorder = fun (phosphor:bool) (monotrace:bool) (approx_hori_size:int) (approx_verti_size:int) (max_number:int) ->
 try
  begin
   let date = ref ""
   and step = ref 0
   and dname = ref ( Sys.getcwd () )
   and picture_file = ref ""
   and picture_address = ref ""
   and picture = ref ( Array.make_matrix 3 1 [| 0. |] )
   and height = ref 1
   and width = ref 1
   and factor = ref 1
   and reduc = ref ( Array.make_matrix 3 1 [| 0. |] )
   and coefficients = Array.make 3 2
   and threshold = [| 1. |]
   and marks = ref ( Array.make_matrix 0 4 0. )
   and mark = Array.make 4 0.
   and affine_isomorphism = ref [| [| [| 1. ; 0. |] ; [| 0. ; -1. |] |] ; [| [| 0. ; 0. |] |] |]
   and ll_corner = ref ( Array.make 2 0. )
   and ur_corner = ref ( Array.make 2 0. )
   and horizontal_units = [| "1." ; " " ; "pix" ; "pix" |]
   and vertical_units = [| "1." ; " " ; "pix" ; "pix" |]
   and source = ref ( Array.make_matrix 3 1 [| 0. |] )
   and bigomega = ref 0.
   and number_of_traces = ref 0
   and samples = ref ( Array.make_matrix 0 0 0. )
   and interval = ref [| 0. |]
   and curves = ref ( Array.make_matrix 0 0 [| 0. |] )
   and derivatives = ref ( Array.make_matrix 0 0 0. )
   and speeds = ref ( Array.make_matrix 0 0 [| 0. |] )
   and transfos = ref ( Array.make_matrix 0 0 [| 0. |] )
   and modules = ref ( Array.make_matrix 0 0 0. )
   and crop_values = ref ( Array.make 6 0 )
   and never_cropped = ref true
   and truncate = fun (peak:float) (x:float array) -> Array.map ( function y -> if classify_float y = FP_normal then min peak ( max 0. y ) else 0. ) x
   and accu_int = fun s x -> s ^ " " ^ ( string_of_int x )
   and accu_float = fun s x -> s ^ " " ^ ( string_of_float x )
   and end_message = [| " Terminer " ; " Finish " |]
   and go_on_messages = [| [| "Choisir le répertoire de l'image" ; "Choose the directory of the picture" |] ;
    [| "Choisir l'image" ; "Choose the picture" |] ;
    [| "Recadrer l'image" ; "Crop the picture" |] ;
    [| "Traiter les couleurs" ; "Treat the colors" |] ; 
    [| "Choisir le seuil" ; "Choose the threshold" |] ;
    [| "Affichage brut des courbes" ; "Raw display of the curves" |] ;
    [| "Marquer des points du" ; "réticule sur l'image" ; "Mark graticule points" ; "on the picture" |] ;
    [| "Raffiner le marquage" ; "Refine the marks" |] ;
    [| "Choisir les unités horizontales" ; "Choose the horizontal units" |] ;
    [| "Choisir les unités verticales" ; "Choose the vertical units" |] ;
    [| "Affichage des courbes" ; "en divisions" ; "Display of the curves" ; "in divisions" |] ;
    [| "Affichage des" ; "courbes en unités" ; "Display of the" ; "curves in units" |] ;
    [| "Affichage brut" ; "des dérivées" ; "Raw display of" ; "the derivatives" |] ;
    [| "Affichage brut" ; "des transformées" ; "de Fourier"; "Raw display of" ; "the Fourier" ; "transforms" |] ;
    [| "Suite" ; "Continuation" |] ;
    [| "Reprendre à partir" ; "du recadrage" ; "Resume from" ; "the cropping" |] ;
    [| "Terminer" ; "Finish" |] |]
   and save_name = ref "" and current_character = ref "0"
   and ending = ref [| 0 |] and go_on = ref [| 0 |] in
    let hori_shift = approx_hori_size / 20
    and metadata = ref [| "phosphor: " ^ ( string_of_bool phosphor ) ; "monotrace: " ^ ( string_of_bool monotrace ) ;
     "size: " ^ ( string_of_int approx_hori_size ) ^ "x" ^ ( string_of_int approx_verti_size ) ; "max number for selectors: " ^ ( string_of_int max_number ) |]
    and verti_shift = approx_verti_size / 20 in
     Graphics.close_graph () ;
     let f_init = function () ->
      begin
       Gc.compact () ;
       Graphics.open_graph ( " " ^ ( string_of_int approx_hori_size ) ^ "x" ^ ( string_of_int approx_verti_size ) ) ;
       Graphics.set_window_title "Analogic.oscillo" ;
       if !step = 0 then
        begin
         Graphics.moveto ( 5 * hori_shift ) ( 17 * verti_shift ) ;
         Graphics.draw_string "L'oscilloscope a été inventé par Karl Ferdinand Braun (1850-1918)." ;
         Graphics.moveto ( 5 * hori_shift ) ( 15 * verti_shift ) ;
         Graphics.draw_string "The oscilloscope has been invented by Karl Ferdinand Braun (1850-1918)."
        end ;
       go_on := Widget.rect_switch ( 10 * hori_shift ) ( 10 * verti_shift ) Graphics.green Graphics.black go_on_messages.(!step) ;
       ending := Widget.rect_switch ( approx_hori_size - hori_shift ) verti_shift Graphics.red Graphics.black end_message ;
      end
     and f_end = function () ->
      begin
       Graphics.close_graph () ;
       Readwrite.array_write_text_file !metadata ( !picture_file ^ !date ^ ".metadata" ) ;
       raise Widget.End
      end
     and f_key = function c -> ( current_character := Char.escaped c ) in
      let f_mouse = fun x y ->
       begin (** f_mouse *)
        if Widget.is_over_rect_button !ending x y then
         begin
          f_end ()
         end
        else
        if Widget.is_over_rect_button !go_on x y then
         begin
          match !step with
          | 0 -> (** Répertoire - Directory *)
           begin
            dname := Widget.choose_directory approx_hori_size approx_verti_size max_number !dname ;
            incr step ;
            f_init () 
           end
          | 1 -> (** Fichier - File *)
           begin
            picture_file := Widget.choose_regular_file approx_hori_size approx_verti_size max_number !dname ;
            picture_address := Filename.concat !dname !picture_file ;
            if ( ( String.length !picture_file ) = 0 ) || ( not ( Sys.file_exists !picture_address ) ) then
             step := 1
            else
             incr step ;
            f_init () 
           end
          | 2 -> (** Recadrage - Croping *)
           begin
            if !never_cropped then
             begin
              Widget.sand_glass approx_hori_size approx_verti_size ;
              picture := Readwrite.read_picture_float_rgb !picture_address ;
              height := Array.length !picture.(0) ;
              width := Array.length !picture.(0).(0) ;
              !affine_isomorphism.(1).(0).(1) <- float !height ;
              let g = Util.int_gcd !height !width in
               factor := min g ( 2 + ( max ( !height / approx_verti_size ) ( !width / approx_hori_size ) ) ) ;
               while g mod !factor != 0 do
                factor := succ !factor
               done ;
               if ( !height mod !factor != 0 ) && ( !width mod !factor != 0 ) then failwith "The under-sampling factor must divide the height and the width of the picture in Analogic.oscillo." ;
               reduc := Readwrite.matrix_float_rgb_under_sample !factor !picture ;
               crop_values := autocrop ( Readwrite.matrix_float_rgb_to_luminance !picture ) ;
               never_cropped := false
             end ;
            ignore ( float_rgb_crop_image approx_hori_size approx_verti_size !picture !reduc !crop_values ) ;
            metadata := Array.append !metadata [| "crop values: " ^ ( Array.fold_left accu_int "" !crop_values ) |] ;
            if not phosphor then
             incr step
            else step := !step + 2 ;
            f_init ()
           end
          | 3 -> (** Traitement des couleurs - Color treatment *)
           begin
            ignore ( float_rgb_color_treatment approx_hori_size approx_verti_size !reduc coefficients ) ;
            begin
             match coefficients.(0) with
             | 0 -> !picture.(0) <- Matrix.matrix_float_scal_right_sub 255. !picture.(0)
             | 1 -> !picture.(0) <- Matrix.zeros_float !picture.(0)
             | _ ->  ()
            end ;
            begin
             match coefficients.(1) with
             | 0 -> !picture.(1) <- Matrix.matrix_float_scal_right_sub 255. !picture.(1)
             | 1 -> !picture.(1) <- Matrix.zeros_float !picture.(1)
             | _ -> ()
            end ;
            begin
             match coefficients.(2) with
             | 0 -> !picture.(2) <- Matrix.matrix_float_scal_right_sub 255. !picture.(2)
             | 1 -> !picture.(2) <- Matrix.zeros_float !picture.(2)
             | _ -> ()
            end ;
            metadata := Array.append !metadata [| "red: " ^ ( string_of_int ( pred coefficients.(0) ) ) ; 
             "green: " ^ ( string_of_int ( pred coefficients.(1) ) ) ; "blue: " ^ ( string_of_int ( pred coefficients.(2) ) ) |] ;
            Gc.compact () ;
            incr step ;
            f_init () 
           end
          | 4 -> (** Seuil - Threshold *)
           begin
            ignore ( Widget.choose_real approx_hori_size approx_verti_size [| 0. ; 10. |] threshold ) ;
            metadata := Array.append !metadata [| "threshold: " ^ ( string_of_float threshold.(0) ) |] ;
            incr step ;
            f_init ()
           end
          | 5 -> (** Affichage brut des courbes - Raw display of the curves *)
           begin
            Widget.sand_glass approx_hori_size approx_verti_size ;
            source := Readwrite.rgb_crop !picture !crop_values.(2) !crop_values.(3) !crop_values.(0) !crop_values.(1) ;
            let d = Unix.localtime ( Unix.time () ) in
             date := ( string_of_int ( 1900 + d.Unix.tm_year ) ) ^ "." ^ ( string_of_int ( succ d.Unix.tm_mon ) ) ^ "." ^ ( string_of_int d.Unix.tm_mday ) 
              ^ "." ^ ( string_of_int d.Unix.tm_hour ) ^ "." ^ ( string_of_int d.Unix.tm_min ) ^ "." ^ ( string_of_int d.Unix.tm_sec ) ;
             Readwrite.write_float_rgb !source ( !picture_file ^ !date ) ;
             let luminance = Readwrite.matrix_float_rgb_to_luminance !source
             and splitting = ref ( Array.make 2 [| |] ) in
              if monotrace then
               begin
                splitting := prepare_trace threshold.(0) ( antigamma luminance ) ;
                number_of_traces := 1 ;
               end
              else
               begin
                splitting := split_traces threshold.(0) ( antigamma luminance ) ;
                number_of_traces := Array.length !splitting.(0) ;
               end ;
              samples := Array.make_matrix !number_of_traces ( Array.length !source ) 0. ;
              for i = 0 to pred !number_of_traces do
               !samples.(i) <- Matrix.vector_float_scal_add ( float !crop_values.(2) ) ( automatic_digitize !splitting.(0).(i) !splitting.(1).(i) ) ;
               Readwrite.vector_float_write !samples.(i) ( !picture_file ^ !date ^ "trace" ^ ( string_of_int i ) )
              done ;
              Readwrite.write_matrix_float_to_float64_au 100 !samples ( !picture_file ^ !date ^ "traces.au" ) ;
              bigomega := float ( Array.length !source.(0).(0) ) ;
              Draw.discrete_multicolor_segment_1_1 Draw.color_list !samples [| 0. ; !bigomega ; float !height ; 0. |] [| approx_hori_size ; approx_verti_size |] ;
              Graphics.set_window_title "Analogic.oscillo : raw display" ;
              save_name := ( !picture_file ^ !date ^ ".raw.ppm" ) ;
              Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 approx_hori_size approx_verti_size ) ) !save_name ;
              metadata := Array.append !metadata [| "number of traces: " ^ ( string_of_int !number_of_traces ) |] ;
              incr step ;
              go_on := Widget.rect_switch ( approx_hori_size - hori_shift ) ( 18 * verti_shift ) Graphics.green Graphics.black go_on_messages.(!step) ;
              Gc.compact ()
           end
          | 6 -> (** Marquage - Marking *)
           begin
            while mark.(0) >= 0. do
             ignore ( float_rgb_mark_image approx_hori_size approx_verti_size !picture !reduc mark ) ;
             marks := Array.append !marks [| [| mark.(0) ; mark.(1) ; mark.(2) ; mark.(3) |] |]
            done ;
            if Array.length !marks >= 4 then
             incr step
            else
             step := !step + 6 ;
            for i = 0 to 3 do
             mark.(i) <- 0.
            done ;
            f_init ()
           end
          | 7 -> (** Raffinement du marquage - Refining the marks *)
           begin
            for i = 0 to ( Array.length !marks ) - 2 do
             Widget.sand_glass approx_hori_size approx_verti_size ;
             ignore ( float_rgb_refine_mark approx_hori_size approx_verti_size !picture !marks.(i) ) ;
            done ;
            metadata := Array.append !metadata [| "marks: " |] ;
            let lm = Array.length !marks in
             let m_m = Array.sub !marks 0  ( pred lm ) in
              metadata := Array.append !metadata ( Array.map ( Array.fold_left accu_float "" ) m_m ) ;
              if lm >= 4 then
               begin
                let m = Matrix.float_transpose m_m in
(** Les coordonnées doivent être du même ordre de grandeur pour que la régression linéaire fonctionne.
The coordinates must have the same order of magnitude in order for the regression to work. *)
                 let departures = Matrix.matrix_float_scal_mult 1e-3 ( Array.sub m 0 2 ) in
                  let r = Matrix.float_rank departures in
                   if r = 2 then
                    begin
                     affine_isomorphism := linear_regression departures ( Array.sub m 2 2 ) ;
                     !affine_isomorphism.(0) <- Matrix.matrix_float_scal_mult 1e-3 !affine_isomorphism.(0) ;
                     incr step
                    end
                   else
                    step := !step + 5
               end
              else
               step := !step + 5 ;
              metadata := Array.append !metadata [| "affine isomorphism: " |] ;
              metadata := Array.append !metadata ( Array.map ( Array.fold_left accu_float "" ) !affine_isomorphism.(0) ) ;
              metadata := Array.append !metadata [| string_of_float !affine_isomorphism.(1).(0).(0) |] ;
              metadata := Array.append !metadata [| string_of_float !affine_isomorphism.(1).(0).(1) |] ;
              f_init ()
           end
          | 8 -> (** Unités horizontales - Horizontal units *)
           begin
            ignore ( choose_unit approx_hori_size approx_verti_size horizontal_units ) ;
            incr step ;
            f_init ()
           end
          | 9 -> (** Unités verticales - Vertical units *)
           begin
            ignore ( choose_unit approx_hori_size approx_verti_size vertical_units ) ;
            incr step ;
            f_init ()
           end
          | 10 -> (** Affichage des courbes en divisions - Curves display in divisions *)
           begin
            metadata := Array.append !metadata [| "horizontal units: " ^ ( String.concat " " ( Array.to_list horizontal_units ) ) |] ;
            metadata := Array.append !metadata [| "vertical units: " ^ ( String.concat " " ( Array.to_list vertical_units ) ) |] ;
            let decalage = float !crop_values.(0)
            and a = !affine_isomorphism.(0)
            and b = !affine_isomorphism.(1).(0) in
             interval := Matrix.float_closed_equal_subdivision decalage ( int_of_float !bigomega ) ( !bigomega +. decalage -. 1. ) ;
             curves := Array.make_matrix !number_of_traces 0 [| 0. |] ;
             for i = 0 to pred !number_of_traces do
              let input_curve = [| !interval ; !samples.(i) |] in
               !curves.(i) <- Matrix.matrix_float_column_apply_vect ( Matrix.vector_float_plus b ) ( Matrix.matrix_float_prod a input_curve ) ;
               Readwrite.matrix_float_write !curves.(i) ( !picture_file ^ !date ^ "curve" ^ ( string_of_int i ) ) ;
               Readwrite.write_matrix_float_to_float64_au ( 200 + i ) !curves.(i) ( !picture_file ^ !date ^ "curve" ^ (string_of_int i ) ^ ".au" ) ;
             done ;
             ll_corner := Matrix.vector_float_plus b ( Matrix.matrix_vector_float_prod a [| 0. ; float !height |] ) ;
             ur_corner := Matrix.vector_float_plus b ( Matrix.matrix_vector_float_prod a [| float !width ; 0. |]  ) ;
             !ll_corner.(0) <- min ( floor !ll_corner.(0) ) ( -5. ) ;
             !ll_corner.(1) <- min ( floor !ll_corner.(1) ) ( -4. ) ;
             !ur_corner.(0) <- max ( ceil !ur_corner.(0) ) 5. ;
             !ur_corner.(1) <- max ( ceil !ur_corner.(1) ) 4. ;
             Draw.discrete_trans_multicolor_segment_1_2 Draw.color_list !curves [| 0. ; 1. |] [| !ll_corner.(0) ; !ur_corner.(0) ; !ll_corner.(1) ; !ur_corner.(1) ; |] [| approx_hori_size ; approx_verti_size |] ;
             Graphics.set_window_title "Analogic.oscillo : curves DIV display" ;
             save_name := ( !picture_file ^ !date ^ ".div.ppm" ) ;
             Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 approx_hori_size approx_verti_size ) ) !save_name ;
             incr step ;
             go_on := Widget.rect_switch ( approx_hori_size - hori_shift ) ( 18 * verti_shift ) Graphics.green Graphics.black go_on_messages.(!step) ;
             Gc.compact () ;
           end
          | 11 -> (** Affichage des courbes en unités - Curves display in units *)
           begin
            let u = [| float_of_string horizontal_units.(0) ; float_of_string vertical_units.(0) |] in
             begin
              match horizontal_units.(3) with
              | "pix" -> ( u.(0) <- u.(0) *. ( float !width ) /. ( !ur_corner.(0) -. !ll_corner.(0) ) )
              | "scrn" -> ( u.(0) <- u.(0) /. ( !ur_corner.(0) -. !ll_corner.(0) ) )
              | _ -> () ;
             end ;
             begin
              match vertical_units.(3) with
              | "pix" -> ( u.(1) <- u.(1) *. ( float !height ) /. ( !ur_corner.(1) -. !ll_corner.(1) ) )
              | "scrn" -> ( u.(1) <- u.(1) /. ( !ur_corner.(1) -. !ll_corner.(1) ) )
              | _ -> () ;
             end ;
             curves := Array.map ( Array.mapi ( fun i x -> Matrix.vector_float_scal_mult u.(i) x ) ) !curves ;
             ll_corner := [| u.(0) *. !ll_corner.(0) ; u.(1) *. !ll_corner.(1) |] ;
             ur_corner := [| u.(0) *. !ur_corner.(0) ; u.(1) *. !ur_corner.(1) |] ;
             Draw.discrete_trans_multicolor_segment_1_2 Draw.color_list !curves [| 0. ; 1. |] [| !ll_corner.(0) ; !ur_corner.(0) ; !ll_corner.(1) ; !ur_corner.(1) ; |] [| approx_hori_size ; approx_verti_size |] ;
             Graphics.set_window_title "Analogic.oscillo : curves UNITS display" ;
             for i = 0 to pred !number_of_traces do
              Readwrite.matrix_float_write !curves.(i) ( !picture_file ^ !date ^ "magnitude" ^ ( string_of_int i ) ) ;
              Readwrite.write_matrix_float_to_float64_au ( 300 + i ) !curves.(i) ( !picture_file ^ !date ^ "magnitude" ^ (string_of_int i ) ^ ".au" ) ;
             done ;
             save_name := ( !picture_file ^ !date ^ ".units.ppm" ) ;
             Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 approx_hori_size approx_verti_size ) ) !save_name ;
             incr step ;
             go_on := Widget.rect_switch ( approx_hori_size - hori_shift ) ( 17 * verti_shift ) Graphics.green Graphics.black go_on_messages.(!step) ;
             Gc.compact () ;
           end
          | 12 -> (** Affichage brut des dérivées - Raw display of the derivatives *)
           begin
            (** Les lignes sont numérotées du haut vers le bas. - The lines are numbered from top to bottom. *)
            derivatives := Infinitesimal.discrete_trans_vector_speed Infinitesimal.mean_float_discrete_richardson_binary_diff (-1.) !samples ;
            for i = 0 to pred !number_of_traces do
             Readwrite.vector_float_write !derivatives.(i) ( !picture_file ^ !date ^ "deriv" ^ ( string_of_int i ) )
            done ;
            Readwrite.write_matrix_float_to_float64_au 400 !derivatives ( !picture_file ^ !date ^ "derivatives.au" ) ;
            Gc.compact () ;
            Draw.discrete_multicolor_segment_1_1 Draw.color_list !derivatives [| 0. ; !bigomega ; Matrix.matrix_float_min !derivatives ; Matrix.matrix_float_max !derivatives |] [| approx_hori_size ; approx_verti_size |] ;
            Graphics.set_window_title "Analogic.oscillo : raw derivatives display" ;
            save_name := ( !picture_file ^ !date ^ ".raw_deriv.ppm" ) ;
            Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 approx_hori_size approx_verti_size ) ) !save_name ;
            if Array.length !curves >= 5 then
             begin
              speeds := Array.map ( Infinitesimal.discrete_trans_vector_speed Infinitesimal.mean_float_discrete_richardson_binary_diff ( !ur_corner.(0) -. !ll_corner.(0) ) ) !curves ;
              for i = 0 to pred !number_of_traces do
               Readwrite.matrix_float_write !speeds.(i) ( !picture_file ^ !date ^ "speed" ^ ( string_of_int i ) ) ;
               Readwrite.write_matrix_float_to_float64_au ( 500 + i ) !speeds.(i) ( !picture_file ^ !date ^ "speed" ^ ( string_of_int i ) ^ ".au" ) ;
              done ;
             end ;
            incr step ;
            go_on := Widget.rect_switch ( approx_hori_size - hori_shift ) ( 17 * verti_shift ) Graphics.green Graphics.black go_on_messages.(!step) ;
            Gc.compact () ;
           end
          | 13 -> (** Affichage brut des transformées - Raw display of the transforms *)
           begin
            Widget.sand_glass approx_hori_size approx_verti_size ;
            let tf = fun omega z -> Infinitesimal.discrete_float_causal_fourier_transform z omega
            and value = ( !bigomega -. 1. ) /. 2. in
             interval := Matrix.float_closed_equal_subdivision ( -. value ) ( int_of_float !bigomega ) value ;
             transfos := Array.make ( Array.length !samples ) ( Array.make_matrix 0 0 0. ) ;
             modules := Array.make_matrix ( Array.length !samples ) 0 0. ;
             for i = 0 to pred !number_of_traces do
              let ft = function omega -> tf omega ( truncate ( float !height ) !samples.(i) ) in
              !transfos.(i) <- Array.map ft !interval ;
              !modules.(i) <- Array.map Matrix.vector_float_norm_2 !transfos.(i) ;
              Readwrite.matrix_float_write !transfos.(i) ( !picture_file ^ !date ^ "Fourier" ^ ( string_of_int i ) ) ;
              Readwrite.write_matrix_float_to_float64_au ( 600 + i ) ( Matrix.float_transpose !transfos.(i) ) ( !picture_file ^ !date ^ "Fourier" ^ ( string_of_int i ) ^ ".au" )
             done ;
             Readwrite.matrix_float_write !modules ( !picture_file ^ !date ^ "transfoModules" ) ;
             Readwrite.write_matrix_float_to_float64_au ( 700 ) ( Matrix.float_transpose !modules ) ( !picture_file ^ !date ^ "transfoModules.au" ) ;
             Draw.discrete_multicolor_segment_1_1 Draw.color_list !modules [| -. value ; value ; floor ( 1.1 *. ( Matrix.matrix_float_min !modules ) ) ; ceil ( 1.1 *. ( Matrix.matrix_float_max !modules ) ) |] [| approx_hori_size ; approx_verti_size |] ;
             Graphics.set_window_title "Analogic.oscillo : raw Fourier transforms display" ;
             save_name := ( !picture_file ^ !date ^ ".raw_Fourier.ppm" ) ;
             Readwrite.write_ppm_binary_color ( Graphics.dump_image ( Graphics.get_image 0 0 approx_hori_size approx_verti_size ) ) !save_name ;
             incr step ;
             go_on := Widget.rect_switch ( approx_hori_size - hori_shift ) ( 18 * verti_shift ) Graphics.green Graphics.black go_on_messages.(!step) ;
             Gc.compact () ;
           end
          | 14 -> (** Choix - Choice *)
           begin
            incr step ;
            f_init ()
           end
          | 15 -> (** Bouclage - Looping *)
           begin
            step := 2
           end
          | _ ->
           f_end ()
         end
       end (** f_mouse *)
      and f_except = function unknown -> ( Graphics.close_graph () ; raise Widget.End ) in
       Widget.skeleton f_init f_end f_key f_mouse f_except ;
       ()
  end
 with Widget.End -> Gc.compact () ;
 () ;;



(** {v oscillo monotrace_switch hori_size verti_size max_number v} This function acts only by side effect, writing data into files.

Cette fonction opère uniquement par effet de bord, en écrivant les données dans des fichiers. *)
let oscillo = oscillo_recorder true ;;



(** {v recorder monotrace_switch hori_size verti_size max_number v} This function acts only by side effect, writing data into files.

Cette fonction opère uniquement par effet de bord, en écrivant les données dans des fichiers. *)
let recorder = oscillo_recorder false ;;

















(** {C § § § } *)


Graphics.close_graph () ;;


end ;;


