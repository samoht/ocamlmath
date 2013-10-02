(*
#load "graphics.cma";;

Graphics.open_graph " 1300x700" ;;
Graphics.open_graph "" ;;
Graphics.clear_graph () ;;
Graphics.close_graph () ;;
Graphics.set_color Graphics.black ;;
Graphics.fill_rect 0 0 500 500 ;;
Graphics.fill_rect 0 0 1300 700 ;;

Graphics.current_point () ;;
Graphics.moveto 250 250 ;;
Graphics.draw_string " ici +250 ..." ;;

Graphics.set_color Graphics.white ;;
for i = 200 to 300 do
 Graphics.plot 200 i ;
 Graphics.plot i 100 ;
done ;;

Draw.light_rectangle true 33 4 ;;
Draw.light_rectangle false 33 4 ;;
*)

let coordinates = [| -1e20 *. Infinitesimal.doublepi ; 1e20 *. Infinitesimal.doublepi ; -1e20 *. Infinitesimal.pi ; 1e20 *. Infinitesimal.pi |] ;;
let g = function x -> Infinitesimal.halfpi *. 1e20 *. sin ( x *. (-5e-21) ) ;;
Draw.function_draw_1_1 g coordinates [| 700 ; 500 |] ;;
Draw.function_segment_1_1 g coordinates [| 1000 ; 600 |] ;;

Draw.function_draw_1_1 g coordinates [| 1333 ; 666 |] ;;

Draw.function_grid_draw_1_1 g coordinates [| 1333 ; 666 |] ;;
Draw.function_grid_segment_1_1 g coordinates [| 1333 ; 666 |] ;;
Draw.function_grid_smooth_1_1 ( max 0.3 ) g coordinates [| 1333 ; 666 |] ;;
Draw.function_grid_oversample_1_1 9 sqrt g coordinates [| 666 ; 333 |] ;;

Draw.function_multi_segment_1_1 [| g ; cos |] coordinates [| 1333 ; 666 |] ;;
Draw.function_multi_draw_1_1 [| g ; cos |] coordinates [| 444 ; 444 |] ;;
Draw.function_grid_multi_segment_1_1 [| g ; cos |] coordinates [| 1333 ; 666 |] ;;
Draw.function_grid_multi_draw_1_1 [| g ; cos |] coordinates [| 444 ; 444 |] ;;
Draw.function_grid_multi_oversample_1_1 4 sqrt [| g ; cos |] coordinates [| 777 ; 777 |] ;;

let couleurs = [| Graphics.blue ; Graphics.green |] ;;

Draw.function_grid_multicolor_draw_1_1 couleurs [| g ; cos |] coordinates [| 444 ; 444 |] ;;
Draw.function_grid_multicolor_segment_1_1 couleurs [| g ; cos |] coordinates [| 444 ; 444 |] ;;
Draw.function_multicolor_draw_1_1 couleurs [| g ; cos |] coordinates [| 444 ; 444 |] ;;
Draw.function_multicolor_segment_1_1 couleurs [| g ; cos |] coordinates [| 444 ; 444 |] ;;
Draw.function_multicolor_oversample_1_1 5 sqrt couleurs [| g ; cos |] coordinates [| 777 ; 777 |] ;;

let coordinates = [| 0. ; Infinitesimal.doublepi ; -2. ; 2. |] ;;
Draw.function_draw_1_1 sin coordinates [| 700 ; 500 |] ;;
Draw.function_draw_1_1 sin coordinates [| 1000 ; 600 |] ;;
Draw.function_draw_1_1 sin coordinates [| 1111 ; 555 |] ;;

let a = Array.map ( function x -> cos ( Random.float x ) ) ( Matrix.float_closed_equal_subdivision 0. 30 Infinitesimal.doublepi ) ;;
Draw.discrete_draw_1_1 a coordinates [| 1000 ; 700 |] ;;
Draw.discrete_segment_1_1 a coordinates [| 1000 ; 700 |] ;;
Draw.discrete_smooth_1_1 Util.float_identity a coordinates [| 1000 ; 700 |] ;;
Draw.discrete_grid_draw_1_1 a coordinates [| 1000 ; 700 |] ;;
Draw.discrete_grid_segment_1_1 a coordinates [| 1000 ; 700 |] ;;
Draw.discrete_grid_smooth_1_1 ( Draw.power_gamma 0.7 ) a coordinates [| 1000 ; 700 |] ;;

let b = [| a ; Matrix.vector_float_coeff_prod a a |] ;;
Draw.discrete_multi_draw_1_1 b coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multi_draw_1_1 b coordinates [| 700 ; 500 |] ;;
Draw.discrete_multicolor_draw_1_1 couleurs b coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multicolor_draw_1_1 couleurs b coordinates [| 700 ; 500 |] ;;

Draw.discrete_multi_segment_1_1 b coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multi_segment_1_1 b coordinates [| 700 ; 500 |] ;;
Draw.discrete_multicolor_segment_1_1 couleurs b coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multicolor_segment_1_1 couleurs b coordinates [| 700 ; 500 |] ;;


Draw.function_grid_draw_1_2 ( Infinitesimal.cycloid 1. 1. ) [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_grid_segment_1_2 ( Infinitesimal.cycloid 1. 1. ) [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_grid_smooth_1_2 sqrt ( Infinitesimal.cycloid 1. 1. ) [| -10. ; 10. |] 100 coordinates [| 1000 ; 500 |] ;;
Draw.function_grid_oversample_1_2 3 sqrt ( Infinitesimal.cycloid 1. 1. ) [| -10. ; 10. |] 4000 coordinates [| 700 ; 500 |] ;;

Draw.function_draw_1_2 ( Infinitesimal.cycloid 1. 1. ) [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_segment_1_2 ( Infinitesimal.cycloid 1. 1. ) [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_smooth_1_2 sqrt ( Infinitesimal.cycloid 1. 1. ) [| -10. ; 10. |] 1000 coordinates [| 1000 ; 500 |] ;;
Draw.function_oversample_1_2 2 sqrt ( Infinitesimal.cycloid 1. 1. ) [| -10. ; 10. |] 2000 coordinates [| 500 ; 300 |] ;;

Draw.function_multi_draw_1_2 [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_multi_segment_1_2 [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_multi_smooth_1_2 sqrt [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_multi_oversample_1_2 3 sqrt [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 1000 coordinates [| 500 ; 300 |] ;;

Draw.function_grid_multi_draw_1_2 [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_grid_multi_segment_1_2 [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_grid_multi_smooth_1_2 sqrt [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_grid_multi_oversample_1_2 3 sqrt [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 1000 coordinates [| 500 ; 300 |] ;;

Draw.function_multicolor_draw_1_2 couleurs [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_multicolor_segment_1_2 couleurs [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_multicolor_smooth_1_2 sqrt couleurs [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_multicolor_oversample_1_2 3 sqrt couleurs [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 1000 coordinates [| 500 ; 300 |] ;;

Draw.function_grid_multicolor_draw_1_2 couleurs [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 700 ; 500 |] ;;
Draw.function_grid_multicolor_segment_1_2 couleurs [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 700 ; 500 |] ;;
Draw.function_grid_multicolor_smooth_1_2 sqrt couleurs [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 10000 coordinates [| 700 ; 500 |] ;;
Draw.function_grid_multicolor_oversample_1_2 3 sqrt couleurs [| Infinitesimal.cycloid 1. 1. ; Infinitesimal.unit_circle |] [| -10. ; 10. |] 1000 coordinates [| 700 ; 500 |] ;;

(*
let coordinates = [| -10. ; 10. ; -1. ; 3. |] ;;
let fc0 = function x -> ( x +. 10. ) *. 12.5 ;;
let fc1 = function x -> let y = fc0 x in Array.make 3 y ;;
let fc2 = function x -> let y = fc0 x in [| y ; 0. ; y |] ;;
Draw.function_color_draw_1_2 ( Infinitesimal.cycloid 1. 1. ) fc1 [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
Draw.function_color_draw_1_2 ( Infinitesimal.cycloid 1. 1. ) fc2 [| -10. ; 10. |] 10000 coordinates [| 1000 ; 500 |] ;;
*)

let coordinates = [| -2. ; 2. ; -2. ; 2. |] ;;
Draw.discrete_trans_draw_1_2 b [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
let bb = Matrix.float_transpose b ;;
Draw.discrete_draw_1_2 bb [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_trans_grid_draw_1_2 b [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_segment_1_2 bb [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_smooth_1_2 sqrt bb [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;

let c = [| Matrix.vector_float_coeff_prod b.(1) a ; b.(1)|] ;;
let cc = Matrix.float_transpose c ;;
let d = [| b ; c |] ;;
Draw.discrete_trans_grid_multi_draw_1_2 d [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multi_draw_1_2 [| bb ; cc |] [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multi_segment_1_2 [| bb ; cc |] [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;

Draw.discrete_trans_multi_draw_1_2 d [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_multi_draw_1_2 [| bb ; cc |] [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;

Draw.discrete_trans_grid_multicolor_draw_1_2 couleurs d [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_trans_multicolor_draw_1_2 couleurs d [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;

Draw.discrete_multicolor_draw_1_2 couleurs [| bb ; cc |] [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multicolor_draw_1_2 couleurs [| bb ; cc |] [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multicolor_segment_1_2 couleurs [| bb ; cc |] [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;
Draw.discrete_grid_multicolor_smooth_1_2 sqrt couleurs [| bb ; cc |] [| 0. ; 1. |] coordinates [| 700 ; 500 |] ;;

Draw.arrow ( Draw.draw_smooth_segment ( max 1. ) Graphics.yellow ) 100 150 250 200 ;;
Draw.arrow ( Draw.draw_smooth_segment ( max 1. ) Graphics.cyan ) 250 250 (-30) (-70) ;;

let coordinates = [| -5. ; 5. ; -2. ; 3. |] ;;
let f = function x -> Array.append ( Infinitesimal.unit_circle x ) ( Infinitesimal.vector_speed ( Infinitesimal.float_richardson_binary_deriv 2 1e-3 ) Infinitesimal.unit_circle x ) ;;
let ff = function x -> Array.append ( Infinitesimal.cycloid 1. 1. x ) ( Infinitesimal.vector_speed ( Infinitesimal.float_richardson_binary_deriv 2 1e-3 ) ( Infinitesimal.cycloid 1. 1. ) x ) ;;
Draw.field_grid_draw_1_2 f [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 30 coordinates [| 700 ; 700 |] ;;
Draw.field_grid_segment_1_2 f [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 30 coordinates [| 700 ; 700 |] ;;
Draw.field_grid_smooth_1_2 sqrt f [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 30 coordinates [| 700 ; 700 |] ;;
Draw.field_grid_oversample_1_2 3 sqrt f [| -. Infinitesimal.pi ; Infinitesimal.pi |] 1000 30 coordinates [| 700 ; 700 |] ;;

Draw.field_draw_1_2 f [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 30 coordinates [| 700 ; 700 |] ;;
Draw.field_grid_multicolor_draw_1_2 couleurs [| f ; ff |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 30 coordinates [| 1000 ; 500 |] ;;
Draw.field_grid_multi_draw_1_2 [| f ; ff |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 30 coordinates [| 1000 ; 500 |] ;;
Draw.field_multicolor_draw_1_2 couleurs [| f ; ff |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 30 coordinates [| 1000 ; 500 |] ;;
Draw.field_multi_draw_1_2 [| f ; ff |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 30 coordinates [| 1000 ; 500 |] ;;

Draw.field_multicolor_oversample_1_2 3 sqrt couleurs [| f ; ff |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 1000 30 coordinates [| 1000 ; 500 |] ;;

let b_b = Infinitesimal.discrete_trans_vector_speed Infinitesimal.mean_float_discrete_richardson_binary_diff 1. b ;;
let bbb = Array.append b b_b ;;
let b__b = Matrix.matrix_float_opp bbb ;;

let bbbb = Matrix.float_transpose bbb ;;
let bb_bb = Matrix.matrix_float_opp bbbb ;;

let coordinates = [| -3. ; 3. ; -3. ; 3. |] ;;
Draw.discrete_field_trans_draw_1_2 bbb [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_trans_grid_draw_1_2 bbb [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_draw_1_2 bbbb [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_grid_draw_1_2 bbbb [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_trans_grid_multi_draw_1_2 [| bbb ; b__b |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_grid_multi_draw_1_2 [| bbbb ; bb_bb |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_trans_grid_multicolor_draw_1_2 couleurs [| bbb ; b__b |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_grid_multicolor_draw_1_2 couleurs [| bbbb ; bb_bb |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_trans_multi_draw_1_2 [| bbb ; b__b |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_multi_draw_1_2 [| bbbb ; bb_bb |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_trans_multicolor_draw_1_2 couleurs [| bbb ; b__b |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_multicolor_draw_1_2 couleurs [| bbbb ; bb_bb |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_field_multicolor_segment_1_2 couleurs [| bbbb ; bb_bb |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;

let fff = function x -> Array.append ( f x ) ( ff x ) ;;
let gg = function x -> Matrix.vector_float_scal_add 0.5 ( fff x ) ;;
Draw.frame_field_draw_1_2 fff [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 10 coordinates [| 700 ; 700 |] ;;
Draw.frame_field_multi_draw_1_2 [| fff ; gg |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 10 coordinates [| 700 ; 700 |] ;;
Draw.frame_field_multicolor_draw_1_2 couleurs [| fff ; gg |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 10 coordinates [| 700 ; 700 |] ;;

Draw.frame_field_grid_draw_1_2 fff [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 10 coordinates [| 700 ; 700 |] ;;
Draw.frame_field_grid_multi_draw_1_2 [| fff ; gg |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 10 coordinates [| 700 ; 700 |] ;;
Draw.frame_field_grid_multicolor_draw_1_2 couleurs [| fff ; gg |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 10000 7 coordinates [| 700 ; 700 |] ;;
Draw.frame_field_grid_multicolor_segment_1_2 couleurs [| fff ; gg |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 100 7 coordinates [| 700 ; 700 |] ;;
Draw.frame_field_grid_multicolor_smooth_1_2 sqrt couleurs [| fff ; gg |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 100 7 coordinates [| 700 ; 700 |] ;;
Draw.frame_field_grid_multicolor_oversample_1_2 2 sqrt couleurs [| fff ; gg |] [| -. Infinitesimal.pi ; Infinitesimal.pi |] 1000 7 coordinates [| 700 ; 700 |] ;;

let b0 = Array.append bbb b__b ;;
let b1 = Array.append b__b bbb ;;
Draw.discrete_frame_field_trans_draw_1_2 b0 [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;

Draw.discrete_frame_field_trans_multi_draw_1_2 [| b0 ; b1 |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;
Draw.discrete_frame_field_trans_multicolor_draw_1_2 couleurs [| b0 ; b1 |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;

Draw.discrete_frame_field_trans_multicolor_segment_1_2 couleurs [| b0 ; b1 |] [| 0. ; 1. |] 20 coordinates [| 1000 ; 700 |] ;;

