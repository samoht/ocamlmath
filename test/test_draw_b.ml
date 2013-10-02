let couleurs = [| Graphics.blue ; Graphics.green |] ;;

Matrix.float_trans_ortho_proj [| [| 1. ; 0. ; 0. |] ; [| 0. ; 1. ; 0. |] |] [| 1. ; 2. ; 3. |] ;;

let coordinates = [| -3. ; 3. ; -3. ; 3. ; -3. ; 10. |] ;;
let f0 = Infinitesimal.helicoid 1. 1. ;;
let f1 = Infinitesimal.helicoid 2. (-0.5) ;;
let f2 = Infinitesimal.spherical_loxodromy 5. (-0.15) ;;

Draw.function_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] f0 [| 0. ; 30. |] 10000 coordinates [| 700 ; 500 |] ;;
Draw.function_draw_1_3 [| 0.2 ; 0.2 ; 1.2 |] f1 [| 0. ; 30. |] 10000 coordinates [| 400 ; 400 |] ;;
Draw.function_draw_1_3 [| 0.2 ; 0.2 ; 1.2 |] f2 [| 0. ; 30. |] 10000 coordinates [| 400 ; 400 |] ;;

Draw.function_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] f0 [| 0. ; 30. |] 10000 coordinates [| 700 ; 500 |] ;;
Draw.function_oversample_1_3 3 sqrt [| 0.3 ; 0.3 ; 1.5 |] f0 [| 0. ; 30. |] 1000 coordinates [| 700 ; 500 |] ;;

(*
Draw.function_partial_draw_1_3 Graphics.blue [| 0.2 ; 0.2 ; 1.2 |] f1 [| 0. ; 30. |] 10000 coordinates [| 400 ; 400 |] ;;
*)
Draw.function_multi_draw_1_3 [| 0.2 ; 0.2 ; 1.2 |] [| f0 ; f1 ; f2 |] [| 0. ; 30. |] 10000 coordinates [| 700 ; 500 |] ;;
Draw.function_multi_segment_1_3 [| 0.2 ; 0.2 ; 1.2 |] [| f0 ; f1 ; f2 |] [| 0. ; 30. |] 50 coordinates [| 700 ; 500 |] ;;
Draw.function_multi_smooth_1_3 sqrt [| 0.2 ; 0.2 ; 1.2 |] [| f0 ; f1 ; f2 |] [| 0. ; 30. |] 50 coordinates [| 700 ; 500 |] ;;
Draw.function_multi_oversample_1_3 3 sqrt [| 0.2 ; 0.2 ; 1.2 |] [| f0 ; f1 ; f2 |] [| 0. ; 30. |] 500 coordinates [| 700 ; 500 |] ;;

let col0 = Array.append couleurs [| Graphics.magenta |] ;;
Draw.function_multicolor_draw_1_3 col0 [| 1. ; 1. ; 2. |] [| f0 ; f1 ; f2 |] [| 0. ; 40. |] 10000 coordinates [| 700 ; 500 |] ;;
Draw.function_multicolor_segment_1_3 col0 [| 1. ; 1. ; 2. |] [| f0 ; f1 ; f2 |] [| 0. ; 40. |] 10000 coordinates [| 700 ; 500 |] ;;
Draw.function_multicolor_smooth_1_3 sqrt col0 [| 1. ; 1. ; 2. |] [| f0 ; f1 ; f2 |] [| 0. ; 40. |] 1000 coordinates [| 700 ; 500 |] ;;
Draw.function_multicolor_oversample_1_3 3 sqrt col0 [| 1. ; 1. ; 2. |] [| f0 ; f1 ; f2 |] [| 0. ; 40. |] 1000 coordinates [| 700 ; 500 |] ;;

let c0 = Array.map f0 ( Matrix.float_closed_equal_subdivision 0. 300 30. ) ;;
let c1 = Matrix.float_transpose c0 ;;
let c2 = Array.map ( Matrix.vector_float_opp ) c0 ;;
let c3 = Matrix.float_transpose c2 ;;
Draw.discrete_trans_draw_1_3 [| 1. ; 1. ; 2. |] c1 [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_draw_1_3 [| 1. ; 1. ; 2. |] c0 [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_segment_1_3 [| 1. ; 1. ; 2. |] c0 [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_smooth_1_3 sqrt [| 1. ; 1. ; 2. |] c0 [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;

Draw.discrete_trans_multi_draw_1_3 [| 1. ; 1. ; 2. |] [| c1 ; c3 |] [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_multi_draw_1_3 [| 1. ; 1. ; 2. |] [| c0 ; c2 |] [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_multi_segment_1_3 [| 1. ; 1. ; 2. |] [| c0 ; c2 |] [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_multi_smooth_1_3 sqrt [| 1. ; 1. ; 2. |] [| c0 ; c2 |] [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;

Draw.discrete_trans_multicolor_draw_1_3 couleurs [| 1. ; 1. ; 2. |] [| c1 ; c3 |] [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_multicolor_draw_1_3 couleurs [| 1. ; 1. ; 2. |] [| c0 ; c2 |] [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_multicolor_segment_1_3 couleurs [| 1. ; 1. ; 2. |] [| c0 ; c2 |] [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;
Draw.discrete_multicolor_smooth_1_3 sqrt couleurs [| 1. ; 1. ; 2. |] [| c0 ; c2 |] [| 0. ; 30. |] coordinates [| 400 ; 400 |] ;;

let f3 = function x -> Array.append ( f0 x ) ( f1 x ) ;;
let f4 = function x -> Array.append ( f1 x ) ( f2 x ) ;;
let f4a = function x -> Matrix.vector_float_scal_mult (0.5) ( Infinitesimal.serret_frenet_3 ( Infinitesimal.float_richardson_binary_deriv 2 1e-3 ) f0 x ).(2) ;;
let f4b = function x -> Array.append ( f0 x ) ( f4a x ) ;;
Draw.field_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] f4b [| 0. ; 30. |] 1000 50 coordinates [| 700 ; 500 |] ;;
Draw.field_segment_1_3 [| 0.3 ; 0.3 ; 1.5 |] f3 [| 0. ; 30. |] 1000 30 coordinates [| 700 ; 500 |] ;;
Draw.field_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] f3 [| 0. ; 30. |] 1000 25 coordinates [| 700 ; 500 |] ;;
Draw.field_oversample_1_3 4 sqrt [| 0.3 ; 0.3 ; 1.5 |] f3 [| 0. ; 30. |] 1000 25 coordinates [| 700 ; 500 |] ;;

(*
Draw.field_partial_draw_1_3 couleurs.(0) couleurs.(1) [| 0.3 ; 0.3 ; 1.5 |] f4 [| 0. ; 30. |] 1000 30 coordinates [| 700 ; 500 |] ;;
*)
Draw.field_multi_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| f3 ; f4 |] [| 0. ; 30. |] 10000 30 coordinates [| 700 ; 500 |] ;;
Draw.field_multi_segment_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| f3 ; f4 |] [| 0. ; 30. |] 10000 30 coordinates [| 700 ; 500 |] ;;
Draw.field_multi_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] [| f3 ; f4 |] [| 0. ; 30. |] 10000 30 coordinates [| 700 ; 500 |] ;;
Draw.field_multi_oversample_1_3 3 sqrt [| 0.3 ; 0.3 ; 1.5 |] [| f3 ; f4 |] [| 0. ; 30. |] 800 30 coordinates [| 700 ; 500 |] ;;

Draw.field_multicolor_draw_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| f3 ; f4 |] [| 0. ; 30. |] 10000 30 coordinates [| 700 ; 500 |] ;;
Draw.field_multicolor_segment_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| f3 ; f4 |] [| 0. ; 30. |] 100 30 coordinates [| 700 ; 500 |] ;;
Draw.field_multicolor_smooth_1_3 sqrt couleurs [| 0.3 ; 0.3 ; 1.5 |] [| f3 ; f4 |] [| 0. ; 30. |] 300 30 coordinates [| 700 ; 500 |] ;;
Draw.field_multicolor_oversample_1_3 2 sqrt couleurs [| 0.3 ; 0.3 ; 1.5 |] [| f3 ; f4 |] [| 0. ; 30. |] 1000 30 coordinates [| 700 ; 500 |] ;;

let c4 = Array.map f3 ( Matrix.float_closed_equal_subdivision 0. 100 30. ) ;;
let c5 = Matrix.float_transpose c4 ;;
let c6 = Matrix.matrix_float_opp c5 ;;
let c7 = Matrix.float_transpose c6 ;;

Draw.discrete_field_trans_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] c5 [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] c4 [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_segment_1_3 [| 0.3 ; 0.3 ; 1.5 |] c4 [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] c4 [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;

Draw.discrete_field_trans_multi_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| c5 ; c6 |] [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_multi_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| c4 ; c7 |] [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_multi_segment_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| c4 ; c7 |] [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_multi_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] [| c4 ; c7 |] [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;

Draw.discrete_field_trans_multicolor_draw_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| c5 ; c6 |] [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_multicolor_draw_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| c4 ; c7 |] [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_multicolor_segment_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| c4 ; c7 |] [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;
Draw.discrete_field_multicolor_smooth_1_3 sqrt couleurs [| 0.3 ; 0.3 ; 1.5 |] [| c4 ; c7 |] [| 0. ; 30. |] 30 coordinates [| 700 ; 500 |] ;;

let f5 = function x -> Array.append ( f3 x ) ( f2 x ) ;;
let f6 = function x -> Array.append ( f2 x ) ( f3 x ) ;;
Draw.frame_field_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] f5 [| 0. ; 30. |] 10000 30 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_segment_1_3 [| 0.3 ; 0.3 ; 1.5 |] f5 [| 0. ; 30. |] 1000 30 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] f5 [| 0. ; 30. |] 1000 30 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_oversample_1_3 2 sqrt [| 0.3 ; 0.3 ; 1.5 |] f5 [| 0. ; 30. |] 300 30 coordinates [| 700 ; 500 |] ;;

(*
Draw.frame_field_partial_draw_1_3 couleurs.(0) [| 0.3 ; 0.3 ; 1.5 |] f5 [| 0. ; 30. |] 10000 30 coordinates [| 700 ; 500 |] ;;
*)
Draw.frame_field_multi_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| f5 ; f6 |] [| 0. ; 30. |] 10000 10 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_multi_segment_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| f5 ; f6 |] [| 0. ; 30. |] 1000 10 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_multi_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] [| f5 ; f6 |] [| 0. ; 30. |] 1000 10 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_multi_oversample_1_3 3 sqrt [| 0.3 ; 0.3 ; 1.5 |] [| f5 ; f6 |] [| 0. ; 30. |] 500 10 coordinates [| 700 ; 500 |] ;;

Draw.frame_field_multicolor_draw_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| f5 ; f6 |] [| 0. ; 30. |] 10000 11 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_multicolor_segment_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| f5 ; f6 |] [| 0. ; 30. |] 1000 11 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_multicolor_smooth_1_3 sqrt couleurs [| 0.3 ; 0.3 ; 1.5 |] [| f5 ; f6 |] [| 0. ; 30. |] 1000 11 coordinates [| 700 ; 500 |] ;;
Draw.frame_field_multicolor_oversample_1_3 2 sqrt couleurs [| 0.3 ; 0.3 ; 1.5 |] [| f5 ; f6 |] [| 0. ; 30. |] 300 11 coordinates [| 700 ; 500 |] ;;

let c8 = Array.append c5 c6 ;;
let c9 = Matrix.float_transpose c8 ;;

Draw.discrete_frame_field_trans_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] c8 [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] c9 [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_segment_1_3 [| 0.3 ; 0.3 ; 1.5 |] c9 [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] c9 [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;

Draw.discrete_frame_field_trans_multi_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| c8 ; Matrix.matrix_float_opp c8 |] [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_multi_draw_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| c9 ; Matrix.matrix_float_opp c9 |] [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_multi_segment_1_3 [| 0.3 ; 0.3 ; 1.5 |] [| c9 ; Matrix.matrix_float_opp c9 |] [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_multi_smooth_1_3 sqrt [| 0.3 ; 0.3 ; 1.5 |] [| c9 ; Matrix.matrix_float_opp c9 |] [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;

Draw.discrete_frame_field_trans_multicolor_draw_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| c8 ; Matrix.matrix_float_opp c8 |] [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_multicolor_draw_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| c9 ; Matrix.matrix_float_opp c9 |] [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_multicolor_segment_1_3 couleurs [| 0.3 ; 0.3 ; 1.5 |] [| c9 ; Matrix.matrix_float_opp c9 |] [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;
Draw.discrete_frame_field_multicolor_smooth_1_3 sqrt couleurs [| 0.3 ; 0.3 ; 1.5 |] [| c9 ; Matrix.matrix_float_opp c9 |] [| 0. ; 30. |] 17 coordinates [| 700 ; 500 |] ;;


