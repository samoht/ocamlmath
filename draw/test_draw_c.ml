let couleurs = [| Graphics.blue ; Graphics.green |] ;;

let coord = [| -1. ; 1. ; -1. ; 1. ; -1. ; 1. |] ;;
let s0 = Infinitesimal.sinc ;;
let p2 = Infinitesimal.doublepi ;;
let pi = Infinitesimal.pi ;;
let surf0 = Infinitesimal.sph_coord_unit_sphere ;;
Draw.function_draw_2_3 [| 0.3 ; 0.3 ; 0.5 |] surf0 [| 0. ; 0. |] [| p2 ; p2 |] 1000 10 coord [| 700 ; 500 |] ;;
let surf1 = Infinitesimal.pseudo_sphere ;;
Draw.function_draw_2_3 [| 0.3 ; 0.3 ; 1. |] surf1 [| 0. ; 0. |] [| p2 ; 6. |] 1000 15 coord [| 700 ; 500 |] ;;
let surf2 = function x -> [| x.(0) ; x.(1) ; 2.5 *. sin x.(0) |] ;;
Draw.function_draw_2_3 [| 3. ; 4. ; 8. |] surf2 [| 0. ; 0. |] [| 3. *. p2 ; 9. |] 100 10 coord [| 700 ; 500 |] ;;
Draw.function_draw_2_3 [| -0.3 ; -0.5 ; 1. |] surf1 [| 0. ; 0. |] [| Infinitesimal.pi ; 6. |] 1000 10 coord [| 700 ; 500 |] ;;
Draw.function_segment_2_3 [| -0.3 ; -0.5 ; 1. |] surf1 [| 0. ; 0. |] [| Infinitesimal.pi ; 6. |] 100 10 coord [| 700 ; 500 |] ;;
Draw.function_smooth_2_3 sqrt [| -0.3 ; -0.5 ; 1. |] surf1 [| 0. ; 0. |] [| Infinitesimal.pi ; 6. |] 100 10 coord [| 700 ; 500 |] ;;
Draw.function_oversample_2_3 3 sqrt [| -0.3 ; -0.5 ; 1. |] surf1 [| 0. ; 0. |] [| Infinitesimal.pi ; 6. |] 100 10 coord [| 700 ; 500 |] ;;

let surf3 = function x -> [| x.(0) ; x.(1) ; 30. *. s0 x.(0) *. s0 x.(1) |] ;;
Draw.function_draw_2_3 [| 8. ; 12. ; 12. |] surf3 [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;
Draw.function_reverse_draw_2_3 [| 8. ; 12. ; 12. |] surf3 [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;
Draw.function_reverse_segment_2_3 [| 8. ; 12. ; 12. |] surf3 [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;
Draw.function_reverse_smooth_2_3 sqrt [| 8. ; 12. ; 12. |] surf3 [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;
Draw.function_reverse_oversample_2_3 9 sqrt [| 8. ; 12. ; 12. |] surf3 [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;

let cote_abs = fun f scale x -> let z = abs_float ( scale *. ( f x ).(2) ) in [| 255. -. z ; z ; 255. -. z |] ;;
let cote0 = fun f scale x -> let z = scale *. ( f x ).(2) in [| 2. *. abs_float z ; z +. 127. ; 2. *. abs_float z |] ;;
let cote1 = fun f scale x -> let z = scale *. ( f x ).(2) in [| 2. *. abs_float z ; z +. 127. ; 127. -. z |] ;;
let cote2 = fun f scale x -> let z = scale *. ( f x ).(2) in [| z +. 127. ; 127. ; 127. -. z |] ;;

Draw.function_color_draw_2_3 [| 0.3 ; 0.3 ; 0.5 |] surf0 ( cote_abs surf0 255. ) [| 0. ; 0. |] [| p2 ; p2 |] 100 10 coord [| 700 ; 500 |] ;;

Draw.function_color_draw_2_3 [| 8. ; 12. ; 12. |] surf3 ( cote_abs surf3 255. ) [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;
Draw.function_color_draw_2_3 [| 8. ; 12. ; 12. |] surf3 ( cote0 surf3 400. ) [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;
Draw.function_color_draw_2_3 [| 8. ; 12. ; 12. |] surf3 ( cote1 surf3 600. ) [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;
Draw.function_reverse_color_draw_2_3 [| 8. ; 12. ; 12. |] surf3 ( cote1 surf3 600. ) [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;
Draw.function_reverse_color_segment_2_3 [| 8. ; 12. ; 12. |] surf3 ( cote1 surf3 600. ) [| -. 3. *. p2 ; -. 3. *. p2 |] [| 3. *. p2 ; 3. *. p2 |] 100 33 coord [| 1000 ; 700 |] ;;

let g0 = function x -> 10. *. ( s0 x.(0) ) *. ( s0 x.(1) ) ;;
Draw.graph_draw_2_3 [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 1000 20 coord [| 700 ; 500 |] ;;
Draw.graph_segment_2_3 [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 100 20 coord [| 700 ; 500 |] ;;
Draw.graph_smooth_2_3 sqrt [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 100 20 coord [| 700 ; 500 |] ;;
Draw.graph_oversample_2_3 3 sqrt [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 300 20 coord [| 700 ; 500 |] ;;

Draw.graph_draw_2_3 [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 1000 20 coord [| 700 ; 500 |] ;;
Draw.graph_segment_2_3 [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 100 20 coord [| 700 ; 500 |] ;;
Draw.graph_smooth_2_3 sqrt [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 100 20 coord [| 700 ; 500 |] ;;
Draw.graph_oversample_2_3 3 sqrt [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 300 20 coord [| 700 ; 500 |] ;;

Draw.graph_reverse_draw_2_3 [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 1000 20 coord [| 700 ; 500 |] ;;
Draw.graph_reverse_segment_2_3 [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 100 20 coord [| 700 ; 500 |] ;;
Draw.graph_reverse_smooth_2_3 sqrt [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 100 20 coord [| 700 ; 500 |] ;;
Draw.graph_reverse_oversample_2_3 3 sqrt [| 5. ; 5. ; 5. |] g0 [| -10. ; -10. |] [| 10. ; 10. |] 300 20 coord [| 700 ; 500 |] ;;

let gc0 = function x -> let z = g0 x in [| 60. *. ( ( 1. -. ( abs_float z ) /. 10. ) ** 0.82 ) ; 70. *. ( abs_float z ) ** 0.3 ; 25.5 *. abs_float z |] ;;
Draw.graph_color_draw_2_3 [| 5. ; 5. ; 5. |] g0 gc0 [| -10. ; -10. |] [| 10. ; 10. |] 100 100 coord [| 700 ; 500 |] ;;
Draw.graph_color_segment_2_3 [| 5. ; 5. ; 5. |] g0 gc0 [| -10. ; -10. |] [| 10. ; 10. |] 100 100 coord [| 700 ; 500 |] ;;

Draw.graph_reverse_color_draw_2_3 [| 5. ; 5. ; 5. |] g0 gc0 [| -10. ; -10. |] [| 10. ; 10. |] 100 100 coord [| 700 ; 500 |] ;;
Draw.graph_reverse_color_segment_2_3 [| 5. ; 5. ; 5. |] g0 gc0 [| -10. ; -10. |] [| 10. ; 10. |] 100 100 coord [| 700 ; 500 |] ;;

let a0 = Array.mapi ( fun i x -> Matrix.float_closed_equal_subdivision ( float i ) 20 ( float ( - i ) ) ) ( Array.make 20 0. ) ;;
Draw.discrete_graph_draw_2_3 [| 5. ; 5. ; 5. |] a0 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;
Draw.discrete_graph_segment_2_3 [| 5. ; 5. ; 5. |] a0 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;
Draw.discrete_graph_smooth_2_3 sqrt [| 5. ; 5. ; 5. |] a0 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;

Draw.discrete_graph_color_draw_2_3 [| 5. ; 5. ; 5. |] a0 gc0 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;
Draw.discrete_graph_color_segment_2_3 [| 5. ; 5. ; 5. |] a0 gc0 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;

let a1 = Array.map ( Array.mapi ( fun i x -> [| float i ; Random.float x ; x /. 2. |] ) )  a0 ;;
let f_f = Infinitesimal.interpol_2 ( Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol Infinitesimal.mean_float_discrete_richardson_binary_diff Infinitesimal.float_decay_16 ) ) a1 ;;
Draw.discrete_draw_2_3 [| 5. ; 5. ; 5. |] a1 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;
Draw.discrete_segment_2_3 [| 5. ; 5. ; 5. |] a1 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;
Draw.discrete_smooth_2_3 sqrt [| 5. ; 5. ; 5. |] a1 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;

Draw.discrete_color_draw_2_3 [| 5. ; 5. ; 5. |] a1 gc0 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;
Draw.discrete_color_segment_2_3 [| 5. ; 5. ; 5. |] a1 gc0 [| -10. ; -10. |] [| 10. ; 10. |] coord [| 700 ; 500 |] ;;

let deriv0 = Infinitesimal.compensated_float_richardson_deriv ( Matrix.float_brezinski 2 1 ) 1.5 4 4e-2 ;;
let deriv1 = Infinitesimal.float_approx_deriv (1e-4) ;;

let fiber1 = function x -> Matrix.vector_float_scal_mult (-0.2) ( Infinitesimal.surface_normal_3 deriv0 surf1 x ) ;;
let field1 = function x -> Array.append ( surf1 x ) ( fiber1 x ) ;;
Draw.function_draw_2_3 [| 0.3 ; 0.3 ; 0.3 |] fiber1 [| 0. ; 0. |] [| p2 ; 6. |] 200 10 coord [| 700 ; 500 |] ;;

(* Le résultat de la normale sur la pseudosphère n'est pas convaincant. *)
Draw.field_draw_2_3 [| -0.05 ; -0.1 ; -0.6 |] field1 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 200 6 coord [| 700 ; 500 |] ;;
let fiber11 = function x -> Matrix.vector_float_scal_mult (-0.2) ( Infinitesimal.surface_normal_3 deriv1 surf1 x ) ;;
let field11 = function x -> Array.append ( surf1 x ) ( fiber11 x ) ;;
Draw.field_draw_2_3 [| -0.05 ; -0.1 ; -0.6 |] field11 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 200 6 coord [| 700 ; 500 |] ;;
Draw.field_segment_2_3 [| -0.05 ; -0.1 ; -0.6 |] field11 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 100 6 coord [| 700 ; 500 |] ;;
Draw.field_smooth_2_3 sqrt [| -0.05 ; -0.1 ; -0.6 |] field11 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 100 6 coord [| 700 ; 500 |] ;;
Draw.field_oversample_2_3 3 sqrt [| -0.05 ; -0.1 ; -0.6 |] field11 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 100 6 coord [| 700 ; 500 |] ;;

Draw.field_reverse_draw_2_3 [| -0.05 ; -0.1 ; -0.6 |] field11 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 200 6 coord [| 700 ; 500 |] ;;
Draw.field_reverse_segment_2_3 [| -0.05 ; -0.1 ; -0.6 |] field11 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 100 6 coord [| 700 ; 500 |] ;;
Draw.field_reverse_smooth_2_3 sqrt [| -0.05 ; -0.1 ; -0.6 |] field11 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 100 6 coord [| 700 ; 500 |] ;;
Draw.field_reverse_oversample_2_3 3 sqrt [| -0.05 ; -0.1 ; -0.6 |] field11 [| 0. ; 0. |] [| Infinitesimal.pi ; 6.1 |] 100 6 coord [| 700 ; 500 |] ;;

let fiber0 = function x -> Matrix.vector_float_scal_mult (1.) ( Infinitesimal.surface_normal_3 ( Infinitesimal.float_richardson_binary_deriv 2 1e-3 ) surf0 x ) ;;
let field0 = function x -> Array.append ( surf0 x ) ( fiber0 x ) ;;
Draw.field_draw_2_3 [| 0.4 ; 0.4 ; 0.4 |] field0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 1000 10 coord [| 700 ; 500 |] ;;

Draw.field_draw_2_3 [| 0.4 ; 0.4 ; 0.4 |] field0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 50 10 coord [| 700 ; 500 |] ;;

let a2 = Matrix.matrix_float_random 10 10 1. ;;
(*
let a3 = Array.map ( Array.mapi ( fun i x -> let ii = float i in [| ii +. x ; x *. ii ; ii ; 0.1 *. Random.float x ; 0.1 *. Random.float ii ; 0.1 *. Random.float x |] ) ) a2 ;;
Draw.discrete_field_draw_2_3 [| 0.4 ; 0.4 ; 0.4 |] a3 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] coord [| 700 ; 500 |] ;;
*)
let a4 = Array.mapi ( fun j y -> let jj = float j in Array.mapi ( fun i x -> let ii = float i in [| 0.1 *. jj ; 0.1 *. ii ; 0.01 *. ii *. jj ; 0.1 *. x ; 0.1 *. Random.float x ; 0.1 *. x *. x |] ) y ) a2 ;;
Draw.discrete_field_draw_2_3 [| -0.4 ; -0.4 ; 0.8 |] a4 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] coord [| 700 ; 500 |] ;;
Draw.discrete_field_segment_2_3 [| -0.4 ; -0.4 ; 0.8 |] a4 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] coord [| 700 ; 500 |] ;;
Draw.discrete_field_smooth_2_3 sqrt [| -0.4 ; -0.4 ; 0.8 |] a4 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] coord [| 700 ; 500 |] ;;
Draw.discrete_field_reverse_draw_2_3 [| -0.4 ; -0.4 ; 0.8 |] a4 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] coord [| 700 ; 500 |] ;;
Draw.discrete_field_reverse_segment_2_3 [| -0.4 ; -0.4 ; 0.8 |] a4 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] coord [| 700 ; 500 |] ;;
Draw.discrete_field_reverse_smooth_2_3 sqrt [| -0.4 ; -0.4 ; 0.8 |] a4 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] coord [| 700 ; 500 |] ;;

let a5 = Array.mapi ( fun j y -> let jj = float j in Array.mapi ( fun i x -> let ii = float i in [| 0.1 *. jj ; 0.1 *. ii ; 0.01 *. ii *. jj ; 0.1 *. x ; 0.1 *. Random.float x ; 0.1 *. x *. x ; 0.01 *. y.(0) ; 0.1 *. x ; 0.01 *. jj |] ) y ) a2 ;;
Draw.discrete_frame_field_draw_2_3 [| -0.4 ; -0.4 ; 0.8 |] a5 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] coord [| 700 ; 500 |] ;;

let ff0 = function x -> Array.append ( field0 x ) ( fiber1 x ) ;; 
Draw.frame_field_draw_2_3 [| 0.4 ; 0.4 ; 0.4 |] ff0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 100 4 coord [| 700 ; 500 |] ;;
Draw.frame_field_segment_2_3 [| 0.4 ; 0.4 ; 0.4 |] ff0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 100 4 coord [| 700 ; 500 |] ;;
Draw.frame_field_smooth_2_3 sqrt [| 0.4 ; 0.4 ; 0.4 |] ff0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 100 4 coord [| 700 ; 500 |] ;;
Draw.frame_field_oversample_2_3 2 sqrt [| 0.4 ; 0.4 ; 0.4 |] ff0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 100 4 coord [| 700 ; 500 |] ;;

Draw.frame_field_reverse_draw_2_3 [| 0.4 ; 0.4 ; 0.4 |] ff0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 100 4 coord [| 700 ; 500 |] ;;
Draw.frame_field_reverse_segment_2_3 [| 0.4 ; 0.4 ; 0.4 |] ff0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 100 4 coord [| 700 ; 500 |] ;;
Draw.frame_field_reverse_smooth_2_3 sqrt [| 0.4 ; 0.4 ; 0.4 |] ff0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 100 4 coord [| 700 ; 500 |] ;;
Draw.frame_field_reverse_oversample_2_3 2 sqrt [| 0.4 ; 0.4 ; 0.4 |] ff0 [| -. pi /. 2. ; -. pi |] [| 0. ; 0. |] 100 4 coord [| 700 ; 500 |] ;;

let ff1 = function x -> Array.append ( field1 x ) ( fiber0 x ) ;;
Draw.frame_field_draw_2_3 [| 1.4 ; 1.4 ; 1.4 |] ff1 [| -. pi ; -. pi |] [| -. pi /. 2. ; pi |] 100 10 coord [| 700 ; 500 |] ;;
Draw.frame_field_segment_2_3 [| -0.6 ; 0.6 ; 0.6 |] field1 [| -. pi ; -. pi |] [| 0. ; pi |] 100 7 coord [| 700 ; 700 |] ;;
Draw.frame_field_smooth_2_3 sqrt [| 0.25 ; 0.12 ; 0.5 |] field1 [| -. pi ; -. pi |] [| -. pi /. 2. ; pi |] 100 5 coord [| 700 ; 500 |] ;;
Draw.frame_field_oversample_2_3 3 sqrt [| 0.25 ; 0.12 ; 0.5 |] field1 [| -. pi ; -. pi |] [| -. pi /. 2. ; pi |] 1000 5 coord [| 700 ; 500 |] ;;


