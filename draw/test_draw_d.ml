let couleurs = [| Graphics.blue ; Graphics.green |] ;;


let ch0 = function x -> [| (-5e-2) *. x.(1) ; 5e-2 *. x.(0) |] ;;
Draw.field_draw_2_2 ch0 5 [| 0. ; 1. ; 0. ; 1. |] [| 600 ; 600 |] ;;
let ch1 = function x -> [| (-1e-1) *. x.(1) ; 1e-1 *. x.(0) |] ;;
Draw.field_draw_2_2 ch1 5 [| (-1.) ; 1. ; (-1.) ; 1. |] [| 600 ; 600 |] ;;
Draw.field_smooth_2_2 sqrt ch1 5 [| (-1.) ; 1. ; (-1.) ; 1. |] [| 600 ; 600 |] ;;


let a2 = Matrix.matrix_float_random 10 10 1. ;;
let a6 = Array.mapi ( fun j y -> let jj = float j in Array.mapi ( fun i x -> let ii = float i in [| 1e-3 *. ii *. jj ; -1e-1 *. x |] ) y ) a2 ;;
Draw.discrete_field_draw_2_2 a6 [| 0. ; 1. ; 0. ; 1. |] [| 700 ; 700 |] ;;
Draw.discrete_field_smooth_2_2 sqrt a6 [| 0. ; 1. ; 0. ; 1. |] [| 700 ; 700 |] ;;


let ch2 = function x -> Array.append ( ch1 x ) ( ch0 x ) ;;
Draw.frame_field_draw_2_2 ch2 5 [| (-1.) ; 1. ; (-1.) ; 1. |] [| 600 ; 600 |] ;;
Draw.frame_field_smooth_2_2 sqrt ch2 5 [| (-1.) ; 1. ; (-1.) ; 1. |] [| 600 ; 600 |] ;;

let a7 = Array.mapi ( fun j y -> let jj = float j in Array.mapi ( fun i x -> let ii = float i in [| 1e-3 *. ii *. jj ; 1e-1 *. x ; -1e-2 *. ii ; 1e-2 *. jj |] ) y ) a2 ;;
Draw.discrete_frame_field_draw_2_2 a7 [| 0. ; 1. ; 0. ; 1. |] [| 700 ; 700 |] ;;
Draw.discrete_frame_field_smooth_2_2 sqrt a7 [| 0. ; 1. ; 0. ; 1. |] [| 700 ; 700 |] ;;


let ch3 = function x -> [| (5e-2) *. x.(1) ; (5e-2) *. x.(2) ; (5e-2) *. x.(0) |] ;;
Draw.field_draw_3_3 [| 5e-1 ; 6e-1 ; 7e-1 |] Graphics.magenta Graphics.cyan ch3 2 [| (-1.) ; 1. ; (-1.) ; 1. ; (-1.) ; 1. |] [| 700 ; 700 |] ;;
Draw.field_smooth_3_3 sqrt [| 5e-1 ; 6e-1 ; 7e-1 |] Graphics.magenta Graphics.yellow ch3 2 [| (-1.) ; 1. ; (-1.) ; 1. ; (-1.) ; 1. |] [| 700 ; 700 |] ;;
Draw.field_draw_3_3 [| 11e-1 ; 6e-1 ; 2e-1 |] Graphics.magenta Graphics.yellow ch3 2 [| (-1.) ; 1. ; (-1.) ; 1. ; (-1.) ; 1. |] [| 700 ; 700 |] ;;
Draw.field_draw_3_3 [| 5e-1 ; 6e-1 ; 9e-1 |] Graphics.white Graphics.yellow ch3 2 [| (-1.) ; 1. ; (-1.) ; 1. ; (-1.) ; 1. |] [| 700 ; 700 |] ;;

let coordinates = [| -3. ; 3. ; -3. ; 3. ; -3. ; 10. |] ;;

let a8 = Array.map ( Array.map ( Matrix.vector_float_random 10 ) ) a2 ;;
let a9 = Array.mapi ( fun k z ->  let kk = float k in Array.mapi ( fun j y -> let jj = float j in Array.mapi ( fun i x -> let ii = float i in [| 0.03 *. jj ; 0.03 *. ii ; 0.01 *. ii *. jj |] ) y ) z ) a8 ;;
Draw.discrete_field_smooth_3_3 sqrt [| 0.4 ; 0.6 ; 0.8 |] Graphics.magenta Graphics.yellow a9 coordinates [| 700 ; 700 |] ;;

let ch4 = function x -> [| (7e-2) *. x.(0) ; (7e-2) *. x.(1) ; (7e-2) *. x.(2) |] ;;
let cr0 = function x -> Array.append ( ch3 x ) ( ch4 x ) ;;

Draw.frame_field_draw_3_3 [| 5e-1 ; 6e-1 ; 9e-1 |] [| Graphics.white ; Graphics.green |] [| Graphics.magenta ; Graphics.yellow |] cr0 2 [| (-1.) ; 1. ; (-1.) ; 1. ; (-1.) ; 1. |] [| 700 ; 700 |] ;;
Draw.frame_field_smooth_3_3 sqrt [| 5e-1 ; 6e-1 ; 9e-1 |] [| Graphics.white ; Graphics.green |] [| Graphics.yellow ; Graphics.cyan |] cr0 2 [| (-1.) ; 1. ; (-1.) ; 1. ; (-1.) ; 1. |] [| 700 ; 700 |] ;;

let a10 = Array.mapi ( fun k z ->  let kk = float k in Array.mapi ( fun j y -> let jj = float j in Array.mapi ( fun i x -> let ii = float i in [| 0.03 *. jj ; 0.03 *. ii ; 0.01 *. ii *. jj ; 0.03 *. kk ; 0.03 *. ii ; 0.03 *. jj |] ) y ) z ) a8 ;;
Draw.discrete_frame_field_draw_3_3 [| 0.4 ; 0.6 ; 0.8 |] [| Graphics.magenta ; Graphics.yellow |] couleurs a10 [| -3. ; 3. ; -3. ; 3. ; -3. ; 3. |] [| 700 ; 700 |] ;;
Draw.discrete_frame_field_smooth_3_3 sqrt [| 0.4 ; 0.6 ; 0.8 |] [| Graphics.magenta ; Graphics.yellow |] [| Graphics.cyan ; Graphics.white |] a10 [| -3. ; 3. ; -3. ; 3. ; -3. ; 3. |] [| 700 ; 700 |] ;;

