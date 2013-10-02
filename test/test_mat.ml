
module H = Mat.Rng (Data.Zcoeff) ;;

let hj = H.Full_vector [| 0 ; 1 ; 0 ; 1 ; 0 ; 1 ; 0 ; 1 ; 0 ; 1 ; |] ;;

let hj0 = H.vector_embed 30 10 hj ;;
H.vector_dimension hj0 ;;

let v = [| hj ; H.vector_scal_mult 2 hj ; H.vector_scal_mult 4 hj |] ;;
let v0 = H.vector_of_blocks v ;;
let v1 = Array.map H.vector_to_sparse v ;;
let v2 = H.vector_of_blocks v1 ;;
H.vector_dimension hj ;;
H.vector_dimension v0 ;;
H.vector_to_full v2 ;;
H.vector_eq v0 v2 ;;
let v3 = Array.append v v1 ;;
let v4 = H.vector_of_blocks v3 ;;
let v5 = Array.map H.vector_to_string v3 ;;
let v6 = Array.map H.vector_of_string v5 ;;
Util.array_map2 H.vector_eq v3 v6 ;;


let hm = H.Full_matrix ( Array.make 3 ( H.vector_full_demakeup hj ) ) ;;
let hm0 = H.matrix_to_string hm ;;
let hm0a = H.matrix_of_string hm0 ;;

let hm1 = H.matrix_auto_to_sparse 0.3 hm ;;
H.matrix_print hm ;;
H.matrix_print hm1 ;;
let hm2 = H.matrix_to_full hm1 ;;

H.matrix_eq_zero hm ;;
H.matrix_eq hm hm ;;
H.matrix_eq hm hm1 ;;
H.matrix_eq hm hm2 ;;
H.matrix_eq hm1 hm ;;
H.matrix_eq hm1 hm1 ;;
H.matrix_eq hm1 hm2 ;;
H.matrix_eq hm2 hm ;;
H.matrix_eq hm2 hm1 ;;
H.matrix_eq hm2 hm2 ;;

H.matrix_row_sum hm ;;
H.matrix_row_sum hm1 ;;

H.matrix_embed [| 5 ; 20 |] [| 1 ; 2 |] hm ;;

let m = Array.make_matrix 3 2 hm ;;
let m0 = H.matrix_of_blocks m ;;
let m1 = H.matrix_sparse_of_blocks ( H.matrix_auto_to_sparse 0.7 ) m ;;
let m2 = H.sparse_diag_matrix_of_blocks ( H.matrix_auto_to_sparse 0.7 ) ( Array.make 3 hm ) ;;
let m2a = H.matrix_to_full m2 ;;


(*******************************************************************************)

module G = Mat.Field (Data.Rcoeff) ;;

G.N.coeff_two ;;

let m2 = G.Full_matrix ( Array.map ( Array.map Random.float ) ( Array.make_matrix 3 3 1. ) ) ;;
let r0m2 = G.matrix_row_extract 0 m2
let m2a = G.pivot_downward m2 m2 ;;
let m2b = G.invertibility m2 ;;
let m2c = G.matrix_copy m2 ;;
G.matrix_row_replace ( G.vector_null 3 ) 2 m2c ;;
let m2c0 = G.invertibility m2c ;;
let m2d = G.det m2 ;;
(*
#use "matrix.ml" ;;
Matrix.float_det ( G.matrix_full_demakeup m2 ) ;;
*)
G.det ( G.Full_matrix ( Array.make_matrix 3 3 1. ) ) ;;
G.invertibility ( G.Full_matrix ( Array.make_matrix 3 3 1. ) ) ;;

let id3 = G.Full_matrix [| [| 1. ; 0. ; 0. |] ; [| 0. ; 1. ; 0. |] ; [| 0. ; 0. ; 1. |] |] ;;

let m2e = G.pivot_downward m2 id3 ;;
let m2f = G.pivot_upward m2e.(0) m2e.(1) ;;
let m2g = G.matrix_mult m2 m2f.(1) ;;
let m2h = G.matrix_mult m2f.(1) m2 ;;
let m2i = G.inv m2 ;;
let m2j = G.matrix_mult m2 m2i ;;
let m2h = G.matrix_mult m2i m2 ;;
let m2h0 = G.matrix_to_full m2h ;;
let m2j0 = G.matrix_to_full m2j ;;



