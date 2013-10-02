

module E = Sparse_vector.Rng (Data.Zindex) (Hash.Z) (Data.Zcoeff) ;;


E.H.add ;;

let v = [| 0 ; 1 ; 0 ; 0 ; 2 ; 0 ; 3 ; 0 ; 0 ; 4 ; 0 ; 0 ; 0 ; 5 |] ;;
let v0 = E.to_sparse 2 v ;;
E.filling v0 ;;
let v0a = E.to_full v0 ;;
let v1 = E.copy v0 ;;
E.filling v1 ;;

let l0 = E.H.flush ( snd v0 ) ;;
E.filling v0 ;;
let v0b = E.to_full v0 ;;

let v2 = E.map pred v1 ;;
let v2a = E.to_full v2 ;;
E.filling v2 ;;

let v1a = E.to_full v1 ;;
E.filling v1 ;;

let v3 = E.add v2 v1 ;;
let v2a = E.to_full v2 ;;
let v3a = E.to_full v3 ;;

let v4 = E.add v2 v1 ;;
let v1a = E.to_full v1 ;;
let v2a = E.to_full v2 ;;
let v4a = E.to_full v4 ;;

let v5 = E.sub v4 v3 ;;
E.filling v5 ;;
let v5a = E.to_full v5 ;;

let v6 = E.sub_vector 2 7 v1 ;;
let v6a = E.to_full v6 ;;

E.find_all 2 v1 ;;
E.list_find_all 2 v1 ;;
E.index_list_find_all 2 v1 ;;

E.sum v1 ;;
E.contraction 1 v1 ;;
E.norm_1 v1 ;;

let v1b = E.to_string v1 ;;
E.print v1 ;;
let v1c = E.of_string v1b ;;
let v1d = E.to_full v1c ;;

E.min v1 ;;
E.max v1 ;;
E.norm_1 v1 ;;
E.norm_inf v1 ;;

E.in_place_coeff_prod v3 v4 ;;
let v3b = E.to_full v3 ;;

E.scal_prod v4 v1 ;;
E.square_norm_2 v1 ;;

E.compare v1 v4 ;;

let v7 = E.copy v3 ;;
E.exchange 1 13 v7 ;;
E.exchange 7 9 v7 ;;
let v7b = E.to_full v7 ;;

let v7c = E.filter ( ( > ) 20 ) v7 ;;
let v7d = E.to_full ( E.beginning 6 v7 ) ;;
let v7e = E.to_full ( E.ending 7 v7 ) ;;


module T = Sparse_tensor.Rng (Data.Zindex) (Hash.Z) (Data.Zcoeff) ;;


let t0 = T.null [| 2 ; 3 ; 4 |];;
let ( e0 , i0 , w0 ) = T.flat_tensor_demakeup t0 ;;
print_newline () ;;

let t0a = T.to_string t0 ;;

let t1 = T.of_string t0a ;;

let t2 = T.copy t0 ;;
T.replace 1 [| 0 ; 0 ; 0 |] t2 ;;
T.replace 2 [| 1 ; 1 ; 1 |] t2 ;;
T.replace 3 [| 0 ; 2 ; 2 |] t2 ;;
T.replace 4 [| 1 ; 0 ; 3 |] t2 ;;
T.replace 5 [| 0 ; 1 ; 0 |] t2 ;;
let t2a = T.copy t2 ;;
T.to_string t2 ;;
T.bare_to_string t2 ;;

T.cleanup t2 ;;
T.to_string t2 ;;
T.bare_to_string t2 ;;

let t2z = T.sub_tensor_extract 0 0 t2 ;;
T.to_string t2z ;;
T.bare_to_string t2z ;;

let t2y = T.sub_tensor_extract 2 3 t2 ;;
T.to_string t2y ;;
T.bare_to_string t2y ;;

T.sub_tensor_remove 1 0 t2z ;;
T.to_string t2z ;;
T.bare_to_string t2z ;;
T.cleanup t2z ;;
T.to_string t2z ;;
T.bare_to_string t2z ;;

T.sub_tensor_replace t2z 0 0 t2 ;;
T.to_string t2 ;;

T.bare_to_string t2 ;;
T.cleanup t2 ;;
T.to_string t2 ;;
T.bare_to_string t2 ;;


t2a ;;
T.to_string t2a ;;
let t2b = T.sub_flat_tensor [| 1 |] t2a ;;
T.to_string t2b ;;
T.bare_to_string t2b ;;
let t2c = T.suffix_sub_flat_tensor [| 0 |] t2a ;;
T.to_string t2c;;
T.bare_to_string t2c ;;
let t2d = T.sub_vector_tensor [| 0 ; 1 |] t2a ;;
T.V.to_string t2d;;
let t2e = T.suffix_sub_vector_tensor [| 0 ; 3 |] t2a ;;
T.V.to_string t2e;;
let t2f = T.sub_flat_tensor [| 0 |] t2a ;;
T.to_string t2f ;;
T.bare_to_string t2f ;;
let t2g = T.suffix_sub_flat_tensor [| 0 |] t2a ;;
T.to_string t2g ;;
T.bare_to_string t2g ;;

T.sum t2a ;;

let t3 = T.map ( ( * ) 2 ) t2a ;;
T.bare_to_string t3 ;;
T.fold ( fun ( i , x ) y -> x + y ) t2a 0 ;;
T.fold ( fun ( i , x ) y -> x + y ) t3 0 ;;
let t4 = T.sub t2a ( T.sub t3 t2a ) ;;
T.eq_zero t4 ;;
let t5 = T.scal_add 3 t3 ;;
T.bare_to_string t5 ;;
T.sum t5 ;;
let t6 = T.scal_mult (-10) t3 ;;
T.bare_to_string t6 ;;
T.sum t6 ;;
let t7 = T.scal_left_sub 2 t3 ;;
T.bare_to_string t7 ;;
T.sum t7 ;;
let t8 = T.scal_right_sub 10 t3 ;;
T.bare_to_string t8 ;;
T.sum t8 ;;
let t9 = T.coeff_prod t2 t2 ;;
T.bare_to_string t9 ;;
T.sum t9 ;;
T.scal_prod t2 t2 ;;
T.scal_prod t2 t6 ;;
T.norm_1 t2 ;;
T.norm_inf t2 ;;
T.square_norm_2 t2 ;;
T.norm_compare T.norm_inf t2 t3 ;;
T.compare t2 t3 ;;

let t10 = T.copy t2 ;;

T.exchange 2 1 2 t10 ;;
T.bare_to_string t10 ;;
let t11 = T.sub t2 t10 ;;
T.bare_to_string t11 ;;

let t12 = T.copy t2a ;;
T.level_exchange 0 2 t12 ;;
T.bare_to_string t12 ;;
let t13 = T.sub t2a t12 ;;
T.bare_to_string t13 ;;

let t2h = T.to_string t2a ;;
let t2i = T.of_string t2h ;;
let t2j = T.to_string t2i ;;
T.eq_zero ( T.sub t2a t2i ) ;;
String.compare t2h t2j ;;

T.sizes t2a ;;


module M = Sparse_matrix.Rng (Data.Zindex) (Hash.Z) (Data.Zcoeff) ;;


let m0 = M.null [| 3 ; 4 |] ;;
let m0t = M.sparse_tensor_matrix_demakeup m0 ;;

T.replace 1 [| 0 ; 0 |] m0t ;;
T.replace 2 [| 1 ; 1 |] m0t ;;
T.replace 3 [| 2 ; 2 |] m0t ;;
T.replace 4 [| 0 ; 3 |] m0t ;;
T.replace 5 [| 1 ; 0 |] m0t ;;

m0t ;;
m0 ;;

let m0a = M.to_string m0 ;;
let m0a0 = M.of_string m0a ;;
let m0a0t = M.sparse_tensor_matrix_demakeup m0a0 ;;
let m0a0a = M.to_string m0a0 ;;
String.compare m0a m0a0a ;;
M.T.eq m0t m0a0t ;;

let m1a = [| [| 1 ; 0 ; 2 ; 0 |] ; [| 0 ; 0 ; 0 ; 3 |] ; [| 0 ; 4 ; 5 ; 0 |] |] ;;
let m1 = M.Half_full_matrix ( Array.map ( M.V.to_sparse 2 ) m1a ) ;;
let m1a0 = M.to_string m1 ;;
let m1a0a = M.of_string m1a0 ;;
let m1a0b = Array.map M.V.to_full ( M.half_full_matrix_demakeup m1a0a ) ;;

let m2 = M.Diff_to_scal_matrix ( 10 , M.T.copy m0t ) ;;
let m2a = M.to_string m2 ;;
let m2a0 = M.of_string m2a ;;

let m3 = M.Diff_to_multi_scal_matrix ( [| -10 ; 10 ; 20 |] , M.T.copy m0t ) ;;
let m3a = M.to_string m3 ;;
let m3a0 = M.of_string m3a ;;

let m4 = M.Diff_to_diag_matrix ( [| -11 ; 13 ; 17 ; 29 ; 41 |] , M.T.copy m0t ) ;;
let m4a = M.to_string m4 ;;
let m4a0 = M.of_string m4a ;;

let m5 = M.Diff_to_multi_diag_matrix ( [| [| -11 ; 13 ; 17 ; 29 ; 41 |] ; [| 1 ; 2 ; 3 ; 4 ; 5 |] ; [| -5 ; -4 ; -3 ; -2 ; -1 |] |] , M.T.copy m0t ) ;;
let m5a = M.to_string m5 ;;
let m5a0 = M.of_string m5a ;;
M.print m5a0 ;;

let mm = [| m0 ; m1 ; m2 ; m3 ; m4 ; m5 |] ;;
Array.map M.sizes mm ;;
let mm0 = Array.map ( M.row_extract 0 ) mm ;;
let mm0a = Array.map M.V.to_full mm0 ;;
let mm1 = Array.map ( M.column_extract 0 ) mm ;;
let mm1a = Array.map M.V.to_full mm1 ;;

let d0 = M.sparse_diag_extract m0 ;;
let d1 = M.sparse_diag_extract m1 ;;
let d0a = M.V.to_full d0 ;;
let d1a = M.V.to_full d1 ;;

let d = Array.map M.full_diag_extract mm ;;

let m0b = M.to_half_full m0 ;;
let m1b = M.to_half_full m1 ;;
let m2b = M.to_half_full m2 ;;
let m3b = M.to_half_full m3 ;;
let m4b = M.to_half_full m4 ;;
let m5b = M.to_half_full m5 ;;

let m0d = M.to_sparse_tensor ( M.copy m0 ) ;;
let m1d = M.to_sparse_tensor ( M.copy m1 ) ;;
let m2d = M.to_sparse_tensor ( M.copy m2 ) ;;
let m3d = M.to_sparse_tensor ( M.copy m3 ) ;;
let m4d = M.to_sparse_tensor ( M.copy m4 ) ;;
let m5d = M.to_sparse_tensor ( M.copy m5 ) ;;

let m0f = M.to_diff_to_scal ( M.copy m0 ) ;;
let m1f = M.to_diff_to_scal ( M.copy m1 ) ;;
let m2f = M.to_diff_to_scal ( M.copy m2 ) ;;
let m3f = M.to_diff_to_scal ( M.copy m3 ) ;;
let m4f = M.to_diff_to_scal ( M.copy m4 ) ;;
let m5f = M.to_diff_to_scal ( M.copy m5 ) ;;

let m0h = M.to_diff_to_multi_scal ( M.copy m0 ) ;;
let m1h = M.to_diff_to_multi_scal ( M.copy m1 ) ;;
let m2h = M.to_diff_to_multi_scal ( M.copy m2 ) ;;
let m3h = M.to_diff_to_multi_scal ( M.copy m3 ) ;;
let m4h = M.to_diff_to_multi_scal ( M.copy m4 ) ;;
let m5h = M.to_diff_to_multi_scal ( M.copy m5 ) ;;

let m0j = M.to_diff_to_diag ( M.copy m0 ) ;;
let m1j = M.to_diff_to_diag ( M.copy m1 ) ;;
let m2j = M.to_diff_to_diag ( M.copy m2 ) ;;
let m3j = M.to_diff_to_diag ( M.copy m3 ) ;;
let m4j = M.to_diff_to_diag ( M.copy m4 ) ;;
let m5j = M.to_diff_to_diag ( M.copy m5 ) ;;

let m0l = M.to_diff_to_multi_diag ( M.copy m0 ) ;;
let m1l = M.to_diff_to_multi_diag ( M.copy m1 ) ;;
let m2l = M.to_diff_to_multi_diag ( M.copy m2 ) ;;
let m3l = M.to_diff_to_multi_diag ( M.copy m3 ) ;;
let m4l = M.to_diff_to_multi_diag ( M.copy m4 ) ;;
let m5l = M.to_diff_to_multi_diag ( M.copy m5 ) ;;


let m0c = M.half_full_matrix_to_full m0b ;;
let m1c = M.half_full_matrix_to_full m1b ;;
let m2c = M.half_full_matrix_to_full m2b ;;
let m3c = M.half_full_matrix_to_full m3b ;;
let m4c = M.half_full_matrix_to_full m4b ;;
let m5c = M.half_full_matrix_to_full m5b ;;

let m0e = M.to_full m0d ;;
let m1e = M.to_full m1d ;;
let m2e = M.to_full m2d ;;
let m3e = M.to_full m3d ;;
let m4e = M.to_full m4d ;;
let m5e = M.to_full m5d ;;

let m0g = M.to_full m0f ;;
let m1g = M.to_full m1f ;;
let m2g = M.to_full m2f ;;
let m3g = M.to_full m3f ;;
let m4g = M.to_full m4f ;;
let m5g = M.to_full m5f ;;

let m0i = M.to_full m0h ;;
let m1i = M.to_full m1h ;;
let m2i = M.to_full m2h ;;
let m3i = M.to_full m3h ;;
let m4i = M.to_full m4h ;;
let m5i = M.to_full m5h ;;

let m0k = M.to_full m0j ;;
let m1k = M.to_full m1j ;;
let m2k = M.to_full m2j ;;
let m3k = M.to_full m3j ;;
let m4k = M.to_full m4j ;;
let m5k = M.to_full m5j ;;

let m0m = M.to_full m0l ;;
let m1m = M.to_full m1l ;;
let m2m = M.to_full m2l ;;
let m3m = M.to_full m3l ;;
let m4m = M.to_full m4l ;;
let m5m = M.to_full m5l ;;

let a0 = [| m0c ; m1c ; m2c ; m3c ; m4c ; m5c |] ;;
let a1 = Array.map ( M.to_sparse 2 0.9 ) a0 ;;
let a2 = Array.map M.to_full a1 ;;

M.eq_zero m0 ;;

M.V.to_full ( M.row_extract 0 m0 ) ;;
M.V.to_full ( M.row_extract 0 m1 ) ;;
M.V.to_full ( M.row_extract 0 m2 ) ;;
M.V.to_full ( M.row_extract 0 m3 ) ;;
M.V.to_full ( M.row_extract 0 m4 ) ;;
M.V.to_full ( M.row_extract 0 m5 ) ;;

M.V.to_full ( M.column_extract 0 m0 ) ;;
M.V.to_full ( M.column_extract 0 m1 ) ;;
M.V.to_full ( M.column_extract 0 m2 ) ;;
M.V.to_full ( M.column_extract 0 m3 ) ;;
M.V.to_full ( M.column_extract 0 m4 ) ;;
M.V.to_full ( M.column_extract 0 m5 ) ;;

Array.map ( M.extract 0 0 ) a1 ;;
let a0a = [| m0 ; m1 ; m2 ; m3 ; m4 ; m5 |] ;;
let a0a0 = Array.map M.to_full a0a ;;
let a0b = Array.map M.transpose a0a ;;
let a0c = Array.map M.to_full a0b ;;
let a0d = Array.map Util.transpose ( Array.map M.to_full a0a ) ;;

let a0e = Array.map M.V.to_full ( Array.map M.sparse_diag_extract [| m0 ; m1 |] ) ;;
let a0f = Array.map M.full_diag_extract a0a ;;
let a0g = Array.map M.diag_isolate a0a ;;
let a0h = Array.map M.to_full a0g ;;
let a0i = Array.map M.out_diag_isolate a0a ;;
let a0j = Array.map M.to_full a0i ;;
let a0k = Array.map M.upper_diag_isolate a0a ;;
let a0l = Array.map M.to_full a0k ;;
let a0m = Array.map M.lower_diag_isolate a0a ;;
let a0n = Array.map M.to_full a0m ;;

M.raw_find 1 m0 ;;
M.raw_find 3 m0 ;;
M.raw_find 0 m0 ;;
M.raw_find 1 m1 ;;
M.raw_find 3 m1 ;;
M.raw_find 0 m1 ;;

Array.map ( M.find 5 ) a0a ;;

let rm0t = M.tensor_sub_row_extract 1 1 2 m0t ;;
M.V.to_full rm0t ;;
let cm0t = M.tensor_sub_column_extract 1 1 2 m0t ;;
M.V.to_full cm0t ;;
let hm0t = M.tensor_masked_hor_band 1 1 m0t ;;
M.to_full ( M.Sparse_tensor_matrix hm0t ) ;;
let vm0t = M.tensor_masked_vert_band 1 3 m0t ;;
M.to_full ( M.Sparse_tensor_matrix vm0t ) ;;

let a0p = Array.map ( M.masked_hor_band 1 1 ) a0a ;;
let a0q = Array.map M.to_full a0p ;;
let a0r = Array.map ( M.masked_vert_band 1 2 ) a0a ;;
let a0s = Array.map M.to_full ( Array.map M.transpose a0r ) ;;
let a0t = Array.map ( M.masked_head 1 2 ) a0a ;;
let a0u = Array.map M.to_full a0t ;;
let a0v = Array.map ( M.masked_tail 1 2 ) a0a ;;
let a0w = Array.map M.to_full a0v ;;
let a0x = Array.map ( M.masked_sample 1 1 1 2 ) a0a ;;
let a0y = Array.map M.to_full a0x ;;
let a0z = Array.map M.to_full ( Array.map ( M.masked_sample 0 1 1 2 ) a0a ) ;;

let a0p0 = Array.map ( M.hor_band 1 1 ) a0a ;;
let a0q0 = Array.map M.to_full a0p0 ;;
let a0r0 = Array.map ( M.vert_band 1 2 ) a0a ;;
let a0s0 = Array.map M.to_full a0r0 ;;
let a0t0 = Array.map ( M.head 1 2 ) a0a ;;
let a0u0 = Array.map M.to_full a0t0 ;;
let a0v0 = Array.map ( M.tail 1 2 ) a0a ;;
let a0w0 = Array.map M.to_full a0v0 ;;
let a0x0 = Array.map ( M.sample 1 1 1 2 ) a0a ;;
let a0y0 = Array.map M.to_full a0x0 ;;
let a0z0 = Array.map M.to_full ( Array.map ( M.sample 0 1 1 2 ) a0a ) ;;

let a3a = Array.map ( M.copy ) a0a ;;
let a3b0 = Array.map M.to_full a3a ;;
Array.map ( M.row_exchange 0 1 ) a3a ;;
let a3b1 = Array.map M.to_full a3a ;;

let a4a = Array.map ( M.copy ) a0a ;;
Array.map ( M.column_exchange 0 1 ) a4a ;;
let a4b1 = Array.map M.to_full a4a ;;

let a5a = Array.map ( M.copy ) a0a ;;
a5a.(2) <- M.null [| 3 ; 4 |] ;;
Array.map ( M.in_place_map ( ~- ) ) a5a ;;
let a5a0 = Array.map M.to_full a5a ;;

let a6a = Array.map ( M.copy ) a0a ;;
let a6b = Array.map ( M.map ( ~- ) ) a6a ;;
let a6b0 = Array.map M.to_full a6b ;;

let a6c = Array.map ( M.copy ) a0a ;;
let a6d = Array.map ( M.opp ) a6c ;;
let a6d0 = Array.map M.to_full a6d ;;

let a7a = Array.map ( M.copy ) a0a ;;
let a7b = Array.map ( M.embed [| 6 ; 6 |] [| 2 ; 1 |] ) a7a ;;
let a7c = Array.map ( M.embed [| 5 ; 6 |] [| 1 ; 1 |] ) a7a ;;
let a7b0 = Array.map M.to_full a7b ;;
let a7c0 = Array.map M.to_full a7c ;;

let a8a = Array.map ( M.copy ) a0a ;;
let f8 = fun x v -> M.V.sum v ;;
let v8 = M.V.null 3 ;;
let a8b = Array.map ( M.sparse_row_fold f8 v8 ) a8a ;;
let a8b0 = Array.map M.V.to_full a8b ;;
let a8c = Array.map M.sparse_row_sum a8a ;;
let a8c0 = Array.map M.V.to_full a8c ;;

let a9a = Array.map ( M.copy ) a0a ;;
let f9 = fun x v -> M.V.sum v ;;
let v9 = M.V.null 4 ;;
let a9b = Array.map ( M.sparse_column_fold f9 v9 ) a9a ;;
let a9b0 = Array.map M.V.to_full a9b ;;
let a9c = Array.map M.sparse_column_sum a9a ;;
let a9c0 = Array.map M.V.to_full a9c ;;

let aAa = Array.map ( M.copy ) a0a ;;
let fA = fun x v -> M.V.sum v ;;
let vA = Array.make 3 0 ;;
let aAb = Array.map ( M.full_row_fold fA vA ) aAa ;;
let aAc = Array.map M.full_row_sum aAa ;;

let aBa = Array.map ( M.copy ) a0a ;;
let fB = fun x v -> M.V.sum v ;;
let vB = Array.make 4 0 ;;
let aBb = Array.map ( M.full_column_fold fB vB ) aBa ;;
let aBc = Array.map M.full_column_sum aBa ;;

let m00 =  M.sparse_tensor_matrix_demakeup m0 ;;
let r0 = M.sparse_tensor_dirty_rows_list m00 ;;
let c0 = M.sparse_tensor_dirty_columns_list m00 ;;
let r1 = M.sparse_tensor_dirty_rows_array m00 ;;
let c1 = M.sparse_tensor_dirty_columns_array m00 ;;

Array.map M.sparse_matrix_sum a9a ;;
Array.map M.full_matrix_sum a9a ;;

let aCa = Array.map ( M.copy ) a0a ;;
let vCa = Array.make 4 1 ;;
let vC = M.V.to_sparse 3 vCa ;;
let vC0 = Array.map ( function x -> M.matrix_sparse_vector_sparse_prod x vC ) aCa ;;
let vC1 = Array.map M.V.to_full vC0 ;;
let vC2 = Array.map ( function x -> M.matrix_full_vector_sparse_prod x vCa ) aCa ;;
let vC3 = Array.map M.V.to_full vC2 ;;

let vCb = Array.make 3 1 ;;
let vCc = M.V.to_sparse 3 vCb ;;
let vC4 = Array.map ( M.sparse_vector_matrix_sparse_prod vCc ) aCa ;;
let vC5 = Array.map M.V.to_full vC4 ;;
let vC6 = Array.map ( M.full_vector_matrix_sparse_prod vCb ) aCa ;;
let vC7 = Array.map M.V.to_full vC6 ;;

let vC8 = Array.map ( function x -> M.matrix_sparse_vector_full_prod x vC ) aCa ;;
let vC9 = Array.map ( function x -> M.matrix_full_vector_full_prod x vCa ) aCa ;;
let vCA = Array.map ( M.sparse_vector_matrix_full_prod vCc ) aCa ;;
let vCB = Array.map ( M.full_vector_matrix_full_prod vCb ) aCa ;;

let b8a = Array.map ( M.copy ) a0a ;;
let b8c = Array.map M.sparse_row_max b8a ;;
let b8c0 = Array.map M.V.to_full b8c ;;

let b9a = Array.map ( M.copy ) a0a ;;
let b9c = Array.map M.sparse_column_max b9a ;;
let b9c0 = Array.map M.V.to_full b9c ;;

let bAa = Array.map ( M.copy ) a0a ;;
let bAb = Array.map ( M.full_row_fold fA vA ) bAa ;;
let bAc = Array.map M.full_row_max bAa ;;

let bBa = Array.map ( M.copy ) a0a ;;
let bBb = Array.map ( M.full_column_fold fB vB ) bBa ;;
let bBc = Array.map M.full_column_max bBa ;;

let c8a = Array.map ( M.copy ) a0a ;;
let c8c = Array.map M.sparse_row_min c8a ;;
let c8c0 = Array.map M.V.to_full c8c ;;

let c9a = Array.map ( M.copy ) a0a ;;
let c9c = Array.map M.sparse_column_min c9a ;;
let c9c0 = Array.map M.V.to_full c9c ;;

let cAa = Array.map ( M.copy ) a0a ;;
let cAb = Array.map ( M.full_row_fold fA vA ) cAa ;;
let cAc = Array.map M.full_row_min cAa ;;

let cBa = Array.map ( M.copy ) a0a ;;
let cBb = Array.map ( M.full_column_fold fB vB ) cBa ;;
let cBc = Array.map M.full_column_min cBa ;;

let bCa = Array.map ( M.norm_inf ) b8a ;;
let bCb = Array.map ( M.norm_1 ) b8a ;;
let bCc = Array.map ( M.square_norm_frobenius ) b8a ;;
let bCd = Array.map ( M.norm_sum ) b8a ;;

let bCe = Array.map ( M.trace ) b8a ;;
let bCf = Array.map ( M.full_trace ) b8a ;;

let d8a = Array.map ( M.scal_mult ( -10 ) ) a8a ;;
let d8b = Array.map M.to_full d8a ;;

let e8a = Array.make 6 ( Array.map M.copy a0a ) ;;
let e8a0 = Array.map ( Array.map ( M.to_full ) ) e8a ;;
let ff8 = fun i x -> Array.map ( M.add a8a.(i) ) x ;;
let e8b = Array.mapi ff8 e8a ;;
let e8c = Array.map ( Array.map ( M.to_full ) ) e8b ;;

let ff9 = fun i x -> Array.map ( M.sub a8a.(i) ) x ;;
let e9b = Array.mapi ff9 e8a ;;
let e9c = Array.map ( Array.map ( M.to_full ) ) e9b ;;

let ffA = fun i x -> Array.map ( M.eq a8a.(i) ) x ;;
let eAb = Array.mapi ffA e8a ;;

let g8 = Array.map ( M.twisted_prod a8a.(1) ) a8a ;;
let g8a = Array.map M.to_full g8 ;;
let g8b = Array.map ( M.twisted_prod a8a.(0) ) a8a ;;
let g8c = Array.map M.to_full g8b ;;
let g8d = Array.map ( M.twisted_prod a8a.(2) ) a8a ;;
let g8e = Array.map M.to_full g8d ;;
let g8f = Array.map ( M.twisted_prod a8a.(3) ) a8a ;;
let g8g = Array.map M.to_full g8f ;;
let g8h = Array.map ( M.twisted_prod a8a.(4) ) a8a ;;
let g8i = Array.map M.to_full g8h ;;
let g8j = Array.map ( M.twisted_prod a8a.(5) ) a8a ;;
let g8k = Array.map M.to_full g8j ;;
M.to_full ( M.twisted_prod a8a.(0) a8a.(0) ) ;;
Array.map M.to_full a8a ;;

let h0 = Array.map ( Array.map Random.int ) ( Array.make_matrix 3 3 10 ) ;;
let h0a = M.to_sparse 2 0.3 h0 ;;
let h1a = M.to_sparse 2 1. h0 ;;
let h2a = M.to_sparse 2 10. h0 ;;
let h0a0 = M.to_full h0a ;;
let h1a0 = M.to_full h1a ;;
let h2a0 = M.to_full h2a ;;
let ( h00b , h01b , h02b ) = ( M.mult h0a h0a , M.mult h0a h1a , M.mult h0a h2a ) ;;
let ( h10b , h11b , h12b ) = ( M.mult h1a h0a , M.mult h1a h1a , M.mult h1a h2a ) ;;
let ( h20b , h21b , h22b ) = ( M.mult h2a h0a , M.mult h2a h1a , M.mult h2a h2a ) ;;
let ( h00c , h01c , h02c ) = ( M.to_full h00b , M.to_full h01b , M.to_full h02b ) ;;
let ( h10c , h11c , h12c ) = ( M.to_full h10b , M.to_full h11b , M.to_full h12b ) ;;
let ( h20c , h21c , h22c ) = ( M.to_full h20b , M.to_full h21b , M.to_full h22b ) ;;
(* 
let h0d = Matrix.matrix_int_prod h0 h0 ;;
*)


M.sparse_tensor_dirty_rows_list m0t ;;
M.sparse_tensor_dirty_columns_list m0t ;;


module N = Sparse_matrix.Field (Data.Zindex) (Hash.Z) (Data.Rcoeff) ;;


let n0 = N.null [| 3 ; 3 |] ;;
let n00 = N.to_full n0 ;;
let n0a = N.invertibility n0 ;;
let n0b = N.det n0 ;;

let id0 = N.Diff_to_scal_matrix ( 1. , N.S.null [| 3 ; 3 |] ) ;;

let n1 = Array.make_matrix 3 3 1. ;;
let n1a = Array.map ( Array.map Random.float ) n1 ;;
let n1b = N.to_sparse 2 0.3 n1a ;;
let n1b0 = N.to_full n1b ;;
let n1c = N.invertibility n1b ;;
let n1d = N.det n1b ;;

let n1e = N.pivot_downward n1b id0 ;;
let n1f = Array.map N.to_full n1e ;;
let n1g = N.pivot_upward n1e.(0) n1e.(1) ;;
let n1h = Array.map N.to_full n1g ;;
let n1i = N.inv n1b ;;
let n1j = N.to_full n1i ;;
let n1k = N.left_quotient n1b n1b ;;
let n1l = N.right_quotient n1b n1b ;;
Array.map N.to_full [| n1k ; n1l |] ;;
N.full_solve n1b [| 1. ; 2. ; 3. |] ;;
N.U.to_full ( N.naive_solve n1b ( N.U.to_sparse (-1) [| 1. ; 2. ; 3. |] ) ) ;;
N.U.to_full ( N.solve n1b ( N.U.to_sparse (-1) [| 1. ; 2. ; 3. |] ) ) ;;
let n1m = N.tune_inv n1b n1i ;;
let n1n = N.to_full n1m ;;

(*
#use "../matrix/matrix.ml" ;;
let n1d0 = Matrix.float_det n1a ;;
let n1d1 = Matrix.float_slow_det n1a ;;
Matrix.matrix_float_prod n1a n1h.(1) ;;
Matrix.matrix_float_prod n1a n1j ;;
Matrix.matrix_float_prod n1j n1a ;;
Matrix.matrix_float_prod n1n n1a ;;
*)

let i1 = N.Diff_to_scal_matrix ( 1. , N.S.null [| 3 ; 3 |] ) ;;
let i2 = N.copy i1 ;;
N.insert_add 1. 1 2 i2 ;;
N.insert_add 1. 2 1 i2 ;;

N.insert_add 2. 0 2 i1 ;;
N.insert_add 3. 2 1 i1 ;;
N.insert_add (-4.) 1 1 i1 ;;
let i1a = N.to_full i1 ;;
let i1b = N.inv i1 ;;
let i1b0 = N.to_full i1b ;;
let i1b1 = N.mult i1 i1b ;;
let i1b2 = N.mult i1b i1 ;;
N.print i1b1 ;;
N.print i1b2 ;;
N.to_full i1b1 ;;
N.to_full i1b2 ;;

let i1c = N.diff_to_id_pivot_downward i1b i1 ;;
Array.map N.to_full i1c ;;
let i1d = N.diff_to_id_invertibility i1b ;;
let i1i = N.diff_to_id_invertibility i1 ;;
let i2i = N.diff_to_id_invertibility i2 ;;
N.diff_to_id_det i2 ;;
N.diff_to_id_det i1 ;;
N.diff_to_id_det i1b ;;
N.diff_to_id_det i1b1 ;;
N.diff_to_id_det i1b2 ;;
let i1e = N.diff_to_id_inv i1 ;;
let i1e0 = N.mult i1e i1 ;;
let i1e1 = N.mult i1 i1e ;;
N.print i1e0 ;;
N.print i1e1 ;;

(*
module O = Sparse_matrix.Field (Data.Num_index) (Hash.Number) (Data.Rcoeff) ;;
let g0 = O.Diff_to_scal_matrix ( 1. , O.S.null [| Sci.num_2_pow_2048 ; Sci.num_2_pow_2048 |] ) ;;
O.insert_add 2. Sci.num_0 Sci.num_0 g0 ;;
O.insert_add 3. Sci.num_0 Sci.num_2_pow_1024 g0 ;;
O.insert_add ( -2. ) Sci.num_2_pow_1024 Sci.num_0 g0 ;;
O.insert_add 4. Sci.num_2_pow_1024 Sci.num_2_pow_1024 g0 ;;
O.diff_to_id_det g0 ;;
O.diff_to_id_invertibility g0 ;;
let g1 = O.diff_to_id_inv g0 ;;

let g2 = O.mult g1 g0 ;;
let g3 = O.mult g0 g1 ;;
O.print g1 ;;
O.print g2 ;;
O.print g3 ;;

Array.map Num.string_of_num ( O.dimensions g0 ) ;;
*)


