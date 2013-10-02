
let z0 = {Complex.re=1. ; Complex.im=1e-8};;
let z1 = Reduc.built_in_complex_to_matrix z0 ;;
let y0 = Reduc.built_in_complex_to_matrix ( Complex.inv z0 ) ;;
let y1 = Reduc.complex_inv_bis z1 ;;
let y2 = Matrix.matrix_float_plus ( Reduc.clean_complex y1 ) ( Reduc.clean_complex ( Matrix.matrix_float_scal_mult 2. ( Matrix.matrix_float_minus y1 y0 ) ) ) ;;
let y3 = Matrix.float_tune_inv Matrix.matrix_float_norm_frobenius z1 y2 ;;
let y4 = Matrix.matrix_float_approx [| y0 ; y1 ; y2 |] ;;
let y5 = Matrix.matrix_float_approx [| y0 ; y2 ; y1 |] ;;
let y6 = Reduc.complex_inv_ter 2. z1 ;;
let y7 = Reduc.complex_inv_ter 1.5 z1 ;;
let y8 = Reduc.complex_inv z1 ;;
let y9 = Reduc.complex_slow_inv z1 ;;

let zzzz = {Complex.re=1e-8 ; Complex.im=1.};;
let zzz = Reduc.built_in_complex_to_matrix zzzz ;;
let yyyy = Reduc.built_in_complex_to_matrix ( Complex.inv zzzz ) ;;
let yyy = Reduc.complex_inv zzz ;;
let y_y = Reduc.complex_slow_inv zzz ;;

let a = [| [| 1. ; 2. |] ; [| -2. ; 1. |] |] ;;

Reduc.apply_built_in_complex_to_matrix Complex.exp Reduc.complex_1 ;;
Reduc.apply_built_in_complex_to_matrix Complex.exp Reduc.complex_i ;;
cos 1. ;;
sin 1. ;;
Reduc.apply_built_in_complex_to_matrix Complex.exp a ;;

Reduc.apply_built_in_complex_float_to_matrix Complex.norm2 a ;;
sqrt 5. ;;
Reduc.apply_built_in_complex_float_to_matrix Complex.norm a ;;
Reduc.apply_built_in_complex_float_to_matrix Complex.arg a ;;

Reduc.polar_to_matrix 1. 1. ;;

Reduc.apply_built_in_complex_to_matrix Complex.log Reduc.complex_i ;;
Reduc.apply2_built_in_complex_to_matrix Complex.mul Reduc.complex_i Reduc.complex_i ;;

let p = Matrix.float_closed_equal_range 1. 1. 10. ;;
Reduc.poly_real_deriv p ;;
Reduc.poly_real_plus p p ;;
Reduc.poly_real_minus p p ;;
Reduc.poly_real_mult p p ;;
Reduc.poly_real_mult_karatsuba 2 p p ;;
Reduc.poly_real_mult p p ;;
Reduc.poly_real_pow Reduc.poly_real_mult 2 p ;;

let pp = Matrix.int_of_vector p ;;
Reduc.poly_int_deriv pp ;;
Reduc.poly_int_plus pp pp ;;
Reduc.poly_int_minus pp pp ;;
Reduc.poly_int_mult pp pp ;;
Reduc.poly_int_pow Reduc.poly_int_mult 2 pp ;;

(*
session gp
? p=1+2*x+3*x^2+4*x^3+5*x^4+6*x^5+7*x^6+8*x^7+9*x^8+10*x^9
%7 = 10*x^9 + 9*x^8 + 8*x^7 + 7*x^6 + 6*x^5 + 5*x^4 + 4*x^3 + 3*x^2 + 2*x + 1
? p*p
%8 = 100*x^18 + 180*x^17 + 241*x^16 + 284*x^15 + 310*x^14 + 320*x^13 + 315*x^12 + 296*x^11 + 264*x^10 + 220*x^9 + 165*x^8 + 120*x^7 + 84*x^6 + 56*x^5 + 35*x^4 + 20*x^3 + 10*x^2 + 4*x + 1
*)

Reduc.poly_real_pow Reduc.poly_real_mult 4 p ;;

let q = [| Reduc.complex_1 ; Reduc.complex_i ; Reduc.complex_1 |] ;;
Reduc.poly_complex_deriv q ;;
Reduc.poly_complex_plus q q ;;
Reduc.poly_complex_minus q q ;;
Reduc.poly_complex_mult q q ;;
Reduc.poly_complex_pow Reduc.poly_complex_mult 2 q ;;
(*
on doit trouver
1+2iX+X^2+2iX^3+X^4
*)

let r = [| [| [| 1. ; -1. |] ; [| 1. ; 1. |] |] ; [| [| -2. ; 1. |] ; [| -1. ; -2. |] |] |] ;;
Reduc.poly_complex_deriv r ;;
Reduc.poly_complex_plus r r ;;
Reduc.poly_complex_minus r r ;;
Reduc.poly_complex_mult r r ;;
Reduc.poly_complex_pow Reduc.poly_complex_mult 2 r ;;
(*
on doit trouver
-2i+(-2-6i)X+(3+4i)X^2
*)
Reduc.poly_complex_pow Reduc.poly_complex_mult 9 r ;;

Reduc.poly_real_to_complex p ;;

Reduc.poly_complex_real_part r ;;
Reduc.poly_complex_imag_part r ;;

Reduc.poly_real_div [|1.|] [|1.|] ;;
Reduc.poly_real_div [|0.|] [|1.|] ;;
Reduc.poly_real_div p [|1.|] ;;
Reduc.poly_real_div p [| 1. ; 2. |] ;;
Reduc.poly_real_div [| 1. ; 2. |] p ;;
(*
let s = Reduc.poly_real_div [|1.|] [|0.|] ;;
Array.map Reduc.poly_real_deg s ;;
Array.map Reduc.poly_real_val s ;;
*)

Reduc.poly_real_div_inc 2 [|1.|] [|1.|] ;;
Reduc.poly_real_div_inc 2 [|0.|] [|1.|] ;;
Reduc.poly_real_div_inc 2 p [|1.|] ;;
Reduc.poly_real_div_inc 1 p [| 1. ; 2. |] ;;
Reduc.poly_real_div_inc 1 [| 1. ; 2. |] p ;;
Reduc.poly_real_div_inc 1 [| 0. ; 1. ; 2. |] p ;;
Reduc.poly_real_div_inc 2 p [| 1. ; 2. |] ;;
Reduc.poly_real_div_inc 2 [| 1. ; 2. |] p ;;
Reduc.poly_real_div_inc 2 [| 0. ; 1. ; 2. |] p ;;
(*
Reduc.poly_real_div_inc 2 p [| 0. ; 1. ; 2. |] ;;
let s = Reduc.poly_real_div_inc 2 [|1.|] [|0.|] ;;
Array.map Reduc.poly_real_deg s ;;
Array.map Reduc.poly_real_val s ;;
*)
Reduc.poly_real_div_inc 2 [| 0. ; 1. ; 2. |] [| 1. ; 1. ; 1. |] ;;

let x2 = [|0.;0.;1.|] ;;
Reduc.poly_real_horner_comp p p ;;

Reduc.poly_real_ranged_horner_comp Reduc.poly_real_mult 5 3 p p ;;
Reduc.poly_real_ranged_horner_comp Reduc.poly_real_mult 0 3 p x2 ;;
Reduc.poly_real_ranged_horner_comp Reduc.poly_real_mult 0 3 p p ;;

let p_p = Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 4 p p ;;
Reduc.poly_real_cleanup p_p ;;
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 3 p p ;;
(*
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 2 p p ;;
*)
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 9 p p ;;
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 10 p p ;;

Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 4 p x2 ;;
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 3 p x2 ;;
(*
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 2 p x2 ;;
*)
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 9 p x2 ;;
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 10 p x2 ;;

Matrix.vector_float_minus ( Reduc.poly_real_horner_comp p p ) ( Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 4 p p ) ;;

(*
let p0 = Matrix.vector_float_random 20 1. ;;
let p0 = Matrix.vector_float_random 25 1. ;;
let p0 = Matrix.vector_float_random 30 1. ;;
let p0 = Matrix.vector_float_random 40 1. ;;
let p0 = Matrix.vector_float_random 50 1. ;;
let p0 = Matrix.vector_float_random 60 1. ;;
*)
let p0 = Matrix.vector_float_random 20 1. ;;
Reduc.poly_real_horner_comp p0 p0 ;;
Reduc.poly_real_brent_kung_hart_novocin_comp Reduc.poly_real_mult 4 p0 p0 ;;

Reduc.poly_real_mod p x2 ;;
Reduc.poly_real_gcd p x2 ;;
Reduc.poly_real_gcd [| 0. ; -1. ; 1. |] x2 ;;
Reduc.poly_real_gcd x2 [| 0. ; -1. ; 1. |] ;;
Reduc.poly_real_bezout Reduc.poly_real_mult x2 [| 0. ; -1. ; 1. |] ;;
Reduc.poly_real_bezout Reduc.poly_real_mult [| 0. ; -1. ; 1. |] x2 ;;
Reduc.poly_real_bezout Reduc.poly_real_mult [| 10. |] x2 ;;
Reduc.poly_real_bezout Reduc.poly_real_mult x2 [| 10. |] ;;

Reduc.poly_real_lcm Reduc.poly_real_mult x2 [| 0. ; -1. ; 1. |] ;;
Reduc.poly_real_lcm Reduc.poly_real_mult [| 0. ; -1. ; 1. |] x2 ;;

let m = Reduc.real_companion p ;;
Reduc.leverrier_real_char_poly m ;;
let mm = Matrix.float_companion p ;;
Reduc.leverrier_real_char_poly mm ;;
let i = Matrix.identity_float 4 4 ;;
Reduc.leverrier_real_char_poly i ;;
let a = [|[| 0. ; 1. |] ; [| 1. ; 1. |] |] ;;
Reduc.leverrier_real_char_poly a ;;
let b = [|[| 0. ; 1. ; 0. |] ; [| 1. ; 1. ; 0. |] ; [| 0. ; 0. ; 1. |] |] ;;
Reduc.leverrier_real_char_poly b ;;
let b0 = Reduc.matrix_real_inv ( Reduc.leverrier_real_char_poly b ) b ;;
Matrix.matrix_float_prod b b0 ;;

let ii = Reduc.scal_complex 2 2 Reduc.complex_i ;;
Matrix.matrix_float_prod ii ii ;;
Reduc.matrix_complex_scal_mult Reduc.complex_i ii ;;

let n = Reduc.complex_companion q ;;
Reduc.leverrier_complex_char_poly n ;;

Reduc.matrix_complex_extract_coefficient 1 1 n ;;
Reduc.matrix_complex_extract_row_to_poly 1 n ;;
Reduc.matrix_complex_extract_row_to_vector 1 n ;;
Reduc.matrix_complex_extract_row_to_matrix 1 n ;;
let u = Reduc.matrix_complex_extract_row_to_matrix_trans 1 n ;;
Reduc.matrix_complex_extract_column_to_poly 1 n ;;
Reduc.matrix_complex_extract_column_to_vector 1 n ;;
Reduc.matrix_complex_extract_column_to_matrix 1 n ;;
let v = Reduc.matrix_complex_extract_column_to_matrix_trans 1 n ;;

Reduc.vector_complex_hermitian_prod u u ;;
Reduc.vector_complex_hermitian_prod u v ;;
Reduc.vector_complex_hermitian_prod v u ;;
Reduc.vector_complex_hermitian_prod v v ;;

Reduc.leverrier_complex_char_poly n ;;
let qq = [| Matrix.matrix_float_scal_mult 2. Reduc.complex_i ; Reduc.complex_1 ; Reduc.float_to_complex 3. |] ;;
let nn = Reduc.complex_companion qq ;;
Reduc.leverrier_complex_char_poly nn ;;

let f = Reduc.poly_real_finite_prod Reduc.poly_real_mult [| [| -1. ; 1. |] ; [| -2. ; 1. |] ; [| -3. ; 1. |] ; [| -4. ; 1. |] |] ;;
Reduc.poly_real_from_roots Reduc.poly_real_mult [| 1. ; 2. ; 3. ; 4. |] ;;
let f_f = Reduc.real_companion f ;;
Reduc.leverrier_real_char_poly f_f ;;
let ff = Reduc.poly_int_finite_prod Reduc.poly_int_mult [| [| -1 ; 1 |] ; [| -2 ; 1 |] ; [| -3 ; 1 |] ; [| -4 ; 1 |] |] ;;
let fF = Reduc.int_companion ff ;;
Reduc.leverrier_int_char_poly fF ;;
let fff = Reduc.poly_complex_finite_prod Reduc.poly_complex_mult [| Reduc.poly_complex_x_a Reduc.complex_1 ; Reduc.poly_complex_x_a ( Matrix.scal_float 2 2 2. ) ;  Reduc.poly_complex_x_a ( Matrix.scal_float 2 2 3. ) ; Reduc.poly_complex_x_a ( Matrix.scal_float 2 2 4. ) |] ;;
let fFf = Reduc.complex_companion fff ;;
Reduc.leverrier_complex_char_poly fFf ;;
let ffff = Reduc.poly_gauss_finite_prod Reduc.poly_gauss_mult [| Reduc.poly_gauss_x_a Reduc.gauss_1 ; Reduc.poly_gauss_x_a ( Matrix.scal_int 2 2 2 ) ;  Reduc.poly_gauss_x_a ( Matrix.scal_int 2 2 3 ) ; Reduc.poly_gauss_x_a ( Matrix.scal_int 2 2 4 ) |] ;;
let fFff = Reduc.gauss_companion ffff ;;
Reduc.leverrier_gauss_char_poly fFff ;;

let z2 = [| Sci.sci_1 ; Sci.sci_1 ; Sci.sci_2 |] ;;
let zz2 = Reduc.poly_sci_horner_comp z2 z2 ;;
Matrix.vector_float_print ( Reduc.poly_sci_real_part zz2 ) ;;

let pz0 = Reduc.sci_jordan_decomposition_polynomial Reduc.poly_sci_mult Reduc.poly_sci_horner_comp [| Sci.sci_minus_1 ; Sci.sci_minus_1 ; Sci.sci_1 |] ;;
let pz1 = Reduc.lento_complex_jordan_decomposition_polynomial Reduc.poly_sci_mult Reduc.poly_sci_horner_comp [| Reduc.complex_minus_1 ; Reduc.complex_minus_1 ; Reduc.complex_1 |] ;;
let pz2 = Reduc.real_jordan_decomposition_polynomial Reduc.poly_real_mult Reduc.poly_real_horner_comp [| -1. ; -1. ; 1. |] ;;
let pz3 = Reduc.lento_real_jordan_decomposition_polynomial Reduc.poly_sci_mult Reduc.poly_sci_horner_comp [| -1. ; -1. ; 1. |] ;;

let px = Reduc.lento_real_jordan_decomposition_polynomial Reduc.poly_sci_mult Reduc.poly_sci_horner_comp x2 ;;
let x2x = Reduc.real_companion x2 ;;
Reduc.jordan_decomposition Reduc.poly_real_apply_matrix px x2x ;;

let j0 = Matrix.matrix_float_Jordan 20 3. ;;
let pj0 = Reduc.leverrier_real_char_poly j0 ;;
let ppj0 = Reduc.lento_real_jordan_decomposition_polynomial ( Reduc.poly_sci_mult_karatsuba 2 ) ( Reduc.poly_sci_brent_kung_hart_novocin_comp ( Reduc.poly_sci_mult_karatsuba 2 ) 4 ) pj0 ;;
let jj0 = Reduc.jordan_decomposition Reduc.poly_real_apply_matrix ppj0 j0 ;;
jj0.(1) ;;

Reduc.sci_jordan_decomposition_polynomial Reduc.poly_sci_mult Reduc.poly_sci_horner_comp ( Reduc.poly_sci_pow Reduc.poly_sci_mult 4 ( Reduc.poly_sci_x_a Sci.sci_3 ) ) ;;
Reduc.lento_complex_jordan_decomposition_polynomial Reduc.poly_sci_mult Reduc.poly_sci_horner_comp ( Reduc.poly_complex_pow Reduc.poly_complex_mult 4 ( Reduc.poly_complex_x_a ( Reduc.float_to_complex 3. ) ) ) ;;
Reduc.real_jordan_decomposition_polynomial Reduc.poly_real_mult Reduc.poly_real_horner_comp ( Reduc.poly_real_pow Reduc.poly_real_mult 4 [| -3. ; 1. |] ) ;;

Matrix.matrix_float_norm_inf jj0.(1) ;;
Matrix.matrix_float_plus jj0.(0) jj0.(1) ;;

Reduc.matrix_complex_jordan 3 Reduc.complex_i ;;
Reduc.matrix_gauss_jordan 3 Reduc.gauss_i ;;


(*
Les résultats qui suivent sont différents pour les différents choix d'algorithmes,
preuve que les erreurs d'arrondis sont prépondérantes.
Avec la donnée suivante, cela ne fonctionne plus.
let j0 = Matrix.matrix_float_Jordan 60 3. ;;
*)
let k0 = Reduc.jordan_decomposition Reduc.poly_complex_apply_matrix ( Reduc.poly_real_to_complex ppj0 ) j0 ;;
Matrix.matrix_float_norm_inf k0.(1) ;;
Matrix.matrix_float_norm_1 k0.(1) ;;
Matrix.matrix_float_plus k0.(0) k0.(1) ;;

let fff0 = Reduc.complex_jordan_decomposition_polynomial Reduc.poly_complex_mult Reduc.poly_complex_horner_comp fff ;;
let fFf0 = Reduc.jordan_decomposition Reduc.poly_complex_apply_matrix fff0 fFf ;;
Matrix.matrix_float_norm_inf fff0.(1) ;;
Matrix.matrix_float_plus fff0.(0) fff0.(1) ;;

Reduc.matrix_complex_extract_column_to_matrix_trans 0 n ;;
Reduc.matrix_complex_extract_column_to_poly 0 n ;;

let threshold_qr = epsilon_float ;;
Reduc.complex_householder_step threshold_qr 0 n ;;
Reduc.matrix_complex_extract_column_to_poly 0 fFf ;;
Reduc.complex_householder_step threshold_qr 0 fFf ;;

let n2 = Reduc.complex_householder_step threshold_qr 0 n ;;
let n3 = Matrix.matrix_float_prod n2.(0) n ;;
Reduc.matrix_complex_extract_column_to_poly 0 n3 ;;
Reduc.matrix_complex_extract_column_to_poly 1 n3 ;;

let n1 = Reduc.complex_qr_decomposition threshold_qr n ;;
Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.eye_float n1.(0) ) ( Matrix.matrix_float_prod n1.(0) n1.(2) ) ) ;;
Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus n ( Matrix.matrix_float_prod n1.(0) n1.(1) ) ) ;;
n1.(1);;
Reduc.matrix_complex_extract_column_to_poly 0 n1.(1) ;;
Reduc.matrix_complex_extract_column_to_poly 1 n1.(1) ;;

Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus n ( Matrix.matrix_float_prod n1.(0) n1.(1) ) ) ;;

let fFf1 = Reduc.complex_qr_decomposition threshold_qr fFf ;;
Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.eye_float fFf1.(0) ) ( Matrix.matrix_float_prod fFf1.(0) fFf1.(2) ) ) ;;
fFf1.(1);;
Reduc.matrix_complex_extract_column_to_poly 0 fFf1.(1) ;;
Reduc.matrix_complex_extract_column_to_poly 1 fFf1.(1) ;;
Reduc.matrix_complex_extract_column_to_poly 2 fFf1.(1) ;;
Reduc.matrix_complex_extract_column_to_poly 3 fFf1.(1) ;;

let threshold = epsilon_float ;;

let n4 = Reduc.complex_francis_iteration threshold_qr threshold 22 n ;;
Matrix.matrix_float_triple_prod n4.(1) n4.(0) n4.(2) ;;
Matrix.matrix_float_prod n4.(1) n4.(2) ;;

let fFf2 = Reduc.complex_francis_iteration threshold_qr threshold 10 fFf ;;
fFf2.(3) ;;
fFf2.(0) ;;

Reduc.complex_francis_schur_decomposition threshold_qr threshold 10 n ;;
Reduc.complex_francis_schur_decomposition threshold_qr threshold 11 n ;;
Reduc.complex_francis_spectrum threshold_qr threshold 10 n ;;

Reduc.complex_francis_schur_decomposition threshold_qr threshold 7 fFf ;;
let fFf3 = Reduc.complex_francis_schur_decomposition threshold_qr threshold 4 fFf ;;
Matrix.matrix_float_prod fFf3.(1) fFf3.(2) ;;
Matrix.matrix_float_triple_prod fFf3.(1) fFf3.(0) fFf3.(2) ;;
Reduc.matrix_complex_extract_diag_to_poly fFf3.(0) ;;
fFf3.(3);;

Reduc.complex_francis_spectrum threshold_qr threshold 8 fFf ;;
Reduc.complex_francis_spectrum threshold_qr threshold 12 fFf ;;
Reduc.complex_francis_spectrum threshold_qr threshold 25 fFf ;;
Reduc.leverrier_complex_char_poly fFf ;;

let methode_reduc = Matrix.sym_float_tune_reduc threshold 33 ;;
let methode_ker = Matrix.float_ker methode_reduc threshold ;;

let fFf4 = Reduc.direct_complex_diagonalization methode_ker threshold_qr threshold 128 8192 fFf ;;
fFf4.(0) ;;
Reduc.complex_francis_spectrum threshold_qr threshold 25 fFf ;;
let v = Matrix.float_extract_column 0 fFf4.(1).(0) ;;
Matrix.float_normalized_iterate Matrix.vector_float_norm_inf 22222 fFf v ;;

let fFf5 = fFf4.(1).(0) ;;
let fFf6 = Matrix.clean_inv fFf5 ;;
let fFf7 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.eye_float fFf5 ) ( Matrix.matrix_float_prod fFf5 fFf6 ) ) ;;

let fFf8 = fFf4.(2).(0) ;;
let fFf9 = Matrix.clean_inv fFf8 ;;
let fFf10 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.eye_float fFf5 ) ( Matrix.matrix_float_prod fFf8 fFf9 ) ) ;;

let fFf11 = Reduc.direct_complex_spectrum threshold_qr threshold 1024 1024 fFf ;;

let fFf12 = Matrix.matrix_float_triple_prod fFf6 fFf fFf5 ;;
let fFf13 = Reduc.matrix_complex_non_diagonal_part fFf12 ;;
let fFf14 = Matrix.matrix_float_norm_inf fFf13 ;;

(*
let methode_spectre = Reduc.direct_complex_spectrum threshold_qr threshold 8192 8192 ;;
*)
let methode_spectre = Reduc.direct_complex_spectrum threshold_qr threshold 1024 2222 ;;

(*
let meth_sp = Reduc.complex_diago_spectrum ( Reduc.indirect_complex_diagonalization 0.6 methode_ker threshold_qr ( sqrt min_float ) 128 8192 ;;
let meth_sp = Reduc.complex_diago_spectrum ( Reduc.indirect_complex_diagonalization 0.4 methode_ker threshold_qr ( sqrt min_float ) 128 8192 ;;
let meth_sp = Reduc.complex_diago_spectrum ( Reduc.indirect_complex_diagonalization 0.38 methode_ker threshold_qr ( sqrt min_float ) 128 8192 ;;
*)
let meth_sp = Reduc.complex_diago_spectrum ( Reduc.indirect_complex_diagonalization 10. methode_ker threshold_qr ( sqrt min_float ) 128 8192 ) ;;

let jdm = fun p m -> Reduc.jordan_decomposition Reduc.poly_complex_apply_matrix ( Reduc.lento_complex_jordan_decomposition_polynomial ( Reduc.poly_sci_mult_karatsuba 2 ) ( Reduc.poly_sci_brent_kung_hart_novocin_comp ( Reduc.poly_sci_mult_karatsuba 2 ) 4 ) p ) m ;;

Reduc.poly_complex_raw_roots methode_spectre jdm fff ;;
Reduc.poly_real_complex_raw_roots methode_spectre jdm f ;;

let x1 = Reduc.complex_companion ( Reduc.poly_real_to_complex Reduc.poly_real_x ) ;;
Reduc.poly_real_complex_raw_roots methode_spectre jdm Reduc.poly_real_x ;;
Reduc.complex_householder_step threshold_qr 0 x1 ;;
Reduc.poly_real_complex_raw_roots methode_spectre jdm Reduc.poly_real_x ;;

Reduc.poly_real_complex_raw_roots methode_spectre jdm x2 ;;

let xxx = Reduc.poly_real_from_roots Reduc.poly_real_mult [| 1. ; 2. ; 1. ; 2. |] ;;
let px0 = Reduc.lento_real_jordan_decomposition_polynomial Reduc.poly_sci_mult ( Reduc.poly_sci_brent_kung_hart_novocin_comp Reduc.poly_sci_mult 4 ) xxx ;;
let px1 = Reduc.lento_real_jordan_decomposition_polynomial Reduc.poly_sci_mult Reduc.poly_sci_horner_comp xxx ;;
let px2 = Reduc.real_jordan_decomposition_polynomial Reduc.poly_real_mult Reduc.poly_real_horner_comp xxx ;;

let x_x = Reduc.real_companion xxx ;;
let dn0 = Reduc.jordan_decomposition Reduc.poly_real_apply_matrix px0 x_x ;;
Matrix.matrix_float_prod dn0.(1) dn0.(1) ;;

let xXX = Reduc.poly_real_to_complex xxx ;;
let xxX = Reduc.complex_companion xXX ;;
let xXx = ( jdm xXX xxX ).(0) ;;
Matrix.float_power ( ( Array.length xxX ) / 2 ) ( jdm xXX xxX ).(1) ;;

let xXx0 = Reduc.direct_complex_diagonalization methode_ker threshold_qr threshold 128 128 xXx ;;
xXx0.(0) ;;
let xXx1 = xXx0.(1).(0) ;;

Reduc.poly_real_complex_raw_roots methode_spectre jdm xxx ;;

let methode_diag = Reduc.direct_complex_diagonalization methode_ker threshold_qr threshold 128 128 ;;

Reduc.poly_real_complex_raw_roots methode_spectre jdm x2 ;;

Reduc.poly_real_complex_raw_roots methode_spectre jdm f ;;
(*
Reduc.poly_real_complex_raw_roots meth_sp jdm f ;;
*)

let r0 = Reduc.poly_real_complex_raw_roots methode_spectre jdm xxx ;;
let r1 = Reduc.poly_real_complex_raw_roots meth_sp jdm xxx ;;

let derivative_array = [| xXX ; Reduc.poly_complex_deriv xXX |] ;;
(*
let thr = 1e222 *. min_float ;;
let thr = 1e2 *. min_float ;;
let thr = 1e1 *. epsilon_float ;;
let thr = 1e-33 *. epsilon_float ;;
let thr = 0. ;;
let thr = sqrt min_float ;;
*)
let thr = sqrt min_float ;;
let er0 = Reduc.poly_complex_evaluate Reduc.poly_complex_horner_comp ;;
let er1 = Reduc.poly_sci_evaluate Reduc.poly_sci_horner_comp ;;
let er2 = Reduc.poly_sci_1024_evaluate Reduc.poly_sci_1024_horner_comp ;;

let r2 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r0.(2) ;;
let r4 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r2.(0).(0).(0) ;;
let r6 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r4.(0).(0).(0) ;;
let r8 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r6.(0).(0).(0) ;;
let r10 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r8.(0).(0).(0) ;;

r2.(0).(0).(0) ;;
r2.(0).(0).(2).(0).(0) ;;
r2.(0).(0).(1).(0).(0) ;;
r4.(0).(0).(0) ;;
r4.(0).(0).(2).(0).(0) ;;
r4.(0).(0).(1).(0).(0) ;;
r6.(0).(0).(0) ;;
r6.(0).(0).(2).(0).(0) ;;
r6.(0).(0).(1).(0).(0) ;;
r8.(0).(0).(0) ;;
r8.(0).(0).(2).(0).(0) ;;
r8.(0).(0).(1).(0).(0) ;;
r10.(0).(0).(0) ;;
r10.(0).(0).(2).(0).(0) ;;
r10.(0).(0).(1).(0).(0) ;;

(*
let r3 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r1.(2) ;;
let r5 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r3.(0).(0).(0) ;;
let r7 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r5.(0).(0).(0) ;;
let r9 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r7.(0).(0).(0) ;;
let r11 = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr r9.(0).(0).(0) ;;

r3.(0).(0).(0) ;;
r3.(0).(0).(2).(0).(0) ;;
r3.(0).(0).(1).(0).(0) ;;
r5.(0).(0).(0) ;;
r5.(0).(0).(2).(0).(0) ;;
r5.(0).(0).(1).(0).(0) ;;
r7.(0).(0).(0) ;;
r7.(0).(0).(2).(0).(0) ;;
r7.(0).(0).(1).(0).(0) ;;
r9.(0).(0).(0) ;;
r9.(0).(0).(2).(0).(0) ;;
r9.(0).(0).(1).(0).(0) ;;
r11.(0).(0).(0) ;;
r11.(0).(0).(2).(0).(0) ;;
r11.(0).(0).(1).(0).(0) ;;
*)

Reduc.poly_complex_apply_matrix xXX Reduc.complex_1 ;;
let rr = Reduc.poly_complex_tune_root_step er0 xXX derivative_array thr Reduc.complex_1 ;;
rr.(0).(0).(0) ;;
rr.(0).(0).(2).(0).(0) ;;
rr.(0).(0).(1).(0).(0) ;;

let ca = methode_spectre ( Reduc.complex_companion ( Reduc.poly_real_to_complex f ) ) ;;
let mult_threshold = 1e-3 ;;

( Reduc.poly_real_complex_tune_roots er0 thr mult_threshold 1 ca f ).(0) ;;
( Reduc.poly_real_complex_tune_roots er0 thr mult_threshold 2 ca f ).(0) ;;
( Reduc.poly_real_complex_tune_roots er0 thr mult_threshold 3 ca f ).(0) ;;
( Reduc.poly_real_complex_tune_roots er0 thr mult_threshold 8 ca f ).(0) ;;

let cand = methode_spectre xXx ;;

( Reduc.poly_complex_tune_roots er0 thr mult_threshold 2 cand xXX ).(0) ;;
(*
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 3 cand xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 4 cand xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 8 cand xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 11 cand xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 22 cand xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 222 cand xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 1024 cand xXX ).(0) ;;
( Reduc.poly_real_complex_tune_roots er0 thr mult_threshold 44 cand xxx ).(0) ;;
*)

(*
let candi = meth_sp xXx ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 2 candi xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 3 candi xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 4 candi xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 5 candi xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 7 candi xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 11 candi xXX ).(0) ;;
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 1024 candi xXX ).(0) ;;
( Reduc.poly_real_complex_tune_roots er0 thr mult_threshold 4 candi xxx ).(0) ;;
*)

let methode_roots = Reduc.poly_complex_tune_roots er0 thr mult_threshold 128 ;;
let dm = Reduc.complex_diagonalization methode_ker threshold_qr 1e-3 128 128 methode_roots ;;

let xXx7 = dm xXX xXx ;;
let xXx8 = xXx7.(1).(0) ;;
let xXx9 = Matrix.clean_inv xXx8 ;;
let xXx10 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.eye_float fFf5 ) ( Matrix.matrix_float_prod xXx8 xXx9 ) ) ;;
let xXx11 = Matrix.matrix_float_triple_prod xXx9 xXx xXx8 ;;
let xXx12 = Reduc.matrix_complex_non_diagonal_part xXx11 ;;
let xXx13 = Matrix.matrix_float_norm_inf xXx12 ;;
xXx7.(0) ;;

(*
let odm = Reduc.complex_tune_diagonalization 6e-1 ( dm xXX ) ;;
let xXx14 = odm xXx xXx8 ;;
let xXx15 = xXx14.(1).(0) ;;
let xXx16 = Matrix.clean_inv xXx15 ;;
let xXx17 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.eye_float fFf5 ) ( Matrix.matrix_float_prod xXx15 xXx16 ) ) ;;
let xXx18 = Matrix.matrix_float_triple_prod xXx16 xXx xXx15 ;;
let xXx19 = Reduc.matrix_complex_non_diagonal_part xXx18 ;;
let xXx20 = Matrix.matrix_float_norm_inf xXx19 ;;
xXx14.(0) ;;
*)

let msp = Reduc.complex_diago_spectrum ( dm xXX ) ;;
let msp0 = Reduc.complex_diago_spectrum methode_diag ;;

msp xXx ;;
Reduc.complex_det msp xXx ;;
msp0 xXx ;;
Reduc.complex_det msp0 xXx ;;

(* 1e-9 *)
let candide = Reduc.poly_real_complex_raw_roots methode_spectre jdm xxx ;;
(* 3e-11 *)
(*
( Reduc.largo_poly_real_complex_tune_roots er2 1e-21 mult_threshold 6 candide xxx ).(0) ;;
*)
(* 1e-11 *)
(*
( Reduc.largo_poly_real_complex_tune_roots er2 1e-22 mult_threshold 7 candide xxx ).(0) ;;
*)

let methode_s = Reduc.direct_complex_spectrum threshold_qr threshold 11 1024 ;;

let xX_X = Reduc.poly_complex_pow ( Reduc.poly_complex_mult_karatsuba 2 ) 2 xXX ;;

(*
let meth_sp0 = Reduc.complex_diago_spectrum ( Reduc.indirect_complex_diagonalization 1e-44 methode_ker ( sqrt min_float ) 128 55 ) ;;
let meth_sp0 = Reduc.complex_diago_spectrum ( Reduc.indirect_complex_diagonalization 0. methode_ker 1e-10 1 1 ) ;;
let candidae = Reduc.poly_complex_raw_roots meth_sp0 jdm xX_X ;;
( Reduc.poly_complex_tune_roots thr mult_threshold 8192 candidae xX_X ).(0) ;;
*)

let x00 = Reduc.complex_companion xX_X ;;

let mk0 = Matrix.float_ker methode_reduc thr ;;
(* 1e-3 *)
let candid = ( Reduc.direct_complex_diagonalization methode_ker threshold_qr 1e-30 1024 8192 x00 ).(0) ;;
(* 3.5e-4 *)
(*
let candidat = ( Reduc.poly_complex_tune_roots er0 thr mult_threshold 5 candid xX_X ).(0) ;;
*)
(* 1.2e-4 *)
(*
( Reduc.largo_poly_complex_tune_roots er2 1e-18 mult_threshold 2 candidat.(0) xX_X ).(0) ;;
*)

let msp1 = Reduc.direct_complex_spectrum threshold_qr thr 8192 8192 ;;
(* 2e-4 *)
let candidate = Reduc.poly_complex_raw_roots msp1 jdm xX_X ;;
(* 3.7e-4 *)
(*
( Reduc.poly_complex_tune_roots er0 thr mult_threshold 2 candidate xX_X ).(0) ;;
*)
(* 8e-5 *)
(*
( Reduc.largo_poly_complex_tune_roots er2 1e-16 mult_threshold 2 candidate xX_X ).(0) ;;
*)

(* 2e-4 *)
let candidates = Reduc.poly_complex_raw_roots methode_spectre jdm xX_X ;;
(* 3.3e-4 *)
(*
let seta = ( Reduc.poly_complex_tune_roots er0 thr mult_threshold 2 candidates xX_X ).(0) ;;
*)
(* 7.6e-5 *)
(*
let setad = ( Reduc.largo_poly_complex_tune_roots er2 1e-17 mult_threshold 2 seta.(0) xX_X ).(0) ;;
*)
(* 3.5e-5 *)
(*
let setadi = ( Reduc.largo_poly_complex_tune_roots er2 1e-19 mult_threshold 2 setad.(0) xX_X ).(0) ;;
*)
(* 8.4e-6 *)
(*
( Reduc.largo_poly_complex_tune_roots er2 1e-20 mult_threshold 2 setadi.(0) xX_X ).(0) ;;
*)

let meth_roots = Reduc.lento_poly_complex_tune_roots er1 1e-31 mult_threshold 2 ;;
let diag_meth = Reduc.complex_diagonalization methode_ker threshold_qr 1e-3 128 128 meth_roots ;;

let xXx21 = diag_meth xXX xXx ;;
xXx21.(0) ;;

(* 2e-4 *)
(*
let setadid = Reduc.poly_complex_raw_roots ( Reduc.complex_diago_spectrum ( diag_meth xX_X ) ) jdm xX_X ;;
*)
(* 7e-5 *)
(*
let setadidn = ( Reduc.largo_poly_complex_tune_roots er2 1e-16 mult_threshold 2 setadid xX_X ).(0) ;;
*)
(* 5e-5 *)
(*
let setadidna = ( Reduc.largo_poly_complex_tune_roots er2 1e-17 mult_threshold 2 setadidn.(0) xX_X ).(0) ;;
*)
(* 1.7e-5 *)
(*
let setadidnac =( Reduc.largo_poly_complex_tune_roots er2 1e-19 mult_threshold 2 setadidna.(0) xX_X ).(0) ;;
*)

let cx = Array.map Sci.sci_of_complex candidate ;;
let pcx = Array.map Sci.sci_of_complex xX_X ;;
(* 2e-8 *)
(*
let rx = Reduc.poly_aitken_seki_1024_tune_roots 0. mult_threshold cx pcx ;;
let rxr = Array.map Sci.complex_of_sci rx.(0).(0) ;;
Array.map Sci.print_sci_1024_10 rx.(0).(0) ;;
*)
(* 6.7e-9 *)
(*
let rxrx0 = Reduc.poly_brezinski_1024_tune_roots 2 0. mult_threshold cx pcx ;;
let rxrxr0 = Array.map Sci.complex_of_sci rxrx0.(0).(0) ;;
Array.map Sci.print_sci_1024_10 rxrx0.(0).(0) ;;
*)
(* 6e-13 *)
(*
let rxx = Reduc.poly_shanks2_1024_tune_roots 0. mult_threshold cx pcx ;;
let rxxr = Array.map Sci.complex_of_sci rxx.(0).(0) ;;
Array.map Sci.print_sci_1024_10 rxx.(0).(0) ;;
*)
(* 2.3e-13 *)
(*
let rx0 = Reduc.poly_aitken_seki_rec_1024_tune_roots 2 0. mult_threshold cx pcx ;;
let rxr0 = Array.map Sci.complex_of_sci rx0.(0).(0) ;;
Array.map Sci.print_sci_1024_10 rx0.(0).(0) ;;
*)

let fFf15 = Reduc.complex_diago_spectrum ( Reduc.indirect_complex_diagonalization 10. methode_ker threshold_qr threshold 1024 1024 ) fFf ;;
let fFf16 = Reduc.matrix_complex_extract_diag_to_poly ( Reduc.complex_francis_schur_decomposition 0. 0. 7 fFf ).(0) ;;

(*
let accel = Matrix.vector_float_approx ;;
let accel = Matrix.vector_float_approx_bis ;;
let accel = Matrix.vector_float_aitken_seki_rec 2 0 ;;
let accel = Matrix.vector_float_wynn 2 1 ;;
let accel = Matrix.vector_float_wynn_rho 2 1 ;;
let accel = Matrix.vector_float_brezinski 2 0 ;;
*)

let accel1 = Matrix.vector_float_approx_bis ;;
let accel2 = Matrix.vector_float_approx ;;

let fFf17 = Reduc.complex_francis_spectrum_seq 0. 0. 23 fFf ;;
(*
Matrix.vector_float_aitken_seki fFf17.(0) fFf17.(1) fFf17.(2) ;;
*)
let fFf18 = Reduc.complex_compensated_francis_spectrum accel1 0. 0. 7 fFf ;;

let fFf19 = Reduc.direct_complex_compensated_spectrum accel1 5 0.5 methode_ker 0. 0. 7 128 fFf ;;
let fFf20 = Reduc.direct_complex_compensated_spectrum accel2 5 0.5 methode_ker 0. 0. 7 128 fFf ;;
let fFf21 = Reduc.direct_complex_compensated_spectrum accel1 5 0.58 methode_ker 0. 0. 7 128 fFf ;;
let fFf22 = Reduc.direct_complex_compensated_spectrum accel2 5 0.58 methode_ker 0. 0. 7 128 fFf ;;
fFf11;;
let fFf23 = Reduc.complex_compensated_spectrum accel1 7 0.5 0. 0. 7 128 fFf ;;
let fFf24 = Reduc.complex_compensated_spectrum accel2 7 0.57 0. 0. 7 128 fFf ;;
let fFf25 = Reduc.complex_compensated_spectrum accel1 7 0.58 0. 0. 7 128 fFf ;;
let fFf26 = Reduc.complex_compensated_spectrum accel2 7 0.58 0. 0. 7 128 fFf ;;
let fFf27 = Reduc.complex_compensated_spectrum accel2 7 0.59 0. 0. 7 128 fFf ;;

let accel3 = Sci.approx_1024 ;;
let fFf28 = Reduc.largo_complex_compensated_spectrum accel3 5 0.579 0. 0. 7 128 fFf ;;

let fFf29 = Reduc.clean_complex_spectrum 3 110 fFf ;;
(*
let fFf30 = Reduc.clean_complex_spectrum 7 110 fFf ;;
let fFf31 = Reduc.clean_complex_spectrum 9 122 fFf ;;
*)

Reduc.naive_complex_roots er0 1 0 11 threshold mult_threshold Reduc.poly_complex_x ;;
Reduc.naive_complex_roots er0 2 0 11 threshold mult_threshold Reduc.poly_complex_x ;;
Reduc.naive_complex_roots er0 1 0 11 threshold mult_threshold fff ;;
Reduc.naive_complex_roots er0 1 0 11 threshold mult_threshold xX_X ;;
Reduc.naive_complex_roots er0 1 0 128 threshold mult_threshold xX_X ;;
Reduc.naive_complex_roots er0 2 0 7 threshold mult_threshold xX_X ;;


let fFf32 = Reduc.complex_krylov_reduction Reduc.poly_complex_apply_matrix fff0 methode_diag fFf ;;
let fFf33 = fFf32.(0) ;;
let fFf34 = Matrix.matrix_float_triple_prod fFf32.(1) fFf33 fFf32.(2) ;;
let fFf35 = Matrix.matrix_float_triple_prod fFf32.(2) fFf fFf32.(1) ;;
let fFf36 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus fFf34 fFf ) ;;
let fFf37 = fFf32.(3) ;;
let fFf38 = fFf32.(4) ;;
let fFf39 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus fFf35 fFf33 ) ;;

let xxX0 = Reduc.lento_complex_jordan_decomposition_polynomial ( Reduc.poly_sci_mult_karatsuba 2 ) ( Reduc.poly_sci_brent_kung_hart_novocin_comp ( Reduc.poly_sci_mult_karatsuba 2 ) 4 ) xXX ;;
let xxX1 = Reduc.complex_krylov_reduction Reduc.poly_complex_apply_matrix xxX0 methode_diag xxX ;;
let xxX2 = xxX1.(0) ;;
let xxX3 = Matrix.matrix_float_triple_prod xxX1.(1) xxX2 xxX1.(2) ;;
let xxX4 = Matrix.matrix_float_triple_prod xxX1.(2) xxX xxX1.(1) ;;
let xxX5 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus xxX3 xxX ) ;;
let xxX6 = xxX1.(3) ;;
let xxX7 = xxX1.(4) ;;
let xxX8 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus xxX4 xxX2 ) ;;
Matrix.float_nilpotence_order Matrix.matrix_float_norm_inf 1e-6 xxX7 ;;
let xxX9 = Matrix.matrix_float_norm_inf xxX7 ;;
let xxX10 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_prod xxX7 xxX7 ) ;;
xxX10 /. xxX9 ;;


Reduc.newton_float_binom_coeff Reduc.poly_real_mult 5 ;;
Reduc.newton_complex_binom_coeff Reduc.poly_complex_mult 5 ;;

let u0 = Reduc.matrix_unitary_random 3 2. ;;
let u1 = Matrix.matrix_float_twisted_prod u0 u0 ;;
Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.identity_float 6 6 ) u1 ) ;;
Reduc.matrix_complex_anti_herm_random 3 3. ;;

Reduc.complex_random 3. ;;
Reduc.poly_complex_random 3 3. ;;
Reduc.poly_complex_unitary_random 3 3. ;;

let seq0 = [| 1.00093397534851669 ; 1.0003103531 ; 1.00010334391 ; 1.0000344361 ; 1.00001147738 |] ;;
let seq1 = Reduc.float_euler_sum_sequence Reduc.poly_real_mult seq0 ;;
let seq2 = [| 0.9 ; 1.05 ; 0.975 ; 1.0125 ; 0.99375 |] ;;
let seq3 = Reduc.float_euler_sum_sequence Reduc.poly_real_mult seq2 ;;
let seq4 = [| 1. ; -1. ; 1. ; -1. ; 1. ; -1. ; 1. |] ;;
let seq5 = Reduc.float_euler_sum_sequence Reduc.poly_real_mult seq4 ;;
let seq5a = Reduc.float_euler_sum_series Reduc.poly_real_mult seq4 ;;

let seq6 = [| [|[|1.00093397534851669; 1.88849435087711631e-21|] ; [|-1.88849435087711631e-21; 1.00093397534851669|]|] ;
 [|[|1.0003103531; 6.25561639667e-22|] ; [|-6.25561639667e-22; 1.0003103531|]|] ;
 [|[|1.00010334391; 2.08088496508e-22|] ; [|-2.08088496508e-22; 1.00010334391|]|] ;
 [|[|1.0000344361; 6.93150212666e-23|] ; [|-6.93150212666e-23; 1.0000344361|]|] ;
 [|[|1.00001147738; 2.30997019396e-23|] ; [|-2.30997019396e-23; 1.00001147738|]|] |] ;;
let seq7 = Reduc.complex_euler_sum_sequence Reduc.poly_complex_mult seq6 ;;


let h0 = 1e-1 ;;
let sol0 = [| 1. +. h0 ; 1. ; 1. -. h0 |] ;;
let p1 = Reduc.poly_real_from_roots Reduc.poly_real_mult sol0 ;;
let p2 = Reduc.poly_real_pow Reduc.poly_real_mult 2 p1 ;;
let p3 = Reduc.poly_real_to_complex p1 ;;
let p4 = Reduc.poly_real_to_complex p2 ;;
let p5 = Reduc.poly_complex_to_sci p3 ;;
let p6 = Reduc.poly_sci_1024_pow Reduc.poly_sci_1024_mult 2 p5 ;;
(*
let p7 = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots 1 2 7 1e-30 p5 ) ;;
*)
let p8 = Reduc.simple_complex_roots er0 1 0 11 1e-10 p3 ;;
let p8a = Reduc.simple_direct_complex_roots er0 0 11 1e-20 1e-10 p3 ;;
let pA = Reduc.naive_sci_1024_roots er2 1 0 7 1e-30 1e-5 p5 ;;
let pB = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 1 0 11 1e-300 1e-5 p5 ) ;;
let pC = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 1 0 1024 1e-110 1e-5 p6 ) ;;
let pD = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 1 0 8192 1e-200 1e-5 p6 ) ;;
let pE = Reduc.naive_complex_roots er0 1 0 1024 1e-20 1e-5 p3 ;;
let pF = Reduc.naive_complex_roots er0 3 1 1024 1e-20 1e-5 p4 ;;
let pG = Reduc.direct_complex_roots er0 0 1024 1e-20 1e-10 1e-5 p4 ;;

let h1 = 1e-2 ;;
let sol1 = [| 1. +. h1 ; 1. |] ;;
let p11 = Reduc.poly_real_from_roots Reduc.poly_real_mult sol1 ;;
let p12 = Reduc.poly_real_pow Reduc.poly_real_mult 2 p11 ;;
let p13 = Reduc.poly_real_to_complex p11 ;;
let p14 = Reduc.poly_real_to_complex p12 ;;
let p15 = Reduc.poly_complex_to_sci p13 ;;
let p16 = Reduc.poly_sci_1024_pow Reduc.poly_sci_1024_mult 2 p15 ;;
let p17 = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots er2 1 2 11 1e-10 p15 ) ;;
let p18 = Reduc.poly_sci_to_complex ( Reduc.simple_direct_sci_1024_roots er2 1 11 1e-30 1e-10 p15 ) ;;
let p19 = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 1 0 11 1e-30 1e-5 p15 ) ;;
let p1A = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 2 0 11 1e-30 1e-5 p15 ) ;;
let p1B = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 2 0 1024 1e-50 1e-7 p16 ) ;;
let p1C = Array.map Reduc.poly_sci_to_complex ( Reduc.direct_sci_1024_roots er2 0 1024 1e-10 1e-20 1e-7 p16 ) ;;
(*
let p1D = Array.map Reduc.poly_sci_to_complex ( Reduc.direct_sci_1024_roots er2 1 1024 1e-10 1e-20 1e-7 p16 ) ;;
*)
let p1E = Reduc.naive_complex_roots er0 1 0 1024 1e-20 1e-5 p13 ;;
let p1F = Reduc.naive_complex_roots er0 1 0 8192 1e-70 1e-5 p14 ;;
let p1G = Reduc.complex_shifted_roots er0 1 0 1024 1e-100 1e-4 [|[| 1.01 ; -0.01 |] ; [| 0.01 ; 1.01 |]|] p14 ;;

let h2 = 1e-4 ;;
let sol2 = [| 1. +. h2 ; 1. ; 1. -. h2 |] ;;
let p21 = Reduc.poly_real_from_roots Reduc.poly_real_mult sol2 ;;
let p22 = Reduc.poly_real_pow Reduc.poly_real_mult 2 p21 ;;
let p23 = Reduc.poly_real_to_complex p21 ;;
let p24 = Reduc.poly_real_to_complex p22 ;;
let p25 = Reduc.poly_complex_to_sci p23 ;;
let p26 = Reduc.poly_sci_1024_pow Reduc.poly_sci_1024_mult 2 p25 ;;
let p27 = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots er2 1 0 8192 0. p25 ) ;;
let p28 = Reduc.poly_sci_to_complex ( Reduc.simple_direct_sci_1024_roots er2 1 8192 1e-20 0. p25 ) ;;
let p29 = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots er2 3 0 8192 0. p25 ) ;;
let p2A = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots er2 4 0 8192 0. p25 ) ;;
(*
let p2B = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots er2 1 1 8192 0. p25 ) ;;
let p2C = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots er2 1 2 8192 0. p25 ) ;;
let p2D = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots er2 3 3 8192 0. p25 ) ;;
let p2E = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots er2 4 3 8192 0. p25 ) ;;
*)
let p2F = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 1 0 8192 1e-30 1e-5 p25 ) ;;
(*
let p2G = Array.map Reduc.poly_sci_to_complex ( Reduc.sci_1024_shifted_roots er2 2 0 131072 0. 1e-5 Reduc.complex_1 p26 ) ;;
let p2H = Array.map Reduc.poly_sci_to_complex ( Reduc.direct_sci_1024_shifted_roots er2 0 131072 1e-70 1e-20 1e-5 Reduc.complex_1 p26 ) ;;
let p2I = Array.map Reduc.poly_sci_to_complex ( Reduc.direct_sci_1024_shifted_roots er2 1 128 0. 0. 1e-8 [|[|1.00004636997; -0.0493380011738|]; [|0.0493380011738; 1.00004636997|]|] p26 ) ;;
*)
let p2J = Reduc.naive_complex_roots er0 1 0 1024 1e-20 1e-5 p23 ;;
let p2K = Reduc.naive_complex_roots er0 2 0 8192 1e-20 1e-5 p23 ;;
(*
let p2L = Reduc.direct_complex_shifted_roots er0 1 128 0. 0. 1e-11 [|[|1.00004636997; -0.0493380011738|]; [|0.0493380011738; 1.00004636997|]|] p24 ;;
let p2M = Reduc.simple_complex_shifted_roots er0 2 0 8192 0. [|[|1.00004636997; -0.0493380011738|]; [|0.0493380011738; 1.00004636997|]|] p24 ;;
let p2N = Reduc.simple_complex_shifted_roots er0 2 0 8192 0. [|[|1.; -0.000005|]; [|0.000005; 1.|]|] p24 ;;
let p2P = Reduc.simple_direct_complex_shifted_roots er0 1 8192 0. 0. [|[|1.; -0.000005|]; [|0.000005; 1.|]|] p24 ;;
let p2Q = Reduc.complex_shifted_roots er0 1 0 8192 0. 1e-8 [| [| 1.00005; -0.000005 |]; [| 0.000005; 1.00005 |] |] p24 ;;
let p2R = Reduc.complex_shifted_roots er0 1 2 8192 0. 1e-9 [| [| 1.00005; -0.000005 |]; [| 0.000005; 1.00005 |] |] p24 ;;
*)

let sol3 = [| 1. +. 2. *.h0 ; 1. +. h0 ; 1. ; 1. -. h0 ; 1. -. 2. *. h0 |] ;;
let p31 = Reduc.poly_real_from_roots Reduc.poly_real_mult sol3 ;;
let p32 = Reduc.poly_real_pow Reduc.poly_real_mult 2 p31 ;;
let p33 = Reduc.poly_real_to_complex p31 ;;
let p34 = Reduc.poly_real_to_complex p32 ;;
let p35 = Reduc.poly_complex_to_sci p33 ;;
let p36 = Reduc.poly_sci_1024_pow Reduc.poly_sci_1024_mult 2 p35 ;;
(*
let p37 = Reduc.poly_sci_to_complex ( Reduc.simple_sci_1024_roots 1 2 7 1e-30 p35 ) ;;
*)
let p38 = Reduc.simple_complex_roots er0 1 0 11 1e-10 p33 ;;
let p39 = Reduc.simple_direct_complex_roots er0 0 11 1e-20 1e-10 p33 ;;
let p3A = Reduc.naive_sci_1024_roots er2 1 0 7 1e-30 1e-5 p35 ;;
let p3B = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 1 0 11 1e-300 1e-5 p35 ) ;;
let p3C = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 1 0 1024 1e-110 1e-7 p36 ) ;;
(*
let p3D = Array.map Reduc.poly_sci_to_complex ( Reduc.naive_sci_1024_roots er2 1 0 8192 1e-200 1e-7 p36 ) ;;
*)
let p3E = Reduc.naive_complex_roots er0 1 0 1024 1e-20 1e-5 p33 ;;
let p3F = Reduc.naive_complex_roots er0 3 1 1024 1e-20 1e-7 p34 ;;
let p3G = Reduc.direct_complex_roots er0 0 1024 1e-20 1e-10 1e-7 p34 ;;

let p41 = Reduc.naive_complex_roots er0 1 0 128 1e-20 1e-4 xX_X ;;
let p42 = Reduc.naive_complex_roots er0 1 0 128 1e-90 1e-4 fff ;;
let p43 = Reduc.naive_complex_roots er0 5 1 128 1e-90 1e-4 fff ;;

Reduc.real_sylvester_matrix p21 p21 ;;
Reduc.real_sylvester_matrix p21 p31 ;;
let m0 = Reduc.real_sylvester_matrix Reduc.poly_real_x [| 2. |] ;;
let m1 = Reduc.real_sylvester_matrix p21 ( Reduc.poly_real_deriv p21 ) ;;
Matrix.float_invertibility_evaluation m1 ;;
Reduc.real_discriminant Matrix.float_slow_det p21 ;;
Reduc.real_discriminant Matrix.float_slow_det p31 ;;
let m2 = Reduc.real_sylvester_matrix [| 1. ; 1. ; 1. |] [| 1. ; 2. |] ;;
Reduc.real_discriminant Matrix.float_slow_det [| 1. ; 1. ; 1. |] ;;
Reduc.real_discriminant_bis Reduc.leverrier_real_char_poly [| 1. ; 1. ; 1. |] ;;

Reduc.int_discriminant Matrix.int_slow_det [| 1 ; 1 ; 1 |] ;;
Reduc.int_discriminant_bis Reduc.leverrier_int_char_poly [| 1 ; 1 ; 1 |] ;;

let m3 = Reduc.complex_sylvester_matrix p23 p23 ;;
Reduc.complex_discriminant ( Reduc.complex_det msp1 ) p23 ;;
let m4 = Reduc.complex_sylvester_matrix [| Reduc.complex_1 ; Reduc.complex_1 ; Reduc.complex_1 |] [| Reduc.complex_1 ; Reduc.float_to_complex 2. |] ;;
Reduc.complex_discriminant ( Reduc.complex_det msp1 ) [| Reduc.complex_1 ; Reduc.complex_1 ; Reduc.complex_1 |] ;;
Reduc.complex_discriminant ( Reduc.complex_poly_det ( Reduc.leverrier_complex_char_poly m4 ) ) [| Reduc.complex_1 ; Reduc.complex_1 ; Reduc.complex_1 |] ;;
Reduc.complex_discriminant_bis Reduc.leverrier_complex_char_poly [| Reduc.complex_1 ; Reduc.complex_1 ; Reduc.complex_1 |] ;;
let m5 = Reduc.gauss_sylvester_matrix [| Reduc.gauss_1 ; Reduc.gauss_1 ; Reduc.gauss_1 |] [| Reduc.gauss_1 ; Reduc.int_to_gauss 2 |] ;;
Reduc.gauss_discriminant ( Reduc.gauss_det msp1 ) [| Reduc.gauss_1 ; Reduc.gauss_1 ; Reduc.gauss_1 |] ;;
Reduc.gauss_discriminant ( Reduc.gauss_poly_det ( Reduc.leverrier_gauss_char_poly m5 ) ) [| Reduc.gauss_1 ; Reduc.gauss_1 ; Reduc.gauss_1 |] ;;
Reduc.gauss_discriminant_bis Reduc.leverrier_gauss_char_poly [| Reduc.gauss_1 ; Reduc.gauss_1 ; Reduc.gauss_1 |] ;;


let s0 = Reduc.complex_shifted_roots er0 1 0 1024 1e-70 1e-4 Reduc.complex_1 xX_X ;;
let s1 = Reduc.demultip s0 ;;

let x0 = Reduc.poly_complex_from_roots Reduc.poly_complex_mult s1 ;;
Reduc.vector_complex_norm_inf ( Reduc.poly_complex_minus x0 xX_X ) ;;


let r20 = [| Sci.sci_1 ; Sci.sci_05 ; Sci.sci_025 ; Sci.sci_0125 ; Sci.sci_2_pow_minus_4 ; Sci.sci_2_pow_minus_5 ; Sci.sci_2_pow_minus_6 |] ;;
let r21 = Array.map ( Sci.mult Sci.sci_2_pow_minus_7 ) r20 ;;
let r22 = Array.append r20 r21 ;;
Array.map Sci.float_of_sci r22 ;;
Array.map Sci.complex_of_sci r22 ;;

(*
L'article
http:www.cs.iastate.edu/~cs577/handouts/polyroots.pdf
donne une erreur absolue d'environ 1. pour la m\'ethode classique 
et 4e-11 pour la m\'ethode de Maehly pour le polynôme pp1.
*)

let pp0 = Reduc.poly_sci_1024_from_roots Reduc.poly_sci_1024_mult r20 ;;
let pp00 = Reduc.naive_sci_1024_roots er2 1 1 55 0. 1e-5 pp0 ;;
Array.map Sci.complex_of_sci pp00.(0) ;;
let pp01 = Array.map Sci.complex_of_sci pp0 ;;

let pp1 = Reduc.poly_sci_1024_from_roots Reduc.poly_sci_1024_mult r22 ;;

(*
let pp11 = Reduc.naive_sci_1024_roots er2 1 1 111 0. 1e-5 pp1 ;;
Array.map Sci.complex_of_sci pp11.(0) ;;
Array.map Sci.float_of_sci pp11.(0) ;;0
let pp12 = Reduc.simple_sci_1024_roots er2 1 1 111 0. pp1 ;;
Array.map Sci.complex_of_sci pp12 ;;
Array.map Sci.float_of_sci pp12 ;;
*)

let dpp01 = Reduc.poly_complex_deriv pp01 ;;
let pp01a = Reduc.simple_maehly_complex_roots Reduc.poly_complex_mult er0 2 0 111 1e-11 [| |] pp01 dpp01 ;;

(*
let dpp0 = Reduc.poly_sci_1024_deriv pp0 ;;
let pp02 = Reduc.simple_maehly_sci_1024_roots Reduc.poly_sci_1024_mult er2 2 0 111 0. [| |] pp0 dpp0 ;;
Array.map Sci.float_of_sci pp02 ;;
*)

let dpp1 = Reduc.poly_sci_1024_deriv pp1 ;;
let cmp = fun x y -> compare ( Reduc.complex_module ( Sci.complex_of_sci y ) ) ( Reduc.complex_module ( Sci.complex_of_sci x ) ) ;;

let pp1a = Reduc.poly_sci_to_complex pp1 ;;
Reduc.largo_complex_roots 1e-30 pp1a ;;

let pp13 = Reduc.simple_maehly_sci_1024_roots Reduc.poly_sci_1024_mult er2 2 0 111 min_float [| |] pp1 dpp1 ;;
Array.sort cmp pp13 ;;
Array.map Sci.print_sci_1024_10 pp13 ;;
Array.mapi ( fun i x -> Reduc.complex_abs_max ( Sci.complex_of_sci ( Sci.minus_1024 pp13.(i) x ) ) ) r22 ;;
Array.map Sci.float_of_sci pp13 ;;
Array.map Sci.complex_of_sci pp13 ;;

let p9 = Array.make 10 0. ;;
p9.(0) <- 1. ;;
p9.(9) <- 1. ;;
let q9 = Reduc.poly_real_to_complex p9 ;;
let cube = function x -> Matrix.matrix_float_triple_prod x x x ;;
let s9 = Reduc.complex_roots 1e-10 q9 ;;
Array.map cube ( Array.map cube s9 ) ;;

