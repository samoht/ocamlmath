module K = Data2.Sci_1024_coeff ;;
module C = Data2.C_coeff ;;

module M = Mat.Field (K) ;;
module N = Mat.Field (C) ;;

module F = Fft.Field (K) ;;

let omega3 = Sci.sci_1024_j ;;
let omega9 = Sci.sci_1024_primitive_root_of_unity 9 ;;
let omega6 = Sci.sci_1024_primitive_root_of_unity 6 ;;

let omega2 = Sci.sci_1024_primitive_root_of_unity 2 ;;
let omega4 = Sci.sci_1024_primitive_root_of_unity 4 ;;
let omega8 = Sci.sci_1024_primitive_root_of_unity 8 ;;
let powers = [| K.one () ; omega8 ; omega4 ; K.mult omega8 omega4 ; omega2 |] ;;
let inv_powers = Array.map K.inv powers ;;

let t3 = F.dft omega9 9 ;;
let t3a = Array.map ( Array.map Sci.complex_of_sci ) ( M.matrix_full_demakeup t3 ) ;;
let t3b = N.matrix_map ( Reduc.apply_built_in_complex_to_matrix Complex.log ) ( N.Full_matrix t3a ) ;;
let t3b0 = Array.map ( Array.map Reduc.complex_real_part ) ( N.matrix_full_demakeup t3b ) ;;
let t3b1 = Array.map ( Array.map Reduc.complex_imaginary_part ) ( N.matrix_full_demakeup t3b ) ;;

let t3i = F.primary_fft_matrix omega3 3 omega9 2 ;;
let t3j = Array.map ( Array.map Sci.complex_of_sci ) ( M.matrix_full_demakeup ( M.matrix_to_full t3i ) ) ;;
let t3k = Array.map ( Array.map ( Reduc.apply_built_in_complex_to_matrix Complex.log ) ) t3j ;;
let t3k0 = Array.map ( Array.map Reduc.complex_real_part ) t3k ;;
let t3k1 = Array.map ( Array.map Reduc.complex_imaginary_part ) t3k ;;
let t3l = Matrix.matrix_float_minus t3b1 t3k1 ;;

let t4 = F.binary_fft_matrix omega8 3 ;;
let t4a =  Array.map ( Array.map Sci.complex_of_sci ) ( F.M.matrix_full_demakeup t4 ) ;;
let t5 = F.inverse_binary_fft_matrix omega8 3 ;;
let t5a =  Array.map ( Array.map Sci.complex_of_sci ) ( F.M.matrix_full_demakeup t5 ) ;;
let t6 = N.matrix_mult ( N.Full_matrix t4a ) ( N.Full_matrix t5a ) ;;

let s0 = F.binary_twist_fft_vector omega8 3 ;;
let s0a = Array.map Sci.complex_of_sci ( F.M.vector_full_demakeup s0 ) ;;

let s1 = F.binary_twisted_fft_matrix omega8 3 omega9 ;;
let s1a = Array.map ( Array.map Sci.complex_of_sci ) ( M.matrix_full_demakeup s1 ) ;;
let s2 = F.primary_twist_fft_vector 3 omega9 2 ;;
let s2a = Array.map Sci.complex_of_sci ( M.vector_full_demakeup s2 ) ;;
let s3 = F.primary_twisted_fft_matrix omega3 3 omega9 2 omega8 ;;
let s3a = Array.map ( Array.map Sci.complex_of_sci ) ( M.matrix_full_demakeup s3 ) ;;

let void = [| F.M.matrix_zero () |] ;;
let s4 = Util.primo ( F.vector_binary_fft 2 omega8 3 powers void s0 ) ;;
let s4a = F.M.matrix_vector_prod t4 s0 ;;
let s4b = F.M.vector_sub s4a s4 ;;
let s4c = Array.map Sci.complex_of_sci ( M.vector_full_demakeup s4b ) ;;

let id8 = F.M.identity_matrix [| 8 ; 8 |] ;;
let id8b = Array.map ( Array.map Sci.complex_of_sci ) ( F.M.matrix_full_demakeup ( F.M.matrix_to_full id8 ) ) ;;
let e0 = Array.map ( function i -> F.M.matrix_row_extract i id8 ) [| 0 ; 1 ; 2 ; 3 ; 4 ; 5 ; 6 ; 7 |] ;;
let e0a = Array.map ( function x -> Array.map Sci.complex_of_sci ( F.M.vector_full_demakeup ( F.M.vector_to_full x ) ) ) e0 ;;
let e0b = Array.map ( function x -> Util.primo ( F.vector_binary_fft 2 omega8 3 powers void x ) ) e0 ;;
let e0c = Util.transpose ( Array.map ( function x -> Array.map Sci.complex_of_sci ( F.M.vector_full_demakeup ( F.M.vector_to_full x ) ) ) e0b ) ;;
let e0d = N.matrix_sub ( N.Full_matrix t4a ) ( N.Full_matrix e0c ) ;;
let e0e = N.matrix_norm_1 e0d ;;

let e1 = Array.map ( function x -> Util.primo ( F.vector_inverse_binary_fft 2 omega8 3 inv_powers void x ) ) e0 ;;
let e1a = Util.transpose ( Array.map ( function x -> Array.map Sci.complex_of_sci ( F.M.vector_full_demakeup ( F.M.vector_to_full x ) ) ) e1 ) ;;
let e1b = N.matrix_mult ( N.Full_matrix e1a ) ( N.Full_matrix e0c ) ;;
let e1c = N.matrix_sub e1b ( N.Full_matrix e0a ) ;;
let e1d = N.matrix_norm_1 e1c ;;
let e1e = N.matrix_sub ( N.Full_matrix e1a ) ( N.Full_matrix t5a ) ;;
let e1f = N.matrix_norm_1 e1e ;;
