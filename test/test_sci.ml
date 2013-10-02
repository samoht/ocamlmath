
Sci.print_sci_1024_10 Sci.sci_epsilon_float ;;
Sci.print_sci_1024_10 Sci.sci_min_float ;;
Sci.print_sci_1024_10 Sci.sci_max_float ;;
Sci.print_sci_1024_10 Sci.sci_min_int ;;
Sci.print_sci_1024_10 Sci.sci_max_int ;;

let a = Sci.format [| Sci.num_2 ; Sci.num_1 ; Sci.num_0 |] ;;
Sci.float_of_sci a ;;
let b = Sci.format [| Sci.num_1 ; Sci.num_2 ; Sci.num_0 |] ;;
Sci.float_of_sci b ;;
let c = Sci.format [| Sci.num_4 ; Sci.num_0 ; Sci.num_4 |] ;;
Sci.float_of_sci c ;;
let d = Sci.format [| Sci.num_2 ; Sci.num_1 ; Num.minus_num Sci.num_1 |] ;;
Sci.float_of_sci d ;;
let e = Sci.format [| Sci.num_1 ; Sci.num_2 ; Num.minus_num Sci.num_1 |] ;;
Sci.float_of_sci e ;;

let m = Sci.square_module a ;;
Sci.float_of_sci m ;;
Sci.complex_of_sci m ;;

let n = Sci.plus a b ;;
Sci.float_of_sci n ;;
Sci.complex_of_sci n ;;

let o = Sci.plus b a ;;
Sci.float_of_sci o ;;
Sci.complex_of_sci o ;;

let p = Sci.plus a d ;;
Sci.float_of_sci p ;;
Sci.complex_of_sci p ;;

let q = Sci.minus a d ;;
Sci.float_of_sci q ;;
Sci.complex_of_sci q ;;

let r = Sci.mult a d ;;
Sci.float_of_sci r ;;
Sci.complex_of_sci r ;;

let s = Sci.div a d ;;
Sci.float_of_sci s ;;
Sci.complex_of_sci s ;;

Sci.eq r s ;;

let t = Sci.int_pow 800 a ;;
Sci.float_of_sci t ;;
Sci.complex_of_sci t ;;

Sci.print_num_fact ( Num.num_of_int 69 ) ;;
let u = Sci.sci_fact ( Num.num_of_int 69 ) ;;
Sci.float_of_sci u ;;
Sci.complex_of_sci u ;;

Sci.num_show_pi 50 ;;
let pi = Sci.sci_approx_pi 50 ;;
Sci.float_of_sci pi ;;
Sci.complex_of_sci pi ;;
let ipi = Sci.sci_approx_ipi 50 ;;
Sci.float_of_sci ipi ;;
Sci.complex_of_sci ipi ;;

Num.string_of_num Sci.num_2_pow_1024 ;;

Num.string_of_num Sci.num_epsilon_float ;;

Num.string_of_num ( Sci.num_of_float 1. ) ;;
Num.string_of_num ( Sci.num_of_float 100. ) ;;
Num.string_of_num ( Sci.num_of_float 1.23 ) ;;
Num.string_of_num ( Sci.num_of_float 100.23 ) ;;

let v = Sci.sci_of_complex [| [| 1. ; 2. |] ; [| -2. ; 1. |] |] ;;
Sci.float_of_sci v ;;
Sci.complex_of_sci v ;;

let maf = Sci.sci_of_num Sci.num_2_pow_1024 ;;
Sci.float_of_sci maf ;;

Sci.float_of_sci ( Sci.sci_of_num Sci.num_2_pow_128 ) ;;

Sci.print_sci_1024_10 ( Sci.sci_of_num Sci.num_2_pow_minus_1024 ) ;;
Sci.print_sci_1024_10 ( Sci.sci_of_num Sci.num_2_pow_minus_128 ) ;;
Sci.print_sci_1024_10 ( Sci.sci_of_num ( Num.power_num Sci.num_05 ( Num.num_of_int 63 ) ) ) ;;
Sci.print_sci_1024_10 ( Sci.sci_of_num ( Num.power_num Sci.num_05 ( Num.num_of_int 65 ) ) ) ;;

Sci.print_sci_1024_10 ( Sci.sci_of_string_array [| "1" ; "2" ; "10" |] ) ;;

let mmff = Sci.int_pow 65536 maf ;;
Sci.print_sci_1024_2 mmff ;;

let r = Sci.real_sqrt_1024 Sci.sci_2 ;;
Sci.complex_of_sci r ;;
sqrt 2. ;;

let r0 = Sci.complex_of_sci ( Sci.sqrt_1024 Sci.sci_2i ) ;;
(*
Matrix.matrix_float_prod r0 r0 ;;
*)
let r1 = Sci.complex_of_sci ( Sci.sqrt_1024 ( Sci.sci_of_float (-0.75) ) );;

let sol = Sci.solve_degree_2_1024 Sci.sci_1 Sci.sci_1 Sci.sci_1 ;;
let sol0 = Array.map Sci.complex_of_sci sol ;;

let z = Sci.plus_1024 Sci.half_pi_1000_10 Sci.half_pi_1000_10 ;;
Sci.minus z Sci.pi_1000_10 ;;

let y = Sci.mult_1024 Sci.half_pi_1000_10 Sci.pi_1000_10 ;;
let x = Sci.mult Sci.pi_1000_10 Sci.half_pi_1000_10 ;;
(*
Sci.minus y x ;;
*)
Sci.minus_1024 y x ;;
Sci.div_1024 y x ;;
(*
Sci.div y x ;;
*)

let rr = Sci.int_pow_1024 2 r ;;
Sci.complex_of_sci rr ;;


let a0 = [|[|0.999537131586786143; -1.22955137826996658e-19|] ; [|1.22955137826996658e-19; 0.999537131586786143|]|];;
let b0 = [|[|0.999845450393193; -4.10992935015358837e-20|] ; [|4.10992935015358837e-20; 0.999845450393193|]|]  ;;
let c0 = [|[|0.999948456937; -1.37138697602e-20|] ; [|1.37138697602e-20; 0.999948456937|]|]  ;;
let aA = Sci.sci_of_complex a0 ;;
let bB = Sci.sci_of_complex b0 ;;
let cC = Sci.sci_of_complex c0 ;;
let dD = Sci.aitken_seki_1024 aA bB cC ;;
let d = Sci.complex_of_sci dD ;;

let aa = Sci.sci_of_complex [|[|1.00093397534851669; 1.88849435087711631e-21|] ; [|-1.88849435087711631e-21; 1.00093397534851669|]|] ;;
let bb = Sci.sci_of_complex [|[|1.0003103531; 6.25561639667e-22|] ; [|-6.25561639667e-22; 1.0003103531|]|] ;;
let cc = Sci.sci_of_complex [|[|1.00010334391; 2.08088496508e-22|] ; [|-2.08088496508e-22; 1.00010334391|]|] ;;
let dd = Sci.sci_of_complex [|[|1.0000344361; 6.93150212666e-23|] ; [|-6.93150212666e-23; 1.0000344361|]|] ;;
let ee = Sci.sci_of_complex [|[|1.00001147738; 2.30997019396e-23|] ; [|-2.30997019396e-23; 1.00001147738|]|] ;;
let ff = Sci.shanks2_1024 aa bb cc dd ee ;;
let f = Sci.complex_of_sci ff ;;

let s0 = [| aA ; bB ; cC |] ;;
let s1 = [| aa ; bb ; cc ; dd ; ee |] ;;

let ddd = Sci.wynn_1024 2 0 s0 ;;
let d_d = Sci.complex_of_sci ddd ;;

let d0 = Sci.wynn_rho_1024 2 0 s0 ;;
let d1 = Sci.complex_of_sci ddd ;;

let fff = Sci.wynn_1024 4 0 s1 ;;
let f_f = Sci.complex_of_sci fff ;;

let f0 = Sci.wynn_rho_1024 4 0 s1 ;;
let f1 = Sci.complex_of_sci f0 ;;

let f2 = Sci.aitken_seki_rec_1024 2 0 s1 ;;
let f3 = Sci.complex_of_sci f2 ;;

Sci.complex_of_sci ( Sci.approx_1024 s0 ) ;;
Sci.complex_of_sci ( Sci.approx_1024 ( Array.sub s1 0 3 ) ) ;;
Sci.complex_of_sci ( Sci.approx_1024 ( Array.sub s1 0 4 ) ) ;;
Sci.complex_of_sci ( Sci.approx_1024 s1 ) ;;

Sci.print_sci_1024_2 fff ;;
Sci.print_sci_1024_10 fff ;;
let ffff = Sci.mult_1024 Sci.sci_1024 fff ;;
Sci.print_sci_1024_2 ffff ;;
Sci.print_sci_1024_10 ffff ;;

let fF = Sci.brezinski_1024 2 0 s1 ;;
Sci.print_sci_1024_10 fF ;;
Sci.complex_of_sci ( fF ) ;;


Sci.real_cubic_root_1024 Sci.sci_8 ;;

let rc2 = Sci.real_cubic_root_1024 Sci.sci_2 ;;
Sci.complex_of_sci rc2 ;;
exp ( ( log 2. ) /. 3. ) ;;
let vrc2 = Sci.mult_1024 ( Sci.mult_1024 rc2 rc2 ) rc2 ;;
Sci.complex_of_sci vrc2 ;;
Sci.print_sci_1024_10 vrc2 ;;

let rc3 = Sci.real_cubic_root_1024 Sci.sci_3 ;;
Sci.complex_of_sci rc3 ;;
exp ( ( log 3. ) /. 3. ) ;;
let vrc3 = Sci.mult_1024 ( Sci.mult_1024 rc3 rc3 ) rc3 ;;
Sci.complex_of_sci vrc3 ;;
Sci.print_sci_1024_10 vrc3 ;;

let rcm3000 = Sci.real_cubic_root_1024 ( Sci.sci_of_float (-3000.) ) ;;
Sci.complex_of_sci rcm3000 ;;
let vrcm3000 = Sci.mult_1024 ( Sci.mult_1024 rcm3000 rcm3000 ) rcm3000 ;;
Sci.complex_of_sci vrcm3000 ;;
Sci.print_sci_1024_10 vrcm3000 ;;

let c2 = Sci.cubic_root_1024 Sci.sci_2 ;;
Sci.complex_of_sci c2 ;;
let vc2 = Sci.mult_1024 ( Sci.mult_1024 c2 c2 ) c2 ;;
Sci.complex_of_sci vc2 ;;
Sci.print_sci_1024_10 vc2 ;;

let c3 = Sci.cubic_root_1024 Sci.sci_3 ;;
Sci.complex_of_sci c3 ;;
exp ( ( log 3. ) /. 3. ) ;;
let vc3 = Sci.mult_1024 ( Sci.mult_1024 c3 c3 ) c3 ;;
Sci.complex_of_sci vc3 ;;
Sci.print_sci_1024_10 vc3 ;;

let m3000 = Sci.sci_of_float (-3000.) ;;
let cm3000 = Sci.cubic_root_1024 m3000 ;;
Sci.complex_of_sci cm3000 ;;
let vcm3000 = Sci.mult_1024 ( Sci.mult_1024 cm3000 cm3000 ) cm3000 ;;
Sci.complex_of_sci vcm3000 ;;
Sci.print_sci_1024_10 vcm3000 ;;

let c3i = Sci.cubic_root_1024 Sci.sci_3i ;;
Sci.complex_of_sci c3i ;;
let vc3i = Sci.mult_1024 ( Sci.mult_1024 c3i c3i ) c3i ;;
Sci.complex_of_sci vc3i ;;
Sci.print_sci_1024_10 vc3i ;;

let m3000i = Sci.mult m3000 Sci.sci_i ;;
let cm3000i = Sci.cubic_root_1024 m3000i ;;
Sci.complex_of_sci cm3000i ;;
let vcm3000i = Sci.mult_1024 ( Sci.mult_1024 cm3000i cm3000i ) cm3000i ;;
Sci.complex_of_sci vcm3000i ;;
Sci.print_sci_1024_10 vcm3000i ;;

Sci.print_sci_1024_10 Sci.sci_1024_j ;;
let vj = Sci.mult_1024 ( Sci.mult_1024 Sci.sci_1024_j Sci.sci_1024_j ) Sci.sci_1024_j ;;
Sci.print_sci_1024_10 vj ;;

Sci.cubic_root_1024 Sci.sci_0 ;;
Sci.real_cubic_root_1024 Sci.sci_0 ;;
Sci.nth_root_1024 5 Sci.sci_0 ;;

let fr2 = Sci.nth_root_1024 5 Sci.sci_2 ;;
Sci.complex_of_sci fr2 ;;
let vfr2 = Sci.int_pow 5 fr2 ;;
Sci.complex_of_sci vfr2 ;;
Sci.print_sci_1024_10 vfr2 ;;

let k = Sci.sci_1024_primitive_root_of_unity 5 ;;
let vk = Sci.int_pow_1024 5 k ;;
Sci.complex_of_sci vk ;;
Sci.print_sci_1024_10 vk ;;

let s0 = Sci.solve_degree_3_1024 Sci.sci_1 Sci.sci_0 Sci.sci_0 Sci.sci_minus_1 ;;
Array.map Sci.complex_of_sci s0 ;;
let s1 = Array.map ( Sci.int_pow_1024 3 ) s0 ;;
Array.map Sci.print_sci_1024_10 s1 ;;
Array.map Sci.complex_of_sci s1 ;;

let s2 = Sci.solve_degree_3_1024 Sci.sci_1 Sci.sci_0 Sci.sci_0 m3000i ;;
Array.map Sci.complex_of_sci s2 ;;
let s3 = Array.map ( Sci.int_pow_1024 3 ) s2 ;;
Array.map Sci.print_sci_1024_10 s3 ;;
Array.map Sci.complex_of_sci s3 ;;

let s4 = Sci.solve_degree_3_1024 Sci.sci_1 Sci.sci_1 Sci.sci_1 Sci.sci_1 ;;
Array.map Sci.complex_of_sci s4 ;;
let s5 = Array.map ( Sci.int_pow_1024 4 ) s4 ;;
Array.map Sci.print_sci_1024_10 s5 ;;
Array.map Sci.complex_of_sci s5 ;;

let s6 = Sci.solve_degree_4_1024 Sci.sci_1 Sci.sci_0 Sci.sci_0 Sci.sci_0 Sci.sci_minus_1 ;;
Array.map Sci.complex_of_sci s6 ;;
let s7 = Array.map ( Sci.int_pow_1024 4 ) s6 ;;
Array.map Sci.print_sci_1024_10 s7 ;;
Array.map Sci.complex_of_sci s7 ;;

let s8 = Sci.solve_degree_4_1024 Sci.sci_1 Sci.sci_1 Sci.sci_1 Sci.sci_1 Sci.sci_1 ;;
Array.map Sci.complex_of_sci s8 ;;
let s9 = Array.map ( Sci.int_pow_1024 5 ) s8 ;;
Array.map Sci.print_sci_1024_10 s9 ;;
Array.map Sci.complex_of_sci s9 ;;


Num.float_of_num Data.Classical.num_e_1024 ;;
Num.float_of_num Data.Classical.num_log_2_1024 ;;

Sci.float_of_sci ( Sci.th_of_real_1024 0.01 Sci.sci_2 ) ;;
Sci.th_of_real_1024 0.01 [| Sci.num_1 ; Sci.num_0 ; Sci.num_1024 |] ;;
Sci.complex_of_sci ( Sci.expm1_1024 Sci.sci_1 ) ;;
Sci.complex_of_sci ( Sci.expm1_1024 Sci.sci_2 ) ;;
Sci.print_sci_1024_10 ( Sci.expm1_1024 ( Sci.ipi_1024 ) ) ;;
Sci.complex_of_sci ( Sci.integer_part_1024 ( Sci.expm1_1024 Sci.sci_2 ) ) ;;
Sci.complex_of_sci ( Sci.integer_part_1024 ( Sci.mult_1024 Sci.sci_3 ( Sci.expm1_1024 Sci.quarter_ipi_1024 ) ) ) ;;
Sci.print_sci_1024_10 ( Sci.plus_1024 Sci.sci_1 ( Sci.expm1_1024 Sci.quarter_ipi_1024 ) ) ;;
Sci.print_sci_1024_10 ( Sci.sqrt_1024 Sci.sci_05 ) ;;

Sci.complex_of_sci ( Sci.tan_of_real_1024 0.01 Sci.sci_1 ) ;;
Sci.complex_of_sci ( Sci.tan_of_real_1024 0.01 Sci.sci_10 ) ;;

let p4 = Sci.quarter_pi_1024 ;;
let ta = Sci.direct_tan_1024 0.01 p4 ;;
let tb = Sci.tan_1024 p4 ;;
let ta0 = Sci.minus_1024 ta Sci.sci_1 ;;
let tb0 = Sci.minus_1024 tb Sci.sci_1 ;;
Sci.print_sci_1024_10 ta0 ;;
Sci.print_sci_1024_10 tb0 ;;


Sci.complex_of_sci ( Sci.th_1024 Sci.sci_1 ) ;;
Sci.complex_of_sci ( Sci.ch_1024 Sci.sci_1 ) ;;
Sci.complex_of_sci ( Sci.cos_1024 Sci.sci_1 ) ;;

let c10 = Sci.cos_1024 Sci.quarter_pi_1024 ;;
Sci.print_sci_1024_10 c10 ;;
let c11 = Sci.mult_1024 c10 c10 ;;
Sci.print_sci_1024_10 c11 ;;

let e12 = Sci.plus_1024 Sci.sci_1 ( Sci.expm1_1024 Sci.sci_1 ) ;;
let e13 = Sci.minus_1024 e12 [| Data.Classical.num_e_1024 ; Sci.num_0 ; Sci.num_0 |] ;;
Sci.print_sci_1024_10 e13 ;;


Sci.agm_log_1024 Sci.sci_1 ;;
let ml2 = Sci.agm_log_1024 Sci.sci_05 ;;
let ml2a = Sci.plus ml2 Sci.log_2_1024 ;;
Sci.print_sci_1024_10 ml2 ;;
Sci.print_sci_1024_10 ml2a ;;
let z0 = Sci.mult_1024 c10 ( Sci.plus Sci.sci_1 Sci.sci_i ) ;;
Sci.print_sci_1024_10 z0 ;;
let l0 = Sci.agm_log_1024 z0 ;;
let l00 = Sci.complex_of_sci l0 ;;

let t2 = Sci.exp_1024 Sci.sci_1 ;;
let t20 = Sci.minus_1024 t2 Sci.e_1024 ;;
Sci.print_sci_1024_10 t20 ;;

let t3 = Sci.exp_1024 Sci.log_10_1024 ;;
let t30 = Sci.minus_1024 t3 Sci.sci_10 ;;
Sci.print_sci_1024_10 t30 ;;

let t4 = Sci.exp_1024 Sci.log_2_1024 ;;
let t40 = Sci.minus_1024 t4 Sci.sci_2 ;;
Sci.print_sci_1024_10 t40 ;;

let l20 = Sci.direct_log_1024 (-100.) Sci.sci_2 ;;
let l20a = Sci.log_1024 Sci.sci_2 ;;
let l20b = Sci.minus_1024 l20 Sci.log_2_1024 ;;
let l20c = Sci.minus_1024 l20a Sci.log_2_1024 ;;
Sci.print_sci_1024_10 l20b ;;
Sci.print_sci_1024_10 l20c ;;

let l10 = Sci.log_1024 Sci.sci_10 ;;
Sci.print_sci_1024_10 l10 ;;
let t1 = Sci.exp_1024 l10 ;;
Sci.print_sci_1024_10 t1 ;;
let l100 = Sci.direct_log_1024 (-100.) Sci.sci_10 ;;
let l100a = Sci.minus_1024 l100 Sci.log_10_1024 ;;
let l10a = Sci.minus_1024 l10 Sci.log_10_1024 ;;
Sci.print_sci_1024_10 l100a ;;
Sci.print_sci_1024_10 l10a ;;

let le = Sci.direct_log_1024 (-100.) Sci.e_1024 ;;
let lea = Sci.log_1024 Sci.e_1024 ;;
let leb = Sci.minus_1024 le Sci.sci_1 ;;
let lec = Sci.minus_1024 lea Sci.sci_1 ;;
Sci.print_sci_1024_10 leb ;;
Sci.print_sci_1024_10 lec ;;


let a00 = Sci.arccos_1024 Sci.sci_05 ;;
Sci.print_sci_1024_10 a00 ;;
let c12 = Sci.cos_1024 a00 ;;
Sci.print_sci_1024_10 c12 ;;
Sci.minus_1024 c12 Sci.sci_05 ;;

let a01 = Sci.argch_1024 Sci.sci_1 ;;

let a01a = Sci.ch_1024 ( Sci.argch_1024 Sci.sci_2 ) ;;
let a01a0 = Sci.minus_1024 a01a Sci.sci_2 ;;

let a02 = Sci.arccos_1024 Sci.sci_1 ;;

let a03 = Sci.arccos_1024 c10 ;;
Sci.print_sci_1024_10 a03 ;;
let c13 = Sci.cos_1024 a03 ;;
Sci.print_sci_1024_10 c13 ;;
Sci.minus_1024 c10 c13 ;;

let a04 = Sci.arcsin_1024 Sci.sci_05 ;;
Sci.print_sci_1024_10 a04 ;;
let s10 = Sci.sin_1024 a04 ;;
Sci.print_sci_1024_10 s10 ;;
let s10a = Sci.minus_1024 s10 Sci.sci_05 ;;
Sci.print_sci_1024_10 s10a ;;

let u0 = Sci.cos_1024 Sci.pi_1024 ;;
let u1 = Sci.exp_1024 Sci.ipi_1024 ;;
let u2 = Sci.sin_1024 Sci.pi_1024 ;;
let u0a = Sci.plus_1024 u0 Sci.sci_1 ;;
let u1a = Sci.plus_1024 u1 Sci.sci_1 ;;
Sci.print_sci_1024_10 u0a ;;
Sci.print_sci_1024_10 u1a ;;
Sci.print_sci_1024_10 u2 ;;

let u3 = Sci.exp_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
let u4 = Sci.log_1024 u3 ;;
Sci.print_sci_1024_10 u3 ;;
Sci.print_sci_1024_10 u4 ;;
Sci.print_sci_1024_10 ( Sci.mult_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_16 |] u4 ) ;;

let u5 = Sci.exp_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;
let u6 = Sci.log_1024 u5 ;;
Sci.print_sci_1024_10 u5 ;;
Sci.print_sci_1024_10 u6 ;;

let u7 = Sci.sin_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.minus_1024 ( Sci.arcsin_1024 u7 ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.minus_1024 ( Sci.direct_arcsin_1024 3 u7 ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
let u8 = Sci.direct_sin_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;
Sci.minus_1024 ( Sci.arcsin_1024 u8 ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;

let u7a = Sci.sh_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.minus_1024 ( Sci.argsh_1024 u7a ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.minus_1024 ( Sci.direct_argsh_1024 3 u7a ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;

let u7b = Sci.th_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.minus_1024 ( Sci.argth_1024 u7b ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;

let u7c = Sci.tan_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.minus_1024 ( Sci.arctan_1024 u7c ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;

let u8a = Sci.sh_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;
Sci.print_sci_1024_10 u8a ;;
Sci.float_of_sci u8a ;;

let u9 = Sci.arcsin_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
let u10 = Sci.arcsin_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;
Sci.print_sci_1024_10 u9 ;;
Sci.print_sci_1024_10 u10 ;;
Sci.minus_1024 ( Sci.arcsin_1024 u9 ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.minus_1024 ( Sci.arcsin_1024 u10 ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;

let u9a = Sci.argsh_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.print_sci_1024_10 u9a ;;
Sci.minus_1024 ( Sci.sh_1024 u9a ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
let u9b = Sci.direct_argsh_1024 3 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.print_sci_1024_10 u9b ;;
Sci.minus_1024 ( Sci.direct_sh_1024 u9b ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
Sci.minus_1024 ( Sci.sh_1024 u9b ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
let u10a = Sci.argsh_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;
Sci.print_sci_1024_10 u10a ;;
Sci.minus_1024 ( Sci.sh_1024 u10a ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;
let u10b = Sci.direct_argsh_1024 3 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;
Sci.print_sci_1024_10 u10b ;;
Sci.minus_1024 ( Sci.direct_sh_1024 u10b ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;
Sci.minus_1024 ( Sci.sh_1024 u10b ) [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_128 |] ;;

let c14 = Sci.plus_1024 Sci.half_pi_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
let c15 = Sci.cos_1024 c14 ;;
Sci.minus_1024 ( Sci.arccos_1024 c15 ) c14 ;;

let c16 = Sci.plus_1024 Sci.half_ipi_1024 [| Sci.num_1 ; Sci.num_0 ; Sci.num_minus_16 |] ;;
let c17 = Sci.ch_1024 c16 ;;
Sci.minus_1024 ( Sci.argch_1024 c17 ) c16 ;;

(*
Sci.tan_1024 Sci.half_pi_1024 ;;
*)
let u11 = Sci.tan_1024 ( Sci.plus_1024 Sci.half_pi_1024 Sci.sci_i ) ;;

(*
Sci.th_1024 Sci.half_ipi_1024 ;;
*)
let u12 = Sci.th_1024 ( Sci.plus_1024 Sci.sci_1 Sci.half_ipi_1024 ) ;;
let u13 = Sci.plus_1024 u12 ( Sci.mult_1024 Sci.sci_i u11 ) ;;

Sci.cos_1024 Sci.half_pi_1024 ;;
Sci.cos_1024 Sci.minus_half_pi_1024 ;;
Sci.cos_1024 Sci.pi_1024 ;;
Sci.cos_1024 Sci.minus_pi_1024 ;;
Sci.sin_1024 Sci.half_pi_1024 ;;
Sci.sin_1024 Sci.minus_half_pi_1024 ;;
Sci.sin_1024 Sci.pi_1024 ;;
Sci.sin_1024 Sci.minus_pi_1024 ;;

Sci.ch_1024 Sci.half_ipi_1024 ;;
Sci.ch_1024 Sci.minus_half_ipi_1024 ;;
Sci.ch_1024 Sci.ipi_1024 ;;
Sci.ch_1024 Sci.minus_ipi_1024 ;;
Sci.sh_1024 Sci.half_ipi_1024 ;;
Sci.sh_1024 Sci.minus_half_ipi_1024 ;;
Sci.sh_1024 Sci.ipi_1024 ;;
Sci.sh_1024 Sci.minus_ipi_1024 ;;

Sci.exp_1024 Sci.ipi_1024 ;;
let u14 = Sci.exp_1024 ( Sci.mult_1024 ( Sci.sci_of_int 17 ) Sci.ipi_1024 ) ;;
Sci.print_sci_1024_10 u14 ;;
let u15 = Sci.exp_1024 ( Sci.mult_1024 ( Sci.sci_of_int 99999999 ) Sci.ipi_1024 ) ;;
Sci.print_sci_1024_10 u14 ;;

let u16 = Sci.log_1024 Sci.sci_minus_1 ;;
Sci.print_sci_1024_10 u16 ;;
Sci.print_sci_1024_10 ( Sci.minus_1024 Sci.ipi_1024 u16 ) ;;

(*
Sci.argth_1024 Sci.sci_minus_1 ;;
Sci.argth_1024 Sci.sci_1 ;;
*)
let u17 = Sci.argth_1024 Sci.sci_2 ;;
Sci.print_sci_1024_10 u17 ;;
let u18 = Sci.th_1024 u17 ;;
Sci.print_sci_1024_10 u18 ;;


Sci.cotan_1024 Sci.half_pi_1024 ;;
Sci.cotan_1024 Sci.minus_half_pi_1024 ;;
(*
Sci.cotan_1024 Sci.pi_1024 ;;
*)
Sci.tan_1024 Sci.minus_pi_1024 ;;
(*
Sci.cotan_1024 Sci.minus_pi_1024 ;;
*)

(*
Sci.sec_1024 Sci.half_pi_1024 ;;
Sci.sec_1024 Sci.minus_half_pi_1024 ;;
*)
Sci.sec_1024 Sci.pi_1024 ;;
Sci.sec_1024 Sci.minus_pi_1024 ;;

Sci.cosec_1024 Sci.half_pi_1024 ;;
Sci.cosec_1024 Sci.minus_half_pi_1024 ;;
(*
Sci.cosec_1024 Sci.pi_1024 ;;
Sci.cosec_1024 Sci.minus_pi_1024 ;;
*)

(*****************************************************************************)
Sci.complete_sieve 10 29 ;;
Sci.primes_sieve 10 29 ;;
let f29 = Sci.fact 10 29 ;;
Big_int.float_of_big_int f29 ;;
Sci.approx_decimal_fact 10 29 ;;
(*
Infinitesimal.low_stirling 29. ;;
Infinitesimal.up_stirling 29. ;;
*)
Sci.factors ( 37 * 37 ) ;;
Sci.factors ( 37 * 41 ) ;;
Sci.factors ( 37 * 38 ) ;;
Sci.factors ( 37 * 36 ) ;;
Sci.factors ( 1024 * 2187 ) ;;
Sci.is_prime 257 ;;
Sci.is_prime 259 ;;
Sci.is_prime 1021 ;; 
Sci.sieve 1021 ;;

let f170 = Sci.fact 100 170 ;;
Big_int.float_of_big_int f170 ;;
Sci.approx_decimal_fact 100 170 ;;

Sci.phi_euler 10 ;;
Sci.phi_euler ( 1024 * 2187 ) ;;
Sci.phi_euler 437 ;;

let ( tr , r ) = let a = Sys.time () in let s = Sci.complete_sieve 100 2_000 in let b = Sys.time () in ( b -. a , s ) ;;
Util.primo r ;;
Util.secundo r ;;
Util.tertio r ;;
let ( tf , f ) = let a = Sys.time () in let s = Sci.factorial_sieve 100 2_000 in let b = Sys.time () in ( b -. a , s ) ;;
fst f ;;
snd f ;;
let ( tp , p ) = let a = Sys.time () in let s = Sci.sieve 50_000 in let b = Sys.time () in ( b -. a , s ) ;;
let ( tx , x ) = let a = Sys.time () in let s = Sci.factors 444_444_444_444 in let b = Sys.time () in ( b -. a , s ) ;;
let ( ty , y ) = let a = Sys.time () in let s = Sci.factors 444_444_444 in let b = Sys.time () in ( b -. a , s ) ;;
let ( tz , z ) = let a = Sys.time () in let s = Sci.factors 2294011 in let b = Sys.time () in ( b -. a , s ) ;;

Sci.is_prime 2294011 ;;
