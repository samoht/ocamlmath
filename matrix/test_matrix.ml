(*******************************************************)
(* une matrice (bidimensionnelle) est un vecteur ligne *)
(* dont chaque élément est une ligne de la matrice *****)
(*******************************************************)

(**********************)
(* Tests **************)
(**********************)

(*
epsilon
*)
(( 4. /. 3. ) -. 1. ) *. 3. -. 1. ;;

(*
http://www.cs.berkeley.edu/~wkahan/Math128/Projects/m128bfin.pdf

Q. Does my computer have extra-precision accumulation ?
A. Try [ (2 - 2^33), 2^33, -1] * [ (1 + 2^32) ; 2^32 ; 1] ;
   on MATLAB. If the answer is +1, then you have extra-
   precision accumulation. If it is -1, then you don't. As an
   alternative, try running mxmuleps.m from
   Appendix B on your MATLAB.
*)

let a = [| 2. -. 2. ** 33. ; 2. ** 33. ; -1. |] ;;
let b = [| 1. +. 2. ** 32. ; 2. ** 32. ; 1. |] ;;
Matrix.vector_float_scal_prod a b ;;

let a = Array.make_matrix 4 4 0 ;;
a.(0).(1) <- 1;;
a.(1).(2) <- 1;;
a.(2).(3) <- 1;;
a.(3).(0) <- 1;;

let a_a = Array.make_matrix 3 4 (4. *. atan 1.) ;;
let aaa = Util.transpose a ;;

let b = [|0;1;2;3;4|] ;;
let bb = [|0.;1.;2.;3.;4.|] ;;
let bbb = Matrix.vector_int_scal_mult (-1) b ;;
let bbbb = Matrix.vector_float_scal_mult (-1.) bb ;;

let c = Matrix.matrix_int_prod a aaa ;;
let cc = Matrix.matrix_int_prod aaa a ;;

let e = Matrix.identity_int 4 4 ;;
let ee = Matrix.identity_int 4 4 ;;

let d = Matrix.matrix_int_prod e c ;;
let dd = Matrix.matrix_int_prod ee cc ;;
let ddd = Matrix.matrix_int_prod c e ;;
let dddd = Matrix.matrix_int_prod cc ee ;;

let f = Array.append b b ;;
let ff = Array.append bb bb ;;

let z = [| [|1; 2|]; [|0; 3|] |] ;;
let y = [| [|2; 1|]; [|5; -1|] |] ;;

let x = Matrix.matrix_int_prod z y ;;
let w = Matrix.matrix_int_prod y z ;;

let zz = [| [|1.; 2.|]; [|0.; 3.|] |] ;;
let yy = [| [|2.; 1.|]; [|5.; -1.|] |] ;;

let xx = Matrix.matrix_float_prod zz yy ;;
let ww = Matrix.matrix_float_prod yy zz ;;

let trouve = Matrix.matrix_find_all ( = ) 0 cc ;;

let uu0 =[|[|1.;2.|];[|3.;4.|]|];;
let ii = Matrix.identity_float 2 2 ;;
Matrix.float_pivot_downward 0 uu0 ii ;;
uu0 ;;
ii ;;

let aa = Matrix.float_of_matrix a ;;
let a_AA = Matrix.identity_float 4 4 ;;
Matrix.float_pivot_downward 0 aa a_AA ;;
aa ;;
a_AA ;;
Matrix.float_pivot_downward 1 aa a_AA ;;
aa ;;
a_AA ;;
Matrix.float_pivot_downward 2 aa a_AA ;;
aa ;;
a_AA ;;

let zzz = [| [|1.;2.|] ; [|3.;4.|] |] ;;
Matrix.float_inv zzz ;;
Matrix.float_slow_inv zzz ;;

Matrix.slow_float_target_inv Matrix.matrix_float_norm_inf Matrix.float_inv 0.1 zzz ;;
Matrix.slow_float_target_inv Matrix.matrix_float_norm_inf Matrix.float_inv 1e-10 zzz ;;
Matrix.float_target_inv Matrix.matrix_float_norm_inf Matrix.float_inv 0.1 zzz ;;
Matrix.float_target_inv Matrix.matrix_float_norm_inf Matrix.float_inv 1e-10 zzz ;;


Matrix.float_inv aa ;;
aa;;

let g = Matrix.matrix_float_random 100 100 1. ;;
let gg = Matrix.float_inv g ;;
let ggg = Matrix.matrix_float_prod g gg ;;
let gggg = Matrix.matrix_float_minus ggg ( Matrix.identity_float 100 100) ;;
let gG = Matrix.matrix_max ( Matrix.matrix_float_abs gggg) ;;
let gGg = Matrix.matrix_float_sum ( Matrix.matrix_float_abs gggg) ;;

(*******
let g00 = alpha_float_inv g ;;
let g01 = Matrix.matrix_float_abs ( Matrix.matrix_float_minus ( Matrix.matrix_float_prod g g00 ) ( Matrix.identity_float 100 100) ) ;;
let g02 = Matrix.matrix_max g01 ;;
let g03 = Matrix.matrix_float_sum g01 ;;
let k = Matrix.matrix_float_random 1000 1000 1. ;;
********)

let j =  Matrix.Float_vector_cons (Array.make 16 0.) ;;
let jj = Matrix.vector_foa_cut 8 j ;;
let jjj = Matrix.vector_foa_cut 4 jj ;;
let jjjj = Matrix.vector_foa_cut 2 jjj ;;
let kkk = Matrix.vector_foa_paste jjjj ;;
let kk = Matrix.vector_foa_paste kkk ;;
let k = Matrix.vector_foa_paste kk ;;

let l = Matrix.Int_vector_cons (Array.create 16 3) ;;
let ll = Matrix.vector_ioa_cut 8 l ;;
let lll = Matrix.vector_ioa_cut 4 ll ;;
let llll = Matrix.vector_ioa_cut 2 lll ;;
let mmm = Matrix.vector_ioa_paste llll ;;
let mm = Matrix.vector_ioa_paste mmm ;;
let m = Matrix.vector_ioa_paste mm ;;

let n = Matrix.Float_matrix_cons a_a ;;
Matrix.foa_numrows n ;;
Matrix.foa_numcolumns n ;;

let nn = Matrix.Foa_matrix_cons [|[|n;n|];[|n;n|]|] ;;
Matrix.foa_numrows nn ;;
Matrix.foa_numcolumns nn ;;

let o = Matrix.Int_matrix_cons a ;;
Matrix.ioa_numrows o ;;
Matrix.ioa_numcolumns o ;;

let nnn = Matrix.matrix_foa_cut 2 n ;;
let n_n = Matrix.matrix_foa_cut 2 nn ;;
Matrix.foa_thickness n ;;
Matrix.foa_thickness nnn ;;
Matrix.foa_thickness n_n ;;

let h = Matrix.Float_matrix_cons g ;;
let hh = Matrix.matrix_foa_cut 33 h ;;
let hhh = Matrix.matrix_foa_cut 11 hh ;;
let hhhh = Matrix.matrix_foa_cut 3 hhh ;;

let hHh = Matrix.matrix_foa_paste hhhh ;;
let hH = Matrix.matrix_foa_paste hHh ;;
let h_h = Matrix.matrix_foa_paste hH ;;
h_h = h ;;

let pP = Matrix.matrix_int_random 100 100 10 ;;
let p = Matrix.Int_matrix_cons pP ;;
let pp = Matrix.matrix_ioa_cut 33 p ;;
let ppp = Matrix.matrix_ioa_cut 11 pp ;;
let pppp = Matrix.matrix_ioa_cut 3 ppp ;;

let pPp = Matrix.matrix_ioa_paste pppp ;;
let pPpp = Matrix.matrix_ioa_paste pPp ;;
let p_p = Matrix.matrix_ioa_paste pPpp ;;
p_p = p ;;

let q = Matrix.vector_ioa_plus l l ;;
let qq = Matrix.vector_ioa_plus ll ll ;;
let qqq = Matrix.vector_ioa_plus lll lll ;;
let qqqq = Matrix.vector_ioa_plus llll llll ;;

let r = Matrix.vector_ioa_opp q ;;
let rr = Matrix.vector_ioa_opp qq ;;
let rrr = Matrix.vector_ioa_opp  qqq ;;
let rrrr = Matrix.vector_ioa_opp qqqq ;;

let s = Matrix.vector_ioa_coeff_prod q q ;;
let ss = Matrix.vector_ioa_coeff_prod qq qq ;;
let sss = Matrix.vector_ioa_coeff_prod qqq qqq ;;
let ssss = Matrix.vector_ioa_coeff_prod qqqq qqqq ;;

let t = Matrix.vector_ioa_sum q ;;
let tt = Matrix.vector_ioa_sum qq ;;
let ttt = Matrix.vector_ioa_sum  qqq ;;
let tttt = Matrix.vector_ioa_sum qqqq ;;

let u = Matrix.vector_ioa_scal_prod q q ;;
let uu = Matrix.vector_ioa_scal_prod qq qq ;;
let uuu = Matrix.vector_ioa_scal_prod qqq qqq ;;
let uuuu = Matrix.vector_ioa_scal_prod qqqq qqqq ;;

let l_l = Matrix.Float_vector_cons ( Array.make 16 3. );;
let lL = Matrix.vector_foa_cut 8 l_l ;;
let lLl = Matrix.vector_foa_cut 4 lL ;;
let lLll = Matrix.vector_foa_cut 2 lLl ;;
let mMm = Matrix.vector_foa_paste lLll ;;
let mM = Matrix.vector_foa_paste mMm ;;
let m_m = Matrix.vector_foa_paste mM ;;

let q_q = Matrix.vector_foa_plus l_l l_l ;;
let qQ = Matrix.vector_foa_plus lL lL ;;
let qQq = Matrix.vector_foa_plus lLl lLl ;;
let qQqq = Matrix.vector_foa_plus lLll lLll ;;

let l0 = Matrix.Foa_vector_cons [| lLll;qQqq|] ;;
Matrix.vector_foa_print l0 ;;

let u = Matrix.vector_foa_scal_prod q_q q_q ;;
let uu = Matrix.vector_foa_scal_prod qQ qQ ;;
let uuu = Matrix.vector_foa_scal_prod qQq qQq ;;
let uuuu = Matrix.vector_foa_scal_prod qQqq qQqq ;;

Matrix.vector_foa_find_last 3. l_l ;;
Matrix.vector_foa_find_last 3. lL ;;
Matrix.vector_foa_find_last 3. lLl ;;
Matrix.vector_foa_find_last 3. lLll ;;

Matrix.vector_foa_find_last 1. l_l ;;
Matrix.vector_foa_find_last 1. l_l ;;
Matrix.vector_foa_find_last 1. l_l ;;
Matrix.vector_foa_find_last 1. l_l ;;

Matrix.vector_foa_find_first 3. l_l ;;
Matrix.vector_foa_find_first 3. lL ;;
Matrix.vector_foa_find_first 3. lLl ;;
Matrix.vector_foa_find_first 3. lLll ;;

Matrix.vector_foa_find_first 1. l_l ;;
Matrix.vector_foa_find_first 1. l_l ;;
Matrix.vector_foa_find_first 1. l_l ;;
Matrix.vector_foa_find_first 1. l_l ;;

Matrix.vector_foa_extract_element [0;0;0;0] lLll ;;
Matrix.vector_ioa_extract_element [0;0] qqqq ;;
let l_L = Matrix.vector_foa_affect_element [0;0;0;0] (Matrix.Float_cons 4.56) lLll;;

Matrix.matrix_foa_extract_element [[|0;0|];[|0;0|];[|0;0|];[|0;0|]] hhhh ;;
Matrix.matrix_ioa_extract_element [[|0;0|];[|0;0|];[|0;0|];[|0;0|]] pppp ;;
let h_H = Matrix.matrix_foa_affect_element [[|0;0|];[|0;0|];[|0;0|];[|0;0|]] (Matrix.Float_cons 9191.9191) hhhh ;;
Matrix.matrix_foa_extract_element [[|0;0|];[|0;0|];[|0;0|]] hhhh;;
Matrix.matrix_foa_extract_element [[|0;0|];[|0;0|];[|0;0|]] h_H;;

Matrix.matrix_foa_extract_column 1 hhhh ;;
Matrix.matrix_foa_extract_row 1 hhhh ;;

Matrix.matrix_foa_rec_size 2 hhhh ;;
Matrix.matrix_ioa_rec_size 2 pppp ;;

Matrix.matrix_ioa_find_last 99 pppp;;
Matrix.matrix_ioa_find_first 99 pppp;;
Matrix.matrix_ioa_find_last 1 pppp;;
Matrix.matrix_ioa_find_first 1 pppp;;

Matrix.matrix_foa_find_first 0. nn ;;
Matrix.matrix_foa_find_last 0. nn ;;
Matrix.matrix_foa_find_first 3.14159265358979312 nn ;;
Matrix.matrix_foa_find_last 3.14159265358979312 nn ;;

Matrix.matrix_foa_plus n n ;;
Matrix.matrix_foa_plus nn nn ;;
Matrix.matrix_foa_minus n n ;;
Matrix.matrix_foa_minus nn nn ;;

Matrix.matrix_foa_coeff_prod nn nn ;;
Matrix.matrix_foa_coeff_prod nn ( Matrix.Float_matrix_cons zz ) ;;
Matrix.matrix_foa_coeff_prod ( Matrix.Float_matrix_cons zz ) nn ;;

Matrix.matrix_foa_scal_mult (-1.) n ;;
Matrix.matrix_foa_scal_mult (-1.) nn ;;
Matrix.matrix_foa_scal_right_div (-2.) n ;;
Matrix.matrix_foa_scal_right_div (-2.) nn ;;

let nnnn = Matrix.Foa_matrix_cons [|[|nn;nn|]|] ;;
Matrix.foa_transpose nnnn ;;
Matrix.matrix_foa_rec_size 2 nnnn ;;
Matrix.matrix_foa_rec_size 2 ( Matrix.foa_transpose nnnn) ;;

Matrix.foa_trace nn ;;
Matrix.foa_rec_trace nn ;;

let nN = Matrix.Foa_matrix_cons [| [| Matrix.Float_cons 0. ; n |] ; [| n ; n |] |] ;; 
let nNn = Matrix.Foa_matrix_cons [| [| Matrix.Float_cons 0. ; n |] ; [| nN ; nN |] |] ;;
let nNnn = Matrix.Foa_matrix_cons [| [| Matrix.Float_cons 0. ; nNn |] |] ;;
Matrix.matrix_foa_rec_size 3 ( Matrix.matrix_foa_copy nNnn) ;;
Matrix.foa_thickness nNnn ;;
let nNnN =  Matrix.ioa_of_foa nNnn ;;
Matrix.matrix_ioa_rec_size 3 ( Matrix.matrix_ioa_copy nNnN) ;;

let zZ = Matrix.Float_matrix_cons zz ;;
let zZz = Matrix.Float_matrix_cons zzz ;;
let zzzz = Matrix.Foa_matrix_cons [| [| zZz ; zZ |] ; [| Matrix.foa_transpose zZ ; Matrix.foa_transpose zZz |] |] ;;
let zZzz = Matrix.Foa_matrix_cons [| [| zZ ; zZz |] ; [| zZz ; zZ |] |] ;;
Matrix.matrix_foa_prod zZ zZz ;;
Matrix.matrix_foa_prod zZzz zZzz ;;

let za = [|[|1;2;1;2|];[|0;3;3;4|];[|1;2;1;2|];[|3;4;0;3|]|];;
let zb = [|[|1;2;1;2|];[|3;4;0;3|];[|1;0;1;3|];[|2;3;2;4|]|];;
Matrix.matrix_int_prod za za ;;
Matrix.matrix_int_prod zb za ;;
Matrix.matrix_foa_prod zzzz zZzz ;;
Matrix.matrix_foa_prod zZ zzzz ;;
Matrix.matrix_foa_prod zzzz zZ ;;



let z_z = Matrix.Foa_matrix_cons [| [| zZz ; Matrix.matrix_foa_opp zZ |] ; [| zZ ; zZz |] |] ;;
let z_Z = Matrix.Foa_matrix_cons [| [| zZ ; zZz |] ; [| Matrix.matrix_foa_opp zZz ; zZ |] |] ;;
Matrix.matrix_foa_prod z_z zzzz ;;
Matrix.matrix_foa_prod z_Z zZzz ;;
let z_zz = Matrix.Foa_matrix_cons [| [| z_z ; z_Z |] ; [| zzzz ; zZzz |] |] ;;
let zz_z = Matrix.Foa_matrix_cons [| [| z_Z ; zzzz |] ; [| zZzz ; z_z |] |] ;;
Matrix.matrix_foa_prod z_zz zz_z ;;
Matrix.matrix_foa_prod zz_z z_zz ;;
let zZZ = Matrix.Foa_matrix_cons [| [| (Matrix.Float_cons 0.) ; (Matrix.Float_cons 0.) |] ; [| zZ ; zZz |] |] ;;
Matrix.matrix_foa_prod zZZ zzzz ;;
let z_Z = Matrix.Foa_matrix_cons [| [| (Matrix.Float_cons 0.) ; (Matrix.Float_cons 0.) |] ; [| zzzz ; zZzz |] |] ;;
Matrix.matrix_foa_prod z_Z zz_z ;;

let zZZ = Matrix.Foa_matrix_cons [| [| (Matrix.Float_cons 0.) ; zZ |] ; [| zZ ; zZz |] |] ;;
Matrix.matrix_foa_minus zZZ zZZ ;;
Matrix.foa_transpose zZZ;;

let zZZZ = Matrix.Foa_matrix_cons [| [| zZ ; zZ |] ; [| (Matrix.Float_cons 0.) ; zZz |] |] ;;
Matrix.matrix_foa_prod zZZ zZZZ ;;

let z_Z = Matrix.Foa_matrix_cons [| [| (Matrix.Float_cons 0.) ; zzzz |] ; [| (Matrix.Float_cons 0.) ; zZzz |] |] ;;
z_Z = Matrix.matrix_foa_minus z_Z ( Matrix.zeros_foa z_Z);;
Matrix.matrix_foa_prod zz_z z_Z ;;

let d0 = Matrix.Foa_strip_cons [| Matrix.Float_cons (-3.) ; Matrix.Float_cons 2. |] ;;
Matrix.foa_diag_left_mult d0 z_Z ;;
let d1 = Matrix.Foa_strip_cons [| zZ ; zZ |] ;;
Matrix.foa_diag_left_mult d1 z_Z ;;
Matrix.foa_diag_right_mult d1 z_Z ;;
let element = [| zZ ; zZ |] ;;
let diagonale = [| [| Matrix.Float_cons 0. ; zZ |]; [| zZ ; Matrix.Float_cons 0. |] |] ;;
let elem = Matrix.Foa_matrix_cons diagonale ;;
let d2 = Matrix.Foa_strip_cons [| elem ; elem |] ;;
Matrix.foa_diag_left_mult d2 z_Z ;;
Matrix.foa_diag_right_mult d2 z_Z ;;

Matrix.float_invertibility g ;;
Matrix.eye_foa z_Z ;;

let aaaa = Matrix.Float_matrix_cons aa ;;
let a_A = Matrix.foa_inv aaaa ;;
let a_aa = Matrix.matrix_foa_prod aaaa a_A ;;

(*******
let gGgg = Matrix.Float_matrix_cons g ;;
let gGgG = Matrix.foa_inv gGgg ;;
let g_g = Matrix.matrix_foa_prod gGgg gGgG ;;
let g_gg = Matrix.Float_matrix_cons (identity_float 100 100) ;;
let gg_g = Matrix.matrix_foa_minus g_g g_gg ;;
Matrix.matrix_float_sum ( Matrix.matrix_float_abs (matrix_float_demakeup gg_g) )
********)

let zZzZ = Matrix.foa_inv zZZZ;;

let gGgg = Matrix.Float_matrix_cons g ;;
let gGgG = Matrix.matrix_foa_hash 10 gGgg ;;
Matrix.matrix_foa_size gGgg;;
Matrix.matrix_foa_size gGgG;;
Matrix.matrix_foa_rec_size 2 gGgg;;
Matrix.matrix_foa_rec_size 2 gGgG;;
let gGGg = Matrix.foa_inv gGgg ;;
let gGGG = Matrix.foa_inv gGgG ;;
let produit = Matrix.matrix_foa_prod gGGG gGgG ;;
let difference = Matrix.matrix_foa_minus produit ( Matrix.eye_foa gGgG) ;;
let difference = Matrix.matrix_float_demakeup ( Matrix.matrix_foa_crash difference) ;;
let g_gg = Matrix.matrix_max ( Matrix.matrix_float_abs difference) ;;
let g_ggg = Matrix.matrix_float_sum ( Matrix.matrix_float_abs difference) ;;

(********
let g_g = Matrix.matrix_foa_hash 32 ( Matrix.Float_matrix_cons ( Matrix.matrix_float_random 512 512 1. ) ) ;;
let g_G = Matrix.foa_inv g_g ;;
let g_gg = Matrix.matrix_float_abs ( Matrix.matrix_float_demakeup ( Matrix.matrix_foa_crash ( Matrix.matrix_foa_minus (matrix_foa_prod g_g g_G) (eye_foa g_g) ) ) ) ;;
Matrix.matrix_max g_gg ;;
Matrix.matrix_float_sum g_gg ;;
********)

let p_P = Matrix.square_ioa_hash 8 p ( Matrix.Ioa_vector_cons [| Matrix.Int_vector_cons [|0;0|] |] ) ;;
let p_Pp = Matrix.matrix_ioa_demakeup p_P.(0) ;;
let p_pP = Matrix.matrix_ioa_demakeup p_Pp.(1).(1) ;;
Matrix.matrix_ioa_rec_size 2 p_P.(0) ;;
p_pP.(5) ;;
Matrix.ioa_thickness p_P.(0) ;;
Matrix.matrix_ioa_size p_P.(0) ;;
Matrix.matrix_ioa_size p ;;
p_P.(1) ;;

let b0 = Matrix.identity_float 5 5 ;;
let b1 = Matrix.matrix_vector_float_prod b0 bb ;;
let b2 = Matrix.vector_matrix_float_prod bb b0 ;;


let d10 = Matrix.vector_float_random 1000 1. ;;
let d11 = Matrix.vector_float_mean d10 ;;
let d12 = Matrix.vector_float_median d10 ;;
let d14 = ( let d13 = Matrix.vector_float_scal_add (-1.) ( Matrix.vector_float_random 1000 (-0.1) )
 in ( Matrix.vector_float_plus d13 ( Matrix.vector_float_scal_mult (-2.) d10) ) ) ;;
let d15 = Matrix.vector_float_linear_regression d10 d14 ;;

Matrix.vector_float_norm_1 bb ;;
Matrix.vector_float_norm_2 bb ;;
Matrix.vector_float_norm 3. bb ;;
Matrix.vector_float_norm_inf bb ;;

let g0 = Matrix.float_inv g ;;
let g1 = Matrix.matrix_float_prod g g0 ;;
let g2 = Matrix.matrix_float_minus g1 ( Matrix.identity_float 100 100) ;;
let g3 = Matrix.matrix_max ( Matrix.matrix_float_abs g2) ;;
let g4 = Matrix.matrix_float_sum ( Matrix.matrix_float_abs g2) ;;
gG;;
gGg;;

let pi = 4. *. atan 1. ;;
let z0 = Matrix.matrix_float_scal_mult ( - 4. *. pi ) zz ;;
let z1 = Matrix.matrix_float_round z0;;
let z2 = Matrix.int_invertibility 0.1 z1 ;;
let z3 = [|[|1;2|];[|-1;-1|]|] ;;
let z4 = Matrix.int_inv z3;;
Matrix.matrix_int_prod z3 z4 = Matrix.eye_int z3 ;;


let machin = Matrix.Float_cons 0. ;;
let truc = Matrix.Float_vector_cons bb ;;
let bidule = Matrix.Foa_vector_cons [| machin ; truc |] ;;
let chose = Matrix.Foa_vector_cons [| machin ; bidule |] ;;
let chouette = Matrix.Foa_matrix_cons [| [| chose ; bidule |]; [| machin ; truc |] |] ;;
let extra = Matrix.Foa_vector_cons [| truc ; truc |] ;;

let mac = Matrix.Int_cons 1 ;;
let tru = Matrix.Int_vector_cons b ;;
let bid = Matrix.Ioa_vector_cons [| mac ; tru |] ;;
let cho = Matrix.Ioa_vector_cons [| mac ; bid |] ;;

Matrix.vector_foa_plus extra bidule;;
let v = Matrix.Foa_vector_cons [| extra ; bidule |] ;;
let vv = Matrix.Foa_vector_cons [| bidule ; extra |] ;;
let vvv = Matrix.Foa_vector_cons [| Matrix.Float_cons 0. ;  bidule |] ;;
Matrix.vector_foa_plus v vv ;;
Matrix.vector_foa_plus v vvv ;;
Matrix.vector_foa_minus ( Matrix.vector_foa_minus vvv v) v ;;
Matrix.vector_foa_min ( Matrix.vector_foa_minus ( Matrix.vector_foa_minus vvv v) v) ;;

Matrix.ioa_thickness mac ;;
Matrix.ioa_thickness tru ;;
Matrix.ioa_thickness bid ;;
Matrix.ioa_thickness cho ;;
Matrix.foa_thickness machin ;;
Matrix.foa_thickness truc ;;
Matrix.foa_thickness bidule ;;
Matrix.foa_thickness chose ;;
Matrix.foa_thickness chouette ;;
Matrix.foa_thickness v ;;
Matrix.foa_thickness vv ;;
Matrix.foa_thickness vvv ;;

print_string "On doit trouver 0 0 1 2 0 0 1 2 3 2 2 2." ;;

(******
Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.alpha_float_inv g ) g0 ) ;;
******)

Matrix.gen_sympl_float 2 ;;
Matrix.gen_sympl_int 3 ;;

let g5 = Matrix.Float_matrix_cons g ;;
let g6 = Matrix.square_foa_hash 2 g5 ( Matrix.Foa_vector_cons [|Matrix.Float_vector_cons [| 0. ; 0.|] |] ) ;;
let g6 = Matrix.exp_foa_hash 10 g5 ( Matrix.Foa_vector_cons [|Matrix.Float_vector_cons [| 0. ; 0.|] |] ) ;;
let g7 = g6.(0) ;;
let g8 = g6.(1) ;;
Matrix.matrix_foa_size g7 ;;
let g9 = Matrix.foa_thickness g7 ;;
Matrix.matrix_foa_rec_size g9 g7;;
(*
let g10 = Matrix.foa_slow_inv g7 ;;
*)
let g10 = Matrix.foa_inv g7 ;;
let g11 = Matrix.square_foa_crash [| g10 ; g8 |] ;;
let g12 = Matrix.matrix_float_demakeup g11.(0) ;;
let g13 = Matrix.matrix_float_prod g12 g;;
let g14 = Matrix.matrix_float_minus g13 ( Matrix.identity_float 100 100) ;;
let g15 = Matrix.matrix_max ( Matrix.matrix_float_abs g14) ;;
let g16 = Matrix.matrix_float_sum ( Matrix.matrix_float_abs g14) ;;
let g17 = Matrix.matrix_float_norm_inf g14 ;;
g9 ;;

Matrix.foa_slow_invertibility g7 ;;
Matrix.foa_slow_invertibility_evaluation g7 ;;
Matrix.foa_slow_invertibility ( Matrix.zeros_foa g7 ) ;;
Matrix.foa_slow_invertibility_evaluation ( Matrix.zeros_foa g7 ) ;;
Matrix.foa_slow_abs_det g7 ;;
Matrix.float_slow_det g ;;
Matrix.foa_slow_abs_det ( Matrix.square_foa_hash 2 ( Matrix.Float_matrix_cons ( Matrix.identity_float 100 100 ) ) ( Matrix.Foa_vector_cons [|Matrix.Float_vector_cons [| 0. ; 0.|] |] ) ).(0) ;;
Matrix.foa_invertibility g7 ;;
Matrix.foa_invertibility_evaluation g7 ;;
Matrix.foa_invertibility ( Matrix.zeros_foa g7 ) ;;
Matrix.foa_invertibility_evaluation ( Matrix.zeros_foa g7 ) ;;
Matrix.foa_abs_det g7 ;;
Matrix.float_det g ;;
Matrix.foa_abs_det ( Matrix.square_foa_hash 2 ( Matrix.Float_matrix_cons ( Matrix.identity_float 100 100 ) ) ( Matrix.Foa_vector_cons [|Matrix.Float_vector_cons [| 0. ; 0.|] |] ) ).(0) ;;
Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.identity_float 100 100 ) ( Matrix.matrix_float_prod ( Matrix.generic_float_inv g) g ) ) ;;

let g20 = Matrix.matrix_float_random 50 50 1. in
 Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.identity_float 50 50 ) ( Matrix.matrix_float_prod ( Matrix.generic_float_inv g20) g20 ) ) ;;


let z5 = Matrix.matrix_foa_minus ( Matrix.matrix_foa_prod ( Matrix.foa_inv zzzz ) zzzz ) ( Matrix.eye_foa zzzz ) ;;
let z6 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_demakeup ( Matrix.matrix_foa_paste z5 ) ) ;;
Matrix.line_foa_left_quotient zzzz [| zzzz ; zzzz |] ;;
Matrix.line_float_slow_left_quotient zz [| zz ; zz |] ;;
let q0 = Matrix.ortho_float_bal_random 2 1. ;;
let q1 = Matrix.float_transpose q0 ;;
Matrix.matrix_float_prod q0 q1 ;;

let q2= Matrix.ortho_float_bal_random 100 1. ;;
let q3 = Matrix.float_transpose q2 ;;
let q4 = Matrix.matrix_float_norm_inf ( Matrix.matrix_float_minus ( Matrix.matrix_float_prod q3 q2 ) ( Matrix.identity_float 100 100) ) ;;
let q5 = Matrix.sym_float_bal_random 40 3. ;;
let q6 = Matrix.sym_float_reduc 0.01 1000 q5 ;;
let q7 = Matrix.matrix_float_non_diagonality Matrix.matrix_float_norm_inf q6.(2) ;;
let q8 = Matrix.sym_float_reduc_seq 1e-3 1000 q5 ;;
let q9 = Array.map Matrix.matrix_float_approx q8 ;;
let q10 = Array.map ( Matrix.matrix_float_wynn_rho ( 2 * ( ( Array.length q8.(0) ) / 4 ) ) 0 ) q8 ;;
Matrix.matrix_float_triple_prod q9.(1) q5 q9.(2) ;;
Matrix.matrix_float_triple_prod q10.(1) q5 q10.(2) ;;

Matrix.float_companion [| 2. ; 3. ; 4. |] ;;

let x1 = Matrix.saturate_float 2 2 3. ;;
Matrix.sym_float_reduc 0.01 10 x1 ;;
Matrix.sym_float_tune_reduc 0.01 10 x1 ;;
Matrix.sym_float_classical_jacobi_step epsilon_float x1 ;;
Matrix.sym_float_classical_jacobi_reduc epsilon_float 0.1 1e-10 100 x1 ;;


(*
let methode_reduc = Matrix.sym_float_tune_reduc ( sqrt epsilon_float ) 100 ;;
let methode_reduc = Matrix.sym_float_classical_jacobi_reduc 1e-5 1e-3 1e-20 100 ;;
let methode_reduc = Matrix.sym_float_classical_jacobi_reduc 1e-5 0.9 1e-20 100 ;;
let methode_reduc = Matrix.sym_float_classical_jacobi_reduc 1e-5 1e-30 1e-20 100 ;;
*)
let methode_reduc = Matrix.sym_float_tune_reduc ( sqrt epsilon_float ) 100 ;;
let y0 = Matrix.float_pca methode_reduc ( Matrix.matrix_float_random 3 1000 10. ) ;;

let d16 = Matrix.float_pca methode_reduc [| d10 ; d14 |] ;;
let d17 = d16.(1).(1).(1) /. d16.(1).(1).(0) ;;
let d18 = d16.(0).(0).(1) -. d17 *. d16.(0).(0).(0) ;;
d15 ;;

let d19 = Matrix.float_pca ( Matrix.sym_float_indirect_reduc methode_reduc ) [| d10 ; d14 |] ;;
let d20 = d19.(1).(1).(1) /. d19.(1).(1).(0) ;;
let d21 = d19.(0).(0).(1) -. d20 *. d19.(0).(0).(0) ;;

Matrix.upper_trig_float_random 3 3 3. ;;
Matrix.upper_trig_float_bal_random 3 3 3. ;;
Matrix.lower_trig_float_random 3 3 3. ;;
Matrix.lower_trig_float_bal_random 3 3 3. ;;
let unfr = Matrix.upper_nil_float_random 3 3 3. ;;
Matrix.float_nilpotence_order Matrix.matrix_float_norm_inf epsilon_float unfr ;;

Matrix.upper_nil_float_bal_random 3 3 3. ;;
Matrix.lower_nil_float_random 3 3 3. ;;
Matrix.lower_nil_float_bal_random 3 3 3. ;;
Matrix.upper_unip_float_random 3 3 3. ;;
Matrix.upper_unip_float_bal_random 3 3 3. ;;
Matrix.lower_unip_float_random 3 3 3. ;;
Matrix.lower_unip_float_bal_random 3 3 3. ;;

Matrix.upper_trig_int_random 4 4 5 ;;
Matrix.upper_trig_int_bal_random 4 4 5 ;;
Matrix.lower_trig_int_random 4 4 5 ;;
Matrix.lower_trig_int_bal_random 4 4 5 ;;
Matrix.upper_nil_int_random 4 4 5 ;;
Matrix.upper_nil_int_bal_random 4 4 5 ;;
let lnir = Matrix.lower_nil_int_random 4 4 5 ;;
Matrix.int_nilpotence_order lnir ;;
Matrix.int_power 2 lnir ;;
Matrix.int_power 3 lnir ;;
Matrix.int_power 4 lnir ;;

Matrix.lower_nil_int_bal_random 4 4 5 ;;
let x2 = Matrix.upper_unip_int_random 4 4 5 ;;
Matrix.upper_unip_int_bal_random 4 4 5 ;;
Matrix.lower_unip_int_random 4 4 5 ;;
let x3 = Matrix.lower_unip_int_bal_random 4 4 5 ;;
let x4 = Matrix.matrix_int_prod x2 x3 ;;
let x5 = Matrix.int_inv x4 ;;
Matrix.matrix_int_prod x4 x5 ;;
let x6 = Matrix.special_int_pseudo_random 4 9 ;;
Matrix.int_det x6 ;;
let x60 = Matrix.special_int_random 4 9 ;;
Matrix.int_det x60 ;;
let x7 = Matrix.invertible_int_random 4 9 ;;
let x70 = Matrix.invertible_int_random 44 1 ;;
(* x70 is too big for the determinant. *)
Array.map Matrix.int_slow_det [| x2 ; x3 ; x4 ; x5 ; x6 ; x60 ; x7 ; x70 |] ;;
let x8 = Matrix.special_float_pseudo_random 4 99. ;;
Matrix.float_det x8 ;;
Matrix.float_slow_det x8 ;;
let x80 = Matrix.special_float_random 4 9. ;;
Matrix.float_det x80 ;;
Matrix.float_slow_det x80 ;;

let x9 = [|[|1;1|];[|1;2|]|];;
let x10 = Matrix.int_power 8 x9 ;;
let x11 = Matrix.int_power (-8) x9 ;;
Matrix.matrix_int_prod x10 x11 ;;

let fFf = [|[|0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.|]; [|0.; 0.; 1.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.|];
  [|-24.; 50.; -35.; 10.; 0.; 0.; 0.; 0.|];
  [|-0.; -0.; -0.; -0.; 0.; 1.; 0.; 0.|];
  [|-0.; -0.; -0.; -0.; 0.; 0.; 1.; 0.|];
  [|-0.; -0.; -0.; -0.; 0.; 0.; 0.; 1.|];
  [|-0.; -0.; -0.; -0.; -24.; 50.; -35.; 10.|]|] ;;

let fFf1 = Matrix.float_normalized_iterate Matrix.vector_float_norm_inf 1111 fFf [| 1. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. |] ;;
let fFf2 = Matrix.matrix_vector_float_prod fFf fFf1 ;;
let fFf3 = Matrix.float_normalized_iterate Matrix.vector_float_norm_inf 76 fFf [| 1. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. ; 0. |] ;;
let fFf4 = Matrix.float_normalized_iterate_seq Matrix.vector_float_norm_inf 9 fFf fFf3  ;;
let fFf5 = Matrix.vector_float_approx fFf4 ;;
let fFf6 = Matrix.vector_float_wynn ( 2 * ( ( Array.length fFf4 ) / 4 ) ) 0 fFf4 ;;
let fFf7 = Matrix.vector_float_brezinski ( 2 * ( ( Array.length fFf4 ) / 4 ) ) 0 fFf4 ;;
let fFf8 = Matrix.vector_float_wynn_rho ( 2 * ( ( Array.length fFf4 ) / 4 ) ) 0 fFf4 ;;
let fFf9 = Matrix.matrix_vector_float_prod fFf fFf5 ;;

(*
let fFf99 = Array.map ( Array.map Sci.sci_of_float ) ( Matrix.float_transpose fFf4 ) ;;
let fFf98 = Sci.approx_1024 fFf99.(0) ;;
let fFf97 = Sci.complex_of_sci fFf98 ;;
let fFf96 = Sci.approx_1024 fFf99.(1) ;;
let fFf95 = Sci.complex_of_sci fFf96 ;;
let fFf94 = Sci.approx_1024 ( Array.sub fFf99.(2) 0 5 ) ;;
let fFf93 = Sci.complex_of_sci fFf94 ;;
*)

let x12 = Matrix.float_of_matrix x9 ;;
let iteration = 7 ;;
let x13 = Matrix.float_principal iteration x12 [|1.;1.|] ;;
let x14 = Matrix.float_principal ( - iteration ) x12 [|1.;1.|] ;;
let x15 = Matrix.float_principal iteration x12 [|1.;-1.|] ;;
let x16 = Matrix.float_principal ( - iteration ) x12 [|1.;-1.|] ;;
let x17 = Matrix.float_principal iteration x12 x14.(1) ;; 
exp ( -. x14.(0).(0) ) -. ( 3. -. (sqrt 5.) ) /. 2. ;;
exp x17.(0).(0) -. ( 3. -. (sqrt 5.) ) /. 2. ;;
exp x13.(0).(0) -. ( 3. +. (sqrt 5.) ) /. 2. ;;

Matrix.matrix_foa_prod g7 g7 = Matrix.matrix_foa_prod g7 g7 ;;

let g21 = ( Matrix.square_foa_hash 2 ( Matrix.Float_matrix_cons ( Matrix.matrix_float_random 100 100 1. ) )  ( Matrix.Foa_vector_cons [|Matrix.Float_vector_cons [| 0. ; 0.|] |] ) ).(0) ;;

Matrix.matrix_foa_prod g21 g21 = Matrix.matrix_foa_prod g21 g21 ;;

let x18 = Matrix.ortho_float_bal_random 2 1. ;;
let x19 = Matrix.Float_matrix_cons x18 ;;
let x20 = Matrix.Foa_matrix_cons [| [| x19 ; x19 |] ; [| Matrix.foa_transpose x19 ; x19 |] |];;
Matrix.matrix_foa_prod x20 x20 ;;

let a0 = [| [|1.;2.;3.;4.;5.|] ; [|0.;0.;0.;0.;1.|] ; [|0.;1.;0.;1.;0.|] ; [|0.;0.;0.;0.;1.|] |] ;;
let a1 = Util.transpose a0 ;;

Matrix.numrows a0 ;;
Matrix.numcolumns a0 ;;
Matrix.float_rank a0;;
Matrix.numrows a1 ;;
Matrix.numcolumns a1 ;;
Matrix.float_rank a1 ;;
let a2 = Matrix.float_im a0 ;;
let a3 = Matrix.float_ker methode_reduc 1e-5 a0 ;;
(*
let a00 = Util.transpose a0 ;;
let a01 = Matrix.matrix_float_twisted_prod a00 a00 ;;
methode_reduc a01 ;;
methode_reduc x1 ;;
Matrix.sym_float_classical_jacobi_reduc 1e-5 1e-0 1e-20 100 x1 ;;
*)
Matrix.matrix_float_twisted_prod a0 a3 ;;
let a4 = Matrix.float_im a1 ;;
let a5 = Matrix.float_ker methode_reduc ( sqrt epsilon_float ) a1 ;;
Matrix.matrix_float_twisted_prod a1 a5 ;;

let a6 = [| [|0;0;0;0;1|] ; [|1;2;3;4;5|] ; [|0;0;0;0;1|] ; [|0;1;0;1;0|] |] ;;
Matrix.int_rank a6 ;;
Matrix.int_im a6 ;;

let a7 = Matrix.matrix_int_bal_random 5 15 99 ;;
let a8 = Array.append a7 a7 ;;
let a9 = Matrix.int_im a8 ;;
Matrix.int_rank a9 ;;

let a10 = Matrix.float_trans_orthonormalize a2 ;;
Matrix.matrix_float_twisted_prod a10 a10 ;;

let b0 = [|[|1.;1.|];[|1.;1.|]|];;
let methode_reduc0 = Matrix.sym_float_tune_reduc 1e-14 100000 ;;
let b1 = Matrix.sym_float_pinv methode_reduc0 1e-14 b0 ;;
Matrix.matrix_float_prod b1 b0 ;;
let b2 = Matrix.sym_float_sqrt methode_reduc0 b0 ;;
Matrix.matrix_float_prod b2 b2 ;;
Matrix.sym_float_apply methode_reduc0 ( function x -> x +. 2. ) b0 ;;
Matrix.sym_float_apply methode_reduc0 ( function x -> x ) b0 ;;

Matrix.matrix_float_commut zz yy;;
Matrix.matrix_float_twisted_commut_bis zz yy ;;
Matrix.matrix_float_twisted_commut zz yy;;



let s0 = Matrix.sym_float_bal_random 3 3. ;;
let s1 = Matrix.sym_float_adapt s0 ;;
let s2 = Matrix.sym_float_reduc ( sqrt ( epsilon_float ) ) 100 s0 ;;
let s3 = Matrix.sym_float_sort_reduc ( Matrix.sym_float_tune_reduc ( sqrt ( epsilon_float ) ) 100 ) s0 ;;
let s4 = Matrix.sym_float_reduc_seq 1e-10 100 s0 ;;
let s5 = Array.map Matrix.matrix_float_approx_bis s4 ;;
let s6 = Array.map Matrix.matrix_float_approx s4 ;;
Matrix.matrix_float_triple_prod s6.(1) s0 s6.(2) ;;

g_gg;;
g_ggg;;
g3;; 
g4;;
g15;;
g16;;
g17;;
q4;;
z6;;

Matrix.matrix_float_serret_frenet ( Matrix.vector_float_random_progression 10 1. ) ;;


Matrix.float_aitken_seki 0.999537131586786143 0.999845450393193 0.999948456937;;
let seq0 = [| 0.999537131586786143 ; 0.999845450393193 ; 0.999948456937 |] ;;
Matrix.float_aitken_seki_rec 1 0 seq0 ;;
let seq1 = [| 1.00093397534851669 ; 1.0003103531 ; 1.00010334391 ; 1.0000344361 ; 1.00001147738 |] ;;
(* The following result is false, probably because of rounding errors ? *)
Matrix.float_shanks2 seq1.(0) seq1.(1) seq1.(2) seq1.(3) seq1.(4) ;;
Matrix.float_aitken_seki_rec 2 0 seq1 ;;
Matrix.float_wynn 2 0 seq1 ;;
Matrix.float_wynn_rho 2 0 seq1 ;;
Matrix.float_brezinski 2 0 seq1 ;;
Matrix.float_approx seq1 ;;

let seq2 = [| [|1.00093397534851669; 1.88849435087711631e-21|] ; [|1.0003103531; 6.25561639667e-22|] ; [|1.00010334391; 2.08088496508e-22|] ;
 [|1.0000344361; 6.93150212666e-23|] ; [|1.00001147738; 2.30997019396e-23|] |] ;;

Matrix.vector_float_aitken_seki seq2.(0) seq2.(1) seq2.(2) ;;
Matrix.vector_float_aitken_seki_rec 2 0 seq2 ;;
Matrix.vector_float_wynn 2 0 seq2 ;;
Matrix.vector_float_wynn_rho 2 0 seq2 ;;
Matrix.vector_float_brezinski 2 0 seq2 ;;
Matrix.vector_float_approx seq2 ;;

let seq3 = [| [|[|1.00093397534851669; 1.88849435087711631e-21|] ; [|-1.88849435087711631e-21; 1.00093397534851669|]|] ;
[|[|1.0003103531; 6.25561639667e-22|] ; [|-6.25561639667e-22; 1.0003103531|]|] ;
[|[|1.00010334391; 2.08088496508e-22|] ; [|-2.08088496508e-22; 1.00010334391|]|] ;
[|[|1.0000344361; 6.93150212666e-23|] ; [|-6.93150212666e-23; 1.0000344361|]|] ;
[|[|1.00001147738; 2.30997019396e-23|] ; [|-2.30997019396e-23; 1.00001147738|]|] |] ;;

Matrix.matrix_float_aitken_seki_bis seq3.(0) seq3.(1) seq3.(2) ;;
Matrix.matrix_float_aitken_seki seq3.(0) seq3.(1) seq3.(2) ;;
Matrix.matrix_float_aitken_seki_rec_bis 2 0 seq3 ;;
Matrix.matrix_float_aitken_seki_rec 2 0 seq3 ;;
Matrix.matrix_float_wynn 2 0 seq3 ;;
Matrix.matrix_float_wynn_rho 2 0 seq3 ;;
Matrix.matrix_float_brezinski_bis 2 0 seq3 ;;
Matrix.matrix_float_brezinski 2 0 seq3 ;;
Matrix.matrix_float_approx seq3 ;;

0.00001147738 /. 0.00000000001955169 ;;




(*
La matrice mal conditionnée qui suit est dans
http://www.cs.berkeley.edu/~wkahan/Math128/FailMode.pdf
*)
let petit = 1e-6 ;;
let toutpetit = 1e-12 ;;
let essai = [| [| -2. ; 1. ; 1. |] ; [| 1. ; toutpetit ; toutpetit |] ; [| 1. ; toutpetit ; 0. |] |] ;;

let conditionnement = Matrix.float_cond Matrix.matrix_float_norm_inf Matrix.float_slow_inv essai ;;
let essai0 = Matrix.float_inv essai ;;
let essai1 = Matrix.float_slow_inv essai ;;

let essai2 = Matrix.slow_float_tune_inv Matrix.matrix_float_norm_inf essai essai0 ;;
let essai3 = Matrix.float_tune_inv Matrix.matrix_float_norm_inf essai essai0 ;;
let essai4 = Matrix.slow_float_approx_inv Matrix.matrix_float_norm_inf Matrix.float_slow_inv essai ;;
let essai5 = Matrix.float_approx_inv Matrix.matrix_float_norm_inf Matrix.float_slow_inv essai ;;
let essai6 = Matrix.slow_float_loop_approx_inv Matrix.matrix_float_norm_inf Matrix.float_slow_inv essai ;;
let essai7 = Matrix.slow_float_target_inv Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0.1 essai ;;
let essai8 = Matrix.float_loop_approx_inv Matrix.matrix_float_norm_inf Matrix.float_slow_inv essai ;;
let essai9 = Matrix.float_target_inv Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0.1 essai ;;

let change = Matrix.diag_float [| petit ; 1. /. petit ; 1. /. petit |] ;;
let antichange = Matrix.float_slow_inv change ;;
let autre_essai = Matrix.matrix_float_triple_prod change essai change ;;
let test = Matrix.matrix_float_triple_prod change ( Matrix.float_slow_inv autre_essai ) change ;;
let autre_conditionnement = Matrix.float_cond Matrix.matrix_float_norm_inf Matrix.float_slow_inv autre_essai ;;

Matrix.matrix_float_prod essai essai0 ;;
Matrix.matrix_float_prod essai0 essai ;;
Matrix.matrix_float_prod essai essai1 ;;
Matrix.matrix_float_prod essai1 essai ;;
Matrix.matrix_float_prod essai essai2.(0) ;;
Matrix.matrix_float_prod essai2.(0) essai ;;
Matrix.matrix_float_prod essai essai3.(0) ;;
Matrix.matrix_float_prod essai3.(0) essai ;;
Matrix.matrix_float_prod essai essai4.(0) ;;
Matrix.matrix_float_prod essai4.(0) essai ;;
Matrix.matrix_float_prod essai essai5.(0) ;;
Matrix.matrix_float_prod essai5.(0) essai ;;
Matrix.matrix_float_prod essai essai6.(0) ;;
Matrix.matrix_float_prod essai6.(0) essai ;;
Matrix.matrix_float_prod essai essai7.(0) ;;
Matrix.matrix_float_prod essai7.(0) essai ;;
Matrix.matrix_float_prod essai essai8.(0) ;;
Matrix.matrix_float_prod essai8.(0) essai ;;
Matrix.matrix_float_prod essai essai9.(0) ;;
Matrix.matrix_float_prod essai9.(0) essai ;;
Matrix.matrix_float_prod essai test ;;
Matrix.matrix_float_prod test essai ;;

let invertor = Matrix.float_inv ;;
let appl = function x -> ( Matrix.float_tune_inv Matrix.matrix_float_norm_inf essai x ).(0) ;;
let size = 5 ;;
let seq = Array.make size ( invertor essai ) ;;
for i = 1 to pred size do
 seq.(i) <- appl seq.( pred i ) 
done ;;

let essai10 = Matrix.matrix_float_approx_bis seq ;;
let essai11 = Matrix.matrix_float_approx seq ;;
let essai12 = Matrix.matrix_float_aitken_seki_rec_bis ( ( size - 1 ) / 2 ) 0 seq ;;
let essai13 = Matrix.matrix_float_wynn_rho 2 0 seq ;;
let essai14 = Matrix.matrix_float_brezinski 2 0 seq ;;
let essai15 = Matrix.matrix_float_brezinski_bis 2 0 seq ;;
Matrix.matrix_float_prod essai essai10 ;;
Matrix.matrix_float_prod essai essai11 ;;
Matrix.matrix_float_prod essai essai12 ;;
Matrix.matrix_float_prod essai essai13 ;;
Matrix.matrix_float_prod essai essai14 ;;
Matrix.matrix_float_prod essai essai15 ;;

let essai16 = Matrix.float_target_inv_seq Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0. essai ;;
let essai17 = Matrix.float_compensated_target_inv Matrix.matrix_float_approx_bis Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0. essai ;;
let essai18 = Matrix.float_compensated_target_inv Matrix.matrix_float_approx Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0. essai ;;
let essai19 = Matrix.float_compensated_target_inv ( Matrix.matrix_float_aitken_seki_rec_bis 1 1 ) Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0. essai ;;
let essai20 = Matrix.float_compensated_target_inv ( Matrix.matrix_float_wynn_rho 2 1 ) Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0. essai ;;
let essai21 = Matrix.float_compensated_target_inv ( Matrix.matrix_float_wynn_rho 2 1 ) Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0. essai ;;
let essai22 = Matrix.float_compensated_target_inv ( Matrix.matrix_float_brezinski 2 0 ) Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0. essai ;;
let essai23 = Matrix.float_compensated_target_inv ( Matrix.matrix_float_brezinski_bis 2 0 ) Matrix.matrix_float_norm_inf Matrix.float_slow_inv 0. essai ;;
Matrix.matrix_float_prod essai essai17 ;;
Matrix.matrix_float_prod essai essai18 ;;
Matrix.matrix_float_prod essai essai19 ;;
Matrix.matrix_float_prod essai essai20 ;;
Matrix.matrix_float_prod essai essai21 ;;
Matrix.matrix_float_prod essai essai22 ;;
Matrix.matrix_float_prod essai essai23 ;;

let step = sqrt epsilon_float ;;
let essai24 = Matrix.extrap_inv step essai ;;
Matrix.matrix_float_prod essai essai24 ;;

let essai25 = Matrix.aggressive_inv essai ;;
Matrix.matrix_float_prod essai essai25 ;;


