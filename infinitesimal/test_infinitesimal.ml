
Infinitesimal.low_stirling 69. ;;
Infinitesimal.up_stirling 69. ;;
Infinitesimal.stirling_bis 69. ;;

Infinitesimal.low_stirling 6. ;;
Infinitesimal.up_stirling 6. ;;
Infinitesimal.stirling_bis 6. ;;

let cp0 = [| 1. ; 1. ; 1. |] ;;
let p0 = Infinitesimal.float_polynomial_1 cp0 ;;
p0 1. ;;
p0 2. ;;

let p00 = Infinitesimal.float_jet ( Infinitesimal.float_richardson_binary_deriv 5 1e-3 ) 2 p0 ;;
let p01 = Infinitesimal.float_poly_coeff ( Infinitesimal.float_richardson_binary_deriv 5 1e-3 ) 2 p0 ;;
p00 0. ;;
p01 0. ;;

let cp1 = [| [| 1. ; 0. |] ; [| 1. ; 1. |] ; [| 1. ; 0. |] |] ;;
let p1 = Infinitesimal.float_polynomial_2 cp1 ;;
p1 1. 0. ;;
p1 1. 1. ;;
p1 2. 1. ;;

let p2 = Infinitesimal.float_rational_1 [| 1. ; 1. ; 1. |] [| 0. ; 1. |] ;;
p2 1. ;;
p2 2. ;;

let cp3 =  [| [| 0. |] ; [| 1. |] |] ;;
let p3 = Infinitesimal.float_rational_2 cp1 cp3 ;;
p3 1. 1. ;;
p3 2. 1. ;;




Infinitesimal.rotating_frame Infinitesimal.unit_circle Infinitesimal.quarterpi ;;

let c = Infinitesimal.float_approx_deriv 1e-4 sin ;;
let s = Infinitesimal.float_approx_deriv 1e-4  cos ;;
let e = Infinitesimal.float_approx_deriv 1e-4  exp ;;
let u = Infinitesimal.float_approx_deriv 1e-4  Util.float_identity  ;;
let z = Infinitesimal.float_approx_deriv 1e-4  Util.float_zero ;;
let zz = Infinitesimal.float_approx_deriv 1e-4  Util.float_one ;;

c 0. ;;
c 1. ;;
s 0. ;;
s 1. ;;
e 0. ;;
e 1. ;;
u 0. ;;
u 1. ;;
z 0. ;;
z 1. ;;
zz 0. ;; 
zz 1. ;;

let c1 = Infinitesimal.float_richardson_binary_deriv 1 1e-4 sin ;;
let s1 = Infinitesimal.float_richardson_binary_deriv 1 1e-4  cos ;;
let e1 = Infinitesimal.float_richardson_binary_deriv 1 1e-4  exp ;;
let u1 = Infinitesimal.float_richardson_binary_deriv 1 1e-4  Util.float_identity  ;;
let z1 = Infinitesimal.float_richardson_binary_deriv 1 1e-4  Util.float_zero ;;
let zz1 = Infinitesimal.float_richardson_binary_deriv 1 1e-4  Util.float_one ;;

c1 0. ;;
c1 1. ;;
s1 0. ;;
s1 1. ;;
e1 0. ;;
e1 1. ;;
u1 0. ;;
u1 1. ;;
z1 0. ;;
z1 1. ;;
zz1 0. ;; 
zz1 1. ;;

let c2 = Infinitesimal.float_richardson_binary_deriv 2 1e-4 sin ;;
let s2 = Infinitesimal.float_richardson_binary_deriv 2 1e-4  cos ;;
let e2 = Infinitesimal.float_richardson_binary_deriv 2 1e-4  exp ;;
let u2 = Infinitesimal.float_richardson_binary_deriv 2 1e-4  Util.float_identity  ;;
let z2 = Infinitesimal.float_richardson_binary_deriv 2 1e-4  Util.float_zero ;;
let zz2 = Infinitesimal.float_richardson_binary_deriv 2 1e-4  Util.float_one ;;

c2 0. ;;
c2 1. ;;
s2 0. ;;
s2 1. ;;
e2 0. ;;
e2 1. ;;
u2 0. ;;
u2 1. ;;
z2 0. ;;
z2 1. ;;
zz2 0. ;; 
zz2 1. ;;

Infinitesimal.float_richardson_deriv 2. 2 1e-4 exp 1. ;;
Infinitesimal.float_richardson_binary_deriv  2 1e-4 exp 1. ;;
Infinitesimal.float_richardson_deriv 3.  2 1e-4 exp 1. ;;
Infinitesimal.float_richardson_binary_deriv 3  1e-4 exp 1. ;;
Infinitesimal.float_richardson_deriv 3. 3 1e-4 exp 1. ;;
Infinitesimal.float_richardson_deriv 3. 3 1e-3 exp 1. ;;
Infinitesimal.float_richardson_deriv 3. 4 1e-3 exp 1. ;;
exp 1. ;;

Infinitesimal.float_richardson_deriv 3. 4 1e-3 Infinitesimal.heaviside_step (-1e-10) ;;
Infinitesimal.float_richardson_deriv 3. 3 1e-7 Infinitesimal.heaviside_step (-1e-10) ;;
Infinitesimal.float_richardson_binary_deriv 3 1e-7 Infinitesimal.heaviside_step (-1e-10) ;;
Infinitesimal.float_approx_deriv 1e-5 Infinitesimal.heaviside_step (-1e-10) ;;
Infinitesimal.float_approx_deriv 1e-10 Infinitesimal.heaviside_step (-1e-10) ;;

let f = function x -> [| x ; exp x |] ;;
let g = function x -> (f x).(1) ;;
Infinitesimal.float_approx_deriv 1e-3 g 1. ;; 

Infinitesimal.vector_speed ( Infinitesimal.float_approx_deriv 1e-3 ) f 1. ;;
Infinitesimal.vector_speed ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) f 1. ;;
Infinitesimal.vector_speed ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) f 1. ;;

let methode_reduc = Matrix.sym_float_tune_reduc 1e-3 1 ;;
let h = fun v -> v.(0) *. v.(0) *. v.(1) ** 3. ;;
let h0 = [| 3. ; 2. |] ;;
let gg = Infinitesimal.gradient ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) h h0 ;;
let hh = Infinitesimal.hess ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) h h0 ;;
let h1 = Infinitesimal.vector_float_halley_zero methode_reduc ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 h h0 ;;

let k = fun u -> ( exp u.(0) ) *. ( cos u.(1) ) ;;
let l = fun u -> [| h u ; k u |] ;;
Infinitesimal.tlm ( Infinitesimal.float_approx_deriv 1e-3 ) l h0 ;;
let m0 = [| [| 2. *. 3. *. 8. ; 3. *. 9. *. 4. |] ; [| (exp 3.) *. (cos 2.) ; (exp 3.) *. (-. sin 2.) |] |] ;;
Infinitesimal.div ( Infinitesimal.float_approx_deriv 1e-3 ) l h0 ;;
Infinitesimal.tlm ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) l h0 ;;
Infinitesimal.div ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) l h0 ;;
Infinitesimal.det_jac ( Infinitesimal.float_approx_deriv 1e-3 ) l h0 ;;
Infinitesimal.det_jac ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) l h0 ;;
Matrix.float_slow_det m0 ;;

let m = fun u -> [| ( exp u.(0) ) *. ( cos u.(1) ) *. u.(2) ; u.(2) ; u.(0) |] ;;
let w = [|3.;2.;4.|] ;;
Infinitesimal.rot_curl ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) m w ;;

let n = function x ->
 let y = x -. exp 1. in
  y *. y ;;

let o = Infinitesimal.float_jet ( Infinitesimal.float_approx_deriv 1e-3 ) 3 exp ;;
( o 1. ).(3) ;;

let methode_reduc0 = Matrix.sym_float_tune_reduc 1e-3 10 ;;

Infinitesimal.float_zero_householder ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 3 100 n 0. ;;
Infinitesimal.float_zero_householder ( Infinitesimal.float_richardson_deriv 2. 3 1e-4 ) 3 100 n 0. ;;
Infinitesimal.float_zero_halley ( Infinitesimal.float_richardson_deriv 2. 3 1e-5 ) 100 n 0. ;;
Infinitesimal.float_zero_newton ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 n 0. ;;
Infinitesimal.float_zero_secant 100 ( function x -> x -. exp 1. ) 0. ;;
Infinitesimal.float_zero_householder ( Infinitesimal.float_richardson_binary_deriv 3 1e-3 ) 3 100 n 0. ;;
Infinitesimal.float_zero_pot_pourri ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) 3 100 n 0. ;;
Infinitesimal.float_zero_general ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) 3 100 n 0. ;;

Infinitesimal.vector_zero_newton ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 l h0 ;;
Infinitesimal.desc_grad_zero ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 h h0 ;;
Infinitesimal.vector_float_zero_general ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 10 h h0 ;;
Infinitesimal.vector_float_zero_general_2 methode_reduc0 ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 10 h h0 ;;
Infinitesimal.desc_grad_zero ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 k h0 ;;
Infinitesimal.vector_float_zero_general ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 k h0 ;;
Infinitesimal.vector_float_zero_general_2 methode_reduc0 ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 k h0 ;;
Infinitesimal.vector_zero_general ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 l h0 ;;
Infinitesimal.vector_zero_general_2 methode_reduc0 ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 10 l [| 0. ; 1.4 |] ;;

Infinitesimal.acceleration ( Infinitesimal.float_approx_deriv 1e-3 ) f 1. ;;
Infinitesimal.acceleration ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) f 1. ;;
Infinitesimal.acceleration ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) f 1. ;;

let kappa = Infinitesimal.graph_curvature ( Infinitesimal.float_richardson_binary_deriv 3 1e-5 ) Infinitesimal.half_unit_circle ;;
kappa 0. ;;
kappa (-0.5) ;;
kappa 0.9 ;;

let kapp = Infinitesimal.curvature_2 ( Infinitesimal.float_richardson_binary_deriv 3 1e-5 ) Infinitesimal.unit_circle ;;
kapp 0. ;;
kapp 1. ;;
kapp 10. ;;

let cyc = function x -> Infinitesimal.cycloid 1. 1. x ;;
cyc Infinitesimal.pi ;;
let cy = Infinitesimal.developpee_2 ( Infinitesimal.float_richardson_binary_deriv 3 1e-5 ) cyc ;;
cy Infinitesimal.pi ;;

let kap = Infinitesimal.curvature ( Infinitesimal.float_richardson_binary_deriv 3 1e-5 ) ( function x -> Matrix.vector_float_scal_mult (-0.1) ( Infinitesimal.unit_circle ( (-10.) *. x ) ) ) ;;
kap 0. ;;
kap 1. ;;
kap 10. ;;

let sf = Infinitesimal.serret_frenet_3 ( Infinitesimal.float_richardson_binary_deriv 3 1e-3 ) ( Infinitesimal.helicoid 1. 1. ) ;;
sf 0. ;;
sf 1. ;;
sf 10. ;;

Matrix.float_trans_orthonormalize ( Infinitesimal.vector_jet ( Infinitesimal.float_approx_deriv 1e-3 ) 3 ( Infinitesimal.helicoid 1. 1. ) 0. ) ;;

let sf0 = Infinitesimal.serret_frenet ( Infinitesimal.float_approx_deriv 1e-3 ) ( Infinitesimal.helicoid 1. 1. ) ;;
sf0 0. ;;
sf0 1. ;;
sf0 10. ;;

let sf1 = Infinitesimal.serret_frenet ( Infinitesimal.float_richardson_binary_deriv 2 1e-3 ) Infinitesimal.unit_circle ;;
sf1 0. ;;
( sf1 0. ).(1) ;;

let mat0 = fun t -> [| Infinitesimal.cycloid 1. 1. t ; Infinitesimal.cardioid t |] ;;
let mat1 = Infinitesimal.matrix_speed ( Infinitesimal.float_richardson_binary_deriv 3 1e-3 ) mat0 ;;
let mat2 = Infinitesimal.matrix_acceleration ( Infinitesimal.float_richardson_binary_deriv 3 1e-3 ) mat0 ;;


let echant = Array.make 11 0. ;;
let echantill = Array.mapi ( fun i x -> 1e-1 *. float i ) echant ;;
let echantillon = Array.map Infinitesimal.half_unit_circle echantill ;;
Infinitesimal.float_discrete_diff 0.1 echantillon ;;
Infinitesimal.mean_float_discrete_diff 0.1 echantillon ;;
Infinitesimal.float_discrete_richardson_binary_diff 0 0.1 echantillon ;;
Infinitesimal.float_discrete_richardson_binary_diff 1 0.1 echantillon ;;
Infinitesimal.mean_float_discrete_richardson_binary_diff 0.1 echantillon ;;
Infinitesimal.float_discrete_richardson_binary_diff 2 0.1 echantillon ;;

let p = [| echantill ; echantillon |] ;;

let v0 = Infinitesimal.discrete_trans_vector_speed Infinitesimal.float_discrete_diff 0.1 p ;;
let v1 = Infinitesimal.discrete_trans_vector_speed ( Infinitesimal.float_discrete_richardson_binary_diff 1 ) 0.1 p ;;
let v2 = Infinitesimal.discrete_trans_vector_speed ( Infinitesimal.float_discrete_richardson_binary_diff 2 ) 0.1 p ;;
let v3 = Infinitesimal.discrete_trans_vector_speed ( Infinitesimal.float_discrete_richardson_diff 3 ) 0.1 p ;;
let v4 = Infinitesimal.discrete_trans_vector_speed Infinitesimal.mean_float_discrete_diff 0.1 p ;;
let v5 = Infinitesimal.discrete_trans_vector_speed Infinitesimal.mean_float_discrete_richardson_binary_diff 0.1 p ;;

let a0 = Infinitesimal.discrete_trans_acceleration Infinitesimal.float_discrete_diff 0.1 p ;;
let a1 = Infinitesimal.discrete_trans_acceleration ( Infinitesimal.float_discrete_richardson_binary_diff 1 ) 0.1 p ;;
let a2 = Infinitesimal.discrete_trans_acceleration ( Infinitesimal.float_discrete_richardson_binary_diff 2 ) 0.1 p ;;
let a3 = Infinitesimal.discrete_trans_acceleration ( Infinitesimal.float_discrete_richardson_diff 3 ) 0.1 p ;;
let a4 = Infinitesimal.discrete_trans_acceleration Infinitesimal.mean_float_discrete_diff 0.1 p ;;
let a5 = Infinitesimal.discrete_trans_acceleration Infinitesimal.mean_float_discrete_richardson_binary_diff 0.1 p ;;

let j0 = Infinitesimal.discrete_trans_jet Infinitesimal.float_discrete_diff 3 0.1 p ;;
let j1 = Infinitesimal.discrete_trans_jet ( Infinitesimal.float_discrete_richardson_binary_diff 1 ) 3 0.1 p ;;
let j2 = Infinitesimal.discrete_trans_jet ( Infinitesimal.float_discrete_richardson_binary_diff 2 ) 2 0.1 p ;;
let j3 = Infinitesimal.discrete_trans_jet ( Infinitesimal.float_discrete_richardson_diff 3 ) 3 0.1 p ;;
let j4 = Infinitesimal.discrete_trans_jet Infinitesimal.mean_float_discrete_diff 3 0.1 p ;;
let j5 = Infinitesimal.discrete_trans_jet Infinitesimal.mean_float_discrete_richardson_binary_diff 3 0.1 p ;;

let k0 = Infinitesimal.discrete_graph_curvature Infinitesimal.float_discrete_diff 0.1 echantillon ;;
let k1 = Infinitesimal.discrete_graph_curvature ( Infinitesimal.float_discrete_richardson_binary_diff 1 ) 0.1 echantillon ;;
let k2 = Infinitesimal.discrete_graph_curvature ( Infinitesimal.float_discrete_richardson_binary_diff 2 ) 0.1 echantillon ;;
let k3 = Infinitesimal.discrete_graph_curvature ( Infinitesimal.float_discrete_richardson_diff 3 ) 0.1 echantillon ;;
let k4 = Infinitesimal.discrete_graph_curvature Infinitesimal.mean_float_discrete_diff 0.1 echantillon ;;
let k5 = Infinitesimal.discrete_graph_curvature Infinitesimal.mean_float_discrete_richardson_binary_diff 0.1 echantillon ;;

let ech = [| Array.map cos echantill ; Array.map sin echantill |] ;;
let ka0 = Infinitesimal.discrete_trans_curvature_2 Infinitesimal.float_discrete_diff 0.1 ech ;;
let ka1 = Infinitesimal.discrete_trans_curvature_2 ( Infinitesimal.float_discrete_richardson_binary_diff 1 ) 0.1 ech ;;
let ka2 = Infinitesimal.discrete_trans_curvature_2 ( Infinitesimal.float_discrete_richardson_binary_diff 2 ) 0.1 ech ;;
let ka3 = Infinitesimal.discrete_trans_curvature_2 ( Infinitesimal.float_discrete_richardson_diff 3 ) 0.1 ech ;;
let ka4 = Infinitesimal.discrete_trans_curvature_2 Infinitesimal.mean_float_discrete_diff 0.1 ech ;;
let ka5 = Infinitesimal.discrete_trans_curvature_2 Infinitesimal.mean_float_discrete_richardson_binary_diff 0.1 ech ;;
let ka6 = Infinitesimal.discrete_trans_curvature Infinitesimal.mean_float_discrete_richardson_binary_diff 0.1 ech ;;

let ec = Array.make 101 0. ;;
let echa = Array.mapi ( fun i x -> 1e-2 *. float i ) ec ;;
let echan = [| Array.map cos echa ; Array.map sin echa |] ;;
let sefe0 = Infinitesimal.discrete_trans_serret_frenet Infinitesimal.mean_float_discrete_diff 0.1 echan ;;
let echantillo = Matrix.float_transpose echan ;;
let sefe1 = Infinitesimal.discrete_serret_frenet Infinitesimal.mean_float_discrete_diff 0.1 echantillo ;;
sefe1.(0) ;;



(*
let methode = Infinitesimal.float_zero_householder ( Infinitesimal.float_richardson_binary_deriv 2 1e-5 ) 2 100 ;;
let methode = Infinitesimal.float_zero_newton ( Infinitesimal.float_approx_deriv 1e-4 ) 100 ;;
let methode = Infinitesimal.float_zero_general ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) 3 100 ;;
*)
let methode = Infinitesimal.float_zero_general ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) 3 20 ;;

Infinitesimal.float_critical methode ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) Infinitesimal.half_unit_circle 0.5 ;;
Infinitesimal.vector_float_critical ( Infinitesimal.vector_zero_general ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) 100 ) ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) h h0 ;;
(*
Infinitesimal.vector_critical ( Infinitesimal.vector_float_zero_general ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) 5 ) ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) l [| 0.25 ; 0.25 |] ;;
*)

let mechant = function x -> 
 let y = abs_float x in
  match y +. x  with
  | 0. -> Infinitesimal.sin_inv x
  | _ -> 5.555555 *. Infinitesimal.sin_inv ( x *. 5.555555 ) ;;

Infinitesimal.halfpi -. Infinitesimal.float_int_monte_carlo 1000 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_rect 1000 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_trapez 1000 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_simpson 1000 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_romberg 10 4 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_romberg 11 4 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_monte_carlo 1000000 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_simpson 1000000 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_romberg 20 2 Infinitesimal.half_unit_circle (-1.) 1. ;;
(*
Infinitesimal.halfpi -. Infinitesimal.float_int_simpson 33554432 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_romberg 25 2 Infinitesimal.half_unit_circle (-1.) 1. ;;
*)
Infinitesimal.halfpi -. Infinitesimal.float_int_dichot_adapt ( Infinitesimal.float_int_simpson 100000 ) 1e-1 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_dichot_adapt ( Infinitesimal.float_int_romberg 20 2 ) 1e-1 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_trapez 100000 ) 30 1e-2 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_simpson 100000 ) 30 1e-2 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_romberg 17 3 ) 30 1e-2 Infinitesimal.half_unit_circle (-1.) 1. ;;
(*
Infinitesimal.halfpi -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_romberg 18 4 ) 10 1e-1 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_rect 4000000 ) 32 1e-1 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_simpson 1000000 ) 32 1e-9 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_trapez 3000000 ) 32 1e-1 Infinitesimal.half_unit_circle (-1.) 1. ;;
*)
Infinitesimal.halfpi -. Infinitesimal.float_int_adapt_trapez_simpson 100000 1e-4 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_adapt_trapez_simpson 1000000 1e-3 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 300 ) 1000000 1e-3 Infinitesimal.half_unit_circle (-1.) 1. ;;
(*
Cette dernière méthode est plutôt rapide et donne une erreur de -3.43947093028873496e-13
avec ces paramètres et cette fonction particuliers.
2. /. 3. -. Infinitesimal.float_int_adapt ( Infinitesimal.float_int_simpson 1000 ) 1000000 1e-4 sqrt 0. 1. ;;
n'est pas mal non plus.
Pour les fonctions plus délicates, même une méthode plus tortueuse comme ci-dessous
donne des résultats entre 1e-6 et 1e-8 ; dans cette méthode et avec cette fonction,
changer les paramètres pour affiner les calculs augmente l'erreur.
À condition d'être patient, Une méthode plus naïve comme
Infinitesimal.float_int_simpson 33554432 Infinitesimal.sin_inv (-1.) 1. ;;
Infinitesimal.float_int_simpson 1234567 Infinitesimal.sin_inv (-1.) 1. ;;
donne des erreurs largement inférieures à 1e-9.
Infinitesimal.float_int_simpson 12345678 mechant (-5.555555) 1. ;;
donne -1.07727123869181511e-06.
Les méthodes
Infinitesimal.float_int_trapez 1234567 Infinitesimal.sin_inv (-1.) 1. ;;
Infinitesimal.float_int_trapez 33554432 Infinitesimal.sin_inv (-1.) 1. ;;
Infinitesimal.float_int_romberg 15 2 Infinitesimal.sin_inv (-1.) 1. ;;
donnent encore mieux.
La méthode Infinitesimal.float_int_simpson donne des résultats acceptables sur les fonctions sauvages.
*)
Infinitesimal.float_int_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 100 ) 1000 1e-3 ) 10000 1e-1 Infinitesimal.sin_inv (-1.) 1. ;;
Infinitesimal.float_int_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 100 ) 1000 1e-3 ) 10000 1e-1 mechant (-5.555555) 1. ;;
Infinitesimal.float_int_trapez 1234567 Infinitesimal.sin_inv (-1.) 1. ;;
Infinitesimal.float_int_simpson 1234567 mechant (-5.555555) 1. ;;

let essaiScilab = function x ->
 let y = x -. 8. *. atan 1. in
  match y with
  | 0. -> 0.
  | _ -> x *. sin ( 30. *. x ) /. sqrt ( 1. -. ( ( x /. ( 8. *. atan 1. ) ) ** 2. ) ) ;;
let exactScilab = -2.5432596188 ;;
(*
exactScilab -. Infinitesimal.float_int_trapez 12345678 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 100 ) 1000 1e-3 ) 10000 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_romberg 22 2 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 300 ) 1000000 1e-3 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_trapez 33554432 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_simpson 12345678 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_simpson 1000000 ) 16 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_trapez 10000000 ) 4 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 300 ) 1000000 1e-3 ) 4 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 300 ) 10000 1e-3 ) 100 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 1000 ) 1000 1e-1 ) 1000 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 1000 ) 10000 1e-1 ) 6000 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
Cette dernière méthode donne -1.19952492383390563e-10 mais est assez longue.
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 100 ) 10000 1e-3 ) 3000 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
Cette dernière méthode donne 2.98636670947871607e-11 mais est moyennement longue.
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_trapez 1000 ) 10000 1e-3 ) 3000 3e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_simpson 100000 ) 10 1e-6 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_adapt ( Infinitesimal.float_int_simpson 1000 ) 100 1e-2 ) 10 1e-6 essaiScilab 0. ( 8. *. atan 1. ) ;;
exactScilab -. Infinitesimal.float_int_multi_adapt ( Infinitesimal.float_int_simpson 30000 ) 300 1e-1 essaiScilab 0. ( 8. *. atan 1. ) ;;
Cette dernière méthode donne -5.49237011426839672e-09 en un temps raisonnable.
*)
let seq = Array.make 11 0. ;;
seq.(0) <- Infinitesimal.float_int_trapez 100 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(1) <- Infinitesimal.float_int_trapez 316 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(2) <- Infinitesimal.float_int_trapez 1000 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(3) <- Infinitesimal.float_int_trapez 3162 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(4) <- Infinitesimal.float_int_trapez 10000 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(5) <- Infinitesimal.float_int_trapez 31623 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(6) <- Infinitesimal.float_int_trapez 100000 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(7) <- Infinitesimal.float_int_trapez 316228 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(8) <- Infinitesimal.float_int_trapez 1000000 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(9) <- Infinitesimal.float_int_trapez 3162277 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq.(10) <- Infinitesimal.float_int_trapez 10000000 essaiScilab 0. ( 8. *. atan 1. ) ;;
let seq0 = Matrix.float_approx ( Array.sub seq 2 9 ) ;;
let seq1 = Matrix.float_approx seq ;;
exactScilab -. seq1 ;;

let seq2 = Array.make 11 0. ;;
seq2.(0) <- Infinitesimal.float_int_simpson 100 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(1) <- Infinitesimal.float_int_simpson 316 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(2) <- Infinitesimal.float_int_simpson 1000 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(3) <- Infinitesimal.float_int_simpson 3162 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(4) <- Infinitesimal.float_int_simpson 10000 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(5) <- Infinitesimal.float_int_simpson 31623 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(6) <- Infinitesimal.float_int_simpson 100000 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(7) <- Infinitesimal.float_int_simpson 316228 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(8) <- Infinitesimal.float_int_simpson 1000000 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(9) <- Infinitesimal.float_int_simpson 3162277 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq2.(10) <- Infinitesimal.float_int_simpson 10000000 essaiScilab 0. ( 8. *. atan 1. ) ;;
let seq3 = Matrix.float_approx ( Array.sub seq2 2 9 ) ;;
let seq4 = Matrix.float_approx seq2 ;;
exactScilab -. seq4 ;;

let seq5 = Array.make 11 0. ;;
seq5.(0) <- Infinitesimal.float_int_romberg 7 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(1) <- Infinitesimal.float_int_romberg 8 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(2) <- Infinitesimal.float_int_romberg 9 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(3) <- Infinitesimal.float_int_romberg 10 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(4) <- Infinitesimal.float_int_romberg 11 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(5) <- Infinitesimal.float_int_romberg 12 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(6) <- Infinitesimal.float_int_romberg 13 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(7) <- Infinitesimal.float_int_romberg 14 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(8) <- Infinitesimal.float_int_romberg 15 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(9) <- Infinitesimal.float_int_romberg 16 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
seq5.(10) <- Infinitesimal.float_int_romberg 17 4 essaiScilab 0. ( 8. *. atan 1. ) ;;
let seq6 = Matrix.float_approx ( Array.sub seq5 2 9 ) ;;
let seq7 = Matrix.float_approx seq5 ;;
exactScilab -. seq7 ;;

Infinitesimal.float_int 8 8 essaiScilab 0. ( 8. *. atan 1. ) ;;
Infinitesimal.float_int 12 6 essaiScilab 0. ( 8. *. atan 1. ) ;;
Infinitesimal.float_int 9 9 essaiScilab 0. ( 8. *. atan 1. ) ;;
Infinitesimal.float_int 16 5 essaiScilab 0. ( 8. *. atan 1. ) ;;

Infinitesimal.halfpi -. Infinitesimal.float_int_romberg_trapez_adapt 1e-11 Infinitesimal.half_unit_circle (-1.) 1. ;;
exactScilab -. Infinitesimal.float_int_romberg_trapez_adapt 1e-7 essaiScilab 0. ( 8. *. atan 1. ) ;;
Infinitesimal.float_int_romberg_trapez_adapt 1e-10 Infinitesimal.sin_inv (-1.) 1. ;;
Infinitesimal.float_int_romberg_trapez_adapt 1e-1 mechant (-5.555555) 1. ;;
(* Cette dernière méthode donne des résultats intéressants assez rapidement.*)

Infinitesimal.halfpi -. Infinitesimal.float_int_romberg_trapez_bounded 10 1e-4 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_romberg_trapez_bounded 20 1e-4 Infinitesimal.half_unit_circle (-1.) 1. ;;

Infinitesimal.halfpi -. Infinitesimal.float_int_romberg_simpson_adapt 1e-13 Infinitesimal.half_unit_circle (-1.) 1. ;;
exactScilab -. Infinitesimal.float_int_romberg_simpson_adapt 1e-8 essaiScilab 0. ( 8. *. atan 1. ) ;;
Infinitesimal.float_int_romberg_simpson_adapt 1e-10 Infinitesimal.sin_inv (-1.) 1. ;;
Infinitesimal.float_int_romberg_simpson_adapt 1e-1 mechant (-5.555555) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_romberg_gauss_kronrod_adapt 1.2688e-4 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_romberg_gauss_kronrod_adapt 1.26879e-4 Infinitesimal.half_unit_circle (-1.) 1. ;;
exactScilab -. Infinitesimal.float_int_romberg_gauss_kronrod_adapt 1e-4 essaiScilab 0. ( 8. *. atan 1. ) ;;
Infinitesimal.float_int_romberg_gauss_kronrod_adapt 1e-1 Infinitesimal.sin_inv (-1.) 1. ;;
Infinitesimal.float_int_romberg_gauss_kronrod_adapt 1e-1 mechant (-5.555555) 1. ;;
(* Ces deux dernières méthodes donnent de bons résultats et rapidement, 
sauf sur des fonctions sauvages. *)
Infinitesimal.float_int 16 6 mechant (-5.555555) 1. ;;
Infinitesimal.float_int_simpson ( int_of_float ( 2. ** 22. ) ) mechant (-5.555555) 1. ;;
Infinitesimal.float_int_trapez ( int_of_float ( 2. ** 22. ) ) mechant (-5.555555) 1. ;;

Infinitesimal.float_simple_step_gauss_kronrod Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.float_simple_step_gauss_kronrod_generic Infinitesimal.gauss_kronrod_abscissae_21 Infinitesimal.gauss_kronrod_weights_21 Infinitesimal.half_unit_circle (-1.) 1. ;;

Infinitesimal.float_int_romberg_gauss_kronrod_adapt 1e-12 Infinitesimal.nsinc (-1000.) 1000. ;;
Infinitesimal.float_int_romberg_gauss_kronrod_adapt 1e-10 ( function x -> x ** (-0.5) ) (1e-35) 1. ;;
Infinitesimal.float_int_incr_rect 0.8 100 ( function x -> x ** (-0.5) ) (1e-3) 1. ;;
Infinitesimal.float_int_decr_trapez 0.9 100 ( function x -> x ** (-0.5) ) 1. (1e-3) ;;
Infinitesimal.float_int_incr_trapez 0.9 100 exp 0. (-10.) ;;
Infinitesimal.float_int_decr_rect 0.9 100 exp (-10.) 0. ;;

Infinitesimal.float_int_romberg_gauss_kronrod_adapt 1e-10 ( Infinitesimal.float_dirac_family_bell 10. ) (-1.) 1. ;;
Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_legendre_257_x Infinitesimal.gauss_legendre_257_w ( Infinitesimal.float_dirac_family_bell 100. ) (-1.) 1. ;;
Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_legendre_257_x Infinitesimal.gauss_legendre_257_w ( Infinitesimal.float_dirac_family_rectangle 10. ) (-1.) 1. ;;
Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_legendre_257_x Infinitesimal.gauss_legendre_257_w ( Infinitesimal.float_dirac_family_triangle 20. ) (-1.) 1. ;;

Infinitesimal.halfpi -. Infinitesimal.float_int_tanh 100 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.float_int_tanh 1000 Infinitesimal.heaviside_step (-13.) 6. ;;
Infinitesimal.halfpi -. Infinitesimal.float_int_tanh_sinh 5 Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.float_int_tanh_sinh 5 Infinitesimal.heaviside_step (-13.) 6. ;;

Infinitesimal.float_weighted_int_0_1 ( Infinitesimal.adams_bashforth_x 2 ) Infinitesimal.adams_bashforth_2_w Infinitesimal.half_unit_circle ;;
Infinitesimal.float_weighted_int_0_1 ( Infinitesimal.adams_bashforth_x 20 ) Infinitesimal.adams_bashforth_20_w Infinitesimal.half_unit_circle ;;
Infinitesimal.float_weighted_int_0_1 ( Infinitesimal.adams_bashforth_x 5 ) Infinitesimal.adams_bashforth_5_w Util.float_one ;;
Infinitesimal.float_weighted_int_0_1 ( Infinitesimal.adams_bashforth_x 20 ) Infinitesimal.adams_bashforth_20_w Util.float_one ;;

Infinitesimal.quarterpi -. Infinitesimal.float_weighted_int_0_1 Infinitesimal.gauss_legendre_0_1_8_x Infinitesimal.gauss_legendre_0_1_8_w Infinitesimal.half_unit_circle ;;


Infinitesimal.halfpi -. Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_patterson_255_x Infinitesimal.gauss_patterson_255_w Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_patterson_255_x Infinitesimal.gauss_patterson_255_w Infinitesimal.heaviside_step (-13.) 6. ;;

Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.clenshaw_curtis_129_x Infinitesimal.clenshaw_curtis_129_w Util.float_one (-13.) 6. ;;
Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.clenshaw_curtis_12_x Infinitesimal.clenshaw_curtis_12_w Util.float_one (-13.) 6. ;;
Infinitesimal.halfpi -. Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.clenshaw_curtis_129_x Infinitesimal.clenshaw_curtis_129_w Infinitesimal.half_unit_circle (-1.) 1. ;;
Infinitesimal.halfpi -. Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.clenshaw_curtis_12_x Infinitesimal.clenshaw_curtis_12_w Infinitesimal.half_unit_circle (-1.) 1. ;;




Infinitesimal.halfpi -. Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_legendre_12_x Infinitesimal.gauss_legendre_12_w Infinitesimal.half_unit_circle (-1.) 1. ;;

Infinitesimal.halfpi -. Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_legendre_257_x Infinitesimal.gauss_legendre_257_w Infinitesimal.half_unit_circle (-1.) 1. ;;

Infinitesimal.halfpi -. Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_patterson_127_x Infinitesimal.gauss_patterson_127_w Infinitesimal.half_unit_circle (-1.) 1. ;;


let methint = Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_legendre_257_x Infinitesimal.gauss_legendre_257_w ;;
Infinitesimal.float_int_double methint h [| -1.5 ; -1. |] [| 1. ; 1.5 |] ;; 
Infinitesimal.float_int_mult methint h [| -1.5 ; -1. |] [| 1. ; 1.5 |] ;; 

Infinitesimal.float_vector_int methint Infinitesimal.unit_circle 0. Infinitesimal.halfpi ;;

Infinitesimal.float_matrix_int methint ( function x -> ( sf1 x ).(1) ) 0. Infinitesimal.halfpi ;;

Infinitesimal.vector_int_mult methint l [| -1.5 ; -1. |] [| 1. ; 1.5 |] ;; 

Infinitesimal.vector_matrix_int_mult methint ( function x -> [| l x ; [| k x |] |] ) [| -1.5 ; -1. |] [| 1. ; 1.5 |] ;; 

let methint0 = Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_legendre_17_x Infinitesimal.gauss_legendre_17_w ;;
Infinitesimal.matrix_float_int_mult methint0 Matrix.float_trace [| [| 0. ; 0. |] ; [| 0. ; 0. |] |] [| [| 1. ; 1. |] ; [| 1. ; 1. |] |] ;;
Infinitesimal.matrix_float_int_mult ( Infinitesimal.float_int_trapez 20 ) Matrix.float_trace [| [| 0. ; 0. |] ; [| 0. ; 0. |] |] [| [| 1. ; 1. |] ; [| 1. ; 1. |] |] ;;
Infinitesimal.matrix_vector_int_mult ( Infinitesimal.float_int_trapez 20 ) ( function m -> m.(0) ) [| [| 0. ; 0. |] ; [| 0. ; 0. |] |] [| [| 1. ; 1. |] ; [| 1. ; 1. |] |] ;;
Infinitesimal.matrix_int_mult ( Infinitesimal.float_int_trapez 20 ) ( function m -> m ) [| [| 0. ; 0. |] ; [| 0. ; 0. |] |] [| [| 1. ; 1. |] ; [| 1. ; 1. |] |] ;;
Infinitesimal.matrix_int_mult ( Infinitesimal.float_int_monte_carlo 14 ) ( function m -> m ) [| [| 0. ; 0. |] ; [| 0. ; 0. |] |] [| [| 1. ; 1. |] ; [| 1. ; 1. |] |] ;;

let mc0 = Infinitesimal.float_int_mult_monte_carlo 316 Infinitesimal.unit_bowl_indic ( Array.make 2 0. ) 1. ;;
let mc1 = Infinitesimal.float_int_mult_monte_carlo 1000 Infinitesimal.unit_bowl_indic ( Array.make 2 0. ) 1. ;;
let mc2 = Infinitesimal.float_int_mult_monte_carlo 3162 Infinitesimal.unit_bowl_indic ( Array.make 2 0. ) 1. ;;
let mc3 = Infinitesimal.float_int_mult_monte_carlo 10000 Infinitesimal.unit_bowl_indic ( Array.make 2 0. ) 1. ;;
let mc4 = Infinitesimal.float_int_mult_monte_carlo 31622 Infinitesimal.unit_bowl_indic ( Array.make 2 0. ) 1. ;;
let mc5 = Infinitesimal.float_int_mult_monte_carlo 100000 Infinitesimal.unit_bowl_indic ( Array.make 2 0. ) 1. ;;
let mc6 = Infinitesimal.float_int_mult_monte_carlo 316228 Infinitesimal.unit_bowl_indic ( Array.make 2 0. ) 1. ;;
4. *. ( Infinitesimal.float_int_mult_monte_carlo 316228 Infinitesimal.unit_bowl_indic ( Array.make 2 0. ) 2. ) ;;
Matrix.float_aitken_seki mc0 mc1 mc2 ;;
Matrix.float_shanks2 mc0 mc1 mc2 mc3 mc4 ;;
Matrix.float_wynn 6 0 [| mc0 ; mc1 ; mc2 ; mc3 ; mc4 ; mc5 ; mc6 |] ;;
Matrix.float_approx [| mc0 ; mc1 ; mc2 ; mc3 ; mc4 ; mc5 ; mc6 |] ;;

let mc20 = Infinitesimal.float_int_mult_monte_carlo 316 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 1. ;;
let mc21 = Infinitesimal.float_int_mult_monte_carlo 1000 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 1. ;;
let mc22 = Infinitesimal.float_int_mult_monte_carlo 3162 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 1. ;;
let mc23 = Infinitesimal.float_int_mult_monte_carlo 10000 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 1. ;;
let mc24 = Infinitesimal.float_int_mult_monte_carlo 31622 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 1. ;;
let mc25 = Infinitesimal.float_int_mult_monte_carlo 100000 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 1. ;;
let mc26 = Infinitesimal.float_int_mult_monte_carlo 316228 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 1. ;;
(*
let mc27 = Infinitesimal.float_int_mult_monte_carlo 1000000 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 2. ;;
*)
Matrix.float_aitken_seki mc20 mc21 mc22 ;;
Matrix.float_shanks2 mc20 mc21 mc22 mc23 mc24 ;;
Matrix.float_wynn 6 0 [| mc20 ; mc21 ; mc22 ; mc23 ; mc24 ; mc25 ; mc26 |] ;;
Matrix.float_approx [| mc20 ; mc21 ; mc22 ; mc23 ; mc24 ; mc25 ; mc26 |] ;;

(* V(S(n,r)) = r * V(S(n-1,r)) * int_{-pi/2}^{pi/2} (cos t)^n dt 
V(B(n,r) = int_0^r V(S(n-1,r)) dr
V(S(2,1)) = 4pi , V(S(3,1) = 2 pi^2 , V(B(3,1) = 4 pi / 3 , V(S(4,1) = 8 pi^2 / 3 , V(B(4,1) = pi^2 / 2 , V(B(5,1) = 8 pi^2 / 15.
*) 
let mc28 = Infinitesimal.float_compensated_int_mult_monte_carlo Matrix.float_approx 7 0.8 100000 Infinitesimal.unit_bowl_indic ( Array.make 3 0. ) 1. ;;
let mc99 = Infinitesimal.float_int_mult_monte_carlo 1000000 Infinitesimal.unit_bowl_indic ( Array.make 5 0. ) 1. ;;
15. *. mc99 /. ( 8. *. Infinitesimal.pi ** 2. ) ;; 

Infinitesimal.halfpi -. Infinitesimal.float_discrete_int_rect 2. echantillon ;;
Infinitesimal.halfpi -. Infinitesimal.float_discrete_int_trapez 2. echantillon ;;
Infinitesimal.halfpi -. Infinitesimal.float_discrete_int_simpson 2. echantillon ;;

Infinitesimal.float_local_inverse methode 0.5 cos 0. ;;
Infinitesimal.float_flat_search methode ( Infinitesimal.float_int_trapez 100 ) Infinitesimal.gauss_bell 20. 1. ;;
Infinitesimal.vector_local_inverse ( Infinitesimal.vector_zero_general ( Infinitesimal.float_richardson_binary_deriv 1 1e-3 ) 10 ) [| 0.3 ; 1.3 |] l [| 0.9 ; 0.1 |] ;;
Infinitesimal.float_implicit_function methode 0.5 ( fun x y -> ( cosh x ) *. ( cos y ) -. tanh x +. tanh y ) 1. ;;
let m0 = fun x y -> Matrix.vector_float_plus [| Infinitesimal.pi ; Infinitesimal.sqrt_of_2 |] ( Matrix.vector_float_coeff_prod x y ) ;;
Infinitesimal.vector_implicit_function ( Infinitesimal.vector_zero_general ( Infinitesimal.float_richardson_binary_deriv 1 1e-3 ) 100 ) h0 m0 h0 ;;


let c0 = Infinitesimal.float_fourier_coefficient ( Infinitesimal.float_int_trapez 10000 ) cos 1 ;;
let c1 = Infinitesimal.float_fourier_coefficient_general ( Infinitesimal.float_int_trapez 10000 ) 0. ( 8. *. atan 1. ) cos 1 ;;
let c2 = Infinitesimal.float_fourier_series [| [|0.;0.|] ; c0 ; c1 |] ( Infinitesimal.quarterpi /. 2. ) ;;

let f0 = Infinitesimal.float_fourier_transform ( Infinitesimal.float_int_simpson 1234 ) (-1e1) 1e1 Infinitesimal.gauss_bell ;;
let f1 = Infinitesimal.float_inv_fourier_transform ( Infinitesimal.float_int_simpson 1234 ) (-1e1) 1e1 f0 ;;
Infinitesimal.gauss_bell 0.5 ;;
f1 0.5 ;;
Infinitesimal.gauss_bell 0. ;;
f1 0. ;;

let l0 = Infinitesimal.float_laplace_real ( Infinitesimal.float_int_simpson 1234 ) 10. exp ;;
0.25 -. l0 5. ;;
let l1 = Infinitesimal.float_laplace_complex ( Infinitesimal.float_int_simpson 321 ) 15. exp ;;
let l2 = Infinitesimal.float_inv_laplace_complex ( Infinitesimal.float_int_simpson 321 ) (1.) 15. l1 ;;
l2 1. ;;

let s0 = Infinitesimal.float_sumudu_real ( Infinitesimal.float_int_simpson 1234 ) 100. Infinitesimal.heaviside_step ;;
s0 1. ;;

let z0 = Array.make 35 1. ;;
Infinitesimal.discrete_float_causal_z_transform z0 [| 0. ; 1. |] ;;
Infinitesimal.discrete_float_causal_fourier_transform z0 Infinitesimal.halfpi ;;
Infinitesimal.discrete_float_symmetric_z_transform z0 [| 0. ; 1. |] ;;
Infinitesimal.discrete_float_symmetric_fourier_transform z0 Infinitesimal.halfpi ;;
let z1 = function omega -> Infinitesimal.discrete_float_symmetric_fourier_transform z0 omega ;;
let z2 = Infinitesimal.float_inv_fourier_transform ( Infinitesimal.float_int_simpson 1234 ) (-17.) (17.) z1 ;;
Array.map z2 ( Matrix.float_closed_equal_subdivision (-17.) 35 17. ) ;;
Array.map z2 ( Matrix.float_closed_equal_subdivision ( -. Infinitesimal.pi ) 10 Infinitesimal.pi ) ;;

(*
let q = fun t y -> -15. *. y ;;
*)
let q = fun t y -> y ;;

let mid_point_un = Infinitesimal.float_ode_mid_point q 100 1. 0. 4. ;;
let rk4_un = Infinitesimal.float_ode_rk4 q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_adams_bashforth_2 q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_adams_bashforth 2 q 100 1. 0. 4. ;;

( Infinitesimal.float_ode_adams_bashforth_3 q 100 1. 0. 4. ).(100) ;;
Infinitesimal.float_end_ode_adams_bashforth_3 q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_adams_bashforth 3 q 100 1. 0. 4. ;;

Infinitesimal.float_end_ode_mid_point q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_rk4 q 100 1. 0. 4. ;;

Infinitesimal.float_ode_euler q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_euler q 100 1. 0. 4. ;;

let o0 = Infinitesimal.float_ode_runge_kutta Infinitesimal.rk2_a Infinitesimal.rk2_b_fine q 100 1. 0. 4. ;;
let o0e = Infinitesimal.float_end_ode_runge_kutta Infinitesimal.rk2_a Infinitesimal.rk2_b_fine q 100 1. 0. 4. ;;
let o1 = Infinitesimal.float_ode_runge_kutta Infinitesimal.bogacki_shampine_a Infinitesimal.bogacki_shampine_b_fine q 100 1. 0. 4. ;;
let o2 = Infinitesimal.float_ode_runge_kutta Infinitesimal.cash_karp_a Infinitesimal.cash_karp_b_fine q 100 1. 0. 4. ;;
let o3 = Infinitesimal.float_ode_runge_kutta Infinitesimal.dormand_prince_4_5_a Infinitesimal.dormand_prince_4_5_b_fine q 100 1. 0. 4. ;;
let o4 = Infinitesimal.float_ode_runge_kutta Infinitesimal.runge_kutta_fehlberg_a Infinitesimal.runge_kutta_fehlberg_b_fine q 100 1. 0. 4. ;;
let o5 = Infinitesimal.float_ode_runge_kutta Infinitesimal.hem_4_5_a Infinitesimal.hem_4_5_b q 100 1. 0. 4. ;;
let o6 = Infinitesimal.float_ode_runge_kutta Infinitesimal.rk5_kutta_first_a Infinitesimal.rk5_kutta_first_b q 100 1. 0. 4. ;;
let o7 = Infinitesimal.float_ode_runge_kutta Infinitesimal.rk5_kutta_second_a Infinitesimal.rk5_kutta_second_b q 100 1. 0. 4. ;;
let o8 = Infinitesimal.float_ode_runge_kutta Infinitesimal.rk5_cassity_a Infinitesimal.rk5_cassity_b q 100 1. 0. 4. ;;
let o9 = Infinitesimal.float_ode_runge_kutta Infinitesimal.rk6_butcher_a Infinitesimal.rk6_butcher_b q 100 1. 0. 4. ;;
let mid_point_deux = Infinitesimal.float_ode_runge_kutta Infinitesimal.mid_point_a Infinitesimal.mid_point_b q 100 1. 0. 4. ;;
let rk4_deux = Infinitesimal.float_ode_runge_kutta Infinitesimal.rk4_a Infinitesimal.rk4_b q 100 1. 0. 4. ;;

Infinitesimal.float_end_ode_runge_kutta Infinitesimal.rk6_4_cash669_second_a Infinitesimal.rk6_4_cash669_second_b_fine q 100 1. 0. 4. ;;
Infinitesimal.float_ode_runge_kutta Infinitesimal.rk5_4_cash669_first_a Infinitesimal.rk5_4_cash669_first_b_fine q 100 1. 0. 4. ;;
(*
( exp (-4.) ) *. ( Infinitesimal.float_end_ode_runge_kutta Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine q 100 1. 0. 4. ) -. 1. ;;
*)


Infinitesimal.float_ode_runge_kutta_adapt Infinitesimal.rk2_a Infinitesimal.rk2_b_fine Infinitesimal.rk2_b_raw 1e-4 q 100 1. 0. 4. ;;
Infinitesimal.float_ode_runge_kutta_bounded 2 Infinitesimal.rk2_a Infinitesimal.rk2_b_fine Infinitesimal.rk2_b_raw 1e-4 q 100 1. 0. 4. ;;
Infinitesimal.float_ode_runge_kutta_adapt Infinitesimal.cash_karp_a Infinitesimal.cash_karp_b_fine Infinitesimal.cash_karp_b_raw 3e-16 q 100 1. 0. 4. ;;
(*
Cette dernière méthode donne pour l'équation y' = y :
exp 4. avec une précision relative de 15 chiffres ou 50 bits,
et pour l'équation y' = -15 y :
exp (-60.) avec une précision relative de 13 chiffres ou 43 bits.
Pour l'équation y' = -15 y en passant à 1000 pas au lieu de 100, 
on obtient 46 bits et presque 14 chiffres.
*)
(*
let z = Infinitesimal.float_ode_runge_kutta_adapt Infinitesimal.cash_karp_a Infinitesimal.cash_karp_b_fine Infinitesimal.cash_karp_b_raw 3e-16 q 1000 1. 0. 4. ;;
let a2 = z.(1000) /. exp (-60.) -. 1. ;;
log (abs_float a2) /. log 2. ;;
*)


let err0 = ( exp (-4. ) ) *. ( Infinitesimal.float_end_ode_runge_kutta_bounded 2 Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-2 ( fun x y -> y ) 35 1. 0. 4. ) -. 1. ;;

(* tolérance 1e-20 ou tolérance 0. c'est pareil !! *)
let err1 = ( exp (-4. ) ) *. ( Infinitesimal.float_end_ode_runge_kutta_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ( fun x y -> y ) 35 1. 0. 4. ) -. 1. ;;
let err2 = ( exp 60. ) *. ( Infinitesimal.float_end_ode_runge_kutta_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 0. ( fun t y -> -15. *. y ) 36 1. 0. 4. ) -. 1. ;;
-. log ( abs_float err1) /. ( log 2. ) ;;
-. log ( abs_float err2) /. ( log 2. ) ;;
(* On a obtenu respectivement 52 bits, plus de 15 chiffres, i. e. epsilon_float ; et 48 bits, plus de 14 hiffres, i. e. 13 *. epsilon_float. *)

let oo0 = Infinitesimal.float_ode_back_euler methode q 100 1. 0. 4. ;;
let oo1 = Infinitesimal.float_ode_trapezoid methode q 100 1. 0. 4. ;;
let oo2 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.rk1_gauss_a Infinitesimal.rk1_gauss_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl methode Infinitesimal.rk1_gauss_a Infinitesimal.rk1_gauss_b q 100 1. 0. 4. ;;
let oo3 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.rk2_burrage_a Infinitesimal.rk2_burrage_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl methode Infinitesimal.rk2_burrage_a Infinitesimal.rk2_burrage_b q 100 1. 0. 4. ;;
let oo4 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.imex_ssp2_3_2_2_impl_a Infinitesimal.imex_ssp2_3_2_2_impl_b q 100 1. 0. 4. ;;
let oo5 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.mod_ext_bdf_a Infinitesimal.mod_ext_bdf_b q 100 1. 0. 4. ;;
let oo6 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.rk5_try_a Infinitesimal.rk5_try_b q 100 1. 0. 4. ;;
let oo7 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.lambda_example_a Infinitesimal.lambda_example_b q 100 1. 0. 4. ;;
let oo8 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.rk2_lobattoIIIA_a Infinitesimal.rk2_lobattoIIIA_b q 100 1. 0. 4. ;;
let oo9 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.rk2_lobattoIIIB_a Infinitesimal.rk2_lobattoIIIB_b q 100 1. 0. 4. ;;
let oo10 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.rk2_lobattoIIICstar_a Infinitesimal.rk2_lobattoIIICstar_b q 100 1. 0. 4. ;;
let oo11 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.rk4_lobattoIIICstar_a Infinitesimal.rk4_lobattoIIICstar_b q 100 1. 0. 4. ;;
let oo12 = Infinitesimal.float_ode_runge_kutta_impl methode Infinitesimal.rk2_sdirk_3A_a Infinitesimal.rk2_sdirk_3A_b q 100 1. 0. 4. ;;

let oo13 = ( Infinitesimal.float_ode_adams_moulton_2 methode q 10 1. 0. 4. ).(10) ;;
Infinitesimal.float_end_ode_adams_moulton_2 methode q 10 1. 0. 4. ;;
let oo14 = ( Infinitesimal.float_ode_milne_simpson_2 methode q 10 1. 0. 4. ).(10) ;;
Infinitesimal.float_end_ode_milne_simpson_2 methode q 10 1. 0. 4. ;;
let oo15 = ( Infinitesimal.float_ode_bdf_2 methode q 10 1. 0. 4. ).(10) ;;
Infinitesimal.float_end_ode_bdf_2 methode q 10 1. 0. 4. ;;
let oo16 = ( Infinitesimal.float_ode_bdf_3 methode q 10 1. 0. 4. ).(10) ;;
Infinitesimal.float_end_ode_bdf_3 methode q 10 1. 0. 4. ;;


( Infinitesimal.float_ode_adapt ( Infinitesimal.float_ode_back_euler methode ) 1e-3 q 100 1. 0. 4. ).(100) ;;
Infinitesimal.float_end_ode_adapt ( Infinitesimal.float_end_ode_back_euler methode ) 1e-3 q 100 1. 0. 4. ;;
( Infinitesimal.float_ode_bounded 2 ( Infinitesimal.float_ode_back_euler methode ) 1e-3 q 100 1. 0. 4. ).(100) ;;
Infinitesimal.float_end_ode_bounded 2 ( Infinitesimal.float_end_ode_back_euler methode ) 1e-3 q 100 1. 0. 4. ;;


Infinitesimal.float_ode_adapt ( Infinitesimal.float_ode_euler ) 1e-7 q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_adapt ( Infinitesimal.float_end_ode_euler ) 1e-7 q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_bounded 2 ( Infinitesimal.float_end_ode_euler ) 1e-7 q 100 1. 0. 4. ;;
Infinitesimal.float_ode_adapt ( Infinitesimal.float_ode_trapezoid methode ) 1e-5 q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_bounded 2 ( Infinitesimal.float_end_ode_trapezoid methode ) 1e-5 q 100 1. 0. 4. ;;
Infinitesimal.float_ode_adapt ( Infinitesimal.float_ode_runge_kutta_adapt Infinitesimal.cash_karp_a Infinitesimal.cash_karp_b_fine Infinitesimal.cash_karp_b_raw 1e-1 ) 1e-2 q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_bounded 2 ( Infinitesimal.float_end_ode_runge_kutta_adapt Infinitesimal.cash_karp_a Infinitesimal.cash_karp_b_fine Infinitesimal.cash_karp_b_raw 1e-1 ) 1e-2 q 100 1. 0. 4. ;;

let ooo0 = Infinitesimal.float_ode_runge_kutta_impl_multi ( Infinitesimal.vector_zero_general  ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 ) Infinitesimal.rk4_lobattoIIICstar_a Infinitesimal.rk4_lobattoIIICstar_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl_multi ( Infinitesimal.vector_zero_general  ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 ) Infinitesimal.rk4_lobattoIIICstar_a Infinitesimal.rk4_lobattoIIICstar_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl_multi ( Infinitesimal.vector_zero_general  ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 ) Infinitesimal.trapezoid_a Infinitesimal.trapezoid_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl_multi ( Infinitesimal.vector_zero_general  ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 ) Infinitesimal.rk5_radauIA_a Infinitesimal.rk5_radauIA_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl_multi ( Infinitesimal.vector_zero_general  ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 ) Infinitesimal.rk4_lobattoIIICstar_a Infinitesimal.rk4_lobattoIIICstar_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl_multi ( Infinitesimal.vector_zero_general  ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 ) Infinitesimal.example_DESIRE_a Infinitesimal.example_DESIRE_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl_multi ( Infinitesimal.vector_zero_general  ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 ) Infinitesimal.rk6_lobattoIIIA_a Infinitesimal.rk6_lobattoIIIA_b q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta_impl_multi ( Infinitesimal.vector_zero_general  ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 100 ) Infinitesimal.rk6_lobattoIIIB_a Infinitesimal.rk6_lobattoIIIB_b q 100 1. 0. 4. ;;

let r = fun x y z -> z -. y ;;
let methode_zero = ( Infinitesimal.float_zero_general ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 2 10 ) ;;
let r0 = Infinitesimal.float_ode_implicit ( Infinitesimal.float_ode_trapezoid methode_zero ) methode_zero r 3. 100 1. 0. 4. ;;
let r1 = Infinitesimal.float_ode_implicit ( Infinitesimal.float_end_ode_trapezoid methode_zero ) methode_zero r 3. 100 1. 0. 4. ;;

mid_point_un = mid_point_deux ;;
exp 4. ;;
exp (-60.) ;;

let v00 = [| 1. ; 1. |] ;;
let q0 = fun (x:float) (y:float array) -> y ;;

let m00 = [| [| 1. ; 0. |] ; [| 0. ; 1. |] |] ;;
let anti = [| [| 0. ; -1. |] ; [| 1. ; 0. |] |] ;;
let q1 = fun (x:float) (y:float array array) -> Matrix.matrix_float_prod anti y ;;
let q2 = fun (x:float) (y:float array array) -> Matrix.matrix_float_plus ( Matrix.matrix_float_prod y anti ) ( Matrix.matrix_float_prod anti y ) ;;

let oo17 = Infinitesimal.vector_end_ode_euler q0 100 v00 0. 4. ;;
let oo18 = Infinitesimal.matrix_end_ode_euler q1 100 m00 0. Infinitesimal.halfpi ;;

let oo19 = Infinitesimal.vector_end_ode_adams_bashforth_2 q0 100 v00 0. 4. ;;
let oo19 = ( Infinitesimal.vector_ode_adams_bashforth_2 q0 100 v00 0. 4. ).(100) ;;

Infinitesimal.vector_end_ode_adams_bashforth 2 q0 100 v00 0. 4. ;;
( Infinitesimal.vector_ode_adams_bashforth 2 q0 100 v00 0. 4. ).(100) ;;

let oo20 = Infinitesimal.matrix_end_ode_adams_bashforth_2 q1 100 m00 0. Infinitesimal.halfpi ;;
let oo20 = ( Infinitesimal.matrix_ode_adams_bashforth_2 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;

Infinitesimal.matrix_end_ode_adams_bashforth 2 q1 100 m00 0. Infinitesimal.halfpi ;;
( Infinitesimal.matrix_ode_adams_bashforth 2 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;

let oo21 = Infinitesimal.vector_end_ode_adams_bashforth_3 q0 100 v00 0. 4. ;;
let oo21 = ( Infinitesimal.vector_ode_adams_bashforth_3 q0 100 v00 0. 4. ).(100) ;;

Infinitesimal.vector_end_ode_adams_bashforth 3 q0 100 v00 0. 4. ;;
( Infinitesimal.vector_ode_adams_bashforth 3 q0 100 v00 0. 4. ).(100) ;;

let oo22 = Infinitesimal.matrix_end_ode_adams_bashforth_3 q1 100 m00 0. Infinitesimal.halfpi ;;
let oo22 = ( Infinitesimal.matrix_ode_adams_bashforth_3 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;

Infinitesimal.matrix_end_ode_adams_bashforth 3 q1 100 m00 0. Infinitesimal.halfpi ;;
( Infinitesimal.matrix_ode_adams_bashforth 3 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;

Infinitesimal.matrix_end_ode_adams_bashforth 4 q1 100 m00 0. Infinitesimal.halfpi ;;
( Infinitesimal.matrix_ode_adams_bashforth 4 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;


(*
let npas = 100 ;;
Infinitesimal.vector_end_ode_adams_bashforth_2 q0 npas v00 0. (float npas) ;;
Infinitesimal.vector_ode_adams_bashforth_2 q0 npas v00 0. (float npas) ;;

Infinitesimal.vector_end_ode_adams_bashforth_3 q0 npas v00 0. (float npas) ;;
Infinitesimal.vector_ode_adams_bashforth_3 q0 npas v00 0. (float npas) ;;

Infinitesimal.float_end_ode_adams_bashforth_2 q npas 1. 0. 4. ;;
( Infinitesimal.float_ode_adams_bashforth_2 q npas 1. 0. 4. ).(npas) ;;
( Infinitesimal.float_ode_adams_bashforth 2 q npas 1. 0. 4. ).(npas) ;;
Infinitesimal.float_end_ode_adams_bashforth 2 q npas 1. 0. 4. ;;
Infinitesimal.float_end_ode_adams_bashforth_3 q npas 1. 0. 4. ;;
( Infinitesimal.float_ode_adams_bashforth_3 q npas 1. 0. 4. ).(npas) ;;
( Infinitesimal.float_ode_adams_bashforth 3 q npas 1. 0. 4. ).(npas) ;;
Infinitesimal.float_end_ode_adams_bashforth 3 q npas 1. 0. 4. ;;
( Infinitesimal.float_ode_adams_bashforth 4 q npas 1. 0. 4. ).(npas) ;;
Infinitesimal.float_end_ode_adams_bashforth 4 q npas 1. 0. 4. ;;
*)

let npas = 100 ;;
let oo22 = Infinitesimal.matrix_end_ode_adams_bashforth_3 q1 npas m00 0. Infinitesimal.halfpi ;;
let oo22 = ( Infinitesimal.matrix_ode_adams_bashforth_3 q1 npas m00 0. Infinitesimal.halfpi ).(npas) ;;

let oo23 = ( Infinitesimal.float_ode_adams_bashforth_2 q 1000 1. 0. 4. ).(1000) ;;
let oo24 = Infinitesimal.float_end_ode_adams_bashforth_2 q 1000 1. 0. 4. ;;

let oo25 = ( Infinitesimal.float_ode_adams_bashforth_3 q 100 1. 0. 4. ).(100) ;;
let oo26 = Infinitesimal.float_end_ode_adams_bashforth_3 q 100 1. 0. 4. ;;


Infinitesimal.float_ode_nystroem_3 q 100 1. 0. 4. ;;
Infinitesimal.float_end_ode_nystroem_3 q 100 1. 0. 4. ;;
( Infinitesimal.vector_ode_nystroem_3 q0 100 v00 0. 4. ).(100) ;;
Infinitesimal.vector_end_ode_nystroem_3 q0 100 v00 0. 4. ;;
( Infinitesimal.matrix_ode_nystroem_3 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;
Infinitesimal.matrix_end_ode_nystroem_3 q1 100 m00 0. Infinitesimal.halfpi ;;

( mid_point_un ).(100) ;;
( mid_point_deux ).(100) ;;
Infinitesimal.float_end_ode_mid_point q 100 1. 0. 4. ;;
( Infinitesimal.vector_ode_mid_point q0 100 v00 0. 4. ).(100) ;;
Infinitesimal.vector_end_ode_mid_point q0 100 v00 0. 4. ;;
( Infinitesimal.matrix_ode_mid_point q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;
Infinitesimal.matrix_end_ode_mid_point q1 100 m00 0. Infinitesimal.halfpi ;;

( rk4_un ).(100) ;;
( rk4_deux ).(100) ;;
( Infinitesimal.vector_ode_rk4 q0 100 v00 0. 4. ).(100) ;;
Infinitesimal.vector_end_ode_rk4 q0 100 v00 0. 4. ;;
( Infinitesimal.matrix_ode_rk4 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;
Infinitesimal.matrix_end_ode_rk4 q1 100 m00 0. Infinitesimal.halfpi ;;

Infinitesimal.float_ode_runge_kutta Infinitesimal.rk2_a Infinitesimal.rk2_b_fine q 100 1. 0. 4. ;;
Infinitesimal.vector_ode_runge_kutta Infinitesimal.rk2_a Infinitesimal.rk2_b_fine q0 100 v00 0. 4. ;;
Infinitesimal.float_end_ode_runge_kutta Infinitesimal.rk2_a Infinitesimal.rk2_b_fine q 100 1. 0. 4. ;;
( Infinitesimal.vector_ode_runge_kutta Infinitesimal.rk2_a Infinitesimal.rk2_b_fine q0 100 v00 0. 4. ).(100) ;;

Infinitesimal.float_end_ode_runge_kutta Infinitesimal.mid_point_a Infinitesimal.mid_point_b q 100 1. 0. 4. ;;
( Infinitesimal.vector_ode_runge_kutta Infinitesimal.mid_point_a Infinitesimal.mid_point_b q0 100 v00 0. 4. ).(100) ;;
Infinitesimal.vector_end_ode_runge_kutta Infinitesimal.mid_point_a Infinitesimal.mid_point_b q0 100 v00 0. 4. ;;

( Infinitesimal.matrix_ode_runge_kutta Infinitesimal.mid_point_a Infinitesimal.mid_point_b q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;
Infinitesimal.matrix_end_ode_runge_kutta Infinitesimal.mid_point_a Infinitesimal.mid_point_b q1 100 m00 0. Infinitesimal.halfpi ;;


( Infinitesimal.float_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ( fun x y -> y ) 35 1. 0. 4. ).(35) ;;
( exp (-4. ) ) *. ( Infinitesimal.float_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ( fun x y -> y ) 35 1. 0. 4. ) -. 1. ;;
( Infinitesimal.vector_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q0 35 v00 0. 4. ).(35) ;;
Infinitesimal.vector_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q0 35 v00 0. 4. ;;
Infinitesimal.vector_end_ode_runge_kutta_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q0 35 v00 0. 4. ;;
Infinitesimal.vector_end_ode_runge_kutta_bounded 0 Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q0 35 v00 0. 4. ;;
( Infinitesimal.vector_ode_runge_kutta_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q0 35 v00 0. 4. ).(35) ;;
( Infinitesimal.vector_ode_runge_kutta_bounded 0 Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q0 35 v00 0. 4. ).(35) ;;

( Infinitesimal.matrix_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q1 35 m00 0. Infinitesimal.halfpi ).(35) ;;
( Infinitesimal.matrix_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q2 35 m00 0. Infinitesimal.quarterpi ).(35) ;;
Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q1 35 m00 0. Infinitesimal.halfpi ;;
Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q2 35 m00 0. Infinitesimal.quarterpi ;;
Infinitesimal.matrix_end_ode_runge_kutta_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q2 35 m00 0. Infinitesimal.quarterpi ;;
Infinitesimal.matrix_end_ode_runge_kutta_bounded 0 Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q2 35 m00 0. Infinitesimal.quarterpi ;;
( Infinitesimal.matrix_ode_runge_kutta_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q1 35 m00 0. Infinitesimal.halfpi ).(35) ;;
( Infinitesimal.matrix_ode_runge_kutta_bounded 0 Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 q1 35 m00 0. Infinitesimal.halfpi ).(35) ;;


Infinitesimal.vector_end_ode_adapt ( Infinitesimal.vector_end_ode_euler ) 1e-5 q0 100 v00 0. 4. ;;
Infinitesimal.vector_end_ode_bounded 1 ( Infinitesimal.vector_end_ode_euler ) 1e-5 q0 100 v00 0. 4. ;;
( Infinitesimal.vector_ode_adapt ( Infinitesimal.vector_ode_euler ) 1e-5 q0 100 v00 0. 4. ).(100) ;;
( Infinitesimal.vector_ode_bounded 1 ( Infinitesimal.vector_ode_euler ) 1e-5 q0 100 v00 0. 4. ).(100) ;;

Infinitesimal.matrix_end_ode_adapt ( Infinitesimal.matrix_end_ode_euler ) 1e-3 q1 100 m00 0. Infinitesimal.halfpi ;;
Infinitesimal.matrix_end_ode_bounded 1 ( Infinitesimal.matrix_end_ode_euler ) 1e-5 q1 100 m00 0. Infinitesimal.halfpi ;;
( Infinitesimal.matrix_ode_adapt ( Infinitesimal.matrix_ode_euler ) 1e-3 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;
( Infinitesimal.matrix_ode_bounded 1 ( Infinitesimal.matrix_ode_euler ) 1e-3 q1 100 m00 0. Infinitesimal.halfpi ).(100) ;;

let r2 = fun x y z -> Matrix.vector_float_minus z y ;;
let methode_zerovect = ( Infinitesimal.vector_zero_general_alea ( Infinitesimal.float_richardson_deriv 3. 4 1e-3 ) 1 ) ;;
( Infinitesimal.vector_ode_implicit ( Infinitesimal.vector_ode_euler ) methode_zerovect r2 v00 100 v00 0. 4. ).(100) ;;



let v = [| 0. ; 10. ; 1. ; 9. ; 2. ; 3. ; 4. ; 6. ; 5. |] ;;
let v0 = Infinitesimal.float_linear_interpol v ;;
let v1 = Infinitesimal.float_regular_stair_interpol Infinitesimal.float_decay_2 v ;;
let v2 = Infinitesimal.float_medium_interpol Infinitesimal.float_decay_2 v ;;

v;;
let v3 = Infinitesimal.float_fit_interpol ( Infinitesimal.mean_float_discrete_richardson_binary_diff ) Infinitesimal.float_decay_16 (1e2) v ;;
Array.map v3 [| 0.1 ; 0.3 ; 0.9 ; 4.8 ; 4.9 ; 5.1 ; 5.2 ; 6.9 ; 7. ; 7.1 |] ;;
let v4 = Infinitesimal.float_fit_interpol ( Infinitesimal.mean_float_discrete_richardson_binary_diff ) Infinitesimal.float_decay_2 (1e3) v ;;
Array.map v4 [| 0.1 ; 0.3 ; 0.9 ; 4.8 ; 4.9 ; 5.1 ; 5.2 ; 6.9 ; 7. ; 7.1 |] ;;
let v5 = Infinitesimal.float_tune_interpol ( Infinitesimal.mean_float_discrete_richardson_binary_diff ) Infinitesimal.float_decay_2 v ;;
Array.map v5 [| 0.1 ; 0.3 ; 0.9 ; 4.8 ; 4.9 ; 5.1 ; 5.2 ; 6.9 ; 7. ; 7.1 |] ;;
let v6 = Infinitesimal.float_tune_interpol ( Infinitesimal.mean_float_discrete_richardson_binary_diff ) Infinitesimal.float_decay_16 v ;;
Array.map v6 [| 0.1 ; 0.3 ; 0.9 ; 4.8 ; 4.9 ; 5.1 ; 5.2 ; 6.9 ; 7. ; 7.1 |] ;;

let v7 = Infinitesimal.vector_trans_interpol ( Infinitesimal.float_tune_interpol ( Infinitesimal.mean_float_discrete_richardson_binary_diff ) Infinitesimal.float_decay_16 ) [| v ; v |] ;;
Array.map v7 [| 0.1 ; 0.3 ; 0.9 ; 4.8 ; 4.9 ; 5.1 ; 5.2 ; 6.9 ; 7. ; 7.1 |] ;;

let v8 = Infinitesimal.vector_interpol ( Infinitesimal.float_tune_interpol ( Infinitesimal.mean_float_discrete_richardson_binary_diff ) Infinitesimal.float_decay_16 ) ( Util.transpose [| v ; v |] ) ;;
Matrix.float_transpose ( Array.map v8 [| 0.1 ; 0.3 ; 0.9 ; 4.8 ; 4.9 ; 5.1 ; 5.2 ; 6.9 ; 7. ; 7.1 |] ) ;;

let v9 = Infinitesimal.matrix_trans_interpol ( Infinitesimal.float_tune_interpol ( Infinitesimal.mean_float_discrete_richardson_binary_diff ) Infinitesimal.float_decay_16 ) [| [| v ; v |] ; [| v ; v |] |] ;;
Array.map v9 [| 0.1 ; 0.3 ; 0.9 ; 4.8 ; 4.9 ; 5.1 ; 5.2 ; 6.9 ; 7. ; 7.1 |] ;;

let v10 = Infinitesimal.matrix_interpol ( Infinitesimal.float_tune_interpol ( Infinitesimal.mean_float_discrete_richardson_binary_diff ) Infinitesimal.float_decay_16 ) ( Util.transpose ( Array.map Matrix.float_transpose [| [| v ; v |] ; [| v ; v |] |] ) ) ;;
Array.map v10 [| 0.1 ; 0.3 ; 0.9 ; 4.8 ; 4.9 ; 5.1 ; 5.2 ; 6.9 ; 7. ; 7.1 |] ;;

Infinitesimal.interpol_2 Infinitesimal.float_linear_interpol ( Matrix.matrix_float_random 3 3 1. ) 2.5 1.4 ;;
Infinitesimal.interpol_2 ( Infinitesimal.vector_interpol Infinitesimal.float_linear_interpol ) ( Array.map ( Matrix.matrix_float_random 3 3 ) ( Matrix.vector_float_random_progression 3 1. ) ) 2.5 1.4 ;;
Infinitesimal.interpol_2 ( Infinitesimal.matrix_interpol Infinitesimal.float_linear_interpol ) ( Array.map ( Array.map ( Matrix.matrix_float_random 3 3 ) ) ( Matrix.matrix_float_random 3 3 1. ) ) 2.5 1.4 ;;



Infinitesimal.normal_cumul_distribution 0. ;;
Infinitesimal.normal_cumul_distribution 100. ;;
Infinitesimal.normal_cumul_distribution_complem 0. ;;
Infinitesimal.normal_cumul_distribution_complem 100. ;;
Infinitesimal.erf 1. ;;
Infinitesimal.erf_complem 1. ;;
Infinitesimal.normal_quantile 0.1 ;;
Infinitesimal.probit 0.1 ;;
Infinitesimal.sin_int 1. ;;
Infinitesimal.si 1. ;;

Infinitesimal.elliptic_integral_first_kind 0.5 1. ;;
Infinitesimal.elliptic_integral_second_kind 0.5 1. ;;
Infinitesimal.elliptic_integral_secondBis_kind 0.5 1. ;;
Infinitesimal.elliptic_integral_third_kind 0.5 2. 1. ;;
Infinitesimal.elliptic_integral ( Infinitesimal.float_int_trapez 100 ) cp1 cp3 cp0 0.1 1. ;;

(*
Les résultats numériques pour l'exponentielle matricielle sont comparables à ceux de scilab.
Pour m02 et m03, les résultats sont légèrement meilleurs que ceux de scilab.
Pour le cas m04 <-> m05, avec m07 et m09, les résultats sont comparables à ceux de scilab, voire deux ou trois fois meilleurs.
Pour m14 <-> m15, et m24 <-> m25 c'est meilleur que scilab, mais l'interpreteur ocaml est considerablement plus lent.
Pour m34 <-> m35, c'est le point où la divergence commence. L'erreur reste généralement en-dessous de 0.1, 
alors que celle de scilab peut dépasser 100. En remplaçant 8. par 12., les résultats sont aussi catastrophiques que ceux de scilab...
Pour m27, m29 et m31, les résultats sont comparables à ceux de scilab, mais c'est franchement lent.
Pour m51, les résultats sont très lents et aussi catastrophiques que ceux de scilab.
*)

let m01 = Matrix.matrix_float_scal_mult Infinitesimal.halfpi anti ;;
Infinitesimal.expm_ode ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ) 35 m01 ;;
Infinitesimal.expm_ode_bis ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ) 35 m01 ;;
Infinitesimal.expm m01 ;;
Infinitesimal.expm_bis m01 ;;
Infinitesimal.direct_expm m01 ;;
Infinitesimal.direct_expm_bis m01 ;;

let m02 = Matrix.matrix_float_scal_mult ( 17. *. Infinitesimal.halfpi ) anti ;;
Infinitesimal.expm_ode ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ) 35 m02 ;;
Infinitesimal.expm_ode_bis ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ) 35 m02 ;;
Infinitesimal.expm m02 ;;
Infinitesimal.slow_expm m02 ;;
Infinitesimal.expm_bis m02 ;;
Infinitesimal.direct_expm m02 ;;
Infinitesimal.direct_expm_bis m02 ;;
Infinitesimal.tune_expm 1e-8 m02 ;;
Infinitesimal.tune_expm_bis 1e-8 m02 ;;

let m03 = Matrix.matrix_float_scal_mult ( 177. *. Infinitesimal.halfpi ) anti ;;
Infinitesimal.expm_ode ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ) 35 m03 ;;
Infinitesimal.expm_ode_bis ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ) 35 m03 ;;
Infinitesimal.expm_ode ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-2 ) 35 m03 ;;
Infinitesimal.expm_ode_bis ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-2 ) 35 m03 ;;
Infinitesimal.expm_bis m03 ;;
Infinitesimal.direct_expm_bis m03 ;;
Infinitesimal.tune_expm_bis 1e-14 m03 ;;
Infinitesimal.expm m03 ;;
Infinitesimal.slow_expm m03 ;;
Infinitesimal.direct_expm m03 ;;
Infinitesimal.tune_expm 1e-14 m03 ;;

let m04 = Matrix.matrix_float_random 10 10 1. ;;
let m05 = Matrix.matrix_float_opp m04 ;;
Matrix.matrix_float_plus m04 m05 ;;
let m06 = Matrix.matrix_float_prod ( Infinitesimal.expm m04 ) ( Infinitesimal.expm m05 ) ;;
let m07 = Matrix.matrix_float_minus m06 ( Matrix.eye_float m06 ) ;;
let m08 = Matrix.matrix_float_prod ( Infinitesimal.slow_expm m04 ) ( Infinitesimal.slow_expm m05 ) ;;
let m09 = Matrix.matrix_float_minus m08 ( Matrix.eye_float m08 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m07 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m09 ) ;;


(*
let m08 = Matrix.matrix_float_prod ( Infinitesimal.slow_expm_ter m04 ) ( Infinitesimal.slow_expm_ter m05 ) ;;
let m09 = Matrix.matrix_float_minus m08 ( Matrix.eye_float m08 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m09 ) ;;

let m10 = Matrix.matrix_float_prod ( Infinitesimal.tune_expm 1e-3 m04 ) ( Infinitesimal.tune_expm 1e-3 m05 ) ;;
let m11 = Matrix.matrix_float_minus m06 ( Matrix.eye_float m10 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m11 ) ;;
*)

let m14 = Matrix.matrix_float_random 30 30 1. ;;
let m15 = Matrix.matrix_float_opp m14 ;;
let m16 = Matrix.matrix_float_prod ( Infinitesimal.expm m14 ) ( Infinitesimal.expm m15 ) ;;
let m17 = Matrix.matrix_float_minus m16 ( Matrix.eye_float m16 ) ;;
(*
let m16o = Matrix.matrix_float_prod ( Infinitesimal.slow_expm m14 ) ( Infinitesimal.slow_expm m15 ) ;;
let m17o = Matrix.matrix_float_minus m16o ( Matrix.eye_float m16o ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m17o ) ;;
*)
Matrix.matrix_float_max ( Matrix.matrix_float_abs m17 ) ;;

let m18 = Matrix.matrix_float_prod ( Infinitesimal.expm_bis m14 ) ( Infinitesimal.expm_bis m15 ) ;;
let m19 = Matrix.matrix_float_minus m18 ( Matrix.eye_float m18 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m19 ) ;;

let m34 = Matrix.matrix_float_bal_random 10 10 8. ;;
let m35 = Matrix.matrix_float_opp m34 ;;
let m36 = Matrix.matrix_float_prod ( Infinitesimal.expm m34 ) ( Infinitesimal.expm m35 ) ;;
let m37 = Matrix.matrix_float_minus m36 ( Matrix.eye_float m36 ) ;;
(*
let m38 = Matrix.matrix_float_prod ( Infinitesimal.slow_expm m34 ) ( Infinitesimal.slow_expm m35 ) ;;
let m39 = Matrix.matrix_float_minus m38 ( Matrix.eye_float m38 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m39 ) ;;
*)
Matrix.matrix_float_max ( Matrix.matrix_float_abs m37 ) ;;

(*
let m24 = Matrix.matrix_float_random 50 50 1. ;;
let m25 = Matrix.matrix_float_opp m24 ;;
let m26 = Matrix.matrix_float_prod ( Infinitesimal.expm m24 ) ( Infinitesimal.expm m25 ) ;;
let m27 = Matrix.matrix_float_minus m26 ( Matrix.eye_float m26 ) ;;
let m28 = Matrix.matrix_float_prod ( Infinitesimal.expm_bis m24 ) ( Infinitesimal.expm_bis m25 ) ;;
let m29 = Matrix.matrix_float_minus m28 ( Matrix.eye_float m28 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m27 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m29 ) ;;
let m30 = Matrix.matrix_float_prod ( Infinitesimal.tune_expm 0.1 m24 ) ( Infinitesimal.tune_expm 0.1 m25 ) ;;
let m31 = Matrix.matrix_float_minus m30 ( Matrix.eye_float m30 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m31 ) ;;

let m44 = Matrix.matrix_float_random 100 100 1. ;;
let m45 = Matrix.matrix_float_opp m44 ;;
let m50 = Matrix.matrix_float_prod ( Infinitesimal.expm m44 ) ( Infinitesimal.expm m45 ) ;;
let m51 = Matrix.matrix_float_minus m50 ( Matrix.eye_float m50 ) ;;
Matrix.matrix_float_max ( Matrix.matrix_float_abs m51 ) ;;

Infinitesimal.expm_ode ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ) 35 m04 ;;
Infinitesimal.expm_ode_bis ( Infinitesimal.matrix_end_ode_runge_kutta_simple_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-20 ) 35 m04 ;;
Infinitesimal.expm m04 ;;
*)

let methode_diff = Infinitesimal.float_richardson_binary_deriv 2 1e-3 ;;
let methode_int = Infinitesimal.float_weighted_int_minus1_1 Infinitesimal.gauss_legendre_257_x Infinitesimal.gauss_legendre_257_w ;;
Infinitesimal.curv_absc methode_diff methode_int Infinitesimal.unit_circle 0. Infinitesimal.pi ;;
let arc0 = Infinitesimal.developpee_2  methode_diff Infinitesimal.unit_circle ;;
let arc1 = Infinitesimal.parallel_arc_2 methode_diff Infinitesimal.unit_circle (-2.) ;;
let arc2 = Infinitesimal.parallel_arc methode_diff Infinitesimal.unit_circle (-2.) ;;
arc0 0. ;;
arc0 Infinitesimal.pi ;;
arc1 0. ;;
arc1 Infinitesimal.pi ;;
arc2 0. ;;
arc2 Infinitesimal.pi ;;

Infinitesimal.tlm methode_diff Infinitesimal.cyl_coord_unit_sphere [| 0.5 ; 0. |] ;;
let w0 = Infinitesimal.surface_weingarten_3 methode_diff Infinitesimal.cyl_coord_unit_sphere ;; 
(*
Array.map w0 [| [| 0.5 ; 0. |] ; [| 0.5 ; 1. |] ; [| 0.5 ; Infinitesimal.halfpi |] ; [| 0.5 ; Infinitesimal.pi |] |] ;;
Array.map w0 ( Matrix.matrix_float_random 3 2 1. ) ;;
let w1 = Infinitesimal.surface_principal_curvatures_3 methode_diff Infinitesimal.cyl_coord_unit_sphere ;;
Array.map w1 ( Matrix.matrix_float_random 3 2 0.3 ) ;;
*)
let w2 = Infinitesimal.surface_mean_curvature_3 methode_diff Infinitesimal.cyl_coord_unit_sphere ;;
Array.map w2 ( Matrix.matrix_float_random 3 2 1. ) ;;
let w3 = Infinitesimal.surface_gauss_curvature_3 methode_diff Infinitesimal.cyl_coord_unit_sphere ;;
Array.map w3 ( Matrix.matrix_float_random 3 2 1. ) ;;

(*
let w4 = Infinitesimal.surface_principal_curvatures_3 methode_diff Infinitesimal.sph_coord_unit_sphere ;;
Array.map w4 ( Matrix.matrix_float_random 3 2 0.5 ) ;;
let w5 = Infinitesimal.surface_mean_curvature_3 methode_diff Infinitesimal.sph_coord_unit_sphere ;;
Array.map w5 ( Matrix.matrix_float_random 3 2 0.5 ) ;;
let w6 = Infinitesimal.surface_gauss_curvature_3 methode_diff Infinitesimal.sph_coord_unit_sphere ;;
Array.map w6 ( Matrix.matrix_float_random 3 2 0.5 ) ;;
*)

let w7 = Infinitesimal.surface_principal_curvatures_3 methode_diff Infinitesimal.pseudo_sphere ;;
Array.map w7 ( Matrix.matrix_float_random 3 2 1. ) ;;
let w8 = Infinitesimal.surface_mean_curvature_3 methode_diff Infinitesimal.pseudo_sphere ;;
Array.map w8 ( Matrix.matrix_float_random 3 2 1. ) ;;
let w9 = Infinitesimal.surface_gauss_curvature_3 methode_diff Infinitesimal.pseudo_sphere ;;
Array.map w9 ( Matrix.matrix_float_random 3 2 1. ) ;;

let w10 = Infinitesimal.graph_principal_curvatures_3 methode_diff h ;;
Array.map w10 ( Matrix.matrix_float_bal_random 3 2 1. ) ;;
let w11 = Infinitesimal.graph_mean_curvature_3 methode_diff h ;;
Array.map w11 ( Matrix.matrix_float_bal_random 3 2 1. ) ;;
let w12 = Infinitesimal.graph_gauss_curvature_3 methode_diff h ;;
Array.map w12 ( Matrix.matrix_float_bal_random 3 2 1. ) ;;

let methode_ode = Infinitesimal.vector_end_ode_runge_kutta_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-3 ;;
let methode_ode = Infinitesimal.vector_ode_runge_kutta_adapt Infinitesimal.rk7_dormand_prince_a Infinitesimal.rk7_dormand_prince_b_fine Infinitesimal.rk7_dormand_prince_b_raw 1e-3 ;;
let c00 = Infinitesimal.prescribed_curvature_2 methode_ode Util.float_one 35 0. ;;
c00 0. ;;
c00 Infinitesimal.halfpi ;;
( c00 Infinitesimal.halfpi ).(35) ;;
c00 Infinitesimal.pi ;;
c00 Infinitesimal.doublepi ;;

let c01 = Infinitesimal.prescribed_curvature_torsion_3 methode_ode Util.float_one Util.float_one 35 0. ;;
c01 2. ;;

let c02 = Infinitesimal.prescribed_multicurvature methode_ode ( Util.vector_one 2 ) 35 0. ;;
c02 2. ;;

let c03 = Infinitesimal.clothoid ( Infinitesimal.float_int_trapez 10000 ) ;;
c03 99. ;;

Infinitesimal.float_dirac_mass Infinitesimal.half_unit_circle ;;
let d00 = Infinitesimal.float_distrib_deriv ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) Infinitesimal.float_dirac_mass ;;
d00 cos ;;
d00 sin ;;

let d01 = Infinitesimal.float_distrib_deriv ( Infinitesimal.float_richardson_binary_deriv 4 1e-3 ) d00 ;;
d01 cos ;;
d01 sin ;;

let d02 = Array.make 4 ( Infinitesimal.float_distrib_deriv ( Infinitesimal.float_richardson_binary_deriv 1 1e-3 ) ) ;;
let d03 = fun a x -> a x ;;
let d04 = Array.fold_right d03 d02 Infinitesimal.float_dirac_mass ;;
d04 cos ;;
d04 sin ;;
d04 exp ;;

let d05 = Array.make 4 ( Infinitesimal.float_distrib_deriv ( Infinitesimal.compensated_float_richardson_binary_deriv 4 3e-3 ) ) ;;
let d06 = fun a x -> a x ;;
let d07 = Array.fold_right d06 d05 Infinitesimal.float_dirac_mass ;;
d07 cos ;;
d07 sin ;;
d07 exp ;;

let d08 = Array.make 4 ( Infinitesimal.float_distrib_deriv ( Infinitesimal.compensated_float_richardson_deriv ( Matrix.float_brezinski 2 1 ) 1.5 4 4e-2 ) ) ;;
let d09 = fun a x -> a x ;;
let d10 = Array.fold_right d09 d08 Infinitesimal.float_dirac_mass ;;
d10 cos ;;
d10 sin ;;
d10 exp ;;

