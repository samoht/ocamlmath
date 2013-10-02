
#load "str.cma" ;;
#load "nums.cma" ;;
#use "util.ml" ;;
#use "../data/data.ml" ;;
#use "bary.ml" ;;


module B = Bary.Make ( Data.Zindex ) ( Data.Zcoeff ) ;;
module C = Bary.Make ( Data.Zindex ) ( Data.Rcoeff ) ;;

let s = B.singleton (1,1) ;;
B.add (2,1) s ;;
B.add (1,3) s ;;
B.sub (2,6) s ;;

let s0 = B.singleton (1,9) ;;
B.add (4,4) s0 ;;

let s1 = B.union s s0 ;;
let s2 = B.diff s s0 ;;

B.elements s ;;
B.elements s0 ;;
B.elements s1 ;;
B.elements s2 ;;

let t = C.singleton (1,1.5) ;;
C.add (2,3.6) t ;;
C.add (1,5.8) t ;;
C.sub (2,8.7) t ;;

let e = C.to_list t ;;
let t0 = C.of_list e ;;
let e0 = C.elements t0 ;;
let t1 = C.opp t ;;
let e1 = C.elements t1 ;;
let c = C.compare t0 t ;;
C.equal t0 t ;;
C.equal t1 t ;;

let t2 = C.union t t1 ;;
let t3 = C.diff t t1 ;;
let t4 = C.inter t1 t0 ;;
let e2 = C.elements t2 ;;
let e3 = C.elements t3 ;;
let e4 = C.elements t4 ;;




