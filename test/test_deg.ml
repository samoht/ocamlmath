
let a = Deg.Int 1 ;;
let b = Deg.Float 1. ;;
let c = Deg.Big ( Big_int.big_int_of_int 1000 ) ;;
let i = Deg.Float infinity ;;
let m = Deg.Float neg_infinity ;;

let e = Deg.Float ( 2. ** 60. ) ;;

let s = function x -> Big_int.int_of_big_int ( Deg.big_demakeup x ) ;;

print_newline () ;;

Deg.maximum a b ;;
s ( Deg.maximum a c ) ;;
Deg.maximum a i ;;
Deg.maximum a m ;;
s ( Deg.maximum b c ) ;;
Deg.maximum b i ;;
Deg.maximum b m ;;
Deg.maximum c i ;;
s ( Deg.maximum c m ) ;;
Deg.maximum i m ;;

print_newline () ;;

Deg.minimum a b ;;
s ( Deg.minimum a c ) ;;
Deg.minimum a i ;;
Deg.minimum a m ;;
s ( Deg.minimum b c ) ;;
Deg.minimum b i ;;
Deg.minimum b m ;;
s ( Deg.minimum c i ) ;;
Deg.minimum c m ;;
Deg.minimum i m ;;

print_newline () ;;

s ( Deg.add a b ) ;;
s ( Deg.add a c ) ;;
Deg.add a i ;;
Deg.add a m ;;
s ( Deg.add b c ) ;;
Deg.add b i ;;
Deg.add b m ;;
Deg.add c i ;;
Deg.add c m ;;
(*
Deg.add i m ;;
*)

print_newline () ;;

s ( Deg.sub a b ) ;;
s ( Deg.sub a c ) ;;
Deg.sub a i ;;
Deg.sub a m ;;
s ( Deg.sub b c ) ;;
Deg.sub b i ;;
Deg.sub b m ;;
Deg.sub c i ;;
Deg.sub c m ;;
Deg.sub i m ;;

print_newline () ;;

s ( Deg.mult a b ) ;;
s ( Deg.mult a c ) ;;
Deg.mult a i ;;
Deg.mult a m ;;
s ( Deg.mult b c ) ;;
Deg.mult b i ;;
Deg.mult b m ;;
Deg.mult c i ;;
Deg.mult c m ;;
Deg.mult i m ;;

print_newline () ;;

Deg.compare a b ;;
Deg.compare a c ;;
Deg.compare a i ;;
Deg.compare a m ;;
Deg.compare b c ;;
Deg.compare b i ;;
Deg.compare b m ;;
Deg.compare c i ;;
Deg.compare c m ;;
Deg.compare i m ;;


print_string "Raise exception on purpose ---> " ;;
Deg.maximum a e ;;
