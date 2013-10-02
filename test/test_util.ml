
let a0 = [| 1. ; exp 1. ; -4. *. ( atan 1. ) |] ;;
let s0 = Util.bare_vector_to_string string_of_float a0 ;;
let a1 = Util.bare_vector_of_string float_of_string s0 ;;
let m0 = [| a0 ; Array.make 3 0. |] ;;
(*
let s1 = Matrix.string_of_vector_float a0 ;;
let a2 = Matrix.vector_float_of_string s1 ;;
let s2 = Matrix.matrix_float_to_string m0 ;;
let m1 = Matrix.matrix_float_of_string s2 ;;
*)
let s3 = Util.vector_to_string ( Util.bare_vector_to_string string_of_float ) "[|\n" " ;\n" "\n|]" m0 ;;
let m2 = Util.vector_of_string ( Util.bare_vector_of_string float_of_string ) "[|\n" " ;\n" "\n|]" s3 ;;
