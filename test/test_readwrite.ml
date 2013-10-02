
Readwrite.write_float_value 1. "Real" ;;
Readwrite.write_float_value 0.5 "Real0" ;;
Readwrite.write_float_value 1024. "Real1" ;;
Readwrite.write_float_value (-1.) "Real2" ;;
Readwrite.write_float_value max_float "Real3" ;;
Readwrite.write_float_value epsilon_float "Real4" ;;
Readwrite.write_float_value nan "Real5" ;;
Readwrite.write_float_value (-. max_float ) "Real6" ;;
Readwrite.write_float_value min_float "Real7" ;;
Readwrite.write_float_value (-. min_float ) "Real8" ;;
Readwrite.write_float_value (-. max_float ) "Real9" ;;

Readwrite.write_float_array_value [| 1. ; 0.5 ; max_float |] "Vector" ;;

(*
let v = Readwrite.array_read_text_file "essai.xpm" ;;
*)

Readwrite.list_write_text_file [ "ceci " ; "est " ; "un essai." ] "essai0.txt" ;;
Readwrite.array_write_text_file [| "ceci " ; "est " ; "un essai." |] "essai1.txt" ;;

let v0 = [| 1 ; 2 ; 3 |] ;;
let v1 = [| 1.1 ; 2.2 ; 3.3 |] ;;
Readwrite.vector_int_write v0 "essai2.int" ;;
Readwrite.vector_float_write v1 "essai3.float" ;;

let l2 = Readwrite.list_reverse_int_read "essai2.int" ;;
let l3 = Readwrite.list_reverse_float_read "essai3.float" ;;
let v2 = Readwrite.vector_int_read "essai2.int" ;;
let v3 = Readwrite.vector_float_read "essai3.float" ;;

let m0 = [| v0 ; [| 6 ; 5 ; 4 |] |] ;;
let m1 = [| v1 ; Array.map ( ( *. ) 9.9 ) v1 |] ;;
Readwrite.matrix_int_write m0 "essai4.int" ;;
Readwrite.matrix_float_write m1 "essai5.float" ;;
let m2 = Readwrite.matrix_int_read "essai4.int" ;;
let m3 = Readwrite.matrix_float_read "essai5.float" ;;
Matrix.matrix_float_minus m1 m3 ;;
Matrix.matrix_int_minus m0 m2 ;;

(*
let vv = Readwrite.xpm_color_array v ;;

let a = Readwrite.read_pnm_header "image.pgm" ;;
let b = Readwrite.read_pgm_binary_int_unicolor "image.pgm" ;;
let bb = Readwrite.read_pgm_binary_color "image.pgm" ;;
let bbb = Readwrite.read_pgm_binary_float_unicolor "image.pgm" ;;
let c = Readwrite.read_ppm_binary_int_rgb "image.ppm" ;;
let cc = Readwrite.read_ppm_binary_color "image.ppm" ;;
let ccc = Readwrite.read_ppm_binary_float_rgb "image.ppm" ;;
let cccc = Readwrite.read_ppm_binary_color "../analogic/DSC0058.ppm" ;;

Graphics.open_graph " 300x200" ;;
Graphics.draw_image ( Graphics.make_image cc ) 0 0 ;;
Graphics.close_graph () ;;

Graphics.open_graph " 1300x800" ;;
let c_c = Graphics.make_image cccc ;;
Graphics.draw_image c_c 0 0 ;;
Graphics.close_graph () ;;

Readwrite.write_pgm_binary_int_unicolor b "essai.pgm" ;; 
Readwrite.write_pgm_binary_float_unicolor bbb "essai0.pgm" ;; 
Readwrite.write_ppm_binary_int_rgb c "essai.ppm" ;; 
Readwrite.write_ppm_binary_color cc "essai0.ppm" ;; 
Readwrite.write_ppm_binary_float_rgb ccc "essai1.ppm" ;; 
*)


Readwrite.write_vector_float_to_float32_au 44100 v1 "essai.au" ;;
Readwrite.read_header_au "essai.au" ;;
let v4 = Readwrite.read_float32_au "essai.au" ;;
let v5 = [| v1 ; Matrix.vector_float_scal_mult 0.5 v1 |] ;;
Readwrite.write_matrix_float_to_float32_au 44100 v5 "essai0.au" ;;
let v6 = Readwrite.read_float32_au "essai0.au" ;;

Matrix.matrix_float_minus v6 v5 ;;


Readwrite.write_vector_float_to_float64_au 44100 v1 "essai1.au" ;;
Readwrite.read_header_au "essai1.au" ;;
let v9 = Readwrite.read_vector_float64_au "essai1.au" ;;
let v10 = [| 1. ; 0.5 ; max_float ; min_float ; epsilon_float ; infinity ; nan ; neg_infinity |] ;;
Readwrite.write_vector_float_to_float64_au 44100 v10 "essai2.au" ;;
let v11 = Readwrite.read_vector_float64_au "essai2.au" ;;

let m4 = [| v10 ; Matrix.vector_float_opp v10 |] ;;
Readwrite.write_matrix_float_to_float64_au 44100 m4 "essai3.au" ;;
let m5 = Readwrite.read_matrix_float64_au "essai3.au" ;;

(*
let i = Readwrite.read_pbm_binary_color "fumicaml.pbm" ;;
let i = Readwrite.read_pgm_binary_color "fumicaml.pgm" ;;
let i = Readwrite.read_ppm_binary_color "fumicaml.ppm" ;;

let p = Readwrite.read_picture_float_rgb "fumicaml.pbm" ;;

Readwrite.read_bmp_header "bmps/ApprocheBook.bmp" ;;
Readwrite.read_bmp_header "bmps/seize.bmp" ;;
let i = Readwrite.read_palette_bmp_color "bmps/seize.bmp" ;;
let i = Readwrite.read_bmp_color "bmps/ApprocheBook.bmp" ;;
let i = Readwrite.read_bmp_color "bmps/OReillyBook.bmp" ;;
*)

(*
#load "graphics.cma" ;;
Graphics.open_graph " 1000x500" ;;
let ii = Graphics.make_image i ;;
Graphics.draw_image ii 0 0 ;;
*)



