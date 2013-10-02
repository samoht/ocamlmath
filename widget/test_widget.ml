 (*

*)

let a = ref [| 0 |] ;;
let aa =  [| "W essai M" |] ;;
let aaa = [| "Changé !!" |] ;;

let b = ref [| 0 |] ;;
let bb =  [| "W essai M" ; "Ceci est un essai !!" |] ;;
let bbb = [| "Ce bouton" ; "a bien été modifié !" |] ;;

let c = ref [| 0 |] ;;
let cc =  [| "W essai M" ; "Ceci est  " ; "un essai!" |] ;;
let ccc = [| "Ce bouton" ; "a bien été" ; "modifié. " |] ;;

let d = ref [| 0 |] ;;
let dd =  [| "Encore un" ; "essai...." ; "W essai M" |] ;;
let ddd = [| "Ce bouton" ; "est bien " ; "modifié. " |] ;;

let e = ref [| 0 |] ;;
let ee =[| "truc" ; "bidule" ; "machin" |] ;;
let eee = [| "machin" ; "chouette" ; "truc" |] ;;

let f = ref [| 0 |] ;;
let ff = Array.map string_of_int ( Matrix.int_equal_range 0 2 7 ) ;;

let g = ref [| 0. |] ;;

let h = ref [| 0 |] ;;

let i = ref [| "" |] ;;
let initial = ref "bla blabla blablabla essai" ;;
let final = ref "bla blabla blablabla" ;;

let current_character = ref ( " " ) ;;

let f_init = function () ->
 Graphics.open_graph " 600x400" ;
 a := Widget.rect_switch 20 10 Graphics.yellow Graphics.cyan aa ;
(*
 b := Widget.rect_switch 100 10 Graphics.yellow Graphics.cyan bb ;
 b := Widget.lr_switch 300 10 Graphics.yellow Graphics.cyan bb ;
*)
 b := Widget.ur_switch 500 210 Graphics.yellow Graphics.cyan cc ;
(*
 c := Widget.hat_switch 30 100 Graphics.yellow Graphics.cyan cc ;
 c := Widget.ll_switch 30 100 Graphics.yellow Graphics.cyan cc ;
*)
 c := Widget.ul_switch 30 200 Graphics.yellow Graphics.cyan cc ;
 d := Widget.cup_switch 300 300 Graphics.yellow Graphics.cyan dd ;
(*
 e := Widget.right_switch 30 300 Graphics.yellow Graphics.cyan ee ;
 e := Widget.left_switch 130 300 Graphics.yellow Graphics.cyan ee ;
*)
 e := Widget.left_switch 130 300 Graphics.yellow Graphics.cyan ee ;
 f := Widget.cursor_round_selector 200 200 10 60 Graphics.red Graphics.yellow Graphics.cyan Graphics.black Graphics.white ff 3 ;;

let g_init = function () ->
 Graphics.open_graph " 600x400" ;
 g := Widget.float_vernier 100 100 10 Graphics.red Graphics.yellow Graphics.cyan Graphics.white Graphics.black 10 [| 0. ; 100. |] 20. ;
 h := Widget.int_vernier 200 200 10 Graphics.red Graphics.yellow Graphics.cyan Graphics.white Graphics.black 10 [| 0 ; 100 |] 20 ;
 i := Widget.string_edit 400 300 100 Graphics.black Graphics.white Graphics.green !initial !final ;;

let f_end = function () -> Graphics.close_graph () ;;

let f_key = function c -> ( print_char c ; print_newline () ) ;;

let g_key = function c -> ( current_character := Char.escaped c ) ;;

let f_mouse = fun x y ->
 if Widget.is_over_rect_button !a x y then
  a := Widget.rect_switch !a.(0) !a.(1) Graphics.green Graphics.magenta aaa
 else
(*
  if Widget.is_over_rect_button !b x y then 
   b := Widget.rect_switch !b.(0) !b.(1) Graphics.red Graphics.blue bbb
  if Widget.is_over_lr_button !b x y then 
   b := Widget.lr_switch !b.(0) !b.(1) Graphics.red Graphics.blue bbb
*)
  if Widget.is_over_ur_button !b x y then 
   b := Widget.ur_switch !b.(0) !b.(1) Graphics.red Graphics.blue ccc
  else
(*
   if Widget.is_over_hat_button !c x y then
    c := Widget.hat_switch !c.(0) !c.(1) Graphics.blue Graphics.red ccc
   if Widget.is_over_ll_button !c x y then
    c := Widget.ll_switch !c.(0) !c.(1) Graphics.blue Graphics.red ccc
*)
   if Widget.is_over_ul_button !c x y then
    c := Widget.ul_switch !c.(0) !c.(1) Graphics.blue Graphics.red ccc
   else
    if Widget.is_over_cup_button !d x y then
     d := Widget.cup_switch !d.(0) !d.(1) Graphics.magenta Graphics.black ddd
    else
(*
     if Widget.is_over_right_button !e x y then
      e := Widget.right_switch 30 300 Graphics.magenta Graphics.yellow eee
*)
     if Widget.is_over_left_button !e x y then
      e := Widget.left_switch 130 300 Graphics.magenta Graphics.yellow eee
     else
      let s = Widget.over_selector !f x y in
       if s != !f.(5) then
        f := Widget.cursor_round_selector !f.(0) !f.(1) !f.(2) !f.(3) Graphics.red Graphics.yellow Graphics.cyan Graphics.black Graphics.white ff s ;;
(*
        f := Widget.cursor_round_selector !f.(0) !f.(1) !f.(2) !f.(3) Graphics.red Graphics.yellow Graphics.cyan Graphics.black Graphics.white ff s ;;
        f := Widget.cursor_polygon_selector !f.(0) !f.(1) !f.(2) !f.(3) Graphics.red Graphics.yellow Graphics.cyan Graphics.black Graphics.white ff s ;;
        f := Widget.polygon_selector !f.(0) !f.(1) !f.(2) !f.(3) Graphics.red Graphics.yellow Graphics.cyan Graphics.black ff s ;;
        f := Widget.round_selector !f.(0) !f.(1) !f.(2) !f.(3) Graphics.red Graphics.yellow Graphics.cyan Graphics.black ff s ;;
*)


(*
let g_mouse = fun x y ->
 let s = Widget.over_float_vernier !g x y
 and t = Widget.over_int_vernier !h x y  in
  if abs_float ( s -. !g.(7) ) > epsilon_float then
   g := Widget.float_vernier ( int_of_float !g.(0) ) ( int_of_float !g.(1) ) ( int_of_float !g.(2) ) Graphics.red Graphics.yellow Graphics.cyan Graphics.white Graphics.black 10 [| 0. ; 100. |] s
  else
   if abs ( t - !h.(7) ) > 0 then
    h := Widget.int_vernier ( !h.(0) ) ( !h.(1) ) ( !h.(2) ) Graphics.red Graphics.yellow Graphics.cyan Graphics.white Graphics.black 10 [| 0 ; 100 |] t
   else
    if ( Widget.is_over_string_edit !i x y ) then
     let ev = ref ( Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ] ) in
      while ( !ev.Graphics.keypressed ) && ( not ( Graphics.button_down () ) ) do
       g_key ( !ev.Graphics.key ) ;
       let res = Widget.string_treatment !initial !final !current_character in
        initial := res.(0) ;
        final := res.(1) ;
        i := Widget.string_edit 400 300 100 Graphics.black Graphics.white Graphics.green !initial !final ;
        ev := Graphics.wait_next_event [ Graphics.Key_pressed ; Graphics.Button_down ]
       done ;;


let f_except = function unknown -> f_end () ;;

Widget.skeleton f_init f_end f_key f_mouse f_except ;;

Widget.skeleton g_init f_end g_key g_mouse f_except ;;
*)



Readwrite.sub_directories "/" ;;
Readwrite.sub_directories_with_parent "/" ;;

Widget.choose_directory 1300 700 10 "/" ;;
Widget.choose_directory 800 500 10 "/" ;;

Widget.choose_directory 1300 600 10 "../analogic" ;;
Widget.choose_regular_file 1300 700 10 "../analogic" ;;
Widget.choose_regular_file 800 500 10 "." ;;

Widget.choose_real 800 500 [| -10. ; 10. |] [| 5. |] ;;




