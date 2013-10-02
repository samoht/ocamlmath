



module Readwrite = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module:

- utility functions in order to permit the following exchanges between Ocaml and the file system:

+ read and write integer or real vectors and matrices,
+ read and write text files, 
+ read XPM picture files (which are text files indeed),
+ read PNM picture files of type P4, which means bitmap pictures in binary data,
+ read and write PNM picture files of types P5 and P6, which means gray levels pictures (PGM) or trichromic pictures (PPM) in binary data,
+ read and write BMP picture files,
+ read and write sound files in the AU float 32 bits format, 
+ read and write real vectors and matrices in the AU float 64 bits format,
+ read and write sound files in some WAV format,
+ read the directories,

- methods to treat the characters obtained from these files,

- conversions between different ways to record pictures inside Ocaml,

- utility functions in order to retrieve data about the environment of the operating system.


{2 Conventions}


The color used in the Graphics module of Ocaml is coded by an integer according to the formula
color = red * 256 * 256 + green * 256 + blue
where the integers red, green, blue are between 0 and 255.


{2 Comments}


The reading and writing of float 64 bits AU sound files (real vectors and matrices) makes use of
the commands [head] and [tail] available on every minimal UNIX system.

The reading and writing of float 32 bits AU sound files (real vectors) makes use of
conversions with the class [int] which are only correct on 64 bits machines.

The functions about environment use instructions that vary considerably 
from one operating system to another. They have been only tested under FreeBSD.

The functions for reading the content of a file into a list and writing the content of a file from an Ocaml structure come from P. Manoury: 
Programmation de droite à gauche et vice-versa, Paracamplus, Paris, 2011:

[www.paracamplus.com]


This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module :

- des fonctions utilitaires permettant les échanges suivants entre Ocaml et le système de fichiers : 

+ lecture et écriture des vecteurs et matrices entiers ou réels,
+ lecture et écriture des fichiers textes, 
+ lecture des fichiers d'images XPM (qui sont en fait des fichiers textes), 
+ lecture des fichiers d'images PNM de type P4, c'est-à-dire des images noir-blanc en données binaires,
+ lecture et écriture des fichiers d'images PNM de types P5 et P6, c'est-à-dire des images soit en niveaux de gris (PGM) soit trichromiques (PPM) en données binaires,
+ lecture et écriture des fichiers d'images BMP,
+ lecture et écriture des fichiers sons au format AU float 32 bits, 
+ lecture et écriture des vecteurs et matrices réels au format AU float 64 bits,
+ lecture et écriture de fichiers sons à certains formats WAV,
+ lecture des répertoires,

- des méthodes pour traiter les caractères obtenus de ces fichiers,

- des conversions entre différentes manières de consigner les images à l'intérieur d'Ocaml,

- des fonctions utilitaires permettant de récupérer des données sur l'environnement du système d'exploitation.


{2 Conventions}


La couleur utilisée dans le module Graphics d'Ocaml est codée par entier selon la formule
couleur = rouge * 256 * 256 + vert * 256 + bleu 
où les entiers rouge, vert, bleu sont compris entre 0 et 255.


{2 Commentaires}


La lecture et l'écriture de fichiers sons AU float 64 bits (vecteurs et matrices réels)
utilise les commandes [head] et [tail] fournies dans tous les systèmes UNIX minimaux.

La lecture et l'écriture des fichiers sons AU float 32 bits utilisent des conversions avec 
la classe [int] qui ne sont correctes que sur les machines 64 bits.

Les informations sur l'environnement utilisent des instructions qui varient fortement
d'un système d'exploitation à l'autre. Elles ne sont testées que sous FreeBSD.

Les fonctions de lecture d'un fichier vers une liste ou d'écriture d'un fichier depuis une structure d'Ocaml proviennent de P. Manoury : 
Programmation de droite à gauche et vice-versa, Paracamplus, Paris, 2011 :

[www.paracamplus.com]


Ce module est distribué selon la même licence qu'Ocaml.


{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.3}
*)
(**
@version 0.3
*)
(**
@author Stéphane Grognet
*)
(**
@since 2013
*)




open Util ;;
open Matrix ;;




(** {C § } *)
(** 
{1 Fichiers textes, fichiers binaires et traitement des chaînes de caractères}
{1 Text files, binary files and character strings treatment}
*)
(** {C  } *)




(** {v string_match regexp string start v} *)
let string_match = fun r s i ->
 try
  begin
   ignore ( Str.search_forward r s i ) ;
   true
  end
 with Not_found -> false ;;

(** {v list_read_reverse_text_file file_name v} *)
let list_read_reverse_text_file = function (fname:string) ->
 let ic = open_in fname in
   let l = ref [] in
    begin
     try
      while true do
       l := ( input_line ic ) :: !l ;
      done
     with End_of_file -> close_in_noerr ic
    end ;
    !l ;;

(** {v list_read_reverse_text_header file_name v} *)
let list_read_reverse_text_header = fun (h:int) (fname:string) ->
 let ic = open_in fname in
   let l = ref [] in
    begin
     try
      for i = 1 to h do
       l := ( input_line ic ) :: !l ;
      done ;
      close_in_noerr ic
     with End_of_file -> close_in_noerr ic
    end ;
    !l ;;


(** {v list_read_text_file file_name v} *)
let list_read_text_file = function (fname:string) ->
 List.rev ( list_read_reverse_text_file fname ) ;;

(** {v array_read_reverse_text_file file_name v} *)
let array_read_reverse_text_file = function (fname:string) ->
 Array.of_list ( list_read_reverse_text_file fname ) ;;

(** {v array_read_text_file file_name v} *)
let array_read_text_file = function (fname:string) ->
 Array.of_list ( list_read_text_file fname ) ;;


(** {v list_write_text_file string_list file_name v} *)
let list_write_text_file = fun (l:string list) (fname:string) ->
 let oc = open_out fname
 and ll = ref l in
  while List.length !ll > 0 do
   output_string oc ( ( List.hd !ll ) ^ "\n" ) ;
   ll := List.tl !ll
  done ;
  close_out_noerr oc ;;

(** {v array_write_text_file string_array file_name v} *)
let array_write_text_file = fun (a:string array) (fname:string) ->
 let oc = open_out fname
 and l = Array.length a in
  for i = 0 to pred l do
   output_string oc ( a.(i) ^ "\n" ) ;
  done ;
  close_out_noerr oc ;;


(** {v list_read_text_header size file_name v} *)
let list_read_text_header = fun (h:int) (fname:string) ->
 List.rev ( list_read_reverse_text_header h fname ) ;;

(** {v array_read_reverse_text_header size file_name v} *)
let array_read_reverse_text_header = fun (h:int) (fname:string) ->
 Array.of_list ( list_read_reverse_text_header h fname ) ;;

(** {v array_read_text_header size file_name v} *)
let array_read_text_header = fun (h:int) (fname:string) ->
 Array.of_list ( list_read_text_header h fname ) ;;


(** {v list_read_reverse_binary_file file_name v} *)
let list_read_reverse_binary_file = function (fname:string) ->
 let ic = open_in_bin fname in
   let l = ref [] in
    begin
     try
      while true do
       l := ( input_byte ic ) :: !l
      done
     with End_of_file -> close_in_noerr ic
    end ;
    !l ;;

(** {v list_read_binary_file file_name v} *)
let list_read_binary_file = function (fname:string) ->
 List.rev ( list_read_reverse_binary_file fname ) ;;


(** {v step_read_binary_file buffer block_size file_name v} *)
let rec step_read_binary_file = fun (b:Buffer.t) (block_size:int) (ic:in_channel) ->
 try
  begin
   Buffer.add_channel b ic block_size ;
   block_size
  end
 with _ ->
  block_size / 2 ;;

(** {v buffer_to_binary_array buffer v} *)
let buffer_to_binary_array = function (b:Buffer.t) ->
 let r = Buffer.length b in
  let a = Array.make r 0 in
   for i = 0 to pred r do
    a.(i) <- int_of_char ( Buffer.nth b i )
   done ;
   a ;;

(** {v array_read_binary_file init_size file_name v} *)
let rec array_read_binary_file = fun (init_size:int) (fname:string) ->
 let b = Buffer.create init_size
 and block_size = ref init_size
 and ic = open_in_bin fname in
  let f = function () ->
   begin
    block_size := step_read_binary_file b !block_size ic ;
    !block_size >= 1
   end in
   while ( f () ) do
    ()
   done ;
   buffer_to_binary_array b ;;



(** {v array_write_binary_file vector file_name v} *)
let array_write_binary_file = fun a (fname:string) ->
 let l = Array.length a
 and oc = open_out fname in
  for i = 0 to pred l do
   output_byte oc a.(i)
  done ;
  close_out_noerr oc ;;


(** {v hexa_char_to_int figure v} *)
let hexa_char_to_int = function (c:char) ->
 match c with
 | '0' -> 0
 | '1' -> 1
 | '2' -> 2
 | '3' -> 3
 | '4' -> 4
 | '5' -> 5
 | '6' -> 6
 | '7' -> 7
 | '8' -> 8
 | '9' -> 9
 | 'A' -> 10
 | 'B' -> 11
 | 'C' -> 12
 | 'D' -> 13
 | 'E' -> 14
 | 'F' -> 15
 | 'a' -> 10
 | 'b' -> 11
 | 'c' -> 12
 | 'd' -> 13
 | 'e' -> 14
 | 'f' -> 15
 | _ -> failwith "Hexadecimal character not recognized in Readwrite.hexa_char_to_int." ;;
 
(** {v int_to_hexa_char figure v} *)
let int_to_hexa_char = function (c:int) ->
 match c with
 | 0 -> '0'
 | 1 -> '1'
 | 2 -> '2'
 | 3 -> '3'
 | 4 -> '4'
 | 5 -> '5'
 | 6 -> '6'
 | 7 -> '7'
 | 8 -> '8'
 | 9 -> '9'
 | 10 -> 'A'
 | 11 -> 'B'
 | 12 -> 'C'
 | 13 -> 'D'
 | 14 -> 'E'
 | 15 -> 'F'
 | _ -> failwith "Hexadecimal figure not allowed in Readwrite.int_to_hexa_char." ;;
 
(** {v byte_string_to_int string v} *)
let byte_string_to_int = function (s:string) ->
 let a = hexa_char_to_int s.[0]
 and b = hexa_char_to_int s.[1] in
  16 * a + b ;;

(** {v triple_byte_string_to_int_luminance string v} *)
let triple_byte_string_to_int_luminance = function (s:string) ->
(** The string begins with a #. *)
 let indices = [| 1 ; 2 ; 3 ; 4 ; 5 ; 6 |]
 and f = function i -> hexa_char_to_int s.[i] in
  let v = Array.map f indices in
   16 * ( v.(0) + v.(2) + v.(4) ) + v.(1) + v.(3) + v.(5) ;;

(** {v triple_byte_string_to_int_red string v} *)
let triple_byte_string_to_int_red = function (s:string) ->
(** The string begins with a #. *)
 let a = hexa_char_to_int s.[1]
 and b = hexa_char_to_int s.[2] in
  16 * a + b ;;

(** {v triple_byte_string_to_int_green string v} *)
let triple_byte_string_to_int_green = function (s:string) ->
(** The string begins with a #. *)
 let a = hexa_char_to_int s.[3]
 and b = hexa_char_to_int s.[4] in
  16 * a + b ;;

(** {v triple_byte_string_to_int_blue string v} *)
let triple_byte_string_to_int_blue = function (s:string) ->
(** The string begins with a #. *)
 let a = hexa_char_to_int s.[5]
 and b = hexa_char_to_int s.[6] in
  16 * a + b ;;

(** {v triple_byte_string_to_int_rgb string v} *)
let triple_byte_string_to_int_rgb = function (s:string) ->
(** The string begins with a #. *)
 let indices = [| 1 ; 2 ; 3 ; 4 ; 5 ; 6 |]
 and f = function i -> hexa_char_to_int s.[i] in
  let v = Array.map f indices in
   [| 16 * v.(0) + v.(1) ; 16 * v.(2) + v.(3) ; 16 * v.(4) + v.(5) |] ;;

(** {v triple_byte_string_to_color string v} *)
let triple_byte_string_to_color = function (s:string) ->
(** The string begins with a #. *)
 let indices = [| 1 ; 2 ; 3 ; 4 ; 5 ; 6 |]
 and f = function i -> hexa_char_to_int s.[i] in
  let v = Array.map f indices in
   ((((( 16 * v.(0) ) + v.(1) ) * 16 + v.(2) ) * 16 + v.(3) ) * 16 + v.(4) ) * 16 + v.(5) ;;


(** {v hexa_char_to_float figure v} *)
let hexa_char_to_float = function (c:char) ->
 match c with
 | '0' -> 0.
 | '1' -> 1.
 | '2' -> 2.
 | '3' -> 3.
 | '4' -> 4.
 | '5' -> 5.
 | '6' -> 6.
 | '7' -> 7.
 | '8' -> 8.
 | '9' -> 9.
 | 'A' -> 10.
 | 'B' -> 11.
 | 'C' -> 12.
 | 'D' -> 13.
 | 'E' -> 14.
 | 'F' -> 15.
 | 'a' -> 10.
 | 'b' -> 11.
 | 'c' -> 12.
 | 'd' -> 13.
 | 'e' -> 14.
 | 'f' -> 15.
 | _ -> failwith "Hexadecimal character not recognized in Readwrite.hexa_char_to_float." ;;
 
(** {v byte_string_to_float string v} *)
let byte_string_to_float = function (s:string) ->
 let a = hexa_char_to_float s.[0]
 and b = hexa_char_to_float s.[1] in
  16. *. a +. b ;;

(** {v triple_byte_string_to_float_luminance string v} *)
let triple_byte_string_to_float_luminance = function (s:string) ->
(** The string begins with a #. *)
 let indices = [| 1 ; 2 ; 3 ; 4 ; 5 ; 6 |]
 and f = function i -> hexa_char_to_float s.[i] in
  let v = Array.map f indices in
   16. *. ( v.(0) +. v.(2) +. v.(4) ) +. v.(1) +. v.(3) +. v.(5) ;;

(** {v triple_byte_string_to_float_red string v} *)
let triple_byte_string_to_float_red = function (s:string) ->
(** The string begins with a #. *)
 let a = hexa_char_to_float s.[1]
 and b = hexa_char_to_float s.[2] in
  16. *. a +. b ;;

(** {v triple_byte_string_to_float_green string v} *)
let triple_byte_string_to_float_green = function (s:string) ->
(** The string begins with a #. *)
 let a = hexa_char_to_float s.[3]
 and b = hexa_char_to_float s.[4] in
  16. *. a +. b ;;

(** {v triple_byte_string_to_float_blue string v} *)
let triple_byte_string_to_float_blue = function (s:string) ->
(** The string begins with a #. *)
 let a = hexa_char_to_float s.[5]
 and b = hexa_char_to_float s.[6] in
  16. *. a +. b ;;

(** {v triple_byte_string_to_float_rgb string v} *)
let triple_byte_string_to_float_rgb = function (s:string) ->
(** The string begins with a #. *)
 let indices = [| 1 ; 2 ; 3 ; 4 ; 5 ; 6 |]
 and f = function i -> hexa_char_to_float s.[i] in
  let v = Array.map f indices in
   [| 16. *. v.(0) +. v.(1) ; 16. *. v.(2) +. v.(3) ; 16. *. v.(4) +. v.(5) |] ;;


(** {v gray_string_to_float_luminance string v} *)
let gray_string_to_float_luminance = function (s:string) ->
 let l = String.length s in
  let c = String.sub s 4 ( l - 4 ) in
   let n = float_of_string c in
    7.65 *. n ;;

(** {v gray_string_to_float_unicolor string v} *)
let gray_string_to_float_unicolor = function (s:string) ->
 let l = String.length s in
(** The character "G" has been suppressed. *)
  let c = String.sub s 4 ( l - 4 ) in
   let n = float_of_string c in
    2.55 *. n ;;

(** {v gray_string_to_float_rgb string v} *)
let gray_string_to_float_rgb = function (s:string) ->
 let c = gray_string_to_float_unicolor s in
  Array.make 3 c ;;

(** {v gray_string_to_int_unicolor string v} *)
let gray_string_to_int_unicolor = function (s:string) ->
 min 255 ( Util.round ( gray_string_to_float_unicolor s ) ) ;;

(** {v gray_string_to_int_luminance string v} *)
let gray_string_to_int_luminance = function (s:string) ->
 min 255 ( Util.round ( gray_string_to_float_luminance s ) ) ;;

(** {v gray_string_to_int_rgb string v} *)
let gray_string_to_int_rgb = function (s:string) ->
 let c = gray_string_to_int_unicolor s in
  Array.make 3 c ;;

(** {v gray_string_to_color string v} *)
let gray_string_to_color = function (s:string) ->
 min 16777215 ( 65793 * ( Util.round ( gray_string_to_float_unicolor s ) ) ) ;;


(** {v name_to_triple_byte string v} *)
let name_to_triple_byte = function (s:string) ->
 match s with
 | "AntiqueWhite" -> "#FAEBD7"
 | "Aqua" -> "#00FFFF"
 | "Aquamarine" -> "#7FFFD4"
 | "Azure" -> "#F0FFFF"
 | "Beige" -> "#F5F5DC"
 | "Bisque" -> "#FFE4C4"
 | "Black" -> "#000000"
 | "BlanchedAlmond" -> "#FFEBCD"
 | "Blue" -> "#0000FF"
 | "BlueViolet" -> "#8A2BE2"
 | "Brown" -> "#A52A2A"
 | "Burlywood" -> "#DEB887"
 | "CadetBlue" -> "#5F9EA0"
 | "Chartreuse" -> "#7FFF00"
 | "Chocolate" -> "#D2691E"
 | "Coral" -> "#FF7F50"
 | "Cornflower" -> "#6495ED"
 | "Cornsilk" -> "#FFF8DC"
 | "Crimson" -> "#DC143C"
 | "Cyan" -> "#00FFFF"
 | "DarkBlue" -> "#00008B"
 | "DarkCyan" -> "#008B8B"
 | "DarkGoldenrod" -> "#B8860B"
 | "DarkGray" -> "#A9A9A9"
 | "DarkGreen" -> "#006400"
 | "DarkKhaki" -> "#BDB76B"
 | "DarkMagenta" -> "#8B008B"
 | "DarkOliveGreen" -> "#556B2F"
 | "DarkOrange" -> "#FF8C00"
 | "DarkOrchid" -> "#9932CC"
 | "DarkRed" -> "#8B0000"
 | "DarkSalmon" -> "#E9967A"
 | "DarkSeaGreen" -> "#8FBC8F"
 | "DarkSlateBlue" -> "#483D8B"
 | "DarkSlateGray" -> "#2F4F4F"
 | "DarkTurquoise" -> "#00CED1"
 | "DarkViolet" -> "#9400D3"
 | "DeepPink" -> "#FF1493"
 | "DeepSkyBlue" -> "#00BFFF"
 | "DimGray" -> "#696969"
 | "DodgerBlue" -> "#1E90FF"
 | "Firebrick" -> "#B22222"
 | "FloralWhite" -> "#FFFAF0"
 | "ForestGreen" -> "#228B22"
 | "Fuchsia" -> "#FF00FF"
 | "Gainsboro" -> "#DCDCDC"
 | "GhostWhite" -> "#F8F8FF"
 | "Gold" -> "#FFD700"
 | "Goldenrod" -> "#DAA520"
 | "Gray" -> "#BEBEBE"
 | "Green" -> "#00FF00"
 | "GreenYellow" -> "#ADFF2F"
 | "Honeydew" -> "#F0FFF0"
 | "HotPink" -> "#FF69B4"
 | "IndianRed" -> "#CD5C5C"
 | "Indigo" -> "#4B0082"
 | "Ivory" -> "#FFFFF0"
 | "Khaki" -> "#F0E68C"
 | "Lavender" -> "#E6E6FA"
 | "LavenderBlush" -> "#FFF0F5"
 | "LawnGreen" -> "#7CFC00"
 | "LemonChiffon" -> "#FFFACD"
 | "LightBlue" -> "#ADD8E6"
 | "LightCoral" -> "#F08080"
 | "LightCyan" -> "#E0FFFF"
 | "LightGoldenrod" -> "#FAFAD2"
 | "LightGray" -> "#D3D3D3"
 | "LightGreen" -> "#90EE90"
 | "LightPink" -> "#FFB6C1"
 | "LightSalmon" -> "#FFA07A"
 | "LightSeaGreen" -> "#20B2AA"
 | "LightSkyBlue" -> "#87CEFA"
 | "LightSlateGray" -> "#778899"
 | "LightSteelBlue" -> "#B0C4DE"
 | "LightYellow" -> "#FFFFE0"
 | "Lime" -> "#00FF00"
 | "LimeGreen" -> "#32CD32"
 | "Linen" -> "#FAF0E6"
 | "Magenta" -> "#FF00FF"
 | "Maroon" -> "#B03060"
 | "MediumAquamarine" -> "#66CDAA"
 | "MediumBlue" -> "#0000CD"
 | "MediumOrchid" -> "#BA55D3"
 | "MediumPurple" -> "#9370DB"
 | "MediumSeaGreen" -> "#3CB371"
 | "MediumSlateBlue" -> "#7B68EE"
 | "MediumSpringGreen" -> "#00FA9A"
 | "MediumTurquoise" -> "#48D1CC"
 | "MediumVioletRed" -> "#C71585"
 | "MidnightBlue" -> "#191970"
 | "MintCream" -> "#F5FFFA"
 | "MistyRose" -> "#FFE4E1"
 | "Moccasin" -> "#FFE4B5"
 | "NavajoWhite" -> "#FFDEAD"
 | "Navy" -> "#000080"
 | "OldLace" -> "#FDF5E6"
 | "Olive" -> "#808000"
 | "OliveDrab" -> "#6B8E23"
 | "Orange" -> "#FFA500"
 | "OrangeRed" -> "#FF4500"
 | "Orchid" -> "#DA70D6"
 | "PaleGoldenrod" -> "#EEE8AA"
 | "PaleGreen" -> "#98FB98"
 | "PaleTurquoise" -> "#AFEEEE"
 | "PaleVioletRed" -> "#DB7093"
 | "PapayaWhip" -> "#FFEFD5"
 | "PeachPuff" -> "#FFDAB9"
 | "Peru" -> "#CD853F"
 | "Pink" -> "#FFC0CB"
 | "Plum" -> "#DDA0DD"
 | "PowderBlue" -> "#B0E0E6"
 | "Purple" -> "#A020F0"
 | "Red" -> "#FF0000"
 | "RosyBrown" -> "#BC8F8F"
 | "RoyalBlue" -> "#4169E1"
 | "SaddleBrown" -> "#8B4513"
 | "Salmon" -> "#FA8072"
 | "SandyBrown" -> "#F4A460"
 | "SeaGreen" -> "#2E8B57"
 | "Seashell" -> "#FFF5EE"
 | "Sienna" -> "#A0522D"
 | "Silver" -> "#C0C0C0"
 | "SkyBlue" -> "#87CEEB"
 | "SlateBlue" -> "#6A5ACD"
 | "SlateGray" -> "#708090"
 | "Snow" -> "#FFFAFA"
 | "SpringGreen" -> "#00FF7F"
 | "SteelBlue" -> "#4682B4"
 | "Tan" -> "#D2B48C"
 | "Teal" -> "#008080"
 | "Thistle" -> "#D8BFD8"
 | "Tomato" -> "#FF6347"
 | "Turquoise" -> "#40E0D0"
 | "Violet" -> "#EE82EE"
 | "Wheat" -> "#F5DEB3"
 | "White" -> "#FFFFFF"
 | "WhiteSmoke" -> "#F5F5F5"
 | "Yellow" -> "#FFFF00"
 | "YellowGreen" -> "#9ACD32"
 | _ -> failwith "Not an X11 color name in Readwrite.name_to_triple_byte." ;;

(** {v string_to_float_red string v} *)
let string_to_float_red = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_float_red s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_float_unicolor s
  else
   triple_byte_string_to_float_red ( name_to_triple_byte s )
  end ;;

(** {v string_to_float_green string v} *)
let string_to_float_green = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_float_green s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_float_unicolor s
  else
   triple_byte_string_to_float_green ( name_to_triple_byte s )
  end ;;

(** {v string_to_float_blue string v} *)
let string_to_float_blue = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_float_blue s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_float_unicolor s
  else
   triple_byte_string_to_float_blue ( name_to_triple_byte s )
  end ;;

(** {v string_to_float_luminance string v} *)
let string_to_float_luminance = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_float_luminance s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_float_luminance s
  else
   triple_byte_string_to_float_luminance ( name_to_triple_byte s )
  end ;;

(** {v string_to_float_rgb string v} *)
let string_to_float_rgb = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_float_rgb s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_float_rgb s
  else
   triple_byte_string_to_float_rgb ( name_to_triple_byte s )
  end ;;


(** {v string_to_int_red string v} *)
let string_to_int_red = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_int_red s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_int_unicolor s
  else
   triple_byte_string_to_int_red ( name_to_triple_byte s )
  end ;;

(** {v string_to_int_green string v} *)
let string_to_int_green = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_int_green s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_int_unicolor s
  else
   triple_byte_string_to_int_green ( name_to_triple_byte s )
  end ;;

(** {v string_to_int_blue string v} *)
let string_to_int_blue = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_int_blue s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_int_unicolor s
  else
   triple_byte_string_to_int_blue ( name_to_triple_byte s )
  end ;;

(** {v string_to_int_luminance string v} *)
let string_to_int_luminance = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_int_luminance s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_int_luminance s
  else
   triple_byte_string_to_int_luminance ( name_to_triple_byte s )
  end ;;

(** {v string_to_int_rgb string v} *)
let string_to_int_rgb = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_int_rgb s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_int_rgb s
  else
   triple_byte_string_to_int_rgb ( name_to_triple_byte s )
  end ;;

(** {v string_to_color string v} *)
let string_to_color = function (s:string) ->
 if s.[0] = '#' then triple_byte_string_to_color s
 else
  begin
  if Str.string_match ( Str.regexp "Gray[0-9]" ) s 0 then
   gray_string_to_color s
  else
   triple_byte_string_to_color ( name_to_triple_byte s )
  end ;;


(** {v read_float_value file_name v} *)
let read_float_value = function (fname:string) ->
( let ic = open_in fname in
   let z = input_value ic in
    close_in_noerr ic ;
    z : float ) ;;

(** {v read_float_array_value file_name v} *)
let read_float_array_value = function (fname:string) ->
( let ic = open_in fname in
   let z = input_value ic in
    close_in_noerr ic ;
    z : float array ) ;;

(** {v write_float_value real file_name v} *)
let write_float_value = fun (x:float) (fname:string) ->
 let oc = open_out fname in
  output_value oc x ;
   close_out_noerr oc ;;

(** {v write_float_array_value real_vector file_name v} *)
let write_float_array_value = fun (x:float array) (fname:string) ->
 let oc = open_out fname in
  output_value oc x ;
   close_out_noerr oc ;;




(** {C § } *)
(** 
{1 Conversions d'images}
{1 Picture conversions}
*)
(** {C  } *)




(** {v color_to_int_rgb color v} *)
let color_to_int_rgb = function (c:int) ->
 let r = c / 65536 and g = ( c / 256 ) mod 256 and b = c mod 256 in
  [| r ; g ; b |] ;;

(** {v color_to_float_rgb color v} *)
let color_to_float_rgb = function (c:int) ->
 let r = c / 65536 and g = ( c / 256 ) mod 256 and b = c mod 256 in
  Array.map float [| r ; g ; b |] ;;


(** {v matrix_color_to_int_rgb color_matrix v} *)
let matrix_color_to_int_rgb = function (m:int array array) ->
 let l = Array.length m
 and c = Array.length m.(0) in
  let cc = pred c
  and r = Array.make_matrix l c 0
  and g = Array.make_matrix l c 0
  and b = Array.make_matrix l c 0 in
   for i = 0 to pred l do
    let row_input = m.(i)
    and row_red = r.(i)
    and row_green = g.(i)
    and row_blue = b.(i) in
     for j = 0 to cc do
      let color = row_input.(j) in
       let couleur = color /256 in
        row_red.(j) <- couleur / 256 ;
        row_green.(j) <- couleur mod 256 ;
        row_blue.(j) <- color mod 256 ;
     done
   done ;
   [| r ; g ; b |] ;;

(** {v matrix_int_rgb_to_color rgb_matrix v} *)
let matrix_int_rgb_to_color = function (m:int array array array) ->
 let red = m.(0)
 and green = m.(1)
 and blue = m.(2) in
  let l = Array.length blue
  and c = Array.length blue.(0) in
   let w = Array.make_matrix l c 0
   and cc = pred c in
    for i = 0 to pred l do
     let row_output = w.(i)
     and row_red = red.(i)
     and row_green = green.(i)
     and row_blue = blue.(i) in
      for j = 0 to cc do
       row_output.(j) <- row_blue.(j) + 256 * ( row_green.(j) + 256 * row_red.(j) )
      done
    done ;
    w ;;

(** {v matrix_int_unicolor_to_color matrix v} *)
let matrix_int_unicolor_to_color = function (m:int array array) ->
 Matrix.matrix_int_scal_mult 65793 m ;;

(** {v matrix_float_rgb_to_int_rgb rgb_matrix v} *)
let matrix_float_rgb_to_int_rgb = function (m:float array array array) ->
 Array.map ( function x -> Matrix.matrix_int_clip 255 ( Matrix.matrix_float_round x ) ) m ;;

(** {v matrix_int_rgb_to_float_rgb rgb_matrix v} *)
let matrix_int_rgb_to_float_rgb = function (m:int array array array) ->
 Array.map Matrix.float_of_matrix m ;;


(** {v matrix_float_rgb_to_color rgb_matrix v} *)
let matrix_float_rgb_to_color = function (m:float array array array) ->
 matrix_int_rgb_to_color ( matrix_float_rgb_to_int_rgb m ) ;;

(** {v matrix_color_to_float_rgb color_matrix v} *)
let matrix_color_to_float_rgb = function (m:int array array) ->
 Array.map Matrix.float_of_matrix ( matrix_color_to_int_rgb m ) ;;

(** {v matrix_float_unicolor_to_color matrix v} *)
let matrix_float_unicolor_to_color = function (m:float array array) ->
 Matrix.matrix_int_clip 16777215 ( Matrix.matrix_int_scal_mult 65793 ( Matrix.matrix_float_round m ) ) ;;


(** {v matrix_float_rgb_to_gray rgb_matrix v} *)
let matrix_float_rgb_to_gray = function (m:float array array array) ->
 let r = m.(0)
 and g = m.(1)
 and b = m.(2)
 and constant = 1. /. 3. in
  let l = Array.length r
  and c = Array.length r.(0) in
   let n = Array.make_matrix l c 0.
   and cc = pred c in
    for i = 0 to pred l do
     let row_output = n.(i)
     and row_red = r.(i)
     and row_green = g.(i)
     and row_blue = b.(i) in
      for j = 0 to cc do
       row_output.(j) <- constant *. ( row_red.(j) +. row_green.(j) +. row_blue.(j) )
      done
    done ;
    n ;;

(** {v matrix_int_rgb_to_gray rgb_matrix v} *)
let matrix_int_rgb_to_gray = function (m:int array array array) ->
 let n = matrix_int_rgb_to_float_rgb m in
  let o = matrix_float_rgb_to_gray n in
   Matrix.matrix_int_clip 255 ( Matrix.matrix_float_round o ) ;;

(** {v matrix_color_to_int_gray color_matrix v} *)
let matrix_color_to_int_gray = function (m:int array array) ->
 let n = matrix_color_to_int_rgb m in
  matrix_int_rgb_to_gray n ;;

(** {v matrix_color_to_float_gray color_matrix v} *)
let matrix_color_to_float_gray = function (m:int array array) ->
 let n = matrix_color_to_float_rgb m in
  matrix_float_rgb_to_gray n ;;


(** {v matrix_int_rgb_to_luminance rgb_matrix v} *)
let matrix_int_rgb_to_luminance = function (m:int array array array) ->
 let r = m.(0)
 and g = m.(1)
 and b = m.(2) in
  let l = Array.length r
  and c = Array.length r.(0) in
   let n = Array.make_matrix l c 0
   and cc = pred c in
    for i = 0 to pred l do
     let row_output = n.(i)
     and row_red = r.(i)
     and row_green = g.(i)
     and row_blue = b.(i) in
      for j = 0 to cc do
       row_output.(j) <- row_red.(j) + row_green.(j) + row_blue.(j)
      done
    done ;
    n ;;

(** {v matrix_color_to_int_luminance color_matrix v} *)
let matrix_color_to_int_luminance = function (m:int array array) ->
 let n = matrix_color_to_int_rgb m in
  matrix_int_rgb_to_luminance n ;;

(** {v matrix_float_rgb_to_luminance rgb_matrix v} *)
let matrix_float_rgb_to_luminance = function (m:float array array array) ->
 let r = m.(0)
 and g = m.(1)
 and b = m.(2) in
  let l = Array.length r
  and c = Array.length r.(0) in
   let n = Array.make_matrix l c 0.
   and cc = pred c in
    for i = 0 to pred l do
     let row_output = n.(i)
     and row_red = r.(i)
     and row_green = g.(i)
     and row_blue = b.(i) in
      for j = 0 to cc do
       row_output.(j) <- row_red.(j) +. row_green.(j) +. row_blue.(j)
      done
    done ;
    n ;;

(** {v matrix_color_to_float_luminance color_matrix v} *)
let matrix_color_to_float_luminance = function (m:int array array) ->
 let n = matrix_color_to_float_rgb m in
  matrix_float_rgb_to_luminance n ;;


(** {v matrix_float_unicolor_under_sample edge matrix v} *)
let matrix_float_unicolor_under_sample = fun (n:int) (m:float array array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  if ( r mod n != 0 ) || ( c mod n != 0 ) then
   failwith "The edge of the square must be a divisor of the number of lines and of the number of columns of the matrix in Readwrite.matrix_float_unicolor_under_sample." ;
   let rr = r / n
   and cc = c / n
   and f = function x -> Matrix.vector_float_sum ( Matrix.vector_float_demakeup x )
   and s = 1. /. ( float ( n * n ) )
   and nn = pred n in
    let w = Array.make_matrix rr cc 0.
    and row = Array.make cc 0.
    and g = function x -> Matrix.vector_foa_demakeup ( Matrix.vector_foa_cut cc ( Matrix.Float_vector_cons x ) )
    and ccc = pred cc in
     let h = function x -> Array.map f ( g x ) in
      for i = 0 to pred rr do
       let row_output = w.(i)
       and ni = n * i in
        for k = 0 to nn do
         let row_input = h m.( ni + k ) in
          for j = 0 to ccc do
           row.(j) <- row.(j) +. row_input.(j)
          done ;
        done ;
        for j = 0 to ccc do
         row_output.(j) <- row.(j) *. s ;
         row.(j) <- 0.
        done
      done ;
      w ;;


(** {v matrix_float_rgb_under_sample edge rgb_matrix v} *)
let matrix_float_rgb_under_sample = fun (n:int) (m:float array array array) ->
 Array.map ( matrix_float_unicolor_under_sample n ) m ;;

(** {v matrix_int_unicolor_under_sample edge matrix v} *)
let matrix_int_unicolor_under_sample = fun (n:int) (m:int array array) ->
 Matrix.int_of_matrix ( matrix_float_unicolor_under_sample n ( Matrix.float_of_matrix m ) ) ;;

(** {v matrix_int_rgb_under_sample edge rgb_matrix v} *)
let matrix_int_rgb_under_sample = fun (n:int) (m:int array array array) ->
 matrix_float_rgb_to_int_rgb ( matrix_float_rgb_under_sample n ( matrix_int_rgb_to_float_rgb m ) ) ;;


(** {v rgb_crop rgb_matrix beg-row end-row beg-col end-col v} *)
let rgb_crop = fun m (i:int) (ii:int) (j:int) (jj:int) ->
 Array.map ( function x -> Matrix.sub_matrix x i ii j jj ) m ;;

(** {v float_unicolor_magnify factor matrix v} *)
let float_unicolor_magnify = fun (factor:int) (m:float array array) ->
 let r = Array.length m
 and c = Array.length m.(0) in
  let cc = pred c
  and w = Array.make_matrix r c ( Matrix.Float_cons 0. ) in
   for i = 0 to pred r do
    let row_input = m.(i)
    and row_output = w.(i) in
     for j = 0 to cc do
      row_output.(j) <- Matrix.Float_matrix_cons ( Array.make_matrix factor factor row_input.(j) )
     done
   done ;
   Matrix.matrix_float_demakeup ( Matrix.matrix_foa_paste ( Matrix.Foa_matrix_cons w ) ) ;;

(** {v float_rgb_magnify factor rgb_matrix v} *)
let float_rgb_magnify = fun (factor:int) (m:float array array array) ->
 Array.map ( float_unicolor_magnify factor ) m ;;




(** {C § } *)
(** 
{1 Fichiers de vecteurs et matrices}
{1 Vector and matrix files}
*)
(** {C  } *)




(** {v vector_int_of_string string v} *)
let vector_int_of_string = function (s:string) ->
 let l = String.length s in
  let st = String.sub s 3 ( l - 6 ) in
   let liste = ref ( Str.split ( Str.regexp_string " ; " ) st ) in
    let ll = List.length !liste in
     let v = Array.make ll 0 in
      for i = 0 to pred ll do
       v.(i) <- int_of_string ( List.hd !liste ) ;
       liste := List.tl !liste ;
      done ;
      v ;;

(** {v vector_int_write vector filename v} *)
let vector_int_write = fun (v:int array) (fname:string) ->
 let oc = open_out fname
 and l = Array.length v in
  for i = 0 to pred l do
   output_string oc ( ( string_of_int v.(i) ) ^ "\n" ) ;
  done ;
  close_out_noerr oc ;;

(** {v matrix_int_write matrix filename v} *)
let matrix_int_write = fun (v:int array array) (fname:string) ->
 let oc = open_out fname
 and l = Array.length v in
  for i = 0 to pred l do
   output_string oc ( ( Matrix.string_of_vector_int v.(i) ) ^ "\n" ) ;
  done ;
  close_out_noerr oc ;;

(** {v list_reverse_int_read v} *)
let list_reverse_int_read = function (fname:string) ->
 let ic = open_in fname in
   let l = ref [] in
    begin
     try
      while true do
       l := ( int_of_string ( input_line ic ) ) :: !l ;
      done
     with End_of_file -> close_in_noerr ic
    end ;
    !l ;;

(** {v list_int_read v} *)
let list_int_read = function (fname:string) ->
 List.rev ( list_reverse_int_read fname ) ;;

(** {v vector_reverse_int_read v} *)
let vector_reverse_int_read = function (fname:string) ->
 Array.of_list ( list_reverse_int_read fname ) ;;

(** {v vector_int_read v} *)
let vector_int_read = function (fname:string) ->
 Array.of_list ( list_int_read fname ) ;;

(** {v matrix_int_read filename v} *)
let matrix_int_read = function (fname:string) ->
 let ic = open_in fname in
   let l = ref []
   and i = ref 0 in
    begin
     try
      while true do
       l := ( input_line ic ) :: !l ;
       i := succ !i ;
      done
     with End_of_file -> close_in_noerr ic
    end ;
    let m = Array.make_matrix ( succ !i ) 0 0 in
     for j = pred !i downto 0 do
      m.(j) <- vector_int_of_string ( List.hd !l ) ;
      l := List.tl !l ;
     done ;
     m ;;


(** {v vector_float_of_string string v} *)
let vector_float_of_string = function (s:string) ->
 let l = String.length s in
  let st = String.sub s 3 ( l - 6 ) in
   let liste = ref ( Str.split ( Str.regexp_string " ; " ) st ) in
    let ll = List.length !liste in
     let v = Array.make ll 0. in
      for i = 0 to pred ll do
       v.(i) <- float_of_string ( List.hd !liste ) ;
       liste := List.tl !liste ;
      done ;
      v ;;

(** {v vector_float_write vector filename v} *)
let vector_float_write = fun (v:float array) (fname:string) ->
 let oc = open_out fname
 and l = Array.length v in
  for i = 0 to pred l do
   output_string oc ( ( string_of_float v.(i) ) ^ "\n" ) ;
  done ;
  close_out_noerr oc ;;

(** {v matrix_float_write matrix filename v} *)
let matrix_float_write = fun (m:float array array) (fname:string) ->
 let oc = open_out fname
 and l = Array.length m in
  for i = 0 to pred l do
   output_string oc ( ( Matrix.string_of_vector_float m.(i) ) ^ "\n" ) ;
  done ;
  close_out_noerr oc ;;

(** {v list_reverse_float_read v} *)
let list_reverse_float_read = function (fname:string) ->
 let ic = open_in fname in
   let l = ref [] in
    begin
     try
      while true do
       l := ( float_of_string ( input_line ic ) ) :: !l ;
      done
     with End_of_file -> close_in_noerr ic
    end ;
    !l ;;

(** {v list_float_read v} *)
let list_float_read = function (fname:string) ->
 List.rev ( list_reverse_float_read fname ) ;;

(** {v vector_reverse_float_read v} *)
let vector_reverse_float_read = function (fname:string) ->
 Array.of_list ( list_reverse_float_read fname ) ;;

(** {v vector_float_read v} *)
let vector_float_read = function (fname:string) ->
 Array.of_list ( list_float_read fname ) ;;

(** {v matrix_float_read filename v} *)
let matrix_float_read = function (fname:string) ->
 let ic = open_in fname in
   let l = ref []
   and i = ref 0 in
    begin
     try
      while true do
       l := ( input_line ic ) :: !l ;
       i := succ !i ;
      done
     with End_of_file -> close_in_noerr ic
    end ;
    let m = Array.make_matrix ( succ !i ) 0 0. in
     for j = pred !i downto 0 do
      m.(j) <- vector_float_of_string ( List.hd !l ) ;
      l := List.tl !l ;
     done ;
     m ;;




(** {C § } *)
(** 
{1 Fichiers d'images xpm}
{1 Xpm picture files}
*)
(** {C  } *)




(** The XPM files are of type text; they are supposed to have already been
introduced in the memory of Ocaml as arrays of character strings.

Les fichiers XPM sont de type texte ; on suppose qu'ils ont déja été rentrés 
dans la mémoire d'Ocaml sous forme de tableaux de chaînes de caractères.*)

(** {v xpm_colors_index string_array v} *)
let xpm_colors_index = function (v:string array) ->
 Util.vector_find_first Util.string_eq "/* colors */" v ;;

(** {v xpm_pixels_index string_array v} *)
let xpm_pixels_index = function (v:string array) ->
 Util.vector_find_first Util.string_eq "/* pixels */" v ;;

(** {v xpm_pixels_end string_array v} *)
let xpm_pixels_end = function (v:string array) ->
 Util.vector_find_first Util.string_eq "};" v ;;

(** {v xpm_sizes string_array v} *)
let xpm_sizes = function (v:string array) ->
(** Output format : [| width ; height ; ncolors ; chars_per_pixel |] *)
 let i = xpm_colors_index v in
  let c = v.( pred i ) in
   let l = String.length c in
    let cc = String.sub c 1 ( l - 3 )
    and r = Str.regexp_string " " in
     let w = Array.of_list ( Str.split r cc ) in
      let x = Array.map int_of_string w in
(** Eliminate the last color : "None". *)
       x.(2) <- pred x.(2) ;
       x ;;

(** {v xpm_colors_strings string_array v} *)
let xpm_colors_strings = function (v:string array) ->
 let i = xpm_colors_index v
 and j = xpm_pixels_index v in
(** Eliminate the last color : "None". *)
  Array.sub v ( succ i ) ( j - i - 2 ) ;;

(** {v xpm_colors_symbols string_array v} *)
let xpm_colors_symbols = function (v:string array) ->
 let l = (xpm_sizes v).(3)
 and c = xpm_colors_strings v in
  let f = function (s:string) -> Str.regexp_string ( String.sub s 1 l ) in
   Array.map f c ;;

(** {v xpm_colors_values string_array v} *)
let xpm_colors_values = function (v:string array) ->
 let l = (xpm_sizes v).(3)
 and c = xpm_colors_strings v in
  let f = function (s:string) ->
   begin
    let tout = Str.string_after s ( 4 + l ) in
     Str.string_before tout ( ( String.length tout ) - 2 )
   end in
   Array.map f c ;;

(** {v xpm_colors_substitution string_array v} *)
let xpm_colors_substitution = function (v:string array) ->
 let symbols = xpm_colors_symbols v
 and values = xpm_colors_values v in
  let l = Array.length symbols in
   function (y:string) ->
    begin
     let z = ref y
     and i = ref 0 in
      while !i < l do
       let expression = symbols.(!i) in
        if string_match expression !z 0 then
         begin
          z := Str.global_substitute expression ( function x -> values.(!i) ) y ;
          i := l
         end
        else i := succ !i
     done ;
     !z
    end ;;

(** {v pixels_line_splitting string_array v} *)
let pixels_line_splitting = fun (n:int) (c:int) (x:string) ->
 if ( String.length x ) < 2 + n * c then failwith "Bad sizes in Readwrite.pixels_line_splitting" ;
 let v = Array.make n "" in
  for i = 0 to pred n do
   let ii = 1 + i * c in
    v.(i) <- String.sub x ii c
  done ;
  v ;;

(** {v xpm_pixels_strings string_array v} *)
let xpm_pixels_strings = function (v:string array) ->
 let l = xpm_pixels_end v
 and sizes = xpm_sizes v
 and j = xpm_pixels_index v in
(** Eliminate the trailing lines. *)
  let vv = Array.sub v ( succ j ) ( l - j - 1 )
  and c = sizes.(3)
  and w = sizes.(0) in
   let f = pixels_line_splitting w c in
    Array.map f vv ;;

(** {v xpm_pixels_values string_array v} *)
let xpm_pixels_values = function (v:string array) ->
 let p = xpm_pixels_strings v in
  let lines = pred ( Array.length p ) in
   let replace = xpm_colors_substitution v in
    for j = 0 to lines do
     p.(j) <- Array.map replace p.(j) ;
    done ;
    p ;;

(** {v xpm_int_red string_array v} *)
let xpm_int_red = function (v:string array) ->
 let vv = xpm_pixels_values v in
  Array.map ( Array.map string_to_int_red ) vv ;;

(** {v xpm_int_green string_array v} *)
let xpm_int_green = function (v:string array) ->
 let vv = xpm_pixels_values v in
  Array.map ( Array.map string_to_int_green ) vv ;;

(** {v xpm_int_blue string_array v} *)
let xpm_int_blue = function (v:string array) ->
 let vv = xpm_pixels_values v in
  Array.map ( Array.map string_to_int_blue ) vv ;;

(** {v xpm_int_luminance string_array v} *)
let xpm_int_luminance = function (v:string array) ->
 let vv = xpm_pixels_values v in
  Array.map ( Array.map string_to_int_luminance ) vv ;;

(** {v xpm_int_rgb string_array v} *)
let xpm_int_rgb = function (v:string array) ->
 let vv = xpm_pixels_values v in
  let l = Array.length vv
  and w = Array.length vv.(0) in
   let red = Array.make_matrix l w 0
   and green = Array.make_matrix l w 0
   and blue = Array.make_matrix l w 0
   and ww = pred w in
    for i = 0 to pred l do
     let red_output = red.(i)
     and green_output = green.(i)
     and blue_output = blue.(i)
     and row_input = vv.(i) in
       for j = 0 to ww do
        let x = string_to_int_rgb row_input.(j) in
         red_output.(j) <- x.(0) ;
         green_output.(j) <- x.(1) ;
         blue_output.(j) <- x.(2) ;
       done
     done ;
     [| red ; green ; blue |] ;;

(** {v xpm_float_red string_array v} *)
let xpm_float_red = function (v:string array) ->
 let vv = xpm_pixels_values v in
  Array.map ( Array.map string_to_float_red ) vv ;;

(** {v xpm_float_green string_array v} *)
let xpm_float_green = function (v:string array) ->
 let vv = xpm_pixels_values v in
  Array.map ( Array.map string_to_float_green ) vv ;;

(** {v xpm_float_blue string_array v} *)
let xpm_float_blue = function (v:string array) ->
 let vv = xpm_pixels_values v in
  Array.map ( Array.map string_to_float_blue ) vv ;;

(** {v xpm_float_luminance string_array v} *)
let xpm_float_luminance = function (v:string array) ->
 let vv = xpm_pixels_values v in
  Array.map ( Array.map string_to_float_luminance ) vv ;;

(** {v xpm_float_rgb string_array v} *)
let xpm_float_rgb = function (v:string array) ->
 let vv = xpm_pixels_values v in
  let l = Array.length vv
  and w = Array.length vv.(0) in
   let red = Array.make_matrix l w 0.
   and green = Array.make_matrix l w 0.
   and blue = Array.make_matrix l w 0.
   and ww = pred w in
    for i = 0 to pred l do
     let red_output = red.(i)
     and green_output = green.(i)
     and blue_output = blue.(i)
     and row_input = vv.(i) in
       for j = 0 to ww do
        let x = string_to_float_rgb row_input.(j) in
         red_output.(j) <- x.(0) ;
         green_output.(j) <- x.(1) ;
         blue_output.(j) <- x.(2) ;
       done
     done ;
     [| red ; green ; blue |] ;;

(** {v xpm_color_array string_array v} *)
let xpm_color_array = function (v:string array) ->
 let vv = xpm_pixels_values v in
  let l = Array.length vv
  and w = Array.length vv.(0) in
   let p = Array.make_matrix l w 0
   and ww = pred w in
    for i = 0 to pred l do
     let row_output = p.(i)
     and row_input = vv.(i) in
       for j = 0 to ww do
         row_output.(j) <- string_to_color row_input.(j)
       done
     done ;
     p ;;




(** {C § } *)
(** 
{1 Fichiers d'images pnm : pgm et ppm}
{1 Pnm picture files: pgm and ppm}
*)
(** {C  } *)




(** The PNM files split into three classes: PBM with one bit per pixel,
PGM with grey levels, PPM with three primary colors. They may be saved in text files
or in consecutive bytes.

Les fichiers PNM se décomposent en trois classes : PBM à un bit par pixel, 
PGM en niveaux de gris, PPM en trichromie. Ils peuvent être stockés en fichiers textes
ou en octets consécutifs. *)


(** {v read_pnm_header filename v} *)
let read_pnm_header = function (s:string) ->
 let a = ref ( array_read_text_header 3 s )
 and i = ref 1 in
  while !a.(!i).[0] = '#' do
   a := array_read_text_header ( !i + 3 ) s ;
   incr i
  done ;
  !a ;;


(** {v ppm_header_of_rgb rgb_matrix v} *)
let ppm_header_of_rgb = function m ->
 let l = Array.length m.(0)
 and c = Array.length m.(0).(0) in
  [| "P6" ; ( string_of_int c ) ^ " " ^ ( string_of_int l ) ; "255" |] ;;

(** {v pgm_header_of_unicolor matrix v} *)
let pgm_header_of_unicolor = function m ->
 let l = Array.length m
 and c = Array.length m.(0) in
  [| "P5" ; ( string_of_int c ) ^ " " ^ ( string_of_int l ) ; "255" |] ;;


(** {v read_pbm_binary_int_gray filename v} *)
let read_pbm_binary_int_gray = function (s:string) ->
 let e = read_pnm_header s
 and maximum = 255
 and dot = String.make 1 ( char_of_int 10 ) in
  if e.(0).[1] != '4' then failwith "Not a pbm_binary file in Readwrite.read_pbm_binary_int_gray." ;
  let l = Array.length e in
   let one = l - 2
   and sharp = ( if l = 3 then "" else Array.fold_left ( fun x y -> x ^ dot ^ y ) "" ( Array.sub e 1 ( l - 3 ) ) ) in
    let size = e.(one)
    and r = Str.regexp_string " " in
     let dim = Array.of_list ( Str.split r size ) in
      let c = int_of_string dim.(0)
      and l = int_of_string dim.(1) in
       let chaine = e.(0) ^ dot ^ e.(one) ^ dot ^ sharp
       and taille = l * c / 8 in
        let offset = String.length chaine in
         let v = array_read_binary_file ( offset + taille ) s in
          let vv = Array.sub v offset taille
          and cc = pred c
          and result = Array.make_matrix l c 0 in
           for i = 0 to pred l do
            let row = result.(i)
            and ii = i * c in
             for j = 0 to cc do
              let jj = ii + j in
               row.(j) <- ( 1 - ( ( vv.( jj / 8 ) lsr ( 7 - ( jj mod 8 ) ) ) mod 2 ) ) * maximum
             done
           done ;
           result ;;

(** {v read_pbm_binary_int_unicolor filename v} *)
let read_pbm_binary_int_unicolor = function (s:string) ->
 read_pbm_binary_int_gray s ;;

(** {v read_pbm_binary_int_rbg filename v} *)
let read_pbm_binary_int_rbg = function (s:string) ->
 let r = read_pbm_binary_int_gray s in
  let g = Matrix.matrix_int_copy r
  and b = Matrix.matrix_int_copy r in
   [| r ; g ; b |]


(** {v read_pbm_binary_color filename v} *)
let read_pbm_binary_color = function (s:string) ->
 Array.map ( Array.map ( ( * ) 65793 ) ) ( read_pbm_binary_int_gray s ) ;;


(** {v read_pbm_binary_float_gray filename v} *)
let read_pbm_binary_float_gray = function (s:string) ->
 Array.map ( Array.map float ) ( read_pbm_binary_int_gray s ) ;;

(** {v read_pbm_binary_float_unicolor filename v} *)
let read_pbm_binary_float_unicolor = function (s:string) ->
 read_pbm_binary_float_gray s ;;

(** {v read_pbm_binary_float_rgb filename v} *)
let read_pbm_binary_float_rgb = function (s:string) ->
 let r = read_pbm_binary_float_gray s in
  let g = Matrix.matrix_float_copy r
  and b = Matrix.matrix_float_copy r in
   [| r ; g ; b |]



(** {v read_pgm_binary_int_gray filename v} *)
let read_pgm_binary_int_gray = function (s:string) ->
 let e = read_pnm_header s
 and dot = String.make 1 ( char_of_int 10 ) in
  if e.(0).[1] != '5' then failwith "Not a pgm_binary file in Readwrite.read_pgm_binary_int_gray." ;
  let l = Array.length e in
   let one = l - 2
   and two = l - 1
   and sharp = ( if l = 3 then "" else Array.fold_left ( fun x y -> x ^ dot ^ y ) "" ( Array.sub e 1 ( l - 3 ) ) ) in
    let size = e.(one)
    and depth = e.(two)
    and r = Str.regexp_string " " in
     let dim = Array.of_list ( Str.split r size )
     and atom = int_of_string depth in
      if atom != 255 then failwith "Only gray files with depth 255 are taken in account in Readwrite.read_pgm_binary_int_gray." ;
      let c = int_of_string dim.(0)
      and l = int_of_string dim.(1) in
       let chaine = e.(0) ^ dot ^ e.(one) ^ dot ^ e.(two) ^ dot ^ sharp in
        let offset = String.length chaine in
         let v = array_read_binary_file ( offset + l * c ) s in
          Util.vector_to_matrix l c ( Array.sub v offset ( l * c ) ) ;;

(** {v read_pgm_binary_int_unicolor filename v} *)
let read_pgm_binary_int_unicolor = function (s:string) ->
 read_pgm_binary_int_gray s ;;

(** {v read_pgm_binary_int_rbg filename v} *)
let read_pgm_binary_int_rbg = function (s:string) ->
 let r = read_pgm_binary_int_gray s in
  let g = Matrix.matrix_int_copy r
  and b = Matrix.matrix_int_copy r in
   [| r ; g ; b |]

(** {v write_pgm_binary_int_unicolor matrix filename v} *)
let write_pgm_binary_int_unicolor = fun (m:int array array) (fname:string) ->
 let oc = open_out fname
 and l = Array.length m
 and c = Array.length m.(0) in
  output_string oc ( "P5\n" ^ ( string_of_int c ) ^ " " ^ ( string_of_int l ) ^ "\n255\n" ) ;
  set_binary_mode_out oc true ;
  let cc = pred c in
   for i = 0 to pred l do
    let row = m.(i) in
     for j = 0 to cc do
      output_byte oc row.(j)
     done ;
   done ;
   close_out_noerr oc ;;


(** {v read_pgm_binary_color filename v} *)
let read_pgm_binary_color = function (s:string) ->
 Array.map ( Array.map ( ( * ) 65793 ) ) ( read_pgm_binary_int_gray s ) ;;


(** {v read_pgm_binary_float_gray filename v} *)
let read_pgm_binary_float_gray = function (s:string) ->
 Array.map ( Array.map float ) ( read_pgm_binary_int_gray s ) ;;

(** {v read_pgm_binary_float_unicolor filename v} *)
let read_pgm_binary_float_unicolor = function (s:string) ->
 read_pgm_binary_float_gray s ;;

(** {v read_pgm_binary_float_rgb filename v} *)
let read_pgm_binary_float_rgb = function (s:string) ->
 let r = read_pgm_binary_float_gray s in
  let g = Matrix.matrix_float_copy r
  and b = Matrix.matrix_float_copy r in
   [| r ; g ; b |]


(** {v write_pgm_binary_float_unicolor matrix filename v} *)
let write_pgm_binary_float_unicolor = fun (m:float array array) (fname:string) ->
 let oc = open_out fname
 and l = Array.length m
 and c = Array.length m.(0) in
  output_string oc ( "P5\n" ^ ( string_of_int c ) ^ " " ^ ( string_of_int l ) ^ "\n255\n" ) ;
  set_binary_mode_out oc true ;
  let cc = pred c in
   for i = 0 to pred l do
    let row = m.(i) in
     for j = 0 to cc do
      output_byte oc ( min 255 ( Util.round row.(j) ) )
     done ;
   done ;
   close_out_noerr oc ;;



(** {v read_ppm_binary_int_rgb filename v} *)
let read_ppm_binary_int_rgb = function (s:string) ->
 let e = read_pnm_header s
 and dot = String.make 1 ( char_of_int 10 ) in
  if e.(0).[1] != '6' then failwith "Not a ppm_binary file in Readwrite.read_ppm_binary_int_rgb." ;
  let l = Array.length e in
   let one = l - 2
   and two = l - 1
   and sharp = ( if l = 3 then "" else Array.fold_left ( fun x y -> x ^ dot ^ y ) "" ( Array.sub e 1 ( l - 3 ) ) ) in
    let size = e.(one)
    and depth = e.(two)
    and r = Str.regexp_string " " in
     let dim = Array.of_list ( Str.split r size )
     and atom = int_of_string depth in
      if atom != 255 then failwith "Only color files with depth 255 are taken in account in Readwrite.read_ppm_binary_int_rgb." ;
      let c = int_of_string dim.(0)
      and l = int_of_string dim.(1)
      and dot = String.make 1 ( char_of_int 10 ) in
       let chaine = e.(0) ^ dot ^ e.(one) ^ dot ^ e.(two) ^ dot ^ sharp
       and taille = 3 * l * c in
        let offset = String.length chaine in
         let v = array_read_binary_file ( offset + taille ) s in
          let vv = Array.sub v offset taille in
           Array.map ( Util.vector_to_matrix l c ) ( Util.vector_spray 3 vv ) ;;

(** {v read_ppm_binary_int_red filename v} *)
let read_ppm_binary_int_red = function (s:string) ->
 ( read_ppm_binary_int_rgb s ).(0) ;;

(** {v read_ppm_binary_int_green filename v} *)
let read_ppm_binary_int_green = function (s:string) ->
 ( read_ppm_binary_int_rgb s ).(1) ;;

(** {v read_ppm_binary_int_blue filename v} *)
let read_ppm_binary_int_blue = function (s:string) ->
 ( read_ppm_binary_int_rgb s ).(2) ;;


(** {v write_ppm_binary_int_rgb rgb_matrix filename v} *)
let write_ppm_binary_int_rgb = fun (m:int array array array) (fname:string) ->
 let oc = open_out fname
 and red = m.(0)
 and green = m.(1)
 and blue = m.(2) in
  let l = Array.length blue
  and c = Array.length blue.(0) in
   output_string oc ( "P6\n" ^ ( string_of_int c ) ^ " " ^ ( string_of_int l ) ^ "\n255\n" ) ;
   set_binary_mode_out oc true ;
   let cc = pred c in
    for i = 0 to pred l do
     let row_red = red.(i)
     and row_green = green.(i)
     and row_blue = blue.(i) in
      for j = 0 to cc do
       output_byte oc row_red.(j) ;
       output_byte oc row_green.(j) ;
       output_byte oc row_blue.(j) ;
      done ;
    done ;
    close_out_noerr oc ;;


(** {v read_ppm_binary_color filename v} *)
let read_ppm_binary_color = function (s:string) ->
 let e = read_pnm_header s
 and dot = String.make 1 ( char_of_int 10 ) in
  if e.(0).[1] != '6' then failwith "Not a ppm_binary file in Readwrite.read_ppm_binary_color." ;
  let l = Array.length e in
   let one = l - 2
   and two = l - 1
   and sharp = ( if l = 3 then "" else Array.fold_left ( fun x y -> x ^ dot ^ y ) "" ( Array.sub e 1 ( l - 3 ) ) ) in
    let size = e.(one)
    and depth = e.(two)
    and r = Str.regexp_string " " in
     let dim = Array.of_list ( Str.split r size )
     and atom = int_of_string depth in
      if atom != 255 then failwith "Only color files with depth 255 are taken in account in Readwrite.read_ppm_binary_color." ;
      let c = int_of_string dim.(0)
      and l = int_of_string dim.(1) in
       let chaine = e.(0) ^ dot ^ e.(one) ^ dot ^ e.(two) ^ dot ^ sharp
       and length = l * c in
        let offset = String.length chaine
        and taille = 3 * length in
         let v = array_read_binary_file ( offset + taille ) s in
          let vv = Array.sub v offset taille
          and vvv = Array.make length 0 in
           for i = 0 to pred length do
            let ii = 3 * i in
             vvv.(i) <- ( ( 256 * vv.(ii) ) + vv.( ii + 1 ) ) * 256 + vv.( ii + 2 )
           done ;
           Util.vector_to_matrix l c vvv ;;


(** {v write_ppm_binary_color matrix filename v} *)
let write_ppm_binary_color = fun (m:int array array) (fname:string) ->
 let oc = open_out fname
 and l = Array.length m
 and c = Array.length m.(0) in
  output_string oc ( "P6\n" ^ ( string_of_int c ) ^ " " ^ ( string_of_int l ) ^ "\n255\n" ) ;
  set_binary_mode_out oc true ;
  let cc = pred c in
   for i = 0 to pred l do
    let row = m.(i) in
     for j = 0 to cc do
      let x = row.(j) in
       let xx = x / 256 in
        output_byte oc ( xx / 256 ) ;
        output_byte oc ( xx mod 256 ) ;
        output_byte oc ( x mod 256 ) ;
      done ;
    done ;
    close_out_noerr oc ;;


(** {v read_ppm_binary_float_rgb filename v} *)
let read_ppm_binary_float_rgb = function (s:string) ->
 let e = read_pnm_header s
 and dot = String.make 1 ( char_of_int 10 ) in
  if e.(0).[1] != '6' then failwith "Not a ppm_binary file in Readwrite.read_ppm_binary_float_rgb." ;
  let l = Array.length e in
   let one = l - 2
   and two = l - 1
   and sharp = ( if l = 3 then "" else Array.fold_left ( fun x y -> x ^ dot ^ y ) "" ( Array.sub e 1 ( l - 3 ) ) ) in
    let size = e.(one)
    and depth = e.(two)
    and r = Str.regexp_string " " in
     let dim = Array.of_list ( Str.split r size )
     and atom = int_of_string depth in
      if atom != 255 then failwith "Only color files with depth 255 are taken in account in Readwrite.read_ppm_binary_float_rgb." ;
      let c = int_of_string dim.(0)
      and l = int_of_string dim.(1)
      and dot = String.make 1 ( char_of_int 10 ) in
       let chaine = e.(0) ^ dot ^ e.(one) ^ dot ^ e.(two) ^ dot ^ sharp in
        let offset = String.length chaine
        and taille = 3 * l * c in
         let v = array_read_binary_file ( offset + taille ) s in
          let vv = Array.map float ( Array.sub v offset taille ) in
           Array.map ( Util.vector_to_matrix l c ) ( Util.vector_spray 3 vv ) ;;

(** {v read_ppm_binary_float_red filename v} *)
let read_ppm_binary_float_red = function (s:string) ->
 ( read_ppm_binary_float_rgb s ).(0) ;;

(** {v read_ppm_binary_float_green filename v} *)
let read_ppm_binary_float_green = function (s:string) ->
 ( read_ppm_binary_float_rgb s ).(1) ;;

(** {v read_ppm_binary_float_blue filename v} *)
let read_ppm_binary_float_blue = function (s:string) ->
 ( read_ppm_binary_float_rgb s ).(2) ;;


(** {v write_ppm_binary_float_rgb rgb_matrix filename v} *)
let write_ppm_binary_float_rgb = fun (m:float array array array) (fname:string) ->
 let oc = open_out fname
 and red = m.(0)
 and green = m.(1)
 and blue = m.(2) in
  let l = Array.length blue
  and c = Array.length blue.(0) in
   output_string oc ( "P6\n" ^ ( string_of_int c ) ^ " " ^ ( string_of_int l ) ^ "\n255\n" ) ;
   set_binary_mode_out oc true ;
   let cc = pred c in
    for i = 0 to pred l do
     let row_red = red.(i)
     and row_green = green.(i)
     and row_blue = blue.(i) in
      for j = 0 to cc do
       output_byte oc (  min 255 ( Util.round row_red.(j) ) ) ;
       output_byte oc ( min 255 ( Util.round row_green.(j) ) ) ;
       output_byte oc ( min 255 ( Util.round row_blue.(j) ) ) ;
      done ;
    done ;
    close_out_noerr oc ;;


(** {v read_ppm_binary_under_sample_float_rgb edge filename v} *)
let read_ppm_binary_under_sample_float_rgb = fun (n:int) (fname:string) ->
 matrix_float_rgb_under_sample n ( read_ppm_binary_float_rgb fname ) ;;

(** {v read_ppm_binary_under_sample_float_red edge filename v} *)
let read_ppm_binary_under_sample_float_red = fun (n:int) (fname:string) ->
 matrix_float_unicolor_under_sample n ( read_ppm_binary_float_red fname ) ;;

(** {v read_ppm_binary_under_sample_float_green edge filename v} *)
let read_ppm_binary_under_sample_float_green = fun (n:int) (fname:string) ->
 matrix_float_unicolor_under_sample n ( read_ppm_binary_float_green fname ) ;;

(** {v read_ppm_binary_under_sample_float_blue edge filename v} *)
let read_ppm_binary_under_sample_float_blue = fun (n:int) (fname:string) ->
 matrix_float_unicolor_under_sample n ( read_ppm_binary_float_blue fname ) ;;


(** {v read_ppm_binary_under_sample_color edge filename v} *)
let read_ppm_binary_under_sample_color = fun (n:int) (fname:string) ->
 matrix_float_rgb_to_color ( read_ppm_binary_under_sample_float_rgb n fname ) ;;


(** {v read_ppm_binary_under_sample_int_rgb edge filename v} *)
let read_ppm_binary_under_sample_int_rgb = fun (n:int) (fname:string) ->
 matrix_float_rgb_to_int_rgb ( read_ppm_binary_under_sample_float_rgb n fname ) ;;

(** {v read_ppm_binary_under_sample_int_red edge filename v} *)
let read_ppm_binary_under_sample_int_red = fun (n:int) (fname:string) ->
 Matrix.matrix_int_clip 255 ( Matrix.matrix_float_round ( read_ppm_binary_under_sample_float_red n fname ) ) ;;

(** {v read_ppm_binary_under_sample_int_green edge filename v} *)
let read_ppm_binary_under_sample_int_green = fun (n:int) (fname:string) ->
 Matrix.matrix_int_clip 255 ( Matrix.matrix_float_round ( read_ppm_binary_under_sample_float_green n fname ) ) ;;

(** {v read_ppm_binary_under_sample_int_blue edge filename v} *)
let read_ppm_binary_under_sample_int_blue = fun (n:int) (fname:string) ->
 Matrix.matrix_int_clip 255 ( Matrix.matrix_float_round ( read_ppm_binary_under_sample_float_blue n fname ) ) ;;




(** {C § } *)
(** 
{1 Fichiers d'images bitmap}
{1 Bitmap picture files}
*)
(** {C  } *)




(** A description of the bmp format is available in the numbers 62 and 100 of the magazine LOGIN: pages 64-67.

Une description du format bmp est disponible dans les numéros 62 et 100 du magazine LOGIN: pages 64-67. *)


(** {v f32 offset array v} *)
let f32 = fun (i:int) (x:int array) ->
 string_of_int ( x.(i) + 256 * ( x.( i + 1 ) + 256 * ( x.( i + 2 ) + 256 * x.( i + 3 ) ) ) )

(** {v f16 offset array v} *)
let f16 = fun (i:int) (x:int array) ->
 string_of_int ( x.(i) + 256 * ( x.( i + 1 ) ) )

(** {v g16 integer v} *)
let g16 = function (x:int) ->
 [| x mod 256 ; x / 256 |]

(** {v g32 integer v} *)
let g32 = function (x:int) ->
 let xx = x mod 256 in
 let y = x / 256 in
  let yy = y mod 256
  and z = y / 256 in
   let zz = z mod 256 in
 [| xx ; yy ; zz ; x / 16777216 |]

(** {v read_bmp_header filename v} *)
let read_bmp_header = function (fname:string) ->
 let ic = open_in fname
 and a = ref [] in
  begin
   try
    begin
     for i = 0 to 53 do
      a := ( input_byte ic ) :: !a
     done ;
     close_in_noerr ic ;
    end
   with _ ->
    close_in_noerr ic
  end ;
  let b = Array.of_list ( List.rev !a ) in
   if ( char_of_int b.(0) <> 'B' ) || ( char_of_int b.(1) <> 'M' ) then
    failwith "Bad magic number in Readwrite.read_bmp_header." ;
   let c = Array.make 16 "BM" in
    c.(1) <- f32 2 b ;
    c.(2) <- f16 6 b ;
    c.(3) <- f16 8 b ;
    c.(4) <- f32 10 b ;
    c.(5) <- f32 14 b ;
    c.(6) <- f32 18 b ;
    c.(7) <- f32 22 b ;
    c.(8) <- f16 26 b ;
    c.(9) <- f16 28 b ;
    c.(10) <- f32 30 b ;
    c.(11) <- f32 34 b ;
    c.(12) <- f32 38 b ;
    c.(13) <- f32 42 b ;
    c.(14) <- f32 46 b ;
    c.(15) <- f32 50 b ;
    c ;;


(** {v read_raw_bmp_int_rgb filename v} *)
let read_raw_bmp_int_rgb = function (fname:string) ->
 try
  begin
   let h = read_bmp_header fname
   and offset = 54 in
    if int_of_string h.(10) <> 0 then
     failwith "Compressed format in Readwrite.read_raw_bmp_int_rgb." ;
    if int_of_string h.(9) <> 24 then
     failwith "Number of bits per pixel not programmed in Readwrite.read_raw_bmp_int_rgb." ;
    let width = int_of_string h.(6)
    and height = int_of_string h.(7) in
     let ww = pred width
     and hh = pred height
     and tw = 3 * width
     and r = Array.make_matrix height width 0
     and g = Array.make_matrix height width 0
     and b = Array.make_matrix height width 0
     and a = ref ( array_read_binary_file ( width * height + offset ) fname ) in
      a := Util.array_end offset !a ;
      for i = 0 to hh do
       let ii = ( hh - i ) * tw
       and red_output = r.(i)
       and green_output = g.(i)
       and blue_output = b.(i) in
        for j = 0 to ww do
         let jj = 3 * j in
          blue_output.(j) <- !a.( ii + jj ) ;
          green_output.(j) <- !a.( ii + jj + 1 ) ;
          red_output.(j) <- !a.( ii + jj + 2 ) ;
        done
      done ;
      [| r ; g ; b |]
  end
 with _ ->
  failwith "Error in Readwrite.read_raw_bmp_int_rgb." ;;

(** {v read_raw_bmp_red filename v} *)
let read_raw_bmp_red = function (fname:string) ->
 try
  ( read_raw_bmp_int_rgb fname ).(0)
 with _ ->
  failwith "Error in Readwrite.read_raw_bmp_red." ;;

(** {v read_raw_bmp_green filename v} *)
let read_raw_bmp_green = function (fname:string) ->
 try
  ( read_raw_bmp_int_rgb fname ).(1)
 with _ ->
  failwith "Error in Readwrite.read_raw_bmp_red." ;;

(** {v read_raw_bmp_blue filename v} *)
let read_raw_bmp_blue = function (fname:string) ->
 try
  ( read_raw_bmp_int_rgb fname ).(2)
 with _ ->
  failwith "Error in Readwrite.read_raw_bmp_blue." ;;

(** {v read_raw_bmp_color filename v} *)
let read_raw_bmp_color = function (fname:string) ->
 try
  matrix_int_rgb_to_color ( read_raw_bmp_int_rgb fname )
 with _ ->
  failwith "Error in Readwrite.read_raw_bmp_color." ;;


(** {v read_palette_bmp_int_rgb filename v} *)
let read_palette_bmp_int_rgb = function (fname:string) ->
 try
  begin
   let h = read_bmp_header fname
   and decalage = 54 in
    if int_of_string h.(10) <> 0 then
     failwith "Compressed format in Readwrite.read_palette_bmp_int_rgb." ;
    if int_of_string h.(9) <> 8 then
     failwith "Number of bits per pixel not programmed in Readwrite.read_palette_bmp_int_rgb." ;
    let size = int_of_string h.(11)
    and file_size = int_of_string h.(1)
    and height = int_of_string h.(7) in
(** Errors in the declaration of the number of columns are possible.

Des erreurs sur la déclaration du nombre de colonnes sont possibles. *)
     let width = max ( int_of_string h.(6) ) ( size / height )
(** The offset after the header is not always equal to 1024=256*4.

Le décalage après l'entête ne vaut pas toujours 1024=256*4. *)
     and offset = ( min ( int_of_string h.(4) ) ( file_size - size ) ) - 54
     and a = ref ( array_read_binary_file ( file_size ) fname ) in
      a := Util.array_end decalage !a ;
      let ww = pred width
      and hh = pred height
(** The format of the palette is bb gg rr 00 with four bytes per color.

Le format de la palette est bb gg rr 00 avec quatre octets par couleur. *)
      and palette =  Array.sub !a 0 offset
      and r = Array.make_matrix height width 0
      and g = Array.make_matrix height width 0
      and b = Array.make_matrix height width 0 in
       a := Util.array_end offset !a ;
       for i = 0 to hh do
        let red_output = r.(i)
        and green_output = g.(i)
        and blue_output = b.(i)
        and ii = ( hh - i ) * width in
         for j = 0 to ww do
          let color = 4 * !a.( ii + j ) in
           let code = Array.sub palette color 4 in
            blue_output.(j) <- code.(0) ;
            green_output.(j) <- code.(1) ;
            red_output.(j) <- code.(2) ;
         done
       done ;
       [| r ; g ; b |]
  end
 with _ ->
  failwith "Error in Readwrite.read_palette_bmp_int_rgb." ;;

(** {v read_palette_bmp_red filename v} *)
let read_palette_bmp_red = function (fname:string) ->
 try
  ( read_palette_bmp_int_rgb fname ).(0)
 with _ ->
  failwith "Error in Readwrite.read_palette_bmp_red." ;;

(** {v read_palette_bmp_green filename v} *)
let read_palette_bmp_green = function (fname:string) ->
 try
  ( read_palette_bmp_int_rgb fname ).(1)
 with _ ->
  failwith "Error in Readwrite.read_palette_bmp_red." ;;

(** {v read_palette_bmp_blue filename v} *)
let read_palette_bmp_blue = function (fname:string) ->
 try
  ( read_palette_bmp_int_rgb fname ).(2)
 with _ ->
  failwith "Error in Readwrite.read_palette_bmp_blue." ;;

(** {v read_palette_bmp_color filename v} *)
let read_palette_bmp_color = function (fname:string) ->
 try
  matrix_int_rgb_to_color ( read_palette_bmp_int_rgb fname )
 with _ ->
  failwith "Error in Readwrite.read_palette_bmp_color." ;;


(** {v read_bmp_int_rgb filename v} *)
let read_bmp_int_rgb = function (fname:string) ->
 try
  begin
   try
    read_raw_bmp_int_rgb fname
   with _ ->
    read_palette_bmp_int_rgb fname
  end
 with _ ->
  failwith "Fomat not programmed in Readwrite.read_bmp_int_rgb." ;;

(** {v read_bmp_red filename v} *)
let read_bmp_red = function (fname:string) ->
 try
  ( read_bmp_int_rgb fname ).(0)
 with _ ->
  failwith "Fomat not programmed in Readwrite.read_bmp_red." ;;

(** {v read_bmp_green filename v} *)
let read_bmp_green = function (fname:string) ->
 try
  ( read_bmp_int_rgb fname ).(1)
 with _ ->
  failwith "Fomat not programmed in Readwrite.read_bmp_green." ;;

(** {v read_bmp_blue filename v} *)
let read_bmp_blue = function (fname:string) ->
 try
  ( read_bmp_int_rgb fname ).(2)
 with _ ->
  failwith "Fomat not programmed in Readwrite.read_bmp_blue." ;;

(** {v read_bmp_color filename v} *)
let read_bmp_color = function (fname:string) ->
 try
  matrix_int_rgb_to_color ( read_bmp_int_rgb fname )
 with _ ->
  failwith "Fomat not programmed in Readwrite.read_bmp_color." ;;


(** {v bmp_header_of_rgb rgb_matrix v} *)
let bmp_header_of_rgb = function (m:int array array array) ->
 let h = Array.length m.(0)
 and w = Array.length m.(0).(0) in
  let size = 3 * h * w in
   let file_size = 54 + size in
    Array.concat [ [| int_of_char 'B' ; int_of_char 'M' ; |] ; g32 file_size ; g16 0 ; g16 0 ; g32 54 ; g32 40 ; g32 w ; g32 h ; g16 1 ; g16 24 ; g32 0 ; g32 size ; g32 3780 ; g32 3780 ; g32 0 ; g32 0 ] ;;

(** {v write_bmp_int_rgb rgb_matrix v} *)
let write_bmp_int_rgb = fun (m:int array array array) (fname:string) ->
 let oc = open_out fname
 and a = bmp_header_of_rgb m
 and red = m.(0)
 and green = m.(1)
 and blue = m.(2) in
  set_binary_mode_out oc true ;
  for i = 0 to pred ( Array.length a ) do
   output_byte oc a.(i)
  done ;
  let hh = pred ( Array.length red )
   and ww = pred ( Array.length red.(0) ) in
    for i = hh downto 0 do
     let row_red = red.(i)
     and row_green = green.(i)
     and row_blue = blue.(i) in
      for j = 0 to ww do
       output_byte oc row_blue.(j) ;
       output_byte oc row_green.(j) ;
       output_byte oc row_red.(j) ;
      done ;
    done ;
    close_out_noerr oc ;;

(** {v write_bmp_color rgb_matrix v} *)
let write_bmp_color = fun (m:int array array) (fname:string) ->
 let mm = matrix_color_to_int_rgb m in
  write_bmp_int_rgb mm fname ;;

(** {v write_bmp_float_rgb rgb_matrix v} *)
let write_bmp_float_rgb = fun (m:float array array array) (fname:string) ->
 let mm = matrix_float_rgb_to_int_rgb m in
  write_bmp_int_rgb mm fname ;;




(** {C § } *)
(** 
{1 Fichiers sons au}
{1 Au sound files}
*)
(** {C  } *)




(** {v read_header_au v} This function is only for 64 bits platforms. The output has the following form:

[[| magic_type ; offset ; size ; encoding ; sample_rate ; channels |]]

La sortie a la forme précédente.
Cette fonction est réservée aux plateformes 64 bits. *)
let read_header_au = function (fname:string) ->
 let ic = open_in_bin fname
 and v = Array.make 6 0
 and type_snd = Int32.to_int ( Int32.of_string "0x2e736e64" ) in
  begin
   try
(** magic_type *)
    v.(0) <- input_binary_int ic ;
    if compare type_snd v.(0) != 0 then failwith "Not a valid file in Readwrite.read_header_au." ;
(** offset *)
    v.(1) <- input_binary_int ic ;
(** size *)
    v.(2) <- input_binary_int ic ;
(** encoding *)
    v.(3) <- input_binary_int ic ;
(** sample_rate *)
    v.(4) <- input_binary_int ic ;
(** channels *)
    v.(5) <- input_binary_int ic ;
    close_in_noerr ic ;
   with End_of_file -> ( close_in_noerr ic ; failwith "Not a valid header in Readwrite.read_header_au." )
  end ;
  v ;;


(** {v read_float32_au v} This function is only for 64 bits platforms.

Cette fonction est réservée aux plateformes 64 bits. *)
let read_float32_au = function (fname:string) ->
 let h = read_header_au fname
 and ic = open_in_bin fname in
  if compare h.(3) 6 != 0 then failwith "Bad encoding in Readwrite.read_float32_au." ;
  begin
   try seek_in ic h.(1) with End_of_file -> failwith "Bad offset in Readwrite.read_float32_au."
  end ;
  let l = ref []
  and channels = h.(5)
  and count = ref 0 in
  begin
   try
    while true do
     l := ( input_binary_int ic ) :: !l ;
     count := succ !count ;
    done
   with End_of_file -> close_in_noerr ic
  end ;
  let samples = !count / channels in
   let m = Array.make_matrix channels samples 0.
   and c = pred channels in
    for i = pred samples downto 0 do
     for j = c downto 0 do
      m.(j).(i) <- Int32.float_of_bits ( Int32.of_int ( List.hd !l ) ) ;
      l := List.tl !l
     done ;
    done ;
    m ;;


(** {v write_vector_float_to_float32_au v} This function is only for 64 bits platforms.

Cette fonction est réservée aux plateformes 64 bits. *)
let write_vector_float_to_float32_au = fun (sample_rate:int) (v:float array) (fname:string) ->
 let oc = open_out fname
 and offset = 24
 and size = - 1
 and encoding = 6
 and channels = 1
 and l = pred ( Array.length v ) in
  output_string oc ".snd" ;
  set_binary_mode_out oc true ;
  output_binary_int oc offset ;
  output_binary_int oc size ;
  output_binary_int oc encoding ;
  output_binary_int oc sample_rate ;
  output_binary_int oc channels ;
  for i = 0 to l do
   let x = Int32.to_int ( Int32.bits_of_float v.(i) ) in
    output_binary_int oc x ;
  done ;
  close_out_noerr oc ;;


(** {v write_matrix_float_to_float32_au v} This function is only for 64 bits platforms.

Cette fonction est réservée aux plateformes 64 bits. *)
let write_matrix_float_to_float32_au = fun (sample_rate:int) (m:float array array) (fname:string) ->
 let oc = open_out fname
 and offset = 24
 and size = - 1
 and encoding = 6
 and channels = Array.length m
 and samples = Array.length m.(0) in
  output_string oc ".snd" ;
  set_binary_mode_out oc true ;
  output_binary_int oc offset ;
  output_binary_int oc size ;
  output_binary_int oc encoding ;
  output_binary_int oc sample_rate ;
  output_binary_int oc channels ;
   let c = pred channels in
    for i = 0 to pred samples do
     for j = 0 to c do
      let x = Int32.to_int ( Int32.bits_of_float m.(j).(i) ) in
       output_binary_int oc x ;
     done ;
    done ;
  close_out_noerr oc ;;


(** {v read_vector_float64_au v} *)
let read_vector_float64_au = function (fname:string) ->
 let h = read_header_au fname
 and tmp1 = "tmp1" ^ fname
 and tmp2 = "tmp2" ^ fname
 and ic = open_in_bin fname in
  if compare h.(3) 7 != 0 then failwith "Bad encoding in Readwrite.read_vector_float64_au." ;
  begin
   try seek_in ic h.(1) with End_of_file -> failwith "Bad offset in Readwrite.read_vector_float64_au."
  end ;
  if h.(5) != 1 then failwith "Several channels in Readwrite.read_vector_float64_au." ;
  let fake = ref 0
  and count = ref 0 in
  begin
   try
    while true do
     fake := input_binary_int ic ;
     count := succ !count ;
    done
   with End_of_file -> close_in_noerr ic
  end ;
  let samples = !count / 2 in
   let v = Array.make samples 0. in
    write_float_array_value v tmp1 ;
    ignore ( Sys.command ( "head -c 22 " ^ tmp1 ^ " > " ^ tmp2 ) ) ;
    ignore ( Sys.command ( "tail -c+" ^ ( string_of_int ( succ h.(1) ) ) ^ " " ^ fname ^ " >> " ^ tmp2 ) ) ;
     let w = read_float_array_value tmp2 in
      Sys.remove tmp1 ;
      Sys.remove tmp2 ;
      w ;;


(** {v write_vector_float_to_float64_au v}  *)
let write_vector_float_to_float64_au = fun (sample_rate:int) (v:float array) (fname:string) ->
 let oc = open_out fname
 and offset = 24
 and size = - 1
 and encoding = 7
 and channels = 1
 and tmp = "tmp" ^ fname in
  write_float_array_value v tmp ;
  output_string oc ".snd" ;
  set_binary_mode_out oc true ;
  output_binary_int oc offset ;
  output_binary_int oc size ;
  output_binary_int oc encoding ;
  output_binary_int oc sample_rate ;
  output_binary_int oc channels ;
  close_out_noerr oc ;
  ignore ( Sys.command ( "tail -c+23 " ^ tmp ^ " >> " ^ fname ) ) ;
  Sys.remove tmp ;;


(** {v read_matrix_float64_au v} *)
let read_matrix_float64_au = function (fname:string) ->
 let h = read_header_au fname
 and tmp1 = "tmp1" ^ fname
 and tmp2 = "tmp2" ^ fname
 and ic = open_in_bin fname in
  if compare h.(3) 7 != 0 then failwith "Bad encoding in Readwrite.read_matrix_float64_au." ;
  begin
   try seek_in ic h.(1) with End_of_file -> failwith "Bad offset in Readwrite.read_matrix_float64_au."
  end ;
  let fake = ref 0
  and channels = h.(5)
  and count = ref 0 in
  begin
   try
    while true do
     fake := input_binary_int ic ;
     count := succ !count ;
    done
   with End_of_file -> close_in_noerr ic
  end ;
  let samples = !count / 2 in
   let v = Array.make samples 0. in
    write_float_array_value v tmp1 ;
    ignore ( Sys.command ( "head -c 22 " ^ tmp1 ^ " > " ^ tmp2 ) ) ;
    ignore ( Sys.command ( "tail -c+" ^ ( string_of_int ( succ h.(1) ) ) ^ " " ^ fname ^ " >> " ^ tmp2 ) ) ;
     let w = read_float_array_value tmp2 in
      Sys.remove tmp1 ;
      Sys.remove tmp2 ;
      Util.vector_spray channels w ;;


(** {v write_matrix_float_to_float64_au v}  *)
let write_matrix_float_to_float64_au = fun (sample_rate:int) (m:float array array) (fname:string) ->
 let oc = open_out fname
 and offset = 24
 and size = - 1
 and encoding = 7
 and channels = Array.length m
 and v = Util.vector_interlace m
 and tmp = "tmp" ^ fname in
  write_float_array_value v tmp ;
  output_string oc ".snd" ;
  set_binary_mode_out oc true ;
  output_binary_int oc offset ;
  output_binary_int oc size ;
  output_binary_int oc encoding ;
  output_binary_int oc sample_rate ;
  output_binary_int oc channels ;
  close_out_noerr oc ;
  ignore ( Sys.command ( "tail -c+23 " ^ tmp ^ " >> " ^ fname ) ) ;
  Sys.remove tmp ;;




(** {C § } *)
(** 
{1 Fichiers sons wav}
{1 Wav sound files}
*)
(** {C  } *)




(** The wav format is described in the following site.

http://www-mmsp.ece.mcgill.ca/Documents/AudioFormats/WAVE/

Le format wav est décrit dans le site ci-dessus. *)


(** {v read_pcm_wav_header filename v} *)
let read_pcm_wav_header = function (fname:string) ->
 let ic = open_in fname
 and c = ref 'a'
 and n = ref 0
 and m = ref 1
 and s = ref ""
 and a = ref [] in
  let f = function () ->
   begin
    c := input_char ic ;
    s := String.make 1 !c ;
    for i = 1 to 3 do
     c := input_char ic ;
     s := !s ^ ( String.make 1 !c )
    done ;
    a := !s :: !a ;
   end
  and g = function (length:int) ->
   begin
    n := input_byte ic ;
    m := 1 ;
    for i = 2 to length do
     m := 256 * !m ;
     n := !n + !m * ( input_byte ic ) ;
    done ;
    a := ( string_of_int !n ) :: !a ;
   end in
    begin
     try
      begin
       f () ;
       g 4 ;
       f () ;
       f () ;
       g 4 ;
       g 2 ;
       g 2 ;
       g 4 ;
       g 4 ;
       g 2 ;
       g 2 ;
       f () ;
       g 4 ;
      end
     with _ ->
      close_in_noerr ic
    end ;
    Array.of_list ( List.rev !a ) ;;

(** {v read_pcm_wav filename v} *)
let read_pcm_wav = function (fname:string) ->
 let h = read_pcm_wav_header fname
 and factor = 256
 and ic = open_in fname in
  let number_of_samples = ( int_of_string h.(12) ) / ( int_of_string h.(9) )
  and number_of_bits_per_sample = int_of_string h.(10)
  and number_of_channels = int_of_string h.(6) in
   let signal = Array.make_matrix number_of_samples number_of_channels 0
   and number_of_bytes_per_sample = ( if number_of_bits_per_sample mod 8 <> 0 then failwith "Number of bits per sample not programmed in Readwrite.read_pcm_wav." ; number_of_bits_per_sample / 8 )
   and shift = int_of_float ( Util.int_float_power ( pred number_of_bits_per_sample ) 2. )
   and extract_value = fun (s:int) (radix:int) (bytes_per_sample:int) ->
    begin
     let output = ref 0 in
      begin
       match bytes_per_sample with
       | 1 ->
        begin
         output := input_byte ic ;
         output := !output - s ;
        end
       | 2 ->
        begin
         output := input_byte ic ;
         output := !output + factor * ( input_byte ic ) ;
         output := ( ( !output + s ) mod radix ) - s ;
        end
       | _ ->
        begin
         assert ( bytes_per_sample > 0 ) ;
         output := input_byte ic ;
         let m = ref 1 in
          for i = 1 to bytes_per_sample do
           m := factor * !m ;
           output := !output + !m * ( input_byte ic ) ;
          done ;
          output := ( ( !output + s ) mod radix ) - s ;
        end
      end ;
      !output
    end
   and cc = pred number_of_channels in
    let radix = 2 * shift in
     begin
      try
       begin
        for i = 1 to 44 do
         ignore ( input_byte ic )
        done ;
        set_binary_mode_in ic true ;
        for i = 0 to pred number_of_samples do
         let multi_sample = signal.(i) in
          for j = 0 to cc do
           multi_sample.(j) <- extract_value shift radix number_of_bytes_per_sample ;
          done
        done ;
        close_in_noerr ic
       end
      with _ -> 
       close_in_noerr ic
     end ;
     let coeff = 1. /. ( float shift ) in
      Matrix.float_transpose ( Matrix.matrix_float_scal_mult coeff ( Matrix.float_of_matrix signal ) ) ;;

(** {v write_pcm_wav sample_rate bytes_per_sample matrix filename v} *)
let write_pcm_wav = fun (sample_rate:int) (bytes_per_sample:int) (m:float array array) (fname:string) ->
 let oc = open_out fname
 and factor = 256
 and shift = ref 128
 and exponent = ref 1
 and number_of_channels = Array.length m
 and number_of_samples = Array.length m.(0)
 and bits_per_sample = 8 * bytes_per_sample in
  while !exponent < bytes_per_sample do
   shift := !shift * factor ;
   incr exponent ;
  done ;
  let data_rate = sample_rate * bytes_per_sample * number_of_channels
  and float_factor = float !shift
  and cc = pred number_of_channels
  and block_align = number_of_channels * bytes_per_sample in
   let data_size = number_of_samples * block_align
   and inject_value = fun (s:int) (bps:int) (value:int) ->
    begin
     let factor = 256 in
      match bps with
      | 1 ->
       begin
        let x = s + value in
         output_byte oc x ;
       end
      | 2 ->
       begin
        let x = value + s in
         let y = x mod factor in
          output_byte oc y ;
          output_byte oc ( x / factor )
       end
      | _ ->
       begin
        let x = ref ( value + s ) in
         let y = ref ( !x mod factor ) in
          output_byte oc !y ;
          for i = 2 to bps do
           x := !x / factor ;
           y := !x mod factor ;
           output_byte oc !y
          done ;
       end
    end in
     let dsm1 = data_size mod 1 in
      output_char oc 'R' ;
      output_char oc 'I' ;
      output_char oc 'F' ;
      output_char oc 'F' ;
      inject_value 0 4 ( 36 + data_size + dsm1 ) ;
      output_char oc 'W' ;
      output_char oc 'A' ;
      output_char oc 'V' ;
      output_char oc 'E' ;
      output_char oc 'f' ;
      output_char oc 'm' ;
      output_char oc 't' ;
      output_char oc ' ' ;
      inject_value 0 4 16 ;
      inject_value 0 2 1 ;
      inject_value 0 2 number_of_channels ;
      inject_value 0 4 sample_rate ;
      inject_value 0 4 data_rate ;
      inject_value 0 2 block_align ;
      inject_value 0 2 bits_per_sample ;
      output_char oc 'd' ;
      output_char oc 'a' ;
      output_char oc 't' ;
      output_char oc 'a' ;
      inject_value 0 4 data_size ;
      for i = 0 to pred number_of_samples do
       for j = 0 to cc do
        inject_value !shift bytes_per_sample ( int_of_float ( ( m.(j).(i) +. 1. ) *. float_factor ) ) ;
       done ;
      done ;
      if dsm1 <> 0 then
       inject_value 0 1 0 ;
      close_out_noerr oc ;;




(** {C § } *)
(** 
{1 Fichiers et répertoires}
{1 Files and directories}
*)
(** {C  } *)




(** {v sub_directories directory_name v} *)
let sub_directories = function (dname:string) ->
 let l = Sys.readdir dname
 and g = function x ->
  begin
   try
    Sys.is_directory ( Filename.concat dname x )
   with
    Sys_error unknown -> false
  end in
  let f = function x -> ( x , ( x.[0] != '.' ) && ( g x ) )
  and cmp = fun y z -> compare ( snd y ) ( snd z ) in
   let ll = Array.map f l in
    Array.sort cmp ll ;
    let l_l = Array.map snd ll in
     let first_true = Util.vector_find_first ( = ) true l_l in
      if first_true >= 0 then
       begin
        let lll = Array.sub ll first_true ( ( Array.length l ) - first_true ) in
         let result = Array.map fst lll in
          Array.sort compare result ;
          result
       end
      else
       [| |] ;;

(** {v sub_directories_with_parent directory_name v} *)
let sub_directories_with_parent = function (dname:string) ->
 Array.append [| ".." |] ( sub_directories dname ) ;;

(** {v sub_directories_with_current directory_name v} *)
let sub_directories_with_current = function (dname:string) ->
 Array.append [| "." |] ( sub_directories dname ) ;;

(** {v sub_directories_with_parent_and_current directory_name v} *)
let sub_directories_with_parent_and_current = function (dname:string) ->
 Array.append [| ".." ; "." |] ( sub_directories dname ) ;;


(** {v regular_files directory_name v} *)
let regular_files = function (dname:string) ->
 let l = Sys.readdir dname
 and g = function x ->
  begin
   try
    Sys.is_directory ( Filename.concat dname x )
   with
    Sys_error unknown -> false
  end in
  let f = function x -> ( x , ( x.[0] != '.' ) && ( g x ) )
  and cmp = fun y z -> compare ( snd y ) ( snd z ) in
   let ll = Array.map f l in
    Array.sort cmp ll ;
    let l_l = Array.map snd ll in
     let first_true = Util.vector_find_first ( = ) true l_l in
      if first_true > 0 then
       begin
        let lll = Array.sub ll 0 first_true in
         let result = Array.map fst lll in
          Array.sort compare result ;
          result
       end
      else
       begin
        if first_true = 0 then [| |]
        else
         begin
         let lll = Array.map fst ll in
          Array.sort compare lll ;
          lll
         end
       end ;;




(** {C § } *)
(** 
{1 Images réelles}
{1 Floating point pictures}
*)
(** {C  } *)




(** {v read_float_rgb red_name green_name blue_name v} *)
let read_float_rgb = fun (rname:string) (gname:string) (bname:string) ->
 let r = read_matrix_float64_au rname
 and g = read_matrix_float64_au gname
 and b = read_matrix_float64_au bname in
  [| r ; g ; b |] ;;


(** {v write_float_rgb rgb_matrix file_prefix v} *)
let write_float_rgb = fun (m:float array array array) (fname:string) ->
 assert ( Array.length m >= 3 ) ;
 let rname = fname ^ ".red.au"
 and gname = fname ^ ".green.au"
 and bname = fname ^ ".blue.au" in
  write_matrix_float_to_float64_au 0 m.(0) rname ;
  write_matrix_float_to_float64_au 1 m.(1) gname ;
  write_matrix_float_to_float64_au 2 m.(2) bname ;;




(** {v read_picture_float_rgb filename v} The accepted formats are: xpm, pgm binary, ppm binary.

Les formats acceptés sont : xpm, pgm binaire, ppm binaire.*)
let read_picture_float_rgb = function (s:string) ->
 let e = read_pnm_header s in
  match e.(0).[1] with
  | '6' -> read_ppm_binary_float_rgb s
  | '5' -> read_pgm_binary_float_rgb s
  | '4' -> read_pbm_binary_float_rgb s
  | _ -> xpm_float_rgb ( array_read_text_file s ) ;;




(** {C § } *)
(** 
{1 Environnement}
*)
(** {C  } *)




(** {v clear string v} *)
let clear = function (s:string) ->
 try
  ignore ( Sys.command ( "rm -f " ^ s ) )
 with _ ->
  () ;;

(** {v crew_bsd unit v} *)
let crew_bsd = function () ->
 array_write_text_file [| "#!/bin/sh" ; "sysctl -n kern.smp.cpus > tmp_cpu" ;
  "sysctl -n kern.smp.active >> tmp_cpu" ; "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let numbers = array_read_reverse_text_file "tmp_cpu" in
  clear "tmp_script.sh tmp_cpu" ;
  if int_of_string numbers.(0) == 0 then
   1
  else
   int_of_string numbers.(1) ;;

(** {v crew_linux_lscpu unit v} *)
let crew_linux_lscpu = function () ->
 array_write_text_file [| "#!/bin/sh" ; "lscpu -p | grep -v '#' | wc -l | tr -d ' ' > tmp_cpu" ; "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let number = List.hd ( list_read_reverse_text_file "tmp_cpu" ) in
  clear "tmp_script.sh tmp_cpu" ;
  int_of_string number ;;

(** {v crew_linux_sysctl unit v} *)
let crew_linux_sysctl = function () ->
 array_write_text_file [| "#!/bin/sh" ; "sysctl -a 2>1 | grep -v error | grep -o '[.]cpu[.0-9][.0-9]*' | grep -o '[0-9][0-9]*' | sort -u | wc -l | tr -d ' ' > tmp_cpu" ; "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let number = List.hd ( list_read_reverse_text_file "tmp_cpu" ) in
  clear "tmp_script.sh tmp_cpu" ;
  int_of_string number ;;

(** {v crew_unix unit v} *)
let crew_unix = function () ->
 array_write_text_file [| "#!/bin/sh" ; "dmesg | grep -oi 'CPU[0-9][0-9]*' | sort -u | wc -l | tr -d ' ' > tmp_cpu" ;
 "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let number = List.hd ( list_read_reverse_text_file "tmp_cpu" ) in
  clear "tmp_script.sh tmp_cpu" ;
  int_of_string number ;;


(** {v crew unit v} Number of active processors.

Nombre de processeurs en fonctionnement. *)
let crew = function () ->
 try
  begin
   let output = crew_bsd () in
    assert ( output > 0 ) ;
    output
  end
 with _ ->
  begin
   try
    begin
     let output = crew_linux_lscpu () in
      assert ( output > 0 ) ;
      output
    end
   with _ ->
    begin
     try
      begin
       let output = crew_linux_sysctl () in
        assert ( output > 0 ) ;
        output
      end
     with _ ->
      crew_unix ()
    end
  end ;;


(** {v cpu_freq unit v} Frequency in megahertz of the first processor.

Fréquence du premier processeur en mégahertz. *)
let cpu_freq = function () ->
 array_write_text_file [| "#!/bin/sh" ; "sysctl -n dev.cpu.0.freq | tr -d ' ' > tmp_freq" ; "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let freq = List.hd ( list_read_reverse_text_file "tmp_freq" ) in
  clear "tmp_script.sh tmp_freq" ;
  int_of_string freq ;;


(** {v free_ram unit v} Number of free bytes in Random access memory.

Nombre d'octets libres en mémoire vive. *)
let free_ram = function () ->
 array_write_text_file [| "#!/bin/sh" ; "vmstat -s | grep 'pages free' | grep -o '[0-9][0-9]*' > tmp_free_ram" ;
  "vmstat -s | grep 'bytes per page' | grep -o '[0-9][0-9]*' >> tmp_free_ram" ; "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let free = array_read_reverse_text_file "tmp_free_ram" in
  clear "tmp_script.sh tmp_free_ram" ;
  ( int_of_string free.(0) ) * ( int_of_string free.(1) ) ;;


(** {v free_disk unit v} Number of free kilobytes on the file system containing the current directory.

Nombre de kilooctets libres sur le système de fichiers contenant le répertoire courant. *)
let free_disk = function () ->
 array_write_text_file [| "#!/bin/sh" ; "df -k . | grep '/' | tr -s ' ' | cut -d ' ' -f 4 > tmp_free_disk" ; "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let free = List.hd ( list_read_reverse_text_file "tmp_free_disk" ) in
  clear "tmp_script.sh tmp_free_disk" ;
  int_of_string free ;;

(** {v line unit v} *)
let line = function () ->
 array_write_text_file [| "#!/bin/sh" ; "echo `apm -a` > tmp_line" ; "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let line = List.hd ( list_read_reverse_text_file "tmp_line" ) in
  clear "tmp_script.sh tmp_line" ;
  if int_of_string line == 0 then
   false
  else
   true ;;

(** {v battery unit v} *)
let battery = function () ->
 array_write_text_file [| "#!/bin/sh" ; "apm | grep -v 'APM' > tmp_batt" ;
  "acpiconf -i batt >> tmp_batt" ; "exit 0" |] "tmp_script.sh" ;
 ignore ( Sys.command "sh tmp_script.sh" ) ;
 let batt = array_read_text_file "tmp_batt" in
  clear "tmp_script.sh tmp_batt" ;
  Array.map ( Str.global_replace ( Str.regexp ( "\t" ) ) " " ) batt ;;









(** {C § § § } *)




end ;;




