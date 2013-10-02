



module Spec = struct




(** {C § } *)
(** 
{1 Introduction}
*)
(** {C  } *)




(**
The mathematician will find in this module functions in order to:

- find to which module(s) belongs a function,

- extract the basis specification from the html documentation,

- extract the source code (extended specification) from the html documentation,

- find quotations in the html documentation.


{2 Conventions}


The functions [what*], [how*] and [why*] browse the html documentation. 
Some of these functions must receive as arguments character strings in order to build paths toward adequate directories or files.

The functions [which*] proceed by try-and-fail compilation: they do not need any presence of html documentation.


{2 Comments}


In order to avoid early coincidences, it may be useful to put a space at the end of the name of a function.

The extractions from the documentation are done with [lynx] which must be installed on the machine.


This module is distributed under the same licence as Ocaml.


{C § }


La mathématicienne ou le mathématicien trouvera dans ce module des fonctions pour :

- trouver à quel(s) module(s) appartient une fonction,

- extraire la spécification de base à partir de la documentation html,

- extraire le code source (spécification étendue) à partir de la documentation html,

- trouver des citations dans la documentation html.


{2 Conventions}


Les fonctions [what*], [how*] et [why*] explorent la documentation html. 
Il faut fournir en arguments à certaines de ces fonctions des chaînes de caractères pour constituer un chemin vers les répertoires ou fichiers adéquats.

Les fonctions [which*] procèdent par essai-échec de compilation : elles ne nécessitent aucune présence de documentation html.


{2 Commentaires}


Pour éviter des coïncidences précoces, il est parfois utile de mettre un espace à la fin d'un nom de fonction.

Les extractions à partir de la documentation se font avec [lynx] qui doit être installé sur la machine.


Ce module est distribué selon la même licence qu'Ocaml.


{C Copyright Stéphane Grognet }
{C Laboratoire de mathématiques Jean Leray UMR 6629 CNRS }
{C Fédération de recherche mathématique des Pays de la Loire }
{C IREM des Pays de la Loire - Université de Nantes }
{C version 0.2}
*)
(**
@version 0.2
*)
(**
@author Stéphane Grognet
*)
(**
@since 2012, 2013
*)




(** {C § } *)
(** 
{1 Initialisation}
*)
(** {C  } *)




open Util ;;
open Readwrite ;;


(** {v standard_modules_list v} *)
let standard_modules_list = [ "Arg" ; "Array" ; "ArrayLabels" ;
 "Buffer" ;
 "Callback" ; "CamlinternalLazy" ; "CamlinternalMod" ; "CamlinternalOO" ; "Char" ; "Complex" ; 
 "Digest" ;
 "Filename" ; "Format" ;
 "Gc" ; "Genlex" ;
 "Hashtbl" ; "Hashtbl.Make" ;
 "Int32" ; "Int64" ;
 "Lazy" ; "Lexing" ; "List" ; "ListLabels" ;
 "Map" ; "Map.Make" ; "Marshal" ; "MoreLabels" ; "MoreLabels.Hashtbl" ; "MoreLabels.Hashtbl.Make" ; "MoreLabels.Map" ; "MoreLabels.Map.Make" ; "MoreLabels.Set" ; "MoreLabels.Set.Make" ;
 "Nativeint" ;
 "Obj" ;  "Oo" ; 
 "Parsing" ; "Pervasives" ; "Printexc" ; "Printf" ;
 "Queue" ;
 "Random" ; "Random.State" ;
 "Scanf" ;  "Scanf.Scanning" ; "Set" ; "Set.Make" ; "Sort" ; "Stack" ; "StdLabels" ; "StdLabels.Array" ; "StdLabels.List" ; "StdLabels.String" ; "Stream" ; "String" ; "StringLabels" ; "Sys" ;
 "Weak" ; "Weak.Make"
 ] ;;

(** {v non_standard_modules_list v} *)
let non_standard_modules_list = [ ( "Arith_status" , "nums.cma" ) ; 
 ( "Big_int" , "nums.cma" ) ; ( "Bigarray" , "bigarray.cma" ) ; ( "Bigarray.Array1" , "bigarray.cma" ) ; ( "Bigarray.Array2" , "bigarray.cma" ) ; ( "Bigarray.Array3" , "bigarray.cma" ) ; ( "Bigarray.Genarray" , "bigarray.cma" ) ; 
 ( "Com" , "com.cma" ) ;
 ( "Dbm" , "dbm.cma" ) ; ( "Dynlink" , "dynlink.cma" ) ;
 ( "Graphics" , "graphics.cma" ) ; ( "GraphicsX11" , "graphicsX11.cmx" ) ;
 ( "Nat" , "nums.cma" ) ; ( "Num" , "nums.cma" ) ;
 ( "Ratio" , "nums.cma" ) ;
 ( "Str" , "str.cma" ) ;
 ( "Unix" , "unix.cma" ) ; ( "Unix.LargeFile" , "unix.cma" ) ; ( "UnixLabels" , "unix.cma" ) ; ( "UnixLabels.LargeFile" , "unix.cma" )
 ] ;;

(** {v math_modules_list v} *)
let math_modules_list =
 begin
  let dep = ref
   begin
    try
     Readwrite.list_read_reverse_text_file "../math/depend.conf"
    with _ ->
     begin
      try
       Readwrite.list_read_reverse_text_file "../depend.conf"
      with _ ->
       Readwrite.list_read_reverse_text_file ( ( Filename.dirname Sys.executable_name ) ^ "/../depend.conf" )
     end
   end
  and accu = ref []
  and f = function x -> Readwrite.string_match ( Str.regexp ( Str.quote "[" ) ) x 0 in
   let g = function x ->
    begin
     if f x then
      let i = Str.match_beginning () in
       String.sub x 0 i
     else
      failwith "Functor in Spec.math_modules_list."
    end in
    while Util.list_non_empty !dep do
     begin
      try
       accu := ( g ( List.hd !dep ) ) :: !accu
      with _ -> ()
     end ;
     dep := List.tl !dep
    done ;
    !accu
 end ;;

(** {v math_functors_list v} *)
let math_functors_list =
 begin
  let dep = ref
   begin
    try
     Readwrite.list_read_reverse_text_file "../math/depend.conf"
    with _ ->
     begin
      try
       Readwrite.list_read_reverse_text_file "../depend.conf"
      with _ ->
       Readwrite.list_read_reverse_text_file ( ( Filename.dirname Sys.executable_name ) ^ "/../depend.conf" )
     end
   end
  and accu = ref []
  and f = function x -> Readwrite.string_match ( Str.regexp ( Str.quote "[" ) ) x 0 in
   let g = function x ->
    begin
     if not ( f x ) then
      begin
       if Readwrite.string_match ( Str.regexp "[^A-Za-z_.]" ) x 0 then
        begin
         let i = Str.match_beginning () in
          String.sub x 0 i
        end
       else
        failwith "Error in Spec.math_functors_list."
      end
     else
      failwith "Plain module in Spec.math_functors_list." ;
    end in
    while Util.list_non_empty !dep do
     begin
      try
       accu := ( g ( List.hd !dep ) ) :: !accu
      with _ -> ()
     end ;
     dep := List.tl !dep
    done ;
    !accu
 end ;;

(** {v standard_path v} *)
let standard_path =
 ignore ( Sys.command "which ocamlrun > tmp.txt" ) ;
 let chaine = List.hd ( Readwrite.list_read_reverse_text_file "tmp.txt") in
  ignore ( Sys.command "rm tmp.txt" ) ;
  Filename.dirname ( Filename.dirname chaine ) ;;




(** {C § } *)
(** 
{1 Fonctions de recherche et de documentation}
{1 Search and documentation functions}
*)
(** {C  } *)




(** The functions [which*] proceed by try-and-fail compilation.

Les fonctions [which*] procèdent par essai-échec de compilation. *)

(** {v which_standard standard_modules_list value_string v} *)
let which_standard = fun (l:string list) (x:string) ->
 let list = ref l
 and stamp = "standard module"
 and heading = "module Try = struct"
 and closing = "end ;;"
 and module_name = ref ""
 and result = ref []
 and opening = ref ""
 and test = ref "" in
  while Util.list_non_empty !list do
   module_name := List.hd !list ;
   opening := "open " ^ !module_name ^ " ;;" ;
   test := !module_name ^ "." ^ x ;
   Readwrite.array_write_text_file [| heading ; !opening ; "let trying = " ^ !test ^ " ;;" ; closing |] "tmp_try.ml" ;
   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -o /dev/null tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test , stamp ) :: !result
     | _ -> ()
    end ;
    list  := List.tl !list
  done ;
  begin
   try
    begin
     Sys.remove "tmp_try.ml" ;
     Sys.remove "tmp_try.sh" ;
     Sys.remove "tmp_try.cmi"
    end
   with _ ->
    ()
  end ;
  Array.of_list ( List.rev !result ) ;;

(** {v which_non_standard non_standard_modules_list value_string v} *)
let which_non_standard = fun (l:(string * string ) list) (x:string) ->
 let list = ref l
 and stamp = "non standard module"
 and heading = "module Try = struct"
 and closing = "end ;;"
 and module_name = ref ""
 and result = ref []
 and loading = ref ""
 and opening = ref ""
 and test = ref "" in
  while Util.list_non_empty !list do
   let item = List.hd !list in
    module_name := fst item ;
    opening := "open " ^ !module_name ^ " ;;" ;
    loading := snd item ;
    test := !module_name ^ "." ^ x ;
    Readwrite.array_write_text_file [| heading ; !opening ; "let trying = " ^ !test ^ " ;;" ; closing |] "tmp_try.ml" ;
    Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -o /dev/null " ^ !loading ^ " tmp_try.ml" |] "tmp_try.sh" ;
    let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
     begin
      match exit_code with
      | Unix.WEXITED 0 -> result := ( !test , stamp ) :: !result
      | _ -> ()
     end ;
     list  := List.tl !list
  done ;
  begin
   try
    begin
     Sys.remove "tmp_try.ml" ;
     Sys.remove "tmp_try.sh" ;
     Sys.remove "tmp_try.cmi"
    end
   with _ ->
    ()
  end ;
  Array.of_list ( List.rev !result ) ;;

(** {v which_math math_modules_list value_string v} *)
let which_math = fun (l:string list) (x:string) ->
 let list = ref l
 and stamp = "math module"
 and heading = "module Try = struct"
 and closing = "end ;;"
 and module_name = ref ""
 and result = ref []
 and test = ref "" in
  while Util.list_non_empty !list do
   module_name := List.hd !list ;
   test := !module_name ^ "." ^ x ;
   Readwrite.array_write_text_file [| heading ; "open Graphicmath ;;" ; "let trying = " ^ !test ^ " ;;" ; closing |] "tmp_try.ml" ;
(** The files "graphicmath.cma" and "graphicmath.cmi" must be in the same directory as this module.

Les fichiers "graphicmath.cma" et "graphicmath.cmi" doivent être dans le même répertoire que ce module. *)

   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -linkall -o /dev/null " ^ standard_path ^ "/lib/ocaml/graphics.cma " ^ standard_path ^ "/lib/ocaml/nums.cma " ^ standard_path ^ "/lib/ocaml/str.cma " ^ standard_path ^ "/lib/ocaml/unix.cma " ^ "-I " ^ ( Filename.dirname Sys.executable_name ) ^ " graphicmath.cma tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test , stamp ) :: !result
     | _ -> ()
    end ;
    list  := List.tl !list
  done ;
  begin
   try
    begin
     Sys.remove "tmp_try.ml" ;
     Sys.remove "tmp_try.sh" ;
     Sys.remove "tmp_try.cmi"
    end
   with _ ->
    ()
  end ;
  Array.of_list ( List.rev !result ) ;;

(** {v which_math_functor math_modules_list value_string v} *)
let which_math_functor = fun (l:string list) (x:string) ->
 let list = ref l
 and stamp = "math functor"
 and heading = "module Try = struct"
 and closing = "end ;;"
 and module_name = ref ""
 and result = ref []
 and test = ref "" in
(** The files "graphicmath.cma" and "graphicmath.cmi" must be in the same directory as this module.

Les fichiers "graphicmath.cma" et "graphicmath.cmi" doivent être dans le même répertoire que ce module. *)
  while Util.list_non_empty !list do
   module_name := List.hd !list ;
   test := !module_name ;
(** Bary.make *)
   Readwrite.array_write_text_file [| heading ; "open Graphicmath ;;" ; "include ( " ^ !test ^ " (Data.Zindex) (Data.Rbare_coeff) ) ;; " ; "let trying = " ^ x ^ " ;;" ; closing |] "tmp_try.ml" ;
   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -linkall -o /dev/null " ^ standard_path ^ "/lib/ocaml/graphics.cma " ^ standard_path ^ "/lib/ocaml/nums.cma " ^ standard_path ^ "/lib/ocaml/str.cma " ^ standard_path ^ "/lib/ocaml/unix.cma " ^ "-I " ^ ( Filename.dirname Sys.executable_name ) ^ " graphicmath.cma tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test ^ "." ^ x , stamp ) :: !result
     | _ -> ()
    end ;
(** Data.Index_of_array *)
   Readwrite.array_write_text_file [| heading ; "open Graphicmath ;;" ; "include ( " ^ !test ^ " ( struct type t = int ;; let a = [| 1 |] ;; let of_string = int_of_string ;; let to_string = string_of_int ;; let print = print_int ;; let eq = ( = ) ;; end ) ) ;; " ; "let trying = " ^ x ^ " ;;" ; closing |] "tmp_try.ml" ;
   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -linkall -o /dev/null " ^ standard_path ^ "/lib/ocaml/graphics.cma " ^ standard_path ^ "/lib/ocaml/nums.cma " ^ standard_path ^ "/lib/ocaml/str.cma " ^ standard_path ^ "/lib/ocaml/unix.cma " ^ "-I " ^ ( Filename.dirname Sys.executable_name ) ^ " graphicmath.cma tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test ^ "." ^ x , stamp ) :: !result
     | _ -> ()
    end ;
(** Data.Multi_index *)
   Readwrite.array_write_text_file [| heading ; "open Graphicmath ;;" ; "include ( " ^ !test ^ " (Data.Zindex) ) ;; " ; "let trying = " ^ x ^ " ;;" ; closing |] "tmp_try.ml" ;
   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -linkall -o /dev/null " ^ standard_path ^ "/lib/ocaml/graphics.cma " ^ standard_path ^ "/lib/ocaml/nums.cma " ^ standard_path ^ "/lib/ocaml/str.cma " ^ standard_path ^ "/lib/ocaml/unix.cma " ^ "-I " ^ ( Filename.dirname Sys.executable_name ) ^ " graphicmath.cma tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test ^ "." ^ x , stamp ) :: !result
     | _ -> ()
    end ;
(** Data.Normalize_field_coefficient ; Data.Normalize_rng_coefficient *)
   Readwrite.array_write_text_file [| heading ; "open Graphicmath ;;" ; "include ( " ^ !test ^ " (Data.Rbare_coeff) (Data.Rbare_coeff) ) ;; " ; "let trying = " ^ x ^ " ;;" ; closing |] "tmp_try.ml" ;
   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -linkall -o /dev/null " ^ standard_path ^ "/lib/ocaml/graphics.cma " ^ standard_path ^ "/lib/ocaml/nums.cma " ^ standard_path ^ "/lib/ocaml/str.cma " ^ standard_path ^ "/lib/ocaml/unix.cma " ^ "-I " ^ ( Filename.dirname Sys.executable_name ) ^ " graphicmath.cma tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test ^ "." ^ x , stamp ) :: !result
     | _ -> ()
    end ;
(** Fft.Field ; Mat.Field ; Mat.Rng *)
   Readwrite.array_write_text_file [| heading ; "open Graphicmath ;;" ; "include ( " ^ !test ^ " (Data.Rcoeff) ) ;; " ; "let trying = " ^ x ^ " ;;" ; closing |] "tmp_try.ml" ;
   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -linkall -o /dev/null " ^ standard_path ^ "/lib/ocaml/graphics.cma " ^ standard_path ^ "/lib/ocaml/nums.cma " ^ standard_path ^ "/lib/ocaml/str.cma " ^ standard_path ^ "/lib/ocaml/unix.cma " ^ "-I " ^ ( Filename.dirname Sys.executable_name ) ^ " graphicmath.cma tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test ^ "." ^ x , stamp ) :: !result
     | _ -> ()
    end ;
(** Hash.Multi_hasher *)
   Readwrite.array_write_text_file [| heading ; "open Graphicmath ;;" ; "include ( " ^ !test ^ " (Data.Zindex) (Hash.Z) ) ;; " ; "let trying = " ^ x ^ " ;;" ; closing |] "tmp_try.ml" ;
   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -linkall -o /dev/null " ^ standard_path ^ "/lib/ocaml/graphics.cma " ^ standard_path ^ "/lib/ocaml/nums.cma " ^ standard_path ^ "/lib/ocaml/str.cma " ^ standard_path ^ "/lib/ocaml/unix.cma " ^ "-I " ^ ( Filename.dirname Sys.executable_name ) ^ " graphicmath.cma tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test ^ "." ^ x , stamp ) :: !result
     | _ -> ()
    end ;
(** Hash.Make ; Sparse_vector.Rng ; Sparse_vector.Field ; Sparse_tensor.Rng ; Sparse_tensor.Field ; Sparse_matrix.Rng ; Sparse_matrix.Field *)
   Readwrite.array_write_text_file [| heading ; "open Graphicmath ;;" ; "include ( " ^ !test ^ " (Data.Zindex) (Hash.Z) (Data.Rcoeff) ) ;; " ; "let trying = " ^ x ^ " ;;" ; closing |] "tmp_try.ml" ;
   Readwrite.array_write_text_file [| "#!/bin/sh" ; "ocamlc.opt -linkall -o /dev/null " ^ standard_path ^ "/lib/ocaml/graphics.cma " ^ standard_path ^ "/lib/ocaml/nums.cma " ^ standard_path ^ "/lib/ocaml/str.cma " ^ standard_path ^ "/lib/ocaml/unix.cma " ^ "-I " ^ ( Filename.dirname Sys.executable_name ) ^ " graphicmath.cma tmp_try.ml" |] "tmp_try.sh" ;
   let exit_code = Unix.system ( "sh tmp_try.sh > /dev/null 2>&1" ) in
    begin
     match exit_code with
     | Unix.WEXITED 0 -> result := ( !test ^ "." ^ x , stamp ) :: !result
     | _ -> ()
    end ;
    list  := List.tl !list
  done ;
  begin
   try
    begin
     Sys.remove "tmp_try.ml" ;
     Sys.remove "tmp_try.sh" ;
     Sys.remove "tmp_try.cmi"
    end
   with _ ->
    ()
  end ;
  Array.of_list ( List.rev !result ) ;;

(** {v which value_string v} *)
let which = function (x:string) ->
 let standard = which_standard standard_modules_list x
 and non_standard = which_non_standard non_standard_modules_list x
 and math = which_math math_modules_list x
 and math_functors = which_math_functor math_functors_list x in
  Array.concat [ standard ; non_standard ; math_functors ; math ] ;;


(** The functions [how*] proceed by browsing the html documentation made from the source codes by [ocamldoc]
(forbidden [*.mli] interfaces).

Les fonctions [how*] procèdent en butinant la documentation html fabriquée à partir des codes sources par [ocamldoc] 
(interfaces [*.mli] proscrites). *)

(** {v how_generic documentation_directory inter_name complete_value_string v} *)
let how_generic = fun (d:string) (inter_name:string) (x:string) ->
 let exit_code = Unix.system ( "lynx -dump -crawl " ^ d ^ "/" ^ inter_name ^ x ^ ".html > tmp_html 2>&1" ) in
  match exit_code with
  | Unix.WEXITED 0 ->
   begin
    let f = function s -> ( String.length s ) == 0
    and text = Readwrite.array_read_text_file "tmp_html" in
     begin
      try
       Sys.remove "tmp_html" ;
      with _ ->
       ()
     end ;
     let mask = Array.map f text
     and header = Array.sub text 0 2 in
      let beginning = Util.array_find_first ( ( = ) true ) mask in
       let corpus = Util.array_end ( succ beginning ) text in
        Array.append header corpus
   end
  | _ -> failwith "Not found in Spec.how_generic."  ;;

(** {v how_math documentation_directory complete_value_string v} *)
let how_math = fun (d:string) (x:string) ->
 try
  how_generic d "GraphicmathSpec/code_VALGraphicmath." x
 with _ ->
 failwith "Not found in Spec.how_math." ;;

(** {v how complete_value_string v} *)
let how = function (x:string) ->
 try
  how_math "../math" x
 with _ ->
  begin
   try
    how_math ( ( Filename.dirname Sys.executable_name ) ^ "/.." ) x
   with _ ->
    begin
     try
      how_math ( ( Filename.dirname Sys.executable_name ) ^ "/../../../doc/ocamlmath" ) x
     with _ ->
      failwith "Not found in Spec.how."
    end
  end ;;



(** The functions [what*] proceed by browsing the html documentation made by [ocamldoc].

Les fonctions [what*] procèdent en butinant la documentaion html fabriquée par [ocamldoc]. *)

(** {v what_generic documentation_directory inter_name complete_value_string v} *)
let what_generic = fun (d:string) (inter_name:string) (x:string) ->
 let expression = ref ( Str.regexp ( Str.quote "." ) )
 and f = fun e s -> Readwrite.string_match e s 0
 and r = String.length x in
  let position = ref ( Str.search_backward !expression x r ) in
   let cutting = succ !position
   and prefix = String.sub x 0 !position in
    let exit_code = Unix.system ( "lynx -dump -crawl " ^ d ^ "/" ^ inter_name ^ prefix ^ ".html > tmp_html 2>&1" ) in
    match exit_code with
    | Unix.WEXITED 0 ->
     begin
    let postfix = String.sub x cutting ( r - cutting )
    and text = ref ( Readwrite.array_read_text_file "tmp_html" ) in
     begin
      try
       Sys.remove "tmp_html" ;
      with _ ->
       ()
     end ;
     expression := Str.regexp ( Str.quote ( "val " ^ postfix ) ) ;
     position := Util.array_find_first ( f !expression ) !text ;
     text := Util.array_end !position !text ;
     expression := Str.regexp ( "^val " ) ;
     position := Util.array_find_first ( f !expression ) ( Util.array_end 1 !text ) ;
     if !position >= 0 then
      Array.sub !text 0 ( !position + 1 )
     else
(** Last value of the page.

On est à la fin de la page. *)
      !text
     end
    | _ -> failwith "Not found in Spec.what_generic." ;;

(** {v what_math documentation_directory complete_value_string v} *)
let what_math = fun (d:string) (x:string) ->
 try
  what_generic d "GraphicmathSpec/Graphicmath." x
 with _ -> failwith "Not found in Spec.what_math." ;;

(** {v what_standard complete_value_string v} *)
let what_standard = function (x:string) ->
 try
  what_generic ( standard_path ^ "/share/doc/ocaml/html/libref" ) "" x
 with _ -> failwith "not found in Spec.what_standard." ;;

(** {v what_findlib complete_value_string v} *)
let what_findlib = function (x:string) ->
 try
  what_generic ( standard_path ^ "/share/doc/ocaml/findlib/ref-html/lib" ) "" x
 with _ -> failwith "not found in Spec.what_findlib." ;;

(** {v what_ocamlgraph complete_value_string v} *)
let what_ocamlgraph = function (x:string) ->
 try
  what_generic ( standard_path ^ "/share/doc/ocamlgraph" ) "" x
 with _ -> failwith "not found in Spec.what_ocamlgraph." ;;

(** {v what complete_value_string v} *)
let what = function (x:string) ->
 try
  what_math "../math" x
 with _ ->
  begin
   try
    what_math ( ( Filename.dirname Sys.executable_name ) ^ "/.." ) x
   with _ ->
    begin
     try
      what_math ( ( Filename.dirname Sys.executable_name ) ^ "/../../../doc/ocamlmath" ) x
     with _ ->
      begin
       try
        what_standard x
       with _ ->
        begin
         try
          what_findlib x
         with _ ->
          begin
           try
            what_ocamlgraph x
           with _ -> failwith "Not found in Spec.what."
          end
        end
      end
    end
  end ;;


(** The functions [why*] proceed by browsing the html documentation made from the source codes by [ocamldoc]
(forbidden [*.mli] interfaces).

Les fonctions [why*] procèdent en butinant la documentation html fabriquée à partir des codes sources par [ocamldoc] 
(interfaces [*.mli] proscrites). *)

(** {v why_generic documentation_directory inter_name value_string v} *)
let why_generic = fun (d:string) (inter_name:string) (x:string) ->
 let prefix = d ^ "/" ^ inter_name
 and eq = fun a b -> ( fst a ) = ( fst b ) in
  let string = ref ( " ls " ^ prefix ^ " > tmp_list" ) in
   let exit_code = Unix.system !string in
    match exit_code with
    | Unix.WEXITED 0 ->
     begin
      let reading = function s -> String.concat "" ( Readwrite.list_read_text_file ( prefix ^ s ) )
      and dir_list = Readwrite.array_read_text_file "tmp_list" in
       if Array.length dir_list == 0 then
        failwith "Bad directory in Spec.why_generic." ;
       begin
        try
         Sys.remove "tmp_list" ;
        with _ ->
         ()
       end ;
       let f = function s -> ( Readwrite.string_match ( Str.regexp ( Str.quote x ) ) ( reading s ) 0 , s ) in
        let mask = Array.map f dir_list in
         let quoting = Util.vector_find_all eq ( true , "" ) mask
         and g = function y -> snd ( Array.get mask y ) in
          Array.map g quoting
     end
    | _ -> failwith "Not found in Spec.why_generic." ;;

(** {v why_math documentation_directory value_string v} *)
let why_math = fun (d:string) (x:string) ->
 try
  why_generic d "GraphicmathSpec/" x
 with _ -> failwith "Not found in Spec.why_math." ;;

(** {v why value_string v} *)
let why = function (x:string) ->
 try
  why_math "../math" x
 with _ ->
  begin
   try
    why_math ( ( Filename.dirname Sys.executable_name ) ^ "/.." ) x
   with _ ->
    begin
     try
      why_math ( ( Filename.dirname Sys.executable_name ) ^ "/../../../doc/ocamlmath" ) x
     with _ ->
      failwith "Not found in Spec.why."
    end
  end ;;





end ;;



