(**
L'oscilloscope a été inventé par Karl Ferdinand Braun (1850-1918).

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

open Analogic ;;

let v = Sys.argv in
 let arguments = Array.length v in
  if arguments < 6 then failwith "Usage: ferdinand.cmo phosphor_boolean monotrace_boolean window_width window_height max_number_for_selector" ;
  let phosphor = bool_of_string v.(1)
  and monotrace = bool_of_string v.(2)
  and width = int_of_string v.(3)
  and height = int_of_string v.(4)
  and number = int_of_string v.(5) in
   Analogic.oscillo_recorder phosphor monotrace width height number ;;

