type boat
type state

(* transforme une chaine (ex : ‘‘A2H45’’) en une valeur de type boat *)
val boat_of_string : string -> boat
(* represente un bateau sous la forme definie dans le sujet (ex : ‘‘A2H45’’) *)
val string_of_boat : boat -> string
(* add_boat c s ajoute a l’etat s un nouveau bateau c. Leve Invalid_argument ‘‘add_boat’’ si le bateau ne peut etre mis a cette position *)
val add_boat : boat -> state -> state
(* retourne une representation matricielle d’un etat du port*)
val grid_of_state : state -> char array array
(* transforme une representation de l’etat initial du port depuis un canal d’entree vers une valeur de type state *)
val input_state : in_channel -> state
(* imprime une representation matricielle d’un etat du port sur un canal de sortie *)
val output_state : state -> out_channel -> unit
