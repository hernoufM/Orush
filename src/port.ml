
type orientation = Horizontal | Vertical;;
type boat = {ident:char; long: int; orient:orientation; mutable x: int; mutable y:int};;
type state = boat list;;

(* transforme une chaine (ex : ‘‘A2H45’’) en une valeur de type boat *)
let boat_of_string chaine =
  let i = chaine.[0] and
  l = int_of_string (String.make 1 chaine.[1]) and
  o = if(chaine.[2]='H') then Horizontal else Vertical and
  x,y = int_of_string (String.make 1 chaine.[3]), int_of_string (String.make 1 chaine.[4])
  in
  {ident=i; long=l; orient=o; x; y};;

(* represente un bateau sous la forme definie dans le sujet (ex : ‘‘A2H45’’) *)
let string_of_boat boat =
  (String.make 1 boat.ident)^
  (string_of_int boat.long)^
  (if(boat.orient=Horizontal) then "H" else "V")^
  (string_of_int boat.x)^(string_of_int boat.y);;

(* add_boat c s ajoute a l’etat s un nouveau bateau c. Leve Invalid_argument ‘‘add_boat’’ si le bateau ne peut etre mis a cette position *)
let add_boat boat state =
  match boat with
  | {ident=i;long=l;orient=o;x;y} ->
    let rec ne_intersecte_pas s =
      match s with
      | [] -> true
      | b::q -> match o,b.orient with
        | (Horizontal,Horizontal) -> (y+l<=b.y || b.y+b.long<=y)
        | (Vertical, Vertical) -> (x+l<=b.x || b.x+b.long<=x)
        | (Horizontal,Vertical) -> (not (x<=b.x && b.x<x+l) || (b.y > y || y >= b.y+b.long))
        | _ -> (not (y<=b.y && b.y<y+l) || (b.x > x || x >= b.x+b.long))
               && ne_intersecte_pas q
    and
      exists s =
      match s with
      | [] -> false
      | b::q -> if(b.ident=i) then true else exists q
    in
    if(exists state)
    then (let rec loop s =
            match s with
            | [] -> []
            | b::q -> if(b.ident=i) then boat::(loop q) else b::(loop q)
          in
          loop state)
    else if (ne_intersecte_pas state) &&
            ((if(o=Horizontal) then x else y) + l <= 6)
    then boat::state
    else raise (Invalid_argument "add_boat");;

(* retourne une representation matricielle d’un etat du port*)
let grid_of_state state =
  let grid = Array.make_matrix 6 6 '~' in
  let s = ref state in
  while !s<>[] do
    let b = List.hd !s in
    (if(b.orient=Horizontal) then
       for j = b.x to b.x + b.long - 1 do
         grid.(b.y).(j) <- b.ident
       done
     else
       for i = b.y to b.y + b.long -1 do
         grid.(i).(b.x) <- b.ident
       done);
    s:=List.tl !s
  done;
  grid;;

(* transforme une representation de l’etat initial du port depuis un canal d’entree vers une valeur de type state *)
let input_state channel =
  let state = ref [] in
  (try
     while true do
       let line = (input_line channel) in
       state := ((boat_of_string line)::!state)
     done;
   with
   | End_of_file -> ());
  List.rev !state;;

(* imprime une representation matricielle d’un etat du port sur un canal de sortie *)
let output_state state channel =
  let grid = grid_of_state state in
  for i = 0 to 5 do
    for j = 0 to 5 do
      output_char channel grid.(i).(j)
    done;
    output_char channel '\n'
  done;;
