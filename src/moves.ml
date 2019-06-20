type move = Avancer of char
          | Reculer of char;;

exception Cannot_move;;

let position grid id =
  let res = ref (-1,-1) in
  for i = 0 to 5 do
    for j = 0 to 5 do
      if(grid.(i).(j)=id)
      then if(!res = (-1,-1))
        then res:=(i,j)
    done
  done;
  !res;;

let is_horizontal grid id =
  let y,x = position grid id in
  if(y=5)
  then true
  else if (grid.(y+1).(x)=id) then false else true;;

let longeur grid id horizontal x y =
  let long = ref 0 in
  (if horizontal then
     for j = x to 5 do
       if(grid.(y).(j)=id)
       then long:=!long+1
     done
   else
     for i = y to 5 do
       if(grid.(i).(x)=id)
       then long:=!long+1
     done);
  !long;;


(* convertit un move en chaine de caracteres *)
let string_of_move move =
  match move with
  | Avancer(c) -> (String.make 1 c)^">"
  | Reculer(c) -> (String.make 1 c)^"<"

(* convertit une chaine de caracteres en move *)
let move_of_string chaine =
  let id = chaine.[0] and act = chaine.[1] in
  if(act='>') then Avancer(id) else Reculer(id);;

let afficher state =
  let grid = Port.grid_of_state state in
  for i = 0 to 5 do
    for j = 0 to 5 do
      print_char grid.(i).(j)
    done;
    print_newline()
  done;
  print_newline();;

(* apply_move m s applique le mouvement m depuis l’etat s et retourne le nouvel etat. Leve l’exception Cannot_move si le mouvement est impossible *)
let apply_move move state =
  let grid = Port.grid_of_state state in
  match move with
  | Avancer(id) ->
    let horizontal = is_horizontal grid id in
    let y,x = position grid id in
    let long = longeur grid id horizontal x y in
    (if horizontal
     then
       (if ((x+long)<6) && (grid.(y).(x+long) = '~')
        then (grid.(y).(x)<-'~';
              grid.(y).(x+long)<-id;
              (Port.add_boat (Port.boat_of_string
                                ((String.make 1 id)^
                                 (string_of_int long)^
                                 (if horizontal then "H" else "V")^
                                 (string_of_int (x+1))^(string_of_int y))) state))
        else raise Cannot_move)
     else
       (if ((y+long)<6) && (grid.(y+long).(x) = '~')
        then (grid.(y).(x)<-'~';
              grid.(y+long).(x)<-id;
              (Port.add_boat (Port.boat_of_string
                                ((String.make 1 id)^
                                 (string_of_int long)^
                                 (if horizontal then "H" else "V")^
                                 (string_of_int x)^(string_of_int (y+1)))) state))
        else raise Cannot_move))
  | Reculer(id) ->
    let horizontal = is_horizontal grid id in
    let y,x = position grid id in
    let long = longeur grid id horizontal x y in
    (if horizontal
     then
       (if (x>0) && (grid.(y).(x-1) = '~')
        then (grid.(y).(x+long-1)<-'~';
              grid.(y).(x-1)<-id;
              (Port.add_boat (Port.boat_of_string
                                ((String.make 1 id)^
                                 (string_of_int long)^
                                 (if horizontal then "H" else "V")^
                                 (string_of_int (x-1))^(string_of_int y))) state))
        else raise Cannot_move)
     else
       (if (y>0) && (grid.(y-1).(x) = '~')
        then (grid.(y+long-1).(x)<-'~';
              grid.(y-1).(x)<-id;
              (Port.add_boat (Port.boat_of_string
                                ((String.make 1 id)^
                                 (string_of_int long)^
                                 (if horizontal then "H" else "V")^
                                 (string_of_int x)^(string_of_int (y-1)))) state))
        else raise Cannot_move));;

(* indique si un etat est une position gagnante *)
let win state =
  let grid = Port.grid_of_state state in
  grid.(2).(5)='A';;

(* verifie si une suite de mouvements depuis l’etat en parametre est une solution *)
let check_solution state chaine =
  let state = ref state and i = ref 0 in
  (try
     while !i<>(String.length chaine) do
       let sub = String.sub chaine !i 2
       in
       state := (apply_move (move_of_string sub) !state);
       i:=!i+2
     done;
     win !state
   with
   | Cannot_move -> false);;
