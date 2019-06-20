open Graphics;;

type boat_colored = {ident: char; color:Graphics.color; mutable x:int; mutable y:int; width:int; heigth:int};;
type boats = boat_colored list;;

let colors = [green; blue; yellow; cyan; magenta];;
let boats = ref [];;

let boat ident color x y w h =
  {ident; color; x; y; width=w; heigth=h};;

let position grid id =
  let res = ref (-1,-1) in
  for i = 0 to 5 do
    for j = 0 to 5 do
      if(grid.(i).(j)=id)
      then if(!res = (-1,-1))
        then res:=(j,i)
    done
  done;
  !res;;

let is_horizontal grid id =
  let x,y = position grid id in
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

let translate_x_y grid id =
  let x,y = position grid id in
  let horizontal = is_horizontal grid id in
  let long =longeur grid id horizontal x y in
  if horizontal then
    (50+((x)*50),300-(y*50))
  else (50+((x)*50),300-((y+long-1)*50))
;;

let calculer_width grid id =
  let x,y = position grid id in
  let horizontal = is_horizontal grid id in
  let long = longeur grid id horizontal x y in
  if horizontal then long*50
  else 50;;

let calculer_heigth grid id =
  let x,y = position grid id in
  let horizontal = is_horizontal grid id in
  let long = longeur grid id horizontal x y in
  if horizontal then 50
  else long*50;;

let init_boats state =
  let grid = Port.grid_of_state state in
  let couleurs = ref colors in
  for i = 0 to 5 do
    for j = 0 to 5 do
      (if grid.(i).(j)<>'~'
       then (if not (List.mem grid.(i).(j) (List.map (fun a -> a.ident) !boats))
             then
               (if grid.(i).(j) = 'A'
                then boats:= (boat 'A' red (fst (translate_x_y grid 'A')) (snd (translate_x_y grid 'A')) (calculer_width grid 'A') (calculer_heigth grid 'A'))::(!boats)
                else (if (!couleurs=[]) then couleurs := colors;
                      boats := (boat grid.(i).(j) (List.hd !couleurs) (fst (translate_x_y grid grid.(i).(j))) (snd (translate_x_y grid grid.(i).(j))) (calculer_width grid grid.(i).(j)) (calculer_heigth grid grid.(i).(j)))::(!boats);
                      couleurs := List.tl !couleurs;))));
    done;
  done;;

let draw_background () =
  set_window_title "orush";
  set_color black;
  fill_rect 0 0 400 400;
  set_color white;
  fill_rect 50 50 300 300;
  fill_rect 350 200 50 50;
  moveto 75 375; draw_string "0"; moveto 125 375; draw_string "1"; moveto 175 375; draw_string "2"; moveto 225 375; draw_string "3"; moveto 275 375; draw_string "4"; moveto 325 375; draw_string "5";
  moveto 25 75; draw_string "5"; moveto 25 125; draw_string "4"; moveto 25 175; draw_string "3"; moveto 25 225; draw_string "2"; moveto 25 275; draw_string "1"; moveto 25 325; draw_string "0";;


let init_graph () =
  open_graph " 400x400+50-0";
  draw_background ();;

let mise_a_jour_boats state =
  let grid = Port.grid_of_state state in
  let rec loop c =
    match c with
    | b::l ->
      let newX, newY = (translate_x_y grid b.ident) in
      b.x<-newX;
      b.y<-newY;
      loop l;
    | [] -> ()
  in loop !boats;;

let draw_boats () =
  draw_background ();
  let rec loop c =
    match c with
    | b::l ->
      set_color b.color;
      fill_rect (b.x+1) (b.y+1) (b.width-2) (b.heigth-2);
      loop l;
    | [] -> ()
  in loop !boats;;

let selected_boat = ref 'A';;
let selected_direction = ref ' ';;
let draw_selection () =
  set_color black;
  let b =
    let rec loop l =
      match l with
      | b::ll -> if b.ident = !selected_boat then b else loop ll
      | [] -> raise Not_found
    in
    loop !boats
  in
  draw_poly [|(b.x,b.y);(b.x,b.y+b.heigth);(b.x+b.width,b.y+b.heigth);(b.x+b.width,b.y)|];;

let button_handler status state=
  let grid = Port.grid_of_state state  in
  let boat = let rec loop l =
               match l with
               | b::ll -> if b.ident = !selected_boat then b else loop ll
               | [] -> raise Not_found
    in
    loop !boats
  in
  let is_boat =
    let rec loop l =
      match l with
      | b::ll -> if (b.x<=status.mouse_x && b.y<=status.mouse_y && status.mouse_x<b.x+b.width && status.mouse_y < b.y+b.heigth) then true else loop ll
      | [] -> false
    in loop !boats
  in
  if is_boat
  then let b =
         let rec loop l =
           match l with
           | b::ll -> if (b.x<=status.mouse_x && b.y<=status.mouse_y && status.mouse_x<b.x+b.width && status.mouse_y < b.y+b.heigth) then b else loop ll
           | [] -> raise Not_found
         in loop !boats
    in selected_boat := b.ident;
  else
    let horizontal = is_horizontal grid !selected_boat in
    if horizontal
    then (if status.mouse_x<boat.x
          then selected_direction:='<'
          else if boat.x+boat.width<= status.mouse_x
          then selected_direction:='>'
          else selected_direction:=' ')
    else
      (if status.mouse_y<boat.y
       then selected_direction:='>'
       else if boat.y+boat.heigth<= status.mouse_y
       then selected_direction:='<'
       else selected_direction:=' ');;

let key_handler status state =
  let grid = Port.grid_of_state state  in
  let horizontal = is_horizontal grid !selected_boat in
  if horizontal
  then (if status.key = 'q'
        then selected_direction:='<'
        else if status.key = 'd'
        then selected_direction:='>')
  else
    (if status.key = 's'
     then selected_direction:='>'
     else if status.key = 'z'
     then selected_direction:='<');;

let event_handler status state=
  if status.keypressed then key_handler status state
  else button_handler status state;;

let moves state =
  if(!selected_direction = ' ') then state
  else (try
          let newState = Moves.apply_move (Moves.move_of_string ((String.make 1 !selected_boat)^(String.make 1 !selected_direction))) state
          in
          selected_direction := ' ';
          newState
        with
        | Moves.Cannot_move -> selected_direction := ' '; state);;

let draw_message_gagnant () =
  moveto 25 25;
  set_color red;
  draw_string "Felicitations! Appuyez la touche 'e' pour sortir";
  let event_status = wait_next_event [Key_pressed] in
  let touche = ref event_status.key in
  while !touche<>'e' do
    let event_status = wait_next_event [Key_pressed] in
    touche:= event_status.key;
  done;;

let start state =
  init_graph ();
  init_boats state;
  draw_boats ();
  draw_selection ();
  let stateCourante = ref state in
  while not (Moves.win !stateCourante) do
    let event_status = wait_next_event [Button_down; Key_pressed]
    in
    event_handler event_status !stateCourante;
    stateCourante:= moves !stateCourante;
    mise_a_jour_boats !stateCourante;
    draw_background ();
    draw_boats ();
    draw_selection ();
  done;
  draw_message_gagnant ();;
