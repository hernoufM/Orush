
(* calcul de tous les mouvements possibles depuis un etat *)
let all_possible_moves state =
  let grid = Port.grid_of_state state and listeBoats = ref [] and mouvements = ref []
  in
  for i = 0 to 5 do
    for j = 0 to 5 do
      let c = grid.(i).(j) in
      if(c<>'~') && (not (List.mem c !listeBoats))
      then listeBoats := c::(!listeBoats)
    done
  done;
  let curr = listeBoats in
  while !curr<>[] do
    let boatId = List.hd !curr in
    let moveA = (Moves.move_of_string ((String.make 1 boatId)^">")) and
      moveR = (Moves.move_of_string ((String.make 1 boatId)^"<")) in
    if (try
          let _ = Moves.apply_move moveA state
          in true
        with
        | Moves.Cannot_move -> false)
    then mouvements := moveA::(!mouvements);
    if (try
          let _ = Moves.apply_move moveR state
          in true
        with
        | Moves.Cannot_move -> false)
    then mouvements := moveR::(!mouvements);
    curr := List.tl !curr
  done;
  !mouvements;;

(* calcul de tous les etats atteignables depuis un etat et une liste de move *)
let all_reachable_states state moves =
  let states = ref [] and curr = ref moves in
  while !curr<>[] do
    states := (Moves.apply_move (List.hd !curr) state)::(!states);
    curr := List.tl !curr
  done;
  List.rev !states;;

let ajouterFileCh state solution queue =
  let l =ref (all_possible_moves state) in
  let q = ref queue
  in
  while (!l)<>[] do
    q:= File.push (solution^(Moves.string_of_move (List.hd !l))) !q;
    l:=(List.tl !l);
  done;
  !q;;

let ajouterFileSt state queue =
  let l =ref (all_reachable_states state (all_possible_moves state)) in
  let q = ref queue
  in
  while (!l)<>[] do
    q:= File.push (List.hd !l) !q;
    l:=(List.tl !l);
  done;
  !q;;

let stateEgaux st1 st2 =
  let grid1 = Port.grid_of_state st1 and grid2 = Port.grid_of_state st2 in
  let egaux = ref true in
  for i = 0 to 5 do
    for j = 0 to 5 do
      if(grid1.(i).(j)<>grid2.(i).(j))
      then egaux:=false;
    done
  done;
  !egaux;;

include File;;
(* calcul d’une solution a partir d’un etat *)
let solve_state state =
  let queueCh = ref (create()) and queueSt = ref (create()) and stateCourante = ref state
  and solution =ref "" and statesPasse = ref []
  in
  queueCh := ajouterFileCh !stateCourante !solution !queueCh;
  queueSt := ajouterFileSt !stateCourante !queueSt;
  while not (Moves.check_solution state !solution) do
    let (move, f) = pop !queueCh in
    solution:= move;
    queueCh := f;
    let (s,f2) = pop !queueSt in
    statesPasse:=(!stateCourante)::(!statesPasse);
    stateCourante:= s;
    queueSt := f2;
    if not (List.exists (stateEgaux !stateCourante) !statesPasse)
    then
      (queueCh := ajouterFileCh !stateCourante !solution !queueCh;
       queueSt := ajouterFileSt !stateCourante !queueSt;)
  done;
  !solution;;

let solve_input channel =
  let state = Port.input_state channel
  in let solution = solve_state state in
  print_endline solution;
  solution;;
