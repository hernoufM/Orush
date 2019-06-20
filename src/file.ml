
type 'a queue = {mutable debut:'a list; mutable fin:'a list};;

let create () = {debut=[]; fin=[]};;
let push e q =
  {debut=q.debut; fin=(List.cons e q.fin)};;

exception Empty_queue;;
let pop q =
  match q with
  | {debut=[]; fin=[]}-> raise Empty_queue
  | {debut=[]; fin=l}->
    let lr=(List.rev l) in
    let tete = (List.hd lr) in
    (tete, {debut=(List.tl lr); fin=[]})
  | {debut=x1::ll;fin=l} -> (x1, {debut=ll; fin=l});;

let to_list q =
  (List.append q.debut (List.rev q.fin));;
