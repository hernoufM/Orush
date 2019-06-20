type 'a queue

exception Empty_queue

val create : unit -> 'a queue
val push : 'a -> 'a queue -> 'a queue
val pop : 'a queue -> 'a * 'a queue
val to_list : 'a queue -> 'a list
