(*
A mutable queue is used to hold the current battle commands that the
battle controller needs to process. The battle controller is in charge
of updating the queue (adding or deleting items), while the GUI sends
requests for additions and deletions.
*)

type 'a t = 'a list ref

let empty () : 'a t = ref []

let enqueue (x : 'a) (t : 'a t) : unit =
  t := x::!t

let dequeue (t : 'a t ) : 'a  =
  if List.length !t = 0 then
    failwith "Invariant Fail"
  else
    let x = List.hd !t in
    t := List.tl !t; x