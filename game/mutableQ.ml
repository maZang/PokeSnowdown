(*
A mutable queue is used to hold the current battle commands that the
battle controller needs to process. The battle controller is in charge
of updating the queue (adding or deleting items), while the GUI sends
requests for additions and deletions.
*)
open Info
type t = command list ref

let empty () = ref []

let is_empty t = !t = []

let enqueue x t =
  t := !t @ [x]

let dequeue t =
  if List.length !t = 0 then
    failwith "Invariant Fail"
  else
    let x = List.hd !t in
    t := List.tl !t; x

let empty_out t = t := []

let wait_for_input t =
  t := Buffer::(!t)

let take_in_input t =
  let rec find_last_element acc lst =
  match lst with
  | [] -> failwith "Faulty Game Logic"
  | h::[] -> acc, h
  | h::t -> find_last_element (acc @ [h]) t in
  let new_list, last_element = find_last_element [] !t in
  t := last_element::new_list