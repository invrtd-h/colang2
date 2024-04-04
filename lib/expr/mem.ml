module Vec = BatDynArray
module Heap = BatHeap
type 'a vec = 'a Vec.t
type 'a heap = 'a Heap.t

type 'a mem = {
  data : 'a option vec;
  mutable non_alloc_spaces : int heap;
}

type 'a t = 'a mem

let create : unit -> 'a mem
= fun () -> 
  {
    data : 'a option vec = Vec.create ();
    non_alloc_spaces : int heap = Heap.empty;
  }

let get : 'a mem -> int -> 'a
= fun mem addr -> Vec.get mem.data addr |> Option.get

let set : 'a mem -> int -> 'a -> unit
= fun mem addr value -> Vec.set mem.data addr (Some value)

let push : 'a mem -> 'a -> unit
= fun mem value -> Vec.add mem.data (Some value)

let kill : 'a mem -> int -> unit
= fun mem addr -> 
  let _ = Vec.set mem.data addr None in
  mem.non_alloc_spaces <- Heap.insert mem.non_alloc_spaces addr
  
let malloc : 'a mem -> int
= fun mem -> 
  if Heap.size mem.non_alloc_spaces == 0 then
    let _ = Vec.add mem.data None in
    Vec.length mem.data - 1
  else
    let ret = Heap.find_min mem.non_alloc_spaces in
    let _ = mem.non_alloc_spaces <- Heap.del_min mem.non_alloc_spaces in
    ret