(* objpool.ml *)
(* pool of allocated objects for explicit re-use *)
                                  
(* This object pool maintains a set of objects that are available
 * for use.  It must be given a way to create new objects. *)
type 'a t = {
  allocFunc : unit -> 'a;

  (* implementation is just an array of elements that have been made
   * available for re-use; this should be regarded as private
   * inheritance, though I don't see how to do that in OCaml *)
  stack : 'a Arraystack.t;
}

let null () = {
  allocFunc = (fun () -> failwith "unimplemented");
  stack = Arraystack.create ();
}

let make allocFunc = {
  allocFunc;
  stack = Arraystack.create ();
}

(* retrieve an object ready to be used; might return a pool element,
 * or if the pool is empty, will make a new element *)
let alloc pool =
  if not (Arraystack.is_empty pool.stack) then
    (* just grab the topmost element in the pool stack *)
    Arraystack.pop pool.stack
  else
    (* make a new object; I thought about making several at a time
     * but there seems little advantage.. *)
    pool.allocFunc ()

(* return an object to the pool so it can be re-used *)
let dealloc pool obj =
  (* put the element into the stack *)
  Arraystack.push obj pool.stack


(* EOF *)
