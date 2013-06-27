(* stack of pointers implemented as an array *)


(* grow an array *)
let growArray arr newLen =
  Array.init newLen (fun i ->
    if i < Array.length arr then
      Array.unsafe_get arr i
    else
      Obj.magic ()
  )


type 'a t = {
  (* number of (non-null) elements in the array *)
  mutable len : int;

  (* the array; its length may be greater than 'len', to
   * accomodate adding more elements without resizing the array *)
  mutable arr : 'a array;
}


let length { len } = len

let is_empty { len } = len = 0

let clear rep = rep.len <- 0


(* get topmost element but don't change what is stored *)
let top { len; arr } = Array.unsafe_get arr (len - 1)


(* get topmost and remove it *)
let pop rep =
  rep.len <- rep.len - 1;
  Array.unsafe_get rep.arr rep.len


(* add a new topmost element *)
let push obj rep =
  if rep.len = Array.length rep.arr then
    (* need to expand the array *)
    rep.arr <- growArray rep.arr (rep.len * 2);

  (* put new element into the array at the end *)
  Array.unsafe_set rep.arr rep.len obj;
  rep.len <- rep.len + 1


(* get arbitrary element *)
let nth { arr } i =
  arr.(i)


(* set arbitrary element *)
let set { arr } i v =
  arr.(i) <- v


(* iterate *)
let iter f rep =
  for i = 0 to rep.len - 1 do
    f (Array.unsafe_get rep.arr i)
  done


let rec index x arr i =
  if i = -1 then
    -1
  else if Array.unsafe_get arr i == x then
    i
  else
    index x arr (i - 1)

(* search and return the element index, or -1 for not found *)
let index x rep =
  index x rep.arr (rep.len - 1)


let rec find f arr i =
  if i = -1 then
    None
  else let e = Array.unsafe_get arr i in if f e then
    Some e
  else
    find f arr (i - 1)

(* search and return the element, or None *)
let find f rep =
  find f rep.arr (rep.len - 1)


(* swap contents with another array stack *)
let swap rep obj =
  let { len; arr } = rep in

  rep.len <- obj.len;
  rep.arr <- obj.arr;

  obj.len <- len;
  obj.arr <- arr


(* the stack must be given a dummy value for unused array slots *)
let create () =
  { len = 0; arr = Array.make 16 (Obj.magic ()); }
