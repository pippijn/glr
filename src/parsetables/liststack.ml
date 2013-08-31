type 'a t = 'a list ref


let create () =
  ref []


let length stack =
  List.length !stack


let index elt stack =
  let rec loop idx = function
    | [] ->
        assert (idx = -1);
        idx
    | x :: xs ->
        if x == elt then
          idx
        else
          loop (idx - 1) xs
  in
  let idx = length stack - 1 in
  loop idx !stack


let push elt stack =
  stack := elt :: !stack


let pop stack =
  match !stack with
  | [] -> failwith "pop: empty stack"
  | x :: xs -> stack := xs
