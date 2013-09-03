open Sexplib.Conv

type variant_kind =
  | User
  | Ptree
  | Treematch

type 'semantic variant = 'semantic list with sexp

type 'semantic variants
  = 'semantic variant
  * 'semantic variant
  * 'semantic variant
  with sexp


let iter f =
  begin
    f User;
    f Ptree;
    f Treematch;
  end

let prefix_for_variant_kind = function
  | User -> ""
  | Ptree -> "Ptree"
  | Treematch -> "Treematch"


let get_variant which variants =
  match which, variants with
  | User     , (user, ptree, treematch) -> user
  | Ptree    , (user, ptree, treematch) -> ptree
  | Treematch, (user, ptree, treematch) -> treematch

let set_variant which variants value =
  match which, variants with
  | User     , (user, ptree, treematch) -> (value, ptree, treematch)
  | Ptree    , (user, ptree, treematch) -> (user, value, treematch)
  | Treematch, (user, ptree, treematch) -> (user, ptree, value)


let rec find f = function
  | [] -> None
  | hd :: tl ->
      match f hd with
      | None -> find f tl
      | Some _ as some -> some

let find which f variants =
  find f (get_variant which variants)

let add which sem variants =
  set_variant which variants
    (sem :: get_variant which variants)

let add_option which sem variants =
  match sem with
  | None -> variants
  | Some sem -> add which sem variants

let set_list which sems variants =
  if get_variant which variants <> [] then
    failwith "attempted to replace existing semantic information"
  else
    set_variant which variants sems

let replace which why update variants =
  match get_variant which variants with
  | [] ->
      set_list which [update] variants
  | sems ->
      let rec set_loop updated = function
        | [] ->
            update :: updated
        | sem :: sems ->
            if why sem then
              update :: (updated @ sems)
            else
              set_loop (sem :: updated) sems
      in
      set_variant which variants (set_loop [] sems)

let map which f variants =
  set_variant which variants
    (List.map f (get_variant which variants))


let empty () = ([], [], [])

let singleton which sem =
  set_variant which (empty ()) [sem]

let of_list which sems =
  let sems =
    List.fold_left (fun sems -> function
      | None -> sems
      | Some sem -> sem :: sems
    ) [] sems
  in

  set_variant which (empty ()) sems

let of_option which sem =
  of_list which [sem]


let combine a b =
  match a, b with
  | ([], [], []), (u, p, t) -> (u, p, t)
  | ([], [], t), (u, p, []) -> (u, p, t)
  | ([], p, []), (u, [], t) -> (u, p, t)
  | ([], p, t), (u, [], []) -> (u, p, t)
  | (u, [], []), ([], p, t) -> (u, p, t)
  | (u, [], t), ([], p, []) -> (u, p, t)
  | (u, p, []), ([], [], t) -> (u, p, t)
  | (u, p, t), ([], [], []) -> (u, p, t)

  | (_::_, _, _), (_::_, _, _) -> failwith "attempted to replace existing user actions"
  | (_, _::_, _), (_, _::_, _) -> failwith "attempted to replace existing ptree actions"
  | (_, _, _::_), (_, _, _::_) -> failwith "attempted to replace existing treematch actions"
