(* parse tree node for use with ptreeact module *)


(* a node in a parse tree *)
type t = {
  (* list of ambiguous alternatives to this node *)
  merged : t option;

  (* symbol at this node *)
  symbol : string;

  (* list of children *)
  children : t list;
}


let make_leaf sym = {
  merged = None;
  symbol = sym;
  children = [];
}

let make sym child_count child_fun = {
  merged = None;
  symbol = sym;
  children =
    CoreInt.fold_right (fun i children ->
      child_fun i :: children
    ) 0 (child_count - 1) [];
}


let indent out n =
  for i = 0 to min (n - 1) 120 do
    Format.pp_print_char out ' '
  done


let rec merged_fold f init merged =
  match merged with
  | None -> init
  | Some n -> merged_fold f (f init n) n.merged


(* just the length of the 'merged' list *)
let merged_length self =
  merged_fold (fun v _ -> v + 1) 0 (Some self)


(* add an ambiguous alternative *)
let add_alternative self alt =
  (* insert as 2nd element *)
  { self with merged = Some { alt with merged = self.merged } }


let cyclic_skip self indentation out path =
  if PtreeOptions._ptree_cycles () then (
    (* does 'self' appear in 'path'? *)
    let idx = Liststack.index self path in
    if idx >= 0 then (
      (* yes; print a cyclicity reference *)
      indent out indentation;
      Format.fprintf out "[CYCLIC: refers to %d hops up]\n"
        (Liststack.length path - idx + 1);
      true   (* return *)
    ) else (
      (* no; add myself to the path *)
      Liststack.push self path;
      false
    )
  ) else (
    false
  )


let print_merged self indentation symbol =
  (* this is an ambiguity node *)
  let alts = merged_length self in

  (* get nonterm from first; should all be same *)
  let lhs =
    try
      (* extract first word *)
      let firstSpace = String.index symbol ' ' in
      String.sub symbol 0 firstSpace
    with
    | Not_found ->
        symbol    (* no spaces, use whole thing *)
  in

  indentation + PtreeOptions._ptree_indent (), lhs, alts


let print_alt self indentation out expand alts lhs ct node =
  if alts > 1 then (
    indent out (indentation - PtreeOptions._ptree_indent ());
    Format.fprintf out "------------- ambiguous %s: %d of %d ------------\n"
      lhs ct alts
  );

  indent out indentation;

  Format.pp_print_string out node.symbol;

  if expand then (
    (* symbol is just LHS, write out RHS names after "->" *)
    if node.children <> [] then (
      Format.pp_print_string out " ->";
      List.iter (fun c ->
        Format.pp_print_char out ' ';
        Format.pp_print_string out c.symbol;
      ) node.children
    )
  );

  Format.pp_print_char out '\n'


let print_tree out self expand =
  (* for detecting cyclicity *)
  let path = Liststack.create () in

  let rec print_tree self indentation =
    if not (cyclic_skip self indentation out path) then (
      let indentation, lhs, alts =
        match self.merged with
        | Some _ ->
            print_merged self indentation self.symbol
        | None ->
            indentation, "", 1
      in

      (* iterate over interpretations *)
      ignore (merged_fold (fun ct node ->
        print_alt self indentation out expand alts lhs ct node;

        (* iterate over children and print them *)
        List.iter (fun c ->
          print_tree c (indentation + PtreeOptions._ptree_indent ())
        ) node.children;

        ct + 1
      ) 1 (Some self));

      if alts > 1 then (
        (* close up ambiguity display *)
        indent out (indentation - PtreeOptions._ptree_indent ());
        Format.fprintf out "----------- end of ambiguous %s -----------\n" lhs
      );

      if PtreeOptions._ptree_cycles () then
        (* remove myself from the path *)
        ignore (Liststack.pop path)
    );
  in

  print_tree self 0(*indentation*)


let to_string self expand =
  let buf = Buffer.create 128 in
  print_tree (Format.formatter_of_buffer buf) self expand;
  Buffer.contents buf
