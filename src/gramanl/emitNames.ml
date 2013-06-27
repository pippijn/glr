open Camlp4.PreCast
open GrammarType

let (|>) = BatPervasives.(|>)
let ghost = Sloc.ghost "emitNames"


let str str =
  let _loc, str = Sloc._loc str in
  <:expr<$str:str$>>


(* ------------------- description functions ------------------ *)
let make_ml_descriptions terms nonterms =
  let _loc = ghost 15 in
  (* emit a map of terminal ids to their names *)
  let term_names_array =
    let names =
      TermArray.map (fun term -> str term.tbase.name) terms
      |> TermArray.to_list
      |> Ast.exSem_of_list
    in
    <:str_item<let termNamesArray : string array = [| $names$ |]>>
  in

  (* emit a map of terminal ids to their aliases *)
  let term_aliases_array =
    let names =
      TermArray.map (fun term -> str (GrammarUtil.name_of_terminal term)) terms
      |> TermArray.to_list
      |> Ast.exSem_of_list
    in
    <:str_item<let termAliasesArray : string array = [| $names$ |]>>
  in

  (* emit a map of nonterminal ids to their names *)
  let nonterm_names_array =
    let names =
      NtArray.map (fun nonterm -> str nonterm.nbase.name) nonterms
      |> NtArray.to_list
      |> Ast.exSem_of_list
    in
    <:str_item<let nontermNamesArray : string array = [| $names$ |]>>
  in

  <:sig_item<
    val termNamesArray : string array
    val termAliasesArray : string array
    val nontermNamesArray : string array
  >>,
  <:str_item<
    $term_names_array$
    $term_aliases_array$
    $nonterm_names_array$
  >>
