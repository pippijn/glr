open CorePervasives
open Camlp4.PreCast
open GrammarType

let ghost = Sloc.ghost "emitActions"


(************************************************
 * :: Common helpers
 ************************************************)

let fold_bindings =
  let _loc = ghost 13 in
  List.fold_left (fun code binding ->
    <:expr<let $binding$ in $code$>>
  )


(************************************************
 * :: Semantic type helpers
 ************************************************)

let polytype sym =
  let _loc, name = Sloc._loc sym.name in
  <:ctyp<'$lid:"t" ^ name$>>


let semtype variant sym =
  match Semantic.semtype_of_symbol variant sym with
  | None ->
      polytype sym
  | Some semtype ->
      semtype


let final_semtype variant nonterms final_prod =
  match final_prod.right with
  | Nonterminal (_, nt_index) :: _ ->
      let nonterm = NtArray.get nonterms nt_index in
      begin match Semantic.semtype_of_nonterm variant nonterm with
      | None ->
	  failwith "final nonterminal needs defined type"
      | Some <:ctyp<'$_$>> ->
	  failwith "final nonterminal cannot be polymorphic"
      | Some semtype ->
	  semtype
      end

  | _ ->
      failwith "could not find final nonterminal"


(************************************************
 * :: User actions
 ************************************************)

let make_binding variant tag rhs_index sym =
  let semtype = semtype variant sym in
  let polytype = polytype sym in

  let _loc, tag = Sloc._loc tag in

  if semtype = polytype then
    <:binding<
      $lid:tag$ : $semtype$ =
        (SemanticValue.obj svals.($`int:rhs_index$))
    >>
  else
    <:binding<
      (* explicitly state polymorphic type variable so the typing
       * stays sound even when users specify types *)
      $lid:tag$ : $semtype$ =
        (SemanticValue.obj svals.($`int:rhs_index$) : $polytype$)
    >>


(* ------------------- actions ------------------ *)
let make_ml_actions variant index =
  (* iterate over productions, emitting action function closures *)
  let closures =
    ProdArray.map (fun prod ->
      (* put the production in comments above the defn *)
      if false then (
	Printf.printf "(*%a *)\n"
	  (PrintGrammar.print_production index.terms index.nonterms) prod
      );

      (* iterate over RHS elements, emitting bindings for each with a tag *)
      let bindings =
	BatList.mapi (fun rhs_index sym ->
	  match sym with
	  | Terminal (None, _)
	  | Nonterminal (None, _) ->
	      (* only consider elements with a tag *)
	      []

	  | Terminal (Some tag, term) ->
	      let term = TermArray.get index.terms term in
	      [make_binding variant tag rhs_index term.tbase]

	  | Nonterminal (Some tag, nonterm) ->
	      let nonterm = NtArray.get index.nonterms nonterm in
	      [make_binding variant tag rhs_index nonterm.nbase]
	) prod.right
	|> List.concat
      in

      let action_code =
	match Semantic.action_of_prod variant prod with
	| None ->
	    begin match bindings with
	    | [ <:binding<$lid:first_tagged$ = $_$>> ] ->
		let _loc = ghost 115 in
		<:expr<$lid:first_tagged$>>

	    | [binding] ->
		failwith "invalid name binding format"

	    | binding :: _ ->
		(*PrintGrammar.print_production prod;*)
		(* TODO: move this to a semantic check phase *)
		failwith "production with more than one binding must provide action code"

	    | [] ->
		(*PrintGrammar.print_production prod;*)
		(* TODO: this, too *)
		failwith "no name bindings in production with default action"
	    end
	| Some code ->
	    code
      in

      (* give a name to the yielded value so we can ensure it conforms to
       * the declared type *)
      let result =
	let left = NtArray.get index.nonterms prod.left in
	let _loc = ghost 139 in
	<:expr<
	  (* now insert the user's code, to execute in this environment of
	   * properly-typed semantic values *)
	  let __result : $semtype variant left.nbase$ = $action_code$ in
	  (* cast to SemanticValue.t *)
	  SemanticValue.repr __result
	>>
      in

      let fun_body = fold_bindings result bindings in
      
      let _loc = ghost 151 in
      <:expr<fun svals start_p end_p -> $fun_body$>>
    ) index.prods

    |> ProdArray.to_list
    |> Ast.exSem_of_list
  in

  let _loc = ghost 159 in
  <:rec_binding<reductionActionArray = [| $closures$ |]>>


let make_ml_spec_func default semtype polytype rettype kind func id =
  match func with
  | None ->
      let _loc = ghost 166 in
      <:expr<$default$ ($`int:id$)>>

  | Some { params; code } ->
      let real_rettype =
	if rettype = semtype then
	  let _loc = ghost 172 in
	  <:ctyp<SemanticValue.t>>
	else
	  rettype
      in

      let untyped_params =
	List.rev_map (fun param ->
	  let _loc, param = Sloc._loc param in
	  <:patt<($lid:"_" ^ param$ : SemanticValue.t)>>
	) params
      in

      let make_binding =
        if semtype = polytype then
          fun _loc tag ->
            <:binding<
              $lid:tag$ : $semtype$ =
                SemanticValue.obj $lid:"_" ^ tag$
            >>
        else
          fun _loc tag ->
            <:binding<
            (* explicitly state polymorphic type variable so the typing
             * stays sound even when users specify types *)
              $lid:tag$ : $semtype$ =
                (SemanticValue.obj $lid:"_" ^ tag$ : $polytype$)
            >>
      in

      let bindings =
	let _loc = ghost 186 in
	<:binding<__result : $rettype$ = $code$>>
	:: List.map (fun param ->
	  let _loc, param = Sloc._loc param in
          make_binding _loc param
	) params
      in

      let result =
	let _loc = ghost 195 in
	if real_rettype != rettype then
	  <:expr<SemanticValue.repr __result>>
	else
	  <:expr<__result>>
      in

      let fun_body = fold_bindings result bindings in

      let _loc = ghost 204 in
      List.fold_left (fun code param ->
	<:expr<fun $param$ -> $code$>>
      ) fun_body untyped_params


let array_expr_of_array array =
  let exsem =
    array
    |> Array.to_list
    |> Ast.exSem_of_list
  in

  let _loc = ghost 217 in
  <:expr<[| $exsem$ |]>>


let make_spec_func_closures variant name rettype kind base func syms =
  let _loc = ghost 222 in
  let namesModule = <:expr<$uid:Options._module_prefix () ^ "Names"$>> in
  let default = <:expr<$lid:"default_" ^ name$ $namesModule$.$lid:kind ^ "NamesArray"$>> in

  if BatArray.exists (fun sym -> func variant sym != None) syms then
    Array.mapi (fun i sym ->
      let paramtype = semtype variant (base sym) in
      let polytype = polytype (base sym) in
      let rettype = BatOption.default paramtype rettype in
      make_ml_spec_func default paramtype polytype rettype kind (func variant sym) i
    ) syms
    |> array_expr_of_array
  else
    <:expr<Array.init $`int:(Array.length syms)$ (fun i -> $default$ i)>>


let make_ml_dup_del_merge variant terms nonterms =

  let make sf_name a_name rettype kind base func syms =
    let closures = make_spec_func_closures variant sf_name rettype kind base func syms in

    assert (Classify.is_lid a_name);
    let _loc = ghost 243 in
    <:rec_binding<$lid:a_name ^ "Array"$ = $closures$>>
  in

  let make_nonterm sf_name a_name func rettype =
    make sf_name a_name rettype
      "nonterm"
      (fun nonterm -> nonterm.nbase)
      func
      (* XXX: This (and the TermArray one) break type-safety. One solution
       * would be to pass all operations down these functions. That is very
       * ugly, however, so I refrain from doing so.
       * TODO: Think about a better solution. *)
      (NtArray.to_array nonterms)
  in

  let make_term sf_name a_name func rettype =
    make sf_name a_name rettype
      "term"
      (fun term -> term.tbase)
      func
      (TermArray.to_array terms)
  in

  let _loc = ghost 267 in
  [
    (* ------------------- dup/del/merge/keep nonterminals ------------------ *)
    make_nonterm "dup"	 "duplicateNontermValue"   Semantic.dup_of_nonterm	(None);
    make_nonterm "del"	 "deallocateNontermValue"  Semantic.del_of_nonterm	(Some <:ctyp<unit>>);
    make_nonterm "show"	 "showNontermValue"        Semantic.show_of_nonterm     (Some <:ctyp<string>>);
    make_nonterm "merge" "mergeAlternativeParses"  Semantic.merge_of_nonterm	(None);
    make_nonterm "keep"	 "keepNontermValue"	   Semantic.keep_of_nonterm	(Some <:ctyp<bool>>);

    (* ------------------- dup/del/classify terminals ------------------ *)
    make_term "dup"	 "duplicateTerminalValue"  Semantic.dup_of_term		(None);
    make_term "del"	 "deallocateTerminalValue" Semantic.del_of_term		(Some <:ctyp<unit>>);
    make_term "show"	 "showTerminalValue"       Semantic.show_of_term	(Some <:ctyp<string>>);
    make_term "classify" "reclassifyToken"	   Semantic.classify_of_term	(Some <:ctyp<int>>);
  ]


let make_ml_action_code variant index final_prod verbatim =
  let _loc = ghost 282 in

  let result_type = final_semtype variant index.nonterms (ProdArray.get index.prods final_prod) in

  let closures =
    make_ml_actions variant index
    :: make_ml_dup_del_merge variant index.terms index.nonterms
    |> Ast.rbSem_of_list
  in

  let verbatims = Semantic.verbatims variant verbatim in
  let impl_verbatims = Semantic.impl_verbatims variant verbatim in

  let namesModule = <:expr<$uid:Options._module_prefix () ^ "Names"$>> in

  <:sig_item<
    (* insert the stand-alone verbatim sections *)
    $Ast.sgSem_of_list verbatims$

    (* all that goes into the interface is the result type *)
    include UserActions.S with type result = ($result_type$)
  >>,
  <:str_item<
    (* Open module so record field labels are visible *)
    open UserActions

    (* Emit verbatim sections here, so definitions from UserActions don't
     * mess up the action code and types. *)
    $Ast.stSem_of_list impl_verbatims$

    let userFunctions : UserActions.functions = { $closures$ }

    (* main action function; uses the array emitted above *)
    let reductionAction (productionId : int) (svals : SemanticValue.t array) (start_p : Lexing.position) (end_p : Lexing.position) : SemanticValue.t =
      userFunctions.reductionActionArray.(productionId) svals start_p end_p

    (* dup *)
    let duplicateNontermValue (nontermId : int) (sval : SemanticValue.t) : SemanticValue.t =
      userFunctions.duplicateNontermValueArray.(nontermId) sval
    let duplicateTerminalValue (termId : int) (sval : SemanticValue.t) : SemanticValue.t =
      userFunctions.duplicateTerminalValueArray.(termId) sval
    (* del *)
    let deallocateNontermValue (nontermId : int) (sval : SemanticValue.t) : unit =
      userFunctions.deallocateNontermValueArray.(nontermId) sval
    let deallocateTerminalValue (termId : int) (sval : SemanticValue.t) : unit =
      userFunctions.deallocateTerminalValueArray.(termId) sval
    (* show *)
    let showNontermValue (nontermId : int) (sval : SemanticValue.t) : string =
      userFunctions.showNontermValueArray.(nontermId) sval
    let showTerminalValue (termId : int) (sval : SemanticValue.t) : string =
      userFunctions.showTerminalValueArray.(termId) sval
    (* merge *)
    let mergeAlternativeParses (nontermId : int) (left : SemanticValue.t) (right : SemanticValue.t) : SemanticValue.t =
      userFunctions.mergeAlternativeParsesArray.(nontermId) left right
    (* keep *)
    let keepNontermValue (nontermId : int) (sval : SemanticValue.t) : bool =
      userFunctions.keepNontermValueArray.(nontermId) sval
    (* classify *)
    let reclassifyToken (oldTokenType : int) (sval : SemanticValue.t) : int =
      userFunctions.reclassifyTokenArray.(oldTokenType) sval
    (* emit a function to describe terminals; at some point I'd like to
     * extend my grammar format to allow the user to supply
     * token-specific description functions, but for now I will just
     * use the information easily available to synthesise one;
     * I print "sval % 100000" so I get a 5-digit number, which is
     * easy for me to compare for equality without adding much clutter
     *
     * ML: I could do something like this using Obj, but I'd rather
     * not abuse that interface unnecessarily. *)
    let terminalDescription (termId : int) (sval : SemanticValue.t) : string =
      $namesModule$.termNamesArray.(termId)
    (* and a function to describe nonterminals also *)
    let nonterminalDescription (nontermId : int) (sval : SemanticValue.t) : string =
      $namesModule$.nontermNamesArray.(nontermId)
    (* emit functions to get access to the static maps *)
    let terminalName (termId : int) : string =
      $namesModule$.termNamesArray.(termId)
    let terminalAlias (termId : int) : string =
      $namesModule$.termAliasesArray.(termId)
    let nonterminalName (nontermId : int) : string =
      $namesModule$.nontermNamesArray.(nontermId)

    type result = ($result_type$)

    (* wrap all the action stuff up as a record *)
    let userActions : result UserActions.t = {
      reductionAction;
      duplicateTerminalValue;
      duplicateNontermValue;
      deallocateTerminalValue;
      deallocateNontermValue;
      showTerminalValue;
      showNontermValue;
      mergeAlternativeParses;
      keepNontermValue;
      reclassifyToken;
      terminalDescription;
      nonterminalDescription;
      terminalName;
      terminalAlias;
      nonterminalName;
    }
  >>
