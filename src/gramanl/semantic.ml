open GrammarType


let is_verbatim : global_semantic -> CamlAst.sig_item list option = function
  | `SEM_VERBATIM code ->
      Some code
  | _ ->
      None

let verbatims variant semantic =
  BatOption.default []
    (SemanticVariant.find variant is_verbatim semantic)


let is_impl_verbatim : global_semantic -> CamlAst.str_item list option = function
  | `SEM_IMPL_VERBATIM code ->
      Some code
  | _ ->
      None

let impl_verbatims variant semantic =
  BatOption.default []
    (SemanticVariant.find variant is_impl_verbatim semantic)


let is_action : prod_semantic -> CamlAst.expr option = function
  | `SEM_ACTION expr ->
      Some expr

let action_of_prod variant prod =
  SemanticVariant.find variant is_action prod.pbase.semantic

let replace_action variant prod action =
  { prod with
    pbase = { prod.pbase with
      semantic = SemanticVariant.replace variant
        (fun sem -> is_action sem != None)
        (`SEM_ACTION action)
        prod.pbase.semantic;
    }
  }


let is_merge : nonterm_semantic -> spec_func option = function
  | `SEM_MERGE func ->
      Some func
  | _ ->
      None

let merge_of_nonterm variant nonterm =
  SemanticVariant.find variant is_merge nonterm.nbase.semantic


let is_keep : nonterm_semantic -> spec_func option = function
  | `SEM_KEEP func ->
      Some func
  | _ ->
      None

let keep_of_nonterm variant nonterm =
  SemanticVariant.find variant is_keep nonterm.nbase.semantic


let is_classify : term_semantic -> spec_func option = function
  | `SEM_CLASSIFY func ->
      Some func
  | _ ->
      None

let classify_of_term variant term =
  SemanticVariant.find variant is_classify term.tbase.semantic


let is_dup : 'semantic -> spec_func option = function
  | `SEM_DUP func ->
      Some func
  | _ ->
      None

let dup_of_symbol variant sym =
  SemanticVariant.find variant is_dup sym.semantic

let dup_of_nonterm variant nonterm = dup_of_symbol variant nonterm.nbase
let dup_of_term variant term = dup_of_symbol variant term.tbase


let is_del : 'semantic -> spec_func option = function
  | `SEM_DEL func ->
      Some func
  | _ ->
      None

let del_of_symbol variant sym =
  SemanticVariant.find variant is_del sym.semantic

let del_of_nonterm variant nonterm = del_of_symbol variant nonterm.nbase
let del_of_term variant term = del_of_symbol variant term.tbase


let is_show : 'semantic -> spec_func option = function
  | `SEM_SHOW func ->
      Some func
  | _ ->
      None

let show_of_symbol variant sym =
  SemanticVariant.find variant is_show sym.semantic

let show_of_nonterm variant nonterm = show_of_symbol variant nonterm.nbase
let show_of_term variant term = show_of_symbol variant term.tbase


let is_semtype : 'semantic -> CamlAst.ctyp option = function
  | `SEM_TYPE ctyp ->
      Some ctyp
  | _ ->
      None

let semtype_of_symbol variant sym =
  SemanticVariant.find variant is_semtype sym.semantic

let semtype_of_nonterm variant nonterm = semtype_of_symbol variant nonterm.nbase
let semtype_of_term variant term = semtype_of_symbol variant term.tbase
