open CorePervasives
open Camlp4.PreCast
open PtreeType

let ghost = Sloc.ghost "emitPtree"


let make_tycon_arg = function
  | Alias semtype ->
      let _loc, semtype = Sloc._loc semtype in
      <:ctyp<$uid:semtype$.t>>
  | _ ->
      failwith "unsupported in ptree"


let make_tycon (name, args) =
  let types = Ast.tyAnd_of_list (List.map make_tycon_arg args) in
  let _loc, name = Sloc._loc name in
  <:ctyp<$uid:name$ of $types$>>


let make_binding (left, right) =
  let intf_types, impl_types =
    match right with
    | Native ctyp ->
        let _loc = ghost 26 in
        <:sig_item<type t = ($ctyp$)>>,
        <:str_item<type t = ($ctyp$)>>
    | Alias semtype ->
        let _loc, semtype = Sloc._loc semtype in
        <:sig_item<type t = $uid:semtype$.t>>,
        <:str_item<type t = $uid:semtype$.t>>
    | List semtype ->
        let _loc, semtype = Sloc._loc semtype in
        <:sig_item<type t = $uid:semtype$.t list>>,
        <:str_item<type t = $uid:semtype$.t list>>
    | Option semtype ->
        let _loc, semtype = Sloc._loc semtype in
        <:sig_item<type t = $uid:semtype$.t option>>,
        <:str_item<type t = $uid:semtype$.t option>>
    | Tycon types ->
        let types = Ast.tyOr_of_list (List.map make_tycon types) in

        (* TODO: with sexp *)
        let _loc = ghost 45 in
        <:sig_item<type t = $types$ | SEXP>>,
        <:str_item<type t = $types$ | SEXP>>
  in

  let _loc, left = Sloc._loc left in

  (* signature *)
  let intf =
    <:module_type<
      sig
        $intf_types$
        include Sig.ConvertibleType with type t := t
      end
    >>
  in

  (* implementation *)
  let impl =
    <:module_expr<
      struct
        $impl_types$
      end
    >>
  in

  <:module_binding<$left$ : $intf$ = $impl$>>


let make_ml_parse_tree ptree =
  let bindings = List.map make_binding ptree in

  let _loc = ghost 77 in

  let combined =
    BatList.reduce (fun combined binding ->
      <:module_binding<$combined$ and $binding$>>
    ) bindings
  in

  let modules = Ast.StRecMod (_loc, combined) in

  let first_module, _ = List.hd ptree in

  let impl =
    let _loc, first_module = Sloc._loc first_module in
    <:str_item<
      open Sexplib.Conv
      open Glr

      module Ptree = struct
        $modules$

        type t = $uid:first_module$.t
        let t_of_sexp = $uid:first_module$.t_of_sexp
        let sexp_of_t = $uid:first_module$.sexp_of_t
      end
    >>
  in

  impl
