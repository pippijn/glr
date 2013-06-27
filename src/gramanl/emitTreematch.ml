open PtreeType


let output_tycon_arg s = function
  | Alias semtype ->
      s (Sloc.value semtype)
  | _ ->
      failwith "unsupported in treematch"


let output_tycon s (name, args) =
  s (Sloc.value name);
  List.iter (fun arg ->
    s " ";
    output_tycon_arg s arg
  ) args


let output_binding s (left, right) =
  s (Sloc.value left);
  s ": ";
  match right with
  | Native ctyp ->
      s (CamlAst.string_of_ctyp ctyp)
  | Alias semtype ->
      s "=";
      s (Sloc.value semtype)
  | List semtype ->
      s "=[";
      s (Sloc.value semtype);
      s "]"
  | Option semtype ->
      s "=?";
      s (Sloc.value semtype)
  | Tycon types ->
      List.iter (fun tycon ->
        s "\n\t| ";
        output_tycon s tycon
      ) types


let make_ml_treematch ptree =
  let buf = Buffer.create (1024 * 1024) in
  let s = Buffer.add_string buf in

  s "ast Ptree {";
  List.iter (fun binding ->
    s "\n";
    output_binding s binding
  ) ptree;
  s "\n";
  s "}\n";
  s "map identity_default_map : Ptree => Ptree { }\n";

  Buffer.contents buf
