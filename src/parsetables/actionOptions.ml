let _dup_clone		= ref false


let () =
  Cmdline.register "user actions" (Arg.([
    "-dup-clone",		Set _dup_clone,			" make default dup function perform a deep copy";
  ]))


let _dup_clone		() = !_dup_clone
