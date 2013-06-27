let report_unexpected value expected desc =
  if expected = -1 && value > 0 || expected <> -1 && expected <> value then (
    Printf.printf "%d %s" value desc;
    if expected <> -1 then (
      Printf.printf " (expected %d)" expected
    );
    print_newline ()
  )
