(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "arith";
    suffixes = [".c"];
    options = None;
    dirs = [
      "elkhound/arith/tests";
    ];
  };
  {
    tool = "sless";
    suffixes = [".c"];
    options = None;
    dirs = [
      "elkhound/scannerless/tests";
    ];
  };
])
