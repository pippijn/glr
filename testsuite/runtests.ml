(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  {
    tool = "arith.native";
    suffixes = [".c"];
    options = None;
    dirs = [
      "elkhound/arith/tests";
    ];
  };
  {
    tool = "sless.native";
    suffixes = [".c"];
    options = None;
    dirs = [
      "elkhound/scannerless/tests";
    ];
  };
])
