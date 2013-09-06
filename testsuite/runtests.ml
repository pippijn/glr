(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  { empty with
    tool = "arith.native";
    suffixes = [".c"];
    dirs = [
      "arith/tests";
    ];
  };
  { empty with
    tool = "sless.native";
    suffixes = [".c"];
    dirs = [
      "scannerless/tests";
    ];
  };
  { empty with
    tool = "cxxparser.native";
    suffixes = [".cc"];
    dirs = [
      "cxxparser/tests";
    ];
  };
])
