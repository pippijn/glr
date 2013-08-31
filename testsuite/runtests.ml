(* +=====~~~-------------------------------------------------------~~~=====+ *)
(* |                          Test suite description                       | *)
(* +=====~~~-------------------------------------------------------~~~=====+ *)

TestFramework.(run "testsuite" [
  { empty with
    tool = "arith.native";
    suffixes = [".c"];
    dirs = [
      "elkhound/arith/tests";
    ];
  };
  { empty with
    tool = "sless.native";
    suffixes = [".c"];
    dirs = [
      "elkhound/scannerless/tests";
    ];
  };
  { empty with
    tool = "cxxparser.native";
    suffixes = [".cc"];
    options = Some "-ptree-indent 1";
    dirs = [
      "elkhound/cxxparser/tests";
    ];
  };
])
