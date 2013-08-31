(* lrparse.ml *)
(* deterministic LALR(1) parser *)

open Lexerint       (* tLexerInterface *)
open ParseTables    (* actionTable, etc. *)


type tStateId = int

let debug = false

let stateStack = ref (Array.make 10 0)
let svalStack = ref (Array.make 10 (Obj.repr 0))
let stackLen = ref 0

let pushStateSval state sval =
begin
  if ((Array.length !stateStack) = !stackLen) then (
    (* must make it bigger *)
    let newStateStack = (Array.make (!stackLen * 2) 0) in
    let newSvalStack = (Array.make (!stackLen * 2) (Obj.repr 0)) in

    (* copy *)
    (Array.blit
      !stateStack           (* source array *)
      0                     (* source start position *)
      newStateStack         (* dest array *)
      0                     (* dest start position *)
      !stackLen             (* number of elements to copy *)
    );
    (Array.blit
      !svalStack            (* source array *)
      0                     (* source start position *)
      newSvalStack          (* dest array *)
      0                     (* dest start position *)
      !stackLen             (* number of elements to copy *)
    );

    (* switch from old to new *)
    stateStack := newStateStack;
    svalStack := newSvalStack;
  );

  (* put new element into the stack at the end *)
  (!stateStack).(!stackLen) <- state;
  (!svalStack).(!stackLen) <- sval;
  (incr stackLen);
end

let topState() =
begin
  (!stateStack).(!stackLen - 1)
end

(*
let parse lex tables actions =
begin
  (* initial state *)
  (pushStateSval 0 (Obj.repr 0));

  (* loop over all tokens until EOF and stack has just start symbol *)
  while (not (Lexerint.(lex.index #getTokType()) = 0)) ||
        (!stackLen > 2) do
    let tt = (lex#getTokType()) in        (* token type *)
    let state = (topState()) in      (* current state *)

    (*
    if (debug) then (
      (Printf.printf "state=%d tokType=%d sval=%d desc=\"%s\"\n"
                     state
                     tt
                     (lex#getIntSval())
                     (lex#tokenDesc())
                   );
      (flush stdout);
    );
    *)

    (* read from action table *)
    let act = (getActionEntry tables state tt) in

    (* shift? *)
    if (isShiftAction tables act) then (
      let dest = decodeShift tables act tt in   (* destination state *)
      pushStateSval dest (Lexerint.(lex.token ()));

      (* next token *)
      (Lexerint.(lex.token ()));
             
      if (debug) then (
        (Printf.printf "shift to state %d\n" dest);
        (flush stdout);
      );
    )

    (* reduce? *)
    else if (isReduceAction tables act) then (
      let rule = (decodeReduce tables act state) in    (* reduction rule *)
      let ruleLen = (getProdInfo_rhsLen tables rule) in
      let lhs = (getProdInfo_lhsIndex tables rule) in

      (* make an array of semantic values for the action rule; this does
       * an extra copy if we're already using a linear stack, but will
       * be needed for GLR so I'll do it this way *)
      let svalArray : Obj.t array = (Array.make ruleLen (Obj.repr 0)) in
      (Array.blit
        !svalStack            (* source array *)
        (!stackLen - ruleLen) (* source start position *)
        svalArray             (* dest array *)
        0                     (* dest start position *)
        ruleLen               (* number of elements to copy *)
      );

      (* invoke user's reduction action *)
      let sval = (actions.reductionAction rule svalArray) in

      (* pop 'ruleLen' elements *)
      stackLen := (!stackLen - ruleLen);
      let newTopState = (topState()) in

      (* get new state *)
      let dest = (decodeGoto (getGotoEntry tables newTopState lhs) lhs) in
      (pushStateSval dest sval);

      if (debug) then (
        (Printf.printf "reduce by rule %d (len=%d, lhs=%d), goto state %d\n"
                       rule ruleLen lhs dest);
        (flush stdout);
      );
    )

    (* error? *)
    else if (isErrorAction act) then (
      (Printf.printf "parse error in state %d\n" state);
      (flush stdout);
      (failwith "parse error");
    )

    (* bad code? *)
    else (
      (failwith "bad action code");
    );
  done;

  (* print final parse stack *)
  if (debug) then (
    (Printf.printf "final parse stack (up is top):\n");
    let i ref = ref (pred !stackLen) in
    while (!i >= 0) do
      (Printf.printf "  %d\n" (!stateStack).(!i));
      (decr i);
    done;
    (flush stdout);
  );

  (* return value: sval of top element *)
  let topSval = (!svalStack).(!stackLen - 1) in

  topSval
end


(* EOF *)
*)
