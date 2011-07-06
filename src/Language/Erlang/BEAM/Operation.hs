module Language.Erlang.BEAM.Operation where 

import Language.Erlang.BEAM.Types

makeOperation :: String -> [Operand] -> Operation
makeOperation name args =
  case (name, args) of
    ("func_info", [AOperand a, AOperand b, UOperand c]) ->
      OpFuncInfo a b c
    ("int_code_end", []) -> OpIntCodeEnd
    ("label", [UOperand a]) -> OpLabel a
    ("is_eq_exact", [FOperand a, b, c]) -> OpIsEqExact a b c
    ("is_tuple", [FOperand a, b]) -> OpIsTuple a b
    ("init", [a]) -> OpInit a
    ("allocate", [UOperand a, _]) -> OpAllocate a
    ("allocate_zero", [UOperand a, _]) -> OpAllocate a
    
    -- ignore heap allocation but do the stack allocation
    ("allocate_heap_zero", [UOperand a, _, _]) -> OpAllocate a
    
    ("bif0", [UOperand a, b]) -> OpBIF0 a b
    ("gc_bif2", [_, _, UOperand a, b, c, d]) -> OpBIF2 a b c d
    ("get_tuple_element", [a, UOperand b, c]) -> OpGetTupleElement a b c
    ("call", [UOperand a, FOperand b]) -> OpCall a b
    ("call_ext", [UOperand a, UOperand b]) -> OpCallExt a b
    ("call_ext_only", [UOperand a, UOperand b]) -> OpCallExtOnly a b
    ("call_ext_last", [UOperand a, UOperand b, UOperand c]) ->
      OpCallExtLast a b c
    ("move", [a, b]) -> OpMove a b
    ("jump", [FOperand a]) -> OpJump a
    ("call_last", [_, FOperand a, UOperand b]) -> OpCallLast a b
    ("return", []) -> OpReturn
    ("deallocate", [UOperand a]) -> OpDeallocate a
    ("send", []) -> OpSend
    ("loop_rec", [FOperand a, b]) -> OpLoopRec a b
    ("loop_rec_end", [FOperand a]) -> OpLoopRecEnd a
    ("remove_message", []) -> OpRemoveMessage
    ("wait", [FOperand a]) -> OpWait a
    ("put_tuple", [UOperand a, b]) -> OpPutTuple a b
    ("put", [a]) -> OpPut a
    ("put_list", [a, b, c]) -> OpPutList a b c
    ("test_heap", _) -> OpTestHeap
    ("test_arity", [FOperand a, b, UOperand c]) -> OpTestArity a b c
    _ -> OpUnknown name args
    