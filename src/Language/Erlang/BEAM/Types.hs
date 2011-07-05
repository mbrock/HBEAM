{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Erlang.BEAM.Types where

newtype Atom = Atom String deriving (Show, Read, Ord, Eq)
                      
type Index = Int
type Arity = Int
type Label = Int
                                      
data External  = ExtInteger Integer
               | ExtTuple   [External]
               | ExtAtom    String
               | ExtString  String
               | ExtList    [External]
               deriving Show

data MFA    = MFA    Atom Atom  Arity             deriving Show
data Export = Export Atom Arity Label             deriving Show
data FunDef = FunDef Atom Arity Label [Operation] deriving Show
      
data EValue = EVInteger Integer
            | EVAtom Atom
            | EVList [EValue]
            | EVPID PID
            deriving (Show, Eq, Read)
                     
newtype PID = PID Int
            deriving (Show, Eq, Read, Ord, Num, Real, Enum, Integral)

data OperandTag = TagA | TagF | TagH | TagI | TagU
                | TagX | TagY | TagZ
                | TagFR | TagAtom | TagFloat | TagLiteral
                deriving Show

data Operand = IOperand Integer
             | UOperand Int
             | XOperand Int
             | YOperand Int
             | FOperand Int
             | AOperand Atom
             | LOperand External
             deriving Show

data Operation =
    OpAllocate Int
  | OpBIF0 Index Operand
  | OpBIF2 Index Operand Operand Operand
  | OpCall Int Label
  | OpCallExt Arity Index
  | OpCallExtOnly Arity Index
  | OpCallExtLast Arity Index Int
  | OpCallLast Label Int
  | OpDeallocate Index
  | OpFuncInfo Atom Atom Arity
  | OpIntCodeEnd
  | OpIsEqExact Label Operand Operand
  | OpJump Label
  | OpLabel Label
  | OpLoopRec Label Operand
  | OpMove Operand Operand
  | OpPutList Operand Operand Operand
  | OpRemoveMessage
  | OpReturn
  | OpSend
  | OpTestHeap
  | OpUnknown String [Operand]
  | OpWait Label
  deriving Show
