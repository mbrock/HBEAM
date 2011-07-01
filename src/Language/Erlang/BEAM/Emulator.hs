module Language.Erlang.BEAM.Emulator where

import Language.Erlang.BEAM.Loader

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Array.IO
import Data.Array.MArray

import Data.IORef

import Control.Applicative
import Control.Monad

data EValue = EVInteger Integer
            deriving (Show, Eq)

data Function = Function { functionArity  :: Integer
                         , functionLabels :: Map Integer [Operation]
                         , functionEntry  :: Integer }
              deriving Show

data Module = Module { moduleFunctions :: Map (Atom, Arity) Function 
                     , moduleImports   :: [MFA] }
            deriving Show

data Node = Node { nodeAtoms   :: [Atom]
                 , nodeModules :: Map Atom Module }
          deriving Show

data Process = Process { procRegs  :: IOArray Int EValue
                       , procStack :: IORef (IOArray Int EValue)
                       , procSP    :: IORef Int }
               
nodeFromBEAMFile :: BEAMFile -> Node
nodeFromBEAMFile b =
  Node { nodeAtoms = beamFileAtoms b
       , nodeModules = Map.fromList [(name, moduleFromBEAMFile b)] }
    where name = beamFileAtoms b !! 0
                     
moduleFromBEAMFile :: BEAMFile -> Module
moduleFromBEAMFile b =
  Module { moduleFunctions =
              Map.fromList (map functionFromFunDef (beamFileFunDefs b)) 
         , moduleImports = beamFileImports b }

functionFromFunDef :: FunDef -> ((Atom, Arity), Function)
functionFromFunDef (FunDef name arity entry code) =
  ((name, arity), Function { functionArity = arity
                           , functionLabels = splitBlocks code
                           , functionEntry = entry })
  
splitBlocks :: [Operation] -> Map Integer [Operation]
splitBlocks [] = Map.empty
splitBlocks (("label", [UOperand i]) : os) = Map.insert i os (splitBlocks os)
splitBlocks (_:os) = splitBlocks os

spawnProcess :: Node -> MFA -> [EValue] -> IO ()
spawnProcess node mfa args =
  do p <- Process <$> newArray (0, 7) (EVInteger 0)
                  <*> (newArray (0, 7) (EVInteger 0) >>= newIORef)
                  <*> newIORef 0
     let Just f = findMFA node mfa
     call node p f args
         
findMFA :: Node -> MFA -> Maybe Function
findMFA node (MFA m f a) =
  do m' <- Map.lookup m (nodeModules node)
     Map.lookup (f, a) (moduleFunctions m')

call :: Node -> Process -> Function -> [EValue] -> IO ()
call node p f args =
  do forM_ (zip [0..] args) (\(i, x) -> setReg p i x)
     let code = functionLabels f Map.! functionEntry f
     interpret node p f code
     
setReg :: Process -> Int -> EValue -> IO ()
setReg p i x = writeArray (procRegs p) i x

getReg :: Process -> Int -> IO EValue
getReg p i = readArray (procRegs p) i

interpret :: Node -> Process -> Function -> [Operation] -> IO ()
interpret _ _ _ []     = error "interpret: ran off the end"
interpret n p f (o:os) =
  do print o
     interpret1 n p f o os

interpret1 :: Node -> Process -> Function -> Operation -> [Operation] -> IO ()
interpret1 n p f o os =
  case o of
    ("is_eq_exact", [FOperand label, a, b]) ->
      do (a', b') <- (,) <$> getOperand p a <*> getOperand p b
         if a' == b'
           then interpret n p f os
           else jump n p f label
    ("allocate_zero", [UOperand size, _]) ->
      do ensureStackSize p size
         interpret n p f os
    _ ->
      do print "  dunno"
         interpret n p f os
                
ensureStackSize :: Process -> Integer -> IO ()
ensureStackSize p n =
  do stack <- readIORef (procStack p)
     size <- ((+ 1) . snd) <$> getBounds stack
     when (fromIntegral size < n)
       (do newStack <- newArray (0, fromIntegral n - 1) (EVInteger 0)
           forM_ [0..(size - 1)]
             (\i -> readArray stack i >>= writeArray newStack i)
           writeIORef (procStack p) newStack)
           
getOperand :: Process -> Operand -> IO EValue
getOperand p o =
  case o of
    IOperand x -> return (EVInteger x)
    XOperand i -> getReg p (fromIntegral i)
    _          -> error $ "unhandled operand: " ++ show o
    
jump :: Node -> Process -> Function -> Label -> IO ()
jump n p f label = interpret n p f (functionLabels f Map.! label)