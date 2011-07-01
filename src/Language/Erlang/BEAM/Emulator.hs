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
                         , functionEntry  :: Label
                         , functionModule :: Module }
              deriving Show

data Module = Module { moduleFunctions :: Map (Atom, Arity) Function 
                     , moduleEntries   :: Map Label Function
                     , moduleImports   :: [MFA] }
            deriving Show

data Node = Node { nodeAtoms   :: [Atom]
                 , nodeModules :: Map Atom Module }
          deriving Show

type Stack = IOArray Int EValue
type CodePointer = [Operation]

data Process = Process { procRegs     :: IOArray Int EValue
                       , procStack    :: IORef Stack
                       , procSP       :: IORef Int 
                       , procRetStack :: IORef [CodePointer] }
               
nodeFromBEAMFile :: BEAMFile -> Node
nodeFromBEAMFile b =
  Node { nodeAtoms = beamFileAtoms b
       , nodeModules = Map.fromList [(name, moduleFromBEAMFile b)] }
    where name = beamFileAtoms b !! 0
                     
moduleFromBEAMFile :: BEAMFile -> Module
moduleFromBEAMFile b =
  let m = Module { moduleFunctions = Map.fromList fs
                 , moduleEntries =
                     Map.fromList [(functionEntry f, f) | (_, f) <- fs]
                 , moduleImports = beamFileImports b }
      fs = map (functionFromFunDef m) (beamFileFunDefs b)
  in m

functionFromFunDef :: Module -> FunDef -> ((Atom, Arity), Function)
functionFromFunDef m (FunDef name arity entry code) =
  ((name, arity), Function { functionArity = arity
                           , functionLabels = splitBlocks code
                           , functionEntry = entry 
                           , functionModule = m })
  
splitBlocks :: [Operation] -> Map Integer [Operation]
splitBlocks [] = Map.empty
splitBlocks (("label", [UOperand i]) : os) = Map.insert i os (splitBlocks os)
splitBlocks (_:os) = splitBlocks os

spawnProcess :: Node -> MFA -> [EValue] -> IO ()
spawnProcess node mfa args =
  do p <- Process <$> newArray (0, 7) (EVInteger 0)
                  <*> (newArray (0, 7) (EVInteger 0) >>= newIORef)
                  <*> newIORef 0
                  <*> newIORef [[]]
     let Just f = findMFA node mfa
     moveArgsToRegs p args
     call node p f
     result <- getReg p 0
     putStrLn $ "Return value: " ++ show result
         
findMFA :: Node -> MFA -> Maybe Function
findMFA node (MFA m f a) =
  do m' <- Map.lookup m (nodeModules node)
     Map.lookup (f, a) (moduleFunctions m')

call :: Node -> Process -> Function -> IO ()
call n p f = callByLabel n p (functionModule f) (functionEntry f)
     
moveArgsToRegs :: Process -> [EValue] -> IO ()
moveArgsToRegs p args = forM_ (zip [0..] args) (\(i, x) -> setReg p i x)
             
callByLabel :: Node -> Process -> Module -> Label -> IO ()
callByLabel node p m label =
  do let f = moduleEntries m Map.! label
     let code = functionLabels f Map.! label
     interpret node p f code
     
setReg :: Process -> Int -> EValue -> IO ()
setReg p i x = do writeArray (procRegs p) i x
                  putStrLn $ ">> x " ++ show i ++ " := " ++ show x

setStack :: Process -> Int -> EValue -> IO ()
setStack p i x = do stack <- readIORef (procStack p)
                    sp <- readIORef (procSP p)
                    writeArray stack (sp - i) x
                    putStrLn $ ">> y " ++ show sp ++ "-" ++ show i ++
                               " := " ++ show x


getReg :: Process -> Int -> IO EValue
getReg p i = readArray (procRegs p) i

getStack :: Process -> Int -> IO EValue
getStack p i = do stack <- readIORef (procStack p)
                  sp <- readIORef (procSP p)
                  readArray stack (sp - i)


interpret :: Node -> Process -> Function -> [Operation] -> IO ()
interpret _ _ _ []     = return ()
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
      do sp <- readIORef (procSP p)
         ensureStackSize p (fromIntegral (fromIntegral size + sp))
         modifyIORef (procSP p) (+ fromIntegral size)
         interpret n p f os
    ("gc_bif2", [_, _, UOperand i, a, b, dest]) ->
      do let bif = getBIF f i
         (a', b') <- (,) <$> getOperand p a <*> getOperand p b
         setOperand p dest (bif [a', b'])
         interpret n p f os
    ("move", [src, dest]) ->
      do getOperand p src >>= setOperand p dest
         interpret n p f os
    ("call", [_, FOperand label]) ->
      do modifyIORef (procRetStack p) (\rs -> os:rs)
         callByLabel n p (functionModule f) label
    ("return", []) ->
      do (pc':pcs) <- readIORef (procRetStack p)
         writeIORef (procRetStack p) pcs
         interpret n p f pc'
    ("deallocate", [UOperand m]) ->
      do modifyIORef (procSP p) (subtract (fromIntegral m))
         interpret n p f os
    _ ->
      do print "  dunno"
         interpret n p f os
         
getBIF :: Function -> Integer -> ([EValue] -> EValue)
getBIF f i = case (moduleImports (functionModule f)) !! (fromIntegral i) of
  MFA (Atom "erlang") (Atom "-") 2 ->
    \([EVInteger x, EVInteger y]) -> EVInteger (x - y)
  MFA (Atom "erlang") (Atom "*") 2 ->
    \([EVInteger x, EVInteger y]) -> EVInteger (x * y)
  mfa -> error $ "no BIF for " ++ showMFA mfa
                
ensureStackSize :: Process -> Integer -> IO ()
ensureStackSize p n =
  do stack <- readIORef (procStack p)
     size <- ((+ 1) . snd) <$> getBounds stack
     print (size, n)
     when (fromIntegral size <= n)
       (do newStack <- newArray (0, fromIntegral n * 2) (EVInteger 0)
           forM_ [0..(size - 1)]
             (\i -> readArray stack i >>= writeArray newStack i)
           writeIORef (procStack p) newStack)
           
getOperand :: Process -> Operand -> IO EValue
getOperand p o =
  case o of
    IOperand x -> return (EVInteger x)
    XOperand i -> getReg p (fromIntegral i)
    YOperand i -> getStack p (fromIntegral i)
    _          -> error $ "getOperand: " ++ show o
    
setOperand :: Process -> Operand -> EValue -> IO ()
setOperand p o v =
  case o of
    XOperand i -> setReg p (fromIntegral i) v
    YOperand i -> setStack p (fromIntegral i) v
    _          -> error $ "setOperand: " ++ show (o, v)
    
jump :: Node -> Process -> Function -> Label -> IO ()
jump n p f label = interpret n p f (functionLabels f Map.! label)

showMFA :: MFA -> String
showMFA (MFA (Atom m) (Atom f) a) = m ++ ":" ++ f ++ "/" ++ show a
