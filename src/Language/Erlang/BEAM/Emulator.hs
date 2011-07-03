module Language.Erlang.BEAM.Emulator where

import Language.Erlang.BEAM.Loader

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Array.IO
import Data.Array.MArray

import Data.IORef

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

data EValue = EVInteger Integer
            deriving (Show, Eq, Read)

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
               
data EmulationCtx = EmulationCtx { emuNode     :: Node
                                 , emuProcess  :: Process
                                 , emuFunction :: Function }
               
type Emulation a = ReaderT EmulationCtx IO a
               
emuModule :: EmulationCtx -> Module
emuModule = functionModule . emuFunction

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
     result <- runReaderT (moveArgsToRegs args >> call >> getReg 0)
       (EmulationCtx { emuNode = node
                     , emuProcess = p
                     , emuFunction = f })
     putStrLn $ "Return value: " ++ show result
         
findMFA :: Node -> MFA -> Maybe Function
findMFA node (MFA m f a) =
  do m' <- Map.lookup m (nodeModules node)
     Map.lookup (f, a) (moduleFunctions m')

call :: Emulation ()
call = do f <- asks emuFunction
          callByLabel (functionEntry f)
     
moveArgsToRegs :: [EValue] -> Emulation ()
moveArgsToRegs args = forM_ (zip [0..] args) (\(i, x) -> setReg i x)
             
callByLabel :: Label -> Emulation ()
callByLabel label =
  do m <- asks emuModule
     let f    = moduleEntries m Map.! label
         code = functionLabels f Map.! label
     local (\c -> c { emuFunction = f }) (interpret code)
     
setReg :: Int -> EValue -> Emulation ()
setReg i x = do asks emuProcess >>= \p -> liftIO $ writeArray (procRegs p) i x
                liftIO . putStrLn $ ">> x " ++ show i ++ " := " ++ show x

setStack :: Int -> EValue -> Emulation ()
setStack i x = do p <- asks emuProcess
                  liftIO $ do
                    stack <- readIORef (procStack p)
                    sp <- readIORef (procSP p)
                    writeArray stack (sp - i) x
                    putStrLn $ ">> y " ++ show sp ++ "-" ++ show i ++
                      " := " ++ show x

getReg :: Int -> Emulation EValue
getReg i = asks emuProcess >>= \p -> liftIO $ readArray (procRegs p) i

getStack :: Int -> Emulation EValue
getStack i = do p <- asks emuProcess
                liftIO $ do
                  stack <- readIORef (procStack p)
                  sp <- readIORef (procSP p)
                  readArray stack (sp - i)

interpret :: [Operation] -> Emulation ()
interpret []     = return ()
interpret (o:os) =
  do liftIO $ print o
     interpret1 o os

getSP :: Emulation Int
getSP = asks emuProcess >>= liftIO . readIORef . procSP

advanceSP :: Int -> Emulation ()
advanceSP n = asks emuProcess >>= \p -> liftIO (modifyIORef (procSP p) (+ n))

pushRetStack :: CodePointer -> Emulation ()
pushRetStack cp =
  asks emuProcess >>= \p -> liftIO (modifyIORef (procRetStack p) (cp:))

popRetStack :: Emulation CodePointer
popRetStack = do p <- asks emuProcess
                 (pc':pcs) <- liftIO $ readIORef (procRetStack p)
                 liftIO $ writeIORef (procRetStack p) pcs
                 return pc'

interpret1 :: Operation -> [Operation] -> Emulation ()
interpret1 o os =
  case o of
    ("is_eq_exact", [FOperand label, a, b]) ->
      do (a', b') <- (,) <$> getOperand a <*> getOperand b
         if a' == b'
           then interpret os
           else jump label
    ("allocate_zero", [UOperand size, _]) ->
      do sp <- getSP
         ensureStackSize (fromIntegral (fromIntegral size + sp))
         advanceSP (fromIntegral size)
         interpret os
    ("gc_bif2", [_, _, UOperand i, a, b, dest]) ->
      do bif <- getBIF i
         (a', b') <- (,) <$> getOperand a <*> getOperand b
         setOperand dest (bif [a', b'])
         interpret os
    ("move", [src, dest]) ->
      do getOperand src >>= setOperand dest
         interpret os
    ("call", [_, FOperand label]) ->
      do pushRetStack os
         callByLabel label
    ("return", []) ->
      do pc' <- popRetStack
         interpret pc'
    ("deallocate", [UOperand m]) ->
      do advanceSP (- (fromIntegral m))
         interpret os
    _ ->
      do liftIO $ print "  dunno"
         interpret os
         
getBIF :: Integer -> Emulation ([EValue] -> EValue)
getBIF i =
  do f <- asks emuFunction
     case (moduleImports (functionModule f)) !! (fromIntegral i) of
       MFA (Atom "erlang") (Atom "-") 2 ->
         return $ \([EVInteger x, EVInteger y]) -> EVInteger (x - y)
       MFA (Atom "erlang") (Atom "*") 2 ->
         return $ \([EVInteger x, EVInteger y]) -> EVInteger (x * y)
       mfa -> fail $ "no BIF for " ++ showMFA mfa
                
ensureStackSize :: Integer -> Emulation () 
ensureStackSize n =
  do p <- asks emuProcess
     liftIO $ do
       stack <- readIORef (procStack p)
       size <- ((+ 1) . snd) <$> getBounds stack
       when (fromIntegral size <= n)
         (do newStack <- newArray (0, fromIntegral n * 2) (EVInteger 0)
             forM_ [0..(size - 1)]
               (\i -> readArray stack i >>= writeArray newStack i)
             writeIORef (procStack p) newStack)
             
getOperand :: Operand -> Emulation EValue
getOperand o =
  case o of
    IOperand x -> return (EVInteger x)
    XOperand i -> getReg (fromIntegral i)
    YOperand i -> getStack (fromIntegral i)
    _          -> fail $ "getOperand: " ++ show o
    
setOperand :: Operand -> EValue -> Emulation ()
setOperand o v =
  case o of
    XOperand i -> setReg (fromIntegral i) v
    YOperand i -> setStack (fromIntegral i) v
    _          -> fail $ "setOperand: " ++ show (o, v)
    
jump :: Label -> Emulation ()
jump label = do f <- asks emuFunction
                interpret (functionLabels f Map.! label)

showMFA :: MFA -> String
showMFA (MFA (Atom m) (Atom f) a) = m ++ ":" ++ f ++ "/" ++ show a
