{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Erlang.BEAM.Emulator where

import Language.Erlang.BEAM.Loader

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (ord)

import Data.Array.IO
import Data.Array.MArray

import Data.IORef

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader

import Control.Monad.STM
import Control.Concurrent
import Control.Concurrent.STM.TChan

data EValue = EVInteger Integer
            | EVAtom Atom
            | EVList [EValue]
            | EVPID PID
            deriving (Show, Eq, Read)
                     
newtype PID = PID Int
            deriving (Show, Eq, Read, Ord, Num)

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
                 , nodeModules :: Map Atom Module 
                 , nodePIDs    :: IORef (Map PID Process) 
                 , nodeNextPID :: IORef PID }

type Stack = IOArray Int EValue
type CodePointer = [Operation]

data Process = Process { procRegs     :: IOArray Int EValue
                       , procStack    :: IORef Stack
                       , procSP       :: IORef Int 
                       , procRetStack :: IORef [CodePointer] 
                       , procPID      :: PID 
                       , procMailbox  :: TChan EValue }
               
data EmulationCtx = EmulationCtx { emuNode     :: Node
                                 , emuProcess  :: Process
                                 , emuFunction :: Function }
               
type Emulation a = ReaderT EmulationCtx IO a
               
emuModule :: EmulationCtx -> Module
emuModule = functionModule . emuFunction

nodeFromBEAMFile :: BEAMFile -> IO Node
nodeFromBEAMFile b =
  do pids <- newIORef Map.empty
     nextPID <- newIORef 0
     return Node { nodeAtoms = beamFileAtoms b
                 , nodeModules = Map.fromList [(name, moduleFromBEAMFile b)]
                 , nodePIDs = pids 
                 , nodeNextPID = nextPID }
    where name = head (beamFileAtoms b)
                     
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

spawnProcess :: Node -> MFA -> [EValue] -> IO PID
spawnProcess n mfa args =
  do pid <- readIORef (nodeNextPID n)
     modifyIORef (nodeNextPID n) (+ 1)
     p <- Process <$> newArray (0, 7) (EVInteger 0)
                  <*> (newArray (0, 7) (EVInteger 0) >>= newIORef)
                  <*> newIORef 0
                  <*> newIORef [[]]
                  <*> return pid
                  <*> atomically newTChan
     modifyIORef (nodePIDs n) (Map.insert pid p)
     let Just f = findMFA n mfa
     forkIO $ do
       result <- runReaderT (moveArgsToRegs args >> call >> getReg 0)
                   EmulationCtx { emuNode = n
                                , emuProcess = p
                                , emuFunction = f }
       putStrLn $ "PROCESS " ++ show pid ++ ": " ++ show result
     return pid
         
findMFA :: Node -> MFA -> Maybe Function
findMFA node (MFA m f a) =
  do m' <- Map.lookup m (nodeModules node)
     Map.lookup (f, a) (moduleFunctions m')

call :: Emulation ()
call = do f <- asks emuFunction
          callByLabel (functionEntry f)
     
moveArgsToRegs :: [EValue] -> Emulation ()
moveArgsToRegs args = forM_ (zip [0..] args) (uncurry setReg)
             
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
    ("label", _) -> interpret os
    ("is_eq_exact", [FOperand label, a, b]) ->
      do (a', b') <- (,) <$> getOperand a <*> getOperand b
         if a' == b'
           then interpret os
           else jump label
    ("allocate",      [UOperand size, _]) -> allocate size >> interpret os
    ("allocate_zero", [UOperand size, _]) -> allocate size >> interpret os
    ("bif0", [UOperand i, dest]) ->
      do bif <- getBIF i
         bif [] >>= setOperand dest
         liftIO $ putStrLn "ran bif0"
         interpret os
    ("gc_bif2", [_, _, UOperand i, a, b, dest]) ->
      do bif <- getBIF i
         (a', b') <- (,) <$> getOperand a <*> getOperand b
         bif [a', b'] >>= setOperand dest
         interpret os
    ("call_ext", [UOperand n, UOperand i]) ->
      do ext <- getImportedFunction i
         args <- mapM (getOperand . XOperand) [0..(n-1)]
         ext args >>= setOperand (XOperand 0)
         interpret os
    ("move", [src, dest]) ->
      do getOperand src >>= setOperand dest
         interpret os
    ("jump", [FOperand label]) -> jump label
    ("call", [_, FOperand label]) ->
      do pushRetStack os
         callByLabel label
    ("call_last", [_, FOperand label, UOperand dealloc]) ->
      do advanceSP (- (fromIntegral dealloc))
         jump label
    ("return", []) ->
      do pc' <- popRetStack
         interpret pc'
    ("deallocate", [UOperand m]) ->
      do advanceSP (- (fromIntegral m))
         interpret os
    ("send", []) ->
      do EVPID pid <- getReg 0
         v <- getReg 1
         send pid v
         interpret os
    ("loop_rec", [FOperand label, dest]) ->
      do mailbox <- asks (procMailbox . emuProcess)
         noMail <- liftIO $ atomically (isEmptyTChan mailbox)
         if noMail
           then jump label
           else do x <- liftIO $ atomically $ 
                          do x' <- readTChan mailbox
                             unGetTChan mailbox x'
                             return x'
                   setOperand dest x
                   interpret os
    ("remove_message", []) ->
      do mailbox <- asks (procMailbox . emuProcess)
         liftIO $ atomically $ readTChan mailbox
         interpret os
    ("wait", [FOperand label]) ->
      do mailbox <- asks (procMailbox . emuProcess)
         liftIO $ atomically $ do x <- readTChan mailbox
                                  unGetTChan mailbox x
         jump label
    ("put_list", [car, cdr, dest]) ->
      do car' <- getOperand car
         EVList cdr' <- getOperand cdr
         setOperand dest (EVList (car':cdr'))
         interpret os
    ("test_heap", _) -> interpret os
    _ -> fail $ "unhandled instruction: " ++ show o
         
send :: PID -> EValue -> Emulation ()
send pid x =
  do p <- lookupPID pid
     liftIO $ atomically $ writeTChan (procMailbox p) x
     
lookupPID :: PID -> Emulation Process
lookupPID pid =
  do pids <- asks (nodePIDs . emuNode) >>= liftIO . readIORef
     return (pids Map.! pid)
         
allocate :: Integer -> Emulation ()
allocate size =
  do sp <- getSP
     ensureStackSize (fromIntegral (fromIntegral size + sp))
     advanceSP (fromIntegral size)

getImportedFunction :: Integer -> Emulation ([EValue] -> Emulation EValue)
getImportedFunction i =
  do thisModule <- asks emuModule
     case moduleImports thisModule !! fromIntegral i of
       MFA (Atom "erlang") (Atom "spawn") 3 ->
         return $ \[EVAtom m, EVAtom f, EVList args] ->
                    do n <- asks emuNode
                       let mfa = MFA m f (fromIntegral (length args))
                       pid <- liftIO $ spawnProcess n mfa args
                       return (EVPID pid)
       MFA (Atom "hbeam") (Atom "display") _ ->
         return $ \xs -> do liftIO $ putStrLn ("!! " ++ show xs)
                            return (EVInteger 0)
       mfa -> fail $ "no such imported function " ++ showMFA mfa

getBIF :: Integer -> Emulation ([EValue] -> Emulation EValue)
getBIF i =
  do f <- asks emuFunction
     p <- asks emuProcess
     case moduleImports (functionModule f) !! fromIntegral i of
       MFA (Atom "erlang") (Atom "-") 2 ->
         return $ \([EVInteger x, EVInteger y]) -> return (EVInteger (x - y))
       MFA (Atom "erlang") (Atom "+") 2 ->
         return $ \([EVInteger x, EVInteger y]) -> return (EVInteger (x + y))
       MFA (Atom "erlang") (Atom "*") 2 ->
         return $ \([EVInteger x, EVInteger y]) -> return (EVInteger (x * y))
       MFA (Atom "erlang") (Atom "self") 0 ->
         return $ \[] -> return $ EVPID (procPID p)
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
    AOperand a -> return (EVAtom a)
    LOperand (ExtString s) ->
      return (EVList (map (EVInteger . fromIntegral . ord) s))
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
