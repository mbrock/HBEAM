{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Erlang.BEAM.Emulator where

import Language.Erlang.BEAM.Loader
import Language.Erlang.BEAM.Types

import           Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (ord)
import Data.List (tails)

import Data.Array.IO (IOArray)
import Data.Array.MArray (readArray, writeArray, getBounds, newArray)

import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef)

import Control.Applicative ((<$>), (<*>))

import Control.Monad (when, forM_, join)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ReaderT, runReaderT, local, asks)

import Control.Monad.STM (STM, atomically)

import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TVar
import Control.Concurrent.STM.TChan

data EValue = EVInteger Integer
            | EVAtom Atom
            | EVList [EValue]
            | EVPID PID
            deriving (Show, Eq, Read)
                     
newtype PID = PID Int
            deriving (Show, Eq, Read, Ord, Num, Real, Enum, Integral)

data Function = Function { functionArity  :: Arity
                         , functionLabels :: Map Label CodePointer
                         , functionEntry  :: Label
                         , functionModule :: Module }
              deriving Show

data Module = Module { moduleFunctions :: Map (Atom, Arity) Function 
                     , moduleEntries   :: Map Label Function
                     , moduleImports   :: [MFA] }
            deriving Show

data Node = Node { nodeAtoms   :: [Atom]
                 , nodeModules :: Map Atom Module 
                 , nodePIDs    :: TVar (Map PID Process) 
                 , nodeNextPID :: TVar PID }

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
  do pids    <- newTVarIO Map.empty
     nextPID <- newTVarIO 0
     return Node { nodeAtoms   = beamFileAtoms b
                 , nodeModules = Map.fromList [(name, moduleFromBEAMFile b)]
                 , nodePIDs    = pids 
                 , nodeNextPID = nextPID }
    where name = head (beamFileAtoms b)
                     
moduleFromBEAMFile :: BEAMFile -> Module
moduleFromBEAMFile b =
  let m = Module { moduleFunctions = Map.fromList fs
                 , moduleEntries   = es
                 , moduleImports   = beamFileImports b }
      fs = map (functionFromFunDef m) (beamFileFunDefs b)
      es = Map.fromList [(functionEntry f, f) | (_, f) <- fs]
  in m

functionFromFunDef :: Module -> FunDef -> ((Atom, Arity), Function)
functionFromFunDef m (FunDef name arity entry code) =
  ((name, arity), Function { functionArity  = arity
                           , functionLabels = splitBlocks code
                           , functionEntry  = entry 
                           , functionModule = m })
  
splitBlocks :: [Operation] -> Map Label CodePointer
splitBlocks code = foldr f Map.empty (zip code (tail (tails code)))
  where f (OpLabel i, pc) m = Map.insert i pc m
        f _               m = m

spawnProcess :: Node -> MFA -> [EValue] -> IO PID
spawnProcess n mfa args =
  do pid <- atomically $ incrementTVar (nodeNextPID n)
     p   <- newProcess pid
     atomically $ modifyTVar (nodePIDs n) (Map.insert pid p)
     let Just f = findMFA n mfa
     forkIO $ do
       result <- runReaderT (moveArgsToRegs args >> call >> getReg 0)
                   EmulationCtx { emuNode = n
                                , emuProcess = p
                                , emuFunction = f }
       putStrLn $ "PROCESS " ++ show pid ++ ": " ++ show result
     return pid
  
newProcess :: PID -> IO Process
newProcess pid = 
  Process
   <$> newArray (0, 7) (EVInteger 0)
   <*> (newArray (0, 7) (EVInteger 0) >>= newIORef)
   <*> newIORef 0
   <*> newIORef [[]]
   <*> return pid
   <*> atomically newTChan

incrementTVar :: (Num a) => TVar a -> STM a
incrementTVar v =
  do x <- readTVar v
     writeTVar v (x + 1)
     return x

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar v f = readTVar v >>= writeTVar v . f

findMFA :: Node -> MFA -> Maybe Function
findMFA node (MFA m f a) =
  do m' <- Map.lookup m (nodeModules node)
     Map.lookup (f, a) (moduleFunctions m')

call :: Emulation ()
call = asks emuFunction >>= callByLabel . functionEntry
     
moveArgsToRegs :: [EValue] -> Emulation ()
moveArgsToRegs args = mapM_ (uncurry setReg) (zip [0..] args)
             
callByLabel :: Label -> Emulation ()
callByLabel label =
  do f <- asks ((Map.! label) . moduleEntries . emuModule)
     local (\c -> c { emuFunction = f })
       (interpret (functionLabels f Map.! label))
     
setReg :: Int -> EValue -> Emulation ()
setReg i x = do asks emuProcess >>= \p -> liftIO $ writeArray (procRegs p) i x
                pid <- asks (procPID . emuProcess)
                liftIO . putStrLn $ show pid ++ " >> x " ++ show i ++
                  " := " ++ show x

getReg :: Int -> Emulation EValue
getReg i = asks emuProcess >>= \p -> liftIO $ readArray (procRegs p) i

setStack :: Int -> EValue -> Emulation ()
setStack i x = do p <- asks emuProcess
                  liftIO $ do
                    stack <- readIORef (procStack p)
                    sp <- readIORef (procSP p)
                    writeArray stack (sp - i) x
                    putStrLn $ show (procPID p) ++ " >> y " ++ show sp ++
                      "-" ++ show i ++ " := " ++ show x

getStack :: Int -> Emulation EValue
getStack i = do p <- asks emuProcess
                liftIO $ do
                  stack <- readIORef (procStack p)
                  sp <- readIORef (procSP p)
                  readArray stack (sp - i)

getSP :: Emulation Int
getSP = asks emuProcess >>= liftIO . readIORef . procSP

advanceSP :: Int -> Emulation ()
advanceSP n = asks emuProcess >>= \p -> liftIO (modifyIORef (procSP p) (+ n))

allocate :: Int -> Emulation ()
allocate size =
  do getSP >>= ensureStackSize . (+ size)
     advanceSP size

ensureStackSize :: Int -> Emulation () 
ensureStackSize n =
  do p <- asks emuProcess
     liftIO $ do
       stack <- readIORef (procStack p)
       size <- ((+ 1) . snd) <$> getBounds stack
       when (size <= n)
         (do newStack <- newArray (0, n * 2) (EVInteger 0)
             forM_ [0..(size - 1)]
               (\i -> readArray stack i >>= writeArray newStack i)
             writeIORef (procStack p) newStack)
             
pushRetStack :: CodePointer -> Emulation ()
pushRetStack cp =
  asks emuProcess >>= \p -> liftIO (modifyIORef (procRetStack p) (cp:))

popRetStack :: Emulation CodePointer
popRetStack = do p <- asks emuProcess
                 (pc':pcs) <- liftIO $ readIORef (procRetStack p)
                 liftIO $ writeIORef (procRetStack p) pcs
                 return pc'

interpret :: CodePointer -> Emulation ()
interpret []     = return ()
interpret (o:os) = do liftIO $ print o
                      interpret1 o os

jump :: Label -> Emulation ()
jump label = do f <- asks emuFunction
                interpret (functionLabels f Map.! label)

getRegistersUpTo :: Int -> Emulation [EValue]
getRegistersUpTo n = mapM (getOperand . XOperand) [0..(n-1)]

interpret1 :: Operation -> CodePointer -> Emulation ()
interpret1 o os =
  case o of
    OpLabel _ -> interpret os
    OpIsEqExact label a b ->
      do eq <- (==) <$> getOperand a <*> getOperand b
         if eq then interpret os else jump label
    OpAllocate size ->
      allocate size >> interpret os
    OpBIF0 i dest ->
      do getBIF i >>= ($ []) >>= setOperand dest
         interpret os
    OpBIF2 i a b dest ->
      do (a', b') <- (,) <$> getOperand a <*> getOperand b
         getBIF i >>= ($ [a', b']) >>= setOperand dest
         interpret os
    OpCallExt n i ->
      do args <- getRegistersUpTo n
         getImportedFunction i >>= ($ args) >>= setOperand (XOperand 0)
         interpret os
    OpCallExtOnly n i ->
      do args <- getRegistersUpTo n
         getImportedFunction i >>= ($ args) >>= setOperand (XOperand 0)
         popRetStack >>= interpret
    OpCallExtLast n i dealloc ->
      do args <- getRegistersUpTo n
         advanceSP (- (fromIntegral dealloc))
         getImportedFunction i >>= ($ args) >>= setOperand (XOperand 0)
         popRetStack >>= interpret
    OpMove src dest ->
      do getOperand src >>= setOperand dest
         interpret os
    OpJump label -> jump label
    OpCall _ label ->
      do pushRetStack os
         callByLabel label
    OpCallLast label dealloc ->
      do advanceSP (- (fromIntegral dealloc))
         jump label
    OpReturn -> popRetStack >>= interpret
    OpDeallocate m ->
      do advanceSP (- (fromIntegral m))
         interpret os
    OpSend ->
      do join $ send <$> getReg 0 <*> getReg 1
         interpret os
    OpLoopRec label dest ->
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
    OpRemoveMessage ->
      do mailbox <- asks (procMailbox . emuProcess)
         liftIO $ atomically $ readTChan mailbox
         interpret os
    OpWait label ->
      do mailbox <- asks (procMailbox . emuProcess)
         liftIO $ atomically $ do x <- readTChan mailbox
                                  unGetTChan mailbox x
         jump label
    OpPutList car cdr dest ->
      do car' <- getOperand car
         EVList cdr' <- getOperand cdr
         setOperand dest (EVList (car':cdr'))
         interpret os
    OpTestHeap -> interpret os
    _ -> fail $ "unhandled instruction: " ++ show o
         
send :: EValue -> EValue -> Emulation ()
send pid x =
  do p <- lookupPID pid
     liftIO $ atomically $ writeTChan (procMailbox p) x
     
lookupPID :: EValue -> Emulation Process
lookupPID (EVPID pid) =
  (Map.! pid) <$> (asks (nodePIDs . emuNode) >>= liftIO . readTVarIO)
lookupPID x =
  fail $ "lookupPID: " ++ show x ++ " is not a PID"
         
getImportedFunction :: Index -> Emulation ([EValue] -> Emulation EValue)
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

getBIF :: Index -> Emulation ([EValue] -> Emulation EValue)
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
    
showMFA :: MFA -> String
showMFA (MFA (Atom m) (Atom f) a) = m ++ ":" ++ f ++ "/" ++ show a
