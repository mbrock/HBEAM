module Language.Erlang.BEAM.Emulator where

import Language.Erlang.BEAM.Loader
import Language.Erlang.BEAM.Types
import Language.Erlang.BEAM.Utils
import Language.Erlang.BEAM.Mailbox

import           Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (ord, chr)
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


--- Data types relevant to the emulator.

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
                       , procRetStack :: IORef [(Function, CodePointer)]
                       , procPID      :: PID 
                       , procMailbox  :: Mailbox }
               
data EmulationCtx =
  EmulationCtx { emuNode     :: Node
               , emuProcess  :: Process
               , emuFunction :: Function 
               , emuTuple    :: (Arity, [EValue], Operand) }
               
type Emulation a = ReaderT EmulationCtx IO a

liftSTM :: STM a -> Emulation a
liftSTM = liftIO . atomically

--- Emulation state data stuff.

findMFA :: Node -> MFA -> Maybe Function
findMFA node (MFA m f a) =
  do m' <- Map.lookup m (nodeModules node)
     Map.lookup (f, a) (moduleFunctions m')
     
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
  
-- Scan a function block for labels.
splitBlocks :: [Operation] -> Map Label CodePointer
splitBlocks code = foldr f Map.empty (zip code (tail (tails code)))
  where f (OpLabel i, pc) m = Map.insert i pc m
        f _               m = m


--- Processes, PIDs, and messages.

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
                                , emuFunction = f 
                                , emuTuple = undefined }
       putStrLn $ "PROCESS " ++ show pid ++ ": " ++ show result
     return pid
  
lookupPID :: EValue -> Emulation Process
lookupPID (EVPID pid) =
  (Map.! pid) <$> (asks (nodePIDs . emuNode) >>= liftIO . readTVarIO)
lookupPID x =
  fail $ "lookupPID: " ++ show x ++ " is not a PID"

newProcess :: PID -> IO Process
newProcess pid = 
  Process
   <$> newArray (0, 7) (EVInteger 0)
   <*> (newArray (0, 7) (EVInteger 0) >>= newIORef)
   <*> newIORef 0
   <*> newIORef []
   <*> return pid
   <*> atomically newMailbox
   
send :: EValue -> EValue -> Emulation ()
send pid x =
  do p <- lookupPID pid
     liftSTM $ deliverMessage (procMailbox p) x

currentMailbox :: Emulation Mailbox
currentMailbox = asks (procMailbox . emuProcess)
     

--- Registers and stack.

setReg :: Int -> EValue -> Emulation ()
setReg i x = do asks emuProcess >>= \p -> liftIO $ writeArray (procRegs p) i x
                pid <- asks (procPID . emuProcess)
                liftIO . putStrLn $ show pid ++ " >> x " ++ show i ++
                  " := " ++ show x

getReg :: Int -> Emulation EValue
getReg i = asks emuProcess >>= \p -> liftIO $ readArray (procRegs p) i

getRegistersUpTo :: Int -> Emulation [EValue]
getRegistersUpTo n = mapM (getOperand . XOperand) [0..(n-1)]

moveArgsToRegs :: [EValue] -> Emulation ()
moveArgsToRegs args = mapM_ (uncurry setReg) (zip [0..] args)

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


--- The return stack.
             
pushRetStack :: CodePointer -> Emulation ()
pushRetStack cp =
  do p <- asks emuProcess 
     f <- asks emuFunction
     liftIO (modifyIORef (procRetStack p) ((f, cp):))

popRetStack :: Emulation (Maybe (Function, CodePointer))
popRetStack = do p <- asks emuProcess
                 stack <- liftIO $ readIORef (procRetStack p)
                 case stack of
                   (pc:pcs) -> do liftIO $ writeIORef (procRetStack p) pcs
                                  return (Just pc)
                   _ -> return Nothing

doReturn :: Emulation ()
doReturn = do x <- popRetStack
              case x of
                Just (f, pc) ->
                  local (\c -> c { emuFunction = f }) (interpret pc)
                Nothing ->
                  return ()


--- Evaluation.

interpret :: CodePointer -> Emulation ()
interpret []     = return ()
interpret (o:os) = do p <- asks emuProcess
                      liftIO . putStrLn $ show (procPID p) ++ ": " ++ show o
                      interpret1 o os

-- Emulates a single instruction & then continues.
interpret1 :: Operation -> CodePointer -> Emulation ()
interpret1 o os =
  case o of
    OpLabel _  -> interpret os
    
    -- Supposed to test whether heap is full and if so GC.
    OpTestHeap -> interpret os
    
    -- Stack & heap management stuff.
    OpAllocate size -> allocate size            >> interpret os
    OpInit a        -> setOperand a (EVList []) >> interpret os
    OpDeallocate m  ->
      do advanceSP (- (fromIntegral m))
         interpret os

    -- Basic flow control.
    OpJump label -> jump label
    OpReturn     -> doReturn
    
    -- Conditionals.
    OpIsEqExact label a b ->
      do eq <- (==) <$> getOperand a <*> getOperand b
         if eq then interpret os else jump label
    OpTestArity label x n ->
      do EVTuple x' <- getOperand x
         if length x' == n
           then interpret os
           else jump label
         
    -- Type checks.
    OpIsTuple label x ->
      do x' <- getOperand x
         case x' of
           EVTuple _ -> interpret os
           _         -> jump label
         
    -- Invocations.
    OpBIF0 i dest ->
      do getBIF i >>= ($ []) >>= setOperand dest
         interpret os
    OpBIF2 i a b dest ->
      do (a', b') <- (,) <$> getOperand a <*> getOperand b
         getBIF i >>= ($ [a', b']) >>= setOperand dest
         interpret os
    OpCall _ label ->
      do pushRetStack os
         callByLabel label
    OpCallLast label dealloc ->
      do advanceSP (- (fromIntegral dealloc))
         jump label
    OpCallExt n i ->
      do getRegistersUpTo n >>= callImportedFunction i
         interpret os
    OpCallExtOnly n i ->
      -- FIXME: Should this be more tail recursive?
      do getRegistersUpTo n >>= callImportedFunction i
         doReturn
    OpCallExtLast n i dealloc ->
      do args <- getRegistersUpTo n
         advanceSP (- (fromIntegral dealloc))
         callImportedFunction i args
         doReturn
         
    -- Message sending and receiving.
    OpSend ->
      do join $ send <$> getReg 0 <*> getReg 1
         interpret os
    OpLoopRec label dest ->
      currentMailbox >>= liftSTM . pollMailbox >>=
        maybe (jump label) (\x -> setOperand dest x >> interpret os)
    OpLoopRecEnd label ->
      do currentMailbox >>= liftSTM . moveMessageToSaveQueue
         jump label
    OpRemoveMessage ->
      do m <- currentMailbox
         liftSTM (removeMessage m >> unreadSaveQueue m)
         interpret os
    OpWait label ->
      do currentMailbox >>= liftSTM . awaitMessage
         jump label
    OpWaitTimeout label x ->
      do timeout <- getOperand x
         mailbox <- currentMailbox
         case timeout of
           EVAtom (Atom "infinity") ->
             liftSTM (awaitMessage mailbox) >> jump label
           EVInteger n ->
             join . liftIO $
               awaitMessageTimeout mailbox (fromIntegral n)
                                   (jump label) (interpret os)
           _ ->
             fail $ "wait_timeout: " ++ show timeout ++ " not a timeout"
    OpTimeout ->
      do currentMailbox >>= liftSTM . unreadSaveQueue
         interpret os
         
    -- Data stuff.
    OpMove src dest ->
      do getOperand src >>= setOperand dest
         interpret os
    OpPutList car cdr dest ->
      do car' <- getOperand car
         EVList cdr' <- getOperand cdr
         setOperand dest (EVList (car':cdr'))
         interpret os
    OpPutTuple n dest ->
      local (\c -> c { emuTuple = (n, [], dest) }) (interpret os)
    OpPut x ->
      do x' <- getOperand x
         (n, xs, dest) <- asks emuTuple
         case n of
           1 -> do setOperand dest (EVTuple (reverse (x':xs)))
                   interpret os
           _ -> local (\c -> c { emuTuple = (n - 1, x':xs, dest) }) $
                   interpret os
    OpGetTupleElement x i dest ->
      do getOperand x >>= \(EVTuple xs) -> setOperand dest (xs !! i)
         interpret os
      
    _ -> fail $ "unhandled instruction: " ++ show o
         
         
--- Helpers for the evaluator.

call :: Emulation ()
call = asks emuFunction >>= callByLabel . functionEntry
     
-- Call the local function having a given entry point.
callByLabel :: Label -> Emulation ()
callByLabel label =
  do f <- asks ((Map.! label) . moduleEntries . emuModule)
     local (\c -> c { emuFunction = f })
       (interpret (functionLabels f Map.! label))
     
jump :: Label -> Emulation ()
jump label = do f <- asks emuFunction
                interpret (functionLabels f Map.! label)


--- Handling nonlocal function calls.

callImportedFunction :: Index -> [EValue] -> Emulation()
callImportedFunction i args =
  getImportedFunction i >>= ($ args) >>= setOperand (XOperand 0)

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
         return $ \xs ->
           do liftIO $ putStrLn ("!! " ++ displayEVs xs)
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
  
--- Handling operands.
  
getOperand :: Operand -> Emulation EValue
getOperand o =
  case o of
    IOperand x -> return (EVInteger x)
    XOperand i -> getReg (fromIntegral i)
    YOperand i -> getStack (fromIntegral i)
    AOperand (Atom "nil") -> return (EVList [])
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
    
    
--- Formatting utility functions.

displayEVs :: [EValue] -> String
displayEVs evs = concatMap f evs
  where f (EVList xs) = displayEVList xs
        f x = show x
        
displayEVList :: [EValue] -> String
displayEVList xs = if all p xs
                   then map (\(EVInteger x) -> chr (fromIntegral x)) xs
                   else show xs
  where p (EVInteger x) = (0 <= x) && (x <= 255)
        p _ = False

showMFA :: MFA -> String
showMFA (MFA (Atom m) (Atom f) a) = m ++ ":" ++ f ++ "/" ++ show a
