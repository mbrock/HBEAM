module Language.Erlang.BEAM.Mailbox
       (
         Mailbox
       , newMailbox
       , pollMailbox
       , removeMessage
       , awaitMessage
       , awaitMessageTimeout
       , deliverMessage
       , moveMessageToSaveQueue
       , unreadSaveQueue
       ) where

import Language.Erlang.BEAM.Utils (modifyTVar)
import Language.Erlang.BEAM.Types (EValue)

import Control.Monad (liftM2)

import Control.Monad.STM (STM, atomically, orElse)

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

data Mailbox = Mailbox { mailboxChan      :: TChan EValue
                       , mailboxSaveQueue :: TVar [EValue] }

newMailbox :: STM Mailbox
newMailbox = liftM2 Mailbox newTChan (newTVar [])

pollMailbox :: Mailbox -> STM (Maybe EValue)
pollMailbox (Mailbox { mailboxChan = c }) =
  do empty <- isEmptyTChan c
     if empty
       then return Nothing
       else do x <- readTChan c
               unGetTChan c x
               return (Just x)
              
removeMessage :: Mailbox -> STM ()
removeMessage m = readTChan (mailboxChan m) >> return ()

moveMessageToSaveQueue :: Mailbox -> STM EValue
moveMessageToSaveQueue m =
  do x <- readTChan (mailboxChan m)
     modifyTVar (mailboxSaveQueue m) (x:)
     return x
                  
unreadSaveQueue :: Mailbox -> STM ()
unreadSaveQueue m =
  do xs <- readTVar (mailboxSaveQueue m)
     mapM_ (unGetTChan (mailboxChan m)) (reverse xs)
     writeTVar (mailboxSaveQueue m) []

deliverMessage :: Mailbox -> EValue -> STM ()
deliverMessage (Mailbox { mailboxChan = c }) x = writeTChan c x

awaitMessage :: Mailbox -> STM ()
awaitMessage (Mailbox { mailboxChan = c }) = readTChan c >>= unGetTChan c
  
newtype Milliseconds = Milliseconds Int
  
startTimerChan :: Milliseconds -> IO (TChan ())
startTimerChan (Milliseconds n) =
  do c <- newTChanIO
     forkIO $ do threadDelay (n * 1000)
                 atomically (writeTChan c ())
     return c
  
awaitMessageTimeout :: Mailbox -> Int -> a -> a -> IO a
awaitMessageTimeout m msecs success failure =
  do timer <- startTimerChan (Milliseconds msecs)
     atomically $ orElse (awaitMessage m  >> return success)
                         (readTChan timer >> return failure)

