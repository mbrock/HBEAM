module Language.Erlang.BEAM.Mailbox
       (
         Mailbox
       , newMailbox
       , pollMailbox
       , removeMessage
       , awaitMessage
       , deliverMessage
       , moveMessageToSaveQueue
       , unreadSaveQueue
       ) where

import Language.Erlang.BEAM.Utils (modifyTVar)
import Language.Erlang.BEAM.Types (EValue)

import Control.Applicative ((<$>), (<*>))

import Control.Monad.STM (atomically)

import Control.Concurrent.STM.TChan
import Control.Concurrent.STM.TVar

data Mailbox = Mailbox { mailboxChan      :: TChan EValue
                       , mailboxSaveQueue :: TVar [EValue] }

newMailbox :: IO Mailbox
newMailbox = Mailbox <$> atomically newTChan <*> atomically (newTVar [])

pollMailbox :: Mailbox -> IO (Maybe EValue)
pollMailbox (Mailbox { mailboxChan = c }) =
  atomically $ do
    empty <- isEmptyTChan c
    if empty
      then return Nothing
      else do x <- readTChan c
              unGetTChan c x
              return (Just x)
              
removeMessage :: Mailbox -> IO ()
removeMessage m = atomically (readTChan (mailboxChan m)) >> return ()

moveMessageToSaveQueue :: Mailbox -> IO EValue
moveMessageToSaveQueue m =
  atomically $ do x <- readTChan (mailboxChan m)
                  modifyTVar (mailboxSaveQueue m) (x:)
                  return x
                  
unreadSaveQueue :: Mailbox -> IO ()
unreadSaveQueue m =
  atomically $ do xs <- readTVar (mailboxSaveQueue m)
                  mapM_ (unGetTChan (mailboxChan m)) (reverse xs)
                  writeTVar (mailboxSaveQueue m) []

awaitMessage :: Mailbox -> IO ()
awaitMessage (Mailbox { mailboxChan = c }) =
  atomically $ readTChan c >>= unGetTChan c

deliverMessage :: Mailbox -> EValue -> IO ()
deliverMessage (Mailbox { mailboxChan = c }) x =
  atomically $ writeTChan c x

