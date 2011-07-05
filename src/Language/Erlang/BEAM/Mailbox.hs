module Language.Erlang.BEAM.Mailbox
       (
         Mailbox
       , newMailbox
       , pollMailbox
       , removeMessage
       , awaitMessage
       , deliverMessage
       ) where

import Language.Erlang.BEAM.Types (EValue)

import Control.Monad.STM (atomically)

import Control.Concurrent.STM.TChan

newtype Mailbox = Mailbox (TChan EValue)

newMailbox :: IO Mailbox
newMailbox = fmap Mailbox (atomically newTChan)

pollMailbox :: Mailbox -> IO (Maybe EValue)
pollMailbox (Mailbox c) =
  atomically $ do
    empty <- isEmptyTChan c
    if empty
      then return Nothing
      else do x <- readTChan c
              unGetTChan c x
              return (Just x)
              
removeMessage :: Mailbox -> IO EValue
removeMessage (Mailbox c) = atomically (readTChan c)

awaitMessage :: Mailbox -> IO ()
awaitMessage (Mailbox c) = atomically $ readTChan c >>= unGetTChan c

deliverMessage :: Mailbox -> EValue -> IO ()
deliverMessage (Mailbox c) x = atomically $ writeTChan c x
