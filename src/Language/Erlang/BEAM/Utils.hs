module Language.Erlang.BEAM.Utils (modifyTVar) where

import Control.Monad.STM (STM)

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar v f = readTVar v >>= writeTVar v . f
