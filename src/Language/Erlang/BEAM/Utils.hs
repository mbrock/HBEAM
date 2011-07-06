module Language.Erlang.BEAM.Utils
       (
         modifyTVar
       , incrementTVar 
       ) where

import Control.Monad.STM (STM)

import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)

modifyTVar :: TVar a -> (a -> a) -> STM ()
modifyTVar v f = readTVar v >>= writeTVar v . f

incrementTVar :: (Num a) => TVar a -> STM a
incrementTVar v =
  do x <- readTVar v
     writeTVar v (x + 1)
     return x
