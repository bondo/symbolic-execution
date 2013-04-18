{-# LANGUAGE PackageImports #-}

module Heap where

import "mtl" Control.Monad.State
import qualified Data.Map as Map
import Data.Map (Map)

newtype Register = Register Integer
                   deriving (Eq, Ord, Show)

newtype Heap v = Heap (Map Register v)

type RegisterM = State Register
type HeapM v = StateT (Heap v) RegisterM

freshRegister :: HeapM v Register
freshRegister = lift $ do
  Register i <- get
  put $ Register $ i+1
  return $ Register i

malloc :: v -> HeapM v Register
malloc v = do
  r <- freshRegister
  Heap m <- get
  put $ Heap $ Map.insert r v m
  return r

lookup :: Register -> HeapM v (Maybe v)
lookup r = do
  Heap m <- get
  return $ Map.lookup r m

set :: Register -> v -> HeapM v ()
set r v = do
  Heap m <- get
  put $ Heap $ Map.insert r v m

run :: HeapM v a -> (a, Heap v)
run hs = evalState (runStateT hs $ Heap $ Map.empty) (Register 0)

exec :: HeapM v a -> a
exec = fst . run

eval :: HeapM v a -> Heap v
eval = snd . run
