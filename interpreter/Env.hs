module Env where

import qualified Data.Map as Map
import Control.Monad

type Env k v = [Map.Map k v]

emptyEnv :: Env k v
emptyEnv = []

extend :: Ord k => Env k v -> [(k, v)] -> Env k v
extend e assoc = Map.fromList assoc : e

lookup :: Ord k => k -> Env k v -> Maybe v
lookup k e = msum $ map (Map.lookup k) e
