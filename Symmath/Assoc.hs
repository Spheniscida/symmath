module Symmath.Assoc where

import Data.Maybe

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type AssocList a b = Map a b

type VarBind = AssocList Char Double

bindVar :: VarBind -> Char -> Double -> VarBind
bindVar m k a = M.insert k a m

lookupVar :: VarBind -> Char -> Maybe Double
lookupVar = flip M.lookup 

keys :: AssocList a b -> [a]
keys = M.keys

values :: AssocList a b -> [b]
values = M.elems
