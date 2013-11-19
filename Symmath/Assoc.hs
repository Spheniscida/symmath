module Symmath.Assoc where

import Data.Maybe

type AssocList a b = [(a,b)]

type VarBind = AssocList Char Double

bindVar :: VarBind -> Char -> Double -> VarBind
bindVar l c d = (c,d):l

lookupVar :: VarBind -> Char -> Maybe Double
lookupVar l v = foldr (\(var,val) a -> if var == v then Just val else a) Nothing l
