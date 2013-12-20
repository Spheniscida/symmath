module Symmath.Assoc where

import Data.Maybe

type AssocList a b = [(a,b)]

type VarBind = AssocList Char Double

bindVar :: VarBind -> Char -> Double -> VarBind
bindVar = bind

lookupVar :: VarBind -> Char -> Maybe Double
lookupVar = Symmath.Assoc.lookup --foldr (\(var,val) a -> if var == v then Just val else a) Nothing l

lookup :: Eq a => AssocList a b -> a -> Maybe b
lookup l k = foldr (\(k',v) a -> if k' == k then Just v else a) Nothing l

bind :: AssocList a b -> a -> b -> AssocList a b
bind l k v = (k,v):l

keys :: AssocList a b -> [a]
keys [] = []
keys ((a,b):es) = a : keys es

values :: AssocList a b -> [b]
values [] = []
values ((a,b):es) = b : values es
