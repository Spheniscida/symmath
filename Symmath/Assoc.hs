module Symmath.Assoc where

import Data.Maybe

type AssocList a b = [(a,b)]

type VarBind = AssocList Char Double

bindVar :: VarBind -> Char -> Double -> VarBind
bindVar l c d = (c,d):l

lookupVar :: VarBind -> Char -> Maybe Double
lookupVar [] _c = Nothing
lookupVar (v:vs) c = if c == fst v then Just $ snd v else lookupVar vs c
