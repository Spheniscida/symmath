module Symmath.Functiontable where

import Data.Maybe
import Control.Monad
import Data.List

import Text.PrettyPrint.Boxes

import Symmath.Terms
import Symmath.Eval
import Symmath.Util

data FuncValue = Value Double | Undefined

instance Show FuncValue where
    show (Value n) = show n
    show Undefined = "undef"

calcFunc :: SymTerm -> Double -> Double -> Double -> [(Double,FuncValue)]
calcFunc t from to interval = foldr (\x l -> (x,funcAtPt x):l) [] [from,from+interval..to]
        where funcAtPt x = let fx = evalTermP t [('x',x)] in
                                if fx == Nothing
                                then Undefined
                                else Value . fromMb $ fx

printCalcFunc :: SymTerm -> Double -> Double -> Double -> Box
printCalcFunc t from to interval = vcat top $ columnTitle:values
        where termString = show t
              columnTitle = text "x" <> moveRight 5 (text termString) `v0cat`  text (replicate (15 + length termString) '-')
              values = map doubleToBox $ calcFunc t from to interval
              doubleToBox (x,v) = text (show x) <+> moveRight 5 (text . show $ v)

-- Util

v0cat :: Box -> Box -> Box
v0cat a b = vcat top [a,b]
