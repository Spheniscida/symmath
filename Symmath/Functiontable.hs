module Symmath.Functiontable where

import Data.Maybe
import Control.Monad
import Data.List
import Text.Printf

import Text.PrettyPrint.Boxes

import Symmath.Terms
import Symmath.Eval
import Symmath.Util

data FuncValue = Value Double | Undefined


instance Show FuncValue where
    show (Value n) = show n
    show Undefined = "undef"

calcFunc :: SymTerm -> Double -> Double -> Double -> [(Double,FuncValue)]
calcFunc = calcFuncInd 'x'

calcFuncInd :: Char -> SymTerm -> Double -> Double -> Double -> [(Double,FuncValue)]
calcFuncInd indep t from to interval = foldr (\x l -> (x,funcAtPt x):l) [] [from,from+interval..to]
        where funcAtPt x = let fx = evalTermP t [(indep,x)] in
                                if fx == Nothing
                                then Undefined
                                else Value . fromMb $ fx

printCalcFunc :: SymTerm -> Double -> Double -> Double -> Box
printCalcFunc = printCalcFuncInd 'x'

printCalcFuncInd :: Char -> SymTerm -> Double -> Double -> Double -> Box
printCalcFuncInd indep t from to interval = vcat top $ columnTitle:values
        where termString = show t
              columnTitle = char indep <> moveRight 10 (text termString) `v0cat`  text (replicate (15 + length termString) '-')
              values = map doubleToBox $ calcFuncInd indep t from to interval
              doubleToBox (x,v) = text (printf "%.3f" x) <+> moveRight 5 (text . printfFuncValue $ v)

printCalcFuncs :: [SymTerm] -> Double -> Double -> Double -> Box
printCalcFuncs = printCalcFuncsIndSp 'x' 5

printCalcFuncsIndSp :: Char -> Int -> [SymTerm] -> Double -> Double -> Double -> Box
printCalcFuncsIndSp indep space ts from to interval = vcat top [titles,separator,columns]
        where titles = char indep <> foldr (\term box -> moveRight (space+2) (text . ('(':) . (++")") . show $ term) <> box) nullBox ts
              separator = text $ replicate (space + 3 + (length . render $ titles)) '-'
              columns = let funcvals = map (\t -> calcFuncInd indep t from to interval) ts in
                        indepsToColumn <> (moveRight space $ foldr (\vals box -> valsToColumn vals <> (moveRight space box)) nullBox funcvals)
              valsToColumn = foldr (\(x,fx) box -> (text . printfFuncValue $ fx) // box) nullBox
              indepsToColumn = foldr (\x box -> (text . printf "%.3f" $ x) // box) nullBox [from,from+interval..to]

-- Util

v0cat :: Box -> Box -> Box
v0cat a b = vcat top [a,b]

printfFuncValue :: FuncValue -> String
printfFuncValue Undefined = "undef"
printfFuncValue (Value n) = printf "%.3f" n
