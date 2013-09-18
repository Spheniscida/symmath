module Symmath.Functiontable where

import Data.Maybe
import Control.Monad
import Data.List
import Text.Printf

import Text.PrettyPrint

import Symmath.Terms
import Symmath.Eval
import Symmath.Util

data FuncValue = Value Double | Undefined

instance Show FuncValue where
    show (Value n) = show n
    show Undefined = "undef"

-- Use show on the returned Docs to format (render) them.


------------ Functions using defaults --------------------
-- Uses default values for field width, accuracy and the independent variable
defaultFunctionTable :: Double -> Double -> Double -> SymTerm -> Doc
defaultFunctionTable from to ival term = multipleFunctionsDefault from to ival [term]

multipleFunctionsDefault :: Double -> Double -> Double -> [SymTerm] -> Doc
multipleFunctionsDefault from to ival terms = multipleFunctions from to ival width acc terms indep
    where width = 15
          acc = 4
          indep = 'x'


----------- non-default-using functions -------------------

functionTable :: Double -> Double -> Double -> Int -> Int -> SymTerm -> Char -> Doc
functionTable from to ival width acc term indep = multipleFunctions from to ival width acc [term] indep

multipleFunctions :: Double -> Double -> Double -> Int -> Int -> [SymTerm] -> Char -> Doc
multipleFunctions from to ival width acc terms indep = foldr1 ($$) . (header:) . map (foldr1 (<+>) . calcLine) $ [from,ival..to]
    where header = colHeads $$ headSepLine
          colHeads = foldr1 (<+>) . map (text . printf ('%':show width++"s") . show) $ terms'
          headSepLine = foldr1 (<+>) $ replicate (length terms') (text . replicate width $ '-')
          calcLine x = map (calcFunc x) terms'
          calcFunc x term = case evalTermP term [(indep,x)] of
                            Just y -> printfFuncValue width acc $ Value y
                            Nothing -> printfFuncValue width acc Undefined
          terms' = (Variable indep):terms

-- Util

printfFuncValue :: Int -> Int -> FuncValue -> Doc
printfFuncValue width _ Undefined = text $ printf ('%':show width++"s") "undef"
printfFuncValue width acc (Value n) = text $ printf ('%':show width++"."++show acc++"f") n
