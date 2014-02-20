module Symmath.Functiontable where

import Text.Printf

import Text.PrettyPrint

import qualified Data.Map.Strict as M

import Symmath.Terms
import Symmath.Eval

data FuncValue = Value Double | Undefined

instance Show FuncValue where
    show (Value n) = show n
    show Undefined = "undef"

-- Use show on the returned Docs to format (render) them.

---- Simple number lists.

functionEval :: Double -> Double -> SymTerm -> Char -> [FuncValue]
functionEval from ival term indep = map evalForX [from,from+ival..]
    where evalForX x = case evalTermP term (M.insert indep x M.empty) of
                            Left _ -> Undefined
                            Right y -> Value y

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
multipleFunctions from to ival width acc terms indep = foldr1 ($$) . (header:) . map (foldr1 (<+>) . calcLine) $ [from,(from+ival)..to]
    where header = colHeads $$ headSepLine
          colHeads = foldr1 (<+>) . map (text . printf ('%':show width++"s") . show) $ terms'
          headSepLine = foldr1 (<+>) $ replicate (length terms') (text . replicate width $ '-')
          calcLine x = map (calcFunc x) terms'
          calcFunc x term = case evalTermP term (M.insert indep x M.empty) of
                                Right y -> printfFuncValue width acc $ Value y
                                Left _ -> printfFuncValue width acc Undefined
          terms' = (Variable indep):terms

-- Util

printfFuncValue :: Int -> Int -> FuncValue -> Doc
printfFuncValue width _ Undefined = text $ printf ('%':show width++"s") "undef"
printfFuncValue width acc (Value n) = text $ printf ('%':show width++"."++show acc++"f") n
