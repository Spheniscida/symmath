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

-- Returns a list of Docs, representing a column.
-- Takes:       Start value, End value, Interval, field width, accuracy, term, independent variable
funcToDocList :: Double -> Double -> Double -> Int -> Int -> SymTerm -> Char -> [Doc]
funcToDocList from to ival width accur term indep = (text . printf ('%':show width++"s") . show $ term) : (text . printf ('%':show width++"s") . replicate width $ '-') : map (printfFuncValue width accur . calcFunc) [from,ival..to]
    where calcFunc x = case evalTermP term [(indep,x)] of
                            Just y -> Value y
                            Nothing -> Undefined

defaultFunctionTable :: Double -> Double -> Double -> SymTerm -> Doc
defaultFunctionTable from to ival term = foldr1 ($$) $ funcToDocList from to ival 15 3 term 'x'

-- Util

printfFuncValue :: Int -> Int -> FuncValue -> Doc
printfFuncValue width _ Undefined = text $ printf ('%':show width++"s") "undef"
printfFuncValue width acc (Value n) = text $ printf ('%':show width++"."++show acc++"f") n
