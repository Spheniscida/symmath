module Symmath.Terms where

import Data.Char (toLower)
import Symmath.Constants

-- Data types

data SymTerm = Number Double
             | Variable Var
             | Constant Constant
             | Product SymTerm SymTerm
             | Difference SymTerm SymTerm
             | Sum SymTerm SymTerm
             | Fraction SymTerm SymTerm
             | Power SymTerm SymTerm
             | Exp SymTerm
             | Trigo Trigo SymTerm
             | Ln SymTerm
             | Log SymTerm SymTerm
             | Abs SymTerm
             | Signum SymTerm
             | Root SymTerm SymTerm -- Root a b = a'th root of g
             | UndefP Double SymTerm
            deriving (Eq,Show)

type Var = Char

data Constant = Euler | Pi | Phi deriving (Eq, Show)

data Trigo = Sin | Cos | Tan
           | Sinh | Cosh | Tanh
           | Arcsin | Arccos | Arctan
           | Arsinh | Arcosh | Artanh deriving (Eq, Show)

-- Instances

instance Read Trigo where
    readsPrec _ = parseTrigo
        where parseTrigo "sin" = [(Sin,"")]
              parseTrigo "cos" = [(Cos,"")]
              parseTrigo "tan" = [(Tan,"")]
              parseTrigo "arcsin" = [(Arcsin,"")]
              parseTrigo "arccos" = [(Arccos,"")]
              parseTrigo "arctan" = [(Arctan,"")]
              parseTrigo "sinh" = [(Sinh,"")]
              parseTrigo "cosh" = [(Cosh,"")]
              parseTrigo "tanh" = [(Tanh,"")]
              parseTrigo "arsinh" = [(Arsinh,"")]
              parseTrigo "arcosh" = [(Arcosh,"")]
              parseTrigo "artanh" = [(Artanh,"")]
              parseTrigo _ = []

showTerm :: SymTerm -> String
showTerm (Number n) = show n
showTerm (Variable v) = [v]
showTerm (Constant c) = show c
showTerm (Product (Number (-1)) term2) = "(-" ++ (showTerm term2) ++ ")"
showTerm (Product term1 term2) = (showTerm term1) ++ " * " ++ (showTerm term2)
showTerm (Difference term1 term2) = '(' : (showTerm term1) ++ " - " ++ (showTerm term2) ++ ")"
showTerm (Sum term1 (Product (Number (-1)) term2)) = '(' : (showTerm term1) ++ " - " ++ (showTerm term2) ++ ")"
showTerm (Sum term1 term2) = '(' : (showTerm term1) ++ " + " ++ (showTerm term2) ++ ")"
showTerm (Fraction term1 term2) = '(' : (showTerm term1) ++ " / " ++ (showTerm term2) ++ ")"
showTerm (Power term1 term2) = '(' : (showTerm term1) ++ ")^(" ++ (showTerm term2) ++ ")"
showTerm (Exp term1) = "e^(" ++ showTerm term1 ++ ")"
showTerm (Trigo trigo term1) = (map toLower $ show trigo) ++ "(" ++ showTerm term1 ++ ")"
showTerm (Ln term1) = "ln(" ++ showTerm term1 ++ ")"
showTerm (Log base term) = "log(" ++ showTerm base ++ "," ++ showTerm term ++ ")"
showTerm (Abs term) = '|' : showTerm term ++ "|"
showTerm (Signum term) = "sgn(" ++ showTerm term ++ ")"
showTerm (Root t1 t2) = "root(" ++ showTerm t1 ++ "," ++ showTerm t2 ++ ")"
showTerm (UndefP p t) = "undefAt(" ++ show p ++ "," ++ showTerm t ++ ")"
--showTerm t = show t

instance Num SymTerm where
    (+) = Sum
    (*) = Product
    (-) = Difference
    negate = Product (Number (-1))
    abs = Abs
    fromInteger = Number . fromIntegral
    signum = Signum

instance Fractional SymTerm where
    t1 / t2 = Fraction t1 t2
    fromRational = Number . fromRational

-- Helper
constToNumber :: Constant -> Double
constToNumber Euler = euler
constToNumber Pi = pi
constToNumber Phi = phi

rec :: SymTerm -> SymTerm
rec u = Power u (Number (-1))

