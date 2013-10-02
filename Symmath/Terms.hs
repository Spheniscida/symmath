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
             | Unit SIPrefix Unit
             | UndefP Double SymTerm
            deriving Eq

type Var = Char

data Constant = Euler | Pi | Phi deriving (Eq, Show)

data Trigo = Sin | Cos | Tan deriving (Eq, Show)

data SIPrefix =   Yocto
                | Zepto
                | Atto
                | Femto
                | Pico
                | Nano
                | Micro
                | Milli
                | One
                | Kilo
                | Mega
                | Giga
                | Tera
                | Peta
                | Exa
                | Zetta
                | Yotta
                 deriving (Eq,Ord,Show)

data Unit = -- SI base
              Kilogram
            | Meter
            | Ampere
            | Candela
            | Kelvin
            | Second
            | Mole
            -- Derived
            | Gram
            | Newton
            | Pascal
            | Joule
            | Watt
             deriving (Eq,Ord,Show)


-- Instances

instance Show SymTerm where
    show (Number n) = show n
    show (Variable v) = [v]
    show (Constant c) = show c
    show (Product (Number (-1)) term2) = "(-" ++ (show term2) ++ ")"
    show (Product term1 term2) = (show term1) ++ " * " ++ (show term2)
    show (Difference term1 term2) = '(' : (show term1) ++ " - " ++ (show term2) ++ ")"
    show (Sum term1 (Product (Number (-1)) term2)) = '(' : (show term1) ++ " - " ++ (show term2) ++ ")"
    show (Sum term1 term2) = '(' : (show term1) ++ " + " ++ (show term2) ++ ")"
    show (Fraction term1 term2) = '(' : (show term1) ++ " / " ++ (show term2) ++ ")"
    show (Power term1 term2) = '(' : (show term1) ++ ")^(" ++ (show term2) ++ ")"
    show (Exp term1) = "exp(" ++ show term1 ++ ")"
    show (Trigo trigo term1) = (map toLower $ show trigo) ++ "(" ++ show term1 ++ ")"
    show (Ln term1) = "ln(" ++ show term1 ++ ")"
    show (Log base term) = "log(" ++ show base ++ "," ++ show term ++ ")"
    show (Abs term) = '|' : show term ++ "|"
    show (Signum term) = "sgn(" ++ show term ++ ")"
    show (Unit pref un) | pref /= One = show pref ++ show un
                        | otherwise = show un
    show (UndefP p t) = "undefAt(" ++ show p ++ "," ++ show t ++ ")"

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

prefToPower :: SIPrefix -> SymTerm
prefToPower Yocto = Power (Number 10) (Number (-24))
prefToPower Zepto = Power (Number 10) (Number (-21))
prefToPower Atto = Power (Number 10) (Number (-18))
prefToPower Femto = Power (Number 10) (Number (-15))
prefToPower Pico = Power (Number 10) (Number (-12))
prefToPower Nano = Power (Number 10) (Number (-9))
prefToPower Micro = Power (Number 10) (Number (-6))
prefToPower Milli = Power (Number 10) (Number (-3))
prefToPower One = Power (Number 10) (Number 0)
prefToPower Kilo = Power (Number 10) (Number 3)
prefToPower Mega = Power (Number 10) (Number 6)
prefToPower Giga = Power (Number 10) (Number 9)
prefToPower Tera = Power (Number 10) (Number 12)
prefToPower Peta = Power (Number 10) (Number 15)
prefToPower Exa = Power (Number 10) (Number 18)
prefToPower Zetta = Power (Number 10) (Number 21)
prefToPower Yotta = Power (Number 10) (Number 24)

expToPrefix :: Int -> SIPrefix
expToPrefix i | i < 0 && i >= (-24) = [One,Milli,Micro,Nano,Pico,Femto,Atto,Zepto,Yocto] !! ((-i) `div` 3)
              | i > 0 && i <= 24 = [One,Kilo,Mega,Giga,Tera,Peta,Exa,Zetta,Yotta] !! (i `div` 3)
              | i == 0 = One

prefToExp :: SIPrefix -> Integer
prefToExp p = toN . prefToPower $ p
    where toN (Power (Number 10) (Number e)) = round $ 10**e

recipUnit u = Power u (Number (-1))

meter = Unit One Meter
kilogram = Unit One Kilogram
kelvin = Unit One Kelvin
newton = Unit One Newton
candela = Unit One Candela
second = Unit One Second
ampere = Unit One Ampere
mole = Unit One Mole
pascal = Unit One Pascal
joule = Unit One Joule
watt = Unit One Watt
