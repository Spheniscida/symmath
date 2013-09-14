module Symmath.Simplify where

import Data.List

import Symmath.Util
import Symmath.Terms

-- simplify terms with simplifyOnce until the simplified version is the same as the one from the last simplification step
simplify :: SymTerm -> SymTerm
simplify t = let simplifiedT = simplifiedT_f in
                if t == simplifiedT
                then t
                else simplify (simplifyOnce t)
    where simplifiedT_f = simplifyOnce t

-- simplify terms (one simplification step)

simplifyOnce :: SymTerm ->  SymTerm
simplifyOnce s@(Sum _ _) = simplifySum s
simplifyOnce p@(Product _ _) = simplifyProd p
simplifyOnce d@(Difference _ _) = simplifyDiff d
simplifyOnce f@(Fraction _ _) = simplifyFrac f
simplifyOnce p@(Power _ _) = simplifyPow p
simplifyOnce l@(Ln t) = Ln $ simplifyOnce t
simplifyOnce l@(Log t1 t2) = Log (simplifyOnce t1) (simplifyOnce t2)
simplifyOnce a@(Abs _) = simplifyAbs a
simplifyOnce s@(Signum t) = Signum $ simplifyOnce t
simplifyOnce e@(Exp _) = simplifyExp e
simplifyOnce u@(UndefP d t) = UndefP d $ simplifyOnce t
simplifyOnce a = a

-- Special cases

simplifySum :: SymTerm -> SymTerm
-- Numbers
simplifySum (Sum (Number n1) (Number n2)) = Number $ n1 + n2

-- (n1 + x) + n2
simplifySum (Sum (Sum t1 (Number n1)) (Number n2)) = Sum (t1) (Number (n1 + n2))
-- (x + n1) + n2
simplifySum (Sum (Sum (Number n1) t1) (Number n2)) = Sum (t1) (Number (n1 + n2))
-- n1 + (x + n2)
simplifySum (Sum (Number n1) (Sum t1 (Number n2))) = Sum (t1) (Number (n1 + n2))
-- n1 + (n2 + x)
simplifySum (Sum (Number n1) (Sum (Number n2) t1)) = Sum (Number (n1 + n2)) t1
simplifySum (Sum t1 (Product t2 t3)) | t1 == t3 = Product (Sum t2 (Number 1)) t1
                                     | t1 == t2 = Product (Sum t3 (Number 1)) t1
simplifySum (Sum t1 t2) | t1 == t2 = Product (Number 2) t1
                        | otherwise = case prodListIntersectTuple (prodToList t1) (prodToList t2) of -- doubled simplifyOnce because un-resolved terms are multiplied with (Number 1)
                                        ([],rest1,rest2) -> Sum (simplifyOnce . simplifyOnce . listToProd $ rest1) (simplifyOnce . simplifyOnce . listToProd $ rest2)
                                        (common,rest1,rest2) -> Product (listToProd common) (Sum (listToProd rest1) (Product (Number 1) (listToProd rest2)))

-- Products
simplifyProd :: SymTerm -> SymTerm
-- Plain numbers and terms
-- 0 * x = 0
simplifyProd (Product (Number 0) _term) = Number 0
-- x * 0 = 0
simplifyProd (Product _term (Number 0)) = Number 0
-- 1 * x = x
simplifyProd (Product (Number 1) term) = term
-- x * 1 = x
simplifyProd (Product term (Number 1)) = term
-- a * b = c (c = a*b)
simplifyProd (Product (Number n1) (Number n2)) = Number $ n1 * n2
-- Equal-base powers: x^a * x^b = x^(a+b)
simplifyProd (Product (Power b1 e1) (Power b2 e2)) | b1 == b2 = Power b1 (Sum e1 e2)
                                                   | otherwise = (Product (Power b1 e1) (Power b2 e2))
simplifyProd (Product (Power b e) t) | b == t = Power b (Sum e (Number 1))
simplifyProd (Product t (Power b e)) | b == t = Power b (Sum e (Number 1))
-- Transforms x^3 * (x^5 * y) to the left-associating (x^3 * x^5) * y. But only if the second term can't be simplified anymore.
simplifyProd (Product t1 p@(Product t2 t3)) | p == simplifyOnce p = Product (Product t1 t2) t3
-- x * x = x^2
simplifyProd (Product t1 t2) | t1 == t2 = (Power t1 (Number 2))
                             | otherwise = Product (simplifyOnce t1) (simplifyOnce t2)
{-
    - Old rules, not used anymore. Do not re-implement!

-- a * (b+c) = a*b + a*c
simplifyProd (Product t1 (Sum t2 t3)) = Sum (Product t1 t2) (Product t1 t3)
-- (a+b) * c = a*c + b*c
simplifyProd (Product (Sum t2 t3) t1) = Sum (Product t1 t2) (Product t1 t3)
-}

-- Differences
simplifyDiff :: SymTerm -> SymTerm
-- Numbers
simplifyDiff (Difference (Number n1) (Number n2)) = Number $ n1 - n2
simplifyDiff (Difference t1 t2) = Sum (t1) (Product (Number (-1)) t2)

-- Fractions
simplifyFrac :: SymTerm -> SymTerm
simplifyFrac (Fraction t1 t2) | t1 == t2 = Number 1
simplifyFrac (Fraction (Number n1) (Number n2)) | isIntegral n1 && isIntegral n2 = Fraction
                                                                                    (Number (n1 / (fromInteger (gcd (round n1) (round n2)))))
                                                                                    (Number (n2 / (fromInteger (gcd (round n1) (round n2)))))
                                                | otherwise = Number $ n1 / n2
simplifyFrac (Fraction e d) = Product (Power e (Number 1)) (Power d (Number (-1)))

-- Powers
simplifyPow :: SymTerm -> SymTerm
-- (a^b)^c = a^(b*c)
simplifyPow (Power (Power b1 e1) e2) = (Power b1 (Product e1 e2)) -- Abs!?
-- (euler^a)^b = euler^(a*b)
simplifyPow (Power (Exp e1) e2) = (Exp (Product e1 e2))
-- x^0 = 1
simplifyPow (Power b (Number 0)) = Number 1
-- x^1 = x
simplifyPow (Power b (Number 1)) = b
simplifyPow (Power (Constant Euler) t) = Exp t
simplifyPow (Power t1 t2) = Power (simplifyOnce t1) (simplifyOnce t2)

simplifyAbs :: SymTerm -> SymTerm
-- abs(a * b) = abs(a) * abs(b)
simplifyAbs (Abs (Product t1 t2)) = Product (Abs t1) (Abs t2)
-- abs(a / b) = abs(a) / abs(b)
simplifyAbs (Abs (Fraction t1 t2)) = Fraction (Abs t1) (Abs t2)
-- abs(a^b) = abs(a)^b
simplifyAbs (Abs (Power b e)) = (Power (Abs b) e)
simplifyAbs (Abs t) = Abs $ simplifyOnce t

simplifyExp :: SymTerm -> SymTerm
-- euler^0 = 1
simplifyExp (Exp (Number 0)) = Number 1
-- euler^1 = euler
simplifyExp (Exp (Number 1)) = Constant Euler
simplifyExp (Exp t) = Exp $ simplifyOnce t

--------------------------------------------------------------

sortProduct :: SymTerm -> SymTerm
sortProduct = listToProd . sortBy termCompare . prodToList

termCompare :: SymTerm -> SymTerm -> Ordering
termCompare (Number n1) (Number n2) = n1 `compare` n2
termCompare (Variable v1) (Variable v2) = v1 `compare` v2
termCompare (Number n1) (Variable v1) = LT
termCompare (Variable _) t = LT
termCompare _ _ = GT

-- Converts a product tree into a list (representing the flat structure of multiplications): (x*y) * ((a*b) * z) = x*y*a*b*z
prodToList :: SymTerm -> [SymTerm]
prodToList (Product t1 t2) = prodToList t1 ++ prodToList t2
prodToList p@(Power b (Number n)) | isIntegral n = replicate (round n) b
                                  | otherwise = [p]
prodToList t = [t]

listToProd :: [SymTerm] -> SymTerm
listToProd [t] = t
listToProd (t:ts) = Product t $ listToProd ts

prodListIntersectTuple :: Eq a => [a] -> [a] -> ([a],[a],[a])
prodListIntersectTuple a b = let is = prodListIntersect a b in
                             (is, a \\ is, b \\ is)

prodListIntersect :: Eq a => [a] -> [a] -> [a]
prodListIntersect (x:xs) ys = if x `elem` ys
                                  then x:(prodListIntersect xs $ delete x ys)
                                  else prodListIntersect xs ys
prodListIntersect _ _ = []
