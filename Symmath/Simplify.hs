module Symmath.Simplify where

import Symmath.Util
import Symmath.Terms

-- Simplify terms.

simplify :: SymTerm ->  SymTerm
simplify s@(Sum _ _) = simplifySum s
simplify p@(Product _ _) = simplifyProd p
simplify d@(Difference _ _) = simplifyDiff d
simplify f@(Fraction _ _) = simplifyFrac f
simplify p@(Power _ _) = simplifyPow p
simplify l@(Ln t) = Ln $ simplify t
simplify l@(Log t1 t2) = Log (simplify t1) (simplify t2)
simplify a@(Abs _) = simplifyAbs a
simplify s@(Signum t) = Signum $ simplify t
simplify e@(Exp _) = simplifyExp e
simplify u@(UndefP d t) = UndefP d $ simplify t
simplify a = a

-- Special cases

simplifySum :: SymTerm -> SymTerm
-- Numbers
simplifySum (Sum (Number n1) (Number n2)) = Number $ n1 + n2
-- (n1 + x) + n2
simplifySum (Sum (Sum t1 (Number n1)) (Number n2)) = simplify $ Sum (t1) (Number (n1 + n2))
-- (x + n1) + n2
simplifySum (Sum (Sum (Number n1) t1) (Number n2)) = simplify $ Sum (t1) (Number (n1 + n2))
-- n1 + (x + n2)
simplifySum (Sum (Number n1) (Sum t1 (Number n2))) = simplify $ Sum (t1) (Number (n1 + n2))
-- n1 + (n2 + x)
simplifySum (Sum (Number n1) (Sum (Number n2) t1)) = simplify $ Sum (Number (n1 + n2)) t1
simplifySum (Sum t1 t2) = Sum (simplify t1) (simplify t2)

-- Products
simplifyProd :: SymTerm -> SymTerm
-- Plain Numbers
simplifyProd (Product (Number n1) (Number n2)) = Number $ n1 * n2
-- Equal-base powers: x^3 * x^5 = x^8
simplifyProd (Product (Power b1 e1) (Power b2 e2)) | b1 == b2 = simplify $ Power (b1) (simplify $ Sum e1 e2)
                                                   | otherwise = (Product (Power (simplify b1) (simplify e1)) (Power (simplify b2) (simplify e2)))
-- Distributive law: a * (b+c) = a*b + a*c
simplifyProd (Product t1 (Sum t2 t3)) = simplify $ Sum (Product t1 t2) (Product t1 t3)
-- (a+b) * c
simplifyProd (Product (Sum t2 t3) t1) = simplify $ Sum (Product t1 t2) (Product t1 t3)
simplifyProd (Product t1 t2) = Product (simplify t1) (simplify t2)

-- Differences
simplifyDiff :: SymTerm -> SymTerm
-- Numbers
simplifyDiff (Difference (Number n1) (Number n2)) = Number $ n1 - n2
simplifyDiff (Difference t1 t2) = simplify $ Sum (t1) (Product (Number (-1)) t2)

-- Fractions
simplifyFrac :: SymTerm -> SymTerm
simplifyFrac (Fraction (Number n1) (Number n2)) | isIntegral n1 && isIntegral n2 = simplify $ Fraction
                                                                                    (Number (n1 / (fromInteger (gcd (round n1) (round n2)))))
                                                                                    (Number (n2 / (fromInteger (gcd (round n1) (round n2)))))
                                                | otherwise = Number $ n1 / n2
simplifyFrac (Fraction t1 t2) | t1 == t2 = Number 1
simplifyFrac (Fraction e d) = simplify $ Product (Power e (Number 1)) (Power d (Number (-1)))

-- Powers
simplifyPow :: SymTerm -> SymTerm
-- Multiple power
simplifyPow (Power (Power b1 e1) e2) = simplify $ (Power b1 (Product e1 e2)) -- Abs!?
simplifyPow (Power (Exp e1) e2) = simplify $ (Exp (Product e1 e2))
simplifyPow (Power b (Number 0)) = Number 1
simplifyPow (Power b (Number 1)) = simplify b
simplifyPow (Power t1 t2) = Power (simplify t1) (simplify t2)

simplifyAbs :: SymTerm -> SymTerm
simplifyAbs (Abs (Product t1 t2)) = simplify $ Product (Abs t1) (Abs t2)
simplifyAbs (Abs (Fraction t1 t2)) = simplify $ Fraction (Abs t1) (Abs t2)
simplifyAbs (Abs (Power b e)) = simplify $ (Power (Abs b) e)
simplifyAbs (Abs t) = Abs $ simplify t

simplifyExp :: SymTerm -> SymTerm
simplifyExp (Exp (Number 0)) = Number 1
simplifyExp (Exp (Number 1)) = Constant Euler
simplifyExp (Exp t) = Exp $ simplify t
