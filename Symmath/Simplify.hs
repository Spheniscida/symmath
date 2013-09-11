module Symmath.Simplify where

import Symmath.Terms

-- Simplify terms.

simplify :: SymTerm ->  SymTerm
-- Evaluate plain operators
simplify (Sum (Number n1) (Number n2)) = Number $ n1 + n2
simplify (Product (Number n1) (Number n2)) = Number $ n1 * n2
simplify (Difference (Number n1) (Number n2)) = Number $ n1 - n2
simplify (Fraction (Number n1) (Number n2)) = Number $ n1 / n2

-- Some more complicated stuff
simplify (Product (Power b1 e1) (Power b2 e2)) | b1 == b2 = Power (simplify b1) (simplify (Sum (simplify e1) (simplify e2)))
                                               | otherwise = (Product (Power (simplify b1) (simplify e1)) (Power (simplify b2) (simplify e2)))


simplify (Fraction e d) = Product (simplify e) (Power (simplify d) (Number (-1)))

simplify a = a
