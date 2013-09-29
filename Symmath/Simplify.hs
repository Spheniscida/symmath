module Symmath.Simplify where

import Data.List

import Symmath.Util
import Symmath.Terms

-- simplify terms with simplifyOnce until the simplified version is the same as the one from the last simplification step
simplify :: SymTerm -> SymTerm
simplify t = if t == simplifyNext
             then t
             else simplify (simplifyNext)
    where simplifyNext = simplifyOnce t

-- simplify terms (one simplification step)

simplifyOnce :: SymTerm ->  SymTerm
simplifyOnce s@(Sum _ _) = simplifySum s
simplifyOnce p@(Product _ _) = simplifyProd p
simplifyOnce d@(Difference _ _) = simplifyDiff d
simplifyOnce f@(Fraction _ _) = simplifyFrac f
simplifyOnce p@(Power _ _) = simplifyPow p
simplifyOnce l@(Ln t) = simplifyLn l
simplifyOnce l@(Log t1 t2) = simplifyLog l
simplifyOnce a@(Abs _) = simplifyAbs a
simplifyOnce s@(Signum t) = Signum $ simplifyOnce t
simplifyOnce e@(Exp _) = simplifyExp e
simplifyOnce u@(Unit _ _) = simplifyUnit u
simplifyOnce u@(UndefP d t) = UndefP d $ simplifyOnce t
simplifyOnce a = a

-- Special cases

simplifySum :: SymTerm -> SymTerm
-- Numbers
simplifySum (Sum (Number n1) (Number n2)) = Number $ n1 + n2
-- n1 + (-n1) = 0
simplifySum (Sum t1 (Product (Number (-1)) t2)) | t1 == t2 = Number 0
simplifySum (Sum t1@(Product _ _) t2@(Product _ _)) = case prodListIntersectTuple (prodToList t1) (prodToList t2) of
                                                        ([],rest1,rest2) -> cleanSum $ Sum (simplify t1) (simplify t2)
                                                        (common,rest1,rest2) -> Product (listToProd common) (Sum (listToProd rest1) (listToProd rest2))
simplifySum s@(Sum t1 t2) = cleanSum s


-- Products
simplifyProd :: SymTerm -> SymTerm
-- Plain numbers and terms
-- 0 * x = 0
simplifyProd (Product (Number 0) _term) = Number 0
-- x * 0 = 0
simplifyProd (Product _term (Number 0)) = Number 0
-- a * b => c (c == a * b)
simplifyProd (Product (Number n1) (Number n2)) = Number $ n1 * n2
simplifyProd p@(Product t1 t2) = cleanUnits . cleanProduct $ p

-- Differences
simplifyDiff :: SymTerm -> SymTerm
-- Numbers
simplifyDiff (Difference (Number n1) (Number n2)) = Number $ n1 - n2
simplifyDiff (Difference t1 t2) = Sum (t1) (Product (Number (-1)) t2)

-- Fractions
simplifyFrac :: SymTerm -> SymTerm
simplifyFrac (Fraction t1 t2) | t1 == t2 = Number 1
                              | t1 == Number 0 = Number 0
simplifyFrac (Fraction (Number n1) (Number n2)) | isIntegral n1 && isIntegral n2 = Fraction
                                                                                    (Number (n1 / (fromInteger (gcd (round n1) (round n2)))))
                                                                                    (Number (n2 / (fromInteger (gcd (round n1) (round n2)))))
                                                | otherwise = Number $ n1 / n2
simplifyFrac (Fraction e d) = Product (Power e (Number 1)) (Power d (Number (-1)))

-- Powers
simplifyPow :: SymTerm -> SymTerm
simplifyPow (Power (Number n1) (Number n2)) = Number $ n1**n2
-- (a^b)^c = a^(b*c)
simplifyPow (Power (Power b1 e1) e2) = (Power b1 (Product e1 e2)) -- Abs!?
-- (euler^a)^b = euler^(a*b)
simplifyPow (Power (Exp e1) e2) = (Exp (Product e1 e2))
-- x^0 = 1
simplifyPow (Power b (Number 0)) = Number 1
-- x^1 = x
simplifyPow (Power b (Number 1)) = b
-- e^x = exp(x)
simplifyPow (Power (Constant Euler) t) = Exp t
-- 10^(log(10,y)) = y
simplifyPow (Power b1 (Log b2 t)) | b1 == b2 = t
simplifyPow (Power b1 p@(Product _ _)) | length isAnyLogBase > 0 = let ((Log b2 e2):ts) = isAnyLogBase in Power (e2) (listToProd ((prodToList p) \\ [Log b2 e2]))
    where isAnyLogBase = filter (hasBase b1) . filter isLog . sortProductList . prodToList $ p
          isLog t = case t of
                        (Log _ _) -> True
                        _ -> False
          hasBase b l = case l of
                        (Log b1 e) -> b == b1
                        _ -> False
simplifyPow (Power t1 t2) = Power (simplify t1) (simplify t2)

simplifyAbs :: SymTerm -> SymTerm
simplifyAbs (Abs (Number n)) = Number $ abs n
simplifyAbs (Abs (Product (Number n) t)) = Product (Number $ abs n) (Abs t)
-- abs(a * b) = abs(a) * abs(b)
simplifyAbs (Abs (Product t1 t2)) = Product (Abs t1) (Abs t2)
-- abs(a / b) = abs(a) / abs(b)
simplifyAbs (Abs (Fraction t1 t2)) = Fraction (Abs t1) (Abs t2)
-- abs(a^b) = abs(a)^b
simplifyAbs (Abs (Power b e)) = (Power (Abs b) e)
simplifyAbs (Abs t) = Abs $ simplify t

simplifyExp :: SymTerm -> SymTerm
-- euler^0 = 1
simplifyExp (Exp (Number 0)) = Number 1
-- euler^1 = euler
simplifyExp (Exp (Number 1)) = Constant Euler
-- euler^(ln(x) * y) = x^y. Only this clause; products are sorted so ln's go to the front
simplifyExp (Exp (Product (Ln t1) t2)) = Power t1 t2
simplifyExp (Exp t) = Exp $ simplify t

simplifyLn :: SymTerm -> SymTerm
simplifyLn (Ln (Number n)) = Number $ log n
simplifyLn (Ln (Exp t)) = t
simplifyLn (Ln t) = Ln (simplify t)

simplifyLog :: SymTerm -> SymTerm
simplifyLog (Log (Number n1) (Number n2)) = Number $ logBase n1 n2
simplifyLog (Log t1 (Power t2 t3)) | t1 == t2 = t3
simplifyLog (Log t1 t2) = Log (simplify t1) (simplify t2)

simplifyUnit :: SymTerm -> SymTerm
simplifyUnit (Unit Kilo Gram) = Unit One Kilogram
simplifyUnit u = u

---------------------------------------------------------------------
-- List-based simplification algorithms for sums and products. ------
---------------------------------------------------------------------
-- Tidy up products
cleanProduct :: SymTerm -> SymTerm
cleanProduct p@(Product _ _) = listToProd . sortProductList . prodListToCommonExps . prodListToPowers . map simplify . prodToList $ p

cleanSum :: SymTerm -> SymTerm
cleanSum s@(Sum _ _) = listToSum . map (foldr1 consolidSum) . groupBy sumGroupable . sortSumList . map simplify . sumToList $ s

cleanUnits :: SymTerm -> SymTerm
cleanUnits u = listToProd . concat . map simplifyUnits . groupBy unitGroupable . sortBy unitProdCompare .
    concat . map derivedToBase . concat . map prodToList . map expandPrefix . concat . map expandPower . prodToList $ u

-- Converts a product tree into a list (representing the flat structure of multiplications): (x*y) * ((a*b) * z) = x*y*a*b*z
prodToList :: SymTerm -> [SymTerm]
prodToList (Product t1 t2) = prodToList t1 ++ prodToList t2
prodToList p@(Power b (Number n)) = map (flip Power (Number n)) . prodToList $ b
prodToList (Exp t) = [Power (Constant Euler) t]
prodToList (Number 1) = []
prodToList t = [t]

listToProd :: [SymTerm] -> SymTerm
listToProd [] = Number 1
listToProd [t] = t
listToProd ((Number 1):ts) = listToProd ts
listToProd (t:ts) = Product t (listToProd ts)

sumToList :: SymTerm -> [SymTerm]
sumToList (Sum t1 t2) = sumToList t1 ++ sumToList t2
sumToList (Number 0) = []
sumToList t = [t]

listToSum :: [SymTerm] -> SymTerm
listToSum [] = Number 0
listToSum [t] = t
listToSum ((Number 0):ts) = listToSum ts
listToSum (t:ts) = Sum t (listToSum ts)

--

prodListIntersectTuple :: [SymTerm] -> [SymTerm] -> ([SymTerm],[SymTerm],[SymTerm])
prodListIntersectTuple a b = let is = prodListIntersect a b in
                             (is, a \\ is, b \\ is)

-- Data.List.intersect doesn't do what we need here
prodListIntersect :: [SymTerm] -> [SymTerm] -> [SymTerm]
--prodListIntersect ((Number n):xs) ys = prodListIntersect xs ys
prodListIntersect (x:xs) ys = if x `elem` ys
                              then x:(prodListIntersect xs $ delete x ys)
                              else prodListIntersect xs ys
prodListIntersect _ _ = []

-- Converts a product of many terms into a product with the same terms transformed to powers

prodListToPowers :: [SymTerm] -> [SymTerm]
prodListToPowers = map sameFacToPower . groupBy prodGroupable . sortProductList

prodListToCommonExps :: [SymTerm] -> [SymTerm]
prodListToCommonExps = map sameExpToProd . groupBy prodExpGroupable . sortProductExpList

sameFacToPower :: [SymTerm] -> SymTerm
sameFacToPower [t] = t
sameFacToPower a@(x:xs) | all (==x) xs = Power x (Number . fromIntegral . length $ a)
                        | all (sameBase x) xs = foldr1 sumExponent a
                        | otherwise = listToProd a

sameBase :: SymTerm -> SymTerm -> Bool
sameBase (Number n1) (Number n2) = True
sameBase (Power b1 _) (Power b2 _) = b1 == b2
sameBase a (Power b _) = a == b
sameBase (Power b _) a = a == b
sameBase a b = a == b

sumExponent :: SymTerm -> SymTerm -> SymTerm
sumExponent (Power b1 e1) (Power b2 e2) | b1 == b2 = Power b1 (Sum e1 e2)
sumExponent t (Power b e) | t == b = Power b (Sum (Number 1) e)
sumExponent (Power b e) t | t == b = Power b (Sum (Number 1) e)
sumExponent t1 t2 = simplify $ Product t1 t2

--

sameExpToProd :: [SymTerm] -> SymTerm
sameExpToProd [t] = t
sameExpToProd ts@((Power b e):xs) = if comfac /= []
                                    then Power (inParens) (listToProd comfac)
                                    else inParens
    where comfac = foldr (\(Power b e) a -> prodListIntersect a (prodToList e)) (prodToList e) xs
          inParens = listToProd $ map (\(Power b e) -> let remainfac = listToProd (prodToList e \\ comfac) in
                                                       if remainfac == Number 1
                                                       then b
                                                       else Power b remainfac) ts

--            New term   Accumulator
consolidSum :: SymTerm -> SymTerm -> SymTerm
consolidSum (Number n1) (Number n2) = simplify $ Sum (Number n1) (Number n2)
consolidSum (Variable v1) (Variable v2) | v1 == v2 = Product (Number 2) (Variable v1)
consolidSum (Variable v1) (Product (Number n) (Variable v2)) | v1 == v2 = Product (Number $ n+1) (Variable v1)
consolidSum (Product (Number n) (Variable v2)) (Variable v1)  | v1 == v2 = Product (Number $ n+1) (Variable v1)
consolidSum (Product (Number n1) t1) (Product (Number n2) t2) | t1 == t2 = Product (Number $ n1 + n2) t1
consolidSum t1 t2 = Sum t1 t2

-- Units

derivedToBase :: SymTerm -> [SymTerm]
derivedToBase (Unit One Newton) = [kilogram, meter, recipUnit second, recipUnit second]
derivedToBase (Unit One Pascal) = [kilogram, recipUnit second, recipUnit second, recipUnit meter]
derivedToBase u = [u]

simplifyUnits :: [SymTerm] -> [SymTerm]
simplifyUnits m | (derivedToBase newton) `elems` m = newton : simplifyUnits (m \\ (derivedToBase newton))
                | (derivedToBase pascal) `elems` m = pascal : simplifyUnits (m \\ (derivedToBase pascal))
                | otherwise = m

expandPower :: SymTerm -> [SymTerm]
expandPower p@(Power t (Number n)) | isIntegral n && n > 0 = replicate (round n) t
                                   | isIntegral n && n < 0 = replicate (abs . round $ n) (Power t (Number (-1)))
                                   | otherwise = [p]
expandPower x = [x]

expandPrefix :: SymTerm -> SymTerm
expandPrefix (Unit p u) = Product (prefToPower p) (Unit One u)
expandPrefix t = t

-- Comparison/sort
sortProductList :: [SymTerm] -> [SymTerm]
sortProductList = sortBy prodTermCompare

sortProductExpList :: [SymTerm] -> [SymTerm]
sortProductExpList = sortBy prodExpTermCompare

sortSumList :: [SymTerm] -> [SymTerm]
sortSumList = sortBy sumTermCompare

prodTermCompare :: SymTerm -> SymTerm -> Ordering
prodTermCompare (Number n1) (Number n2) = n1 `compare` n2
prodTermCompare (Variable v1) (Variable v2) = v1 `compare` v2
prodTermCompare (Number n1) (Variable v1) = LT
prodTermCompare (Variable v1) (Number n1) = GT
prodTermCompare (Number n) term = LT
prodTermCompare term (Number n) = GT
prodTermCompare (Power b1 _) (Power b2 _) = b1 `prodTermCompare` b2
prodTermCompare (Ln a) (Ln b) = a `prodTermCompare` b
prodTermCompare (Ln _) _ = LT
prodTermCompare _ (Ln _) = GT
prodTermCompare (Log b1 e1) (Log b2 e2) = e1 `prodTermCompare` e2
prodTermCompare (Power b1 _) t | b1 == t = GT
                               | otherwise = b1 `prodTermCompare` t
prodTermCompare t (Power b2 _) | b2 == t = LT
                               | otherwise = t `prodTermCompare` b2
prodTermCompare (Variable _) t = LT
prodTermCompare (Abs t1) t2 = t1 `prodTermCompare` t2
prodTermCompare t1 (Abs t2) = t1 `prodTermCompare` t2
prodTermCompare _ _ = EQ

prodExpTermCompare :: SymTerm -> SymTerm -> Ordering
prodExpTermCompare (Number n1) (Number n2) = n1 `compare` n2
prodExpTermCompare (Variable v1) (Variable v2) = v1 `compare` v2
prodExpTermCompare (Number n1) (Variable v1) = LT
prodExpTermCompare (Variable v1) (Number n1) = GT
prodExpTermCompare (Power b1 e1) (Power b2 e2) = e1 `prodExpTermCompare` e2
prodExpTermCompare (Ln a) (Ln b) = a `prodExpTermCompare` b
prodExpTermCompare (Ln _) _ = LT
prodExpTermCompare _ (Ln _) = GT
prodExpTermCompare (Power b1 _) t | b1 == t = LT
                                  | otherwise = b1 `prodExpTermCompare` t
prodExpTermCompare t (Power b2 _) | b2 == t = GT
                                  | otherwise = t `prodExpTermCompare` b2
prodExpTermCompare (Variable _) t = GT
prodExpTermCompare (Abs t1) t2 = t1 `prodExpTermCompare` t2
prodExpTermCompare t1 (Abs t2) = t1 `prodExpTermCompare` t2
prodExpTermCompare _ _ = EQ


sumTermCompare :: SymTerm -> SymTerm -> Ordering
sumTermCompare (Number _) (Number _) = EQ
sumTermCompare (Variable v1) (Variable v2) = v1 `compare` v2
sumTermCompare (Number _) (Variable _) = LT
sumTermCompare (Variable _) (Number _) = GT
sumTermCompare (Variable v1) (Product (Number n) (Variable v2)) = v1 `compare` v2
sumTermCompare (Product (Number n) (Variable v2)) (Variable v1)  = v2 `compare` v1
sumTermCompare (Number _) t = LT
sumTermCompare (Variable _) t = LT
sumTermCompare t (Number _) = GT
sumTermCompare t (Variable _) = GT
sumTermCompare t1 t2 = EQ


unitProdCompare :: SymTerm -> SymTerm -> Ordering
unitProdCompare (Unit _ a) (Unit _ b) = compare a b
unitProdCompare (Power t1 e) t2 = t1 `unitProdCompare` t2
unitProdCompare t1 (Power t2 e) = t1 `unitProdCompare` t2
unitProdCompare (Unit _ _) t = GT
unitProdCompare t (Unit _ _) = LT
unitProdCompare t1 t2 = EQ

-- Grouping predicates
--

prodGroupable :: SymTerm -> SymTerm -> Bool
prodGroupable (Number _) (Number _) = True
prodGroupable (Power b1 _) (Power b2 _) = b1 == b2
prodGroupable t (Power b _) = t == b
prodGroupable (Power b _) t = t == b
prodGroupable a b = a == b

prodExpGroupable :: SymTerm -> SymTerm -> Bool
prodExpGroupable (Power b1 p1@(Product _ _)) (Power b2 p2@(Product _ _)) = 0 < (length $ prodListIntersect (prodToList p1) (prodToList p2))
prodExpGroupable (Power b1 e1) (Power b2 p@(Product _ _)) = e1 `elem` prodToList p
prodExpGroupable (Power b1 p@(Product _ _)) (Power b2 e2) = e2 `elem` prodToList p
prodExpGroupable (Power b1 e1) (Power b2 e2) = e1 == e2
prodExpGroupable _ _ = False

sumGroupable :: SymTerm -> SymTerm -> Bool
sumGroupable (Number _) (Number _) = True
sumGroupable (Variable a) (Variable b) = a == b
sumGroupable (Variable a) (Product (Number n) (Variable b)) | a == b = True
sumGroupable (Product (Number n) (Variable b)) (Variable a) | a == b = True
sumGroupable (Product t1 t2) (Product t3 t4) = t1 == t3 || t1 == t4 || t2 == t3 || t2 == t4
sumGroupable _ _ = False

unitGroupable :: SymTerm -> SymTerm -> Bool
unitGroupable (Unit _ _) (Unit _ _) = True
unitGroupable (Power (Unit _ _) _) (Unit _ _) = True
unitGroupable (Unit _ _) (Power (Unit _ _) _) = True
unitGroupable (Power (Unit _ _) _) (Power (Unit _ _) _) = True
unitGroupable _ _ = False
