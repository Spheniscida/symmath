import Test.HUnit

import Symmath.Terms
import Symmath.RPNParse
import Symmath.Simplify

main = do
       runTestTT tests
       return ()

tests :: Test
tests = TestList [
          testNumberSum1
        , testNumberSum2
        , testNumberSum3
        , testNumberSum4
        , testNumberSum5
        , testSum6
        , testSum7
        , testSum8

        , testNumberProduct1
        , testProduct2
        , testProduct3
        , testProduct4
        , testProduct5
        , testProduct6
        , testProduct7
        , testProduct8
        , testProduct9
        , testProduct10
        , testProduct11
        , testProduct12
        , testProduct13

        , testPower1
        , testPower2
        , testPower3

        , testExp1

        , testNumberDiff1

        , testNumberFrac1
        , testNumberFrac2
        , testFrac3

        , testSquareRoot1
        , testRoot1
        , testRoot2

        , testUPN1
        , testUPN2
        , testUPN3
      ]

testNumberSum1 = TestCase $ assertEqual "3 + 4 == 7" (Number 7) (simplify ((Number 3) + (Number 4)))
testNumberSum2 = TestCase $ assertEqual "(x + 3) + 4 == x + 7" ((Number 7) + x) (simplify $ (x + (Number 3)) + (Number 4))
testNumberSum3 = TestCase $ assertEqual "(3 + x) + 4 == x + 7" ((Number 7) + x) (simplify $ ((Number 3) + x) + (Number 4))
testNumberSum4 = TestCase $ assertEqual "3 + (x + 4) == x + 7" ((Number 7) + x) (simplify $ (Number 3) + (Sum x (Number 4)))
testNumberSum5 = TestCase $ assertEqual "3 + (4 + x) == x + 7" ((Number 7) + x) (simplify $ (Number 3) + (Sum (Number 4) x))
testSum6 = TestCase $ assertEqual "x + y + x = 2*x + y" (2*x + y) (simplify (x + y + x))
testSum7 = TestCase $ assertEqual "x + y + 2*x = 3*x + y" (3*x + y) (simplify (x + y + 2*x))
testSum8 = TestCase $ assertEqual "x + y + 2*x + (-y) = 3*x" (3*x) (simplify (x + y + 2*x - y))

testNumberProduct1 = TestCase $ assertEqual "3 * 4 == 12" (Number 12) (simplify $ (Number 3) * (Number 4))
testProduct2 = TestCase $ assertEqual "x * y == x * y" (x * y) (simplify $ x * y)
testProduct3 = TestCase $ assertEqual "1 * x == x" x (simplify $ (Number 1) * x)
testProduct4 = TestCase $ assertEqual "x * 1 == x" x (simplify $ x * (Number 1))
testProduct5 = TestCase $ assertEqual "x * x == x^2" (Power x (Number 2)) (simplify $ x * x)
testProduct6 = TestCase $ assertEqual "x^2 * x == x^3" (Power x (Number 3)) (simplify $ (Power x (Number 2)) * x)
testProduct7 = TestCase $ assertEqual "x * x * x == x^3" (Power x (Number 3)) (simplify $ x * x * x)
testProduct8 = TestCase $ assertEqual "0 * x == 0" (Number 0) (simplify $ x * (Number 0))
testProduct9 = TestCase $ assertEqual "x * 0 == 0" (Number 0) (simplify $ x * (Number 0))
testProduct10 = TestCase $ assertEqual "(1+2) * (2+3) == 15" (Number 15) (simplify $ ((Number 1) + (Number 2)) * ((Number 2) + (Number 3)))
testProduct11 = TestCase $ assertEqual "(y * x) * y == x * y^2" (x * (Power y (Number 2))) (simplify $ x * y * y)
testProduct12 = TestCase $ assertEqual "(y * x) * y * (z * y * x * y) == x^2 * y^4 * z" ((Power x (Number 2)) * ((Power y (Number 4)) * z)) (simplify $ y * x * y * z * y * x * y)
testProduct13 = TestCase $ assertEqual "y * x * y^(-1) == x" x (simplify $ x * (Power y (Number (-1))) * y)

testPower1 = TestCase $ assertEqual "(x^y)^z = x^(y*z)" (Power x (y * z)) (simplify (Power (Power x y) z))
testPower2 = TestCase $ assertEqual "x^(y*z) * y^(y*z) == (x*y)^(y*z)" (Power (x*y) (y*z)) (simplify $ (Power x (y*z)) * (Power y (y*z)))
testPower3 = TestCase $ assertEqual "x^(y*z) * y^(z*x) == (x^y * y^x)^(z)" (Power ((Power x y) * (Power y x)) z) (simplify $ (Power x (y*z)) * (Power y (z*x)))

testExp1 = TestCase $ assertEqual "eu^(ln(x) * y) == x^y" (Power x y) (simplify (Exp (Product (Ln x) y)))

testNumberDiff1 = TestCase $ assertEqual "3 - 4 == -1" (Number (-1)) (simplify (Difference (Number 3) (Number 4)))

testNumberFrac1 = TestCase $ assertEqual "3 / 4 == 3 / 4" (Fraction (Number 3) (Number 4)) (simplify (Fraction (Number 3) (Number 4)))
testNumberFrac2 = TestCase $ assertEqual "9 / 27 == 1 / 3" (Fraction (Number 1) (Number 3)) (simplify (Fraction (Number 9) (Number 27)))
testFrac3 = TestCase $ assertEqual "(x * y * z^3) / (y * z) == x * z^2" (x * (Power z 2)) (simplify (Fraction (x * y * z^3) (y * z)))

testSquareRoot1 = TestCase $ assertEqual "sqrt(x) * sqrt(y) * sqrt(x) == x * y^(0.5)" (Product x (Power y (Number 0.5))) (simplify $ (Root x 2) * (Root y 2) * (Root x 2))
testRoot1 = TestCase $ assertEqual "rt(x,3) == x^(1/3)" (Power x (Number $ 1/3)) (simplify (Root x 3))
testRoot2 = TestCase $ assertEqual "rt(x,3) * rt(x,2) == x^((1/3) + (1/2))" (Power x (Number $ (1/2) + (1/3))) (simplify $ Product (Root x 3) (Root x 2))

testUPN1 = TestCase $ assertEqual "UPN: x 3 + y 4 + * -> (x+3) * (y+4)" (Right (Product (Sum x 3) (Sum y 4))) (upnToTerm "x 3 + y 4 + *")
testUPN2 = TestCase $ assertEqual "UPN: x 3 + y 4 rt * -> (x+3) * (rt(y,4))" (Right (Product (Sum x 3) (Root y 4))) (upnToTerm "x 3 + y 4 rt *")
testUPN3 = TestCase $ assertEqual "UPN: x ln 3 ln + y 4 eu ^ + * -> (ln(x) + ln(3)) * (y + 4^e)" (Right $ (Product (Sum (Ln (Variable 'x')) (Ln (Number 3.0))))
    (Sum (Variable 'y') (Power (Number 4.0) (Constant Euler)))) (upnToTerm "x ln 3 ln + y 4 eu ^ + *")

-- Parts

x = Variable 'x'
y = Variable 'y'
z = Variable 'z'

