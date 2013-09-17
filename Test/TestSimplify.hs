import Test.HUnit

import Symmath.Terms
import Symmath.Simplify

main = runTestTT tests

tests :: Test
tests = TestList [
          testNumberSum1
        , testNumberSum2
        , testNumberSum3
        , testNumberSum4
        , testNumberSum5

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

        , testExp1

        , testNumberDiff1

        , testNumberFrac1

        , testNumberFrac2
      ]

testNumberSum1 = TestCase $ assertEqual "3 + 4 == 7" (Number 7) (simplify ((Number 3) + (Number 4)))
testNumberSum2 = TestCase $ assertEqual "(x + 3) + 4 == x + 7" ((Number 7) + x) (simplify $ (x + (Number 3)) + (Number 4))
testNumberSum3 = TestCase $ assertEqual "(3 + x) + 4 == x + 7" ((Number 7) + x) (simplify $ ((Number 3) + x) + (Number 4))
testNumberSum4 = TestCase $ assertEqual "3 + (x + 4) == x + 7" ((Number 7) + x) (simplify $ (Number 3) + (Sum x (Number 4)))
testNumberSum5 = TestCase $ assertEqual "3 + (4 + x) == x + 7" ((Number 7) + x) (simplify $ (Number 3) + (Sum (Number 4) x))

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

testExp1 = TestCase $ assertEqual "eu^(ln(x) * y) == x^y" (Power x y) (simplify (Exp (Product (Ln x) y)))

testNumberDiff1 = TestCase $ assertEqual "3 - 4 == -1" (Number (-1)) (simplify (Difference (Number 3) (Number 4)))

testNumberFrac1 = TestCase $ assertEqual "3 / 4 == 3 / 4" (Fraction (Number 3) (Number 4)) (simplify (Fraction (Number 3) (Number 4)))
testNumberFrac2 = TestCase $ assertEqual "9 / 27 == 1 / 3" (Fraction (Number 1) (Number 3)) (simplify (Fraction (Number 9) (Number 27)))

-- Parts

x = Variable 'x'
y = Variable 'y'
z = Variable 'z'

