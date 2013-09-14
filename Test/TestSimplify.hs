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

        , testNumberDiff1

        , testNumberFrac1

        , testNumberFrac2
      ]

testNumberSum1 = TestCase $ assertEqual "3 + 4 == 7" (Number 7) (simplify (Sum (Number 3) (Number 4)))
testNumberSum2 = TestCase $ assertEqual "(x + 3) + 4 == x + 7" (Sum (Variable 'x') (Number 7)) (simplify (Sum (Sum (Variable 'x') (Number 3)) (Number 4)))
testNumberSum3 = TestCase $ assertEqual "(3 + x) + 4 == x + 7" (Sum (Variable 'x') (Number 7)) (simplify (Sum (Sum (Number 3) (Variable 'x')) (Number 4)))
testNumberSum4 = TestCase $ assertEqual "3 + (x + 4) == x + 7" (Sum (Variable 'x') (Number 7)) (simplify (Sum (Number 3) (Sum (Variable 'x') (Number 4))))
testNumberSum5 = TestCase $ assertEqual "3 + (4 + x) == x + 7" (Sum (Variable 'x') (Number 7)) (simplify (Sum (Number 3) (Sum(Number 4) (Variable 'x'))))

testNumberProduct1 = TestCase $ assertEqual "3 * 4 == 12" (Number 12) (simplify (Product (Number 3) (Number 4)))

testNumberDiff1 = TestCase $ assertEqual "3 - 4 == -1" (Number (-1)) (simplify (Difference (Number 3) (Number 4)))

testNumberFrac1 = TestCase $ assertEqual "3 / 4 == 3 / 4" (Fraction (Number 3) (Number 4)) (simplify (Fraction (Number 3) (Number 4)))
testNumberFrac2 = TestCase $ assertEqual "9 / 27 == 1 / 3" (Fraction (Number 1) (Number 3)) (simplify (Fraction (Number 9) (Number 27)))
