module Symmath.RPNParse where

{-
    - RPN parser documentation

This is a standard RPN parser for SymMath terms. RPN stands for "Reverse Polish Notation" and
means that a term is not written as "(x + 4) * 3" but rather as "x 4 + 3 *". Imagine a stack behind
it:

x: Push x
4: Push 4
+: Pop x, 4; Push (x+4)
3: Push 3
*: Pop 3, (x+4); Push (3 * (x+4))

An unary operator pops one element from the stack, a binary one two:

"ln(x)" -> "x ln" (ln is unary)
"x^3" -> "x 3 ^" (^ is binary)

Available operators:

    + * - / ^ :: Standard arithmetic operators. "a b [op]" is the same as "a [op] b" (although the stack structure
            could lead to another assumption)
    -- :: Unary minus (algebraic sign)
    rt :: Binary root operator: the nth root of x is written as "n x rt"
    log :: Binary logBase operator: "base n log" = "log_{base}(n)"
    ln, exp :: Unary "logarithmus naturalis"/exponential function (e^x) operator
    sqrt, sgn, abs :: Unary operators for respectively the square root of a value, the Signum function and the
                    absolute value operator.
    sin, cos, tan, arcsin, arccos, arctan, sinh, cosh, tanh, arsinh, arcosh, artanh :: Trigonometric operators. Unary

Variables are single chars in the ranges ['a'..'z'] and ['A'..'Z']. Actually, every value accepted by isAlpha is accepted
as variable name.

Operators, values and variables are expected to be separated by at least one space character (' ', \n, \t).

-}

import Symmath.Terms
import Symmath.Util

import Text.Parsec
import Text.Parsec.String


type ExprStack = [SymTerm]

rpnToTerm :: String -> Either String SymTerm
rpnToTerm s = case parse (rpnP []) "<term>" s of
                Left e -> Left . show $ e
                Right t -> Right t


rpnP :: ExprStack -> Parser SymTerm
rpnP s =  try (number >>= \n -> rpnP (n:s))
      <|> try (constant >>= \c -> rpnP (c:s))
      <|> try (oneArgExpr >>= \f -> if length s < 1
                                     then fail "one-arg function, without arguments"
                                     else let (x:xs) = s in rpnP $ (f x):xs
              )
      <|> try (twoArgExpr >>= \f -> if length s < 2
                                     then fail "two-arg function, with one or less arguments"
                                     else let (x:y:xs) = s in rpnP $ (f y x):xs
              )
      <|> try (variable >>= \v -> rpnP (v:s))
      <|> try (spaces >> eof >> return (head s))
      <?> "something else. There is something wrong with your syntax"

number :: Parser SymTerm
number = do
    many space
    n <- decimal
    return (Number n)

decimal :: Parser Double
decimal = do
    n <- many1 (oneOf $ '.':['0'..'9'])
    return $ read n

constant :: Parser SymTerm
constant =  many space >>
       (    try (string "eu" >> return (Constant Euler))
        <|> try (string "pi" >> return (Constant Pi))
        <|> try (string "phi" >> return (Constant Phi)))

oneArgExpr :: Parser (SymTerm -> SymTerm)
oneArgExpr = many space >>
    -- Symmath.Util.(>><) "injects" the right-hand value into the monad:
    -- (>><) :: Monad m => m a -> b -> m b. It's like (<$) for functors but works for the parser monad.
    (
    try (string "sqrt" >>< (Root (Number 2)))
    <|> try (string "ln" >>< Ln)
    <|> try (string "sgn" >>< Signum)
    <|> try (string "exp" >>< Exp)
    <|> try (string "abs" >>< Abs)
    <|> try (string "--" >>< (Product (Number (-1))) )
    <|> try (choice trigonometric >>= return . Trigo . read)
    )

trigoNames = ["sin","cos","tan",
              "arcsin","arccos","arctan",
              "sinh","cosh","tanh",
              "arsinh","arcosh","artanh"]

trigonometric :: [Parser String]
trigonometric = map string trigoNames

twoArgExpr :: Parser (SymTerm -> SymTerm -> SymTerm)
twoArgExpr = do
    many space
    fun <- try (choice twoArgOpsP)
    case fun of
        "log" -> return Log
        "rt" -> return Root
        "+" -> return Sum
        "-" -> return Difference
        "*" -> return Product
        "/" -> return Fraction
        "^" -> return Power
        _ -> fail "no feasible 2arg function found"

twoArgOpsP :: [Parser String]
twoArgOpsP = map string twoArgOps

twoArgOps = ["log","rt","+","*","-","/","^"]

variable :: Parser SymTerm
variable = do
    many space
    v <- letter
    space
    return (Variable v)

