module Symmath.UPNParse where

import Symmath.Terms
import Symmath.Assoc as Assoc

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String

import Data.Char
import Data.List

type ExprStack = [SymTerm]

upnToTerm :: String -> Either String SymTerm
upnToTerm s = case parse (upnP []) "<term>" s of
                Left e -> Left . show $ e
                Right t -> Right t


upnP :: ExprStack -> Parser SymTerm
upnP s =  try (number >>= \n -> upnP (n:s))
      <|> try (constant >>= \c -> upnP (c:s))
      <|> try (oneArgExpr >>= \f -> if length s < 1
                                     then fail "one-arg function, without arguments"
                                     else let (x:xs) = s in upnP $ (f x):xs
              )
      <|> try (twoArgExpr >>= \f -> if length s < 2
                                     then fail "two-arg function, with one or less arguments"
                                     else let (x:y:xs) = s in upnP $ (f y x):xs
              )
      <|> try (variable >>= \v -> upnP (v:s))
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
oneArgExpr = do
    many space
    fun <- try (string "sqrt")
       <|> try (string "ln")
       <|> try (string "sgn")
       <|> try (string "exp")
       <|> try (string "abs")
       <|> try (choice trigonometric)
    case fun of
        "sqrt" -> return (Root (Number 2))
        "ln" -> return (Ln)
        "sgn" -> return (Signum)
        "exp" -> return (Exp)
        "abs" -> return (Abs)
        _ -> if fun `elem` trigoNames
              then return (Trigo (read fun))
              else fail "no feasible 1arg function found"

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

