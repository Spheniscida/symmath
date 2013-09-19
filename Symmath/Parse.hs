module Symmath.Parse where

import Control.Applicative
import Numeric (readFloat, readSigned)
import Text.ParserCombinators.Parsec hiding (many, optional, (<|>))
import Text.ParserCombinators.Parsec.Expr

import Symmath.Terms
import Symmath.Util (eitherToMaybe)


type SymParser = Parser SymTerm

parseStr :: String -> Maybe SymTerm
parseStr = eitherToMaybe . parse expr ""

expr :: SymParser
expr = buildExpressionParser opTable term

opTable :: OperatorTable Char () SymTerm
opTable = [[Infix (Power <$ char '^') AssocLeft]
          ,[Infix (Product <$ char '*') AssocLeft, Infix (Fraction <$ char '/') AssocLeft]
          ,[Infix (Sum <$ char '+') AssocLeft, Infix (Difference <$ char '-') AssocLeft]
          ]

term :: SymParser
term = spaces *> mathTerm <* spaces

mathTerm :: SymParser
mathTerm = parens
   <|> try mathFun
   <|> try mathConst
   <|> var
   <|> num

parens :: SymParser
parens = char '(' *> expr <* char ')'

mathFun :: SymParser
mathFun = funName <*> parens

funName :: Parser (SymTerm -> SymTerm)
funName = Exp       <$ string "exp"
      <|> Ln        <$ string "ln"
      <|> Abs       <$ string "abs"
      <|> Signum    <$ string "sgn"
      <|> Trigo Sin <$ string "sin"
      <|> Trigo Cos <$ string "cos"
      <|> Trigo Tan <$ string "tan"

mathConst :: SymParser
mathConst = Constant Euler <$ string "eu"
        <|> Constant Phi   <$ string "phi"
        <|> Constant Pi    <$ string "pi"

var :: SymParser
var = Variable <$> letter

-- Taken from "Real World Haskell", Chap. 16
num :: SymParser
num = do s <- getInput
         case readSigned readFloat s of
              [(n, s')] -> Number n <$ setInput s'
              _         -> empty
