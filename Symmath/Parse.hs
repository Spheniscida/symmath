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
expr = buildExpressionParser opTable term <* eof

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
   <|> unit
   <|> var
   <|> num

parens :: SymParser
parens = char '(' *> expr <* char ')'

mathFun :: SymParser
mathFun = funName <*> parens

funName :: Parser (SymTerm -> SymTerm)
funName =      Abs       <$ string "abs"
      <|>      Trigo Cos <$ string "cos"
      <|>      Exp       <$ string "exp"
      <|>      Ln        <$ string "ln"
      <|> try (Signum    <$ string "sgn")
      <|>      Trigo Sin <$ string "sin"
      <|>      Trigo Tan <$ string "tan"

mathConst :: SymParser
mathConst =      Constant Euler <$ string "eu"
        <|> try (Constant Phi   <$ string "phi")
        <|>      Constant Pi    <$ string "pi"

unit :: SymParser
unit = char '_' *> unit'

unit' :: SymParser
unit' = try (Unit <$> unitPrefix <*> unitName)
    <|>      Unit     One        <$> unitName

unitPrefix :: Parser SIPrefix
unitPrefix = Yocto <$ char 'y'
         <|> Zepto <$ char 'z'
	 <|> Atto  <$ char 'a'
	 <|> Femto <$ char 'f'
	 <|> Pico  <$ char 'p'
	 <|> Nano  <$ char 'n'
	 <|> Micro <$ char 'µ'
	 <|> Micro <$ char 'u'
	 <|> Milli <$ char 'm'
	 <|> Kilo  <$ char 'k'
	 <|> Mega  <$ char 'M'
	 <|> Giga  <$ char 'G'
	 <|> Tera  <$ char 'T'
	 <|> Peta  <$ char 'P'
	 <|> Exa   <$ char 'E'
	 <|> Zetta <$ char 'Z'
	 <|> Yotta <$ char 'Y'

unitName :: Parser Unit
unitName =      Ampere  <$ char   'A'
       <|>      Candela <$ string "cd"
       <|>      Gram    <$ char   'g'
       <|>      Joule   <$ char   'J'
       <|>      Kelvin  <$ char   'K'
       <|> try (Mole    <$ string "mol")
       <|>      Meter   <$ char   'm'
       <|>      Newton  <$ char   'N'
       <|>      Pascal  <$ string "Pa"
       <|>      Second  <$ char   's'
       <|>      Watt    <$ char   'W'

var :: SymParser
var = Variable <$> letter

-- Taken from "Real World Haskell", Chap. 16
num :: SymParser
num = do s <- getInput
         case readSigned readFloat s of
              [(n, s')] -> Number n <$ setInput s'
              _         -> empty
