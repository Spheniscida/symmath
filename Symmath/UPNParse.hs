module Symmath.UPNParse where

import Symmath.Terms
import Symmath.Assoc as Assoc

import Data.Char
import Data.List

import Control.Monad.State
import Control.Monad.Error
import Control.Monad.Identity

type ParseError = String
type ParseInput = String

type SymParse = StateT [SymTerm] (ErrorT ParseError Identity)

-- Code for parsing/managing trigonometric functions
trigoStringFuns :: AssocList String (SymTerm -> SymTerm)
trigoStringFuns = [("sin ", Trigo Sin),("cos ", Trigo Cos),
                ("tan ", Trigo Tan),("arcsin ", Trigo Arcsin),
                ("arccos ", Trigo Arccos),("arctan ", Trigo Arctan),
                ("sinh ", Trigo Sinh),("cosh ", Trigo Cosh),
                ("tanh ", Trigo Tanh),("arsinh ", Trigo Arsinh),
                ("arcosh ", Trigo Arcosh),("artanh ", Trigo Artanh)]

trigoStrings :: [String]
trigoStrings = keys trigoStringFuns

getTrigoFunc :: ParseInput -> String
getTrigoFunc s = let l = matchingPrefs trigoStrings in
                if length (matchingPrefs trigoStrings) > 0
                then head l
                else ""
        where matchingPrefs = filter (flip isPrefixOf $ s)

parseTrigoFunc :: String -> Maybe (SymTerm -> SymTerm)
parseTrigoFunc = Assoc.lookup trigoStringFuns

-- /

parseNum :: ParseInput -> SymParse ParseInput
parseNum s = StateT $ \ts -> let d = read . takeWhile isDigit $ s
                                 r = dropWhile isDigit s
                             in return (r,(Number d):ts)

parseName :: ParseInput -> SymParse ParseInput
parseName [c] | isAlpha c = StateT $ \ts -> return ("",(Variable c):ts)
              | otherwise = throwError "Not a variable name at [end of input]"
-- Possible bug? parses "pia" as [a,Pi]
parseName s@(c:c':cs) | "eu" `isPrefixOf` s = StateT $ \ts -> return (drop 2 s,(Constant Euler):ts)
                      | "pi" `isPrefixOf` s = StateT $ \ts -> return (drop 2 s,(Constant Pi):ts)
                      | "phi"`isPrefixOf` s = StateT $ \ts -> return (drop 3 s,(Constant Phi):ts)
                      | "abs"`isPrefixOf` s = StateT $ \(t:ts) -> return (drop 3 s,(Abs t):ts)
                      | "sgn"`isPrefixOf` s = StateT $ \(t:ts) -> return (drop 3 s,(Signum t):ts)
                      | "log"`isPrefixOf` s = StateT $ \ts -> if length ts < 2
                                                              then throwError $ "Stack error: Not enough items on stack for binary Log at ..." ++ take 10 s
                                                              else let (a:b:ts') = ts in return (drop 3 s,(Log b a):ts')
                      | "ln" `isPrefixOf` s = StateT $ \(t:ts) -> return (drop 2 s,(Ln t):ts)
                      | "sqrt"`isPrefixOf`s = StateT $ \(t:ts) -> return (drop 4 s,(Root t (Number 2)):ts)
                      | "rt" `isPrefixOf` s = StateT $ \ts -> if length ts < 2
                                                              then throwError $ "Stack error: Not enough items on stack for binary root at ..." ++ take 10 s
                                                              else let (a:b:ts') = ts in return (drop 2 s,(Root b a):ts')
                      | getTrigoFunc s /= "" = StateT $ \(t:ts) -> let tcode = getTrigoFunc s in
                                                                   case parseTrigoFunc tcode of
                                                                        Just f -> return (drop (length tcode) s,(f t):ts)
                      | not (isSpace c') = throwError $ "Unable to parse variable/function/constant at ...\"" ++ take 10 s ++ "\"" -- multiple letters without separation
                      | otherwise = StateT $ \ts -> return (c':cs,(Variable c):ts)

parseOperator :: ParseInput -> SymParse ParseInput
parseOperator s@(c:cs) = StateT $ \ts -> if length ts < 2
                                         then throwError $ "Stack error: Not enough items on stack for binary operator at ...\"" ++ take 10 s ++ "\""
                                         else let (a:b:r) = ts in
                                         case c of
                                             '+' -> return (cs,(Sum b a):r)
                                             '-' -> return (cs,(Difference b a):r)
                                             '*' -> return (cs,(Product b a):r)
                                             '/' -> return (cs,(Fraction b a):r)
                                             '^' -> case b of
                                                        (Constant Euler) -> return (cs,(Exp a):r)
                                                        _ -> return (cs,(Power b a):r)



parseUPN :: ParseInput -> SymParse ParseInput
parseUPN [] = return ""
parseUPN s@(c:cs)   | isSpace c = parseUPN cs
                    | isDigit c = parseNum s >>= parseUPN
                    | isAlpha c = parseName s >>= parseUPN -- Is variable or constant or function
                    | c `elem` "+-*/^" = parseOperator s >>= parseUPN
                    | otherwise = throwError "Error encountered"

upnToTerm :: ParseInput -> Either ParseError SymTerm
upnToTerm i = case runIdentity (runErrorT (runStateT (parseUPN (i ++ " ")) [])) of
                Left e -> Left e
                Right (r,ts) -> Right $ head ts
