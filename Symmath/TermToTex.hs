module Symmath.TermToTex where

-- Convert a Symmath term to LaTeX code

import Symmath.Terms
import qualified Data.Text as T
import Data.Text (Text)
import Control.Monad.Reader

data TexConf = TexConf { multdots :: Bool,
                         fracs :: Bool }

defaultConf = TexConf { multdots = True, fracs = True }

(<+>) = T.append
tshow = T.pack . show
tchar = T.pack . (:[])

leftbrack = T.pack "("
rightbrack = T.pack ")"
space = T.pack " "

convertToLaTeX :: SymTerm -> TexConf -> Text
convertToLaTeX t c = runReader (toLaTeX t) c

toLaTeX :: SymTerm -> Reader TexConf Text
toLaTeX (Number n) = return $ tshow n
toLaTeX (Variable c) = return . T.pack $ [c]
toLaTeX (Constant c) = case c of
                        Euler -> return . tchar $ 'e'
                        Pi -> return . T.pack $ " \\pi "
                        Phi -> return . T.pack $ " \\phi "
toLaTeX (Product t1 t2) = do
                            t1' <- toLaTeX t1
                            t2' <- toLaTeX t2
                            conf <- ask
                            if multdots conf
                             then return $ t1' <+> (T.pack " \\cdot ") <+> t2'
                             else return $ t1' <+> space <+> t2'
toLaTeX (Sum t1 t2) = do
                        t1' <- toLaTeX t1
                        t2' <- toLaTeX t2
                        return $ (T.pack "(") <+> t1' <+> (T.pack " + ") <+> t2' <+> (T.pack ")")
toLaTeX (Fraction t1 t2) = do
                        t1' <- toLaTeX t1
                        t2' <- toLaTeX t2
                        conf <- ask
                        if fracs conf
                         then return $ (T.pack "\\frac{") <+> t1' <+> (T.pack "}{") <+> t2' <+> (T.pack "} ")
                         else if multdots conf
                               then return $ t1' <+> (T.pack " \\cdot ") <+> t2' <+> (T.pack "^{-1} ")
                               else return $ t1' <+> (T.pack " ") <+> t2' <+> (T.pack "^{-1} ")
toLaTeX (Difference t1 t2) = do
                        t1' <- toLaTeX t1
                        t2' <- toLaTeX t2
                        return $ leftbrack <+> t1' <+> space <+> T.pack "-" <+> space <+> t2' <+> rightbrack

