module Symmath.NumericIntegration where

import qualified Data.List as L
import qualified Data.Map.Strict as M

import Symmath.Terms
import Symmath.Eval

data IntegrationConf = IntegrationConf { deltaX :: Double,
                                         independent :: Char }


defaultIntConf :: IntegrationConf
defaultIntConf = IntegrationConf { deltaX = 1e-5, independent = 'x' }

--           Function   From        To          Configuration   Area
nIntegrate :: SymTerm -> Double -> Double -> IntegrationConf -> Double
nIntegrate term from to c = L.foldl' (\ar x -> ar + dx * (fx x)) 0 [from,from+dx..to]
    where dx = deltaX c
          var = independent c
          fx x = case evalTermP term (M.insert var x M.empty) of
                    Left e -> 0
                    Right v -> v

