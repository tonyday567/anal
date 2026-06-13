{-# LANGUAGE RebindableSyntax #-}

-- | Time-series models that view daily returns as regressions of Mealy statistics.
module Anal.Model
  ( ModelResult (..),
    modelSummary,
    volatilityModel,
  )
where

import Data.Mealy
import NumHask.Prelude hiding (fold)
import qualified Prelude as P

-- | Lag a series by one observation, filling the first slot with a default.
lag1 :: a -> [a] -> [a]
lag1 x0 xs = x0 : P.init xs

-- | Pair two series and drop the first observations to avoid the spurious
-- initial lagged value.
regPairs :: [a] -> [a] -> [(a, a)]
regPairs x y = P.drop 1000 $ P.zip x y

-- | Simple linear regression with a given exponential decay (1 = classical OLS).
simpleReg :: (ExpField a) => a -> [a] -> [a] -> (a, a)
simpleReg r x y = fold (reg1 (ma r)) (regPairs x y)

-- | R-squared for a simple regression via correlation between actual and
-- fitted values.
rSquared :: (ExpField a) => [a] -> [a] -> a -> a -> a
rSquared x y intercept slope =
  let yhat = P.map (\xi -> intercept + slope * xi) x
   in fold (corrGauss one) (regPairs y yhat) ** (one + one)

-- | Result of fitting an AR(1)-style volatility model.
data ModelResult = ModelResult
  { stdSeries :: [Double],
    avgStdSeries :: [Double],
    volAlpha :: Double,
    volBeta :: Double,
    predStd :: [Double]
  }
  deriving (Show)

-- | Fit an AR(1) model to the slow-moving Mealy standard deviation:
--   std_t = alpha + beta * std_{t-1} + eps.
volatilityModel :: [Double] -> ModelResult
volatilityModel rs =
  let s = scan (std 0.99) rs
      as = scan (ma 0.99) s
      sL = lag1 0 s
      (intercept, slope) = simpleReg one sL s
   in ModelResult
        { stdSeries = s,
          avgStdSeries = as,
          volAlpha = intercept,
          volBeta = slope,
          predStd = P.map (\x -> intercept + slope * x) sL
        }

-- | Fit and report a small family of Mealy-statistic regressions.
modelSummary :: [Double] -> IO ()
modelSummary rs = do
  let vm = volatilityModel rs
      s = stdSeries vm
      as = avgStdSeries vm
      sL = lag1 0 s
      asL = lag1 0 as
      rL = lag1 0 rs

  putStrLn "--- volatility persistence ---"
  let (a1, b1) = simpleReg one sL s
  putStrLn $ "std_t ~ std_{t-1}:      alpha=" <> show a1 <> " beta=" <> show b1 <> " R^2=" <> show (rSquared sL s a1 b1)
  let (a2, b2) = simpleReg one asL s
  putStrLn $ "std_t ~ avg_std_{t-1}:  alpha=" <> show a2 <> " beta=" <> show b2 <> " R^2=" <> show (rSquared asL s a2 b2)

  putStrLn "--- return prediction (simple) ---"
  let (a3, b3) = simpleReg one sL rs
  putStrLn $ "r_t ~ std_{t-1}:        alpha=" <> show a3 <> " beta=" <> show b3 <> " R^2=" <> show (rSquared sL rs a3 b3)
  let (a4, b4) = simpleReg one asL rs
  putStrLn $ "r_t ~ avg_std_{t-1}:    alpha=" <> show a4 <> " beta=" <> show b4 <> " R^2=" <> show (rSquared asL rs a4 b4)
  let (a5, b5) = simpleReg one rL rs
  putStrLn $ "r_t ~ r_{t-1}:          alpha=" <> show a5 <> " beta=" <> show b5 <> " R^2=" <> show (rSquared rL rs a5 b5)

  putStrLn $ "observations: " <> show (P.length rs)
