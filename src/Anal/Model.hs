{-# LANGUAGE RebindableSyntax #-}

-- | Time-series models that view daily returns as regressions of Mealy statistics.
--
-- The focus is on forecasting a well-defined target: the /magnitude/ of today's
-- return, |r_t|.  Mealy statistics such as the online standard deviation are
-- natural predictors of magnitude, but the returns themselves are barely
-- predictable.
module Anal.Model
  ( MagnitudeResult (..),
    magnitudeModel,
    modelSummary,
  )
where

import Data.Mealy
import Data.Mealy.Quantiles
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

-- | Result of fitting a magnitude-forecasting model.
data MagnitudeResult = MagnitudeResult
  { absReturns :: [Double],
    stdSeries :: [Double],
    avgStdSeries :: [Double],
    magAlpha :: Double,
    magBeta :: Double,
    predMagnitude :: [Double]
  }
  deriving (Show)

-- | Forecast |r_t| from yesterday's Mealy standard deviation.
--
-- Uses the same fast-decay (0.01) Mealys that 'Anal.Refresh' reports, so the
-- predictor is genuinely out-of-sample: std_{t-1} only knows returns up to
-- day t-1.
magnitudeModel :: [Double] -> MagnitudeResult
magnitudeModel rs =
  let absR = P.map abs rs
      s = scan (std 0.01) rs
      as = scan (ma 0.01) s
      sL = lag1 0 s
      (intercept, slope) = simpleReg one sL absR
   in MagnitudeResult
        { absReturns = absR,
          stdSeries = s,
          avgStdSeries = as,
          magAlpha = intercept,
          magBeta = slope,
          predMagnitude = P.map (\x -> intercept + slope * x) sL
        }

-- | Summary statistics for a residual series.
residualSummary :: [Double] -> IO ()
residualSummary res = do
  let res' = P.drop 1000 res
      m = fold (ma one) res'
      s = fold (std one) res'
      qs = [0.05, 0.25, 0.5, 0.75, 0.95]
      qvals = fold (quantiles one qs) res'
      ac1 = fold (corrGauss one) (regPairs res' res')
      ac1sq = fold (corrGauss one) (regPairs (P.map (** 2) res') (P.map (** 2) res'))
  putStrLn $ "  mean=" <> show m <> " std=" <> show s
  putStrLn $ "  quantiles [0.05,0.25,0.5,0.75,0.95]: " <> show qvals
  putStrLn $ "  autocorr (lag 1):        " <> show ac1
  putStrLn $ "  squared autocorr (lag 1): " <> show ac1sq

-- | Fit and report magnitude-forecasting regressions and residual checks.
modelSummary :: [Double] -> IO ()
modelSummary rs = do
  let absR = P.map abs rs
      s = scan (std 0.01) rs
      as = scan (ma 0.01) s
      sL = lag1 0 s
      asL = lag1 0 as
      absRL = lag1 0 absR

  putStrLn "--- magnitude forecast: |r_t| ---"
  let (a1, b1) = simpleReg one sL absR
  putStrLn $ "|r_t| ~ std_{t-1}:        alpha=" <> show a1 <> " beta=" <> show b1 <> " R^2=" <> show (rSquared sL absR a1 b1)
  residualSummary (P.zipWith (-) absR (P.map (\x -> a1 + b1 * x) sL))

  let (a2, b2) = simpleReg one asL absR
  putStrLn $ "|r_t| ~ avg_std_{t-1}:    alpha=" <> show a2 <> " beta=" <> show b2 <> " R^2=" <> show (rSquared asL absR a2 b2)
  residualSummary (P.zipWith (-) absR (P.map (\x -> a2 + b2 * x) asL))

  let (a3, b3) = simpleReg one absRL absR
  putStrLn $ "|r_t| ~ |r_{t-1}|:        alpha=" <> show a3 <> " beta=" <> show b3 <> " R^2=" <> show (rSquared absRL absR a3 b3)
  residualSummary (P.zipWith (-) absR (P.map (\x -> a3 + b3 * x) absRL))

  putStrLn $ "observations: " <> show (P.length rs)
