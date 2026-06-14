{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}

-- | Time-series models that view daily returns as regressions of Mealy statistics.
--
-- The focus is on forecasting a well-defined target: the /magnitude/ of today's
-- return, |r_t|, and the /sign/ of today's return via quantile-bucketing of a
-- lagged signal.
--
-- A GARCH(1,1) variance update is also a Mealy machine:
--
-- > h_t = omega + alpha * r_{t-1}^2 + beta * h_{t-1}
module Anal.Model
  ( RegResult (..),
    SignForecastResult (..),
    SimResult (..),
    MagnitudeResult (..),
    magnitudeModel,
    garchMealy,
    signForecast,
    modelStats,
    modelSummary,
    simulateStrategy,
  )
where

import Data.Map.Strict qualified as Map
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Text (Text, unpack)
import NumHask.Prelude hiding (fold)
import System.Random
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

-- | Result of a single simple regression, including residual diagnostics.
data RegResult = RegResult
  { regName :: Text,
    regAlpha :: Double,
    regBeta :: Double,
    regR2 :: Double,
    resMean :: Double,
    resStd :: Double,
    resAutocorr :: Double,
    resSqAutocorr :: Double
  }
  deriving (Show)

-- | Result of a quantile-bucketed sign forecast.
data SignForecastResult = SignForecastResult
  { sfName :: Text,
    sfThresholds :: [Double],
    sfBuckets :: [(Int, Double, Double, Double)],
    -- ^ (bucket label, mean next-day return, std of next-day returns, count)
    sfBucketReturns :: Map.Map Int [Double]
  }
  deriving (Show)

-- | Result of a Monte-Carlo simulation of the bucket model.
data SimResult = SimResult
  { simStrategyFinals :: [Double],
    simBuyHoldFinals :: [Double],
    simStrategyMean :: Double,
    simStrategyStd :: Double,
    simBuyHoldMean :: Double,
    simBuyHoldStd :: Double,
    simBeatFraction :: Double
  }
  deriving (Show)

-- | Result of fitting a magnitude-forecasting model.
data MagnitudeResult = MagnitudeResult
  { absReturns :: [Double],
    stdSeries :: [Double],
    avgStdSeries :: [Double],
    magAlpha :: Double,
    magBeta :: Double,
    predMagnitude :: [Double],
    residuals :: [Double]
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
      predMag = P.map (\x -> intercept + slope * x) sL
   in MagnitudeResult
        { absReturns = absR,
          stdSeries = s,
          avgStdSeries = as,
          magAlpha = intercept,
          magBeta = slope,
          predMagnitude = predMag,
          residuals = P.zipWith (-) absR predMag
        }

-- | Fit a simple regression and collect residual diagnostics.
fitSimple :: Text -> [Double] -> [Double] -> RegResult
fitSimple name x y =
  let (intercept, slope) = simpleReg one x y
      yhat = P.map (\xi -> intercept + slope * xi) x
      res = P.drop 1000 $ P.zipWith (-) y yhat
      m = fold (ma one) res
      s = fold (std one) res
      ac1 = fold (corrGauss one) (regPairs res res)
      ac1sq = fold (corrGauss one) (regPairs (P.map (** 2) res) (P.map (** 2) res))
   in RegResult
        { regName = name,
          regAlpha = intercept,
          regBeta = slope,
          regR2 = rSquared x y intercept slope,
          resMean = m,
          resStd = s,
          resAutocorr = ac1,
          resSqAutocorr = ac1sq
        }

-- | A GARCH(1,1) variance Mealy: given a return, update the conditional
-- variance and emit the conditional standard deviation for the next period.
garchMealy ::
  -- | initial variance h_0
  Double ->
  -- | omega
  Double ->
  -- | alpha
  Double ->
  -- | beta
  Double ->
  Mealy Double Double

garchMealy h0 omega alphaG betaG = M inject step extract
  where
    inject r =
      let h = omega + alphaG * r * r + betaG * h0
       in (h, sqrt h)
    step (hPrev, _) r =
      let h = omega + alphaG * r * r + betaG * hPrev
       in (h, sqrt h)
    extract = snd

-- | Bucket a signal into quantiles and report the mean next-day return per
-- bucket.  Bucketing avoids small-magnitude days swamping the signal: each
-- bucket contains the same number of observations, and we read the sign off
-- the extreme buckets.
signForecast ::
  -- | name for reporting
  Text ->
  -- | signal Mealy
  Mealy Double Double ->
  -- | quantile thresholds
  [Double] ->
  -- | return series
  [Double] ->
  SignForecastResult
signForecast name sig qs rs =
  let sigMealy = (,) <$> id <*> (sig >>> digitize 0.996 qs >>> delay1 0)
      sigged = P.drop 1000 $ scan sigMealy rs
      sumMapM = M (\(v, k) -> Map.singleton k v) (\m (v, k) -> Map.insertWith (+) k v m) id
      sqSumMapM = M (\(v, k) -> Map.singleton k (v * v)) (\m (v, k) -> Map.insertWith (+) k (v * v) m) id
      countMapM = M (\a -> Map.singleton a one) (\m a -> Map.insertWith (+) a one m) id
      returnsMapM = M (\(v, k) -> Map.singleton k [v]) (\m (v, k) -> Map.insertWith (++) k [v] m) id
      sums = fold sumMapM sigged
      sqSums = fold sqSumMapM sigged
      counts = fold countMapM (snd <$> sigged)
      returnsMap = fold returnsMapM sigged
      signalSeries = P.drop 1000 $ scan sig rs
      thresholds = fold (quantiles 0.996 qs) signalSeries
      bucketStats k =
        let n = counts Map.! k
            s = sums Map.! k
            ss = sqSums Map.! k
            mean = s / n
            var = ss / n - mean * mean
         in (k, mean, sqrt var, n)
   in SignForecastResult
        { sfName = name,
          sfThresholds = thresholds,
          sfBuckets = P.map (\(k, _) -> bucketStats k) (Map.toAscList sums),
          sfBucketReturns = returnsMap
        }

-- | Determine the quantile bucket of a signal value given estimated thresholds.
toBucket :: [Double] -> Double -> Int
toBucket thresholds x = P.length (P.takeWhile (<= x) thresholds)

-- | Simulate many paths from the bucket residual model and apply a set of
-- bucket weights.  Returns are bootstrapped within each bucket, preserving
-- the empirical residual distribution.
simulateStrategy ::
  SignForecastResult ->
  -- | weight per bucket (0..5)
  [Double] ->
  -- | number of paths
  Int ->
  -- | path length
  Int ->
  IO SimResult
simulateStrategy sf weights nPaths len = do
  gen <- newStdGen
  let thresholds = sfThresholds sf
      bucketReturns = sfBucketReturns sf
      returnsFor b = Map.findWithDefault [] b bucketReturns
      draw g b =
        let rs = returnsFor b
            (idx, g') = randomR (0, P.length rs - 1) g
         in (rs P.!! idx, g')
      path g0 =
        let loop _ 0 _ stratAcc bhAcc = (stratAcc, bhAcc)
            loop g k prevR stratAcc bhAcc =
              let b = toBucket thresholds prevR
                  (r, g') = draw g b
                  w = weights P.!! b
                  stratAcc' = stratAcc + w * r
                  bhAcc' = bhAcc + r
               in loop g' (k - 1) r stratAcc' bhAcc'
            (stratFinal, bhFinal) = loop g0 len 0 0 0
         in (stratFinal, bhFinal)
      loopPaths _ 0 stratAcc bhAcc = (P.reverse stratAcc, P.reverse bhAcc)
      loopPaths g k stratAcc bhAcc =
        let (g1, g2) = split g
            (s, b) = path g1
         in loopPaths g2 (k - 1) (s : stratAcc) (b : bhAcc)
      (stratFinals, bhFinals) = loopPaths gen nPaths [] []
      meanS = fold (ma 1) stratFinals :: Double
      stdS = fold (std 1) stratFinals :: Double
      meanB = fold (ma 1) bhFinals :: Double
      stdB = fold (std 1) bhFinals :: Double
      beat :: Double
      beat = fromIntegral (P.length (P.filter (\(s, b) -> s > b) (P.zip stratFinals bhFinals))) / fromIntegral nPaths
      result = SimResult stratFinals bhFinals meanS stdS meanB stdB beat
  pure result

-- | Fit all the magnitude models and return their statistics.
modelStats :: [Double] -> [RegResult]
modelStats rs =
  let absR = P.map abs rs
      s = scan (std 0.01) rs
      as = scan (ma 0.01) s
      sL = lag1 0 s
      asL = lag1 0 as
      absRL = lag1 0 absR
      h0 = fold (std one) (P.take 1000 rs) ** 2
      var = fold (std one) rs ** 2
      omega = var * (1 - 0.1 - 0.85)
      garchStd = scan (garchMealy h0 omega 0.1 0.85) rs
      garchStdL = lag1 0 garchStd
   in [ fitSimple "|r_t| ~ std_{t-1}" sL absR,
        fitSimple "|r_t| ~ avg_std_{t-1}" asL absR,
        fitSimple "|r_t| ~ |r_{t-1}|" absRL absR,
        fitSimple "|r_t| ~ garch_std_{t-1}" garchStdL absR
      ]

-- | Pretty-print the model statistics.
modelSummary :: [Double] -> IO ()
modelSummary rs = do
  putStrLn "--- magnitude forecast: |r_t| ---"
  P.mapM_ printResult (modelStats rs)

  putStrLn "--- sign forecast by quantile buckets ---"
  let qs = [0.1, 0.4, 0.5, 0.6, 0.9] :: [Double]
  printSignForecast (signForecast "median - mean" ((\a b -> a - b) <$> median 0.99 <*> ma 0.99) qs rs)
  printSignForecast (signForecast "ma 0.01" (ma 0.01) qs rs)
  printSignForecast (signForecast "r_{t-1}" (delay1 0) qs rs)

  putStrLn $ "observations: " <> show (P.length rs)
  where
    printResult r = do
      putStrLn $
        unpack (regName r)
          <> ": alpha="
          <> show (regAlpha r)
          <> " beta="
          <> show (regBeta r)
          <> " R^2="
          <> show (regR2 r)
      putStrLn $ "  mean=" <> show (resMean r) <> " std=" <> show (resStd r)
      putStrLn $ "  autocorr (lag 1):        " <> show (resAutocorr r)
      putStrLn $ "  squared autocorr (lag 1): " <> show (resSqAutocorr r)
    printSignForecast sf = do
      putStrLn $ unpack (sfName sf) <> ":"
      P.mapM_
        ( \(b, m, s, n) ->
            putStrLn $
              "  bucket "
                <> show b
                <> ": mean="
                <> show m
                <> " std="
                <> show s
                <> " n="
                <> show n
        )
        (sfBuckets sf)
