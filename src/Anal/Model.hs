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
    MemoryReport (..),
    MagnitudeResult (..),
    MultiRegResult (..),
    InfoResult (..),
    magnitudeModel,
    garchMealy,
    maDiffMealy,
    fitMulti,
    signForecast,
    modelStats,
    modelSummary,
    memoryReport,
    simulateStrategy,
    horizonMagnitudeModel,
    infoCoefficient,
    infoReport,
    bucketWeightGradient,
    thresholdSensitivity,
    digitizeDecaySensitivity,
    stdDecaySensitivity,
    strategyFinalFromSign,
  )
where

import Data.List (foldl', transpose)
import Data.Map.Strict qualified as Map
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Text (Text, unpack)
import NumHask.Prelude hiding (fold)
import System.Random
import Prelude qualified as P

-- | Lag a series by one observation, filling the first slot with a default.
lag1 :: a -> [a] -> [a]
lag1 x0 xs = x0 : P.init xs

-- | Lag a series by k observations.
lagk :: Int -> a -> [a] -> [a]
lagk k x0 xs = P.replicate k x0 P.<> P.take (P.length xs - k) xs

-- | Autocorrelation at lag k using the classical correlation estimator.
acf :: Int -> [Double] -> Double
acf k xs = fold (corrGauss 1) (P.drop k $ P.zip xs (lagk k 0 xs))

-- | Dickey-Fuller-style regression: regress the first difference on the
-- lagged level.  A beta significantly less than zero rejects a unit root.
dickeyFuller :: [Double] -> (Double, Double, Double)
dickeyFuller xs =
  let xsL = lag1 0 xs
      dxs = P.zipWith (-) xs xsL
      xs' = P.drop 1000 xsL
      dxs' = P.drop 1000 dxs
      (intercept, slope) = simpleReg one xs' dxs'
   in (intercept, slope, rSquared xs' dxs' intercept slope)

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
  let yhat = P.fmap (\xi -> intercept + slope * xi) x
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
    -- | (bucket label, mean next-day return, std of next-day returns, count)
    sfBuckets :: [(Int, Double, Double, Double)],
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

-- | Result of a multiple regression with named features.
data MultiRegResult = MultiRegResult
  { multiName :: Text,
    multiAlpha :: Double,
    multiBetaNames :: [Text],
    multiBetas :: [Double],
    multiR2 :: Double,
    multiResMean :: Double,
    multiResStd :: Double,
    multiResAutocorr :: Double,
    multiResSqAutocorr :: Double
  }
  deriving (Show)

-- | Information-theoretic dependence between two digitised time series.
data InfoResult = InfoResult
  { infoName :: Text,
    infoHX :: Double,
    infoHY :: Double,
    infoHXY :: Double,
    infoMI :: Double,
    infoNMI :: Double
  }
  deriving (Show)

-- | Stationarity / memory summary.
data MemoryReport = MemoryReport
  { acfReturns :: [(Int, Double)],
    acfAbs :: [(Int, Double)],
    acfStd :: [(Int, Double)],
    dickeyFullerBeta :: Double,
    dickeyFullerR2 :: Double,
    r2OneDay :: Double,
    r2TwoDay :: Double
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
  let absR = P.fmap abs rs
      s = scan (std 0.01) rs
      as = scan (ma 0.01) s
      sL = lag1 0 s
      (intercept, slope) = simpleReg one sL absR
      predMag = P.fmap (\x -> intercept + slope * x) sL
   in MagnitudeResult
        { absReturns = absR,
          stdSeries = s,
          avgStdSeries = as,
          magAlpha = intercept,
          magBeta = slope,
          predMagnitude = predMag,
          residuals = P.zipWith (-) absR predMag
        }

-- | A "difference from moving average" kernel: current value minus its
-- exponential moving average with decay @r@.  Positive values mean the series
-- is above its recent trend; negative values mean it is below.
maDiffMealy :: Double -> Mealy Double Double
maDiffMealy r = (\x m -> x - m) <$> id <*> ma r

-- | Fit a simple regression and collect residual diagnostics.
fitSimple :: Text -> [Double] -> [Double] -> RegResult
fitSimple name x y =
  let (intercept, slope) = simpleReg one x y
      yhat = P.fmap (\xi -> intercept + slope * xi) x
      res = P.drop 1000 $ P.zipWith (-) y yhat
      m = fold (ma one) res
      s = fold (std one) res
      ac1 = acf 1 res
      ac1sq = acf 1 (P.fmap (** 2) res)
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

-- | Solve a square linear system @A x = b@ by Gaussian elimination with partial
-- pivoting.  The input matrix is represented as a list of rows.
solveLinear :: [[Double]] -> [Double] -> [Double]
solveLinear a0 b0 =
  let n = P.length a0
      aug = [row P.<> [b0 P.!! i] | (i, row) <- zip [0 ..] a0]
      swap i j m
        | i == j = m
        | otherwise =
            let (pre, rest) = P.splitAt i m
                (mid, post) = P.splitAt (j - i) rest
             in (pre P.<> [P.head post] P.<> P.tail mid P.<> [P.head mid] P.<> P.tail post)
      forward m k
        | k == n = m
        | otherwise =
            let col = [m P.!! i P.!! k | i <- [k .. n - 1]]
                pivotVal = P.maximum (P.fmap P.abs col)
                pivotRow = k + P.length (P.takeWhile (\x -> P.abs x /= pivotVal) col)
                m1 = swap k pivotRow m
                piv = m1 P.!! k P.!! k
                eliminate i row
                  | i == k = row
                  | otherwise =
                      let factor = row P.!! k / piv
                       in P.zipWith (-) row (P.fmap (* factor) (m1 P.!! k))
                m2 = [eliminate i (m1 P.!! i) | i <- [0 .. n - 1]]
             in forward m2 (k + 1)
      back m =
        let loop i sol
              | i < 0 = sol
              | otherwise =
                  let row = m P.!! i
                      sumKnown = P.sum [row P.!! j * sol P.!! j | j <- [i + 1 .. n - 1]]
                      xi = (row P.!! n - sumKnown) / row P.!! i
                   in loop (i - 1) (P.take i sol P.<> [xi] P.<> P.drop (i + 1) sol)
         in loop (n - 1) (P.replicate n 0)
   in back (forward aug 0)

-- | Ridge-regression solution @beta = (X'X + lambda I)^{-1} X'y@.  The rows of
-- @xs@ are observations; a small ridge term stabilises ill-conditioned feature
-- matrices such as overlapping moving-average kernels.
ridgeSolve :: Double -> [[Double]] -> [Double] -> [Double]
ridgeSolve lambda xs y =
  let p = P.length (P.head xs)
      a =
        [ [ P.sum [xrow P.!! i * xrow P.!! j | xrow <- xs]
              + (if i == j then lambda else 0)
          | j <- [0 .. p - 1]
          ]
        | i <- [0 .. p - 1]
        ]
      b = [P.sum [xrow P.!! i * yi | (xrow, yi) <- P.zip xs y] | i <- [0 .. p - 1]]
   in solveLinear a b

-- | Fit a multiple regression with an intercept and named features.
fitMulti ::
  Text ->
  [Text] ->
  [[Double]] ->
  [Double] ->
  MultiRegResult
fitMulti name names xs y =
  let fs0 = transpose xs
      fs = P.fmap (1 :) fs0
      pairs = P.drop 1000 $ P.zip fs y
      coeffs = ridgeSolve 1e-6 (P.fmap fst pairs) (P.fmap snd pairs)
      alpha = P.head coeffs
      betaList = P.tail coeffs
      yhat = P.fmap (\xrow -> P.sum (P.zipWith (*) coeffs xrow)) fs
      res = P.drop 1000 $ P.zipWith (-) y yhat
      m = fold (ma one) res
      s = fold (std one) res
   in MultiRegResult
        { multiName = name,
          multiAlpha = alpha,
          multiBetaNames = names,
          multiBetas = betaList,
          multiR2 = fold (corrGauss one) (regPairs y yhat) ** (one + one),
          multiResMean = m,
          multiResStd = s,
          multiResAutocorr = acf 1 res,
          multiResSqAutocorr = acf 1 (P.fmap (** 2) res)
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
signForecast = signForecastR 0.996

-- | Generalised 'signForecast' with an explicit decay parameter for the
-- online quantile estimation.
signForecastR ::
  Double ->
  Text ->
  Mealy Double Double ->
  [Double] ->
  [Double] ->
  SignForecastResult
signForecastR r name sig qs rs =
  let sigMealy = (,) <$> id <*> (sig >>> digitize r qs >>> delay1 0)
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
      thresholds = fold (quantiles r qs) signalSeries
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
          sfBuckets = P.fmap (\(k, _) -> bucketStats k) (Map.toAscList sums),
          sfBucketReturns = returnsMap
        }

-- | Determine the quantile bucket of a signal value given estimated thresholds.
toBucket :: [Double] -> Double -> Int
toBucket thresholds x = P.length (P.takeWhile (<= x) thresholds)

-- | Mealy that assigns a value to a bucket given fixed thresholds.
bucketMealy :: [Double] -> Mealy Double Int
bucketMealy thresholds = M (const 0) (\_ r -> toBucket thresholds r) id

-- | Final accumulated return of the bucket-weight strategy for a given
-- 'SignForecastResult' and set of bucket weights.
strategyFinalFromSign :: SignForecastResult -> [Double] -> [Double] -> Double
strategyFinalFromSign sf weights rs =
  let thresholds = sfThresholds sf
      sigged = P.drop 1000 $ scan ((,) <$> id <*> (delay1 0 >>> bucketMealy thresholds)) rs
   in P.sum [weights P.!! b * r_t | (r_t, b) <- sigged]

-- | Gradient of the bucket-weight strategy final return with respect to each
-- bucket weight.  Because the final return is linear in the weights, the
-- gradient is just the sum of returns observed in each bucket.
bucketWeightGradient :: SignForecastResult -> [Double]
bucketWeightGradient sf =
  [P.sum (Map.findWithDefault [] b (sfBucketReturns sf)) | b <- [0 .. 5]]

-- | Finite-difference sensitivity of the strategy final return to each
-- quantile threshold.  Returns (base threshold, derivative wrt that threshold).
thresholdSensitivity :: [Double] -> [Double] -> [Double] -> Double -> [(Double, Double)]
thresholdSensitivity rs qs weights eps =
  let base = strategyFinalFromSign (signForecast "r_{t-1}" (delay1 0) qs rs) weights rs
      n = P.length qs
      sens i =
        let qs' = (P.take i qs P.<> [qs P.!! i + eps] P.<> P.drop (i + 1) qs))
            final = strategyFinalFromSign (signForecast "r_{t-1}" (delay1 0) qs' rs) weights rs
         in (qs P.!! i, (final - base) / eps)
   in P.fmap sens [0 .. n - 1]

-- | Finite-difference sensitivity of the strategy final return to the online
-- quantile-estimation decay rate @r@ used by 'digitize'.
digitizeDecaySensitivity :: [Double] -> [Double] -> [Double] -> Double -> [(Double, Double)]
digitizeDecaySensitivity rs qs weights eps =
  let run r = strategyFinalFromSign (signForecastR r "r_{t-1}" (delay1 0) qs rs) weights rs
      rs' = [0.995, 0.996, 0.997]
   in P.fmap (\r -> (r, (run (r + eps) - run r) / eps)) rs'

-- | R^2 of the simple magnitude model |r_t| ~ std_{t-1} for a range of decay
-- rates, useful for visualising the gradient of model fit around the current
-- rate.
stdDecaySensitivity :: [Double] -> [(Double, Double)]
stdDecaySensitivity rs =
  let absR = P.fmap abs rs
      run r =
        let sL = lag1 0 $ scan (std r) rs
            (a, b) = simpleReg one sL absR
         in rSquared sL absR a b
      rs' = [0.001, 0.005, 0.01, 0.02, 0.05]
   in P.fmap (\r -> (r, run r)) rs'

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
      beat = fromIntegral (P.length (P.filter (P.uncurry (P.>)) (P.zip stratFinals bhFinals))) / fromIntegral nPaths
      result = SimResult stratFinals bhFinals meanS stdS meanB stdB beat
  pure result

-- | Compute stationarity, memory, and forecast-decay statistics.
memoryReport :: [Double] -> MemoryReport
memoryReport rs =
  let absR = P.fmap abs rs
      s = scan (std 0.01) rs
      lags = [1, 2, 3, 5, 10, 20]
      (_, dfBeta, dfR2) = dickeyFuller absR
      sL1 = lag1 0 s
      sL2 = lagk 2 0 s
      (a1, b1) = simpleReg one sL1 absR
      (a2, b2) = simpleReg one sL2 absR
   in MemoryReport
        { acfReturns = P.fmap (\k -> (k, acf k rs)) lags,
          acfAbs = P.fmap (\k -> (k, acf k absR)) lags,
          acfStd = P.fmap (\k -> (k, acf k s)) lags,
          dickeyFullerBeta = dfBeta,
          dickeyFullerR2 = dfR2,
          r2OneDay = rSquared sL1 absR a1 b1,
          r2TwoDay = rSquared sL2 absR a2 b2
        }

-- | Digitise a series into three buckets using the [1/3, 2/3] quantiles.
digit3 :: [Double] -> [Int]
digit3 xs =
  let thresholds = fold (quantiles 0.996 [one / 3, (one + one) / 3]) xs
   in P.fmap (toBucket thresholds) xs

-- | Shannon entropy from a probability distribution (base 2).
entropy :: [Double] -> Double
entropy ps = -P.sum [p * P.logBase 2 p | p <- ps, p > 0]

-- | Normalised mutual information between two signals, computed on 3-digit
-- discretisations.  NMI = I(X;Y) / min(H(X),H(Y)) is 0 for independence and
-- 1 for a deterministic relationship.
infoCoefficient ::
  Text ->
  Mealy Double Double ->
  Mealy Double Double ->
  [Double] ->
  InfoResult
infoCoefficient name sigX sigY rs =
  let xs = P.drop 1000 $ scan sigX rs
      ys = P.drop 1000 $ scan sigY rs
      pairs = P.zip (digit3 xs) (digit3 ys)
      counts = foldl' (\m (x, y) -> Map.insertWith (+) (x, y) (1 :: Int) m) Map.empty pairs
      n = fromIntegral (P.length pairs) :: Double
      joint = Map.map ((/ n) . fromIntegral) counts
      marginalX = Map.fromListWith (+) [(x, p) | ((x, _), p) <- Map.toList joint]
      marginalY = Map.fromListWith (+) [(y, p) | ((_, y), p) <- Map.toList joint]
      hx = entropy (Map.elems marginalX)
      hy = entropy (Map.elems marginalY)
      hxy = entropy (Map.elems joint)
      mi = hx + hy - hxy
   in InfoResult
        { infoName = name,
          infoHX = hx,
          infoHY = hy,
          infoHXY = hxy,
          infoMI = mi,
          infoNMI = mi / P.min hx hy
        }

-- | Information coefficients for a few interesting signal pairs.
infoReport :: [Double] -> [InfoResult]
infoReport rs =
  let absM = abs <$> id
      s = std 0.01
      as = s >>> ma 0.01
      h0 = fold (std one) (P.take 1000 rs) ** 2
      var = fold (std one) rs ** 2
      omega = var * (1 - 0.1 - 0.85)
      g = garchMealy h0 omega 0.1 0.85
   in [ infoCoefficient "r_t vs r_{t-1}" id (delay1 0) rs,
        infoCoefficient "|r_t| vs |r_{t-1}|" absM (abs <$> delay1 0) rs,
        infoCoefficient "r_t vs std_{t-1}" id (s >>> delay1 0) rs,
        infoCoefficient "|r_t| vs std_{t-1}" absM (s >>> delay1 0) rs,
        infoCoefficient "|r_t| vs avg_std_{t-1}" absM (as >>> delay1 0) rs,
        infoCoefficient "|r_t| vs garch_std_{t-1}" absM (g >>> delay1 0) rs
      ]

-- | Fit all the magnitude models and return their statistics.
modelStats :: [Double] -> [RegResult]
modelStats rs =
  let absR = P.fmap abs rs
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

-- | Multi-feature magnitude model: a baseline lagged absolute return plus
-- moving-average-difference kernels at weekly, monthly and yearly horizons.
horizonMagnitudeModel :: [Double] -> MultiRegResult
horizonMagnitudeModel rs =
  let absR = P.fmap abs rs
      absRL = lag1 0 absR
      weekly = lag1 0 $ scan (maDiffMealy 0.8) absR
      monthly = lag1 0 $ scan (maDiffMealy 0.95) absR
      yearly = lag1 0 $ scan (maDiffMealy 0.996) absR
   in fitMulti
        "|r_t| ~ x0*|r_{t-1}| + weekly + monthly + yearly"
        ["|r_{t-1}|", "weekly", "monthly", "yearly"]
        [absRL, weekly, monthly, yearly]
        absR

-- | Pretty-print the model statistics.
modelSummary :: [Double] -> IO ()
modelSummary rs = do
  putStrLn "--- magnitude forecast: |r_t| ---"
  P.mapM_ printResult (modelStats rs)

  putStrLn "--- multi-feature magnitude model ---"
  let multi = horizonMagnitudeModel rs
  putStrLn $ unpack (multiName multi) <> ": alpha=" <> show (multiAlpha multi) <> " R^2=" <> show (multiR2 multi)
  P.mapM_
    ( \(n, b) ->
        putStrLn $ "  " <> unpack n <> " beta=" <> show b
    )
    (P.zip (multiBetaNames multi) (multiBetas multi))
  putStrLn $ "  mean=" <> show (multiResMean multi) <> " std=" <> show (multiResStd multi)
  putStrLn $ "  autocorr (lag 1):        " <> show (multiResAutocorr multi)
  putStrLn $ "  squared autocorr (lag 1): " <> show (multiResSqAutocorr multi)

  putStrLn "--- sign forecast by quantile buckets ---"
  let qs = [0.1, 0.4, 0.5, 0.6, 0.9] :: [Double]
  printSignForecast (signForecast "median - mean" ((\a b -> a - b) <$> median 0.99 <*> ma 0.99) qs rs)
  printSignForecast (signForecast "ma 0.01" (ma 0.01) qs rs)
  printSignForecast (signForecast "r_{t-1}" (delay1 0) qs rs)
  printSignForecast (signForecast "r_{t-2}" (delay1 0 >>> delay1 0) qs rs)

  putStrLn "--- stationarity / memory ---"
  let absR = P.fmap abs rs
      s = scan (std 0.01) rs
  putStrLn "autocorrelations of returns:"
  P.mapM_ (\k -> putStrLn $ "  lag " <> show k <> ": " <> show (acf k rs)) [1, 2, 3, 5, 10, 20]
  putStrLn "autocorrelations of |r|:"
  P.mapM_ (\k -> putStrLn $ "  lag " <> show k <> ": " <> show (acf k absR)) [1, 2, 3, 5, 10, 20]
  putStrLn "autocorrelations of std 0.01:"
  P.mapM_ (\k -> putStrLn $ "  lag " <> show k <> ": " <> show (acf k s)) [1, 2, 3, 5, 10, 20]
  let (aDF, bDF, r2DF) = dickeyFuller absR
  putStrLn $ "Dickey-Fuller on |r|: alpha=" <> show aDF <> " beta=" <> show bDF <> " R^2=" <> show r2DF

  putStrLn "--- two-day decay ---"
  let sL2 = lagk 2 0 s
      (a2, b2) = simpleReg one sL2 absR
  putStrLn $ "|r_t| ~ std_{t-2}: alpha=" <> show a2 <> " beta=" <> show b2 <> " R^2=" <> show (rSquared sL2 absR a2 b2)

  putStrLn "--- information coefficients (3-digit NMI) ---"
  P.mapM_ printInfo (infoReport rs)

  putStrLn $ "observations: " <> show (P.length rs)
  where
    printInfo i =
      putStrLn $
        unpack (infoName i)
          <> ": H(X)="
          <> show (infoHX i)
          <> " H(Y)="
          <> show (infoHY i)
          <> " H(X,Y)="
          <> show (infoHXY i)
          <> " MI="
          <> show (infoMI i)
          <> " NMI="
          <> show (infoNMI i)
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
