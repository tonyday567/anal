{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TupleSections #-}

-- | Refresh the analyses from readme.org and write the resulting charts.
module Anal.Refresh
  ( refresh,
  )
where

import Anal
import Anal.Model
import Anal.Returns
import Chart
import Data.Bifunctor
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Profunctor
import Data.Time
import NumHask.Prelude hiding (fold)
import Prettychart
import qualified Prelude as P

refresh :: IO ()
refresh = do
  putStrLn "regenerating other/returns.csv ..."
  makeReturns
  r <- getReturns
  putStrLn $ "returns count: " <> show (length r)
  putStrLn $ "day range: " <> show (space1 (fst <$> r) :: Maybe (Range Day))

  -- accumulated return
  let accret' = scan (second' (dipure (+))) r
  putStrLn $ "first accumulated return: " <> show (take 3 accret')
  putStrLn $ "last accumulated return: " <> show (P.last accret')

  -- moving average / std
  let rs = snd <$> r
      xma = scan (ma 0.01) rs
      xstd = scan (std 0.01) rs
  putStrLn $ "last ma: " <> show (P.last xma)
  putStrLn $ "last std: " <> show (P.last xstd)

  -- mean vs quantiles chart
  let mvq = second' ((\a b -> a : (b !! 1 - a) : b) <$> ma 0.99 <*> quantiles 0.99 [0.4, 0.5, 0.6])
      mvqData = drop 1000 $ scan mvq (taker 2000 r)
      mvqChart = dayChart ["mean", "skew", "40th", "median", "60th"] mvqData
  writeChartOptions "other/mvq.svg" mvqChart
  putStrLn "wrote other/mvq.svg"

  -- median minus mean chart
  let mvm = second' ((\a b -> b - a) <$> ma 0.99 <*> median 0.99)
      mvmChart = dayChart ["median - mean"] (drop 1000 $ fmap (second (:[])) $ scan mvm (taker 2000 r))
  writeChartOptions "other/mvm.svg" mvmChart
  putStrLn "wrote other/mvm.svg"

  -- digitize median vs mean
  let qs = [0.2, 0.4, 0.6, 0.8] :: [Double]
      mvmd = ((-) <$> median 0.99 <*> ma 0.99) >>> digitize 0.996 qs
      dMvmd = drop 1000 $ scan (second' mvmd) (taker 2000 r)
      mvmdChart = digitChart defaultDigitChartStyle ((\x -> UTCTime x (P.fromInteger 0)) . fst <$> dMvmd) (fromIntegral . snd <$> dMvmd)
  writeChartOptions "other/mvmd.svg" mvmdChart
  putStrLn "wrote other/mvmd.svg"

  -- trading signal buckets
  let n = 2000
      pren = 1000
      mvmRaw = (\a b -> a - b) <$> median 0.99 <*> ma 0.99
      sigQs = [0.1, 0.4, 0.5, 0.6, 0.9] :: [Double]
      sig = scanRet (sigRet mvmRaw sigQs 0.996) n pren r
  putStrLn $ "signal sample: " <> show (take 10 sig)
  putStrLn $ "signal counts: " <> show (fold countM (snd . snd <$> sig))

  let arcData = dayChart (qRangeLabel sigQs) (scanRet (fmap (ardList 6) (sigRet mvmRaw sigQs 0.996 >>> accRetDigits)) n 0 r)
  writeChartOptions "other/arc.svg" arcData

  putStrLn "wrote other/arc.svg"

  -- inversion demo: reconstruct returns from accumulated returns
  let returns = snd <$> r
      accretS = scan (dipure (+)) returns
      returnInverse = M (\a0 -> (0, a0)) (\(_, prev) cur -> (prev, cur)) (\(prev, cur) -> cur - prev)
      reconReturns = scan returnInverse accretS
  let maxReturnErr = P.maximum $ P.zipWith (\a b -> abs (a - b)) returns reconReturns
  putStrLn $ "return inversion max error: " <> show maxReturnErr

  -- inversion demo: reconstruct prices backward from final price and returns
  prices <- getPrices
  let ps = snd <$> prices
      pLast = P.last ps
      returnsBack = P.reverse $ snd <$> r
      priceInverse = M (\_ -> pLast) (\p ret -> p / exp ret) id
      priceBack = scan priceInverse (0 : returnsBack)
      reconPrices = P.reverse priceBack
      maxPriceErr = P.maximum $ P.zipWith (\a b -> abs (a - b)) ps reconPrices
  putStrLn $ "price inversion (rounded returns) max error: " <> show maxPriceErr

  -- using full-precision returns computed directly from prices
  let fullReturns = P.zipWith (\p prev -> log (p / prev)) (P.tail ps) ps
      fullReturnsBack = P.reverse fullReturns
      priceBack' = scan priceInverse (0 : fullReturnsBack)
      reconPrices' = P.reverse priceBack'
      maxPriceErr' = P.maximum $ P.zipWith (\a b -> abs (a - b)) ps reconPrices'
  putStrLn $ "price inversion (full precision returns) max error: " <> show maxPriceErr'

  modelSummary returns

  -- magnitude forecast chart (last 1000 days): actual |r_t| vs predicted |r_t|
  let mm = magnitudeModel returns
      magData = P.drop (P.length r - 1000) $ P.zip (fst <$> r) (P.zipWith (\a p -> [a, p]) (absReturns mm) (predMagnitude mm))
      magChart = dayChart ["|r_t|", "predicted |r_t|"] magData
  writeChartOptions "other/mag.svg" magChart
  putStrLn "wrote other/mag.svg"

  -- residual histograms
  let res = P.drop 1000 $ residuals mm
      resStd = fold (std one) res
      stdRes = P.map (/ resStd) res
      resRange = maybe (Range (-1) 1) id (space1 res)
      stdResRange = maybe (Range (-5) 5) id (space1 stdRes)
  writeChartOptions "other/resid_hist.svg" (histChart resRange 60 res)
  putStrLn "wrote other/resid_hist.svg"
  writeChartOptions "other/stdresid_hist.svg" (histChart stdResRange 60 stdRes)
  putStrLn "wrote other/stdresid_hist.svg"

  putStrLn "refresh complete"
