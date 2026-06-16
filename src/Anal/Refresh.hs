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
import Data.Text (Text, pack, unpack)
import Data.Time
import NumHask.Prelude hiding (fold)
import Prettychart
import qualified Prelude as P

fmt :: Double -> Text
fmt = fixed (Just 4)

row :: RegResult -> Text
row r =
  mconcat
    [ "<tr>",
      "<td>" <> regName r <> "</td>",
      "<td>" <> fmt (regAlpha r) <> "</td>",
      "<td>" <> fmt (regBeta r) <> "</td>",
      "<td>" <> fmt (regR2 r) <> "</td>",
      "<td>" <> fmt (resMean r) <> "</td>",
      "<td>" <> fmt (resStd r) <> "</td>",
      "<td>" <> fmt (resAutocorr r) <> "</td>",
      "<td>" <> fmt (resSqAutocorr r) <> "</td>",
      "</tr>"
    ]

writeDashboard :: [RegResult] -> MultiRegResult -> [InfoResult] -> SignForecastResult -> (Double, Double, Double, Double, Double) -> SimResult -> MemoryReport -> IO ()
writeDashboard stats multi infoResults signResult (stratFinal, bhFinal, avgWeight, avgTrade, annFactor) sim memReport = do
  let bestR2 = P.max (P.maximum (P.map regR2 stats)) (multiR2 multi)
      html =
        mconcat
          [ "<!DOCTYPE html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n<title>Anal Dashboard</title>\n<style>",
            css,
            "</style>\n</head>\n<body>",
            header,
            tableSection,
            multiSection,
            commentary bestR2,
            histSection,
            magSection,
            signSection,
            infoSection,
            stratSection stratFinal bhFinal avgWeight avgTrade annFactor,
            simSection sim stratFinal bhFinal annFactor,
            memorySection memReport,
            "</body>\n</html>\n"
          ]
  writeFile "other/dashboard.html" (unpack html)
  putStrLn "wrote other/dashboard.html"
  where
    css =
      pack
        "body{font-family:system-ui,-apple-system,Segoe UI,Roboto,sans-serif;max-width:1200px;margin:0 auto;padding:20px;background:#fff;color:#222}\n\
        \h1{font-size:1.6rem;margin-bottom:0.2em}\n\
        \h2{font-size:1.2rem;margin-top:2rem}\n\
        \.subtitle{color:#555;font-size:0.95rem;margin-bottom:1.5rem}\n\
        \.row{display:flex;gap:20px;flex-wrap:wrap;margin-top:1rem}\n\
        \.card{flex:1;min-width:320px;border:1px solid #ddd;border-radius:8px;padding:15px;background:#fafafa}\n\
        \.card img{width:100%;height:auto;display:block}\n\
        \table{border-collapse:collapse;width:100%;margin-top:10px;font-size:0.9rem}\n\
        \th,td{border:1px solid #ddd;padding:8px;text-align:left}\n\
        \th{background:#f0f0f0}\n\
        \.commentary{background:#f5f5f5;padding:15px;border-radius:8px;margin-top:1.5rem;line-height:1.5}\n\
        \code{background:#e8e8e8;padding:2px 4px;border-radius:3px;font-family:SFMono-Regular,Menlo,monospace}\n"
    header =
      mconcat
        [ "<h1>Anal: daily return magnitude models</h1>",
          "<p class=\"subtitle\">Forecasting <code>|r_t|</code>, the absolute log-return of the S&amp;P 500, from lagged Mealy statistics. All predictors are known before day <code>t</code>.</p>"
        ]
    tableSection =
      mconcat
        [ "<h2>Model comparison</h2>",
          "<table><thead><tr><th>Model</th><th>α</th><th>β</th><th>R²</th><th>residual mean</th><th>residual std</th><th>residual autocorr</th><th>squared residual autocorr</th></tr></thead><tbody>",
          mconcat (P.map row stats),
          "</tbody></table>"
        ]
    multiSection =
      mconcat
        [ "<h2>Multi-feature magnitude model</h2>",
          "<p class=\"subtitle\">Calibrating a baseline lagged absolute return against moving-average-difference kernels at weekly (0.8), monthly (0.95) and yearly (0.996) horizons. All predictors are lagged by one day.</p>",
          "<table><thead><tr><th>feature</th><th>beta</th></tr></thead><tbody>",
          "<tr><td>α</td><td>" <> fmt (multiAlpha multi) <> "</td></tr>",
          mconcat (P.map multiRow (P.zip (multiBetaNames multi) (multiBetas multi))),
          "</tbody></table>",
          "<table><thead><tr><th>metric</th><th>value</th></tr></thead><tbody>",
          "<tr><td>R²</td><td>" <> fmt (multiR2 multi) <> "</td></tr>",
          "<tr><td>residual mean</td><td>" <> fmt (multiResMean multi) <> "</td></tr>",
          "<tr><td>residual std</td><td>" <> fmt (multiResStd multi) <> "</td></tr>",
          "<tr><td>residual autocorr (lag 1)</td><td>" <> fmt (multiResAutocorr multi) <> "</td></tr>",
          "<tr><td>squared residual autocorr (lag 1)</td><td>" <> fmt (multiResSqAutocorr multi) <> "</td></tr>",
          "</tbody></table>"
        ]
      where
        multiRow (n, b) = "<tr><td>" <> n <> "</td><td>" <> fmt b <> "</td></tr>"
    commentary best =
      mconcat
        [ "<div class=\"commentary\">",
          "<p>The best one-step magnitude forecast is now the multi-feature model, which explains about <strong>" <> fmt best <> "</strong> of the variance of <code>|r_t|</code>. It combines a baseline lagged absolute return (<code>|r_{t-1}|</code>) with moving-average-difference kernels at weekly, monthly and yearly horizons. The GARCH(1,1) conditional standard deviation is close behind. Daily return magnitude is only weakly predictable, which is what we expect for liquid markets.</p>",
          "<p>Once the lagged absolute return is included, the residual autocorrelation drops to near zero for the multi-feature model. The simple one-lag predictors leave small but positive residual autocorrelation, showing that volatility clustering is partly a short-memory effect. The histograms below show the raw and standardized residuals from the Mealy-std model.</p>",
          "</div>"
        ]
    histSection =
      mconcat
        [ "<h2>Residual histograms</h2>",
          "<div class=\"row\">",
          "<div class=\"card\"><h3>Raw residuals</h3><img src=\"resid_hist.svg\" alt=\"Raw residual histogram\"></div>",
          "<div class=\"card\"><h3>Standardized residuals</h3><img src=\"stdresid_hist.svg\" alt=\"Standardized residual histogram\"></div>",
          "</div>"
        ]
    magSection =
      mconcat
        [ "<h2>Magnitude forecast (last 1000 days)</h2>",
          "<div class=\"row\"><div class=\"card\"><img src=\"mag.svg\" alt=\"Magnitude forecast\"></div></div>"
        ]
    signSection =
      mconcat
        [ "<h2>Sign forecast by buckets</h2>",
          "<p class=\"subtitle\">Signal: <code>" <> sfName signResult <> "</code>. Returns are split into quantile buckets; the bar shows the mean next-day return for each bucket. Extreme buckets isolate the tails, so a genuine sign signal would show low buckets negative and high buckets positive.</p>",
          "<div class=\"row\"><div class=\"card\"><img src=\"sign_forecast.svg\" alt=\"Sign forecast\"></div></div>",
          "<table><thead><tr><th>bucket</th><th>mean next-day return</th><th>std</th><th>count</th></tr></thead><tbody>",
          mconcat (P.map signRow (sfBuckets signResult)),
          "</tbody></table>"
        ]
    signRow (b, m, s, n) =
      "<tr><td>" <> pack (show b) <> "</td><td>" <> fmt m <> "</td><td>" <> fmt s <> "</td><td>" <> pack (show n) <> "</td></tr>"
    infoSection =
      mconcat
        [ "<h2>Information coefficients (3-digit NMI)</h2>",
          "<p class=\"subtitle\">Each series is split into three tertile digits ([0.33, 0.67]). The joint 3×3 table gives Shannon entropies and normalised mutual information. NMI = 0 means independence; NMI = 1 means a deterministic relationship.</p>",
          "<table><thead><tr><th>pair</th><th>H(X)</th><th>H(Y)</th><th>H(X,Y)</th><th>MI</th><th>NMI</th></tr></thead><tbody>",
          mconcat (P.map infoRow infoResults),
          "</tbody></table>"
        ]
      where
        infoRow i =
          "<tr><td>" <> infoName i <> "</td><td>" <> fmt (infoHX i) <> "</td><td>" <> fmt (infoHY i) <> "</td><td>" <> fmt (infoHXY i) <> "</td><td>" <> fmt (infoMI i) <> "</td><td>" <> fmt (infoNMI i) <> "</td></tr>"
    stratSection stratFinal bhFinal avgWeight avgTrade annFactor =
      let ann x = x / annFactor
       in mconcat
            [ "<h2>Signal strategy</h2>",
              "<p class=\"subtitle\">The previous-day return is mapped to a <code>[0,1]</code> position size: the most negative bucket is 100% long, the most positive bucket is 0% long. Strategy return = weight × next-day return. Annualised figures assume 250 trading days per year.</p>",
              "<div class=\"row\"><div class=\"card\"><img src=\"signal_strategy.svg\" alt=\"Signal strategy\"></div></div>",
              "<table><thead><tr><th>metric</th><th>total</th><th>annualised</th></tr></thead><tbody>",
              "<tr><td>signal strategy final return</td><td>" <> fmt stratFinal <> "</td><td>" <> fmt (ann stratFinal) <> "</td></tr>",
              "<tr><td>buy &amp; hold final return</td><td>" <> fmt bhFinal <> "</td><td>" <> fmt (ann bhFinal) <> "</td></tr>",
              "<tr><td>average position size</td><td>" <> fmt avgWeight <> "</td><td>-</td></tr>",
              "<tr><td>average absolute weight change per day</td><td>" <> fmt avgTrade <> "</td><td>-</td></tr>",
              "<tr><td>strategy scaled to 100% average exposure</td><td>" <> fmt (stratFinal / avgWeight) <> "</td><td>" <> fmt (ann (stratFinal / avgWeight)) <> "</td></tr>",
              "</tbody></table>"
            ]
    simSection sim stratFinal bhFinal annFactor =
      let ann x = x / annFactor
          annStd x = x / sqrt annFactor
       in mconcat
            [ "<h2>Monte-Carlo simulation</h2>",
              "<p class=\"subtitle\">We simulate 1000 paths from the bucket residual model: each day's return is bootstrapped from the empirical returns in the bucket determined by the previous day's return. The same bucket weights are applied, so the histograms below show the distribution of final log-returns we might expect from the model. Red dots mark the actual historical outcome.</p>",
              "<div class=\"row\">",
              "<div class=\"card\"><h3>Signal strategy finals</h3><img src=\"sim_strat_hist.svg\" alt=\"Simulated strategy histogram\"></div>",
              "<div class=\"card\"><h3>Buy &amp; hold finals</h3><img src=\"sim_bh_hist.svg\" alt=\"Simulated buy-hold histogram\"></div>",
              "</div>",
              "<table><thead><tr><th>metric</th><th>actual</th><th>actual ann.</th><th>simulated mean</th><th>sim. mean ann.</th><th>simulated std</th><th>sim. std ann.</th></tr></thead><tbody>",
              "<tr><td>signal strategy final return</td><td>" <> fmt stratFinal <> "</td><td>" <> fmt (ann stratFinal) <> "</td><td>" <> fmt (simStrategyMean sim) <> "</td><td>" <> fmt (ann (simStrategyMean sim)) <> "</td><td>" <> fmt (simStrategyStd sim) <> "</td><td>" <> fmt (annStd (simStrategyStd sim)) <> "</td></tr>",
              "<tr><td>buy &amp; hold final return</td><td>" <> fmt bhFinal <> "</td><td>" <> fmt (ann bhFinal) <> "</td><td>" <> fmt (simBuyHoldMean sim) <> "</td><td>" <> fmt (ann (simBuyHoldMean sim)) <> "</td><td>" <> fmt (simBuyHoldStd sim) <> "</td><td>" <> fmt (annStd (simBuyHoldStd sim)) <> "</td></tr>",
              "<tr><td>fraction of paths where strategy beats buy-hold</td><td>-</td><td>-</td><td>" <> fmt (simBeatFraction sim) <> "</td><td>-</td><td>-</td><td>-</td></tr>",
              "</tbody></table>"
            ]
    memorySection memReport =
      mconcat
        [ "<h2>Stationarity, memory and forecast decay</h2>",
          "<p class=\"subtitle\">Autocorrelations show that raw returns are close to white noise, while |r| and the Mealy std are highly persistent. The Dickey-Fuller regression on |r| rejects a unit root (beta is negative) but the process has long memory. The two-day magnitude forecast loses almost none of its explanatory power, so the std signal decays very slowly.</p>",
          "<table><thead><tr><th>lag</th><th>acf(returns)</th><th>acf(|r|)</th><th>acf(std 0.01)</th></tr></thead><tbody>",
          mconcat (P.map acfRow (P.zip3 (acfReturns memReport) (acfAbs memReport) (acfStd memReport))),
          "</tbody></table>",
          "<table><thead><tr><th>test</th><th>value</th></tr></thead><tbody>",
          "<tr><td>Dickey-Fuller beta on |r|</td><td>" <> fmt (dickeyFullerBeta memReport) <> "</td></tr>",
          "<tr><td>Dickey-Fuller R² on |r|</td><td>" <> fmt (dickeyFullerR2 memReport) <> "</td></tr>",
          "<tr><td>|r_t| ~ std_{t-1} R²</td><td>" <> fmt (r2OneDay memReport) <> "</td></tr>",
          "<tr><td>|r_t| ~ std_{t-2} R²</td><td>" <> fmt (r2TwoDay memReport) <> "</td></tr>",
          "</tbody></table>"
        ]
      where
        acfRow ((k, ar), (_, ab), (_, ast)) =
          "<tr><td>" <> pack (show k) <> "</td><td>" <> fmt ar <> "</td><td>" <> fmt ab <> "</td><td>" <> fmt ast <> "</td></tr>"

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

  -- sign forecast by quantile buckets (using previous-day return as signal)
  let qs = [0.1, 0.4, 0.5, 0.6, 0.9] :: [Double]
      signResult = signForecast "r_{t-1}" (delay1 0) qs returns
      signBuckets = sfBuckets signResult
      signLabels = P.map (\(b, _, _, _) -> pack (show b)) signBuckets
      signMeans = P.map (\(_, m, _, _) -> m) signBuckets
      signChart = barChart defaultBarOptions (BarData [signMeans] signLabels ["mean next-day return"])
  writeChartOptions "other/sign_forecast.svg" signChart
  putStrLn "wrote other/sign_forecast.svg"

  -- signal-based strategy: map the r_{t-1} bucket to a [0,1] weight and go long
  let sigMealy = (,) <$> id <*> (delay1 0 >>> digitize 0.996 qs)
      sigged = P.drop 1000 $ scan (second' sigMealy) r
      weights = P.map (\(_, (_, b)) -> (5 - fromIntegral b :: Double) / 5) sigged
      rets = P.map (fst . snd) sigged :: [Double]
      stratRets = P.zipWith (*) weights rets
      bhCum = scan (dipure (+)) rets :: [Double]
      stratCum = scan (dipure (+)) stratRets :: [Double]
      stratData = P.zip (P.map fst sigged) (P.zipWith (\s b -> [s, b]) stratCum bhCum)
      stratChart = dayChart ["signal strategy", "buy & hold"] stratData
      avgWeight = fold (ma 1) weights
      avgTrade = fold (ma 1) (P.map abs (P.zipWith (-) (P.tail weights) weights))
  writeChartOptions "other/signal_strategy.svg" stratChart
  putStrLn "wrote other/signal_strategy.svg"
  putStrLn $ "signal strategy final return: " <> show (P.last stratCum)
  putStrLn $ "buy & hold final return:      " <> show (P.last bhCum)
  putStrLn $ "average signal weight:        " <> show avgWeight
  putStrLn $ "average absolute weight change: " <> show avgTrade
  let ann x = x / (fromIntegral (P.length returns) / 250.0)
  putStrLn $ "signal strategy annualised:   " <> show (ann (P.last stratCum))
  putStrLn $ "buy & hold annualised:        " <> show (ann (P.last bhCum))
  putStrLn $ "scaled strategy annualised:   " <> show (ann (P.last stratCum / avgWeight))

  -- Monte-Carlo simulation of the bucket residual model
  let bucketWeights = [1.0, 0.8, 0.6, 0.4, 0.2, 0.0] :: [Double]
  sim <- simulateStrategy signResult bucketWeights 1000 (P.length sigged)
  let simStrat = simStrategyFinals sim
      simBH = simBuyHoldFinals sim
      simStratRange = maybe (Range 0 5) id (space1 simStrat)
      simBHRange = maybe (Range 0 5) id (space1 simBH)
  writeChartOptions "other/sim_strat_hist.svg" (histChart simStratRange 40 simStrat)
  putStrLn "wrote other/sim_strat_hist.svg"
  writeChartOptions "other/sim_bh_hist.svg" (histChart simBHRange 40 simBH)
  putStrLn "wrote other/sim_bh_hist.svg"
  putStrLn $ "simulation strategy mean: " <> show (simStrategyMean sim)
  putStrLn $ "simulation buy-hold mean: " <> show (simBuyHoldMean sim)
  putStrLn $ "simulation strategy std:  " <> show (simStrategyStd sim)
  putStrLn $ "simulation buy-hold std:  " <> show (simBuyHoldStd sim)
  putStrLn $ "strategy beats buy-hold:  " <> show (simBeatFraction sim)
  let annSim x = x / (fromIntegral (P.length returns) / 250.0)
      annStd x = x / sqrt (fromIntegral (P.length returns) / 250.0)
  putStrLn $ "simulation strategy annualised mean: " <> show (annSim (simStrategyMean sim))
  putStrLn $ "simulation buy-hold annualised mean: " <> show (annSim (simBuyHoldMean sim))
  putStrLn $ "simulation strategy annualised std:  " <> show (annStd (simStrategyStd sim))
  putStrLn $ "simulation buy-hold annualised std:  " <> show (annStd (simBuyHoldStd sim))

  putStrLn "--- strategy gradients ---"
  let bucketWeights = [1.0, 0.8, 0.6, 0.4, 0.2, 0.0] :: [Double]
  putStrLn $ "bucket weight gradient:      " <> show (bucketWeightGradient signResult)
  putStrLn $ "threshold sensitivity:       " <> show (thresholdSensitivity returns qs bucketWeights 1.0e-4)
  putStrLn $ "digitize decay sensitivity:  " <> show (digitizeDecaySensitivity returns qs bucketWeights 1.0e-4)
  putStrLn $ "std decay sensitivity (R^2): " <> show (stdDecaySensitivity returns)

  let memReport = memoryReport returns
      annFactor = fromIntegral (P.length returns) / 250.0
      infoResults = infoReport returns
  putStrLn $ "annualisation factor (years): " <> show annFactor
  writeDashboard (modelStats returns) (horizonMagnitudeModel returns) infoResults signResult (P.last stratCum, P.last bhCum, avgWeight, avgTrade, annFactor) sim memReport

  -- sub-sample: results since 2000
  let r2000 = P.filter (\(d, _) -> d >= fromGregorian 2000 1 1) r
      returns2000 = snd <$> r2000
  putStrLn ""
  putStrLn "=== results since 2000 ==="
  putStrLn $ "returns count: " <> show (P.length returns2000)
  modelSummary returns2000

  let signResult2000 = signForecast "r_{t-1}" (delay1 0) qs returns2000
      sigged2000 = P.drop 1000 $ scan (second' sigMealy) r2000
      weights2000 = P.map (\(_, (_, b)) -> (5 - fromIntegral b :: Double) / 5) sigged2000
      rets2000 = P.map (fst . snd) sigged2000 :: [Double]
      stratRets2000 = P.zipWith (*) weights2000 rets2000
      stratCum2000 = scan (dipure (+)) stratRets2000 :: [Double]
      bhCum2000 = scan (dipure (+)) rets2000 :: [Double]
      avgWeight2000 = fold (ma 1) weights2000
      avgTrade2000 = fold (ma 1) (P.map abs (P.zipWith (-) (P.tail weights2000) weights2000))
      ann2000 x = x / (fromIntegral (P.length returns2000) / 250.0)
  putStrLn "--- signal strategy since 2000 ---"
  putStrLn $ "signal strategy final return: " <> show (P.last stratCum2000)
  putStrLn $ "buy & hold final return:      " <> show (P.last bhCum2000)
  putStrLn $ "average signal weight:        " <> show avgWeight2000
  putStrLn $ "average absolute weight change: " <> show avgTrade2000
  putStrLn $ "signal strategy annualised:   " <> show (ann2000 (P.last stratCum2000))
  putStrLn $ "buy & hold annualised:        " <> show (ann2000 (P.last bhCum2000))
  putStrLn $ "scaled strategy annualised:   " <> show (ann2000 (P.last stratCum2000 / avgWeight2000))

  sim2000 <- simulateStrategy signResult2000 bucketWeights 1000 (P.length sigged2000)
  let annStd2000 x = x / sqrt (fromIntegral (P.length returns2000) / 250.0)
  putStrLn "--- simulation since 2000 ---"
  putStrLn $ "simulation strategy mean: " <> show (simStrategyMean sim2000)
  putStrLn $ "simulation buy-hold mean: " <> show (simBuyHoldMean sim2000)
  putStrLn $ "simulation strategy std:  " <> show (simStrategyStd sim2000)
  putStrLn $ "simulation buy-hold std:  " <> show (simBuyHoldStd sim2000)
  putStrLn $ "strategy beats buy-hold:  " <> show (simBeatFraction sim2000)
  putStrLn $ "simulation strategy annualised mean: " <> show (ann2000 (simStrategyMean sim2000))
  putStrLn $ "simulation buy-hold annualised mean: " <> show (ann2000 (simBuyHoldMean sim2000))
  putStrLn $ "simulation strategy annualised std:  " <> show (annStd2000 (simStrategyStd sim2000))
  putStrLn $ "simulation buy-hold annualised std:  " <> show (annStd2000 (simBuyHoldStd sim2000))

  putStrLn "refresh complete"
