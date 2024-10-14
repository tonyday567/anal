{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Anal where

import Chart
import Data.Foldable hiding (fold)
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Profunctor
import Data.Text (Text, pack)
import Data.Time
import GHC.OverloadedLabels
import MarkupParse
import NumHask.Prelude hiding (fold)
import Optics.Core hiding (element)
import Prettychart
import Web.Rep
import Prelude qualified as P
import NumHask.Space.Time

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Anal
-- >>> import FlatParse.Basic
-- >>> import Data.Time.Calendar

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

-- * signals

-- | create a (1-day delayed) digitized signal,return pair stream
sigRet :: Mealy a Double -> [Double] -> Double -> Mealy a (a, Int)
sigRet sig qs r = (,) <$> id <*> (sig >>> digitize r qs >>> delay1 0)

-- | mealy scan of a Day, Return tuple, dropping the first r scans
scanRet :: Mealy a b -> Int -> Int -> [(c, a)] -> [(c, b)]
scanRet m n r rs = drop r $ scan (second' m) $ taker (n + r) rs

-- | Accumulate the return for n buckets based on a digitized signal
accRetDigits :: Mealy (Double, Int) (Map.Map Int Double)
accRetDigits = M (\(v, k) -> Map.singleton k v) (\m (v, k) -> Map.insertWith (+) k v m) id

ardList :: Int -> Map.Map Int Double -> [Double]
ardList n m = fromMaybe zero . flip Map.lookup m <$> [0 .. (n - 1)]

qRangeLabel :: [Double] -> [Text]
qRangeLabel =
  fold
    ( M
        (\a -> (a, ["< " <> (pack $ show a)]))
        (\(x, labels) this -> (this, labels <> [pack (show x) <> " < x < " <> pack (show this)]))
        (\(x, labels) -> labels <> ["> " <> (pack $ show x)])
    )

serve :: IO (ChartOptions -> IO Bool, IO ())
serve =
  startChartServerWith defaultSocketConfig $
    chartSocketPage Nothing
      & #htmlBody
      .~ element
        "div"
        [Attr "class" "container"]
        ( element "div" [Attr "class" "row"] $ element "div" [Attr "class" "col"] (element_ "div" [Attr "id" "prettychart"])
        )

dayChart :: [Text] -> [(Day, [Double])] -> ChartOptions
dayChart labels xs = dayChartWith PosInnerOnly (Just "%b %y") 6 labels xs

dayChartWith :: PosDiscontinuous -> Maybe Text -> Int -> [Text] -> [(Day, [Double])] -> ChartOptions
dayChartWith pos fmt nticks labels xs = mempty & #chartTree .~ named "day" cs & #hudOptions .~ h
  where
    cs = zipWith (\c xs' -> LineChart (defaultLineStyle & #color .~ c & #size .~ 0.003) [xify xs']) (palette <$> [0 ..]) (List.transpose $ snd <$> xs)
    xaxis = Priority 5 $ timeXAxisWith pos fmt nticks ((\x -> UTCTime x (P.fromInteger 0)) . fst <$> xs)

    yaxis = Priority 5 $ defaultYAxisOptions & #place .~ PlaceLeft & #ticks % #tick .~ TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend
    h = defaultHudOptions & #axes .~ [xaxis, yaxis] & #frames %~ (<> [Priority 30 $ defaultFrameOptions & #buffer .~ 0.1]) & #legends .~ leg
    leg =
      [ Priority 12 $
            defaultLegendOptions
              & set #place PlaceBottom
              & set #frame (Just $ border 0.01 light)
              & set (#textStyle % #size) 0.2
              & set #legendCharts (zipWith (\t c -> (t, [c])) labels cs)
      ]

dayAxis :: [Day] -> Priority AxisOptions
dayAxis ds = Priority 5 $ timeXAxis 8 ((\x -> UTCTime x (P.fromInteger 0)) <$> ds)

dayAxisWith :: PosDiscontinuous -> Maybe Text -> Int -> [Day] -> Priority AxisOptions
dayAxisWith pos fmt nticks ds = Priority 5 $ timeXAxisWith pos fmt nticks ((\x -> UTCTime x (P.fromInteger 0)) <$> ds)

-- | legend constructed from different charts
leg' :: [Text] -> [Chart] -> ChartOptions
leg' labels cs =
  mempty
    & #hudOptions
    % #legends
    .~ [ Priority 12 $
             defaultLegendOptions
               & over #frame (fmap (set #color white))
               & set #place PlaceRight
               & set (#textStyle % #size) 0.15
               & set #legendCharts (zipWith (\t c -> (t, [c])) labels cs)
       ]

-- | line chart with vertical axis, no guideline ticks
lchart :: Maybe Place -> Colour -> [Double] -> ChartOptions
lchart p c xs = mempty & #chartTree .~ unnamed [LineChart (defaultLineStyle & #color .~ c & #size .~ 0.003) [xify xs]] & #hudOptions % #axes .~ maybe [] (\p' -> [Priority 5 $ defaultYAxisOptions & #place .~ p' & #ticks % #lineTick .~ Nothing & #ticks % #tick .~ TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend]) p & #hudOptions %~ colourHudOptions (const c)

-- | modification of prettychart quantileChart.
quantileChart' :: Int -> [Double] -> [(Day, Double)] -> ChartOptions
quantileChart' n qs r' = c'
  where
    qss = fmap (taker n) $ List.transpose $ scan (Data.Mealy.Quantiles.quantiles 0.99 qs) (snd <$> r')
    c = quantileChart (quantileNames qs) (blendMidLineStyles (length qss) 0.005 (Colour 0.7 0.1 0.3 0.5, Colour 0.1 0.4 0.8 1)) qss
    xaxis = Priority 5 $ timeXAxis 8 (taker n $ (\x -> UTCTime x (P.fromInteger 0)) . fst <$> r')
    yaxis = Priority 5 $ defaultYAxisOptions & #place .~ PlaceLeft & #ticks % #tick .~ TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend
    c' = c & (#hudOptions % #axes) .~ [xaxis, yaxis]

-- helpers
toCT :: ChartOptions -> ChartTree
toCT co = view #chartTree $ forgetHud co

accret :: [(Day, Double)] -> [(Day, Double)]
accret r = scan (second' (dipure (+))) r

rebase :: Int -> Int -> [Double] -> [Double]
rebase n n' xs = fmap (/ head xs') xs'
  where
    xs' = reindex n n' id xs

reindex :: Int -> Int -> ([a] -> [b]) -> [a] -> [b]
reindex n n' f xs = drop n' $ f $ taker (n + n') xs

-- | Stack a list of tree charts horizontally, then vertically (proceeding downwards which is opposite to the usual coordinate reference system but inutitively the way people read charts)
stack' :: Int -> Double -> [ChartTree] -> ChartTree
stack' _ _ [] = mempty
stack' n gap cs = vert gap (reverse $ hori gap <$> group' cs [])
  where
    group' [] acc = reverse acc
    group' x acc = group' (drop n x) (take n x : acc)
