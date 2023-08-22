{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Anal where

import Chart
import Control.Monad.State.Lazy
import Data.ByteString.Char8 qualified as C
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

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Anal
-- >>> import FlatParse.Basic
-- >>> import Data.Time.Calendar
-- >>> import Data.FormatN

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

-- * signals

-- | create a (1-day delayed) digitized signal,return pair stream
sigRet :: Mealy a Double -> [Double] -> Double -> Mealy a (a, Int)
sigRet sig qs r = (,) <$> id <*> (sig >>> digitize r qs >>> delay1 0)

-- | mealy scan, dropping the first r scans
scanRet :: Mealy a b -> Int -> Int -> [(c, a)] -> [(c, b)]
scanRet m n r rs = drop r $ scan (second' m) $ taker (n+r) rs

-- | Accumulate the return for n buckets based on a digitized signal
accRetDigits :: Mealy (Double, Int) (Map.Map Int Double)
accRetDigits = M (\(v,k) -> Map.singleton k v) (\m (v,k) -> Map.insertWith (+) k v m) id

ardList :: Int -> (Map.Map Int Double) -> [Double]
ardList n m = fromMaybe zero . (flip Map.lookup m) <$> [0..(n-1)]

qRangeLabel :: [Double] -> [Text]
qRangeLabel = fold
  (M
  (\a -> (a,["< " <> (pack $ show a)]))
  (\(x,labels) this -> (this, labels <> [pack (show x) <> " < x < " <> pack (show this)]))
  (\(x, labels) -> labels <> ["> " <> (pack $ show x)]))

serve :: IO (ChartOptions -> IO Bool, IO ())
serve =
  startChartServerWith defaultSocketConfig $
    chartSocketPage
      & #htmlBody
        .~ element "div" [Attr "class" "container"]
          (element "div" [Attr "class" "row"] $ element "div" [Attr "class" "col"] (element_ "div" [Attr "id" "prettychart"])
          )

dayChart :: [Text] -> [(Day, [Double])] -> ChartOptions
dayChart labels xs = mempty & #charts .~ named "day" cs & #hudOptions .~ h
  where
    cs = zipWith (\c xs' -> LineChart (defaultLineStyle & #color .~ c & #size .~ 0.003) [xify xs']) (palette1 <$> [0 ..]) (List.transpose $ snd <$> xs)
    xaxis = (Priority 5, timeXAxis 8 ((\x -> UTCTime x (P.fromInteger 0)) . fst <$> xs))
    yaxis = (Priority 5, defaultAxisOptions & #place .~ PlaceLeft & #ticks % #style .~ TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend)
    h = defaultHudOptions & #axes .~ [xaxis, yaxis] & #frames %~ (<> [(Priority 30, defaultFrameOptions & #buffer .~ 0.1)]) & #legends .~ leg
    leg =
      [ ( Priority 12,
          defaultLegendOptions
            & over #frame (fmap (set #color white))
            & set #place PlaceRight
            & set (#textStyle % #size) 0.15
            & set #legendCharts (zipWith (\t c -> (t, [c])) labels cs)
        )
      ]

-- | modification of prettychart quantileChart.
quantileChart' :: Int -> [Double] -> [(Day, Double)] -> ChartOptions
quantileChart' n qs r' = c'
  where
    qss = fmap (taker n) $ List.transpose $ scan (Data.Mealy.Quantiles.quantiles 0.99 qs) (snd <$> r')
    c = quantileChart (quantileNames qs) (blendMidLineStyles (length qss) 0.005 (Colour 0.7 0.1 0.3 0.5, Colour 0.1 0.4 0.8 1)) qss
    xaxis = (Priority 5, timeXAxis 8 (taker n $ (\x -> UTCTime x (P.fromInteger 0)) . fst <$> r'))
    yaxis = (Priority 5, defaultAxisOptions & #place .~ PlaceLeft & #ticks % #style .~ TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend)
    c' = c & (#hudOptions % #axes) .~ [xaxis, yaxis]

-- * compounding

runHudCompound ::
  -- | initial canvas
  CanvasBox ->
  -- | huds-chart tuples representing independent chart trees occupying the same canvas space
  [([Hud], ChartTree)] ->
  -- | integrated chart tree
  ChartTree
runHudCompound cb ts = mconcat $ zipWith (\i ct -> group (Just ("compound" <> pack (show i))) [ct]) [(0 :: Int) ..] $ runHudCompoundWith cb ts'
  where
    ts' = zipWith (\db (hs, ct) -> (db, hs, ct)) dbs ts
    dbs = singletonGuard . boxes . foldOf charts' . snd <$> ts

-- | Combine a collection of chart trees that share a canvas box.
runHudCompoundWith ::
  -- | initial canvas
  CanvasBox ->
  -- | databox-huds-chart tuples representing independent chart trees occupying the same canvas space
  [(DataBox, [Hud], ChartTree)] ->
  -- | integrated chart trees
  [ChartTree]
runHudCompoundWith cb ts = zipWith mkTree [(0 :: Int) ..] $ (\x -> x s) <$> zipWith (\cs db -> flip execState (HudChart (cs & over chart' (projectWith cb db)) mempty db)) (snd <$> css) (snd <$> dbs)
  where
    s =
      hss
        & List.sortOn (view #priority . snd)
        & List.groupBy (\a b -> view #priority (snd a) == view #priority (snd b))
        & fmap (closes . fmap (view #hud . snd))
        & sequence
    dbs = zip [(0 :: Int) ..] $ fmap (\(x, _, _) -> x) ts
    hss = mconcat $ fmap (\(i, xs) -> fmap (i,) xs) $ zip [(0 :: Int) ..] (fmap (\(_, x, _) -> x) ts)
    css = zip [(0 :: Int) ..] (fmap (\(_, _, x) -> x) ts)
    mkTree i hc = group (Just ("chart" <> pack (show i))) [view #chart hc] <> group (Just ("hud" <> pack (show i))) [view #hud hc]

-- | Decorate a ChartTree with HudOptions
addHudCompound :: [(HudOptions, ChartTree)] -> [ChartTree]
addHudCompound [] = []
addHudCompound ts@((ho0, cs0) : _) =
  runHudCompoundWith
    (initialCanvas (view #chartAspect ho0) cs0)
    (zip3 dbs hss css)
  where
    hss = fst <$> huds
    dbs = snd <$> huds
    css = (snd <$> ts) <> (blank <$> dbs)
    huds = (\(ho, cs) -> toHuds ho (singletonGuard $ view box' cs)) <$> ts

collapseCompound :: [ChartOptions] -> ChartOptions
collapseCompound [] = mempty
collapseCompound cs@(c0 : _) =
  ChartOptions
    (view #markupOptions c0)
    (mempty & set #chartAspect (view (#hudOptions % #chartAspect) c0))
    (group (Just "compound") $ addHudCompound (zip (view #hudOptions <$> cs) (view #charts <$> cs)))

markupCompoundChartOptions :: [ChartOptions] -> Maybe Markup
markupCompoundChartOptions [] = Nothing
markupCompoundChartOptions cs@(co0 : _) =
  Just $
    header
      (view (#markupOptions % #markupHeight) co0)
      viewbox
      ( markupCssOptions (view (#markupOptions % #cssOptions) co0)
          <> mconcat (markupChartTree <$> csAndHuds)
      )
  where
    viewbox = singletonGuard (foldRect $ mconcat $ maybeToList . view styleBox' <$> csAndHuds)
    csAndHuds = addHudCompound (zip (view #hudOptions <$> cs) (view #charts <$> cs))

encodeCompoundChartOptions :: [ChartOptions] -> C.ByteString
encodeCompoundChartOptions cs =
  maybe mempty (markdown_ Compact Xml) (markupCompoundChartOptions cs)

writeCompoundChartOptions :: FilePath -> [ChartOptions] -> IO ()
writeCompoundChartOptions fp cs = C.writeFile fp (encodeCompoundChartOptions cs)

