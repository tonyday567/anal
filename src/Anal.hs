{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

module Anal where

import Chart
import Data.Map qualified as Map
import Data.Maybe
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Profunctor
import Data.Text (Text, pack, intercalate)
import Data.Time
import GHC.OverloadedLabels
import MarkupParse
import NumHask.Prelude hiding (fold)
import Optics.Core hiding (element)
import Prettychart
import Web.Rep
import Harpie.Array qualified as A
import Data.List qualified as List
import NumHask.Space
import Prelude qualified as P

-- import Data.FormatN

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

qsRangeLabel :: [Double] -> [Text]
qsRangeLabel =
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

-- | line chart with verticle axis & no guideline ticks
lchart :: Maybe Place -> Colour -> [Double] -> ChartOptions
lchart p c xs = mempty & #chartTree .~ unnamed [LineChart (defaultLineStyle & #color .~ c & #size .~ 0.003) [xify xs]] & #hudOptions % #axes .~ maybe [] (\p' -> [Priority 5 $ defaultYAxisOptions & #place .~ p' & #ticks % #lineTick .~ Nothing & #ticks % #tick .~ TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend]) p & #hudOptions %~ colourHudOptions (const c)

-- helpers
toCT :: ChartOptions -> ChartTree
toCT co = view #chartTree $ forgetHud co

accret :: [(Day, Double)] -> [(Day, Double)]
accret r = scan (second' (dipure (+))) r

rebase :: Int -> Int -> [Double] -> [Double]
rebase n n' xs = fmap (/ head xs') xs'
  where
    xs' = reindex n n' id xs

rebaseSub :: Int -> Int -> [Double] -> [Double]
rebaseSub n n' xs = fmap (\x -> x - head xs') xs'
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

declutter :: Place -> Int -> ChartOptions -> ChartOptions
declutter place col x = x & set (#hudOptions % #axes % each % #item % #ticks % #lineTick) Nothing & set (#hudOptions % #legends) [] & over (#hudOptions % #axes) (set (each % #item % #place) place . drop 1) & set (#chartTree % charts' % each % #chartStyle % #color) (palette col)

data SdParams = SdParams {
  qs :: [Double],
  decayStd :: Double,
  decayBeta1 :: Double,
  decayQ :: Double,
  sigWeights :: [Double]
} deriving (Eq, Show, Generic)

defaultSdParams :: SdParams
defaultSdParams = SdParams [0.1,0.5,0.8] 0.95 0.99 0.996 [-1,1,1,-1]

data SdDashboardConfig = SdDashboardConfig {
  n :: Int,
  dropN :: Int,
  sdParams :: SdParams,
  dashHud :: HudOptions,
  chartHuds :: HudOptions
} deriving (Eq, Show, Generic)

defaultSdDashboardConfig :: SdDashboardConfig
defaultSdDashboardConfig = SdDashboardConfig 1000 100 defaultSdParams mempty mempty

fs :: HudOptions
fs = mempty & over #frames (<> [(Priority 30 $ FrameOptions (Just (border 0.01 (dark & set opac' 0.3))) HudStyleSection 0.1), (Priority 31 $ FrameOptions (Just (border 0.01 (dark & set opac' 0.3))) HudStyleSection 0.1)])

fixl :: Int -> [Double] -> Text
fixl n xs = xs & fmap (fixed (Just n)) & intercalate "," & (\x -> "[" <> x <> "]")

dashboardTextChart :: SdDashboardConfig -> ChartOptions
dashboardTextChart c = mempty @ChartOptions & set #chartTree (pageChart 0.1 0.15 (Point 0.05 0) ts)
  where
    ts = ["std decay = " <> fixed (Just 2) (view (#sdParams % #decayStd) c), "beta1 decay = " <> fixed (Just 2) (view (#sdParams % #decayBeta1) c), "quantile decay = " <> fixed (Just 2) (view (#sdParams % #decayQ) c), "quantiles = " <> fixl 2 (view (#sdParams % #qs) c), "signal weights = " <> fixl 2 (view (#sdParams % #sigWeights) c)]

irTextChart :: Double -> Double -> ChartOptions
irTextChart irM irSig = mempty @ChartOptions & set #chartTree (pageChart 0.1 0.15 (Point 0.05 0) ts)
  where
    ts = ["ir signal = " <> fixed (Just 3) irSig, "ir market = " <> fixed (Just 3) irM]

textChart :: Maybe (Rect Double) -> Double -> Style -> [Text] -> ChartTree
textChart page gap s ts = named "textChart" ((zipWith (\x t -> TextChart s [(t, Point 0 (x*gap))]) [0..] (reverse ts)) <> maybe mempty (\r -> pure $ RectChart clear [r]) page)

pageChart :: Double -> Double -> Point Double -> [Text] -> ChartTree
pageChart size gap margin ts = textChart (Just $ addPoint (Point (0.5 - _x margin) (-0.5 + fromIntegral (length ts) * gap + _y margin)) one) gap (defaultTextStyle & set #size size & set #textAnchor AnchorStart) ts

sdDash :: SdDashboardConfig -> [(Day, Double)] -> ChartOptions
sdDash cfg r = bool chart'' (error "bucket returns wrong") (0.00001 < (sum (abs <$> err0)))
  where
    n = view #n cfg
    dropN = view #dropN cfg
    qs = view (#sdParams % #qs) cfg
    decayStd = view (#sdParams % #decayStd) cfg
    decayBeta1 = view (#sdParams % #decayBeta1) cfg
    decayQ = view (#sdParams % #decayQ) cfg
    sigw = A.asArray $ view (#sdParams % #sigWeights) cfg

    -- indexed return
    ri = r & fmap snd & zip [0..]
    -- timestamp as a UTC
    days = r & fmap fst & reindex n dropN id & fmap (\x -> UTCTime x (P.fromInteger 0))

    -- mealy: quantile sd
    msdqs = std decayStd >>> digitize decayQ qs >>> delay1 0
    -- mealy: returns bucketed for each sd quantile
    mretb = (\x rb -> A.modify [x] (const rb) (A.konst [length qs + 1] 0.0)) <$> msdqs <*> id
    -- mealy: beta os sd on return
    msdbeta = second' (std decayStd) >>> beta1 (ma decayBeta1) >>> delay1 0
    -- mealy: signal, given quantile
    msig ws = fmap (A.index ws . pure) msdqs
    -- mealy: return signal
    msigret ws = (*) <$> msig ws <*> id
    -- mealy: information ratios
    mIr d = (\m s -> (250*m)/(250**0.5 * s)) <$> ma d <*> std d

    -- calcs
    ret = r & fmap snd & reindex n dropN id & A.asArray
    qret = r & fmap snd & reindex n dropN (scan mretb) & A.asArray
    err0 = A.zipWith (\a b -> a - sum b) ret qret
    -- quantile sd
    sdqs = r & fmap snd & reindex n dropN (scan msdqs)
    -- accumulated return
    retacc = ret & A.arrayAs & scan (dipure (+)) & A.asArray
    -- sd
    retsd = r & fmap snd & reindex n dropN (scan (std decayStd)) & A.asArray
    -- beta of sd
    retsdbeta = ri & reindex n dropN (scan msdbeta) & A.asArray
    -- signal
    sig ws = r & fmap snd & reindex n dropN (scan (msig ws)) & A.asArray
    -- signal return
    sigret ws = r & fmap snd & reindex n dropN (scan (msigret ws)) & A.asArray
    accsigret ws = sigret ws & A.arrayAs & scan (dipure (+)) & A.asArray

    -- return chart
    rcsl = [ lchart Nothing (palette 0) (A.arrayAs retacc), lchart (Just PlaceLeft) (palette 1) (A.arrayAs retsd), lchart Nothing (palette 2) (A.arrayAs retsdbeta)]

    rcs = compoundMerge rcsl & set (#hudOptions % #legends) [(Priority 20 $ defaultLegendOptions & set #place PlaceBottom & set #numStacks 3 & set #alignCharts NoAlign & set #legendCharts (zipWith (\t co -> (t, foldOf (#chartTree % charts') co)) (List.reverse ["accret", "sd", "beta sd"]) (List.reverse rcsl)))]

    -- sd quantiles
    sdc = digitChart (defaultDigitChartStyle & set #decileAxisStyle (Just $ qsAxisStyle qs)) days sdqs

    sm = A.arrayAs $ fmap A.arrayAs $ A.extracts [1] $ A.couple 0 (accsigret sigw) retacc

    -- sigmnal return chart
    src = utcLineChart (defaultUtcLineChartStyle) (["sig","market"]) (zip days sm)

    -- ir chart
    irc = irTextChart (fold (mIr 1) (A.arrayAs ret)) (fold (mIr 1) (A.arrayAs (sigret sigw)))

    -- signal chart
    sigc = simpleRectChart (A.arrayAs $ sig sigw) (blob (grey 0.5 0.2)) & set (#hudOptions % #axes) []

    bucksig = compoundMerge [sdc, sigc]
    irc' = projectChartTree (aspect 5) (toCT irc)
    tc' = projectChartTree (aspect 1) (hori 2 [(toCT (dashboardTextChart cfg)), irc'])
    rcs' = projectChartTree one (toCT rcs)
    sdc' = projectChartTree one (toCT $ bucksig & set (#markupOptions % #chartAspect) (CanvasAspect 1.5))
    src' = projectChartTree one (toCT $ src & set (#markupOptions % #chartAspect) (CanvasAspect 1.5))
    chart'' = mempty & #chartTree .~ stack' 2 0.1 ([tc', rcs', sdc', src']) & set (#hudOptions % #titles) [Priority 5 (defaultTitleOptions "sd dashboard" & set (#style % #size) 0.06)]
