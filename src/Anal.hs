{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Anal where

import Chart
import Chart.FlatParse
import Control.Monad.State.Lazy
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Foldable
import Data.FormatN
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Profunctor
import Data.Text (Text, pack, unpack)
import Data.Time
import Data.Time.Calendar
import Data.Time.Format.ISO8601
import FlatParse.Basic
  ( Parser,
    Result (OK),
    char,
    isDigit,
    runParser,
    satisfy,
    strToUtf8,
  )
import GHC.OverloadedLabels
import Lucid qualified as L
import NumHask.Prelude
import Optics.Core
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

-- | Day parser, consumes separator
--
-- >>> runParser dayP "2020-07-28"
-- OK 2020-07-28 ""
dayP :: Parser e Day
dayP = do
  y <- int
  _ <- $(char '-')
  m <- int
  _ <- $(char '-')
  d <- int
  pure $ fromGregorian (fromIntegral y) m d

fredP :: Parser e (Day, Either () Double)
fredP = (,) <$> dayP <*> (($(char ',')) *> (Right <$> double))

runParserError :: Parser e a -> BS.ByteString -> a
runParserError p bs = case runParser p bs of
  OK r _ -> r
  _ -> error "uncaught flatparse error"

-- | Day parser, consumes separator
--
-- >>> runParser dayP' "07/10/1999"
-- OK 1999-10-07 ""
dayP' :: Parser e Day
dayP' = do
  d <- int
  _ <- $(char '/')
  m <- int
  _ <- $(char '/')
  y <- int
  pure $ fromGregorian (fromIntegral y) m d

quoted :: Parser e a -> Parser e a
quoted p = $(char '"') *> p <* $(char '"')

numString :: Parser e String
numString = filter (/= ',') <$> some (satisfy (\x -> isDigit x || (x == '.') || (x == ',')))

auinvP :: Parser e (Day, String)
auinvP = (,) <$> quoted dayP' <*> (($(char ',')) *> quoted numString)

getPricesFred :: IO [(Day, Double)]
getPricesFred = do
  bs <- BS.readFile "other/SP500.csv"
  pure $ [(d, p) | Just (d, Right p) <- runParserMaybe fredP <$> drop 1 (C.lines bs)]

getPricesAuinv :: FilePath -> IO [(Day, Double)]
getPricesAuinv fp = do
  bs <- BS.readFile fp
  let pricesString = [(d, p) | Just (d, p) <- runParserMaybe auinvP <$> drop 1 (C.lines bs)]
  pure [(d, p) | (d, Just p) <- second (runParserMaybe double . strToUtf8) <$> pricesString]

getPricesAuinvs :: IO [(Day, Double)]
getPricesAuinvs = do
  p1 <- getPricesAuinv "other/S&P 500 Historical Data.csv"
  p4 <- getPricesAuinv "other/S&P 500 Historical Data (4).csv"
  p5 <- getPricesAuinv "other/S&P 500 Historical Data (5).csv"
  let pricesMap = Map.unions [Map.fromList p1, Map.fromList p4, Map.fromList p5]
  pure $ Map.toList pricesMap

getPrices :: IO [(Day, Double)]
getPrices = do
  pFred <- getPricesFred
  pAuinv <- getPricesAuinvs
  pure $ Map.toList $ Map.union (Map.fromList pFred) (Map.fromList pAuinv)

getOriginalReturns :: IO [(Day, Double)]
getOriginalReturns = do
  p <- getPrices
  pure $ drop 1 $ scan ret p

makeReturns :: IO ()
makeReturns = do
  p <- getPrices
  writeReturns $ drop 1 $ scan ret p

writeReturns :: [(Day, Double)] -> IO ()
writeReturns r =
  writeFile "other/returns.csv"
    . unlines
    $ (\(d, r') -> d <> "," <> unpack r')
      . bimap (formatShow iso8601Format) (fixed (Just 6))
      <$> r

dayReturnP :: Parser e (Day, Double)
dayReturnP = (,) <$> dayP <*> ($(char ',') *> signed double)

getReturns :: IO [(Day, Double)]
getReturns = do
  bs <- BS.readFile "other/returns.csv"
  pure $ runParserError dayReturnP <$> C.lines bs

-- return scan
ret :: Mealy (Day, Double) (Day, Double)
ret = second' ((\p p' -> log (p / p')) <$> id <*> delay1 undefined)

-- difference mealy
diff1 :: (a -> a -> b) -> a -> Mealy a b
diff1 f a0 = f <$> id <*> delay1 a0

count :: (Ord a) => [a] -> Map.Map a Int
count = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty

sum' :: (Ord a) => [(a, Double)] -> Map.Map a Double
sum' = foldl' (\m (k, v) -> Map.insertWith (+) k v m) Map.empty

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

signalize :: Double -> [Double] -> Mealy Double Double
signalize r qs' =
  (\x -> fromIntegral x / fromIntegral (length qs' + 1)) <$> digitize r qs'

defaultQuantiles :: [Double]
defaultQuantiles = (0.1 *) <$> [1 .. 9]

serve :: IO (ChartOptions -> IO Bool, IO ())
serve =
  startChartServerWith defaultSocketConfig $
    chartSocketPage
      & #htmlBody
        .~ divClass_
          "container"
          ( mconcat
              [ divClass_ "row" $ divClass_ "col" (L.with L.div_ [L.id_ "prettychart"] mempty)
              ]
          )

accretChart :: [(Day, Double)] -> ChartOptions
accretChart xs = mempty & #charts .~ named "line" [c] & #hudOptions .~ h :: ChartOptions
  where
    c = simpleLineChart 0.01 (palette1 2) (snd <$> xs)
    xaxis = (Priority 5, timeXAxis 8 ((\x -> UTCTime x (P.fromInteger 0)) . fst <$> xs))
    yaxis = (Priority 5, defaultAxisOptions & #place .~ PlaceLeft & #ticks % #style .~ TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend)
    h = defaultHudOptions & #titles .~ [(Priority 8, defaultTitle "accumulated return" & set #place PlaceLeft & set (#style % #size) 0.06 & set #buffer 0.1)] & #axes .~ [xaxis, yaxis] & #frames %~ (<> [(Priority 30, defaultFrameOptions & #buffer .~ 0.1)])

quantileChart' :: Int -> [Double] -> [(Day, Double)] -> ChartOptions
quantileChart' n qs r' = c'
  where
    qss = fmap (taker n) $ List.transpose $ scan (Data.Mealy.Quantiles.quantiles 0.99 qs) (snd <$> r')
    c = quantileChart (quantileNames qs) (blendMidLineStyles (length qss) 0.005 (Colour 0.7 0.1 0.3 0.5, Colour 0.1 0.4 0.8 1)) qss
    xaxis = (Priority 5, timeXAxis 8 (taker n $ (\x -> UTCTime x (P.fromInteger 0)) . fst <$> r'))
    yaxis = (Priority 5, defaultAxisOptions & #place .~ PlaceLeft & #ticks % #style .~ TickRound (FormatN FSPercent (Just 2) 4 True True) 6 TickExtend)
    c' = c & (#hudOptions % #axes) .~ [xaxis, yaxis]

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
            & set #content (zipWith (\t c -> (t, [c])) labels cs)
        )
      ]

listify :: Mealy a b -> Mealy [a] [b]
listify (M sExtract sStep sInject) = M extract step inject
  where
    extract = fmap sExtract
    step = zipWith sStep
    inject = fmap sInject

-- | A chart drawing quantiles of a time series
--
-- ![digitChart example](other/digit.svg)
digitCharts ::
  [UTCTime] ->
  [[Double]] ->
  [Text] ->
  [Text] ->
  ChartOptions
digitCharts utcs xss ylabels labels =
  mempty & #charts .~ named "scatters" cs & #hudOptions .~ h
  where
    h =
      defaultHudOptions
        & #axes .~ [(Priority 5, timeXAxis 8 utcs), (Priority 5, decileYAxis ylabels)]
        & #legends .~ leg
    cs =
      zipWith
        ( \c xs ->
            GlyphChart
              ( defaultGlyphStyle
                  & #color .~ palette1 c
                  & #shape .~ gpalette List.!! c
                  & #size .~ 0.01
              )
              (xify xs)
        )
        [0 ..]
        (List.transpose xss)
    leg =
      [ ( Priority 12,
          defaultLegendOptions
            & over #frame (fmap (set #color white))
            & set #place PlaceRight
            & set (#textStyle % #size) 0.15
            & set #content (zipWith (\t c -> (t, [c])) labels cs)
        )
      ]

decileYAxis :: [Text] -> AxisOptions
decileYAxis labels =
  defaultAxisOptions
    & #ticks % #style .~ TickPlaced (zip ((+ 0.5) <$> [0 ..]) labels)
    & #ticks % #ltick .~ Nothing
    & #ticks % #ttick %~ fmap (first (#size .~ 0.03))
    & #place .~ PlaceLeft

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
      ( [markupCssOptions (view (#markupOptions % #cssOptions) co0)]
          <> mconcat (markupChartTree <$> csAndHuds)
      )
  where
    viewbox = singletonGuard (foldRect $ mconcat $ maybeToList . view styleBox' <$> csAndHuds)
    csAndHuds = addHudCompound (zip (view #hudOptions <$> cs) (view #charts <$> cs))

encodeCompoundChartOptions :: [ChartOptions] -> C.ByteString
encodeCompoundChartOptions cs =
  maybe mempty encodeMarkup (markupCompoundChartOptions cs)

writeCompoundChartOptions :: FilePath -> [ChartOptions] -> IO ()
writeCompoundChartOptions fp cs = C.writeFile fp (encodeCompoundChartOptions cs)
