
# anal

[![img](https://img.shields.io/hackage/v/anal.svg)](https://hackage.haskell.org/package/anal)
[![img](https://github.com/tonyday567/anal/workflows/haskell-ci/badge.svg)](https://github.com/tonyday567/anal/actions?query=workflow%3Ahaskell-ci)

analysis: the prefix


# Imports

    :r
    :set -Wno-type-defaults
    :set -Wno-name-shadowing
    :set -XOverloadedLabels
    :set -XOverloadedStrings
    :set -XRebindableSyntax
    :set -XTupleSections
    :set -Wno-x-partial
    import Anal
    import Anal.Returns
    import Control.Monad
    import qualified FlatParse.Basic as FP
    import Data.Time
    import Data.Time.Calendar
    import qualified Data.ByteString as BS
    import qualified Data.ByteString.Char8 as C
    import Data.Mealy
    import Data.Profunctor
    import Data.Maybe
    import Data.Bifunctor
    import NumHask.Prelude hiding (fold)
    import qualified Data.Map as Map
    import qualified Data.Text as Text
    import Prettychart
    import Chart
    import qualified Prelude as P
    import GHC.OverloadedLabels
    import Optics.Core
    import Control.Category ((>>>))
    import Control.Applicative
    import Chart.Compound
    import Faker.Lorem
    print "imports loaded"
    r <- getReturns
    length r
    (display, quit) <- serve
    disp x = display $ x & set (#markupOptions % #markupHeight) (Just 250) & set (#hudOptions % #frames % ix 1 % #item % #buffer) 0.1

    Build profile: -w ghc-9.10.1 -O1
    In order, the following will be built (use -v for more details):
     - anal-0.0.4 (interactive) (lib) (first run)
    Preprocessing library for anal-0.0.4...
    GHCi, version 9.10.1: https://www.haskell.org/ghc/  :? for help
    [1 of 2] Compiling Anal             ( src/Anal.hs, interpreted )
    [2 of 2] Compiling Anal.Returns     ( src/Anal/Returns.hs, interpreted )
    Ok, two modules loaded.
    Ok, two modules reloaded.
    "imports loaded"
    10897
    Setting phasers to stun... (port 9160) (ctrl-c to quit)

    import Chart.Examples
    disp lineExample

    True


# analysis


### Accumulated return

    space1 (fst <$> r) :: Maybe (Range Day)
    accret = scan (second' (dipure (+))) r
    decay = 0.01
    rs = snd <$> r
    xma = scan (ma decay) rs
    xstd = scan (std decay) rs
    disp $ dayChart ["accumulated return"] (fmap (second (:[])) (taker 200 accret))

    Just Range 1980-01-02 2023-03-17
    True


### median versus average

1.  mean versus 40th, 50th, 60th quantiles

        mvq = (second' ((\a b -> a:(b!!1-a):b) <$> ma 0.99 <*> Data.Mealy.Quantiles.quantiles 0.99 [0.4,0.5,0.6]))
        
        c = dayChart ["mean", "skew", "40th", "median", "60th"] (drop 1000 $ scan mvq (taker 2000 r))
        disp c
        writeChartOptions "other/mvq.svg" c
    
    ![img](other/mvq.svg)

2.  medium minus mean

        mvm = second' ((\a b -> b - a) <$> ma 0.99 <*> median 0.99)
        mvmChart = dayChart ["median - mean"] (drop 1000 $ fmap (second (:[])) $ scan mvm (taker 2000 r))
        disp mvmChart
        writeChartOptions "other/mvm.svg" mvmChart
    
    ![img](other/mvm.svg)


### digitize median versus mean

    qs = [0.2,0.4,0.6,0.8]
    mvmd = ((-) <$> median 0.99 <*> ma 0.99) >>> digitize 0.996 qs
    d = drop 1000 $ scan (second' mvmd) (taker 2000 r)
    mvmdChart = digitChart ((\x -> UTCTime x (P.fromInteger 0)) . fst <$> d) (fromIntegral . snd <$> d) (quantileNames qs)
    disp mvmdChart
    writeChartOptions "other/mvmd.svg" mvmdChart

![img](other/mvmd.svg)


# trading signals

(Today&rsquo;s return, Yesterday&rsquo;s signal)

    n = 2000
    pren = 1000
    mvmRaw = (\a b -> a-b) <$> median 0.99 <*> ma 0.99
    qs = [0.1,0.4,0.5,0.6,0.9]
    d = scanRet (sigRet mvmRaw qs 0.996) n pren r
    take 10 d

    [(2015-04-09,(4.448e-3,0)),(2015-04-10,(5.189e-3,0)),(2015-04-13,(-4.592e-3,0)),(2015-04-14,(1.628e-3,0)),(2015-04-15,(5.135e-3,0)),(2015-04-16,(-7.79e-4,0)),(2015-04-17,(-1.1376e-2,0)),(2015-04-20,(9.193e-3,1)),(2015-04-21,(-1.482e-3,1)),(2015-04-22,(5.075e-3,1))]

    Data.Mealy.fold countM (snd . snd <$> d)

    fromList [(0,225),(1,550),(2,217),(3,263),(4,511),(5,234)]

cumulative return for each bucket

    n = 1000
    ndrop = 0
    mvmRaw = (\a b -> a-b) <$> median 0.99 <*> ma 0.99
    qs = [0.1,0.4,0.5,0.6,0.9]
    c = dayChart (qRangeLabel qs) (scanRet (fmap (ardList 6) (sigRet mvmRaw qs 0.996 >>> accRetDigits)) n ndrop r)
    c = c & #markupOptions % renderStyle .~ Indented 4
    writeChartOptions "other/arc.svg" c
    disp c

![img](other/arc.svg)


# standard deviation

It looks like the market goes up when sd is declining.

What is the gradient of a moving statistic?


## ToDo not working

    accret = scan (second' (dipure (+))) r
    rebase xs = zip (fst <$> xs) (fmap (/head (snd <$> xs)) (snd <$> xs))
    
    accChart = dayChart ["accumulated return"] (fmap (second (:[])) (rebase $ drop dropN $ taker (n+dropN) accret)) & set (#hudOptions % #legends) []
    accChart' = accChart & over (#hudOptions % #axes) (fmap (second (set ( #ticks % #ltick ) Nothing))) & over (#charts % charts') (fmap (colourChart (const (palette1 2)))) & set (#hudOptions % #legends) []
    
    sdChart = dayChart ["std"] $ second (:[]) <$> scanRet (std decay) n dropN r
    sdChart' = sdChart & #hudOptions .~ (mempty & #axes .~ (view (#hudOptions % #axes) sdChart & (\x -> (List.!!) x 1) & second (set #place PlaceRight) & (:[]))) & over (#hudOptions % #axes) (fmap (second (set ( #ticks % #ltick ) Nothing)))
    
    compChart = compoundMerge [accChart', sdChart']
    compChart' = compChart & set (#hudOptions % #legends) [(Priority 20,defaultLegendOptions & set #legendCharts (zipWith (\t co -> (t, foldOf (#charts % charts') co)) ["return", "sd"] [accChart', sdChart']))]
    writeChartOptions "other/sd.svg" compChart'
    
    disp compChart'

![img](other/sd.svg)


## ToDo gradient of sd

    stdBeta = dayChart ["std"] $ second (:[]) <$> scanRet (second' (std decayStd) >>> beta1 (ma decayBeta1)) n dropN (zip (fst <$> r) (zip [0..] (snd <$> r)))
    stdBeta' = stdBeta & over (#hudOptions % #axes) (fmap (second (set ( #ticks % #ltick ) Nothing))) & over (#charts % charts') (fmap (colourChart (const (palette1 2))))  & set (#hudOptions % #legends) []
    
    betaSdChart = compoundMerge [sdChart', stdBeta']
    betaSdChart' = betaSdChart & set (#hudOptions % #legends) [(Priority 20,defaultLegendOptions & set #legendCharts (zipWith (\t co -> (t, foldOf (#charts % charts') co)) ["beta of sd", "sd"] [stdBeta', sdChart']))]
    disp betaSdChart'

    <interactive>:115:69: error: [GHC-88464]
        Variable not in scope: decayStd :: Double
    
    <interactive>:115:93: error: [GHC-88464]
        Variable not in scope: decayBeta1 :: Double
    
    <interactive>:115:108: error: [GHC-88464]
        Variable not in scope: dropN :: Int
        Suggested fix:
          Perhaps use one of these:
            ‘drop’ (imported from NumHask.Prelude),
            ‘Map.drop’ (imported from Data.Map),
            ‘Text.drop’ (imported from Data.Text)
    <interactive>:116:12: error: [GHC-88464]
        Variable not in scope: stdBeta
    
    <interactive>:116:133: error: [GHC-88464]
        Variable not in scope: colourChart :: (b1 -> a7) -> Chart -> Chart
    
    <interactive>:116:153: error: [GHC-88464]
        Variable not in scope: palette1 :: t0 -> a7
        Suggested fix:
          Perhaps use one of these:
            ‘palette’ (imported from Chart), ‘paletteO’ (imported from Chart),
            ‘paletteR’ (imported from Chart)
    <interactive>:118:30: error: [GHC-88464]
        Variable not in scope: sdChart' :: ChartOptions
    
    <interactive>:118:40: error: [GHC-88464]
        Variable not in scope: stdBeta' :: ChartOptions
    <interactive>:119:16: error: [GHC-88464]
        Variable not in scope: betaSdChart
    
    <interactive>:119:191: error: [GHC-88464]
        Variable not in scope: stdBeta'
    
    <interactive>:119:201: error: [GHC-88464]
        Variable not in scope: sdChart'
    <interactive>:120:6: error: [GHC-88464]
        Variable not in scope: betaSdChart' :: ChartOptions


### ToDo digitize beta

    qs = [0.2,0.4,0.6,0.8]
    qBeta = (second' (std d) >>> beta1 (ma 0.95)) >>> digitize 0.996 qs
    :t qBeta
    d = drop dropN $ scan (second' qBeta) (taker (n+dropN) $ (zip (fst <$> r) (zip [0..] (snd <$> r))))
    betaSdDigitChart = digitChart ((\x -> UTCTime x (P.fromInteger 0)) . fst <$> d) (fromIntegral . snd <$> d) (quantileNames qs)
    disp betaSdDigitChart

    <interactive>:124:51: error: [GHC-83865]
        • Couldn't match type ‘Double’ with ‘[(Day, (Double, Int))]’
          Expected: Mealy [(Day, (Double, Int))] Int
            Actual: Mealy Double Int
        • In the second argument of ‘(>>>)’, namely ‘digitize 0.996 qs’
          In the expression:
            (second' (std d) >>> beta1 (ma 0.95)) >>> digitize 0.996 qs
          In an equation for ‘qBeta’:
              qBeta = (second' (std d) >>> beta1 (ma 0.95)) >>> digitize 0.996 qs
    <interactive>:1:1: error: [GHC-88464] Variable not in scope: qBeta
    <interactive>:126:10: error: [GHC-88464]
        Variable not in scope: dropN :: Int
        Suggested fix:
          Perhaps use one of these:
            ‘drop’ (imported from NumHask.Prelude),
            ‘Map.drop’ (imported from Data.Map),
            ‘Text.drop’ (imported from Data.Text)
    
    <interactive>:126:32: error: [GHC-88464]
        Variable not in scope: qBeta :: Mealy (a0, Double) b
    
    <interactive>:126:49: error: [GHC-88464]
        Variable not in scope: dropN :: Int
        Suggested fix:
          Perhaps use one of these:
            ‘drop’ (imported from NumHask.Prelude),
            ‘Map.drop’ (imported from Data.Map),
            ‘Text.drop’ (imported from Data.Text)
    <interactive>:127:82: error: [GHC-39999]
        • No instance for ‘FromIntegral Double (Double, Int)’
            arising from a use of ‘fromIntegral’
        • In the first argument of ‘(.)’, namely ‘fromIntegral’
          In the first argument of ‘(<$>)’, namely ‘fromIntegral . snd’
          In the second argument of ‘digitChart’, namely
            ‘(fromIntegral . snd <$> d)’
    <interactive>:128:6: error: [GHC-88464]
        Variable not in scope: betaSdDigitChart :: ChartOptions

    :t d

    d :: [(Day, Int)]

Skew away from upper quantiles

    Data.Mealy.fold countM (snd <$> d)

    fromList [(0,892),(1,813),(2,1191),(3,1072),(4,1032)]

    acc = dayChartRhs (fmap (second (:[])) (rebase $ drop dropN $ taker (n+dropN) accret))
    acc' = acc & over (#hudOptions % #axes) (fmap (second (set ( #ticks % #ltick ) Nothing))) & over (#charts % charts') (fmap (colourChart (const (palette1 4))))
    c = dayChart (qRangeLabel qs) (scanRet (fmap (ardList 6) (sigRet mvmRaw qs 0.996 >>> accRetDigits)) n ndrop r)
    
    disp $ compoundMerge [c, acc']

    :t d

    d :: [(Day, Int)]

    n = 2000
    dropN = 100
    qs = [0.1, 0.9] :: [Double]
    decayStd = 0.95
    decayBeta1 = 0.95
    decayQ = 0.95
    qBeta' = (first snd) <$> ((,) <$> id <*> (second' (std decayStd) >>> beta1 (ma decayBeta1) >>> digitize decayQ qs >>> delay1 0))
    buckets = fmap (ardList ((length qs :: Int) + 1)) (qBeta' >>> accRetDigits)
    xs = (drop dropN $ scan (second' buckets) (taker (n+dropN) $ (zip (fst <$> r) (zip [0..] (snd <$> r)))))
    bucketChart = dayChart (qRangeLabel qs) xs
    disp bucketChart

    > True


### ToDo vert

    :t stack 2 0.1
    :t c
    :t stack 2 0.1 [(\c -> addHud (view #hudOptions c) (view #charts c)) c]

    stack 2 0.1 :: [ChartTree] -> ChartTree
    c :: ChartOptions
    <interactive>:1:55: error: [GHC-64725]
        • Data constructor ‘ChartOptions’ doesn't have a field named ‘charts’
        • In the first argument of ‘view’, namely ‘#charts’
          In the second argument of ‘addHud’, namely ‘(view #charts c)’
          In the expression: addHud (view #hudOptions c) (view #charts c)

    qBeta = (second' (std decayStd) >>> beta1 (ma decayBeta1)) >>> digitize decayQ qs
    d = drop dropN $ scan (second' qBeta) (taker (n+dropN) $ (zip (fst <$> r) (zip [0..] (snd <$> r))))
    betaSdDigitChart = digitChart ((\x -> UTCTime x (P.fromInteger 0)) . fst <$> d) (fromIntegral . snd <$> d) (quantileNames qs)
    disp betaSdDigitChart

    <interactive>:136:23: error: [GHC-88464]
        Variable not in scope: decayStd :: Double
    
    <interactive>:136:47: error: [GHC-88464]
        Variable not in scope: decayBeta1 :: Double
    
    <interactive>:136:73: error: [GHC-88464]
        Variable not in scope: decayQ :: Double
        Suggested fix:
          Perhaps use one of these:
            ‘decay’ (line 48), ‘delay’ (imported from Data.Mealy),
            ‘delay1’ (imported from Data.Mealy)
    <interactive>:137:10: error: [GHC-88464]
        Variable not in scope: dropN :: Int
        Suggested fix:
          Perhaps use one of these:
            ‘drop’ (imported from NumHask.Prelude),
            ‘Map.drop’ (imported from Data.Map),
            ‘Text.drop’ (imported from Data.Text)
    
    <interactive>:137:32: error: [GHC-88464]
        Variable not in scope: qBeta :: Mealy (a0, Double) b
    
    <interactive>:137:49: error: [GHC-88464]
        Variable not in scope: dropN :: Int
        Suggested fix:
          Perhaps use one of these:
            ‘drop’ (imported from NumHask.Prelude),
            ‘Map.drop’ (imported from Data.Map),
            ‘Text.drop’ (imported from Data.Text)
    <interactive>:138:82: error: [GHC-39999]
        • No instance for ‘FromIntegral Double (Double, Int)’
            arising from a use of ‘fromIntegral’
        • In the first argument of ‘(.)’, namely ‘fromIntegral’
          In the first argument of ‘(<$>)’, namely ‘fromIntegral . snd’
          In the second argument of ‘digitChart’, namely
            ‘(fromIntegral . snd <$> d)’
    <interactive>:139:6: error: [GHC-88464]
        Variable not in scope: betaSdDigitChart :: ChartOptions

    toCT co = addHud (view #hudOptions co) (view #charts co)

    disp $ mempty & #charts .~ stack 2 0.1 [toCT bucketChart, toCT compChart', toCT betaSdDigitChart, toCT betaSdChart', toCT decayChart]

    <interactive>:156:64: error: [GHC-88464]
        Variable not in scope: compChart'
    
    <interactive>:156:81: error: [GHC-88464]
        Variable not in scope: betaSdDigitChart
    
    <interactive>:156:104: error: [GHC-88464]
        Variable not in scope: betaSdChart'

    ts = pack <$> ["std decay = " <> show decayStd, "beta1 decay = " <> show decayBeta1, "quantile decay = " <> show decayQ]
    s = defaultTextStyle & #anchor .~ AnchorStart
    ts' = [TextChart s (zipWith (\t x -> (t, Point 0 x)) ts [0..])]
    decayChart = mempty & #charts .~ unnamed (ts' <> [padChart 0.2 ts']) :: ChartOptions

    

    styleBoxes ts'

    Just Rect -0.432 2.567 -3.6000000000000004e-2 2.096


# ToDo all in one

-   [ ] try and predict future stats
    -   [ ] calculate ma std etc
    -   [ ] chart of expected future distribution
-   [ ] track a p&l
    -   [ ] random p&l streams
-   [X] smaller text chart
-   [X] bar chart labels too close and a bit too small
-   [X] ticks not scaling and fuzzy
    -   function to scale hud along with the chart (can only do this once I assume, but maybe the chart section of HudChart can help)
-   [X] combine digit chart with digit accumulation
-   [X] better order of stack

    -- parameters
    n = 2000
    dropN = 100
    qs = [0.1, 0.5, 0.8] :: [Double]
    decayStd = 0.95
    decayBeta1 = 0.99
    decayQ = 0.996
    ri = zip [0..] (snd <$> r)
    days = reindex n dropN id (fst <$> r)
    
    accChart = lchart Nothing (palette1 0) (rebase n dropN (scan (dipure (+)) (snd <$> r)))
    
    finishHud = #axes %~ (<> [dayAxis days]) >>> #frames %~ (<> [(Priority 30, defaultFrameOptions & #buffer .~ 0.1)]) :: HudOptions -> HudOptions
    
    sdChart = lchart (Just PlaceLeft) (palette1 1) (reindex n dropN (scan (std decayStd)) (snd <$> r))
    
    betaChart = lchart (Just PlaceRight) (palette1 2) (reindex n dropN (scan (second' (std decayStd) >>> beta1 (ma decayBeta1))) ri)
    
    
    sdCharts = compoundMerge [sdChart,betaChart, accChart & #hudOptions %~ finishHud]
    
    qBeta = (second' (std decayStd) >>> beta1 (ma decayBeta1)) >>> digitize decayQ qs
    rDigit = reindex n dropN (scan qBeta) ri
    cs = Data.Mealy.fold countM (rDigit)
    qCountChart = barChart defaultBarOptions (BarData [fromIntegral <$> toList cs] (qRangeLabel qs) []) & #hudOptions % #frames %~ (<> [(Priority 30, defaultFrameOptions & #buffer .~ 0.2)]) & #hudOptions % #titles %~ (<> [(Priority 10, defaultTitle "quantile counts" & #buffer .~ 0.2 & #style % #color .~ palette1a 1 1)])
    
    qBetaChart = digitChart ((\x -> UTCTime x (P.fromInteger 0)) <$> days) (fromIntegral <$> rDigit) (quantileNames qs) & #hudOptions % #axes .~ []
    
    qBeta' = (first snd) <$> ((,) <$> id <*> (second' (std decayStd) >>> beta1 (ma decayBeta1) >>> digitize decayQ qs >>> delay1 0))
    buckets = fmap (ardList ((length qs :: Int) + 1)) (qBeta' >>> accRetDigits)
    xs = (drop dropN $ scan (second' buckets) (taker (n+dropN) $ (zip (fst <$> r) (zip [0..] (snd <$> r)))))
    bucketChart = dayChart (qRangeLabel qs) xs
    
    accBucketChart = compoundMerge [qBetaChart, bucketChart]
    
    ts = pack <$> reverse ["std decay = " <> show decayStd, "beta1 decay = " <> show decayBeta1, "quantile decay = " <> show decayQ, "quantiles = " <> show qs]
    s = defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.65
    ts' = zipWith (\t x -> TextChart s [(t, Point 0 x)]) ts [0..]
    decayChart = (mempty::ChartOptions) & (#hudOptions % #frames .~ [(Priority 30, FrameOptions (Just clear) 0.05)]) & (#charts .~ unnamed ts')
    
    disp $ mempty & #charts .~ stack' 2 0.1 ([toCT sdCharts, toCT qCountChart, toCT accBucketChart, toCT decayChart])

    <interactive>:152:28: error: [GHC-88464]
        Variable not in scope: palette1 :: t1 -> Colour
        Suggested fix:
          Perhaps use one of these:
            ‘palette’ (imported from Chart), ‘paletteO’ (imported from Chart),
            ‘paletteR’ (imported from Chart)
    <interactive>:154:46: error: [GHC-18872]
        • Couldn't match type: Priority FrameOptions
                         with: (a0 -> Priority a0, FrameOptions)
            arising from the overloaded label ‘#frames’
        • In the first argument of ‘(%~)’, namely ‘#frames’
          In the second argument of ‘(>>>)’, namely
            ‘#frames
               %~ (<> [(Priority 30, defaultFrameOptions & #buffer .~ 0.1)])’
          In the expression:
              #axes %~ (<> [dayAxis days])
                >>>
                  #frames
                    %~ (<> [(Priority 30, defaultFrameOptions & #buffer .~ 0.1)]) ::
                HudOptions -> HudOptions
    <interactive>:156:36: error: [GHC-88464]
        Variable not in scope: palette1 :: t0 -> Colour
        Suggested fix:
          Perhaps use one of these:
            ‘palette’ (imported from Chart), ‘paletteO’ (imported from Chart),
            ‘paletteR’ (imported from Chart)
    <interactive>:158:39: error: [GHC-88464]
        Variable not in scope: palette1 :: t0 -> Colour
        Suggested fix:
          Perhaps use one of these:
            ‘palette’ (imported from Chart), ‘paletteO’ (imported from Chart),
            ‘paletteR’ (imported from Chart)
    <interactive>:161:27: error: [GHC-88464]
        Variable not in scope: sdChart :: ChartOptions
        Suggested fix:
          Perhaps use one of these:
            data constructor ‘Chart’ (imported from Chart),
            data constructor ‘HudChart’ (imported from Chart),
            ‘padChart’ (imported from Chart)
    
    <interactive>:161:35: error: [GHC-88464]
        Variable not in scope: betaChart :: ChartOptions
    
    <interactive>:161:46: error: [GHC-88464]
        Variable not in scope: accChart :: ChartOptions
        Suggested fix: Perhaps use ‘anyChart’ (imported from Prettychart)
    
    <interactive>:161:72: error: [GHC-88464]
        Variable not in scope: finishHud :: HudOptions -> HudOptions
    <interactive>:166:233: error: [GHC-88464]
        Variable not in scope: defaultTitle :: t2 -> a5
        Suggested fix:
          Perhaps use one of these:
            ‘defaultTick’ (imported from Chart),
            ‘defaultStyle’ (imported from Chart)
    
    <interactive>:166:302: error: [GHC-88464]
        Variable not in scope: palette1a :: t0 -> t1 -> b2
        Suggested fix:
          Perhaps use one of these:
            ‘palette’ (imported from Chart), ‘paletteO’ (imported from Chart),
            ‘paletteR’ (imported from Chart)
    <interactive>:180:117: error: [GHC-64725]
        • Data constructor ‘ChartOptions’ doesn't have a field named ‘charts’
        • In the first argument of ‘(.~)’, namely ‘#charts’
          In the second argument of ‘(&)’, namely ‘(#charts .~ unnamed ts')’
          In the expression:
            (mempty :: ChartOptions)
              & (#hudOptions % #frames
                   .~ [(Priority 30, FrameOptions (Just clear) 0.05)])
              & (#charts .~ unnamed ts')
    <interactive>:182:48: error: [GHC-88464]
        Variable not in scope: sdCharts :: ChartOptions
    
    <interactive>:182:63: error: [GHC-88464]
        Variable not in scope: qCountChart :: ChartOptions
    
    <interactive>:182:102: error: [GHC-88464]
        Variable not in scope: decayChart :: ChartOptions
        Suggested fix:
          Perhaps use one of these:
            ‘dayChart’ (line 76),
            data constructor ‘RectChart’ (imported from Chart)

    -- parameters
    n = 2000
    dropN = 100
    qs = [0.1, 0.5, 0.8] :: [Double]
    decayStd = 0.95
    decayBeta1 = 0.99
    decayQ = 0.996
    ri = zip [0..] (snd <$> r)
    days = reindex n dropN id (fst <$> r)
    
    accChart = lchart Nothing (palette1 0) (rebase n dropN (scan (dipure (+)) (snd <$> r)))
    
    finishHud = #axes %~ (<> [dayAxis days]) >>> #frames %~ (<> [(Priority 30, defaultFrameOptions & #buffer .~ 0.1)]) :: HudOptions -> HudOptions
    
    sdChart = lchart (Just PlaceLeft) (palette1 1) (reindex n dropN (scan (std decayStd)) (snd <$> r))
    
    betaChart = lchart (Just PlaceRight) (palette1 2) (reindex n dropN (scan (second' (std decayStd) >>> beta1 (ma decayBeta1))) ri)
    
    
    sdCharts = compoundMerge [sdChart,betaChart, accChart & #hudOptions %~ finishHud]
    
    qBeta = (second' (std decayStd) >>> beta1 (ma decayBeta1)) >>> digitize decayQ qs
    rDigit = reindex n dropN (scan qBeta) ri
    cs = Data.Mealy.fold countM (rDigit)
    qCountChart = barChart defaultBarOptions (BarData [fromIntegral <$> toList cs] (qRangeLabel qs) []) & #hudOptions % #frames %~ (<> [(Priority 30, defaultFrameOptions & #buffer .~ 0.2)]) & #hudOptions % #titles %~ (<> [(Priority 10, defaultTitle "quantile counts" & #buffer .~ 0.2 & #style % #color .~ palette1a 1 1)]) & #hudOptions % #axes %~ fmap (second (#bar .~ Nothing))
    
    
    qBetaChart = digitChart ((\x -> UTCTime x (P.fromInteger 0)) <$> days) (fromIntegral <$> rDigit) (quantileNames qs) & #hudOptions % #axes .~ []
    
    qBeta' = (first snd) <$> ((,) <$> id <*> (second' (std decayStd) >>> beta1 (ma decayBeta1) >>> digitize decayQ qs >>> delay1 0))
    buckets = fmap (ardList ((length qs :: Int) + 1)) (qBeta' >>> accRetDigits)
    xs = (drop dropN $ scan (second' buckets) (taker (n+dropN) $ (zip (fst <$> r) (zip [0..] (snd <$> r)))))
    bucketChart = dayChart (qRangeLabel qs) xs
    
    accBucketChart = compoundMerge [qBetaChart, bucketChart]
    
    ts = pack <$> reverse ["std decay = " <> show decayStd, "beta1 decay = " <> show decayBeta1, "quantile decay = " <> show decayQ, "quantiles = " <> show qs]
    s = defaultTextStyle & #anchor .~ AnchorStart & #hsize .~ 0.65
    ts' = zipWith (\t x -> TextChart s [(t, Point 0 x)]) ts [0..]
    decayChart = (mempty::ChartOptions) & (#hudOptions % #frames .~ [(Priority 30, FrameOptions (Just clear) 0.05)]) & (#charts .~ unnamed ts')
    -- disp $ qCountChart & #hudOptions % #axes %~ fmap (second (#bar .~ Nothing))
    
    disp $ mempty & #charts .~ stack' 2 0.1 ([toCT sdCharts, toCT qCountChart, toCT accBucketChart, toCT decayChart])

    True

