{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

-- | performance measurement
module Main where

import Data.List (intercalate)
import Options.Applicative
import Perf
import Prelude hiding (id)
import Anal
import Optics.Core
import Anal.Returns
import Data.Time
import Harpie.Array qualified as A
import Data.Mealy
import Data.Mealy.Quantiles
import Control.Category ((>>>), id)
import Control.Monad
import Data.Map.Strict (Map)
import Data.Text (Text)
import Data.Semigroup
import Control.DeepSeq
import Data.TDigest hiding (median)
import Data.Maybe

data RunType = RunSdIr | RunSdIrDecomp | RunSdIrSlop deriving (Eq, Show)

data AnalSpeedOptions = AnalSpeedOptions
  { asReportOptions :: ReportOptions,
    asSdDashboardConfig :: SdDashboardConfig,
    asRun :: RunType
  }
  deriving (Eq, Show)

parseRun :: Parser RunType
parseRun =
  flag' RunSdIr (long "sdir" <> help "run sd ir test") <|>
  flag' RunSdIrSlop (long "slop" <> help "run sd ir slop test") <|>
  flag' RunSdIrDecomp (long "decomp" <> help "run sd ir decomposition") <|>
  pure RunSdIr

sdParamsOptions :: SdParams -> Parser SdParams
sdParamsOptions def =
  SdParams <$>
  option auto (value (view #qs def) <> long "quantiles" <> short 'q' <> help "quantiles") <*>
  option auto (value (view #decayStd def) <> long "sddecay" <> short 'd' <> help "standard deviation decay") <*>
  option auto (value (view #decayBeta1 def) <> long "beta1decay" <> short 'b' <> help "standard deviation market beta decay") <*>
  option auto (value (view #decayStd def) <> long "qdecay" <> help "quantile decay") <*>
  option auto (value (view #sigWeights def) <> long "sigweights" <> help "signal weights")

sdDashboardConfigOptions :: SdDashboardConfig -> Parser SdDashboardConfig
sdDashboardConfigOptions def =
  SdDashboardConfig <$>
  option auto (value (view #n def) <> long "nsample" <> short 's' <> help "sample size") <*>
  option auto (value (view #dropN def) <> long "ntraining" <> short 't' <> help "training size") <*>
  sdParamsOptions (view #sdParams def) <*>
  pure (view #dashHeight def) <*>
  pure (view #dashHud def) <*>
  pure (view #chartHuds def)

-- | Command-line parser for 'ReportOptions'
parseReportOptions' :: Parser ReportOptions
parseReportOptions' =
  ReportOptions
    <$> option auto (value 10 <> long "runs" <> short 'n' <> help "number of runs to perform")
    <*> parseClock
    <*> parseStatD
    <*> parseMeasure
    <*> parseGolden
    <*> parseHeader
    <*> parseCompareLevels defaultCompareLevels

analSpeedOptions :: Parser AnalSpeedOptions
analSpeedOptions =
  AnalSpeedOptions
    <$> parseReportOptions'
    <*> sdDashboardConfigOptions defaultSdDashboardConfig
    <*> parseRun

analSpeedOpts :: ParserInfo AnalSpeedOptions
analSpeedOpts =
  info
    (analSpeedOptions <**> helper)
    (fullDesc <> progDesc "anal speed measurement" <> header "measurement of anal performance")

main :: IO ()
main = do
  o <- execParser analSpeedOpts
  let repOptions = asReportOptions o
  let n = reportN repOptions
  let s = reportStatDType repOptions
  let mt = reportMeasureType repOptions
  let c = reportClock repOptions
  let run = asRun o
  let cfg = asSdDashboardConfig o

  r <- getReturns
  let sdir' x = sdIr cfg r (view #sdParams cfg & set #decayStd x)
  case run of
    RunSdIr -> do
      reportMainWith repOptions (intercalate "-" [show run, show n, show (view #n cfg), show s, show mt, show c]) (ffap "sdir" sdir' (view (#sdParams % #decayStd) cfg))
    RunSdIrDecomp -> do
      reportMainWith repOptions (intercalate "-" [show run, show n, show (view #n cfg), show s, show mt, show c]) (sdirDecomp' cfg r (view #sdParams cfg))
    RunSdIrSlop -> do
      (a,m) <- reportMainWith'' repOptions (sdirDecomp' cfg r (view #sdParams cfg))
      sl <- slops 1 (Sum <$> Measure (tickWith c) (tickIOWith c)) (sdirDecomp cfg r (view #sdParams cfg))
      print sl
      print a
      print m

-- | Run and report a benchmark to the console with the supplied options.
reportMainWith'' :: ReportOptions -> PerfT IO [[Double]] a -> IO (a, Map Text [[Double]])
reportMainWith'' o t = do
  let !n = reportN o
  let c = reportClock o
  let mt = reportMeasureType o
  runPerfT (measureDs mt c n) t

reindex' :: (Semigroup t, NFData a, NFData b) => Int -> Int -> ([a] -> [b]) -> [a] -> PerfT IO t [b]
reindex' n n' f xs =
  (ffap "drop" (drop n') <=<
  ffap "f" f <=<
  ffap "taker" (taker (n + n'))) xs

sdirDecomp :: (Semigroup t) => SdDashboardConfig -> [(Day, Double)] -> SdParams -> PerfT IO t Double
sdirDecomp cfg r p =
  -- sigret ws = r & fmap snd & reindex n dropN (scan (msigret ws)) & A.asArray
  (ffap "snd" (fmap snd) >=>
  reindex' (view #n cfg) (view #dropN cfg) (scan (msigret sigw)) >=>
  ffap "ir fold" (fold (mIr 1))) r
  where
    -- mealy: quantile sd
    msdqs = std (view #decayStd p) >>> digitize (view #decayQ p) (view #qs p) >>> delay1 0
    -- mealy: signal, given quantile
    msig ws = fmap (A.index ws . pure) msdqs
    -- mealy: return signal
    msigret ws = (*) <$> msig ws <*> id
    -- mealy: information ratios
    mIr d = (\m s -> (250*m)/(sqrt 250 * s)) <$> ma d <*> std d

    -- converts a list to an array
    sigw = A.asArray $ view #sigWeights p


sdirDecomp' :: (Semigroup t) => SdDashboardConfig -> [(Day, Double)] -> SdParams -> PerfT IO t Double
sdirDecomp' cfg r p =
  -- sigret ws = r & fmap snd & reindex n dropN (scan (msigret ws)) & A.asArray
  (ffap "snd" (fmap snd) >=>
   ffap "taker" (taker (view #n cfg + view #dropN cfg)) >=>
   ffap "scan std" (scan (std (view #decayStd p))) >=>
   ffap "scan digitize" (scan (digitize' (view #decayQ p) (view #qs p))) >=>
   ffap "scan delay1" (scan (delay1 0)) >=>
   ffap "drop" (drop (view #dropN cfg)) >=>
   ffap "A.index" (fmap (A.index sigw . pure)) >=>
   ffap "sum" sum) r
   where
     -- converts a list to an array
     sigw = A.asArray $ view #sigWeights p

-- | A mealy that computes the running quantile bucket. For example,
-- in a scan, @digitize 0.9 [0.5]@ returns:
--
-- * 0 if the current value is less than the current mealy median.
--
-- * 1 if the current value is greater than the current mealy median.
digitize' :: Double -> [Double] -> Mealy Double Int
digitize' r qs = M inject step' extract
  where
    step' (x, _) a = (onlineInsert a x, a)
    inject a = (onlineInsert a (emptyOnlineTDigest r), a)
    extract (x, l) = bucket' qs' l
      where
        qs' = fromMaybe (0 / 0) . (`quantile` t) <$> qs
        (OnlineTDigest t _ _) = onlineCompress x
        bucket' xs l' =
          fold (M id (+) id) $
            ( \x' ->
                if x' > l'
                  then 0
                  else 1
            )
              <$> xs
