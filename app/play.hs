{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

import Box
import Control.Monad
import Data.Bifunctor
import Data.ByteString (ByteString)
import MarkupParse
import Optics.Core hiding (element)
import Options.Applicative
import Web.Rep
import Web.Rep.Examples
import Prelude
import Anal
import Anal.Returns
import Chart
import Data.Time

data AppType
  = SharedTest
  | PlayTest
  | DashboardTest
  | RestartTest
  | ClosureBug
  deriving (Eq, Show)

newtype Options = Options
  { optionAppType :: AppType
  }
  deriving (Eq, Show)

parseAppType :: Parser AppType
parseAppType =
  flag' SharedTest (long "shared" <> help "shared test")
    <|> flag' DashboardTest (long "dash" <> help "dashboard test")
    <|> flag' PlayTest (long "play" <> help "play functionality test")
    <|> flag' RestartTest (long "restart" <> help "console restart test")
    <|> flag' ClosureBug (long "closure" <> help "documents the closure bug")
    <|> pure SharedTest

options :: Parser Options
options =
  Options
    <$> parseAppType

opts :: ParserInfo Options
opts =
  info
    (options <**> helper)
    (fullDesc <> progDesc "web-rep testing" <> Options.Applicative.header "web-rep")

-- | A simple count stream
countStream :: Int -> Double -> CoEmitter IO (Gap, [Code])
countStream n speed = fmap (second ((: []) . Replace "output" . strToUtf8 . show)) <$> qList (zip (0 : repeat (1 / speed)) [0 .. n])

main :: IO ()
main = do
  o <- execParser opts
  let a = optionAppType o
  case a of
    SharedTest -> sharedTest
    PlayTest -> playTest
    DashboardTest -> dashboardTest
    RestartTest -> void restartTest
    ClosureBug -> void closureBug

dashboardTest :: IO ()
dashboardTest = do
  r <- getReturns
  serveRep
    (repSdDashboardConfig defaultSdDashboardConfig)
    replaceInput
    (replaceDashboard r)
    defaultCodeBoxConfig

-- | Convert (typically parsed representation) to Code, replacing the output section of a page, and appending errors.
replaceDashboard :: [(Day,Double)] -> Either ByteString SdDashboardConfig -> [Code]
replaceDashboard r ea =
  case ea of
    Left err -> [Append "debug" err]
    Right a -> [Replace "output" (encodeChartOptions (sdDash a r))]

sharedTest :: IO ()
sharedTest =
  serveRep
    (maybeRep (Just "maybe") False repExamples)
    replaceInput
    replaceOutput
    defaultCodeBoxConfig

playTest :: IO ()
playTest = servePlayStream (PlayConfig True 10 0) (defaultCodeBoxConfig & #codeBoxPage .~ playPage) (countStream 100 1)

playPage :: Page
playPage =
  defaultSocketPage
    & set
      #htmlBody
      ( element
          "div"
          [Attr "class" "container"]
          ( element
              "div"
              [Attr "class" "row"]
              (elementc "h1" [] "replay simulation")
              <> element
                "div"
                [Attr "class" "row"]
                ( mconcat $
                    ( \(t, h) ->
                        element
                          "div"
                          [Attr "class" "row"]
                          (element "h2" [] (elementc "div" [Attr "id" t] h))
                    )
                      <$> sections
                )
          )
      )
  where
    sections =
      [ ("input", mempty),
        ("output", mempty)
      ]

restartTest :: IO (Either Bool ())
restartTest = restart <$> (gapEffect . fmap (1,) <$> resetE 5 10) <*|> pure (glue showStdout . gapEffect <$|> countStream 100 1)

resetE :: Int -> Int -> CoEmitter IO Bool
resetE n m = qList (replicate (n - 1) False <> [True] <> replicate m False)

-- | documenting the issue with left floating compositions in the usage of <$|>
closureBug :: IO (Either Bool ())
closureBug = do
  putStrLn "restart ((== \"q\") <$> fromStdin) (glue showStdout . gapEffect <$|> countStream 10 2)"
  putStrLn "type 'q' to restart"
  restart ((== "q") <$> fromStdin) (glue showStdout . gapEffect <$|> countStream 10 2)
  putStrLn "buggy version"
  putStrLn "restart ((== \"q\") <$> fromStdin) . glue showStdout . gapEffect <$|> countStream 20 1"
  restart ((== "q") <$> fromStdin) . glue showStdout . gapEffect <$|> countStream 10 2

-- | one of each input SharedReps
repSdParams :: (Monad m) => SdParams -> SharedRep m SdParams
repSdParams p = do
  qs <- readTextbox_ (Just "qs") (view #qs p)
  sw <- readTextbox_ (Just "weights") (view #sigWeights p)
  dSd <- sliderV (Just "decay sd") 0.8 1 0.001 (view #decayStd p)
  dB <- sliderV (Just "decay sd beta") 0.8 1 0.001 (view #decayBeta1 p)
  dQ <- sliderV (Just "decay quantiles") 0.8 1 0.001 (view #decayQ p)
  pure (SdParams qs dSd dB dQ sw)

repSdDashboardConfig :: (Monad m) => SdDashboardConfig -> SharedRep m SdDashboardConfig
repSdDashboardConfig cfg = do
  n <- sliderVI (Just "n") 10 3000 10 (view #n cfg)
  dropN <- sliderVI (Just "dropN") 0 500 10 (view #dropN cfg)
  ps <- repSdParams (view #sdParams cfg)
  pure (SdDashboardConfig n dropN ps (view #dashHud cfg) (view #chartHuds cfg))
