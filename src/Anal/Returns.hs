{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Anal.Returns where

import Chart
import Data.Bifunctor
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Map qualified as Map
import Data.Maybe
import Data.Mealy
import Data.Profunctor
import Data.Text (unpack)
import Data.Time
import Data.Time.Format.ISO8601
import FlatParse.Basic
  ( char,
    isDigit,
    satisfy,
  )
import MarkupParse
import MarkupParse.FlatParse
import NumHask.Prelude hiding (diff, fold)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Anal.Returns
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

ret :: Mealy (Day, Double) (Day, Double)
ret = second' (diff (log . (/)))
