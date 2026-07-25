
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Anal.Returns where

import Chart
import Circuit.Parser (Parser, char, runParserError, runParserMaybe, satisfy, some)
import Circuit.Parser.Primitives (double, int, isDigit, signed, strToUtf8)
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Functor.Identity (Identity (..))
import Data.Map qualified as Map
import Data.Maybe
import Process.Stats
import Data.Profunctor
import Data.Text (unpack)
import Data.Time
import Data.Time.Format.ISO8601
import NumHask.Prelude hiding (diff, fold, some)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Anal.Returns
-- >>> import Circuit.Parser
-- >>> import Data.Time.Calendar

-- | Day parser, consumes separator
--
-- >>> runParser dayP "2020-07-28"
-- These 2020-07-28 ""
type Parser' = Parser Identity ByteString Char

runP :: Parser' a -> ByteString -> a
runP p = runIdentity . runParserError p

runPM :: Parser' a -> ByteString -> Maybe a
runPM p = runIdentity . runParserMaybe p

dayP :: Parser' Day
dayP = do
  y <- int
  _ <- char '-'
  m <- int
  _ <- char '-'
  d <- int
  pure $ fromGregorian (fromIntegral y :: Integer) m d

fredP :: Parser' (Day, Either () Double)
fredP = (,) <$> dayP <*> (char ',' *> (Right <$> double))

-- | Day parser, consumes separator
--
-- >>> runParser dayP' "07/10/1999"
-- These 1999-10-07 ""
dayP' :: Parser' Day
dayP' = do
  d <- int
  _ <- char '/'
  m <- int
  _ <- char '/'
  y <- int
  pure $ fromGregorian (fromIntegral y :: Integer) m d

quoted :: Parser' a -> Parser' a
quoted p = char '"' *> p <* char '"'

numString :: Parser' String
numString = filter (/= ',') <$> some (satisfy (\x -> isDigit x || (x == '.') || (x == ',')))

auinvP :: Parser' (Day, String)
auinvP = (,) <$> quoted dayP' <*> (char ',' *> quoted numString)

getPricesFred :: IO [(Day, Double)]
getPricesFred = do
  bs <- BS.readFile "other/SP500.csv"
  pure $ [(d, p) | Just (d, Right p) <- runPM fredP <$> drop 1 (C.lines bs)]

getPricesAuinv :: FilePath -> IO [(Day, Double)]
getPricesAuinv fp = do
  bs <- BS.readFile fp
  let pricesString = [(d, p) | Just (d, p) <- runPM auinvP <$> drop 1 (C.lines bs)]
  pure [(d, p) | (d, Just p) <- second (runPM double . strToUtf8) <$> pricesString]

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
      . bimap (formatShow iso8601Format) (fixed (Just 12))
      <$> r

dayReturnP :: Parser' (Day, Double)
dayReturnP = (,) <$> dayP <*> (char ',' *> signed double)

getReturns :: IO [(Day, Double)]
getReturns = do
  bs <- BS.readFile "other/returns.csv"
  pure $ runP dayReturnP <$> C.lines bs

ret :: Process (Day, Double) (Day, Double)
ret = second' (diff (log . (/)))
