{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Anal.Returns where

import Chart
import Data.Bifunctor
import Data.ByteString (ByteString) 
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as C
import Data.Map qualified as Map
import Data.Maybe
import Data.Mealy
import Data.Profunctor
import Data.Text (unpack)
import Data.Time
import Data.Time.Format.ISO8601
import Mpar (Parser, char, isDigit, satisfy, double, int, signed, strToUtf8)
import Data.These (These (..))
import Mpar.Parser (StateThreader (..))
import NumHask.Prelude hiding (diff, fold, some)

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Anal.Returns
-- >>> import Mpar.Parser
-- >>> import Data.Time.Calendar


-- | Day parser, consumes separator
--
-- >>> runParser dayP "2020-07-28"
-- OK 2020-07-28 ""
dayP :: Parser ByteString Day
dayP = do
  y <- int
  _ <- char '-'
  m <- int
  _ <- char '-'
  d <- int
  pure $ fromGregorian (fromIntegral y) m d

fredP :: Parser ByteString (Day, Either () Double)
fredP = (,) <$> dayP <*> ((char ',') *> (Right <$> double))

-- | Day parser, consumes separator
--
-- >>> runParser dayP' "07/10/1999"
-- OK 1999-10-07 ""
dayP' :: Parser ByteString Day
dayP' = do
  d <- int
  _ <- char '/'
  m <- int
  _ <- char '/'
  y <- int
  pure $ fromGregorian (fromIntegral y) m d

quoted :: Parser ByteString a -> Parser ByteString a
quoted p = char '"' *> p <* char '"'

numString :: Parser ByteString String
numString = filter (/= ',') <$> some (satisfy (\x -> isDigit x || (x == '.') || (x == ',')))

auinvP :: Parser ByteString (Day, String)
auinvP = (,) <$> quoted dayP' <*> ((char ',') *> quoted numString)

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

dayReturnP :: Parser ByteString (Day, Double)
dayReturnP = (,) <$> dayP <*> (char ',' *> signed double)

getReturns :: IO [(Day, Double)]
getReturns = do
  bs <- BS.readFile "other/returns.csv"
  pure $ runParserError dayReturnP <$> C.lines bs

ret :: Mealy (Day, Double) (Day, Double)
ret = second' (diff (log . (/)))
