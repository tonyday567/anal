{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# HLINT ignore "Use <$>" #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Anal where

import Control.Category (id)
import Data.Bifunctor
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import Data.Char hiding (isDigit)
import Data.Foldable
import Data.FormatN
import qualified Data.Map as Map
import Data.Maybe
import Data.Mealy
import Data.Mealy.Quantiles
import Data.Profunctor
import Data.Text (Text, unpack)
import Data.Time.Calendar
import Data.Time.Format.ISO8601
import FlatParse.Basic
  ( Parser,
    Result (Err, Fail, OK),
    chainr,
    char,
    empty,
    isDigit,
    runParser,
    satisfy,
    satisfyAscii,
    some,
    strToUtf8,
    withOption,
    (<|>),
  )
import NumHask.Prelude hiding (id)
import qualified Prelude

-- $setup
--
-- >>> :set -XOverloadedStrings
-- >>> import Anal
-- >>> import FlatParse.Basic
-- >>> import Data.Time.Calendar
-- >>> import Data.FormatN

-- | Parse a digit
digit :: Parser a Int
digit =
  (\c -> ord c - ord '0') <$> satisfyAscii isDigit

-- | (unsigned) Int parser
--
-- >>> runParser int "69"
-- OK 69 ""
int :: Parser a Int
int = do
  (place, n) <- chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))
  case place of
    1 -> empty
    _ -> pure n

digits :: Parser a (Int, Int)
digits = chainr (\n (!place, !acc) -> (place * 10, acc + place * n)) digit (pure (1, 0))

-- | Parse a Double
-- >>> runParser double "1.234x"
-- OK 1.234 "x"
--
-- >>> runParser double "."
-- Fail
--
-- >>> runParser double "123"
-- OK 123.0 ""
--
-- >>> runParser double ".123"
-- OK 0.123 ""
--
-- >>> runParser double "123."
-- OK 123.0 ""
double :: Parser a Double
double = do
  (placel, nl) <- digits
  withOption
    ($(char '.') *> digits)
    ( \(placer, nr) ->
        case (placel, placer) of
          (1, 1) -> empty
          _ -> pure $ fromIntegral nl + fromIntegral nr / fromIntegral placer
    )
    ( case placel of
        1 -> empty
        _ -> pure $ fromIntegral nl
    )

-- | Parse a signed version of a number
--
-- >>> runParser (signed double) "-1.234x"
-- OK (-1.234) "x"
signed :: (Ring b, FromInteger b) => Parser e b -> Parser e b
signed p =
  withOption $(char '-') (const (((-1) *) <$> p)) p

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
fredP = (,) <$> dayP <*> (($(char ',')) *> ((Right <$> double) <|> (Left <$> $(char '.'))))

-- | run a Parser, Nothing on failure
runParserMaybe :: Parser e a -> BS.ByteString -> Maybe a
runParserMaybe p b = case runParser p b of
  OK r _ -> Just r
  Fail -> Nothing
  Err _ -> Nothing

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

-- | Not great because will always succeed
num :: Parser e (Maybe Double)
num = runParserMaybe double . strToUtf8 . filter (/= ',') <$> numString

auinvP :: Parser e (Day, String)
auinvP = (,) <$> quoted dayP' <*> (($(char ',')) *> quoted numString)

-- return scan

ret :: Mealy (Day, Double) (Day, Double)
ret = second' ((\p p' -> log (p / p')) <$> id <*> delay1 undefined)

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

-- difference mealy
diff1 :: (a -> a -> b) -> a -> Mealy a b
diff1 f a0 = f <$> id <*> delay1 a0

count :: (Ord a) => [a] -> Map.Map a Int
count = foldl' (\m k -> Map.insertWith (+) k 1 m) Map.empty

-- | Take the last n of a list.
taker :: Int -> [a] -> [a]
taker n = reverse . take n . reverse

signalize :: Double -> [Double] -> Mealy Double Double
signalize r qs' =
  (\x -> fromIntegral x / fromIntegral (length qs' + 1)) <$> digitize r qs'

defaultQuantiles :: [Double]
defaultQuantiles = (0.1 *) <$> [1 .. 9]

quantileNames :: [Double] -> [Text]
quantileNames qs = (<> "th") . comma (Just 0) . (100 *) <$> qs

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

runParserError :: Parser e a -> BS.ByteString -> a
runParserError p bs = case runParser p bs of
  OK r _ -> r
  _ -> undefined

dayReturnP :: Parser e (Day, Double)
dayReturnP = (,) <$> dayP <*> ($(char ',') *> signed double)

getReturns :: IO [(Day, Double)]
getReturns = do
  bs <- BS.readFile "other/returns.csv"
  pure $ runParserError dayReturnP <$> C.lines bs
