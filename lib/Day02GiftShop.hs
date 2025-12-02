module Day02GiftShop where

import Data.Char
import Text.ParserCombinators.ReadP
import Control.Monad

type    ID      = Int
type    Range   = (ID, ID)
newtype Ranges  = Ranges {getRanges :: [Range]} deriving Show

instance Read Ranges where
  readsPrec _ = readP_to_S (Ranges <$> ranges)
    where ranges = (range `sepBy` char ',') <* char '\n'
          range  = do [f, t] <- (read <$> munch1 isDigit) `sepBy` char '-'
                      return (f, t)

match :: ReadP a -> String -> Bool
match p = any (null . snd) . readP_to_S p

doublePart, multiPart :: ReadP ()
doublePart  = many1 get >>= void . string
multiPart   = many1 get >>= void . many1 . string

invalid1, invalid2 :: ID -> Bool
invalid1 = match doublePart . show
invalid2 = match multiPart . show

candidates :: String -> [ID]
candidates = concatMap (uncurry enumFromTo) . getRanges . read

solve_02_1, solve_02_2 :: String -> Int
solve_02_1 = sum . filter invalid1 . candidates
solve_02_2 = sum . filter invalid2 . candidates
