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

multiPartParser :: ReadP ()
multiPartParser = many1 get >>= void . string

invalid :: ID -> Bool
invalid = match multiPartParser . show

solve_02_1 :: String -> Int
solve_02_1 input =
  let cands = concat [[f..t] | (f, t) <- getRanges (read input)]
  in  sum $ filter invalid cands
