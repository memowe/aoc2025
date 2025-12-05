module Day05Cafeteria where

import Days
import Data.Char
import Text.ParserCombinators.ReadP

type ID         = Int
data Range      = Range {from :: ID, to :: ID} deriving Show
data Inventory  = Inventory {freshRanges :: [Range], ingredients :: [ID]}
                  deriving Show

instance Read Inventory where
  readsPrec _ = readP_to_S inv
    where inv   = do  frs <- range `endBy` nl
                      ids <- nl >> (nat `endBy` nl)
                      return $ Inventory frs ids
          range = Range <$> nat <*> (char '-' >> nat)
          nat   = read <$> munch1 isDigit
          nl    = char '\n'

inRanges :: [Range] -> ID -> Bool
inRanges rs i = any (\r -> from r <= i && i <= to r) rs

solve_1 :: String -> Int
solve_1 input =
  let inv   = read input
      fresh = filter (freshRanges inv `anyContains`) (ingredients inv)
  in  length fresh

solve_2 :: String -> Int
solve_2 = undefined

day :: Day
day = Day 5 "Cafeteria" (show.solve_1) (show.solve_2)
