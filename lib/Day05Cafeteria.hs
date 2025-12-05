module Day05Cafeteria where

import Days
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP

type ID         = Int
data Range      = Range {fromR :: ID, toR :: ID} deriving Show
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

anyContains :: [Range] -> ID -> Bool
anyContains rs i = any contains rs
  where contains r = fromR r <= i && i <= toR r

solve_1 :: String -> Int
solve_1 input =
  let inv   = read input
      fresh = filter (freshRanges inv `anyContains`) (ingredients inv)
  in  length fresh

----

data Boundary = From ID | To ID deriving (Eq, Show)

instance Ord Boundary where
  compare (From i) (From j) = compare i j
  compare (To i)   (To j)   = compare i j
  compare (From i) (To j)   = compare i j <> LT
  compare (To i)   (From j) = compare i j <> GT

boundaries :: [Range] -> [Boundary]
boundaries = sort . concatMap rb
  where rb (Range f t) = [From f, To t]

countFresh :: [Range] -> Int
countFresh = step (0, Nothing) . boundaries
  where step :: (Int, Maybe ID) -> [Boundary] -> Int
        step (0,      _)        []            = 0
        step (0,      Nothing)  (From i : bs) = step (1, Just i) bs
        step (1,      Just i)   (To j : bs)   = (j-i+1) + step (0, Nothing) bs
        step (depth,  Just i)   (To _ : bs)   = step (depth-1, Just i) bs
        step (depth,  Just i)   (From _ : bs) = step (depth+1, Just i) bs
        step _                  _             = error "impossible state"

solve_2 :: String -> Int
solve_2 = countFresh . freshRanges . read

day :: Day
day = Day 5 "Cafeteria" (show.solve_1) (show.solve_2)
