module Day04PrintingDepartment where

import Days
import Data.Function
import qualified Data.Set as S
import Data.Set (Set, member)
import qualified Data.Map as M
import Data.Map (Map)
import Text.ParserCombinators.ReadP

type    X         = Int
type    Y         = Int
type    Coord     = (X, Y)
newtype Rolls     = Rolls {getCoords :: Set Coord} deriving (Eq, Show)
type    Count     = Int
type    Neighbors = Map Coord Count

instance Read Rolls where
  readsPrec _ = readP_to_S rolls
    where rolls = Rolls . S.fromList . concat       <$> rows
          rows  = zipWith (\y -> map (,y)) [1..]    <$> row `endBy` char '\n'
          row   = map fst . filter snd . zip [1..]  <$> many (roll +++ mpty)
          roll  = True  <$ char '@'
          mpty  = False <$ char '.'

rollMinus :: Rolls -> Rolls -> Count
rollMinus = (-) `on` length . getCoords

countNeighbors :: Rolls -> Neighbors
countNeighbors (Rolls cs) = tupelMap (S.map neighbor1 cs)
  where neighbor1 = (,) <*> sum . map (fromEnum . (`member` cs)) . neighbors
        tupelMap  = M.fromList . S.toList
        neighbors (x, y) = [ (x', y') | x' <- [x-1, x, x+1]
                                      , y' <- [y-1, y, y+1]
                                      , (x', y') /= (x, y)
                                      ]

remove :: Rolls -> Rolls
remove = Rolls . M.keysSet . M.filter (>= 4) . countNeighbors

solve_1 :: String -> Int
solve_1 input =
  let rs  = read input
      rs' = remove rs
  in  rs `rollMinus` rs'

fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f = fst . head . dropWhile (uncurry (/=)) . pairs
  where pairs = (zip <*> tail) . iterate f

solve_2 :: String -> Int
solve_2 input =
  let rs  = read input :: Rolls
      rs' = fixpoint remove rs
  in  rs `rollMinus` rs'

day :: Day
day = Day 4 "Printing Department" (show.solve_1) (show.solve_2)
