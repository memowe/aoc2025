module Day04PrintingDepartment where

import qualified Data.Set as S
import Data.Set (Set, member)
import qualified Data.Map as M
import Data.Map (Map)
import Text.ParserCombinators.ReadP

type    X         = Int
type    Y         = Int
type    Coord     = (X, Y)
newtype Rolls     = Rolls (Set Coord) deriving Show
type    Count     = Int
type    Neighbors = Map Coord Count

instance Read Rolls where
  readsPrec _ = readP_to_S rolls
    where rolls = Rolls . S.fromList . concat       <$> rows
          rows  = zipWith (\y -> map (,y)) [1..]    <$> row `endBy` char '\n'
          row   = map fst . filter snd . zip [1..]  <$> many (roll +++ mpty)
          roll  = True  <$ char '@'
          mpty  = False <$ char '.'

countNeighbors :: Rolls -> Neighbors
countNeighbors (Rolls cs) = tupelMap (S.map neighbor1 cs)
  where neighbor1 = (,) <*> sum . map (fromEnum . (`member` cs)) . neighbors
        tupelMap  = M.fromList . S.toList
        neighbors (x, y) = [ (x', y') | x' <- [x-1, x, x+1]
                                      , y' <- [y-1, y, y+1]
                                      , (x', y') /= (x, y)
                                      ]

removable :: Count -> Bool
removable = (< 4)

solve_1 :: String -> Int
solve_1 = length . M.filter removable . countNeighbors . read
