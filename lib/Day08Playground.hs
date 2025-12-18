module Day08Playground where

import Days
import Data.Char
import Data.Ord
import Data.List
import Data.Set (Set)
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Util.UnionFind as UF
import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Monad.State.Lazy

type    X           = Double
type    Y           = Double
type    Z           = Double
type    Coord       = (X, Y, Z)
type    Box         = Coord
newtype Boxes       = Boxes {getBoxes :: Set Box} deriving Show
type    BoxPair     = (Box, Box)
type    Distance    = Double

instance Read Boxes where
  readsPrec _ = readP_to_S boxes
    where boxes = Boxes . S.fromList <$> box `endBy` char '\n'
          box   = do  [x, y, z] <- coord `sepBy` char ','
                      return (x, y, z)
          coord = read <$> munch1 isDigit

distance :: BoxPair -> Distance
distance ((x1,y1,z1), (x2,y2,z2)) =
  sqrt $ sq (x2-x1) + sq (y2-y1) + sq (z2-z1)
  where sq x = x * x

makeCircuits :: Int -> Boxes -> [Set Box]
makeCircuits boxCount boxes =
  let boxList   = S.toList $ getBoxes boxes
      pairs     = [(a, b) | a <- boxList, b <- boxList, a < b]
      initDists = take boxCount $ sortOn distance pairs
  in  flip evalState UF.empty $ do
        forM_ initDists $ \(a,b) -> do
          UF.include a
          UF.include b
          UF.union a b
        M.elems <$> UF.components

solve_1 :: String -> Int
solve_1 input =
  let boxes     = read input
      boxCount  = S.size (getBoxes boxes)
      pairCount = if boxCount <= 100 then 10 else 1000 -- Example input?
      circuits  = makeCircuits pairCount boxes
  in  product $ take 3 $ sortOn Down $ map S.size circuits

findLastConnection :: Boxes -> BoxPair
findLastConnection boxes =
  let boxList = S.toList $ getBoxes boxes
      pairs   = sortOn distance [(a, b) | a <- boxList, b <- boxList, a < b]
  in  flip evalState UF.empty $ do
        forM_ boxList UF.include
        connect (length boxList) pairs
  where connect compCount (p:ps)  = do
          let (a, b) = p
          conn <- UF.connected a b
          if not conn && compCount == 2 then return p
            else do UF.union a b
                    let compCount' = if conn then compCount else compCount - 1
                    connect compCount' ps
        connect _ _ = error "shouldn't happen"

solve_2 :: String -> Int
solve_2 input =
  let ((x1,_,_), (x2,_,_)) = findLastConnection (read input)
  in  round (x1 * x2)

day :: Day
day = Day 8 "Playground" (show.solve_1) (show.solve_2)
