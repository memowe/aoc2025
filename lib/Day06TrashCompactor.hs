module Day06TrashCompactor where

import Days
import Data.Char
import Data.List
import Text.ParserCombinators.ReadP
import Control.Monad

type    Number     = Int
type    Numbers    = [Number]
data    Operation  = Add | Mult                           deriving Show
data    Problem    = Problem Operation Numbers            deriving Show
newtype Problems   = Problems {getProblems :: [Problem]}  deriving Show

instance Read Problems where
  readsPrec _ = readP_to_S problems
    where problems    = do  rows  <- numbers `endBy` char '\n'
                            ops   <- operations <* char '\n'
                            let cols = transpose rows
                            return $ Problems (zipWith Problem ops cols)
          numbers     = between sp sp (number `sepBy` sp)
          number      = read <$> munch1 isDigit
          operations  = ((add +++ mult) `sepBy` sp) <* sp
          add         = Add <$ char '+'
          mult        = Mult <$ char '*'
          sp          = void $ many (char ' ')

solve :: Problem -> Number
solve (Problem Add  ns) = sum ns
solve (Problem Mult ns) = product ns

solve_1 :: String -> Int
solve_1 = sum . map solve . getProblems . read

solve_2 :: String -> Int
solve_2 = undefined

day :: Day
day = Day 6 "Trash Compactor" (show.solve_1) (show.solve_2)
