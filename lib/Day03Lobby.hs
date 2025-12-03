module Day03Lobby where

import Days
import Data.Char

type Joltage        = Int
type SingleJoltage  = Char
type Bank           = [SingleJoltage]

singleJoltage :: SingleJoltage -> Joltage
singleJoltage = digitToInt

combinedJoltage :: [SingleJoltage] -> Joltage
combinedJoltage = read

findMax :: Int -> Bank -> [SingleJoltage]
findMax count bank = walk count (length bank) [] bank
  where walk 0 0 stack []         = reverse stack
        walk _ _ _ []             = error "illegal input list"
        walk n len [] (j:js)      = walk (n-1) (len-1) [j] js
        walk n len (s:ss) (j:js)
          | j > s && n < len      = walk (n+1) len ss (j:js)
          | n == 0                = walk n (len-1) (s:ss) js
          | otherwise             = walk (n-1) (len-1) (j:s:ss) js

solve_1 :: String -> Int
solve_1 = sum . map (combinedJoltage . findMax 2) . lines

solve_2 :: String -> Int
solve_2 = sum . map (combinedJoltage . findMax 12) . lines

day :: Day
day = Day 3 "Lobby" (show.solve_1) (show.solve_2)
