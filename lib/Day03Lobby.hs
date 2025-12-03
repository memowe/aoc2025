module Day03Lobby where

import Data.Char

type Joltage        = Int
type SingleJoltage  = Char
type Bank           = [SingleJoltage]

singleJoltage :: SingleJoltage -> Joltage
singleJoltage = digitToInt

combinedJoltage :: [SingleJoltage] -> Joltage
combinedJoltage = read

findMaxTwo :: Bank -> [SingleJoltage]
findMaxTwo = walk []
  where walk ms []                            = ms
        walk [] (j:js)                        = walk [j] js
        walk [m] [j]                          = [m, j]
        walk [m] (j:js)     | j > m           = walk [j] js
                            | otherwise       = walk [m, j] js
        walk ms [j]         | j >= minimum ms = [maximum ms, j]
                            | otherwise       = ms
        walk (m:n:_) (j:js) | j > m           = walk [j] js
                            | j > n           = walk [m, j] js
                            | otherwise       = walk [m, n] js

solve_1 :: String -> Int
solve_1 = sum . map (combinedJoltage . findMaxTwo) . lines
