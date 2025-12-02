module Days where

import Text.Printf

data Day = Day  { day     :: Int
                , name    :: String
                , solve1  :: String -> String
                , solve2  :: String -> String
                }

instance Show Day where
  show (Day d n _ _) = printf "Day %02d: %s (solve1, solve2)" d n
