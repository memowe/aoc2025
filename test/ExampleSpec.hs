module ExampleSpec where

import Test.Hspec
import Text.Printf
import Days
import Control.Monad

import Paths_aoc2025

import qualified Day01SecretEntrance      as D01
import qualified Day02GiftShop            as D02
import qualified Day03Lobby               as D03
import qualified Day04PrintingDepartment  as D04
import qualified Day05Cafeteria           as D05
import qualified Day06TrashCompactor      as D06
import qualified Day07Laboratories        as D07
import qualified Day08Playground          as D08

data DayTestData = DTD  { dayData   :: Day
                        , input1    :: String
                        , expected1 :: String
                        , input2    :: String
                        , expected2 :: String
                        } deriving Show

dayTestData :: Day -> IO DayTestData
dayTestData d = do
  let fp = printf "day%02d_" (day d)
      fi1 = fp ++ "1.in"; fe1 = fp ++ "1.out"
      fi2 = fp ++ "2.in"; fe2 = fp ++ "2.out"
  [i1, e1, i2, e2] <- (readFile <=< getDataFileName) `mapM` [fi1, fe1, fi2, fe2]
  return $ DTD d i1 e1 i2 e2

addNL :: String -> String
addNL s = if last s == '\n' then s else s ++ "\n"

days :: [Day]
days =  [ D01.day, D02.day, D03.day, D04.day, D05.day, D06.day
        , D07.day, D08.day
        ]

spec :: Spec
spec = describe "Example solution tests" $ forM_ days $ \d -> do

  describe (printf "Day %02d: %s" (day d) (name d)) $ do
    td <- runIO $ dayTestData d
    it "Part 1" $ addNL (solve1 d (input1 td)) `shouldBe` expected1 td
    it "Part 2" $ addNL (solve2 d (input2 td)) `shouldBe` expected2 td
