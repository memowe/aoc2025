module Day01SecretEntrance where

import Data.Char
import Data.Default
import Control.Monad
import Control.Monad.State.Lazy as S
import Control.Monad.Writer.Lazy
import Text.ParserCombinators.ReadP

inEnum :: Enum a => (Int -> Int) -> a -> a
inEnum f = toEnum . f . fromEnum

type    Move  = Int
newtype Moves = Moves {getMoves :: [Move]} deriving Show

instance Read Moves where
  readsPrec _ = readP_to_S moves
    where moves = Moves <$> (left +++ right) `sepBy` char '\n'
          left  = char 'L' >> negate <$>  size
          right = char 'R' >>             size
          size  = read <$> munch1 isDigit

newtype Dial = Dial {getDial :: Int} deriving (Show, Eq)

instance Enum Dial where
  toEnum    = Dial . (`mod` 100)
  fromEnum  = getDial

instance Default Dial where
  def = Dial 50

turn :: Move -> Dial -> Dial
turn n = inEnum (+ n)

type Counter = [()]
one :: Counter
one = [()]

run1 :: Moves -> Int
run1 = length . flip evalState def . execWriterT . mapM_ dial . getMoves
  where dial m = do newDial <- turn m <$> S.get
                    when (newDial == Dial 0) $ tell one
                    S.put newDial

run2 :: Moves -> Int
run2 = length . flip evalState def . execWriterT . mapM_ dial . getMoves
  where dial 0  = return ()
        dial n  = do  n' <- if n > 0
                              then modify (inEnum succ) >> return (n - 1)
                              else modify (inEnum pred) >> return (n + 1)
                      zero
                      dial n'
        zero    = do  d <- S.get
                      when (d == Dial 0) $ tell one

solve_01_1 :: String -> Int
solve_01_1 = run1 . read

solve_01_2 :: String -> Int
solve_01_2 = run2 . read
