{-# LANGUAGE TemplateHaskell #-}
module Day07Laboratories where

import Days
import Data.Bifunctor
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Monad.State
import Control.Lens
import GHC.Utils.Monad

type X        = Int
type Y        = Int
type Coord    = (X, Y)
type Manifold = Map Coord ManPart
data ManPart  = MPEmpty | MPSplitter deriving Show
data TachMan  = TachMan { start     :: Coord
                        , manifold  :: Manifold
                        } deriving Show

data Cell = CellEmpty | CellSplitter | CellStart

toManPart :: Cell -> ManPart
toManPart CellSplitter = MPSplitter; toManPart _ = MPEmpty

instance Read TachMan where
  readsPrec _ = readP_to_S tachman
    where tachman   = do  rows <- row `endBy` char '\n'
                          let pairs   = concat $ zipWith (\y -> map $ first (,y)) [0..] rows
                              manMap  = M.map toManPart $ M.fromList pairs
                          [coord] <- return [c | (c, CellStart) <- pairs]
                          return $ TachMan coord manMap
          row       = zip [0..]     <$> many (empty +++ splitter +++ startP)
          empty     = CellEmpty     <$ char '.'
          splitter  = CellSplitter  <$ char '^'
          startP    = CellStart     <$ char 'S'

type Beam       = Coord
data TachState  = TachState { _beams      :: Set Beam
                            , _splitCount :: Int
                            } deriving Show

makeLenses ''TachState

countSplits :: Manifold -> State TachState ()
countSplits m = do
  bs <- use beams
  forM_ bs $ \beam -> do
    beams %= S.delete beam
    let beam' = second (+1) beam
    case m !? beam of
      Nothing         -> return ()
      Just MPEmpty    -> beams %= S.insert beam'
      Just MPSplitter -> do let bl = first pred beam'
                                br = first succ beam'
                            forM_ (filter (`M.member` m) [bl, br]) $ \b -> do
                              beams %= S.insert b
                            splitCount %= (+1)
  unlessM (beams `uses` null) $ countSplits m

solve_1 :: String -> Int
solve_1 input =
  let TachMan s m = read input
      initState   = TachState (S.singleton s) 0
  in  execState (countSplits m) initState ^. splitCount

data TimelineState = TLS  { _lbeams     :: Map Coord Int
                          , _timelines  :: Int
                          } deriving Show

makeLenses ''TimelineState

countTimelines :: Manifold -> State TimelineState ()
countTimelines m = do
  bs <- use lbeams
  forM_ (M.toList bs) $ \(crd, cnt) -> do
    lbeams %= M.delete crd
    let crd' = second succ crd
    case m !? crd' of
      Nothing         -> timelines %= (+ cnt)
      Just MPEmpty    -> lbeams %= M.insertWith (+) crd' cnt
      Just MPSplitter -> do let crdl = first pred crd'
                                crdr = first succ crd'
                            forM_ (filter (`M.member` m) [crdl, crdr]) $ \c -> do
                              lbeams %= M.insertWith (+) c cnt
  unlessM (lbeams `uses` null) $ countTimelines m

solve_2 :: String -> Int
solve_2 input =
  let TachMan s m = read input
      initState   = TLS (M.singleton s 1) 1
  in  pred $ execState (countTimelines m) initState ^. timelines

day :: Day
day = Day 7 "Laboratories" (show.solve_1) (show.solve_2)
