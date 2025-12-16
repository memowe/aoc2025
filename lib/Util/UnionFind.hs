module Util.UnionFind where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad
import Control.Monad.State
import GHC.Utils.Monad

type UnionFind a = Map a a

empty :: UnionFind a
empty = M.empty

include :: Ord a => a -> State (UnionFind a) ()
include x = do  unlessM (gets (x `M.member`)) $ do
                  modify $ M.insert x x

find :: Ord a => a -> State (UnionFind a) a
find x = do p <- gets (! x)
            if p == x
              then return x
              else  do  pp <- find p
                        modify $ M.insert x pp
                        return pp

union :: Ord a => a -> a -> State (UnionFind a) ()
union x y = do  px <- find x
                py <- find y
                unless (px == py) $ do
                  modify $ M.insert py px

connected :: Ord a => a -> a -> State (UnionFind a) Bool
connected x y = do  px <- find x
                    py <- find y
                    return $ px == py

member :: Ord a => a -> State (UnionFind a) Bool
member = gets . M.member

runUnionFind :: UnionFind a -> State (UnionFind a) r -> r
runUnionFind = flip evalState

components :: Ord a => State (UnionFind a) (Map a (Set a))
components = do elems <- gets M.keys
                M.fromListWith S.union <$> mapM connect elems
  where connect e = (, S.singleton e) <$> find e

showUnionFind :: (Ord a, Show a) => State (UnionFind a) String
showUnionFind = do
  rm <- M.toList . M.map S.toList <$> components
  let ls = map (\(repr, xs) -> show repr ++ ": " ++ show xs) rm
  return $ unlines ls
