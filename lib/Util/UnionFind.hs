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

include :: (MonadState (UnionFind a) m, Ord a) => a -> m ()
include x = do  unlessM (gets (x `M.member`)) $ do
                  modify $ M.insert x x

find :: (MonadState (UnionFind a) m, Ord a) => a -> m a
find x = do p <- gets (! x)
            if p == x
              then return x
              else  do  pp <- find p
                        modify $ M.insert x pp
                        return pp

union :: (MonadState (UnionFind a) m, Ord a) => a -> a -> m ()
union x y = do  px <- find x
                py <- find y
                unless (px == py) $ do
                  modify $ M.insert py px

connected :: (MonadState (UnionFind a) m, Ord a) => a -> a -> m Bool
connected x y = do  px <- find x
                    py <- find y
                    return $ px == py

member :: (MonadState (UnionFind a) m, Ord a) => a -> m Bool
member = gets . M.member

components :: (MonadState (UnionFind a) m, Ord a) => m (Map a (Set a))
components = do elems <- gets M.keys
                M.fromListWith S.union <$> mapM connect elems
  where connect e = (, S.singleton e) <$> find e

showUnionFind :: (MonadState (UnionFind a) m, Ord a, Show a) => m String
showUnionFind = do
  rm <- M.toList . M.map S.toList <$> components
  let ls = map (\(repr, xs) -> show repr ++ ": " ++ show xs) rm
  return $ unlines ls
