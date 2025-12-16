module UtilSpec where

import Test.Hspec
import Test.QuickCheck

import Util.UnionFind
import Data.Map ((!))
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.State

genUF :: Gen (UnionFind Int)
genUF = do  n   <- choose (0, 40)
            ops <- genOps n
            return $ execState ops empty
  where genOps n  = do  numOps  <- choose (0, n*2)
                        ops     <- vectorOf numOps (genOp n)
                        return $ sequence_ ops
        genOp n   = do  x <- choose (0, n-1)
                        y <- choose (0, n-1)
                        elements  [ include x
                                  , include x >> include y >> x `union` y
                                  ]

genUF1 :: Gen (UnionFind Int, Int)
genUF1 = do uf  <- genUF `suchThat` (not . null)
            x   <- elements (M.elems uf)
            return (uf, x)

genUF2 :: Gen (UnionFind Int, Int, Int)
genUF2 = do uf  <- genUF `suchThat` ((>= 2) . M.size)
            x   <- elements (M.elems uf)
            y   <- elements (M.elems uf)
            return (uf, x, y)

genUF3 :: Gen (UnionFind Int, Int, Int, Int)
genUF3 = do uf  <- genUF `suchThat` ((>= 3) . M.size)
            x   <- elements (M.elems uf)
            y   <- elements (M.elems uf)
            z   <- elements (M.elems uf)
            return (uf, x, y, z)

runUF :: UnionFind Int -> State (UnionFind Int) a -> a
runUF = runUnionFind

infixr 1 ===>
(===>) :: Bool -> Bool -> Bool
a ===> b = not a || b

infix 1 <==>
(<==>) :: Bool -> Bool -> Bool
a <==> b = (a ===> b) && (b ===> a)

spec :: Spec
spec = describe "Utility module tests" $ do

  describe "UnionFind" $ do

    it "Reflexivity" $
      forAll genUF1 $ \(uf, x) ->
        runUF uf $ include x >> connected x x

    it "Member after include" $
      forAll genUF1 $ \(uf, x) ->
        runUF uf $ include x >> member x

    it "Include idempotence" $
      forAll genUF1 $ \(uf, x) ->
        let ufm1 = runUF uf $ include x               >> get
            ufm2 = runUF uf $ include x >> include x  >> get
        in  ufm1 `shouldBe` ufm2

    it "Symmetry" $
      forAll genUF2 $ \(uf, x, y) ->
        runUF uf $ do xey <- connected x y
                      yex <- connected y x
                      return $ xey `shouldBe` yex

    it "Union base effect" $
      forAll genUF2 $ \(uf, x, y) ->
        runUF uf $ union x y >> connected x y

    it "Union self-idempotence" $
      forAll genUF1 $ \(uf, x) ->
        let ufcs  = runUF uf $              components
            uf'cs = runUF uf $ union x x >> components
        in  uf'cs `shouldBe` ufcs

    it "Transitivity" $
      forAll genUF3 $ \(uf, x, y, z) ->
        let cxy = runUF uf $ connected x y
            cyz = runUF uf $ connected y z
            cxz = runUF uf $ connected x z
        in  cxy && cyz ===> cxz

    it "Connected iff same representative" $
      forAll genUF2 $ \(uf, x, y) ->
        let cxy = runUF uf $ connected x y
            fx  = runUF uf $ find x
            fy  = runUF uf $ find y
        in  cxy <==> fx == fy

    it "Components are equivalence classes wrt connected" $
      forAll genUF2 $ \(uf, x, y) ->
        let comps     = runUF uf components
            sameComp  = any (\c -> x `S.member` c && y `S.member` c)
                          (M.elems comps)
            conn      = runUF uf $ connected x y
        in  sameComp <==> conn

    it "All elements are found in a component" $
      forAll (genUF `suchThat` (not . null)) $ \uf ->
        let comps = runUF uf components
        in  forAll (elements $ M.keys uf) $ \e ->
              let repr = runUF uf $ find e
              in  e `S.member` (comps ! repr)

    it "Union reduces components" $
      forAll genUF2 $ \(uf, x, y) ->
        let count1  = runUF uf $              (length <$> components)
            count2  = runUF uf $ union x y >> (length <$> components)
            conn    = runUF uf $ connected x y
        in  if conn then count2 == count1
                    else count2 == count1 - 1

    context "Path compression" $ do
      it "Find idempotence" $
        forAll genUF1 $ \(uf, x) ->
          let fx1 = runUF uf $ find x
              fx2 = runUF uf $ find x >> find x
          in  fx1 `shouldBe` fx2
      it "Find preserves components" $
        forAll genUF1 $ \(uf, x) ->
          let comps1 = runUF uf $           components
              comps2 = runUF uf $ find x >> components
          in  comps1 `shouldBe` comps2
      it "Path compression flattens paths" $
        forAll genUF1 $ \(uf, x) ->
          let (root, uf') = runState (find x) uf
              parent      = uf' ! x
          in  parent `shouldBe` root
