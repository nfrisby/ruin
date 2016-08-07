{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Test.RNA (test) where

import Control.Monad.Writer (Writer,tell)
import Test.Hspec

import Data.Ruin

import XY

data X = X deriving (Eq,Show)

data Y = Y deriving (Eq,Show)

test :: Spec
test = do
  describe "rnaA" $ do
    it "uses given order for effects" $ do
      let x =
              hoid @(Writer String)
            $ X <$ tell "x"
      let y = Y <$ tell "y"

      [rnaA|XY x y|] `shouldBe` (     MkXY <$> x <*> y)
      [rnaA|XY y x|] `shouldBe` (flip MkXY <$> y <*> x)
