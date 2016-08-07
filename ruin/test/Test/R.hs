{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Test.R (test) where

import GHC.Generics (to,from)
import Test.Hspec

import Data.Ruin.R

test :: Spec
test = do
  describe "Generics" $ do
    it "supports conversion" $ do
      let gid = (to . from) `asTypeOf` id
      let r = oneR #x False `plusR` oneR #y "y"
      gid r `shouldBe` r
