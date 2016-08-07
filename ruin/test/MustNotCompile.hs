{-# Language DataKinds #-}
{-# Language OverloadedLabels #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language TypeOperators #-}

{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

module MustNotCompile (test) where

import Language.Haskell.TH.Syntax (Lift(lift))
import Test.Hspec (Spec,describe,it)
import Test.ShouldNotTypecheck (shouldNotTypecheck)

import Data.Ruin
import Data.Ruin.Ancillaries (rupNonStrict)

x :: "x" :@ ()
x = dub #x ()

y :: "y" :@ ()
y = dub #y ()

test :: Spec
test = do
  describe "Missing fields prevent" $ do
    it "`extricate1'" $ shouldNotTypecheck $
      extricate1 #x y
    it "`rup'" $ shouldNotTypecheck $
      rup y `asTypeOf` x
    it "`rupNonStrict'" $ shouldNotTypecheck $
      rupNonStrict y `asTypeOf` x

  describe "`lift'" $ do
    it "preserves the field name for singleton records" $ shouldNotTypecheck $
      $(lift (dub #x ())) `asTypeOf` dub #y ()

{-
  -- Test incorrectly fails, perhaps because of https://github.com/CRogers/should-not-typecheck/issues/5
  describe "Extra fields prevent" $ do
    it "`req'" $ shouldNotTypecheck $
      req (x,y) `asTypeOf` y

  -- Each of these causes a panic in GHC 8.0.1
  data RecordDict :: * -> * where
    MkRecordDict :: Record t => RecordDict t
  describe "These are not instances of `Record'" $ do
    it "Types with overlapping fields, e.g. (X,X)" $ shouldNotTypecheck $
      MkRecordDict @(X,X)
    it "Pair _ _" $ shouldNotTypecheck $
      MkRecord @(Pair () ())
  describe "A function" $ do
    it "is not a record" $ shouldNotTypecheck $
      MkRecord @(() -> ())
-}
