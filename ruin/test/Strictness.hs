{-# Language BangPatterns #-}
{-# Language DataKinds #-}
{-# Language ExplicitForAll #-}
{-# Language FlexibleContexts #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedLabels #-}
{-# Language PolyKinds #-}
{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}

module Strictness (test) where

import           Data.Functor.Compose
import           Data.Functor.Identity
import           Test.Hspec

import           Data.Ruin
import           Data.Ruin.Ancillaries (GiveAllItHas(..),Pair(..),genericBuild,genericBuildNonStrict,genericExtricate1,rupEval,rupNonStrict)
import           Data.Ruin.Eval (Eval(Done),runEval)

import qualified StrictCheck
import           StrictnessTypes

sameStrictness :: forall a b c. StrictCheck.SSerial a => (a -> b) -> (a -> c) -> IO ()
sameStrictness f g = StrictCheck.sameStrictness f g d >>= (`shouldBe` True)
  where
  d = 1000   -- this depth is exhausive for our examples here

nonstrict :: forall a b. StrictCheck.SSerial a => (a -> b) -> IO ()
nonstrict f = sameStrictness f (const ())

cei :: a -> Compose Eval Identity a
cei = pure

test :: Spec
test = do
  describe "Non-strict" $ do
    it "`Done'" $ do
      nonstrict @() Done

    it "`extricate1' for singleton records" $ do
      nonstrict @("s" :@ ()) (extricate1 #s)

    it "`genericBuildNonStrict'" $ do
      let eta t = ((genericBuildNonStrict . MkGiveAllItHas) `asTypeOf` id) t
      nonstrict @L eta
      nonstrict @XY eta

    it "`rup' for ()'" $ do
      let eta t = (rup `asTypeOf` id) t
      nonstrict @() eta

    it "`rupNonStrict' for tuples'" $ do
      let eta t = (rupNonStrict `asTypeOf` id) t
      nonstrict @() eta
      nonstrict @(L,R) eta
      nonstrict @XY eta

    it "`genericBuild' for newtypes" $ do
      nonstrict @NT ((genericBuild . MkGiveAllItHas) `asTypeOf` cei)

    it "`rupEval' for singleton records'" $ do
      nonstrict @("s" :@ ()) (rupEval `asTypeOf` pure)

  describe "Only strict enough to ensure the necessary fields exist" $ do
    it "`genericExtricate1'" $ do
      sameStrictness (genericExtricate1 #l) $ \MkL{} -> ()
      sameStrictness (genericExtricate1 #x) $ \MkXY{} -> ()
      sameStrictness (genericExtricate1 #y) $ \MkXY{} -> ()
      sameStrictness (genericExtricate1 #nt) $ \MkNT{} -> ()
      nonstrict @NT (genericExtricate1 #nt)

    describe "`extricate1' for" $ do
      it "singleton records" $ do
        nonstrict @("s" :@ ()) (extricate1 #s)

      it "tuples" $ do
        sameStrictness @(L,R) (extricate1 #l) $ \(MkL{},_) -> ()
        sameStrictness @(L,R) (extricate1 #r) $ \(_,MkR{}) -> ()
        sameStrictness @((XY,(L,R)),NT) (extricate1 #r) $ \((_,(_,MkR{})),_) -> ()
        sameStrictness @(XY,L,R,NT) (extricate1 #r) $ \(_,_,MkR{},_) -> ()

      it "`Pair'" $ do
        sameStrictness @(Pair L R) (extricate1 #l) $ \(MkPair MkL{} _) -> ()
        sameStrictness @(Pair L R) (extricate1 #r) $ \(MkPair _ MkR{}) -> ()
        sameStrictness @(XY `Pair` (L `Pair` R) `Pair` (NT `Pair` R))
          (extricate1 #r)
          (\((_ `MkPair` (_ `MkPair` MkR{})) `MkPair` _) -> ())

    it "`genericBuild'" $ do
      let eta t = ((genericBuild . MkGiveAllItHas) `asTypeOf` cei) t
      sameStrictness eta $ \MkL{} -> ()
      sameStrictness eta $ \MkXY{} -> ()
      sameStrictness eta $ \MkNT{} -> ()
      nonstrict @NT eta

    it "`rupEval' for non-() tuples" $ do
      let eta t = (rupEval `asTypeOf` pure) t
      nonstrict @() eta
      sameStrictness eta $ \(MkL{},MkR{}) -> ()

  describe "`rna' quasiquoter" $ do
    it "always has a data constructor" $ do
      nonstrict (\ ~[rna||] -> ())
      nonstrict (\ ~[rna|x|] -> () `asTypeOf` x)
      nonstrict (\ ~[rna|x y|] -> () `asTypeOf` x `asTypeOf` y)

      sameStrictness (\[rna||] -> ()) id
      sameStrictness (\[rna|x|] -> () `asTypeOf` x) id
      sameStrictness (\[rna|x y|] -> () `asTypeOf` x `asTypeOf` y) id

    it "is strict in a field if and only if it has a bang annotation" $ do
      sameStrictness (\(rfrom @XY -> [rna| _@x _@y|]) -> ()) (extricate1 #x)
      sameStrictness (\(rfrom @XY -> [rna|!_@x _@y|]) -> ()) (runEval . extricate1 #x)

      nonstrict (\(rfrom @L -> ~[rna| _@l|]) -> ())
      sameStrictness (\(rfrom @L -> [rna| _@l|]) -> ()) (extricate1 #l)
      sameStrictness (\(rfrom @L -> [rna|!_@l|]) -> ()) (runEval . extricate1 #l)
