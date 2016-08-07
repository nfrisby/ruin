module Main where

import Test.Hspec (hspec)

import MustCompile.ClosedHas ()
import MustCompile.HRDatabase ()
import MustCompile.Physics ()
import MustCompile.PrintAndTime ()
import MustCompile.RNA ()
import MustCompile.RSplat ()
import MustCompile.RSplit ()
import MustCompile.TH ()

import qualified MustNotCompile
import qualified Strictness
import qualified Test.RNA
import qualified Test.R

main :: IO ()
main = hspec $ do
  MustNotCompile.test
  Strictness.test
  Test.RNA.test
  Test.R.test
