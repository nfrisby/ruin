{-# Language DataKinds #-}
{-# Language QuasiQuotes #-}
{-# Language TypeApplications #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MustCompile.RSplat () where

import Data.Ruin

import XY

xy = [rna|XY x y|]
  where
  x = True
  y = "Hello"

_test = hoid @XY $ rsplat (rsym [rna|x y|]) xy
  where
  x = show
  y = (++ ", World.")
