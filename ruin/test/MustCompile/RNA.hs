{-# Language DataKinds #-}
{-# Language QuasiQuotes #-}
{-# Language TypeOperators #-}
{-# Language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MustCompile.RNA () where

import Data.Ruin
import Data.Ruin.Ancillaries (Tup1)

import XY

_p = (
    (\[rna|(l...) x y|] -> [lx,ly]) :: ("x" :@ a,"y" :@ a) -> [a]
  ,
    (\[rna|(...R) x y|] -> [xR,yR]) :: ("x" :@ a,"y" :@ a) -> [a]
  ,
    \[rna||] -> ()
  ,
    (\[rna|c@C|] -> c) :: Tup1 ("C" :@ a) -> a
  ,
    (\[rna| !strict !bang@! |] -> [strict,bang]) :: ("strict" :@ a,"!" :@ a) -> [a]
  ,
    (\[rna|_ _@x|] -> ()) :: ("_" :@ a, "x" :@ x) -> ()
  ,
    (\[rna|XY (...R) a@x|] -> aR) :: XY x y -> x
  )

_e = (
    [rna||] :: ()
  ,
    [rna|a|] :: Tup1 ("a" :@ ())
  ,
    [rna|a b|] :: ("a" :@ (),"b" :@ ())
  ,
    [rna|a b|] :: ("a" :@ (),"b" :@ ())
  ,
    [rna|XY a@x b@y|] :: XY () ()
  )
  where
  a = ()
  b = ()
