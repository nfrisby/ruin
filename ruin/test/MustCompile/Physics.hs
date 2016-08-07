{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language OverloadedLabels #-}
{-# Language PartialTypeSignatures #-}
{-# Language QuasiQuotes #-}
{-# Language TypeApplications #-}
{-# Language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module MustCompile.Physics () where

import Data.Ruin

-- | The vacuum permittivity constant, in farads per meter.
eps0 :: Fractional a => a
eps0 = 8.854187817e-12

-- | Capacitance of a conducting cylinder of radius @r@ and length @l@
-- surrounded concentrically by conducting cylindrical shell of inner
-- radius @r + gap@ and equal length where the gap has a dielectric
-- constant of @kappa@.
--
-- From <http://www.phys.uri.edu/gerhard/PHY204/tsl105.pdf> and
-- <http://physics.info/equations/> (\"cylindrical capacitor\").
cylindricalCapacitance :: Floating a => _ -> a
cylindricalCapacitance [rna|kappa l gap r|] =
    2 * pi * kappa * eps0 * l
  / log (b / a)
  where
  a = r
  b = r + gap

test :: Double
test =
  cylindricalCapacitance $ rsym (
      dub #l 3
    ,
      dub #r 1.2
    ,
      dub #gap 1
    ,
      dub #kappa 2
    )
