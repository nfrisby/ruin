{-# OPTIONS_GHC -Werror -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}

{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedLabels #-}
{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language UndecidableInstances #-}
{-# Language ViewPatterns #-}

module Demo (test,test2,test3,test4) where

import Control.Lens (Iso',iso,view)
import GHC.Generics (Generic)

import Data.Ruin
import Data.Ruin.Ancillaries

data XY a = MkXY {x,y :: a}
  deriving (Generic,Show)

instance NoWarnUnusedTopBind (XY a) where
  noWarnUnusedTopBind MkXY{x=_,y=_} = ()

instance Has "x" (XY a)
instance Has "y" (XY a)
instance Build (XY a) where
  {-# INLINE build #-}
  build = genericBuild

-----

data WZ a = MkWZ {w,z :: a} deriving Generic

data X a = MkX {x :: a} deriving Generic

$(makeRecords [ ''WZ , 'MkX ])

-----

x_over_z [rna|x z|] = fromIntegral x / fromIntegral z

w_over_x_plus_y_over_z [rna|w x y z|] =
    x_over_z (rup (dub #x w,dub #z x))
  +
    x_over_z (dub mkLabel y,dub mkLabel z)   -- positional arguments remain an option

rc = (MkXY 1 2,MkWZ 3 4)

test = x_over_z (rup rc)

test2 = x_over_z $ rup $ dub #x 100 <@ rc

test3 = w_over_x_plus_y_over_z (rup rc)

-----

data Polar = MkPolar {r,theta :: Double}
  deriving (Generic,Show)

instance NoWarnUnusedTopBind Polar where
  noWarnUnusedTopBind MkPolar{r=_,theta=_} = ()

instance Has "r" Polar where
instance Has "theta" Polar where
instance Build Polar where
  {-# INLINE build #-}
  build = genericBuild

isoPolar :: Iso' Polar (XY Double)
isoPolar = iso (g . rup) (f . rup)
  where
  f [rna|x y|] = rsym (
      dub #r (sqrt (x*x + y*y))
    ,
      dub #theta (atan (y/x))
    )
  g [rna|r theta|] = rsym (
      dub #x (r * cos theta)
    ,
      dub #y (r * sin theta)
    )

instance Has "x" Polar where
  type FieldType "x" Polar = Double
  extricate1 lbl = extricate1 lbl . view isoPolar
instance Has "y" Polar where
  type FieldType "y" Polar = Double
  extricate1 lbl = extricate1 lbl . view isoPolar

aPolar :: Polar
aPolar =
  rto @Polar (
      dub #r 1
    ,
      dub #theta (3 * pi / 4)
    )

test4 :: XY Double
test4 = rup aPolar
