{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language UndecidableInstances #-}
{-# Language ViewPatterns #-}

module MustCompile.ClosedHas () where

import GHC.Generics (Generic)

import Data.Ruin
import Data.Ruin.Ancillaries (genericBuild)
import Data.Ruin.ClosedHas

data XY x y = MkXY {x::x,y::y} deriving Generic

instance NoWarnUnusedTopBind XY where
   noWarnUnusedTopBind MkXY{x=_,y=_} = ()

instance ClosedHas s (XY x y) => Has s (XY x y) where
  {-# INLINE extricate1 #-}
  extricate1 = closedExtricate1

instance HasCase "x" (XY x y)
instance HasCase "y" (XY x y)

instance Build (XY x y) where
  {-# INLINE build #-}
  build = genericBuild
