{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}

module StrictnessTypes (
  -- everything but the fields
  L(MkL),
  R(MkR),
  XY(MkXY),
  NT(MkNT),
  ) where

import           GHC.Generics (Generic)
import           StrictCheck

import           Data.Ruin

-- | It's crucial that 'L' and 'R' are box types. IE They have a single
-- constructor that takes one argument and is non-strict. Such types
-- exercise a corner case for 'genericExtricate' and 'genericBuild',
-- since the isomorphism of "GHC.Generics" conflates them with
-- newtypes.
data L = MkL {l :: ()} deriving (Generic)
data R = MkR {r :: ()} deriving (Generic)
data XY = MkXY {x,y :: ()} deriving (Generic)
newtype NT = MkNT {nt :: ()} deriving (Generic)

instance SSerial L where sseries = sdecDepth $ MkL <$> sseries
instance SSerial R where sseries = sdecDepth $ MkR <$> sseries
instance SSerial XY where sseries = sdecDepth $ MkXY <$> sseries <~> sseries
instance SSerial NT where sseries = MkNT <$> sseries

$(makeRecords [''L,''R,''XY,''NT])
