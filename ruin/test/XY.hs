{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language TemplateHaskell #-}
{-# Language UndecidableInstances #-}

module XY (XY(MkXY)) where

import GHC.Generics (Generic)

import Data.Ruin

data XY x y = MkXY {x::x,y::y} deriving (Eq,Generic,Ord,Show)

instance NoWarnUnusedTopBind XY where noWarnUnusedTopBind MkXY{x=_,y=_} = ()

$(makeRecords [''XY])
