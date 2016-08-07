{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language DuplicateRecordFields #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}
{-# Language OverloadedLabels #-}
{-# Language PolyKinds #-}
{-# Language TemplateHaskell #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

module MustCompile.TH where

import GHC.Generics (Generic)
import Language.Haskell.TH.Syntax (Lift(lift))

import Data.Ruin
import Data.Ruin.R

data family Family (t :: k) y

data instance Family t Int where
   MkFamily1 :: {x,y :: Int} -> Family t Int
 deriving Generic

$(makeRecords ['MkFamily1])

newtype instance Family [] Char =
   MkFamily2 {x :: Int}
 deriving Generic

$(makeRecords ['MkFamily2])

-----

r :: Rcrd '[ "a" ::: () , "b" ::: Bool ]
r = $(lift (oneR #a () `plusR` oneR #b True))
