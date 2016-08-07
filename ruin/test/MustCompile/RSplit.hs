{-# Language DataKinds #-}
{-# Language DeriveGeneric #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language QuasiQuotes #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-signatures #-}

module MustCompile.RSplit () where

import Control.Arrow ((***))
import GHC.Generics (Generic)

import Data.Ruin

import XY

data AB a b = MkAB {a::a,b::b} deriving (Generic,Show)
$(makeRecords [''AB])

-- _e ::
--   (Monoid a3, Monoid a2, Monoid a1, Monoid a) =>
--   t -> (XY a2 a3, AB a a1)
_e _ = hoid @XY *** hoid @AB $ rsym [rna|mempty@a mempty@b mempty@x mempty@y|]
