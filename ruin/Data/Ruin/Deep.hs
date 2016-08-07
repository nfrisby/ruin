{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language KindSignatures #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

module Data.Ruin.Deep (
  -- * Sequences of labels
  Labels,
  consLabels,
  mkLabels,
  nilLabels,

  -- * Deep projection
  DeepFieldType,
  DeepHas,
  extricate,
  ) where

import GHC.TypeLits

import Data.Ruin.All
import Data.Ruin.Eval (Eval)
import Data.Ruin.Internal

-----

class DeepHas_ (ss :: [Symbol]) (t :: *) where
  type DeepFieldType_ ss t :: *
  extricate_ :: Labels ss -> t -> Eval (DeepFieldType_ ss t)

instance DeepHas_ '[] t where
  type DeepFieldType_ '[] t = t
  {-# INLINE extricate_ #-}
  extricate_ = \_ -> pure

instance (Has s t,DeepHas ss (FieldType s t)) => DeepHas_ (s ': ss) t where
  type DeepFieldType_ (s ': ss) t = DeepFieldType ss (FieldType s t)
  {-# INLINE extricate_ #-}
  extricate_ = \_ t -> extricate1 (mkLabel @s) t >>= extricate (mkLabels @ss)

-----

-- | This constraint is an implementation detail of 'extricate'. It's
-- just an iteration of 'Has'.
type DeepHas = DeepHas_

-- | This constraint is an implementation detail of 'extricate'. It's
-- just an iteration of 'FieldType'.
type DeepFieldType ss t = DeepFieldType_ ss t

-- | 'extricate' project a field out of nested records by iterating
-- 'extricate1'.
--
-- The first argument is a function type so that the syntax can use
-- @.@ to specify a sequence of labels.
--
--
-- @
--   'extricate' id = return
--
--   'extricate' (\#s . ss) = 'extricate1' \#s Control.Monad.'Control.Monad.>=>' 'extricate' ss
-- @
extricate :: forall ss t. DeepHas ss t => Labels ss -> t -> Eval (DeepFieldType ss t)
{-# INLINE extricate #-}
extricate = extricate_
