{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language KindSignatures #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

-- | This modules makes it possible to declare that a type 'Has'
-- /only/ its 'Fields'. This can lead to better type error messages.
--
-- These classses should be instantiated as follows. Note that
-- 'Data.Ruin.TH.makeRecords' does this automatically.
--
-- @
--   data XY x y = MkXY {x::x,y::y} deriving Generic
--
--   instance 'ClosedHas' s XY => 'Has' s XY where
--     {-\# INLINE 'extricate1' #-}
--     'extricate1' = 'closedExtricate1'
--
--   instance 'HasCase' "x" XY   -- Just like usual 'Has' instances.
--   instance 'HasCase' "y" XY
--
--   instance 'Build' XY where
--     {-\# INLINE 'build' #-}
--     'build' = 'genericBuild'
-- @
module Data.Ruin.ClosedHas (
  ClosedHas,
  HasCase(..),
  closedExtricate1,
  ) where

import GHC.Generics
import GHC.TypeLits

import Data.Ruin.All
import Data.Ruin.Eval
import Data.Ruin.Internal

-- | Exactly the same as 'Has', but 'ClosedHas' delegates to this copy,
-- so that 'Has' can delegate parametrically to 'ClosedHas'!
class HasCase (s :: Symbol) (t :: *) where
  type FieldTypeCase s t :: *
  type FieldTypeCase s t = GenericFieldType s t

  extricate1Case :: Label s -> t -> Eval (FieldType s t)

  default extricate1Case :: (Generic t,GBox (IsABox (Rep t)) t,GenericHas (Rep t) s (FieldType s t)) => Label s -> t -> Eval (FieldType s t)
  {-# INLINE extricate1Case #-}
  extricate1Case = genericExtricate1

-- | Like @'Has' s t@, but gives a type error if @s@ isn't in
-- @'Fields' t@.
type ClosedHas s t = HasIf (Elem s (FieldNames t)) s t

closedExtricate1 :: forall s t. ClosedHas s t => Label s -> t -> Eval (FieldType s t)
{-# INLINE closedExtricate1 #-}
closedExtricate1 = \_ -> extricate1If @(Elem s (FieldNames t)) @s

class HasIf (t_has_field_s :: Bool) (s :: Symbol) (t :: *) where
  extricate1If :: t -> Eval (FieldType s t)

instance TypeError (NoSuchField s t) => HasIf 'False s t where
  extricate1If = undefined

instance HasCase s t => HasIf 'True s t where
  {-# INLINE extricate1If #-}
  extricate1If = extricate1Case (mkLabel @s)
