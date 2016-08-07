{-# Language AllowAmbiguousTypes #-}
{-# Language ExplicitForAll #-}
{-# Language MagicHash #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeInType #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide,not-home #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Data.Ruin.Hoid (
  Hoid,
  hoid,
  hoidProxy,
  phoid,
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import GHC.Exts (type (~~),Any,Constraint,Proxy#)

-- | A family of identity functions indexed by possibly higher-order
-- types. @'hoid' \@t@ asserts that @a@ is either equal to @t@ or is
-- an application of @t@.
--
-- The 'Hoid' type family vanishes if the kind of @t@ is defined
-- enough to fully determine the arity of @t@. If 'Hoid' doesn't
-- vanish in a use case, then 'hoid' is not intended for use in that
-- case.
hoid :: forall t a. Hoid t a => a -> a
hoid = id

-- | 'hoid' but with a proxy argument.
phoid :: forall t a. Hoid t a => Proxy# t -> a -> a
phoid _ = hoid @t

-- | @'hoidProxy \@t' = 'hoid' \@t <$> Proxy@
hoidProxy :: forall t a. Hoid t a => Proxy a
hoidProxy = Proxy

-- | Do not reuse; consider this an implementation detail of 'hoid'.
type family Hoid (t :: k) (a :: k2) :: Constraint where
  Hoid (t :: _ -> cod) a = Hoid (t (Lookup t cod a)) a
  Hoid t a = (t ~~ a)

-----

-- | Because of the final case, the user never sees the 'Lookup' type.
type family Lookup (t :: dom -> k1) (cod :: Type) (a :: k2) :: dom where
  Lookup t (_ -> cod) (f x) = Lookup t cod f
  Lookup _ _ (f x) = x
  Lookup _ _ _ = Any   -- This branch is only involved in type errors.
