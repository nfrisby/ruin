{-# Language AllowAmbiguousTypes #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language KindSignatures #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide,not-home #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}   -- rdrop

-- | Hiding fields.

module Data.Ruin.Hide (
  Hide,
  hide,
  rdrop,
  rtake,
  ) where

import GHC.TypeLits

import Data.Ruin.All
import Data.Ruin.Eval
import Data.Ruin.Internal
import Data.Ruin.R

-- | Deny the @'Has'@ instance for each of @ss@.
newtype Hide (ss :: [Symbol]) rc = MkHide rc

-- | Deny (\"forget\") a @'Has' s@ instance.
hide :: Labels ss -> rc -> Hide ss rc
hide _ = MkHide

instance Has_Hide (Elem s sHiddens) s rc => Has s (Hide sHiddens rc) where
  type FieldType s (Hide sHiddens rc) = FieldType s rc
  {-# INLINE extricate1 #-}
  extricate1 = extricate1_Hide @(Elem s sHiddens)

class Has_Hide (eq :: Bool) (s :: Symbol) (rc :: *) where
  extricate1_Hide :: Label s -> Hide sHiddens rc -> Eval (FieldType s rc)

instance TypeError (FieldIsHidden s rc) => Has_Hide 'True s rc where
  extricate1_Hide = undefined

instance Has s rc => Has_Hide 'False s rc where
  {-# INLINE extricate1_Hide #-}
  extricate1_Hide = \s (MkHide rc) -> extricate1 s rc

-----

type FieldIsHidden (s :: Symbol) (top :: *) =
             'Text "ruin: The field `"
       ':<>: 'Text s
       ':<>: 'Text "' is hidden in the type"
  ':$$: Render top

-----

-- | Create an anonymous record that contains the fields of @t@ that
-- are not named in @fs@.
rdrop ::
     ( rc ~ Rcrd (DifferenceByFst (Fields t) fs)
     , Build rc
     , t `IsSubtypeOf` rc
     )
  => Labels fs
  -> t
  -> rc
{-# INLINE rdrop #-}
rdrop = \_ -> rup

-- | Split a record into two separate types, where the second type is
-- an anonymous record defined as the leftovers from the first type.
rtake ::
     ( leftovers ~ Rcrd (DifferenceByFst (Fields t) (FieldNames taken))
     , Build (taken,leftovers)
     , t `IsSymmetricRecordOf` (taken,leftovers)
     )
  => t
  -> (taken,leftovers)
{-# INLINE rtake #-}
rtake = rsym
