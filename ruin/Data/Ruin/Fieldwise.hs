{-# Language AllowAmbiguousTypes #-}
{-# Language DataKinds #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language ScopedTypeVariables #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}   -- UnifyShape

module Data.Ruin.Fieldwise where

import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Semigroup (Semigroup,(<>))
import           GHC.TypeLits (Symbol)

import           Data.Ruin.All
import           Data.Ruin.Eval
import           Data.Ruin.Internal

-- | How to create a field @s@ of type @b@ from a value of @a@.
class FPure a (s :: Symbol) b where
  fpure :: a -> b

-- | Same as 'rmonopure'.
instance (b ~ (dom -> cod)) => FPure (dom -> cod) s b where
  {-# INLINE fpure #-}
  fpure = id

-- | An implementation detail of 'rpure'.
newtype RPure a = MkRPure a

-- | Defer to 'FPure'.
instance (Applicative i,FPure a s b) => Gives s b i (RPure a) where
  {-# INLINE get #-}
  get = \(MkRPure a) -> pure $ fpure @a @s a

-- | A record where the value of field @s@ is @'fpure' \@a \@s a@, for
-- the given @a@.
--
-- @
--   > :t 'Data.Ruin.Hoid.hoid' \@((':@') "x") . 'rpure'
--   'Data.Ruin.Hoid.hoid' \@((':@') "x") . 'rpure' :: 'FPure' a "x" t => a -> "x" ':@' t
-- @
rpure :: (Build t,GivesThese (Fields t) Identity (RPure a)) => a -> t
{-# INLINE rpure #-}
rpure = runCEI . build . MkRPure

-----

-- | An implementation detail of 'rmonopure'.
newtype RMonoPure a = MkRMonoPure a

instance (a ~ b) => FPure (RMonoPure a) s b where
  {-# INLINE fpure #-}
  fpure = \(MkRMonoPure a) -> a

-- | A record where every field is a given monomorphic value.
--
-- @
--   > :t 'Data.Ruin.Hoid.hoid' \@((':@') "x") . 'rmonopure'
--   'Data.Ruin.Hoid.hoid' \@((':@') "x") . 'rmonopure' :: t -> "x" ':@' t
-- @
rmonopure :: (Build t,GivesThese (Fields t) Identity (RPure (RMonoPure a))) => a -> t
{-# INLINE rmonopure #-}
rmonopure = rpure . MkRMonoPure

-- | Alias for 'rpure', symmetric with 'rmonopure'.
rpolypure :: (Build t,GivesThese (Fields t) Identity (RPure a)) => a -> t
{-# INLINE rpolypure #-}
rpolypure = rpure

-----

-- | An implementation detail of 'rmempty'.
data RMEmpty = MkRMEmpty

instance Monoid a => FPure RMEmpty s a where
  {-# INLINE fpure #-}
  fpure = \_ -> mempty

-- | A record where every field is 'mempty'.
--
-- @
--   > :t 'Data.Ruin.Hoid.hoid' \@((':@') "x") 'rmempty'
--   'Data.Ruin.Hoid.hoid' \@((':@') "x") 'rmempty' :: Monoid t => "x" ':@' t
-- @
rmempty :: (Build t,GivesThese (Fields t) Identity (RPure RMEmpty)) => t
{-# INLINE rmempty #-}
rmempty = rpure MkRMEmpty

-- | An implementation detail of 'rmappend'.
data RMAppend l r = MkRMAppend l r

instance
     ( Applicative i
     , a ~ FieldType s l
     , a ~ FieldType s r
     , Has s l
     , Has s r
     , Monoid a
     )
  => Gives s a i (RMAppend l r) where
  {-# INLINE get #-}
  get = \(MkRMAppend l r) -> Compose $ (\a b -> pure (mappend a b)) <$> extricate1 s l <*> extricate1 s r
    where
    s = mkLabel @s

-- | Combine two records if all of the fields are 'Monoid's.
--
-- @
--   > :t \\l r -> 'Data.Ruin.Hoid.hoid' \@((':@') "x") $ 'rmappend' l r
--   \\l r -> 'Data.Ruin.Hoid.hoid' \@((':@') "x") $ 'rmappend' l r
--     :: Monoid t => "x" ':@' t -> "x" ':@' t -> "x" ':@' t
-- @
rmappend ::
     ( Build t
     , GivesThese (Fields t) Identity (RMAppend t t)
     )
  => t -> t -> t
{-# INLINE rmappend #-}
rmappend = \l r -> runCEI $ build $ MkRMAppend l r

-- | An implementation detail of 'rmappend'.
data RSAppend l r = MkRSAppend l r

instance
     ( Applicative i
     , a ~ FieldType s l
     , a ~ FieldType s r
     , Has s l
     , Has s r
     , Semigroup a
     )
  => Gives s a i (RSAppend l r) where
  {-# INLINE get #-}
  get = \(MkRSAppend l r) -> Compose $ (\a b -> pure (a <> b)) <$> extricate1 s l <*> extricate1 s r
    where
    s = mkLabel @s

-- | Combine two records if all of the fields are 'Semigroups's.
--
-- @
--   > :t \\l r -> 'Data.Ruin.Hoid.hoid' \@((':@') "x") $ 'rsappend' l r
--   \\l r -> 'Data.Ruin.Hoid.hoid' \@((':@') "x") $ 'rsappend' l r
--     :: Semigroup t => "x" ':@' t -> "x" ':@' t -> "x" ':@' t
-- @
rsappend ::
     ( Build t
     , GivesThese (Fields t) Identity (RSAppend t t)
     )
  => t -> t -> t
{-# INLINE rsappend #-}
rsappend = \l r -> runCEI $ build $ MkRSAppend l r

-----

-- | An implementation detail of 'rlabel'.
data RLabel = MkRLabel

instance (a ~ Label s) => FPure RLabel s a where
  {-# INLINE fpure #-}
  fpure = \_ -> mkLabel

-- | The record where the type of field @s@ is @Label s@.
--
-- @
--   > :t 'Data.Ruin.Hoid.hoid' \@((':@') "x") 'rlabel'
--   'Data.Ruin.Hoid.hoid' \@((':@') "x") 'rlabel' :: "x" ':@' 'Label' "x"
-- @
rlabel :: (Build t,GivesThese (Fields t) Identity (RPure RLabel)) => t
{-# INLINE rlabel #-}
rlabel = rpure MkRLabel

-----

infixl 4 `rmap`

-- | If the following constraint holds for every field @s@ in @t@,
-- then @fun@ can map @rc@ to @t@.
--
-- @
--   'FPure' fun s ('FieldType' s rc -> 'FieldType' s t)
-- @
--
-- @
--   > :t \\fun -> 'rmap' fun . 'Data.Ruin.Hoid.hoid' \@((':@') "x")
--   \\fun -> 'rmap' fun . 'Data.Ruin.Hoid.hoid' \@((':@') "x")
--     :: 'FPure' fun "x" (t -> t1) => fun -> "x" ':@' t -> "x" ':@' t1
-- @
rmap ::
  forall fun rc rfun t.
     ( Build rfun
     , Build t
     , GivesThese (Fields rfun) Identity (RPure fun)
     , GivesThese (Fields t) Identity (RSplat rfun rc)
     , UnifyShape rfun t
     , UnifyShape rc t
     )
  => fun -> rc -> t
{-# INLINE rmap #-}
rmap = \fun rc -> (rpure fun :: rfun) `rsplat` rc

-----

infixl 4 `rmapA`

-- | If the following constraint holds for every field @s@ in @t@,
-- then @fun@ can map @rc@ to @t@ within an 'Applicative' functor @i@.
--
-- @
--   'FPure' fun s ('FieldType' s rc -> i ('FieldType' s t))
-- @
--
-- @
--   > :t \\fun -> 'rmapA' fun . 'Data.Ruin.Hoid.hoid' \@((':@') "x")
--   \\fun -> 'rmapA' fun . 'Data.Ruin.Hoid.hoid' \@((':@') "x")
--     :: ('FPure' fun "x" (t -> i t1), Applicative i) =>
--        fun -> "x" ':@' t -> i ("x" ':@' t1)
-- @
rmapA ::
  forall fun i rc rfun t.
     ( Applicative i
     , Build rfun
     , Build t
     , GivesThese (Fields rfun) Identity (RPure fun)
     , GivesThese (Fields t) i (RSplatA rfun rc)
     , UnifyShape rfun t
     , UnifyShape rc t
     )
  => fun -> rc -> i t
{-# INLINE rmapA #-}
rmapA = \fun rc -> (rpure fun :: rfun) `rsplatA` rc

-----

-- | An implementation detail of 'rsplat'.
data RSplat rfun rc = MkRSplat rfun rc

instance
     ( Applicative i
     , FieldType s rfun ~ (FieldType s rc -> b)
     , Has s rfun
     , Has s rc
     )
  => Gives s b i (RSplat rfun rc) where
  {-# INLINE get #-}
  get = \(MkRSplat rfun rc) -> Compose $ fmap @Eval pure $ extricate1 s rfun <*> extricate1 s rc
    where
    s = mkLabel @s

infixl 4 `rsplat`

-- | A record where the value of field @s@ is @'Data.Eval.runEval'
-- ('Data.Ruin.Deep.extricate' \#s rfun \<*>
-- 'Data.Ruin.Deep.extricate' \#s rc)@.
--
-- Compare to \"zippy\" instances of '<*>'.
--
-- @
--   > :t 'rsplat' . 'Data.Ruin.Hoid.hoid' \@((':@') "x")
--   'rsplat' . 'Data.Ruin.Hoid.hoid' \@((':@') "x")
--     :: "x" ':@' (t1 -> t) -> "x" ':@' t1 -> "x" ':@' t
-- @
rsplat ::
     ( Build t
     , GivesThese (Fields t) Identity (RSplat rfun rc)
     , UnifyShape rc t
     , UnifyShape rfun t
     )
  => rfun -> rc -> t
{-# INLINE rsplat #-}
rsplat = \rfun rc -> runCEI $ build $ MkRSplat rfun rc

-----

-- | An implementation detail of 'rsplatA'.
data RSplatA rfun rc = MkRSplatA rfun rc

instance
     ( Applicative i
     , FieldType s rfun ~ (FieldType s rc -> i b)
     , Has s rfun
     , Has s rc
     )
  => Gives s b i (RSplatA rfun rc) where
  {-# INLINE get #-}
  get = \(MkRSplatA rfun rc) -> Compose $ extricate1 s rfun <*> extricate1 s rc
    where
    s = mkLabel @s

infixl 4 `rsplatA`

-- | Like 'rsplat', but in an 'Applicative' functor. Note that every
-- field in @rfun@ must be a function with an @i@-structured codomain.
--
-- @
--   > :t 'rsplatA' . 'Data.Ruin.Hoid.hoid' \@((':@') "x")
--   'rsplatA' . 'Data.Ruin.Hoid.hoid' \@((':@') "x")
--     Applicative i => :: "x" ':@' (t1 -> i t) -> "x" ':@' t1 -> i ("x" ':@' t)
-- @
rsplatA ::
     ( Applicative i
     , Build t
     , GivesThese (Fields t) i (RSplatA rfun rc)
     , UnifyShape rc t
     , UnifyShape rfun t
     )
  => rfun -> rc -> i t
{-# INLINE rsplatA #-}
rsplatA = \rfun rc -> runEval $ getCompose $ build $ MkRSplatA rfun rc
