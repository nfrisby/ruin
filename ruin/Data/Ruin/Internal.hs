{-# Language AllowAmbiguousTypes #-}
{-# Language DataKinds #-}
{-# Language DeriveFunctor #-}
{-# Language ExplicitForAll #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds #-}
{-# Language Rank2Types #-}
{-# Language TypeFamilies #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}

{-# OPTIONS_HADDOCK hide,not-home #-}

module Data.Ruin.Internal (module Data.Ruin.Internal) where

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Proxy (Proxy(..))
import Data.Type.Bool (If)
import GHC.OverloadedLabels
import GHC.TypeLits

import Data.Ruin.Eval
import Data.Ruin.Hoid (Hoid)

runCEI :: Compose Eval Identity a -> a
runCEI = runIdentity . runEval . getCompose

-----

-- | @'proxyOf' = const Proxy@
proxyOf :: a -> Proxy a
proxyOf = const Proxy

-----

-- | Use @-XOverloadedLabels@ to create labels. For example, @#x ::
-- Label "x"@.
--
-- Or use 'mkLabel'.
data Label (s :: Symbol) = MkLabel

-- | Creates a label that is determined either by type inference or
-- via @-XTypeApplications@.
mkLabel :: forall s. Label s
mkLabel = MkLabel

instance (s1 ~ s2) => IsLabel s1 (Label s2) where fromLabel _ = MkLabel

-----

-- | This type is an instance of a type-level difference list, so that
-- sequences of labels can be written as @\#x . \#y . \#z :: 'Labels'
-- '["x","y","z"]@, for example.
type Labels fs = Labels_ '[] -> Labels_ fs

data Labels_ (s :: [Symbol]) = MkLabels_

consLabels :: forall s ss. Labels_ ss -> Labels_ (s ': ss)
consLabels _ = MkLabels_

mkLabels :: forall fs. Labels fs
mkLabels _ = MkLabels_

nilLabels :: Labels_ '[]
nilLabels = MkLabels_

-- | This is essentialy an instance for 'Labels'.
instance (cod ~ Labels_ (s ': ss)) => IsLabel s (Labels_ ss -> cod) where fromLabel = \_ -> consLabels

-----

type family Difference (xs :: [k]) (ys :: [k]) :: [k] where
  Difference '[] ys = '[]
  Difference (x ': xs) ys =
    If (Elem x ys)
      (Difference xs ys)
      (x ': Difference xs ys)

type family Intersection (xs :: [k]) (ys :: [k]) :: [k] where
  Intersection '[] ys = '[]
  Intersection (x ': xs) ys =
    If (Elem x ys)
      (x ': Intersection xs ys)
      (Intersection xs ys)

type family Elem (t :: k) (ts :: [k]) :: Bool where
  Elem t '[] = 'False
  Elem t (t ': ts) = 'True
  Elem t (t2 ': ts) = Elem t ts

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': xs ++ ys

type family MapFst (ps :: [(a,b)]) :: [a] where
  MapFst '[] = '[]
  MapFst ( '(a,b) ': ps ) = a ': MapFst ps

type family MapSecondConst (c :: b) (ps :: [(a,b)]) :: [(a,b)] where
  MapSecondConst _ '[] = '[]
  MapSecondConst c ( '(a,_) ': ps ) = '(a,c) ': MapSecondConst c ps

type family Head (xs :: [a]) :: a where Head (a ': _) = a
type family Tail (xs :: [a]) :: [a] where Tail (_ ': as) = as

type family Fst (p :: (a,b)) :: a where Fst '(a,_) = a
type family Snd (p :: (a,b)) :: b where Snd '(_,b) = b

type family HalfLength (x :: [a]) :: Nat where
  HalfLength (_ ': _ ': xs) = 1 + HalfLength xs
  HalfLength _ = 0

type family Take (n :: Nat) (xs :: [a]) :: [a] where
  Take 0 _ = '[]
  Take n (x ': xs) = x ': Take (n-1) xs

type family Drop (n :: Nat) (xs :: [a]) :: [a] where
  Drop 0 xs = xs
  Drop n (_ ': xs) = Drop (n-1) xs

type FirstHalf xs = Take (HalfLength xs) xs
type SecondHalf xs = Drop (HalfLength xs) xs

type family DifferenceByFst (xs :: [(k,v)]) (ys :: [k]) :: [(k,v)] where
  DifferenceByFst '[] ys = '[]
  DifferenceByFst (x ': xs) ys =
    If (Elem (Fst x) ys)
      (DifferenceByFst xs ys)
      (x ': DifferenceByFst xs ys)

-----

-- | Merely a receptacle in which the user can syntactially use a
-- record selector to avoid the @-Wunused-top-bind@ warning without
-- having to export the record selector.
--
-- @
--   {-\# OPTIONS_GHC -Werror -Wall #-}
--
--   module Foo (Bar(MkBar)) where
--
--   data Bar = MkBar {x,y :: Int}
--
--   instance 'NoWarnUnusedTopBind' Bar where 'noWarnUnusedTopBind' MkBar{x=_,y=_} = ()
--   instance 'Data.Ruin.Has' "x" Bar
--   instance 'Data.Ruin.Has' "y" Bar
--   instance 'Data.Ruin.Build' Bar where
--     {-\# INLINE 'Data.Ruin.rupEval' #-}
--     'Data.Ruin.rupEval' = 'Data.Ruin.genericRupEval'
-- @
--
-- @x@ and @y@ in that example are neither exported nor really used,
-- but there will be no warnings.
--
-- An explicit instance of 'Control.DeepSeq.NFData', for example, will
-- often use a similar record pattern that serves to use the
-- selectors. On the other hand, most such instances are now quite
-- conveient to implicitly derive, so this 'NoWarnUnusedTopBind' class
-- may be the most obvious way to inconsequentially \"use\" a record
-- selector so as to avoid the @-Wunused-top-bind@ warning.
class NoWarnUnusedTopBind t where
  noWarnUnusedTopBind :: Hoid t a => a -> ()

-----

-- | This is a \"tuple of one component\", so that it can have a data
-- constructor like all the other tuples.
--
-- It is crucially not a newtype!
data Tup1 a = MkTup1 a deriving Show
