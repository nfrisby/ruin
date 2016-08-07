{-# Language AllowAmbiguousTypes #-}
{-# Language ConstraintKinds #-}
{-# Language DataKinds #-}
{-# Language DefaultSignatures #-}
{-# Language DeriveDataTypeable #-}
{-# Language DeriveFunctor #-}
{-# Language DeriveGeneric #-}
{-# Language DeriveLift #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language FunctionalDependencies #-}
{-# Language GeneralizedNewtypeDeriving #-}
{-# Language InstanceSigs #-}
{-# Language KindSignatures #-}
{-# Language LambdaCase #-}
{-# Language MagicHash #-}
{-# Language MultiParamTypeClasses #-}
{-# Language PolyKinds #-}
{-# Language Rank2Types #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeInType #-}
{-# Language TypeOperators #-}
{-# Language UndecidableSuperClasses #-}
{-# Language UndecidableInstances #-}
{-# Language ViewPatterns #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}   -- for TypeError, UnifyShape, et al

{-# OPTIONS_HADDOCK hide,not-home #-}

-- | This internal module has everything in it in order to avoid
-- orphans and mutually recursive imports.

module Data.Ruin.All (module Data.Ruin.All) where

import           Control.DeepSeq (NFData(..))
import           Data.Binary (Binary)
import           Data.Data (Data)
import           Data.Char (isSpace)
import           Data.Functor.Compose
import           Data.Functor.Identity
import           Data.Kind
import           Data.Serialize (Serialize)
import           GHC.Exts (Constraint)
import           GHC.Prim (Proxy#,proxy#)
import           GHC.Generics
import           GHC.TypeLits hiding (type (*))
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Lift(lift))

import           Data.Ruin.Eval
import           Data.Ruin.Internal
import           Data.Ruin.Hoid

-- | A custom tuple type. The library user should avoid mentioning
-- this type directly. The only constructor exported by "Data.Ruin" is
-- '<@'.
--
-- __Note__ that the instance @'Has' s ('Pair' l r)@ uses @'Has' s l@ if
-- @s@ is in @'Fields' l@, even if @s@ is also in @'Fields' r@.
--
-- __Note__ the comment on the @'Build' ('Pair' l r)@ instance: unlike
-- tuples, 'Pair's are not record types.
data Pair (l :: *) (r :: *) = MkPair l r
  deriving (Lift)

instance (NFData l,NFData r) => NFData (Pair l r) where
  {-# INLINE rnf #-}
  rnf (MkPair l r) = rnf (l,r)

unPair :: Pair l r -> (l,r)
{-# INLINE unPair #-}
unPair (MkPair l r) = (l,r)

-----

data Loc = Here | L Loc | R Loc

type family Find (rc :: *) (s :: Symbol) :: Maybe Loc where
  Find (Pair l r) s = SearchBoth (Find l s) (Find r s)
  Find rc s = FindViaFields (Fields rc) s

type family SearchBoth (l :: Maybe Loc) (r :: Maybe Loc) :: Maybe Loc where
  SearchBoth ('Just loc) _ = 'Just ('L loc)
  SearchBoth _ ('Just loc) = 'Just ('R loc)
  SearchBoth _ _ = 'Nothing

type family FindViaFields (fields :: [(Symbol,*)]) (s :: Symbol) :: Maybe Loc where
  FindViaFields '[] s = 'Nothing
  FindViaFields ( '(s,ty) ': ss) s = 'Just 'Here
  FindViaFields ( '(s1,ty) ': ss) s2 = FindViaFields ss s2

-----

instance MightHave (Pair l r) (Find (Pair l r) s) (Pair l r) s (FieldType s (Pair l r)) => Has s (Pair l r) where
  type FieldType s (Pair l r) = PairFieldType (Find (Pair l r) s) (Pair l r) s
  {-# INLINE extricate1 #-}
  extricate1 = \_ -> fmap (undub (mkLabel @s)) . mightExtricate1 @(Pair l r) @(Find (Pair l r) s)

type family PairFieldType (ml :: Maybe Loc) (rc :: *) (s :: Symbol) :: * where
  PairFieldType ('Just 'Here) rc s = FieldType s rc
  PairFieldType ('Just ('L loc)) (Pair l r) s = PairFieldType ('Just loc) l s
  PairFieldType ('Just ('R loc)) (Pair l r) s = PairFieldType ('Just loc) r s

-----

class MightHave
  (top :: *)
  (mloc :: Maybe Loc)
  (rc :: *)
  (s :: Symbol) (a :: *)
  where
    mightExtricate1 :: rc -> Eval (s :@ a)

type family Render (t :: *) :: ErrorMessage where
  Render (Pair l r) = Render l ':$$: 'Text "  or in the type" ':$$: Render r
  Render t = 'Text "    " ':<>: 'ShowType t

type NoSuchField (s :: Symbol) (top :: *) =
             'Text "ruin: Could not find a field `"
       ':<>: 'Text s
       ':<>: 'Text "' in the type"
  ':$$: Render top

instance TypeError (NoSuchField s top) => MightHave top 'Nothing rc s a where
  mightExtricate1 = undefined

instance MightHave top ('Just loc) l s a => MightHave top ('Just ('L loc)) (Pair l r) s a where
  {-# INLINE mightExtricate1 #-}
  mightExtricate1 = mightExtricate1 @top @('Just loc) . (\(MkPair l _) -> l)

instance MightHave top ('Just loc) r s a => MightHave top ('Just ('R loc)) (Pair l r) s a where
  {-# INLINE mightExtricate1 #-}
  mightExtricate1 = mightExtricate1 @top @('Just loc) . (\(MkPair _ r) -> r)

instance (Has s rc,a ~ FieldType s rc) => MightHave top ('Just 'Here) rc s a where
  {-# INLINE mightExtricate1 #-}
  mightExtricate1 = fmap (dub s) . extricate1 s
    where
    s = mkLabel @s

-----

type NoBuildPair =
        'Text "ruin: `"
  ':<>: 'ShowType Pair
  ':<>: 'Text "' cannot be an instance of `"
  ':<>: 'ShowType Build
  ':<>: 'Text "'"

-- | This is a non-instance.
instance TypeError NoBuildPair => Build (Pair l r) where
  type Fields (Pair l r) = Fields l ++ DifferenceByFst (Fields r) (FieldNames l)
  build = undefined
  buildNonStrict = undefined
  type Shape (Pair l r) o = (Hoid Pair o,ZipShape (Pair l r) o)

-----

-- | These record types share no field names.
type DisjointFields (a :: *) (b :: *) = MustBeDisjoint a b (Intersection (FieldNames a) (FieldNames b))

type family MustBeDisjoint (a :: *) (b :: *) (ss :: [Symbol]) :: Constraint where
  MustBeDisjoint a b '[] = ()
  MustBeDisjoint a b ss = TypeError (
            'Text "ruin: The record types "
      ':$$: 'Text "  " ':<>: 'ShowType a
      ':$$: 'Text "and"
      ':$$: 'Text "  " ':<>: 'ShowType b
      ':$$: 'Text "must be disjoint, but both have these fields: " ':<>: 'ShowType ss
    )

infixr 3 <@, `MkPair`

-- | Combine two types that might have 'Has' instances for the same
-- 'Symbol' @s@. 'Has' on the result will prefer the first argument.
--
-- NOTE WELL: 'Pair's are not record types. They only have 'Has'
-- instances.
(<@) :: l -> r -> Pair l r
{-# INLINE (<@) #-}
(<@) = MkPair

-----

type FieldNames (t :: *) = MapFst (Fields t)

-----

-- | The key difference betwen 'Gives' and 'Has' is that the codomain
-- is a class index instead of @'FieldType' s rc@. This enables
-- instances like @Monoid a => 'Gives' s a 'MEmpty'@.
class Gives (s :: Symbol) (a :: *) (i :: * -> *) (rc :: *) where get :: rc -> Compose Eval i a

type family GivesThis (field :: (Symbol,*)) (i :: * -> *) (rc :: *) :: Constraint where
  GivesThis f i rc = Gives (Fst f) (Snd f) i rc

type family GivesThese (fields :: [(Symbol,*)]) (i :: * -> *) (rc :: *) :: Constraint where
  GivesThese '[] i rc = ()
  GivesThese (f ': fs) i rc = (GivesThis f i rc,GivesThese fs i rc)

-----

-- | A newtype whose only utility is its parametric 'Gives' instance,
-- which defers to 'Has'.
newtype GiveAllItHas rc = MkGiveAllItHas rc

instance (Applicative i,a ~ FieldType s rc,Has s rc) => Gives s a i (GiveAllItHas rc) where
  {-# INLINE get #-}
  get = \(MkGiveAllItHas rc) -> Compose $ pure <$> extricate1 (mkLabel @s) rc

-- | Like 'GiveAllItHas', but every field in the record must be
-- headed by the 'Applicative' functor @i@.
newtype GiveAllItHasA rc = MkGiveAllItHasA rc

instance (i a ~ FieldType s rc,Has s rc) => Gives s a i (GiveAllItHasA rc) where
  {-# INLINE get #-}
  get = \(MkGiveAllItHasA rc) -> Compose $ extricate1 (mkLabel @s) rc

-----

-- | The /width subtyping/ relation, with evidence.
type rc `IsSubtypeOf` t = GivesThese (Fields t) Identity (GiveAllItHas rc)

-- | Record types: product types where each factor has a static name
-- (i.e. the 'Fields').
class Build (t :: *) where
  -- | Each element of this list is the name of a field and its
  -- type in @t@. Default: 'GenericFields'.
  --
  -- [/Unique Names/] These fields have different names.
  --
  --     @
  --       NubByFst ('Fields' t) = 'Fields' t
  --     @
  --
  -- [/Partitioning/] These fields partition @t@.
  --
  --     @
  --       t is isomorphic to a tuple of the types MapSnd ('Fields' t)
  --     @
  type Fields (t :: *) :: [(Symbol,*)]
  type Fields t = GenericFields t

  -- | The laws for 'build' are given without loss of generality in
  -- terms of 'rupEval'.
  --
  -- [/Eta/] An
  -- <https://ncatlab.org/nlab/show/eta-conversion#for_product_types eta rule>.
  --
  --     @
  --       t '<$' 'rupEval' t = 'rupEval' t
  --     @
  --
  --     This law reasonably requires that @t@ 'Has' all of its own
  --     'Fields'.
  --
  -- [/Strictness/] The 'rupEval' function is strict in its argument,
  -- but it's only strict enough to retrieve the thunks for each of
  -- the necessary fields, without forcing those thunks.
  --
  --     @
  --       seq ('rupEval' rc) =
  --           seq ('extricate1' \#f1 rc)
  --         . seq ('extricate1' \#f2 rc)
  --         ...
  --         . seq ('extricate1' \#fN rc)
  --     @
  --
  -- If @rc@ is a typical single-constructor record type declared with
  -- record syntax and has fields for all of @t@'s 'Fields', then the
  -- /Strictness/ law simplifies to @seq ('rupEval' rc) = seq rc@.
  --
  -- [__Note__] A @GHC.Generics@ default is available as
  -- 'genericBuild'. We do not provide a @DefaultSignature@ because it
  -- is most often critical for performance that 'build' is inlined,
  -- which requires an explicit @INLINE@ pragma (the RHS size gets too
  -- large for inferred inlining with even just three fields). We thus
  -- recommend the following.
  --
  --     @
  --       instance 'Build' Foo where
  --         {-\# INLINE 'build' #-}
  --         'build' = 'genericBuild'
  --     @
  build :: (Applicative i,GivesThese (Fields t) i rc) => rc -> Compose Eval i t

  -- | Like 'build', but maximally non-strict instead of having the
  -- /Strictness/ law. Defaults to 'genericBuildNonStrict', but beware
  -- that a manual @INLINE@ pragma is likely as useful as it is for
  -- 'build'.
  --
  --     @
  --       seq ('build' rc) ('buildNonStrict' rc) = 'runEval' ('build' rc)
  --     @
  buildNonStrict :: GivesThese (Fields t) Identity rc => rc -> t
  default buildNonStrict ::
        ( Fields t ~ GenericFields t
        , Generic t
        , GenericBuild t (Rep t)
        , GivesThese (Fields t) Identity rc
        )
     => rc -> t
  buildNonStrict = genericBuildNonStrict

  -- | The shape of a record type is its most general type, the one
  -- that all instances of that record type are specializations of.
  -- Unless you're being clever, the shape of the type class index @t@
  -- is @t@, since that @t@ is usually as polymorphic as it could be
  -- (i.e. the value that the type variables within @t@ take on do not
  -- change how it instantiates 'Build').
  --
  -- The @'Shape' t o@ constraint requires --- via @~@ --- that @o@
  -- has the same shape as @t@. It must use @~@ to assert this
  -- requirement, so that it can guide type inference.
  --
  -- 'Shape' defaults to 'GenericShape', which is correct for data
  -- types declared with record syntax except for data family
  -- instances. See 'GenericShape' for more info.
  type Shape (t :: *) (o :: *) :: Constraint
  type Shape t o = GenericShape t o

-- | Unify the shape of two record types; see 'Shape'.
type UnifyShape l r = (Shape l r,Shape r l)

-- | Like 'asTypeOf', but doesn't require that the fields have the
-- same types, only that the record types have the same shape.
asShapeOf :: UnifyShape l r => l -> r -> l
{-# INLINE asShapeOf #-}
asShapeOf = const

type family UnifyFieldTypes (ss :: [Symbol]) (t :: *) (h :: *) :: Constraint where
  UnifyFieldTypes '[] _ _ = ()
  UnifyFieldTypes (s ': ss) t h = (FieldType s t ~ FieldType s h,UnifyFieldTypes ss t h)

asFieldTypesOf :: UnifyFieldTypes (FieldNames t) t rc => t -> proxy rc -> t
{-# INLINE asFieldTypesOf #-}
asFieldTypesOf = const

-- | @'GenericShape' t o@ requires that @o@ is headed by the same type
-- constructor that heads @t@:
--
-- @
--   'GenericShape' (T ...) o = 'Hoid' T o
-- @
--
-- This is the correct definition of 'Shape' for all data types
-- declared using record syntax, except for data family instances. For
-- those, the @T@ part should be replaced by the head of the data
-- family instance: the type up to and including the indices but
-- excluding the non-index parameters.
type family GenericShape (t :: k) (o :: *) :: Constraint where
  GenericShape (f _) o = GenericShape f o
  GenericShape t o = Hoid t o

-- | When @tup@ is a product of records (e.g. 'Pair' or '(,,,)'),
-- this constraint applies 'Shape' to the pairwise components.
type family ZipShape (tup :: k) (o :: k) :: Constraint where
  ZipShape (f a) (g b) = (ZipShape f g,Shape a b)
  ZipShape _ _ = ()

-- | 'rup' is an upcast with respect to the /width subtyping/
-- relationship of records; it 'build's a @t@ from any type that has
-- all of @t@'s 'Fields'.
rup :: forall t rc. (Build t,rc `IsSubtypeOf` t) => rc -> t
{-# INLINE rup #-}
rup = runEval . rupEval

-- | 'rup' is an upcast with respect to the /width subtyping/
-- relationship of records; it 'build's a @t@ from any type that has
-- all of @t@'s 'Fields'.
rupA :: forall t rc i. (Applicative i,Build t,GivesThese (Fields t) i (GiveAllItHasA rc)) => rc -> i t
{-# INLINE rupA #-}
rupA = runEval . getCompose . build . MkGiveAllItHasA

-- | @'rup' = 'runEval' . 'rupEval'@
--
-- @'rupEval' = 'build' . 'MkGiveAllItHas'@
rupEval :: forall t rc. (Build t,rc `IsSubtypeOf` t) => rc -> Eval t
{-# INLINE rupEval #-}
rupEval = fmap runIdentity . getCompose . build . MkGiveAllItHas

-- | @'rupNonStrict' = 'buildNonStrict' . 'MkGiveAllItHas'@
rupNonStrict :: forall t rc. (Build t,rc `IsSubtypeOf` t) => rc -> t
{-# INLINE rupNonStrict #-}
rupNonStrict = buildNonStrict . MkGiveAllItHas

-- | "GHC.Generics" implementation of 'Fields'.
type GenericFields t = GFields (Rep t)

-- | "GHC.Generics" implementation of 'rup'.
--
-- Relies on 'extricate1' in order to satisfy the /Strictness/ law of
-- 'Build'.
genericBuild ::
  forall t rc i.
      ( Fields t ~ GenericFields t
      , Applicative i
      , Generic t
      , GenericBuild t (Rep t)
      , GivesThese (Fields t) i rc
      )
   => rc -> Compose Eval i t
{-# INLINE genericBuild #-}
genericBuild = fmap to . gRup @t

-- | "GHC.Generics" implementation of 'buildNonStrict'.
--
-- It is maximally non-strict.
genericBuildNonStrict ::
  forall t rc.
      ( Fields t ~ GenericFields t
      , Generic t
      , GenericBuild t (Rep t)
      , GivesThese (Fields t) Identity rc
      )
   => rc -> t
{-# INLINE genericBuildNonStrict #-}
genericBuildNonStrict = to . gBuildNonStrict @t

-----

type NoFun t =
        'Text "ruin: There is no meaningful instance of `"
  ':<>: 'ShowType t
  ':<>: 'Text "' for functions."
  ':$$: 'Text "  Perhaps you omitted an argument?"

-- | This is a non-instance.
instance TypeError (NoFun Build) => Build (a -> b) where
  type Fields (a -> b) = TypeError (NoFun Build)
  build = undefined
  buildNonStrict = undefined

instance Build () where
  {-# INLINE build #-}
  build rc = genericBuild rc
  {-# INLINE buildNonStrict #-}
  buildNonStrict rc = genericBuildNonStrict rc

instance Build (s :@ a) where
  type Fields (s :@ a) = '[ '(s,a) ]
  {-# INLINE build #-}
  build = fmap (dub mkLabel) . get @s
  {-# INLINE buildNonStrict #-}
  buildNonStrict = runCEI . build
  type Shape (s :@ _) o = Hoid ((:@) s) o

instance Build a => Build (Tup1 a) where
  type Fields (Tup1 a) = Fields a
  {-# INLINE build #-}
  build = fmap MkTup1 . build
  {-# INLINE buildNonStrict #-}
  buildNonStrict = MkTup1 . buildNonStrict
  type Shape (Tup1 a) o = (Hoid Tup1 o,ZipShape (Tup1 a) o)

type family ShapeTup1 (a :: *) (o :: *) where
  ShapeTup1 a (Tup1 oa) = Shape a oa

instance
  ( DisjointFields a b   -- necessary for the eta-rule of 'Build'
  , Lemma_AppendGivesThese (Fields a)
  , Build a
  , Build b
  ) => Build (a,b) where
  type Fields (a,b) = Fields a ++ Fields b
  {-# INLINE build #-}
  build :: forall i rc. (Applicative i,GivesThese (Fields (a,b)) i rc) => rc -> Compose Eval i (a,b)
  build rc =
        (,)
    <$> lemmaFst @(Fields a) (proxy# :: Proxy# i) (proxy# :: Proxy# (Fields b)) build rc
    <*> lemmaSnd @(Fields a) (proxy# :: Proxy# i) (proxy# :: Proxy# (Fields b)) build rc
  {-# INLINE buildNonStrict #-}
  buildNonStrict rc =
    ( lemmaFst @(Fields a) (proxy# :: Proxy# Identity) (proxy# :: Proxy# (Fields b)) buildNonStrict rc
    , lemmaSnd @(Fields a) (proxy# :: Proxy# Identity) (proxy# :: Proxy# (Fields b)) buildNonStrict rc )
  type Shape (a,b) o = (Hoid (,) o,ZipShape (a,b) o)

instance Build ((a,b),c) => Build (a,b,c) where
  type Fields (a,b,c) = Fields (a,b) ++ Fields c
  {-# INLINE build #-}
  build = \rc -> assoc <$> build rc
    where
    assoc ((a,b),c) = (a,b,c)
  {-# INLINE buildNonStrict #-}
  buildNonStrict = \rc -> assoc (buildNonStrict rc)
    where
    assoc ~(~(a,b),c) = (a,b,c)
  type Shape (a,b,c) o = (Hoid (,,) o,ZipShape (a,b,c) o)

instance Build ((a,b),(c,d)) => Build (a,b,c,d) where
  type Fields (a,b,c,d) = Fields (a,b) ++ Fields (c,d)
  {-# INLINE build #-}
  build = \rc -> assoc <$> build rc
    where
    assoc ((a,b),(c,d)) = (a,b,c,d)
  {-# INLINE buildNonStrict #-}
  buildNonStrict = \rc -> assoc (buildNonStrict rc)
    where
    assoc ~(~(a,b),~(c,d)) = (a,b,c,d)
  type Shape (a,b,c,d) o = (Hoid (,,,) o,ZipShape (a,b,c,d) o)

instance Build ((a,b,c),(d,e)) => Build (a,b,c,d,e) where
  type Fields (a,b,c,d,e) = Fields (a,b,c) ++ Fields (d,e)
  {-# INLINE build #-}
  build = \rc -> assoc <$> build rc
    where
    assoc ((a,b,c),(d,e)) = (a,b,c,d,e)
  {-# INLINE buildNonStrict #-}
  buildNonStrict = \rc -> assoc (buildNonStrict rc)
    where
    assoc ~(~(a,b,c),~(d,e)) = (a,b,c,d,e)
  type Shape (a,b,c,d,e) o = (Hoid (,,,,) o,ZipShape (a,b,c,d,e) o)

instance Build ((a,b,c),(d,e,f)) => Build (a,b,c,d,e,f) where
  type Fields (a,b,c,d,e,f) = Fields (a,b,c) ++ Fields (d,e,f)
  {-# INLINE build #-}
  build = \rc -> assoc <$> build rc
    where
    assoc ((a,b,c),(d,e,f)) = (a,b,c,d,e,f)
  {-# INLINE buildNonStrict #-}
  buildNonStrict = \rc -> assoc (buildNonStrict rc)
    where
    assoc ~(~(a,b,c),~(d,e,f)) = (a,b,c,d,e,f)
  type Shape (a,b,c,d,e,f) o = (Hoid (,,,,,) o,ZipShape (a,b,c,d,e,f) o)

instance Build ((a,b,c,d),(e,f,g)) => Build (a,b,c,d,e,f,g) where
  type Fields (a,b,c,d,e,f,g) = Fields (a,b,c,d) ++ Fields (e,f,g)
  {-# INLINE build #-}
  build = \rc -> assoc <$> build rc
    where
    assoc ((a,b,c,d),(e,f,g)) = (a,b,c,d,e,f,g)
  {-# INLINE buildNonStrict #-}
  buildNonStrict = \rc -> assoc (buildNonStrict rc)
    where
    assoc ~(~(a,b,c,d),~(e,f,g)) = (a,b,c,d,e,f,g)
  type Shape (a,b,c,d,e,f,g) o = (Hoid (,,,,,,) o,ZipShape (a,b,c,d,e,f,g) o)

instance Build ((a,b,c,d),(e,f,g,h)) => Build (a,b,c,d,e,f,g,h) where
  type Fields (a,b,c,d,e,f,g,h) = Fields (a,b,c,d) ++ Fields (e,f,g,h)
  {-# INLINE build #-}
  build = \rc -> assoc <$> build rc
    where
    assoc ((a,b,c,d),(e,f,g,h)) = (a,b,c,d,e,f,g,h)
  {-# INLINE buildNonStrict #-}
  buildNonStrict = \rc -> assoc (buildNonStrict rc)
    where
    assoc ~(~(a,b,c,d),~(e,f,g,h)) = (a,b,c,d,e,f,g,h)
  type Shape (a,b,c,d,e,f,g,h) o = (Hoid (,,,,,,,) o,ZipShape (a,b,c,d,e,f,g,h) o)

-----

-- | If @'Has' (fs1 '++' fs2) rc@, then @'Has' fs1 rc@ and @'Has' fs2
-- rc@.
class Lemma_AppendGivesThese (fs1 :: [(Symbol,*)]) where
    lemmaFst :: GivesThese (fs1 ++ fs2) i rc => Proxy# i -> Proxy# fs2 -> (GivesThese fs1 i rc => rc -> a) -> rc -> a
    lemmaSnd :: GivesThese (fs1 ++ fs2) i rc => Proxy# i -> Proxy# fs2 -> (GivesThese fs2 i rc => rc -> a) -> rc -> a

instance Lemma_AppendGivesThese '[] where
  {-# INLINE lemmaFst #-}
  lemmaFst _ _ k = k
  {-# INLINE lemmaSnd #-}
  lemmaSnd _ _ k = k

instance Lemma_AppendGivesThese fs1 => Lemma_AppendGivesThese (f ': fs1) where
  {-# INLINE lemmaFst #-}
  lemmaFst i fs2 k = lemmaFst @fs1 i fs2 k
  {-# INLINE lemmaSnd #-}
  lemmaSnd i fs2 k = lemmaSnd @fs1 i fs2 k

-----

type t `IsSymmetricRecordOf` rc =
  ( t `IsSubtypeOf` rc
  , NoExtraFields t rc (FieldNames t) (FieldNames rc)
  )

-- | This layer stalls until both sets of fields are known. This is
-- just to provide terser constraints in the contexts of inferred polytypes.
type family NoExtraFields (t :: *) (rc :: *) (sts :: [Symbol]) (srcs :: [Symbol]) :: Constraint where
  NoExtraFields _ rc '[]       '[]           = MustHaveNoExtras rc (Difference '[] '[])
  NoExtraFields _ rc (f ': fs) '[]           = MustHaveNoExtras rc (Difference '[] (f ': fs))
  NoExtraFields _ rc '[]       (src ': srcs) = MustHaveNoExtras rc (Difference (src ': srcs) '[])
  NoExtraFields _ rc (f ': fs) (src ': srcs) = MustHaveNoExtras rc (Difference (src ': srcs) (f ': fs))

type family MustHaveNoExtras (rc :: *) (ss :: [Symbol]) :: Constraint where
  MustHaveNoExtras rc '[] = ()
  MustHaveNoExtras rc ss = TypeError (
            'Text "ruin: The argument type"
      ':$$: 'Text "  " ':<>: 'ShowType rc
      ':$$: 'Text "has unused fields: " ':<>: 'ShowType ss
    )

-- | An isomorphism based on 'rup', when the two record types have a
-- symmetric subtyping relation.
--
-- [Isomorphism]
--
--     @
--       forall t s.
--         ( s '`IsSubtypeOf`' t,'Build' s
--         , t '`IsSubtypeOf`' s,'Build' t
--         ) => 'rsym' . id \@t . 'rsym' = id \@s
--     @
rsym ::
    (l `IsSymmetricRecordOf` r,Build r)
 => l -> r
{-# INLINE rsym #-}
rsym = rup

-- | @'rto' \@h = 'hoid' \@h . 'rsym'@
rto :: forall h t rc. (Hoid h t,rc `IsSymmetricRecordOf` t,Build t) => rc -> t
{-# INLINE rto #-}
rto = hoid @h . rsym

-- | @'rfrom' \@h = 'rsym' . 'hoid' \@h@
rfrom :: forall h rc t. (Hoid h rc,rc `IsSymmetricRecordOf` t,Build t) => rc -> t
{-# INLINE rfrom #-}
rfrom = rsym . hoid @h

prto :: forall h t rc. (Hoid h t,rc `IsSymmetricRecordOf` t,Build t) => Proxy# h -> rc -> t
{-# INLINE prto #-}
prto _ = rto @h

prtoA ::
  forall h t rc i.
     (Hoid h t,Applicative i,SymmetricRecordsA t i rc,Build t)
  => Proxy# h
  -> rc -> i t
{-# INLINE prtoA #-}
prtoA _ = rtoA @h

prfrom :: forall h rc t. (Hoid h rc,rc `IsSymmetricRecordOf` t,Build t) => Proxy# h -> rc -> t
{-# INLINE prfrom #-}
prfrom _ = rfrom @h

rsymA ::
    (Applicative i,SymmetricRecordsA t i rc,Build t)
 => rc -> i t
{-# INLINE rsymA #-}
rsymA = rupA

rtoA ::
  forall h t rc i.
     (Hoid h t,Applicative i,SymmetricRecordsA t i rc,Build t)
  => rc -> i t
{-# INLINE rtoA #-}
rtoA = rsymA

rfromA ::
  forall h rc t i.
     (Hoid h rc,Applicative i,SymmetricRecordsA t i rc,Build t)
  => rc -> i t
{-# INLINE rfromA #-}
rfromA = rsymA . hoid @h

type SymmetricRecordsA t i rc =
  ( GivesThese (Fields t) i (GiveAllItHasA rc)
  , NoExtraFields t rc (FieldNames t) (FieldNames rc)
  )

-----

type family GFields (rep :: * -> *) :: [(Symbol,*)] where
  GFields (M1 D c rep) = GFields rep
  GFields (M1 C c rep) = GFields rep
  GFields (M1 S ('MetaSel ('Just s) su ss ds) (K1 i c)) = '[ '(s,c) ]
  GFields (l :*: r) = GFields l ++ GFields r
  GFields U1 = '[]

-----

-- | Generic defintion of 'rup'.
class GenericBuild (top :: *) (rep :: * -> *) where
    gRup :: (Applicative i,GivesThese (GFields rep) i rc) => rc -> Compose Eval i (rep x)
    gBuildNonStrict :: GivesThese (GFields rep) Identity rc => rc -> rep x

type NoConstructors (dn :: Symbol) =
        'Text "ruin: Cannot derive "
  ':<>: 'ShowType Build
  ':<>: 'Text " for `"
  ':<>: 'Text dn
  ':<>: 'Text "' because it doesn't have any constructors."

instance TypeError (NoConstructors dn) => GenericBuild top (M1 D ('MetaData dn mn pn nt) V1) where
  gRup = undefined
  gBuildNonStrict = undefined

type TooManyConstructors (dn :: Symbol) =
        'Text "ruin: Cannot derive "
  ':<>: 'ShowType Build
  ':<>: 'Text " for `"
  ':<>: 'Text dn
  ':<>: 'Text "' because it has more than one constructor."

instance TypeError (TooManyConstructors dn) => GenericBuild top (M1 D ('MetaData dn mn pn nt) (l :+: r)) where
  gRup = undefined
  gBuildNonStrict = undefined

instance GenericBuildConArgs top dn rep => GenericBuild top (M1 D ('MetaData dn mn pn nt) (M1 C c rep)) where
  {-# INLINE gRup #-}
  gRup rc = (M1 . M1) <$> gRupConArgs @top @dn rc
  {-# INLINE gBuildNonStrict #-}
  gBuildNonStrict rc = M1 (M1 (gBuildNonStrictConArgs @top @dn rc))

class GenericBuildConArgs (top :: *) (dn :: Symbol) (rep :: * -> *) where
  -- | Use 'Eval' so that we can project out the components from @rc@
  -- /before/ building the @rep@.
  gRupConArgs :: (Applicative i,GivesThese (GFields rep) i rc) => rc -> Compose Eval i (rep x)
  gBuildNonStrictConArgs :: GivesThese (GFields rep) Identity rc => rc -> rep x

instance (Lemma_AppendGivesThese (GFields l),GenericBuildConArgs top dn l,GenericBuildConArgs top dn r) => GenericBuildConArgs top dn (l :*: r) where
  {-# INLINE gRupConArgs #-}
  gRupConArgs :: forall i rc x. (Applicative i,GivesThese (GFields (l :*: r)) i rc) => rc -> Compose Eval i ((l :*: r) x)
  gRupConArgs rc =
        (:*:)
    <$> lemmaFst @(GFields l) (proxy# :: Proxy# i) (proxy# :: Proxy# (GFields r)) (gRupConArgs @top @dn) rc
    <*> lemmaSnd @(GFields l) (proxy# :: Proxy# i) (proxy# :: Proxy# (GFields r)) (gRupConArgs @top @dn) rc
  {-# INLINE gBuildNonStrictConArgs #-}
  gBuildNonStrictConArgs rc =
        lemmaFst @(GFields l) (proxy# :: Proxy# Identity) (proxy# :: Proxy# (GFields r)) (gBuildNonStrictConArgs @top @dn) rc
    :*: lemmaSnd @(GFields l) (proxy# :: Proxy# Identity) (proxy# :: Proxy# (GFields r)) (gBuildNonStrictConArgs @top @dn) rc

type NotBuildSyntax (dn :: Symbol) =
        'Text "ruin: Cannot derive "
  ':<>: 'ShowType Build
  ':<>: 'Text " for `"
  ':<>: 'Text dn
  ':<>: 'Text "' because its definition doesn't use record type syntax."

instance TypeError (NotBuildSyntax dn) => GenericBuildConArgs top dn (M1 S ('MetaSel 'Nothing su ss ds) rep) where
  gRupConArgs = undefined
  gBuildNonStrictConArgs = undefined

-- The @'Has' s top@ constraint is not strictly necessary, but gives
-- the user a more precise error message when they forget that
-- instance.
instance (Has s top,rep ~ K1 i c) => GenericBuildConArgs top dn (M1 S ('MetaSel ('Just s) su ss ds) rep) where
  {-# INLINE gRupConArgs #-}
  gRupConArgs = fmap (M1 . K1) . get @s
  {-# INLINE gBuildNonStrictConArgs #-}
  gBuildNonStrictConArgs = M1 . K1 . runCEI . get @s

instance GenericBuildConArgs top dn U1 where
  {-# INLINE gRupConArgs #-}
  gRupConArgs _ = pure U1
  {-# INLINE gBuildNonStrictConArgs #-}
  gBuildNonStrictConArgs _ = U1

-----

-- | @t@ has a field named @s@ that inhabits @'FieldType' s t@.
--
-- #careful-strictness#
--
-- 'extricate1' projects out the field, with special care to
-- strictness. The 'Eval' layer provides a stopping point for the
-- projection computation. Without this layer, one would have to force
-- the value itself in order to force the extrication enough so that
-- the rest of @t@ could be GC'd. On the contrary, @case 'extricate1' t
-- of 'Done' x -> x@ neither retains @t@ nor forces @x@.
--
-- [/Strictness/] Forcing the 'Done' layer of 'extricate1' forces
-- enough of @t@ to reach the field but doesn't force the field. This
-- is difficult to formalize in a general and illuminating way, so
-- this law is instantiated below for a simple record type.
--
--     @
--       data XY = MkXY {x,y :: Int}
--
--       'extricate1' #x (undefined :: XY) = undefined
--
--       flip seq () $ 'extricate1' #x (MkXY undefined undefined) = ()
--     @
class Has (s :: Symbol) (t :: *) where
  -- | Default: 'GenericFieldType'
  type FieldType s t :: *
  type FieldType s t = GenericFieldType s t

  -- | Default: 'genericExtricate1'
  extricate1 :: Label s -> t -> Eval (FieldType s t)

  default extricate1 :: (Generic t,GBox (IsABox (Rep t)) t,GenericHas (Rep t) s (FieldType s t)) => Label s -> t -> Eval (FieldType s t)
  {-# INLINE extricate1 #-}
  extricate1 = genericExtricate1

-- | "GHC.Generics" implementation of 'FieldType'.
type GenericFieldType s t = GFieldType (GFind (Rep t) s) (Rep t)

-- | "GHC.Generics" implementation of 'extricate1'.
genericExtricate1 ::
  forall s t.
     (Generic t,GBox (IsABox (Rep t)) t,GenericHas (Rep t) s (FieldType s t))
  => Label s -> t -> Eval (FieldType s t)
{-# INLINE genericExtricate1 #-}
genericExtricate1 = \_ t -> fmap (undub (mkLabel @s)) $ fromEval @(IsABox (Rep t)) t >>= gExtricate1

-- | See 'GBox'.
type family IsABox (rep :: * -> *) :: Bool where
  IsABox (M1 D ('MetaData _ _ _ 'False) (M1 C c (M1 S s (K1 k a)))) = 'True
  IsABox _ = 'False

-- | This class distinguishes between @data T a = MkT a@ and @data T a
-- = Mk !a@/@newtype T a = MkT a@, since 'Generic''s 'from' conflates
-- the two.
--
-- The first index is assumed to be @('IsABox' (Rep t))@.
class GBox (isABox :: Bool) (t :: *) where
  fromEval :: t -> Eval (Rep t x)

instance Generic t => GBox 'False t where
  {-# INLINE fromEval #-}
  fromEval = pure . from
instance Generic t => GBox 'True t where
  {-# INLINE fromEval #-}
  fromEval t = t `seq` pure (from t)

-----

type family GFieldType (ml :: Maybe Loc) (rep :: * -> *) :: * where
  GFieldType ('Just 'Here) (M1 S ('MetaSel ('Just s) su ss ds) (K1 i c)) = c
  GFieldType ('Just loc) (M1 i c rep) = GFieldType ('Just loc) rep
  GFieldType ('Just ('L loc)) (l :*: r) = GFieldType ('Just loc) l
  GFieldType ('Just ('R loc)) (l :*: r) = GFieldType ('Just loc) r

-----

-- | This is a non-instance.
instance TypeError (NoFun Has) => Has s (a -> b) where
  type FieldType s (a -> b) = TypeError (NoFun Has)
  extricate1 = undefined

-- | This is a non-instance.
instance TypeError (NoSuchField s ()) => Has s () where
  type FieldType s () = TypeError (NoSuchField s ())
  extricate1 = undefined

-----

infix 1 :@

-- | A record type with a single field.
newtype (s :: Symbol) :@ (a :: *) = Dub a
  deriving (Binary,Data,Eq,Functor,Generic,Generic1,NFData,Ord,Serialize)

type family SingletonType (singleton :: *) :: * where SingletonType (_ :@ a) = a
type family FunctorType (fa :: *) :: * -> * where FunctorType (f _) = f

instance (s1 ~ s2) => Has s1 (s2 :@ a) where
  type FieldType s1 (s2 :@ a) = a
  {-# INLINE extricate1 #-}
  extricate1 = \_ -> pure . undub mkLabel

instance (Lift a,KnownSymbol s) => Lift (s :@ a) where
  lift (Dub a) =
    [| dub (mkLabel :: Label $(TH.litT (TH.strTyLit s))) a |]
    where
    s = symbolVal' (proxy# :: Proxy# s)

instance (KnownSymbol s,Show a) => Show (s :@ a) where
  showsPrec d (Dub a) = showParen (d > app_prec) $
    prefix . showsPrec (app_prec+1) a
    where
    app_prec = 10
    prefix
     | hasEscapes || hasSpaces = showString "dub (mkLabel @" . showString s' . showString ") "
     | otherwise = showString "dub #" . showString s . showString " "
    s = symbolVal' (proxy# :: Proxy# s)
    s' = show s
    hasEscapes = s' /= '"' : s ++ ['"']
    hasSpaces = any isSpace s

dub :: Label s -> a -> s :@ a
{-# INLINE dub #-}
dub = \_ -> Dub

undub :: Label s -> s :@ a -> a
{-# INLINE undub #-}
undub = \_ (Dub a) -> a

-----

-- | Generic definition of 'has'.
class GenericHas (rep :: * -> *) (s :: Symbol) (a :: *) where
    gExtricate1 :: rep x -> Eval (s :@ a)

instance GArgsHas dn (GFind conargs s) conargs s a => GenericHas (M1 D ('MetaData dn mn pn nt) (M1 C c conargs)) s a where
  {-# INLINE gExtricate1 #-}
  gExtricate1 = gArgsExtricate1 @dn @(GFind conargs s) . unM1 . unM1

type Not1ConstructorMessage (dn :: Symbol) =
        'Text "ruin: The type `"
  ':<>: 'Text dn
  ':<>: 'Text "' must have exactly one constructor to derive `"
  ':<>: 'ShowType Has
  ':<>: 'Text "'"

instance TypeError (Not1ConstructorMessage dn) => GenericHas (M1 D ('MetaData dn mn pn nt) (l :+: r)) s a where
  gExtricate1 = undefined

instance TypeError (Not1ConstructorMessage dn) => GenericHas (M1 D ('MetaData dn mn pn nt) V1) s a where
  gExtricate1 = undefined

type family GFind (rep :: * -> *) (s :: Symbol) :: Maybe Loc where
  GFind (M1 S ('MetaSel ('Just s) su ss ds) rep) s = 'Just 'Here
  GFind (M1 i c rep) s = GFind rep s   -- only used for GFieldType
  GFind (l :*: r) s = MergeLoc (GFind l s) (GFind r s)
  GFind rep s = 'Nothing

type family MergeLoc (l :: Maybe Loc) (r :: Maybe Loc) :: Maybe Loc where
  MergeLoc 'Nothing 'Nothing = 'Nothing
  MergeLoc 'Nothing ('Just r) = 'Just ('R r)
  MergeLoc ('Just l) _ = 'Just ('L l)

class GArgsHas (dn :: Symbol) (ml :: Maybe Loc) (rep :: * -> *) (s :: Symbol) (a :: *) where
    gArgsExtricate1 :: rep x -> Eval (s :@ a)

type NoSuchSelector (dn :: Symbol) (s :: Symbol) =
        'Text "ruin: The type `"
  ':<>: 'Text dn
  ':<>: 'Text "' must declare a record selector named `"
  ':<>: 'Text s
  ':<>: 'Text "' to derive `"
  ':<>: 'ShowType Has
  ':<>: 'Text " "
  ':<>: 'ShowType s
  ':<>: 'Text "'"

instance TypeError (NoSuchSelector dn s) => GArgsHas dn 'Nothing rep s a where
  gArgsExtricate1 = undefined

instance (rep ~ K1 i a) => GArgsHas dn ('Just 'Here) (M1 S ('MetaSel ('Just s) su ss ds) rep) s a where
  {-# INLINE gArgsExtricate1 #-}
  gArgsExtricate1 = pure . dub mkLabel . unK1 . unM1

instance GArgsHas dn ('Just loc) l s a => GArgsHas dn ('Just ('L loc)) (l :*: r) s a where
  {-# INLINE gArgsExtricate1 #-}
  gArgsExtricate1 (l :*: _) = gArgsExtricate1 @dn @('Just loc) l

instance GArgsHas dn ('Just loc) r s a => GArgsHas dn ('Just ('R loc)) (l :*: r) s a where
  {-# INLINE gArgsExtricate1 #-}
  gArgsExtricate1 (_ :*: r) = gArgsExtricate1 @dn @('Just loc) r

-----

instance Has s a => Has s (Tup1 a) where
  type FieldType s (Tup1 a) = FieldType s a
  {-# INLINE extricate1 #-}
  extricate1 s = \(MkTup1 x) -> extricate1 s x

instance (DisjointFields a b,Has s (Pair a b)) => Has s (a,b) where
  type FieldType s (a,b) = FieldType s (Pair a b)
  {-# INLINE extricate1 #-}
  extricate1 s = extricate1 s . uncurry (<@)

instance Has s ((a,b),c) => Has s (a,b,c) where
  type FieldType s (a,b,c) = FieldType s ((a,b),c)
  {-# INLINE extricate1 #-}
  extricate1 = \s -> extricate1 s . reassoc
    where
    reassoc (a,b,c) = ((a,b),c)

instance Has s ((a,b),(c,d)) => Has s (a,b,c,d) where
  type FieldType s (a,b,c,d) = FieldType s ((a,b),(c,d))
  {-# INLINE extricate1 #-}
  extricate1 = \s -> extricate1 s . reassoc
    where
    reassoc (a,b,c,d) = ((a,b),(c,d))

instance Has s ((a,b,c),(d,e)) => Has s (a,b,c,d,e) where
  type FieldType s (a,b,c,d,e) = FieldType s ((a,b,c),(d,e))
  {-# INLINE extricate1 #-}
  extricate1 = \s -> extricate1 s . reassoc
    where
    reassoc (a,b,c,d,e) = ((a,b,c),(d,e))

instance Has s ((a,b,c),(d,e,f)) => Has s (a,b,c,d,e,f) where
  type FieldType s (a,b,c,d,e,f) = FieldType s ((a,b,c),(d,e,f))
  {-# INLINE extricate1 #-}
  extricate1 = \s -> extricate1 s . reassoc
    where
    reassoc (a,b,c,d,e,f) = ((a,b,c),(d,e,f))

instance Has s ((a,b,c,d),(e,f,g)) => Has s (a,b,c,d,e,f,g) where
  type FieldType s (a,b,c,d,e,f,g) = FieldType s ((a,b,c,d),(e,f,g))
  {-# INLINE extricate1 #-}
  extricate1 = \s -> extricate1 s . reassoc
    where
    reassoc (a,b,c,d,e,f,g) = ((a,b,c,d),(e,f,g))

instance Has s ((a,b,c,d),(e,f,g,h)) => Has s (a,b,c,d,e,f,g,h) where
  type FieldType s (a,b,c,d,e,f,g,h) = FieldType s ((a,b,c,d),(e,f,g,h))
  {-# INLINE extricate1 #-}
  extricate1 = \s -> extricate1 s . reassoc
    where
    reassoc (a,b,c,d,e,f,g,h) = ((a,b,c,d),(e,f,g,h))

-----

-- | Get the labels of a record type's fields.
fieldLabelsOf :: forall t proxy. proxy t -> Labels (FieldNames t)
fieldLabelsOf _ = mkLabels @(FieldNames t)
