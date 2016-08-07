{-# Language AllowAmbiguousTypes #-}
{-# Language FlexibleContexts #-}
{-# Language FlexibleInstances #-}
{-# Language MultiParamTypeClasses #-}
{-# Language RoleAnnotations #-}
{-# Language ScopedTypeVariables #-}
{-# Language TemplateHaskell #-}
{-# Language TypeApplications #-}
{-# Language TypeFamilies #-}
{-# Language TypeInType #-}
{-# Language TypeOperators #-}
{-# Language UndecidableInstances #-}
{-# Language UndecidableSuperClasses #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Anonymous records.

module Data.Ruin.R (
  -- * Data kind for fields declarations
  FD,   -- opaque

  -- ** Constructors
  type (:::),
  InsertFD,
  NilFD,
  MkFD,
  PlusFD,

  -- ** Operations
  DeleteFD,
  LookupFD,
  LMergeFD,
  HomogenizeFD,

  -- ** Constraints
  FDAbsent,
  FDFoldable,
  FDFoldable2,
  FDHomogenous,
  FDPure,
  FDSplat,
  FDSplatA,
  fdIdentities,

  -- * Anonymous records
  R,   -- opaque
  Rcrd,

  -- ** Constructors
  nilR,
  oneR,
  plusR,

  -- ** Operations
  addR,
  adjustR,
  deleteR,
  extricate1R,
  getR,
  insertR,
  lensR,
  lmergeR,
  setR,

  -- ** Fieldwise operations
  --
  -- These only compose well if the field names of the involved
  -- records are known. Otherwise the "Show/Read" Problem makes things
  -- quite awkward.
  FPure(..),
  RCompare(..),
  REq(..),
  RShowField(..),
  rfoldR,
  rfoldMapR,
  rfoldMap2R,
  rlabelR,
  rmapR,
  rmapAR,
  rmappendR,
  rmemptyR,
  rpureR,
  rpolypureR,
  rsappendR,
  rsplatR,
  rsplatAR,

  -- ** Monomorphic specializations
  monoadjustR,
  lens'R,
  rmonopureR,

  -- * Field labels
  Label,
  mkLabel,
  ) where

import           Data.Functor.Compose (Compose(..))
import           Data.Functor.Identity (Identity(..))
import qualified Data.HashMap.Lazy as HML
import           Data.Kind (type (*))
import           Data.Monoid (All(..))
import           Data.Semigroup (Semigroup,(<>))
import           Data.Type.Equality
import           GHC.Exts (Any,Constraint)
import qualified GHC.Generics as G
import           GHC.TypeLits hiding (type (*))
import qualified Language.Haskell.TH as TH
import           Language.Haskell.TH.Syntax (Lift(lift))
import           Unsafe.Coerce (unsafeCoerce)

import           Data.Ruin.All
import           Data.Ruin.ClosedHas
import           Data.Ruin.Eval
import           Data.Ruin.Hoid (Hoid)
import           Data.Ruin.Internal
import           Data.Ruin.Fieldwise

-- | An abstract data kind for the field declarations that determine
-- an anonymous record type. The user can only build this type using
-- the handful of type families exported by this module.
--
-- These families ensure that GHC's type equality relation @~@ ignores
-- the order in which fields with different names are added to/removed
-- from the declarations.
newtype FD =
  MkFD [(Symbol,*)]
  -- ^ INVARIANT: strictly ascending by 'Symbol'

type family NilFD :: FD where NilFD = 'MkFD '[]

infix 0 :::
-- | A field declaration.
type s ::: ty = ( '(s,ty) :: (Symbol,*) )

-- | Create a fields declaration from a list of individual field
-- declarations. For example,@'MkFD' '["x" ::: Bool,"y" ::: Maybe
-- Int]) :: 'FD'@.
type family MkFD (ds :: [(Symbol,*)]) :: FD where
  MkFD '[] = NilFD
  MkFD (d ': ds) = InsertFD (Fst d) (Snd d) (MkFD ds)

-----

type family InsertFD (s :: Symbol) (ty :: *) (fd :: FD) :: FD where
  InsertFD s ty ('MkFD ds) = 'MkFD (Insert1 s ty ds)

type family Insert1
  (s :: Symbol)
  (ty :: *)
  (ds :: [(Symbol,*)])
  :: [(Symbol,*)] where
  Insert1 s ty '[] = '[ '(s,ty) ]
  Insert1 s ty (d ': ds) = Insert2 s ty (CmpSymbol s (Fst d)) d ds

type family Insert2
  (s :: Symbol)
  (ty :: *)
  (ord :: Ordering)
  (d :: (Symbol,*))
  (ds :: [(Symbol,*)])
  :: [(Symbol,*)] where
  Insert2 s ty 'LT d ds = '(s,ty) ': d ': ds
  Insert2 s ty 'EQ d ds = '(s,ty) ': ds
  Insert2 s ty 'GT d ds = d ': Insert1 s ty ds

-----

type family DeleteFD (s :: Symbol) (fd :: FD) :: FD where
  DeleteFD s ('MkFD ds) = 'MkFD (Delete1 s ds)

type family Delete1
  (s :: Symbol)
  (ds :: [(Symbol,*)])
  :: [(Symbol,*)] where
  Delete1 s '[] = '[]
  Delete1 s (d ': ds) = Delete2 s (CmpSymbol s (Fst d)) d ds

type family Delete2
  (s :: Symbol)
  (ord :: Ordering)
  (d :: (Symbol,*))
  (ds :: [(Symbol,*)])
  :: [(Symbol,*)] where
  Delete2 s 'LT d ds = d ': ds
  Delete2 s 'EQ d ds = ds
  Delete2 s 'GT d ds = d ': Delete1 s ds

-----

type family LookupFD (s :: Symbol) (fd :: FD) :: * where
  LookupFD s ('MkFD ds) = FinalLookup s ('MkFD ds) (Lookup1 s ds)

type family FinalLookup (s :: Symbol) (fd :: FD) (mty :: Maybe *) :: * where
  FinalLookup s fd 'Nothing = TypeError
    ('Text "Could not find `" ':<>: 'Text s ':<>: 'Text "' in " ':<>: 'ShowType fd)
  FinalLookup _ _ ('Just ty) = ty

type family Lookup1
  (s :: Symbol)
  (ds :: [(Symbol,*)])
  :: Maybe * where
  Lookup1 s '[] = 'Nothing
  Lookup1 s (d ': ds) = Lookup2 s (CmpSymbol s (Fst d)) (Snd d) ds

type family Lookup2
  (s :: Symbol)
  (ord :: Ordering)
  (ty :: *)
  (ds :: [(Symbol,*)])
  :: Maybe * where
  Lookup2 s 'LT _ _ = 'Nothing
  Lookup2 s 'EQ ty _ = 'Just ty
  Lookup2 s 'GT _ ds = Lookup1 s ds

-----

-- | A left-biased version of 'PlusFD'; instead of undefined types,
-- shared fields will keep their type from the left declarations.
type family LMergeFD (fd1 :: FD) (fd2 :: FD) :: FD where
  LMergeFD ('MkFD ds1) ('MkFD ds2) = 'MkFD (LMerge1 ds1 ds2)

type family LMerge1
  (ds1 :: [(Symbol,*)])
  (ds2 :: [(Symbol,*)])
  :: [(Symbol,*)] where
  LMerge1 '[] ds2 = ds2
  LMerge1 ds1 '[] = ds1
  LMerge1 (d1 ': ds1) (d2 ': ds2) = LMerge2 (CmpSymbol (Fst d1) (Fst d2)) d1 ds1 d2 ds2

type family LMerge2
  (ord :: Ordering)
  (d1 :: (Symbol,*))
  (ds1 :: [(Symbol,*)])
  (d2 :: (Symbol,*))
  (ds2 :: [(Symbol,*)])
  :: [(Symbol,*)] where
  LMerge2 'LT d1 ds1 d2 ds2 = d1 ': LMerge1 ds1 (d2 ': ds2)
  LMerge2 'EQ d1 ds1 _  ds2 = d1 ': LMerge1 ds1 ds2
  LMerge2 'GT d1 ds1 d2 ds2 = d2 ': LMerge1 (d1 ': ds1) ds2

-----

-- | Each field that is in both declaration lists will have an undefined type.
--
-- @
--   *Data.Ruin.R> :t 'oneR' \#x () \``plusR`\` ('oneR' \#x () \``plusR`\` 'oneR' \#y ())
--   'oneR' \#x () \``plusR`\` ('oneR' \#x () \``plusR`\` 'oneR' \#y ())
--     :: 'R' ('MkFD '[ '("x", ('TypeError' ...)), '("y", ())) ])
-- @
type family PlusFD (fd1 :: FD) (fd2 :: FD) :: FD where
  PlusFD ('MkFD ds1) ('MkFD ds2) = 'MkFD (Plus1 ('MkFD ds1) ('MkFD ds2) ds1 ds2)

type family Plus1
  (fd1 :: FD)
  (fd2 :: FD)
  (ds1 :: [(Symbol,*)])
  (ds2 :: [(Symbol,*)])
  :: [(Symbol,*)] where
  Plus1 _ _ '[] ds2 = ds2
  Plus1 _ _ ds1 '[] = ds1
  Plus1 fd1 fd2 (d1 ': ds1) (d2 ': ds2) = Plus2 fd1 fd2 (CmpSymbol (Fst d1) (Fst d2)) d1 ds1 d2 ds2

type family Plus2
  (fd1 :: FD)
  (fd2 :: FD)
  (ord :: Ordering)
  (d1 :: (Symbol,*))
  (ds1 :: [(Symbol,*)])
  (d2 :: (Symbol,*))
  (ds2 :: [(Symbol,*)])
  :: [(Symbol,*)] where
  Plus2 fd1 fd2 'LT d1 ds1 d2 ds2 = d1 ': Plus1 fd1 fd2 ds1 (d2 ': ds2)
  Plus2 fd1 fd2 'EQ d1 ds1 _ ds2 =
    '(Fst d1,TypeError (PlusMsg fd1 fd2 (Fst d1))) ': Plus1 fd1 fd2 ds1 ds2
  Plus2 fd1 fd2 'GT d1 ds1 d2 ds2 = d2 ': Plus1 fd1 fd2 (d1 ': ds1) ds2

type PlusMsg fd1 fd2 s =
  'Text "Field `" ':<>: 'Text s ':<>: 'Text "' occurs in both field declaration lists" ':$$: 'Text "    " ':<>: 'ShowType (FieldsFD fd1) ':$$: 'Text "  and" ':$$: 'Text "    " ':<>: 'ShowType (FieldsFD fd2)

-----

type family FDAbsent (s :: Symbol) (fd :: FD) :: Constraint where
  FDAbsent s ('MkFD ds) = FDAbsent1 s ('MkFD ds) (Lookup1 s ds)

type family FDAbsent1 (s :: Symbol) (fd :: FD) (mty :: Maybe *) :: Constraint where
  FDAbsent1 _ _ 'Nothing = ()
  FDAbsent1 s fd ('Just _) = TypeError
    ('Text "`" ':<>: 'Text s ':<>: 'Text "' is already a field in " ':<>: 'ShowType fd)

-----

-- | This type equality provides a "proof by fiat" of some obvious
-- identities involving the fields declarations combinators.
--
-- You typically won't need to use this.
fdIdentities :: forall s fd a.
     '(
        LookupFD s (InsertFD s a fd)
      ,
        InsertFD s (LookupFD s fd) fd
      ,
        DeleteFD s (DeleteFD s fd)
      ,
        InsertFD s a (DeleteFD s fd)
      ,
        Lookup1 s (FieldsFD (DeleteFD s fd))
      ,
        'MkFD (FirstHalf (FieldsFD fd)) `PlusFD` 'MkFD (SecondHalf (FieldsFD fd))
      )
  :~:
    '(
       a
     ,
       fd
     ,
       DeleteFD s fd
     ,
       InsertFD s a fd
     ,
       'Nothing
     ,
       fd
     )
{-# INLINE fdIdentities #-}
fdIdentities = unsafeCoerce (Refl :: () :~: ())

-----

-- | Every field in @fd@ has the same type, @a@.
type family FDHomogenous (a :: *) (fd :: FD) :: Constraint where
  FDHomogenous a fd = (FDHomogenous1 a (FieldsFD fd),Hoid 'MkFD fd)

type family FDHomogenous1 (a :: *) (ds :: [(Symbol,*)]) :: Constraint where
  FDHomogenous1 _ '[] = ()
  FDHomogenous1 a (d ': ds) = (Snd d ~ a,FDHomogenous1 a ds)

-----

-- | A convenient alias.
type Rcrd ds = R (MkFD ds)

-- | A record with fields declarations @fd :: 'FD'@ is a product type
-- with one factor per declared field.
type role R nominal
newtype R (fd :: FD) =
  MkR (HML.HashMap String Any)
  -- ^ INVARIANT: the keys in the map are exactly the fields declared in @fd@.

type family RFD (r :: *) :: FD where RFD (R fd) = fd

nilR :: R NilFD
{-# INLINE nilR #-}
nilR = MkR HML.empty

oneR :: KnownSymbol s => Label s -> a -> R (InsertFD s a NilFD)
{-# INLINE oneR #-}
oneR lbl x = MkR $ HML.singleton (symbolVal lbl) (unsafeCoerce x)

-- | Each field that is in both records will have an undefined type.
plusR :: R fd1 -> R fd2 -> R (PlusFD fd1 fd2)
{-# INLINE plusR #-}
plusR (MkR m1) (MkR m2) = MkR $ HML.union m1 m2

-----

consR :: forall d ds. KnownSymbol (Fst d) => Snd d -> R ('MkFD ds) -> R ('MkFD (d ': ds))
{-# INLINE consR #-}
consR v (MkR m) = MkR $ HML.insert (symbolVal (mkLabel @(Fst d))) (unsafeCoerce v) m

-----

unsafeExtricate1R :: KnownSymbol s => Label s -> R fd -> Eval a
{-# INLINE unsafeExtricate1R #-}
unsafeExtricate1R lbl (MkR m) = case HML.lookup k m of
  Nothing -> error $  "Panic! An ill-formed record is missing field `" ++ k ++ "'"
  Just v -> pure (unsafeCoerce v)
  where
  k = symbolVal lbl

-----

instance FDFoldable RShowField ex1 fd [String] => Show (R fd) where
  showsPrec d r = showParen (d > 10) $ let
    fs = rfoldMapR @ex1 MkRShowField r
    in showString "MkR " . showFields fs

-- | For example:
--
-- @
--   *Data.Ruin.R> mapM_ putStrLn $ 'rfoldMapR' 'MkRShowField' $ 'oneR' \#x "x" \``plusR`\` 'oneR' \#y ()
--   x = "x"
--   y = ()
-- @
data RShowField = MkRShowField

instance (KnownSymbol s, Show a,b ~ (a -> [String])) => FPure RShowField s b where
  fpure = \_ a -> [symbolVal (mkLabel @s) ++ " = " ++ show a]

showFields :: [String] -> ShowS
showFields [] = showString "{}"
showFields (x:xs) = showChar '{' . showString x . go xs
  where
    go [] = showChar '}'
    go (y:ys) = showChar ',' . showString y . go ys

-----

instance FDFoldable2 REq ex1 fd ex3 All => Eq (R fd) where
  r1 == r2 = getAll $ rfoldMap2R @ex1 @fd @ex3 MkREq r1 r2

data REq = MkREq

instance (Eq a,b ~ (a -> a -> All)) => FPure REq s b where
  fpure _ = \x y -> All (x == y)

-----

instance (Eq (R fd),FDFoldable2 RCompare ex1 fd ex3 Ordering) => Ord (R fd) where
  compare r1 r2 = rfoldMap2R @ex1 @fd @ex3 MkRCompare r1 r2

data RCompare = MkRCompare

instance (Ord a,b ~ (a -> a -> Ordering)) => FPure RCompare s b where
  fpure _ = compare

-----

-- | A version of 'insertR' that requires that the field is not
-- already in the record. 'setR' is the opposite.
addR :: (KnownSymbol s,FDAbsent s fd) => Label s -> a -> R fd -> R (InsertFD s a fd)
{-# INLINE addR #-}
addR = insertR

adjustR ::
  ( KnownSymbol s
  , fd1 ~ InsertFD s a fd2
  , fd2 ~ InsertFD s b fd1
  )
  => Label s
  -> (a -> b)
  -> R fd1
  -> R fd2
{-# INLINE adjustR #-}
adjustR lbl f (MkR m) = MkR $ HML.adjust (unsafeCoerce f) (symbolVal lbl) m

deleteR :: KnownSymbol s => Label s -> R fd -> R (DeleteFD s fd)
{-# INLINE deleteR #-}
deleteR lbl (MkR m) = MkR $ HML.delete (symbolVal lbl) m

-- | When forced, this 'Eval' computation extricates the value of the
-- field from the rest of the record without forcing the value of the
-- field itself. See 'extricate1' for further motivation.
extricate1R :: KnownSymbol s => Label s -> R fd -> Eval (LookupFD s fd)
{-# INLINE extricate1R #-}
extricate1R = unsafeExtricate1R

getR :: KnownSymbol s => Label s -> R fd -> LookupFD s fd
{-# INLINE getR #-}
getR lbl = runEval . extricate1R lbl

-- | Add a field to a record, or overwrite it if it's already
-- present. See 'addR' and 'setR'.
insertR :: KnownSymbol s => Label s -> a -> R fd -> R (InsertFD s a fd)
{-# INLINE insertR #-}
insertR lbl x (MkR m) = MkR $ HML.insert (symbolVal lbl) (unsafeCoerce x) m

-- | Left-biased.
lmergeR :: R fd1 -> R fd2 -> R (LMergeFD fd1 fd2)
{-# INLINE lmergeR #-}
lmergeR (MkR m1) (MkR m2) = MkR $ HML.union m1 m2

-- | A version of 'insertR' that requires that the field is already in
-- the record. 'addR' is the opposite.
setR :: forall s fd. KnownSymbol s => Label s -> LookupFD s fd -> R fd -> R fd
{-# INLINE setR #-}
setR = case fdIdentities @s @fd @(LookupFD s fd) of Refl -> insertR

-----

-- | The @"lensR/adjustR" RULE@ rewrites 'lensR' to the more efficient
-- 'adjustR' whenever the lens's functor is 'Identity'.
--
-- Most notably, @"Control.Lens".'Control.Lens.over'@ uses 'Identity'.
lensR ::
  ( KnownSymbol s
  , fd1 ~ InsertFD s a fd2
  , fd2 ~ InsertFD s b fd1
  , Functor f
  )
  => Label s
  -> (a -> f b)
  -> R fd1
  -> f (R fd2)
{-# INLINE[1] lensR #-}
lensR lbl = \f r -> flip (insertR lbl) r <$> f (runEval (unsafeExtricate1R lbl r))

{-# RULES

  "lensR/adjustR"

    forall lbl f.

        lensR lbl f
      =
        Identity . adjustR lbl (runIdentity . f)

  #-}

-----

monoadjustR :: forall s fd. KnownSymbol s => Label s -> (LookupFD s fd -> LookupFD s fd) -> R fd -> R fd
{-# INLINE monoadjustR #-}
monoadjustR = case fdIdentities @s @fd @(LookupFD s fd) of Refl -> adjustR

lens'R ::
  forall s fd f.
  ( KnownSymbol s
  , Functor f
  )
  => Label s
  -> (LookupFD s fd -> f (LookupFD s fd))
  -> R fd
  -> f (R fd)
{-# INLINE lens'R #-}
lens'R = case fdIdentities @s @fd @(LookupFD s fd) of Refl -> lensR

-----

instance KnownSymbol s => HasCase s (R fd) where
  type FieldTypeCase s (R fd) = LookupFD s fd
  {-# INLINE extricate1Case #-}
  extricate1Case = unsafeExtricate1R

instance ClosedHas s (R fd) => Has s (R fd) where
  type FieldType s (R fd) = FieldTypeCase s (R fd)
  {-# INLINE extricate1 #-}
  extricate1 = closedExtricate1

-----

instance (KnownFD fd,Hoid 'MkFD fd) => Build (R fd) where
  type Fields (R fd) = FieldsFD fd
  type Shape (R fd) o = (Hoid R o,Hoid 'MkFD (RFD o),SameFields (FieldsFD fd) (FieldsFD (RFD o)))
  build = buildR
  buildNonStrict = runCEI . build

type family FieldsFD (fd :: FD) :: [(Symbol,*)] where
  FieldsFD ('MkFD ds) = ds

type family SameFields (ds1 :: [(Symbol,*)]) (ds2 :: [(Symbol,*)]) :: Constraint where
  SameFields '[] ds2 = ('[] ~ ds2)
  SameFields (d ': ds1) ds2 =
    ( (Head ds2 ': Tail ds2) ~ ds2
    , '(Fst d,Snd (Head ds2)) ~ Head ds2
    , SameFields ds1 (Tail ds2)
    )

class KnownFD (fd :: FD) where
  buildR :: (Applicative i,GivesThese (Fields (R fd)) i rc) => rc -> Compose Eval i (R fd)

instance KnownFD1 ds => KnownFD ('MkFD ds) where
  buildR = buildR1

class KnownFD1 (ds :: [(Symbol,*)]) where
  buildR1 :: (Applicative i,GivesThese ds i rc) => rc -> Compose Eval i (R ('MkFD ds))

instance KnownFD1 '[] where
  {-# INLINE buildR1 #-}
  buildR1 = const $ pure nilR

instance (KnownSymbol (Fst d),KnownFD1 ds) => KnownFD1 (d ': ds) where
  {-# INLINE buildR1 #-}
  buildR1 rc = consR <$> get @(Fst d) rc <*> buildR1 rc

-----

-- | The type and value of @a@ determine the type and value of every
-- field in @fd@.
type family FDPure (a :: *) (fd :: FD) :: Constraint where
  FDPure a fd = (FDPure0 a fd,Hoid 'MkFD fd)

class FDPure0 (a :: *) (fd :: FD) where
  rpureR0 :: a -> R fd

instance FDPure1 a ds => FDPure0 a ('MkFD ds) where rpureR0 = rpureR1

class FDPure1 (a :: *) (ds :: [(Symbol,*)]) where rpureR1 :: a -> R ('MkFD ds)

instance FDPure1 a '[] where
  {-# INLINE rpureR1 #-}
  rpureR1 = const nilR

instance (KnownSymbol (Fst d),FPure a (Fst d) (Snd d),FDPure1 a ds) => FDPure1 a (d ': ds) where
  {-# INLINE rpureR1 #-}
  rpureR1 a = consR (fpure @a @(Fst d) a) (rpureR1 a)

-- | A specialized 'rpure' for 'R'.
rpureR :: FDPure a fd => a -> R fd
{-# INLINE rpureR #-}
rpureR = rpureR0

-- | A specialized 'rmonopure' for 'R'.
rmonopureR :: FDPure (RMonoPure a) fd => a -> R fd
{-# INLINE rmonopureR #-}
rmonopureR = rpureR . MkRMonoPure

-- | A specialized 'rpolypure' for 'R'.
rpolypureR :: FDPure a fd => a -> R fd
{-# INLINE rpolypureR #-}
rpolypureR = rpureR

-- | A specialized 'rmempty' for 'R'.
rmemptyR :: FDPure RMEmpty fd => R fd
{-# INLINE rmemptyR #-}
rmemptyR = rpureR MkRMEmpty

data RMAppendR = MkRMAppendR

instance (Monoid m,b ~ (m -> m -> m)) => FPure RMAppendR s b where fpure _ = mappend

-- | A specialized 'rmappend' for 'R'.
rmappendR :: FDPure RMAppendR fd => R fd
{-# INLINE rmappendR #-}
rmappendR = rpureR MkRMAppendR

data RSAppendR = MkRSAppendR

instance (Semigroup g,b ~ (g -> g -> g)) => FPure RSAppendR s b where fpure _ = (<>)

-- | A specialized 'rsappend' for 'R'.
rsappendR :: FDPure RSAppendR fd => R fd
{-# INLINE rsappendR #-}
rsappendR = rpureR MkRSAppendR

-- | A specialized 'rlabel' for 'R'.
rlabelR :: FDPure RLabel fd => R fd
{-# INLINE rlabelR #-}
rlabelR = rpureR MkRLabel

-----

infixl 4 `rmapR`

-- | A specialized 'rmap' for 'R'.
rmapR ::
  forall fd1 fd2 fd3 fun.
  ( FDPure fun fd1
  , FDSplat fd1 fd2 fd3
  , UnifyShape (R fd1) (R fd2)
  , UnifyShape (R fd2) (R fd3)
  ) => fun -> R fd2 -> R fd3
{-# INLINE rmapR #-}
rmapR fun r = (rpureR fun :: R fd1) `rsplatR` r

infixl 4 `rsplatR`

-- | A specialized 'rsplat' for 'R'.
rsplatR ::
  ( FDSplat fd1 fd2 fd3
  , UnifyShape (R fd1) (R fd2)
  , UnifyShape (R fd2) (R fd3)
  ) => R fd1 -> R fd2 -> R fd3
{-# INLINE rsplatR #-}
rsplatR (MkR m1) (MkR m2) = MkR $ HML.intersectionWith unsafeCoerce m1 m2

-- | Each field in @fd1@ is a function from the same field in @fd2@ to
-- the same field in @fd3@.
type family FDSplat (fd1 :: FD) (fd2 :: FD) (fd3 :: FD) :: Constraint where
  FDSplat ('MkFD ds1) ('MkFD ds2) ('MkFD ds3) = FDSplat1 ds1 ds2 ds3

type family FDSplat1 (ds1 :: [(Symbol,*)]) (ds2 :: [(Symbol,*)]) (ds3 :: [(Symbol,*)]) :: Constraint where
  FDSplat1 '[] '[] '[] = ()
  FDSplat1 (f ': fs) (a ': as) (b ': bs) = (Snd f ~ (Snd a -> Snd b),FDSplat1 fs as bs)

-----

infixl 4 `rmapAR`

-- | A specialized 'rmapA' for 'R'.
rmapAR ::
  forall fun fd1 fd2 fd3 i.
  ( Applicative i
  , FDPure fun fd1
  , FDSplatA i fd1 fd2 fd3
  , UnifyShape (R fd1) (R fd2)
  , UnifyShape (R fd2) (R fd3)
  ) => fun -> R fd2 -> i (R fd3)
{-# INLINE rmapAR #-}
rmapAR fun r = (rpureR fun :: R fd1) `rsplatAR` r

infixl 4 `rsplatAR`

-- | A specialized 'rsplatA' for 'R'.
rsplatAR ::
  ( Applicative i
  , FDSplatA i fd1 fd2 fd3
  , UnifyShape (R fd1) (R fd2)
  , UnifyShape (R fd2) (R fd3)
  ) => R fd1 -> R fd2 -> i (R fd3)
{-# INLINE rsplatAR #-}
rsplatAR (MkR m1) (MkR m2) = fmap MkR $ sequenceA $ HML.intersectionWith unsafeCoerce m1 m2

-- | Each field in @fd1@ is a function from the same field in @fd2@ to
-- an @i@-structure of the same field in @fd3@.
type family FDSplatA (i :: * -> *) (fd1 :: FD) (fd2 :: FD) (fd3 :: FD) :: Constraint where
  FDSplatA i ('MkFD ds1) ('MkFD ds2) ('MkFD ds3) = FDSplatA1 i ds1 ds2 ds3

type family FDSplatA1 (i :: * -> *) (ds1 :: [(Symbol,*)]) (ds2 :: [(Symbol,*)]) (ds3 :: [(Symbol,*)]) :: Constraint where
  FDSplatA1 _ '[] '[] '[] = ()
  FDSplatA1 i (f ': fs) (a ': as) (b ': bs) = (Snd f ~ (Snd a -> i (Snd b)),FDSplatA1 i fs as bs)

-----

-- | Beware: the order in which the fields are combined is undefined,
-- so the 'Monoid' ought to be commutative.
rfoldR :: (Monoid m,FDHomogenous m fd) => R fd -> m
{-# INLINE rfoldR #-}
rfoldR (MkR m) = foldMap unsafeCoerce m

type family HomogenizeFD (c :: *) (fd :: FD) :: FD where
  HomogenizeFD c ('MkFD ds) = 'MkFD (MapSecondConst c ds)

type family FDFoldable (fun :: *) (fd1 :: FD) (fd2 :: FD) (m :: *) where
  FDFoldable fun fd1 fd2 m =
    ( FDPure fun fd1
    , FDSplat fd1 fd2 (HomogenizeFD m fd2)
    , FDHomogenous m (HomogenizeFD m fd2)
    , Monoid m
    , UnifyShape (R fd1) (R fd2)
    , UnifyShape (R fd2) (R (HomogenizeFD m fd2))
    )

rfoldMapR ::
  forall fd1 fd2 fun m.
     FDFoldable fun fd1 fd2 m
  => fun -> R fd2 -> m
{-# INLINE rfoldMapR #-}
rfoldMapR fun r =
  rfoldR ((rpureR fun :: R fd1) `rsplatR` r :: R (HomogenizeFD m fd2))

type family FDFoldable2 (fun :: *) (fd1 :: FD) (fd2 :: FD) (fd3 :: FD) (m :: *) where
  FDFoldable2 fun fd1 fd2 fd3 m =
    ( FDPure fun fd1
    , FDSplat fd1 fd2 fd3
    , FDSplat fd3 fd2 (HomogenizeFD m fd2)
    , FDHomogenous m (HomogenizeFD m fd2)
    , Monoid m
    , UnifyShape (R fd1) (R fd2)
    , UnifyShape (R fd2) (R fd3)
    , UnifyShape (R fd2) (R (HomogenizeFD m fd2))
    )

rfoldMap2R ::
  forall fd1 fd2 fd3 fun m.
     FDFoldable2 fun fd1 fd2 fd3 m
  => fun -> R fd2 -> R fd2 -> m
{-# INLINE rfoldMap2R #-}
rfoldMap2R fun l r =
  rfoldR
   ((((rpureR fun :: R fd1)
   `rsplatR` l :: R fd3)
   `rsplatR` r :: R (HomogenizeFD m fd2)))

-----

-- | Beware: these conversions are inefficient and very unlikely to be
-- simplified away.
instance (Hoid 'MkFD fd,GenericDs (FieldsFD fd)) => G.Generic (R fd) where
  type Rep (R fd) = RepDs (FieldsFD fd)
  to = toDs
  from = fromDs

class GenericDs (ds :: [(Symbol,*)]) where
  type RepDs ds :: * -> *
  toDs :: RepDs ds x -> R ('MkFD ds)
  fromDs :: R ('MkFD ds) -> RepDs ds x

instance GenericDs '[] where
  type RepDs '[] = G.U1
  toDs _ = nilR
  fromDs _ = G.U1

instance (KnownSymbol (Fst d),Hoid '(,) d) => GenericDs '[d] where
  type RepDs '[d] = G.S1 ('G.MetaSel ('Just (Fst d)) 'G.NoSourceUnpackedness 'G.NoSourceStrictness 'G.DecidedLazy) (G.Rec0 (Snd d))
  toDs (G.M1 (G.K1 x)) = oneR mkLabel x
  fromDs r = G.M1 (G.K1 (getR (mkLabel @(Fst d)) r))

instance
     ( o ~ (d1 ': d2 ': ds)
     , GenericDs (FirstHalf o)
     , GenericDs (SecondHalf o)
     , KnownFD1 (FirstHalf o)
     , KnownFD1 (SecondHalf o)
     , GivesThese (FirstHalf o) Identity (GiveAllItHas (R ('MkFD o)))
     , GivesThese (SecondHalf o) Identity (GiveAllItHas (R ('MkFD o)))
     )
  => GenericDs (d1 ': d2 ': ds) where
  type RepDs (d1 ': d2 ': ds) =
          RepDs (FirstHalf (d1 ': d2 ': ds))
    G.:*:
          RepDs (SecondHalf (d1 ': d2 ': ds))
  toDs (l G.:*: r) = case fdIdentities @"" @('MkFD o) @() of
    Refl ->
              (toDs l :: R ('MkFD (FirstHalf o)))
      `plusR`
              (toDs r :: R ('MkFD (SecondHalf o)))
  fromDs = (\(l,r) -> fromDs l G.:*: fromDs r) . halves

halves ::
     ( KnownFD1 (FirstHalf ds)
     , KnownFD1 (SecondHalf ds)
     , GivesThese (FirstHalf ds) Identity (GiveAllItHas (R ('MkFD ds)))
     , GivesThese (SecondHalf ds) Identity (GiveAllItHas (R ('MkFD ds)))
     )
  => R ('MkFD ds) -> (R ('MkFD (FirstHalf ds)),R ('MkFD (SecondHalf ds)))
halves r = (rupNonStrict r,rupNonStrict r)

-----

instance FDFoldable RLiftField ex1 fd [TH.ExpQ] => Lift (R fd) where
  lift = foldr TH.appE [| nilR |] . id @[TH.ExpQ] . rfoldMapR @ex1 MkRLiftField

data RLiftField = MkRLiftField

instance (KnownSymbol s,Lift a,b ~ (a -> [TH.ExpQ])) => FPure RLiftField s b where
  fpure = \_ a -> [ [| insertR (mkLabel :: Label $s) a |] ]
    where
    s = TH.litT $ TH.strTyLit $ symbolVal (mkLabel @s)
